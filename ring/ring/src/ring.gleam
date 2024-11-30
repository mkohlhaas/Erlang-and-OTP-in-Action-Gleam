import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/list
import gleam/result

pub type Msg {
  Stop
  Done
  Neighbor(Subject(Msg))
  DecCounter(Int)
}

const num_links = 100

const counter = 100_000

const recv_timeout = 100_000

// Head link process /////////////////////////////////////////////////////////////////////

pub fn main() {
  create_links()
}

fn create_links() {
  let main_ch = process.new_subject()
  let exch_ch = process.new_subject()

  // start links
  let link = fn() { link(exch_ch, main_ch) }
  list.range(1, num_links)
  |> list.each(fn(_) { process.start(link, True) })

  // collect channels
  let channels =
    list.range(1, num_links)
    |> list.map(fn(_) { process.receive(exch_ch, recv_timeout) })

  // check channels
  let #(good, bad) = result.partition(channels)

  // io.debug(good)
  io.debug(bad)
  io.debug(list.length(good))

  // set up neighbors
  let assert [first, ..rest] = good
  list.zip(good, list.append(rest, [first]))
  |> list.each(fn(elem) {
    case elem {
      #(left, right) -> {
        process.send(left, Neighbor(right))
      }
    }
  })

  // send initial counter to head link 
  process.send(first, DecCounter(num_links * counter))
  let assert Ok(_) = process.receive(main_ch, recv_timeout)
}

// Link process //////////////////////////////////////////////////////////////////////////

fn link(exch_ch: Subject(Subject(Msg)), head_ch: Subject(Msg)) {
  let recv_ch = process.new_subject()
  process.send(exch_ch, recv_ch)
  handler(recv_ch, head_ch)
}

fn handler(recv_ch: Subject(Msg), head_ch: Subject(Msg)) {
  case process.receive(recv_ch, recv_timeout) {
    Ok(Stop) -> io.debug("Stopping this link!")
    Ok(Done) -> io.debug("Count is zero!")
    Ok(DecCounter(n)) -> {
      case n {
        0 -> process.send(head_ch, Done)
        _ -> {
          let neighbor = get_registry(next_atom())
          process.send(neighbor, DecCounter(n - 1))
        }
      }
      handler(recv_ch, head_ch)
    }
    Ok(Neighbor(neighbor)) -> {
      put_registry(next_atom(), neighbor)
      handler(recv_ch, head_ch)
    }
    Error(_) -> panic as "Don't know what happened!"
  }
}

@external(erlang, "erlang", "put")
fn put_registry(key: Atom, val: Subject(Msg)) -> Nil

@external(erlang, "erlang", "get")
fn get_registry(key: Atom) -> Subject(Msg)

fn next_atom() -> Atom {
  case atom.from_string("next") {
    Ok(atom) -> atom
    Error(_) -> atom.create_from_string("next")
  }
}
