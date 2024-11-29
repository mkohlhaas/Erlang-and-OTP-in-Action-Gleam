import gleam/erlang/process.{type Subject}
import gleam/io

type Message {
  Add(Int)
  Subtract(Int)
  GetTotal
}

type Sum =
  Int

const num_messages = 1_000_000

// main process //////////////////////////////////////////////////////////////////////////

pub fn main() -> Int {
  // Start counter process
  let exch_ch = process.new_subject()
  let rec_ch = process.new_subject()
  let counter = fn() { counter(exch_ch, rec_ch) }
  process.start(counter, True)

  // counter process sends us its receive channel
  let assert Ok(send_ch) = process.receive(exch_ch, 1000)

  send_and_print(send_ch, rec_ch, Add(1))
  send_and_print(send_ch, rec_ch, Subtract(1))
}

fn send_messages(
  send_ch: Subject(Message),
  rec_ch: Subject(Sum),
  num_messages n: Int,
  message message: Message,
) -> Nil {
  case n {
    0 -> Nil
    _ -> {
      process.send(send_ch, message)
      send_messages(send_ch, rec_ch, n - 1, message)
    }
  }
}

// Get result and print it
fn print_counter(send_ch: Subject(Message), rec_ch: Subject(Sum)) -> Int {
  process.send(send_ch, GetTotal)
  let assert Ok(res) = process.receive(rec_ch, 2000)
  io.debug(res)
}

fn send_and_print(
  send_ch: Subject(Message),
  rec_ch: Subject(Sum),
  message: Message,
) -> Int {
  send_messages(send_ch, rec_ch, num_messages, message)
  print_counter(send_ch, rec_ch)
}

// counter process ///////////////////////////////////////////////////////////////////////

fn counter(exch_ch: Subject(Subject(Message)), send_ch: Subject(Sum)) -> Nil {
  // Send receive channel to calling process
  let rec_ch = process.new_subject()
  process.send(exch_ch, rec_ch)
  handler(0, rec_ch, send_ch)
}

fn handler(count: Int, rec_ch: Subject(Message), send_ch: Subject(Sum)) -> Nil {
  case process.receive(rec_ch, 1000) {
    Ok(Add(n)) -> {
      handler(count + n, rec_ch, send_ch)
    }
    Ok(Subtract(n)) -> {
      handler(count - n, rec_ch, send_ch)
    }
    Ok(GetTotal) -> {
      process.send(send_ch, count)
      handler(count, rec_ch, send_ch)
    }
    Error(_) -> panic as "Don't know what to do!"
  }
}
