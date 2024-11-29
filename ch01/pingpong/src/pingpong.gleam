import gleam/erlang/process.{type Subject}
import gleam/io

// 1. Define Messages
// 2. Create send and receive channels including exchange channels
//    send, receive: Naming is always from the perspective of the process.
//    Exchange channels are typically necessary.
// 3. Create functions for processes
// 4. Handler is a wrapper around the process.receive function
//    Handler always gets at least a receive channel as args.
// 5. Write a wrapper around process functions as functions with no args
//    as needed by process.start

// Messages for ping and pong processes //////////////////////////////////////
type Ping {
  Ping
}

type Pong {
  Pong
}

// Main //////////////////////////////////////////////////////////////////////

pub fn main() -> Nil {
  ping()
}

// Ping process //////////////////////////////////////////////////////////////

fn ping() -> Nil {
  // Start pong process
  let exch_ch = process.new_subject()
  let rec_ch = process.new_subject()
  let pong = fn() { pong(exch_ch, rec_ch) }
  process.start(pong, True)

  // pong process sends us its ping channel
  // where we can send ping messages to
  let assert Ok(send_ch) = process.receive(exch_ch, 1000)

  // send ping
  process.send(send_ch, Ping)
  io.println("Ping!")

  // receive pong
  let assert Ok(Pong) = process.receive(rec_ch, 1000)
  io.println("Pong!")
}

// Pong process //////////////////////////////////////////////////////////////

fn pong(exch_ch: Subject(Subject(Ping)), send_ch: Subject(Pong)) -> Nil {
  let rec_ch = process.new_subject()
  process.send(exch_ch, rec_ch)
  handler(rec_ch, send_ch)
}

fn handler(rec_ch: Subject(Ping), send_ch: Subject(Pong)) -> Nil {
  case process.receive(rec_ch, 1000) {
    Ok(Ping) -> process.send(send_ch, Pong)
    Error(Nil) -> panic as "You sent me crap!"
  }
}
