#load "./blackjack.fsx"
open Blackjack

let Assert f b str =
  try
    let a = f()
    if a = b then
      printfn "Successfull for %s" str
      a = b
    else
      printfn "Got %O expected %O for %s" a b str
      a = b
  with
  | :? System.ArgumentException as e ->
    printfn "%O" e
    false
  | Failure(msg) ->
    printfn "%O" msg
    false
  | _ as x ->
    printfn "%O" x
    false

// Create player
let p1 = new Player("Bo", 1, false)

// Create card (11 = Jack) of Hearts
let c1 = new Card(11, Hearts)

// Add card to hand
p1.hand.draw c1

let t1 = Assert ( fun () -> p1.score ) 10 "Card create + Player score test 01";

let c2 = new Card(2, Diamonds)
p1.hand.draw c2

let t2 = Assert ( fun () -> p1.score ) 12 "Card create + Player score test 02";

let tests = [
    t1;
    t2;
]

// Match all tests
Assert ( fun () -> List.forall ( fun x -> x = true ) tests = true ) true "All tests"