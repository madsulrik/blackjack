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

let t3 = Assert ( fun () -> p1.isBusted() ) false "Player isBusted test 01";

let c3 = new Card(11, Clubs)
p1.hand.draw c3

let t4 = Assert ( fun () -> p1.isBusted() ) true "Player isBusted test 02";

//let t5 = Assert ( fun () -> p1.scoreboard() ) () "Player scoreboard test 01";

//let i = System.Console.ReadLine()

// Not shuffled
let t6 = Assert ( fun () -> p1.hand ) p1.hand "Hand shuffle test 01";

let h1 = new Hand([|c1; c2; c3|])

// Shuffled
p1.hand.shuffle()

let t7 = p1.hand <> h1
if t7 then
    printfn "Successfull for Hand shuffle test 02"
else
    printfn "Expected p1 != h1, but got different for Hand shuffle test 02"

let h2 = new Hand([|c1; c2; c3|])
h2.drop

let t8 = h2 <> h1
if t8 then
    printfn "Successfull for Hand drop test 01"
else
    printfn "Expected h2 != h1, but got different for Hand drop test 01"

let tests = [
    t1;
    t2;
    t3;
    t4;
    t5;
    t6;
    t7;
]

// Match all tests
Assert ( fun () -> List.forall ( fun x -> x = true ) tests = true ) true "All tests"