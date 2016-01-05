#load "./blackjack.fsx"
open Blackjack
// Skal compiles med -d:TEST

let Assert f b str =
    try
        let a = f()
        if a = b then
            printfn "Successful for %s" str
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
    printfn "Successful for Hand shuffle test 02"
else
    printfn "Expected p1 != h1, but got different for Hand shuffle test 02"

let h2 = new Hand([|c1; c2; c3|])
h2.drop

let t8 = h2 <> h1
if t8 then
    printfn "Successful for Hand drop test 01"
else
    printfn "Expected h2 != h1, but got different for Hand drop test 01"

let tests = [
    t1;
    t2;
    t3;
    t4;
    //t5;
    t6;
    t7;
]

// Match all tests
Assert ( fun () -> List.forall ( fun x -> x = true ) tests = true ) true "All tests"

// Hand.draw
let handTest1 = Hand()
handTest1.draw(Card(1, Spades))

// Hand.drop
let handTest2 = Hand()
handTest2.draw(Card(1, Hearts))
handTest2.drop
handTest2.draw(Card(1, Spades))

// Hand.replaceWith
let handTest3 = Hand()
handTest3.draw(Card(1, Hearts))
handTest3.replaceWith [|Card(1, Spades)|]

// Hand.shuffle
let handTest4 = Hand()
handTest4.draw(Card(1, Hearts))
handTest4.draw(Card(1, Spades))
handTest4.draw(Card(1, Diamonds))
handTest4.draw(Card(1, Clubs))
handTest4.shuffle()

let card1 = handTest4.cards.[0]
let card2 = handTest4.cards.[1]
let card3 = handTest4.cards.[2]
let card4 = handTest4.cards.[3]

printfn "Hand.draw %A" (handTest1.cards.[0].value = 1 && handTest1.cards.[0].suit = Spades)
printfn "Hand.drop %A" (handTest2.cards.[0].value = 1 && handTest2.cards.[0].suit = Spades)
printfn "Hand.replaceWith %A" (handTest3.cards.[0].value = 1 && handTest3.cards.[0].suit = Spades)
printfn "Hand.shuffle %A" (card1.value = 1 && card1.suit = Hearts &&
                           card2.value = 1 && card2.suit = Spades &&
                           card3.value = 1 && card3.suit = Clubs &&
                           card4.value = 1 && card4.suit = Diamonds)

