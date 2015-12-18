///
///
///
type Suits = Spades | Hearts | Diamonds | Clubs

///
///
///
type Card(value, suit) =
  member this.value:int = value
  member this.suit:Suits = suit
  member this.toString() =
    let suit =
      match this.suit with
      | Spades -> "♠"
      | Hearts -> "♥"
      | Diamonds -> "♦"
      | Clubs -> "♣"
    let value =
      match this.value with
      | 1 ->  "A"
      | 11 -> "J"
      | 12 -> "Q"
      | 13 -> "K"
      | x -> sprintf "%d" x
    sprintf "%s%s" suit value

///
///
///
type Hand() = 
  let mutable hand:(Card array) = [||]
  member this.cards with get () = hand
  member this.drop() =
    let lastIndex = (Array.length hand)-1 
    let card = hand.[0]
    hand <- hand.[1..lastIndex]
    printfn "%A" (card.toString())
    card    
  member this.draw (card:Card) = hand <- Array.append [|card|] hand 
  member this.shuffle() = 
    let len = Array.length hand
    let testCard = new Card(-1,Spades)
    let newHand = Array.create len testCard
    let test (card:Card) = card.toString()=testCard.toString()
    let rnd = System.Random()
    for i in 0..(len-1) do
      let mutable j = rnd.Next(0,len)
      while test hand.[j] do
        j <- rnd.Next(0,len)
      newHand.[i] <- hand.[j]
      hand.[j] <- testCard
    hand <- [||]//newHand

///
///
///
type Player(name, index, AI) =
  let h = new Hand()
  member this.name:string = name
  member this.index:int = index
  member this.hand with get() = h
  member this.AI:bool = AI
  member this.score =
    //implement 1 or 11 with A
    let mutable score = 0
    for card in this.hand.cards do
      score <- score + card.value
    score
  member this.isBusted() = (this.score>=21)
  new(name, index) =
    Player(name, index, false)

let printHand (hand:Hand) =
  let len = Array.length hand.cards
  for card in hand.cards do
    printf "%s " (card.toString())
  if len = 0 then printf "No cards!"
  printfn ""

///
///
///
type Game(dealer, players, numberOfPlayers) =
  let s = new Hand()
  member this.dealer = dealer
  member this.players = players
  member this.numberOfPlayers = numberOfPlayers
  member this.stack = s
