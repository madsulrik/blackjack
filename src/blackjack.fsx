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
type Hand(hand) = 
  let mutable c:(Card array) = hand
  member this.cards with get() = c
  member this.drop() =
    let lastIndex = (Array.length c)-1 
    let card = c.[0]
    c <- c.[1..lastIndex]
    printfn "%A" (card.toString())
    card    
  member this.draw (card:Card) = c <- Array.append [|card|] c 
  member this.shuffle = 
    let len = Array.length c
    let testCard = Card(-1,Spades)
    let newHand = Array.create len testCard
    let test (card:Card) = card.toString()=testCard.toString()
    let rnd = System.Random()
    for i in 0..(len-1) do
      let mutable j = rnd.Next(0,len)
      while test c.[j] do
        j <- rnd.Next(0,len)
      newHand.[i] <- c.[j]
      c.[j] <- testCard
    c <- newHand
  member this.replace cards =
    c <- cards
  new()=Hand([||])
  
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
    let mutable es = 0
    for card in this.hand.cards do
      if card.value = 1 then es <- es + 1
      score <- score + card.value
    while es>0 && floor(float(21-score)/10.)>=1. do
      score <- score+10
      es <- es-1
    score
  member this.isBusted()=(this.score>21)
  new(name, index)=
    Player(name, index, false)

///
///
///
type Game(dealer, players, numberOfPlayers) =
  let s = new Hand()
  member this.dealer = dealer
  member this.players = players
  member this.numberOfPlayers = numberOfPlayers
  member this.stack = s