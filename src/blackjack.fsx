///
///
///
type Suits = Spade | Heart | Diamond | Club

///
///
///
type Card(value, suit) =
  member this.value:int = value
  member this.suit:Suits = suit
  member this.toString() =
    let suit =
      match this.suit with
      | Spade -> "♠"
      | Heart -> "♥"
      | Diamond -> "♦"
      | Club -> "♣"
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
  member this.cards:(Card array) = [||]
  member this.drop() = this.cards
  member this.draw card = Array.append this.cards [|card|]
  member this.shuffle() = this.cards

///
///
///
type Player(name, index, AI) =
  member this.name:string = name
  member this.index:int = index
  member this.hand:Hand = new Hand()
  member this.AI:bool = AI
  member this.isBusted() = false

///
///
///
type Game(dealer, players, numberOfPlayers) =
  member this.dealer = dealer
  member this.players = players
  member this.numberOfPlayers = numberOfPlayers
  member this.stack = new Hand()
  
let card = new Card(1, Heart)
printfn "%s" (card.toString())