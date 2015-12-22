/// <summary>Enumeration representing card-suits</summary>
type Suits = Spades | Hearts | Diamonds | Clubs

/// <summary>Card is an object representing af card, with value and suit</summary>
/// <param name="value">Integer representing the card value. 1 is A, 11-13 are 
/// picturecards</param>
/// <param name="suit">Suits enumeration representing the suit of the card</param>
type Card(value,suit) =
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

/// <summary>Hand is an object representing a players hand. A Hand-object can 
/// draw (Hand.draw card) or drop (Hand.drop) a card. The hand can be shuffled 
/// (Hand.shuffle) and replaced by a new Card Array (Hand.replace cards) </summary>
/// <param name="hand">Card Array that represents the cards on the hand (optional)</param>
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
  new()=
    Hand([||])
  
/// <summary>Player is an object representing a Player (AI or human). A Player 
/// has a name, index (representing order of game-flow), a hand cards, and a 
/// score.
/// The score is updated when called, and are to determine if a player has bust
/// (over 21 points).</summary>
/// <param name="name">String representing the name of the player</param>
/// <param name="index">Integer represebting the index of the position in 
/// Player Array in Game object</param>
/// <param name="AI">Boolean representing whether a player is a NPC 
/// (Non-Playable Character) or PC (Playable Character)</param>
type Player(name,index,AI) =
  let h = new Hand()
  member this.name:string = name
  member this.index:int = index
  member this.hand = h
  member this.AI:bool = AI
  member this.score =
    let mutable score = 0
    let mutable es = 0
    for card in this.hand.cards do
      if card.value = 1 then es <- es + 1
      score <- score + card.value
    while es>0 && floor(float(21-score)/10.)>=1. do
      score <- score+10
      es <- es-1
    score
  member this.isBusted() = (this.score>21)
  new(name, index) =
    Player(name, index, false)

/// <summary>Game is an object which is used to contain a collection of data,
/// for which is used in-game, like players, a dealer, and a card stack.</summary>
/// <param name="dealer">A Player object representing a dealer. Player.AI must be
/// set to true.</param>
/// <param name="players">An Array of Player objects.</param>
type Game(dealer,players) =
  let s = new Hand()
  member this.dealer = dealer
  member this.players = players
  member this.numberOfPlayers = Array.length players
  member this.stack = s