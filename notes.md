#Klasser

- Game (dealer,player,numberOfPlayers)
  * val dealer of Player
  * val players of Player list
  * val numberOfPlayers of int
  * val stack of Hand
  * val mutable current of ref
 
- Player (name,index,AI)
  * val name of string
  * val index of int
  * val hand of Hand
  * val AI of bool
  * fun isBusted

- Hand
  * val mutable cards of Card array
  * fun drop
  * fun draw
  * fun shuffle (maybeeeee)

- Card
  * val value of int
  * val suit of Suits
  * fun toString

#Enum

- Suits
  * Spades (♠)
  * Hearts (♥)
  * Diamonds (♦)
  * Clubs (♣)

#Konsol-kommandoer (ingame)

- "Hit"
  * Draw card to current player
  
- "Stand"
  * Next player


#Funktioner

  
    
    ALGORITHM drawCard game player
      player.hand.draw game.stack.drop
      
  
  
    
    ALGORITHM printBoard game
      let mutable maxCards = 0
      for i=0 to game.numberOfPlayers - 1
      ???
      ––––––––––––––––––– –––––––––––––––––– –––––––––––––––––– 
      |     Player 1    | |    Player 2    | |    Player 3    | 
      | - - - - - - - - | |- - - - - - - - | |- - - - - - - - | 
      |      ♠5 ♠8      | |      ♥K        | |    ♦4 ♣7 ♦A    | 
      ––––––––––––––––––– –––––––––––––––––– –––––––––––––––––– 
      ––––––––––––––––––– –––––––––––––––––– –––––––––––––––––– 
      |     Player 4    | |    Player 5    | |     Dealer     | 
      | - - - - - - - - | |- - - - - - - - | |- - - - - - - - | 
      |      ♥Q ♠2      | |     ♥J ♥K      | |                | 
      ––––––––––––––––––– –––––––––––––––––– –––––––––––––––––– 
      
  
  
      
      ALGORITHM menu()
        let rec numPlayers() =
          printfn "Number of players?"
          let input = ReadLine()
          if isNumeric(input) || input < 1 || input > 5 then
            numPlayers()
          else
            input
        let numberOfPlayers = numPlayers()
      
        let mutable players:Player = [||]
        for i=0 to numberOfPlayers - 1 do
          let mutable name = ""
          while validate_name(name) <> true do
            printf "Player %d's name is: " (i+1)
            name <- ReadLine()
          let mutable input = ""
          while validate_AI(input) <> true do
            printf "Player %d's is AI? (yes/no): " (i+1)
            input <- ReadLine()
          let AI = (input = "yes")
          players <- Array.append players (new Player(input,i,AI))
        let dealer = new Player("Dealer",-1,true)
      
        main (new Game(dealer,players,numberOfPlayers))
        
  
  
      
      ALGORITHM main(game)
        printBoard game
        for i=0 to game.numberOfPlayers - 1
          let mutable command = ""
          while command <> "Stand"
            command <- ReadLine()
            if command <> "Hit"
              drawCard game game.players[i]
              printBoard game
        
        dealerAI()
        winnerScreen
        Readlin() |> ignore //for wait
        call menu
        

