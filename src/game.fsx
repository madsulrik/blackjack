#load "./blackjack.fsx"
open Blackjack

///
///
///
let write (str:string) = System.Console.Write str
let writeln (str:string) = System.Console.WriteLine str
let readln() = System.Console.ReadLine()
let clear() = System.Console.Clear()
let validate_name str = String.length str > 0 && String.length str < 25
let validate_AI str = (str = "y" || str = "n")

///
///
///
let printScoreboard (game:Game) =
  clear()
  for player in game.players do
    player.scoreboard()
  game.dealer.scoreboard()
  System.Console.WriteLine ""

///
///
///
let AI (game:Game) (player:Player) =
  let mutable bestValue = 0
  for player in game.players do
    let score = player.score
    if score<22 && score>bestValue then bestValue <- score
    
    
///
///
///
let rec main (game:Game) =
  for player in game.players do
    printScoreboard game
    let selectPlayer() = 
      let c = (System.Console.CursorLeft,System.Console.CursorTop)
      let fill = "|XXXXXXXXXXXXXXXXXXXXXXX|"
      let x = (String.length fill)*(player.index%3) + (player.index)%3
      let y = (5*(player.index/3)+6)
      System.Console.SetCursorPosition(x,y)
      System.Console.Write fill
      System.Console.SetCursorPosition c
    selectPlayer()
    if player.AI=true then 
      AI game player
    else
      let mutable command = ""
      while command <> "stand" && player.score < 21 do
        command <- readln()
        if command = "hit" then
          game.draw player
        printScoreboard game
        selectPlayer()
    AI game game.dealer
  ()

///
///
///
let setup() =
  clear()
  let rec nop() =
    clear()
    write "Number of players (1-5): "
    let c = 
      try
        readln() |> int
      with
      | _ -> 0
    if c < 1 || c > 5 then nop() else int c
  let numberOfPlayers = nop()
  let mutable players:(Player array) = [||]
  for i=0 to numberOfPlayers-1 do
    let mutable name = ""
    while validate_name name = false do
      clear()
      write (sprintf "Player %d's name is: " (i+1))
      name <- readln()
      writeln ""
    let mutable input = ""
    while validate_AI input = false do
      clear()
      write (sprintf "Is player %d a human (y/n): " (i+1))
      input <- readln()
      writeln ""
    let AI = input = "n"
    players <- Array.append players [|Player(name,i,AI)|]
  clear()
  let dealer = Player("Dealer",numberOfPlayers,true)
  main(Game(dealer,players))

///
///
///
let rec menu() =
  clear()
  match readln() with
  | "1" -> setup()
  | "0" -> exit 0
  | _ -> menu()
menu()