#load "./blackjack.fsx"
#load "./headers.fsx"
open Blackjack
open Headers

/// <summary>Validates if string length is greater than 0 and less than 25,
///   if that is true, then str is a valid name
/// </summary>
/// <param name="str:string">Input string</param>
let validate_name str = String.length str > 0 && String.length str < 25

/// <summary>Validates if str is either "y" or "n"</summary>
/// <param name="str:string">The input string</param>
let validate_yn str = (str = "y" || str = "n")

/// <summary>Prints a game's scoreboard to the console</summary>
/// <param name="game:Game">The game to print as a Game object</param>
/// <returns>unit</returns>
let printScoreboard (game:Game) =
  clear()
  write mainHeader
  for player in game.players do
    player.scoreboard()
  game.dealer.scoreboard()
  System.Console.WriteLine ""

/// <summary>Adds a selection cursor in the console,
///   under the selected player
/// </summary>
/// <param name="player:Player">player:Player</param>
/// <returns>unit</returns>
let selectPlayer (player:Player) = 
  let c = (System.Console.CursorLeft,System.Console.CursorTop)
  let fill = "|XXXXXXXXXXXXXXXXXXXXXXX|"
  let x = (String.length fill)*(player.index%3) + (player.index)%3
  let y = (8*(player.index/3)+11)
  System.Console.SetCursorPosition(x,y)
  System.Console.Write fill
  System.Console.SetCursorPosition c

/// <summary>Performs AI operations, for the selected player</summary>
/// <param name="game:Game">The game object</param>
/// <param name="player:Player"></param>
/// <returns>unit</returns>
let AI (game:Game) (player:Player) =
  let mutable bestValue = 0
  for player in game.players do
    let score = player.score
    if score<22 && score>bestValue then bestValue <- score
  System.Threading.Thread.Sleep(500)
  let mutable IDare = true
  while IDare do 
    let diff = max 0 (21 - player.score)
    let es = Array.filter (fun (x:Card)->x.value=1) game.stack.cards |> Array.length
    let p = Array.filter (fun (x:Card)->x.value<=diff) game.stack.cards |> Array.length
    let pos x = if x < 0 then -x else x 
    if p > 30 || es > 0 && p > 20 || p > 25 && pos (bestValue-player.score) < 4 then
      System.Threading.Thread.Sleep((52-p)*60)
      game.draw player
    else
      IDare <- false
    printScoreboard game
    selectPlayer player
    
///
///
///
let rec main (game:Game) =
  for player in game.players do
    printScoreboard game
    selectPlayer player
    if player.AI=true then 
      AI game player
    else
      let mutable command = ""
      while command <> "stand" && player.score < 21 do
        command <- readln()
        if command = "hit" then
          game.draw player
        printScoreboard game
        selectPlayer player
  AI game game.dealer
  let mutable winners = [||]:(Player array)
  for player in game.players do
    if player.isBusted()=false && player.score > game.dealer.score then
      if (player.score=21 && game.dealer.score=21 && Array.length player.hand.cards=2 
      && Array.length player.hand.cards = Array.length game.dealer.hand.cards)=false then
      
        winners <- Array.append winners [|player|]
  if Array.length winners = 0 && game.dealer.score <= 21 then
    writeln "Dealer was too good!"
  elif Array.length winners = 0 then
    writeln "No winners!"
  else
    writeln "And the winner(s) is:"
  for winner in winners do
    writeln (sprintf " • %s (%d)" winner.name winner.score)
  write "New round (y/n)?"
  let mutable input = readln()
  while validate_yn input = false do
    clear()
    write header
    write "New round (y/n)?"
    input <- readln()
  if input = "y" then
    for player in game.players do player.hand.replaceWith [||]
    game.dealer.hand.replaceWith [||]
    main(Game(game.dealer,game.players))

///
///
///
let setup() =
  clear()
  write header
  let rec nop() =
    clear()
    write header
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
      write header
      write (sprintf "Player %d's name is: " (i+1))
      name <- readln()
      writeln ""
    let mutable input = ""
    while validate_yn input = false do
      clear()
      write header
      write (sprintf "Is player %d a human (y/n): " (i+1))
      input <- readln()
      writeln ""
    let AI = input = "n"
    if AI then name <- name + "(AI)"
    players <- Array.append players [|Player(name,i,AI)|]
  clear()
  let dealer = Player("Dealer",numberOfPlayers,true)
  main(Game(dealer,players))

///
///
///
let rec menu() =
  clear()
  write menuHeader
  let input = System.Console.ReadKey()
  System.Threading.Thread.Sleep(50)
  match input.KeyChar with
  | '1' -> 
    setup()
    menu()
  | '2' -> 
    clear()
    exit 0
  | _ -> menu()
menu()