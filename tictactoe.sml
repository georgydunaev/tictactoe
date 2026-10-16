(* ========================================================================= *)
(* Tic-Tac-Toe in Poly/ML (Standard ML)                                      *)
(* ========================================================================= *)

structure TicTacToe =
struct

  datatype player = X | O
  datatype square = Empty | Mark of player
  type board = square list

  fun otherPlayer X = O
    | otherPlayer O = X

  fun playerToString X = "X"
    | playerToString O = "O"

  val emptyBoard : board = List.tabulate (9, fn _ => Empty)

  fun getSquare (b: board, i: int) : square =
    List.nth (b, i)

  fun setSquare (b: board, i: int, sq: square) : board =
    let
      fun replace ([], _) = []
        | replace (_ :: xs, 0) = sq :: xs
        | replace (x :: xs, n) = x :: replace (xs, n - 1)
    in
      replace (b, i)
    end

  (* Available 0-indexed positions *)
  fun availableMoves (b: board) : int list =
    let
      fun collect (i, []) = []
        | collect (i, Empty :: rest) = i :: collect (i + 1, rest)
        | collect (i, _ :: rest)     = collect (i + 1, rest)
    in
      collect (0, b)
    end

  val winningLines = [
    (0, 1, 2), (3, 4, 5), (6, 7, 8), (* rows *)
    (0, 3, 6), (1, 4, 7), (2, 5, 8), (* columns *)
    (0, 4, 8), (2, 4, 6)             (* diagonals *)
  ]

  fun checkWinner (b: board) : player option =
    let
      fun checkLine (i, j, k) =
        case (getSquare (b, i), getSquare (b, j), getSquare (b, k)) of
           (Mark p1, Mark p2, Mark p3) =>
             if p1 = p2 andalso p2 = p3 then SOME p1 else NONE
         | _ => NONE

      fun scan [] = NONE
        | scan (line :: rest) =
            (case checkLine line of
               SOME p => SOME p
             | NONE => scan rest)
    in
      scan winningLines
    end

  fun isBoardFull (b: board) : bool =
    List.all (fn sq => sq <> Empty) b

  fun isGameOver (b: board) : bool =
    Option.isSome (checkWinner b) orelse isBoardFull b

  (* Board display: shows 1-9 for empty cells, X/O for occupied *)
  fun printBoard (b: board) =
    let
      fun cellStr i =
        case getSquare (b, i) of
           Empty => Int.toString (i + 1)
         | Mark p => playerToString p

      val r1 = " " ^ cellStr 0 ^ " | " ^ cellStr 1 ^ " | " ^ cellStr 2
      val sep = "---+---+---"
      val r2 = " " ^ cellStr 3 ^ " | " ^ cellStr 4 ^ " | " ^ cellStr 5
      val r3 = " " ^ cellStr 6 ^ " | " ^ cellStr 7 ^ " | " ^ cellStr 8
    in
      print ("\n" ^ r1 ^ "\n" ^ sep ^ "\n" ^ r2 ^ "\n" ^ sep ^ "\n" ^ r3 ^ "\n\n")
    end

  (* ----------------------------------------------------------------------- *)
  (* Minimax AI (Unbeatable)                                                 *)
  (* ----------------------------------------------------------------------- *)

  fun minimax (b: board, current: player, aiPlayer: player, depth: int) : int =
    case checkWinner b of
       SOME winner =>
         if winner = aiPlayer then (10 - depth) else (depth - 10)
     | NONE =>
         if isBoardFull b then 0
         else
           let
             val moves = availableMoves b
             fun evalMove m =
               let
                 val nb = setSquare (b, m, Mark current)
               in
                 minimax (nb, otherPlayer current, aiPlayer, depth + 1)
               end
             val scores = List.map evalMove moves
           in
             if current = aiPlayer then
               List.foldl Int.max (~1000) scores
             else
               List.foldl Int.min 1000 scores
           end

  fun findBestMove (b: board, aiPlayer: player) : int =
    let
      val moves = availableMoves b
      fun evalMove m =
        let
          val nb = setSquare (b, m, Mark aiPlayer)
          val score = minimax (nb, otherPlayer aiPlayer, aiPlayer, 1)
        in
          (m, score)
        end
      val scored = List.map evalMove moves
    in
      case scored of
         [] => 0
       | ((firstM, firstS) :: rest) =>
           #1 (List.foldl (fn ((m, s), (bestM, bestS)) =>
                            if s > bestS then (m, s) else (bestM, bestS))
                          (firstM, firstS) rest)
    end

  (* ----------------------------------------------------------------------- *)
  (* Input & Game Loop                                                       *)
  (* ----------------------------------------------------------------------- *)

  fun readLine () : string option =
    case TextIO.inputLine TextIO.stdIn of
       NONE => NONE
     | SOME s =>
         let
           val len = String.size s
         in
           if len > 0 andalso String.sub (s, len - 1) = #"\n" then
             SOME (String.substring (s, 0, len - 1))
           else
             SOME s
         end

  fun promptMove (current: player, b: board) : int option =
    let
      val moves = availableMoves b
      fun loop () =
        (print ("Player " ^ playerToString current ^ ", enter your move (1-9): ");
         case readLine () of
            NONE => NONE
          | SOME line =>
              (case Int.fromString line of
                  SOME num =>
                    let val idx = num - 1 in
                      if idx >= 0 andalso idx < 9 andalso List.exists (fn m => m = idx) moves then
                        SOME idx
                      else
                        (print "Invalid move! Spot is either out of range or already taken.\n";
                         loop ())
                    end
                | NONE =>
                    (print "Please enter a valid number from 1 to 9.\n";
                     loop ())))
    in
      loop ()
    end

  datatype game_mode = TwoPlayer | PlayerVsAI of player

  fun playGame (mode: game_mode) =
    let
      fun turn (b: board, current: player) =
        (printBoard b;
         case checkWinner b of
            SOME winner =>
              print ("Congratulations! Player " ^ playerToString winner ^ " wins!\n\n")
          | NONE =>
              if isBoardFull b then
                print "The game is a draw!\n\n"
              else
                let
                  val isAIMove =
                    case mode of
                       TwoPlayer => false
                     | PlayerVsAI aiP => current = aiP
                in
                  if isAIMove then
                    let
                      val _ = print ("Computer (" ^ playerToString current ^ ") is thinking...\n")
                      val m = findBestMove (b, current)
                      val nb = setSquare (b, m, Mark current)
                    in
                      print ("Computer (" ^ playerToString current ^ ") chose position " ^
                             Int.toString (m + 1) ^ ".\n");
                      turn (nb, otherPlayer current)
                    end
                  else
                    case promptMove (current, b) of
                       NONE => print "\nGame aborted.\n"
                     | SOME m =>
                         let
                           val nb = setSquare (b, m, Mark current)
                         in
                           turn (nb, otherPlayer current)
                         end
                end)
    in
      turn (emptyBoard, X)
    end

  fun promptMenu () : unit =
    let
      fun menuLoop () =
        (print "===================================\n";
         print "       TIC-TAC-TOE (Poly/ML)       \n";
         print "===================================\n";
         print "1. Two Players (Pass & Play)\n";
         print "2. Play against AI (You are X, AI is O)\n";
         print "3. Play against AI (AI is X, You are O)\n";
         print "4. Exit\n";
         print "Select an option (1-4): ";
         case readLine () of
            NONE => ()
          | SOME "1" => (playGame TwoPlayer; askReplay ())
          | SOME "2" => (playGame (PlayerVsAI O); askReplay ())
          | SOME "3" => (playGame (PlayerVsAI X); askReplay ())
          | SOME "4" => print "Thanks for playing! Goodbye.\n"
          | _ =>
              (print "Invalid selection. Please choose 1, 2, 3, or 4.\n\n";
               menuLoop ()))

      and askReplay () =
        (print "Play another game? (y/n): ";
         case readLine () of
            SOME ans =>
              if ans = "y" orelse ans = "Y" orelse ans = "yes" then
                menuLoop ()
              else
                print "Thanks for playing! Goodbye.\n"
          | NONE => ())
    in
      menuLoop ()
    end

  fun main () =
    promptMenu ()

end;

(* Run the game when launched via poly --script *)
val () = TicTacToe.main ();
