open State
open Deck
open Player
open Command

let symbol_of_suit (s : suit) : string = 
  match s with 
  | Heart -> "    ♥    "
  | Diamond -> "    ♦    "
  | Spade -> "    ♠    "
  | Club -> "    ♣    "

let left_face_of_value (v : int) : string =
  if v = 11 then "J        "
  else if v = 12 then "Q        "
  else if v = 13 then "K        "
  else if v = 1 then "A        "
  else if v = 10 then "10       "
  else string_of_int v ^ "       "

let right_face_of_value (v : int) : string =
  if v = 11 then "        J"
  else if v = 12 then "        Q"
  else if v = 13 then "        K"
  else if v = 1 then "        A"
  else if v = 10 then "       10"
  else "       " ^ string_of_int v 

let print_card (st : State.t) (card : card) (i : int) : unit =
  if i = 0 then print_endline "┌─────────┐"
  else if i = 1 then print_endline ("│" ^ left_face_of_value card.value ^ "│")
  else if i = 4 then print_endline ("│" ^ symbol_of_suit card.suit ^ "│")
  else if i = 7 then print_endline ("│" ^ right_face_of_value card.value ^ "│")
  else if i = 8 then print_endline "└─────────┘"
  else print_endline "│         │"

let print_hidden_card : unit =
  begin 
    print_endline "┌─────────┐";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "└─────────┘";
  end

let rec print_hand (st : State.t) (hand : card list) (i : int) : unit =
  (* if i = List.length hand then false else *)
  match hand with 
  | [] -> ()
  | h :: t -> print_card st h i; print_hand st t i

let print (st : State.t) (id: player_id) : unit =
  let player = get_player st id in
  let hand = get_hand player in
  for j = 0 to 8 do
    (print_hand st hand j) done;;

let print_dealer (st : State.t) : unit =
  let player = get_player st 0 in
  let hand = get_hand player in
  match hand with
  | h :: t -> begin print_hidden_card; 
      for j = 0 to 8 do
        (print_hand st t j) done end
  | _ -> ()


let rec betting_phase (st: State.t) (current_better: player_id) (player_count: int) : State.t = 
  if current_better > player_count then
    st else (
    let player = get_player st current_better in 
    let current_cash = player |> get_cash |> string_of_int in 
    ANSITerminal.(print_string [green]
                    ("\n\nPlayer "^(string_of_int current_better)^", enter your bet.\n"));
    print_endline ("You have $"^current_cash^".");
    print_string  "> ";
    match read_line () with
    | exception e -> st
    | bet -> begin 
        if bet > current_cash then (
          ANSITerminal.(print_string [red] ("You can't bet that much."));
          betting_phase st current_better player_count)
        else
          let st' = set_bet st (int_of_string bet) current_better in 
          betting_phase st' (current_better + 1) player_count
      end
  )

let rec dealing_phase (st: State.t) (current_player: player_id) (player_count: int) : State.t = 
  if current_player > player_count then
    st else if current_player = 0 then
    dealing_phase (st |> dealer_hit |> dealer_hit) (current_player + 1) player_count
  else
    dealing_phase (st |> hit current_player |> hit current_player) (current_player + 1) player_count

let rec playing_phase (st: State.t) (current_player: player_id) (player_count: int) : State.t =
  if current_player > player_count then
    st 
  else if current_player = 0 then
    let (dealer, busted) = st.house in
    (* if get_count dealer > 21 then *)

    if get_count dealer <= 16 then
      playing_phase (st |> dealer_hit) (current_player) player_count
    else 
      playing_phase (st) (current_player + 1) player_count
  else
    let player = get_player st current_player in 
    let count = get_count player in 
    ANSITerminal.(print_string [cyan] ("Current player: "^(string_of_int current_player)^"\n"));
    ANSITerminal.(print_string [blue] ("Your count is "^(string_of_int count)^".\nDo you want to hit or stand?\n"));
    print_string "> ";
    let input = read_line () in
    try let parsed = parse input in
      match parsed with
      | Hit -> playing_phase (hit current_player st) (current_player) player_count
      | Stand -> playing_phase st (current_player + 1) player_count
    with Malformed -> (
        print_endline "Not a valid command";
        playing_phase st (current_player) player_count
      )

let rec game_body (st : State.t) : State.t = 
  let len = List.length st.players in 
  (* betting *)
  let st' = betting_phase st 1 len in
  (* dealing *)
  let st'' = dealing_phase st' 0 len in 
  (* playing *)
  let st''' = playing_phase st'' 0 len in  
  st'''

(* moving money *)


let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
  match read_line () with
  | exception e -> ()
  | player_count -> (ignore (new_game (int_of_string player_count) |> game_body);)

(* Start the game. *)
let () = main ()