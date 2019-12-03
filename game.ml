open State
open Deck
open Player
open Command

let rec betting_phase (st: State.t) (current_better: player_id) (player_count: int) : State.t =
  (* print_int player_count;
     print_int current_better; *)
  if current_better > player_count then
    st 
  else (
    let (player, _, _) = get_player st current_better in 
    let current_cash = player |> get_cash in 
    ANSITerminal.(print_string [green]
                    ("\n\nPlayer "^(string_of_int current_better)^", enter your bet.\n"));
    print_endline ("You have $"^(string_of_int current_cash)^".");
    print_string  "> ";
    match read_line () with
    | exception e -> st
    | bet -> begin 
        if (int_of_string bet) > current_cash then (
          (* print_endline ("Cash: "^(string_of_int current_cash)); *)

          ANSITerminal.(print_string [red] ("You can't afford to bet that much."));
          betting_phase st current_better player_count)
        else
          let st' = set_bet st (int_of_string bet) current_better in 
          betting_phase st' (current_better + 1) player_count
      end
  )

let rec dealing_phase (st: State.t) (current_player: player_id) (player_count: int) : State.t = 
  if current_player > player_count then
    st else begin 
    if current_player = 0 then (
      dealing_phase (st |> dealer_hit |> dealer_hit) (current_player + 1) player_count)
    else
      dealing_phase (st |> hit current_player |> hit current_player) (current_player + 1) player_count
  end

let rec playing_phase (st: State.t) (current_player: player_id) (player_count: int) : State.t =
  if current_player < 0 then
    st 
  else if current_player = 0 then
    let (dealer, busted) = st.house in
    if get_count dealer > 21 then (
      ANSITerminal.(print_string [green]
                      "\n\nThe dealer busted!\n");
      playing_phase (bust_player st 0) (current_player - 1) player_count)
    else if get_count dealer <= 16 then (
      ANSITerminal.(print_string [blue ]
                      ("\n\nThe dealers hits on "^(dealer |> get_count |> string_of_int)^".\n"));
      playing_phase (st |> dealer_hit) (current_player) player_count)
    else(
      ANSITerminal.(print_string [blue ]
                      ("\n\nThe dealers stands on "^(dealer |> get_count |> string_of_int)^".\n"));
      playing_phase (st) (current_player - 1) player_count)
  else
    let (player, _, _) = get_player st current_player in 
    let count = get_count player in 
    if count > 21 then (
      ANSITerminal.(print_string [red]
                      ("\n\nPlayer "^(string_of_int current_player)^" busted.\n"));
      playing_phase (bust_player st current_player) (current_player - 1) player_count)
    else begin
      ANSITerminal.(print_string [cyan] ("Current player: "^(string_of_int current_player)^"\n"));
      print st current_player;
      ANSITerminal.(print_string [blue] ("Your count is "^(string_of_int count)^".\nDo you want to hit or stand?\n"));
      print_string "> ";
      let input = read_line () in
      try let parsed = parse input in
        match parsed with
        | Hit -> playing_phase (hit current_player st) (current_player) player_count
        | Stand -> playing_phase st (current_player - 1) player_count
      with Malformed -> (
          print_endline "Not a valid command";
          playing_phase st (current_player) player_count
        )
    end

let rec find_winner (st : State.t) (players : player_id list) (max_score : int) (current_winner : player_id list) : player_id list =
  match players with 
  | [] -> current_winner
  | h :: t -> let (player, _, _) = get_player st h in 
    if player.count > max_score && player.count <= 21 then find_winner st t player.count [h]
    else if player.count = max_score && player.count <= 21 then find_winner st t player.count (h :: current_winner)
    else find_winner st t max_score current_winner

let rec moving_money (st : State.t) (current_player : player_id) (player_count : int) : State.t =
  if current_player < 0 then st 
  else begin
    let (player, bet, bust) = get_player st current_player in
    if bust then
      let st' = (change_money st current_player ((get_cash player) - bet)) in 
      moving_money st' (current_player - 1) player_count
    else let st' = change_money st current_player ((get_cash player) - bet) in 
      moving_money st' (current_player - 1) player_count
  end


let rec game_body (st : State.t) : State.t = 
  let len = List.length st.players in 
  (* betting *)
  let st' = betting_phase st 1 len in
  (* dealing *)
  let st'' = dealing_phase st' 0 len in 
  (* playing *)
  print_dealer st'';
  let st''' = playing_phase st'' len len in  
  st'''

(* moving money *)


let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Blackjack.\n");
  print_endline "Please enter the number of players.\n";
  print_string  "> ";
  let count = read_line () in 
  print_endline "Please enter a (small) integer shuffle seed.\n";
  print_string  "> ";
  let seed = read_line () in 
  match count, seed with
  | exception e -> ()
  | player_count, real_seed -> (ignore (new_game (int_of_string player_count) (int_of_string seed) |> game_body);)

(* Start the game. *)
let () = main ()