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
    let (player, _, _, _) = get_player st current_better in 
    let current_cash = player |> get_cash in 
    (* if current_cash = 0 then 
       begin 
        ANSITerminal.(print_string [red ]
                        ("\n\nPlayer "^(string_of_int current_better)^", you have run out of cash.\n"));
       end 
       else *)
    ANSITerminal.(print_string [green]
                    ("\n\nPlayer "^(string_of_int current_better)^", enter your bet.\n"));
    print_endline ("You have $"^(string_of_int current_cash)^".");
    print_string  "> ";
    match read_line () with
    | exception e -> st
    | bet -> begin
        try
          if (int_of_string bet) > current_cash then (
            (* print_endline ("Cash: "^(string_of_int current_cash)); *)

            ANSITerminal.(print_string [red] ("You can't afford to bet that much."));
            betting_phase st current_better player_count)
          else
            let st' = set_bet st (int_of_string bet) current_better in 
            betting_phase st' (current_better + 1) player_count
        with e -> begin
            ANSITerminal.(print_string [red]
                            ("\nNot a valid int, try again."));
            betting_phase (st) (current_better) (player_count)
          end
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
    let (dealer, busted, hard) = st.house in
    if get_count dealer > 21 then 
      if hard then
        ( ANSITerminal.(print_string [green]
                          "\n\nThe dealer busted!\n");
          playing_phase (bust_player st 0) (current_player - 1) player_count)
      else playing_phase (make_dealer_hard st) (current_player) player_count
    else if get_count dealer <= 16 then (
      ANSITerminal.(print_string [blue ]
                      ("\n\nThe dealers hits on "^(dealer |> get_count |> string_of_int)^".\n"));
      playing_phase (st |> dealer_hit) (current_player) player_count)
    else(
      ANSITerminal.(print_string [blue ]
                      ("\n\nThe dealers stands on "^(dealer |> get_count |> string_of_int)^".\n"));
      playing_phase (st) (current_player - 1) player_count)
  else
    let (player, _, _, hard) = get_player st current_player in 
    let count = get_count player in 
    if count > 21 then begin
      if hard then (
        ANSITerminal.(print_string [red]
                        ("\n\nPlayer "^(string_of_int current_player)^" busted.\n"));
        playing_phase (bust_player st current_player) (current_player - 1) player_count)
      else playing_phase (make_player_hard st player.id) (current_player) player_count 
    end
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

let rec find_busted (st : State.t) : player_id list =
  let busted = List.filter (fun (_, _, bust, _) -> bust) st.players in 
  List.map (fun (p, _, _, _) -> p.id) busted

let rec find_not_busted (st : State.t) : player_id list = 
  let not_busted = List.filter (fun (_, _, bust, _) -> (not bust)) st.players in 
  List.map (fun (p, _, _, _) -> p.id) not_busted

let rec dealer_busted (st : State.t) : bool =
  let (_, bust, _) = st.house in 
  bust

let blackjack (st : State.t) (p_id : player_id) : bool =
  let (player, _, _, _) = get_player st p_id in 
  if get_count player == 21 then true else false

let player_win (st : State.t) (p_id : player_id) : State.t =
  let (player, _, _, _) = get_player st p_id in  
  let new_cash = (get_cash player) + (get_bet st p_id) in 
  change_money st p_id new_cash

let player_loss (st : State.t) (p_id : player_id) : State.t =
  let (player, _, _, _) = get_player st p_id in 
  let new_cash = (get_cash player) - (get_bet st p_id) in 
  change_money st p_id new_cash

let rec players_all_win (st : State.t) (p_ids : player_id list) : State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  players_all_win (player_win st h) t

let rec players_all_lose (st : State.t) (p_ids : player_id list) : State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  players_all_lose (player_loss st h) t

(* assumes house not busted *)
(* assumes player not busted *)
let did_win (st : State.t) (p_id : player_id) : bool =
  let (h, _, _) = st.house in
  let (player, _, _, _) = get_player st p_id in 
  (get_count h < get_count player)

let rec who_won (st : State.t) (not_busted : player_id list) (winners : player_id list) : player_id list = 
  match not_busted with
  | [] -> winners
  | h :: t -> begin 
      if (did_win st h) then
        who_won st t (h :: winners)
      else 
        who_won st t winners
    end

let rec who_lost (st : State.t) (busted : player_id list) (losers : player_id list) : player_id list = 
  match busted with
  | [] -> losers
  | h :: t -> begin 
      if not (did_win st h) then
        who_lost st t (h :: losers)
      else 
        who_lost st t losers
    end

let rec moving_money_phase (st : State.t) : State.t =
  if (dealer_busted st) then
    let not_busted = find_not_busted st in
    let busted = find_busted st in 
    let st' = players_all_win st not_busted in
    let st'' = players_all_lose st' busted in
    st''
  else
    let busted = find_busted st in
    let st' = players_all_lose st busted in
    let not_busted = find_not_busted st' in 
    let winners = who_won st' not_busted [] in 
    let losers = who_lost st' not_busted [] in
    let st'' = players_all_lose st' losers in
    let st''' = players_all_win st'' winners in
    st'''

let rec game_body (st : State.t) : State.t = 
  let len = List.length st.players in 
  (* betting *)
  let st' = betting_phase st 1 len in
  (* dealing *)
  let st'' = dealing_phase st' 0 len in 
  (* playing *)
  print_dealer st'';
  let st''' = playing_phase st'' len len in 
  (* moving money *)
  let st'''' = moving_money_phase st''' in 
  (* reset phase *)
  let st''''' = reset_round st'''' in 
  game_body st'''''


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