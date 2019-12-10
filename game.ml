open State
open Deck
open Player
open Command

(** [betting_phase st current_better player_count] is the state after each 
    player in [st] finishes setting valid bets. *)
let rec betting_phase (st: State.t) (current_better: player_id) 
    (player_count: int) : State.t =
  if current_better > player_count then
    st 
  else (
    let (player, _, _, _, _, _, _) = get_player st current_better in 
    let current_cash = player |> get_cash in 
    ANSITerminal.(print_string [green]
                    ("\n\nPlayer "^
                     (string_of_int current_better)^", enter your bet.\n"));
    print_endline ("You have $"^(string_of_int current_cash)^".");
    print_string  "> ";
    match read_line () with
    | exception e -> st
    | bet -> valid_bet st current_better player_count current_cash bet
  )

and valid_bet (st : State.t) (current_better : player_id) (player_count : int)
    (current_cash : int) (bet : string) =
  try
    if (int_of_string bet) > current_cash then (
      ANSITerminal.(print_string [red] 
                      ("You can't afford to bet that much."));
      betting_phase st current_better player_count)
    else
      let st' = set_bet st (int_of_string bet) current_better in 
      betting_phase st' (current_better + 1) player_count
  with e -> begin
      ANSITerminal.(print_string [red]
                      ("\nNot a valid int, try again."));
      betting_phase (st) (current_better) (player_count)
    end

(** [dealing_phase st current_player player_count] is the state after initial
    cards are dealt to each player. *)
let rec dealing_phase (st: State.t) (current_player: player_id) 
    (player_count: int) : State.t = 
  if current_player > player_count then
    st else begin 
    if current_player = 0 then (
      dealing_phase (st |> dealer_hit |> dealer_hit) (current_player + 1) 
        player_count)
    else
      dealing_phase (st |> hit current_player |> hit current_player) 
        (current_player + 1) player_count
  end

let rec splits_playing_phase (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) : State.t =
  if current_player <= 0 then
    st
  else
    let (player, bet, _, _,  _, _, split_hard) = get_player st current_player in
    if List.length player.split_hand = 0 then
      splits_playing_phase st (current_player - 1) player_count false
    else if List.length player.split_hand = 1 then
      splits_playing_phase (split_hit current_player st) (current_player) 
        player_count false
    else
      let count = get_split_count player in 
      if count > 21 then busted_bet st current_player player_count doubled 
          player split_hard
      else not_busted_bet st current_player player_count doubled 
          player split_hard count

and busted_bet (st: State.t) (current_player: player_id) (player_count: int) 
    (doubled : bool) (player : Player.t) (split_hard : bool): State.t =
  if split_hard then (print_split st current_player;
                      ANSITerminal.(print_string [red]
                                      ("\nPlayer "^
                                       (string_of_int current_player)^
                                       " busted.\n\n"));
                      splits_playing_phase (split_bust_player st current_player)
                        (current_player - 1) 
                        player_count false)
  else splits_playing_phase (make_player_split_hard st player.id) 
      (current_player) player_count false 

and not_busted_bet (st: State.t) (current_player: player_id) (player_count: int)
    (doubled : bool) (player : Player.t) (split_hard : bool) (count : int): 
  State.t =
  ANSITerminal.(print_string [cyan] ("Current player: "^
                                     (string_of_int current_player)^"\n"));
  if doubled then (print_split st current_player; splits_playing_phase st 
                     (current_player - 1) player_count false)
  else not_busted_bet_helper st current_player player_count doubled 
      player split_hard count

and not_busted_bet_helper (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) (player : Player.t) (split_hard : bool)
    (count : int) : State.t = 
  print_split st current_player;
  ANSITerminal.(print_string [blue] ("Your count is "^
                                     (string_of_int count)
                                     ^".\nDo you want to hit or stand?\n"
                                    ));
  print_string "> ";
  let input = read_line () in
  try let parsed = parse input in
    match parsed with
    | Hit -> splits_playing_phase (split_hit current_player st) (current_player) 
               player_count false
    | Stand -> splits_playing_phase st (current_player - 1) player_count false
    | _ ->  (
        print_endline "You can't play that on a split hand";
        splits_playing_phase st (current_player) player_count false
      )
  with Malformed -> (
      print_endline "Not a valid command";
      splits_playing_phase st (current_player) player_count false
    )


(** [playing_phase st current_player player_count] is the state after
    each player finishes hitting and standing. *)
let rec playing_phase (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) : State.t =
  if current_player < 0 then
    st 
  else if current_player = 0 then
    playing_dealer st current_player player_count doubled
  else
    let (player, bet, _, _, _, hard, _) = get_player st current_player in 
    let count = get_count player in 
    if count > 21 then
      playing_player_busted st current_player player_count doubled
    else playing_player_not_busted st current_player player_count doubled

and playing_dealer (st: State.t) (current_player: player_id) (player_count: int)
    (doubled : bool) : State.t =
  let (dealer, busted, hard) = st.house in
  if get_count dealer > 21 then 
    if hard then 
      ( print_dealer st; ANSITerminal.(print_string [green]
                                         "\n\nThe dealer busted!\n");
        playing_phase (bust_player st 0) (current_player - 1) player_count 
          false)
    else playing_phase (make_dealer_hard st) (current_player) player_count 
        false
  else if get_count dealer <= 16 then (
    ANSITerminal.(print_string [blue ]
                    ("\n\nThe dealers hits on "^(dealer |> get_count 
                                                 |> string_of_int)^".\n"));
    playing_phase (st |> dealer_hit) (current_player) player_count) false
  else (
    ANSITerminal.(print_string [blue ]
                    ("\n\nThe dealers stands on "^(dealer |> get_count 
                                                   |> string_of_int)^".\n"));
    playing_phase (st) (current_player - 1) player_count false)

and playing_player_busted (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) : State.t = 
  let (player, bet, _, _, _, hard, _) = get_player st current_player in 
  if hard then (print st current_player;
                ANSITerminal.(print_string [red]
                                ("\nPlayer "^
                                 (string_of_int current_player)^
                                 " busted.\n\n"));
                playing_phase (bust_player st current_player) 
                  (current_player - 1) 
                  player_count false)
  else playing_phase (make_player_hard st player.id) (current_player) 
      player_count false

and playing_player_not_busted (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) : State.t =
  let (player, bet, _, _, _, hard, _) = get_player st current_player in 
  let count = get_count player in 
  begin
    (ANSITerminal.(print_string [cyan] ("Current player: "^
                                        (string_of_int current_player)^"\n")));
    if doubled then (print st current_player;
                     playing_phase st (current_player - 1) player_count false)
    else
      begin print st current_player;
        ANSITerminal.(print_string [blue] ("Your count is "^
                                           (string_of_int count)
                                           ^".\nDo you want to hit, stand, split, or double down?\n"
                                          ));
        print_string "> "; let input = read_line () in
        try 
          playing_player_helper st current_player player_count doubled input
        with Malformed -> (
            print_endline "Not a valid command";
            playing_phase st (current_player) player_count false
          )
      end
  end

and playing_player_helper (st: State.t) (current_player: player_id) 
    (player_count: int) (doubled : bool) (input : string) : State.t =
  let parsed = parse input in
  match parsed with
  | Hit -> playing_phase (hit current_player st) (current_player) 
             player_count false
  | Stand -> playing_phase st (current_player - 1) player_count false
  | Split -> playing_split st current_player player_count doubled
  | Double -> playing_double st current_player player_count doubled

and playing_split (st: State.t) (current_player: player_id) (player_count: int)
    (doubled : bool): State.t = 
  let (player, bet, _, _, _, hard, _) = get_player st current_player in 
  let card1 = List.nth player.hand 0 in 
  let card2 = List.nth player.hand 1 in
  if ((List.length player.hand) = 2 && 
      ((val_of card1) = (val_of card2)) &&
      (List.length player.split_hand) = 0) then
    if get_cash player < (2*bet) then
      begin
        ANSITerminal.(print_string [red] 
                        ("Not enough cash to split." ^
                         "\n"));
        playing_phase st (current_player) player_count false
      end
    else
      let split_state = split_player st current_player in
      ANSITerminal.(print_string [cyan] 
                      ("Your hand is split. You will have the opportunity to play your second hand soon." ^
                       "\n"));
      playing_phase (hit current_player split_state) (current_player) 
        player_count false
  else
    begin
      ANSITerminal.(print_string [red] 
                      ("You can split one time per round when your hand consists of two cards with the same value." ^
                       "\n"));
      playing_phase st (current_player) player_count false
    end

and playing_double (st: State.t) (current_player: player_id) (player_count: int)
    (doubled : bool) : State.t = 
  let (player, bet, _, _, _, hard, _) = get_player st current_player in 
  if List.length player.hand > 2 then 
    begin
      ANSITerminal.(print_string [red] 
                      ("Cannot double down with more than 2 cards." ^
                       "\n"));
      playing_phase st (current_player) player_count false
    end
  else if get_cash player < (2*bet) then 
    begin
      ANSITerminal.(print_string [red] 
                      ("Not enough cash to double down." ^
                       "\n"));
      playing_phase st (current_player) player_count false
    end
  else
    begin
      let st' = double_player st current_player in
      playing_phase (hit (current_player) st') (current_player)
        player_count true 
    end


(** [find_busted st] is the list containing player_id of each player who
    busted in playing phase. *)
let rec find_busted (st : State.t) : player_id list =
  let busted = List.filter (fun (_, _, _, bust, _, _, _) -> bust) st.players in 
  List.map (fun (p, _, _, _, _, _, _) -> p.id) busted

(** [find_busted st] is the list containing player_id of each player who
    busted in playing phase. *)
let rec find_busted_splits (st : State.t) : player_id list =
  let busted = List.filter (fun (_, _, _, _, split_bust, _, _) -> split_bust) 
      st.players in List.map (fun (p, _, _, _, _, _, _) -> p.id) busted

(** [find_not_busted st] is the list containing player_id of each player who
    did not bust in playing phase. *)
let rec find_not_busted (st : State.t) : player_id list = 
  let not_busted = List.filter (fun (_, _, _, bust, _, _, _) -> (not bust)) 
      st.players in List.map (fun (p, _, _, _, _, _, _) -> p.id) not_busted

(** [find_not_busted st] is the list containing player_id of each player who
    did not bust in playing phase. *)
let rec find_not_busted_splits (st : State.t) : player_id list = 
  let not_busted = List.filter (fun (p, _, _, _, split_bust, _, _) -> 
      ((not split_bust) && (List.length p.split_hand > 0))) st.players in
  List.map (fun (p, _, _, _, _, _, _) -> p.id) not_busted

(** [dealer_busted st] is [true] if dealer busted and [false] otherwise. *)
let rec dealer_busted (st : State.t) : bool =
  let (_, bust, _) = st.house in 
  bust

(** [blackjack st p_id] is [true] if player with player_id [p_id] scored
    blackjack during playign phase and [false] otherwise. *)
let blackjack (st : State.t) (p_id : player_id) : bool =
  let (player, _, _, _, _, _, _) = get_player st p_id in 
  if get_count player == 21 then true else false

(** [player_win st p_id] is state with updated player with player_id [p_id]
    after adding the player's bet amount to their money total. *)
let player_win (st : State.t) (p_id : player_id) : State.t =
  let (player, _, doub, _, _, _, _) = get_player st p_id in
  if doub then 
    let new_cash = (get_cash player) + 2*(get_bet st p_id) in 
    change_money st p_id new_cash
  else 
    let new_cash = (get_cash player) + (get_bet st p_id) in 
    change_money st p_id new_cash

(** [player_win st p_id] is state with updated player with player_id [p_id]
    after adding the player's bet amount to their money total. *)
let player_win_split (st : State.t) (p_id : player_id) : State.t =
  let (player, _, _, _, _, _, _) = get_player st p_id in
  let new_cash = (get_cash player) + (get_bet st p_id) in 
  change_money st p_id new_cash

(** [player_loss st p_id] is state with updated player with player_id [p_id]
    after deducting the player's bet amount from their money total.  *)
let player_loss (st : State.t) (p_id : player_id) : State.t =
  let (player, _, doub, _, _, _, _) = get_player st p_id in 
  if doub then 
    let new_cash = (get_cash player) - 2*(get_bet st p_id) in 
    change_money st p_id new_cash
  else 
    let new_cash = (get_cash player) - (get_bet st p_id) in 
    change_money st p_id new_cash

(** [player_loss st p_id] is state with updated player with player_id [p_id]
    after deducting the player's bet amount from their money total.  *)
let player_loss_split (st : State.t) (p_id : player_id) : State.t =
  let (player, _, _, _, _, _, _) = get_player st p_id in 
  let new_cash = (get_cash player) - (get_bet st p_id) in 
  change_money st p_id new_cash

(** [players_all_win st p_ids] is state with updated players with player_id in 
    [p_ids] after adding the players' bet amounts to their money totals. *)
let rec players_all_win (st : State.t) (p_ids : player_id list) : State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  (
      let (player, _, _, _, _, _, _) = get_player st h in 
      ANSITerminal.(print_string [green] ("Player "^(string_of_int h)^" won $"^
                                          string_of_int (get_bet st h) ^
                                          " and has a total of $"^
                                          string_of_int 
                                            (get_cash player + (get_bet st h))^
                                          "\n"));
      players_all_win (player_win st h) t
    )

(** [players_all_win st p_ids] is state with updated players with player_id in 
    [p_ids] after adding the players' bet amounts to their money totals. *)
let rec split_players_all_win (st : State.t) (p_ids : player_id list) : 
  State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  (
      let (player, _, _, _, _, _, _) = get_player st h in 
      ANSITerminal.(print_string [green] ("Player "^(string_of_int h)^" won $"^
                                          string_of_int (get_bet st h) ^
                                          " on a split and has a total of $"^
                                          string_of_int 
                                            (get_cash player + (get_bet st h))^
                                          "\n"));
      split_players_all_win (player_win_split st h) t
    )

(** [players_all_lose st p_ids] is state with updated players with player_id in
    [p_ids] after deducting the players' bet amounts from their money totals. *)
let rec players_all_lose (st : State.t) (p_ids : player_id list) : State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  (
      let (player, _, _, _, _, _, _) = get_player st h in 
      ANSITerminal.(print_string [red] ("Player "^(string_of_int h)^" lost $"^ 
                                        string_of_int (get_bet st h) ^
                                        " and has a total of $"^string_of_int 
                                          (get_cash player - (get_bet st h))^
                                        "\n"));
      players_all_lose (player_loss st h) t
    )

(** [players_all_lose st p_ids] is state with updated players with player_id in
    [p_ids] after deducting the players' bet amounts from their money totals. *)
let rec split_players_all_lose (st : State.t) (p_ids : player_id list) : 
  State.t =
  match p_ids with
  | [] -> st
  | h :: t ->  (
      let (player, _, _, _, _, _, _) = get_player st h in 
      ANSITerminal.(print_string [red] ("Player "^(string_of_int h)^" lost $"^ 
                                        string_of_int (get_bet st h) ^
                                        " on a split and has a total of $"^
                                        string_of_int (get_cash player - 
                                                       (get_bet st h))^"\n"));
      split_players_all_lose (player_loss_split st h) t
    )

(** [did_win st p_id] is [true] if player scored higher than the house and
    [false otherwise]. Assumes house and player did not bust. *)
let did_win (st : State.t) (p_id : player_id) : bool =
  let (h, _, _) = st.house in
  let (player, _, _, _, _, _, _) = get_player st p_id in 
  (get_count h < get_count player)

(** [did_win st p_id] is [true] if player scored higher than the house and
    [false otherwise]. Assumes house and player did not bust. *)
let did_win_split (st : State.t) (p_id : player_id) : bool =
  let (h, _, _) = st.house in
  let (player, _, _, _, _, _, _) = get_player st p_id in 
  (get_count h < get_split_count player)

(** [who_won st not_busted] is list containing player_id of players from 
    [not_busted] who won over the house in the playing phase. *)
let rec who_won (st : State.t) (not_busted : player_id list) 
    (winners : player_id list) : player_id list = 
  match not_busted with
  | [] -> winners
  | h :: t -> begin 
      if (did_win st h) then
        who_won st t (h :: winners)
      else 
        who_won st t winners
    end

(** [who_won st not_busted] is list containing player_id of players from 
    [not_busted] who won over the house in the playing phase. *)
let rec who_won_split (st : State.t) (not_busted : player_id list) 
    (winners : player_id list) : player_id list = 
  match not_busted with
  | [] -> winners
  | h :: t -> begin 
      if (did_win_split st h) then
        who_won_split st t (h :: winners)
      else 
        who_won_split st t winners
    end

(** [who_lost st busted] is list containing player_id of players from [busted] 
    who did not win over the house in the playing phase. *)
let rec who_lost (st : State.t) (not_busted : player_id list) 
    (losers : player_id list) : player_id list = 
  match not_busted with
  | [] -> losers
  | h :: t -> begin 
      if not (did_win st h) then
        who_lost st t (h :: losers)
      else 
        who_lost st t losers
    end

(** [who_lost st busted] is list containing player_id of players from [busted] 
    who did not win over the house in the playing phase. *)
let rec who_lost_split (st : State.t) (not_busted : player_id list) 
    (losers : player_id list) : player_id list = 
  match not_busted with
  | [] -> losers
  | h :: t -> begin 
      if not (did_win_split st h) then
        who_lost_split st t (h :: losers)
      else 
        who_lost_split st t losers
    end

(** [moving_money_phase st] is the state after money is redistributed to
    all players. *)
let rec moving_money_phase (st : State.t) : State.t =
  if (dealer_busted st) then
    money_dealer_busted st
  else
    money_dealer_not_busted st

and money_dealer_busted (st : State.t) : State.t  = 
  let not_busted = find_not_busted st in
  let busted = find_busted st in 
  let st' = players_all_win st not_busted in
  let st'' = players_all_lose st' busted in
  let not_busted_split = find_not_busted_splits st'' in
  let busted_split = find_busted_splits st'' in 
  let st''' = split_players_all_win st'' not_busted_split in 
  let st'''' = split_players_all_lose st''' busted_split in 
  st''''

and money_dealer_not_busted (st : State.t) : State.t = 
  let busted = find_busted st in
  let busted_split = find_busted_splits st in 
  let st' = players_all_lose st busted in
  let st'' = split_players_all_lose st' busted_split in
  let not_busted = find_not_busted st'' in 
  let not_busted_split = find_not_busted_splits st'' in 
  let winners = who_won st'' not_busted [] in 
  let losers = who_lost st'' not_busted [] in
  let split_winners = who_won_split st'' not_busted_split [] in 
  let split_losers = who_lost_split st'' not_busted_split [] in 
  let st''' = players_all_lose st'' losers in
  let st'''' = players_all_win st''' winners in
  let st''''' = split_players_all_lose st'''' split_losers in 
  let st'''''' = split_players_all_win st''''' split_winners in
  st''''''

(** [game_body st] is the state after executing a full round of Blackjack. *)
let rec game_body (st : State.t) : State.t = 
  let len = List.length st.players in 
  let st' = betting_phase st 1 len in
  let st'' = dealing_phase st' 0 len in 
  print_dealer st'';
  let st''' = playing_phase st'' len len false in 
  let st'''' = splits_playing_phase st''' len len false in 
  (* let st'''' = st''' in  *)
  let st''''' = moving_money_phase st'''' in 
  let st'''''' = reset_round st''''' in
  ANSITerminal.(print_string [green] ("Round complete. Would you like to continue? y/n > "));
  let resp = read_line () in (
    if resp = "n" then (
      ANSITerminal.(print_string [red] ("Quitting...")); st''''''
    )
    else
      game_body st''''''
  )


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
  | player_count, real_seed -> (ignore (new_game (int_of_string player_count) 
                                          (int_of_string seed) |> game_body);)

(* Start the game. *)
let () = main ()