open Deck

type player_id = int
type player_count = int
type player_money = int

type t = {
  id: player_id;
  hand: card list;
  count: player_count;
  cash: player_money
}

let new_player (id : player_id) (starting_cash : player_money) : t =
  {
    id= id;
    hand= [];
    count= 0;
    cash = starting_cash;
  }

let get_id (p: t) : player_id =
  p.id

let get_count (p: t) : player_count =
  p.count

let get_cash (p: t) : player_money =
  p.cash

let get_hand (p: t) : card list =
  p.hand

let set_cash (p: t) (money: int) : t =
  {
    id = p.id;
    hand = p.hand;
    count = p.count;
    cash = money;
  }

let new_round (p: t) : t = 
  {
    id=p.id;
    hand=[];
    count=0;
    cash=p.cash
  }

let add_to_hand (p: t) (c: card) : t = 
  {
    id= p.id;
    hand= c :: p.hand;
    count= p.count;
    cash = p.cash;
  }

let inc_count (p: t) (count: int) : t =
  {
    id= p.id;
    hand= p.hand;
    count= p.count + count;
    cash = p.cash;
  }