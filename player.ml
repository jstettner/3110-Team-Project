open Deck

type player_id = int
type player_count = int
type player_money = int

type t = {
  id: player_id;
  hand: card list;
  count: player_count;
  cash: player_money;
  split_hand: card list;
  split_count: player_count;
}

let new_player (id : player_id) (starting_cash : player_money) : t =
  {
    id= id;
    hand= [];
    count= 0;
    cash = starting_cash;
    split_hand = [];
    split_count= 0;
  }

let get_id (p: t) : player_id =
  p.id

let get_count (p: t) : player_count =
  p.count

let get_split_count (p: t) : player_count =
  p.split_count

let sub_count_ten (p: t) : t =
  {
    id= p.id;
    hand= p.hand;
    count= p.count - 10;
    cash = p.cash;
    split_hand = p.split_hand;
    split_count= p.split_count;
  }

let split_sub_count_ten (p: t) : t =
  {
    id= p.id;
    hand= p.hand;
    count= p.count;
    cash = p.cash;
    split_hand = p.split_hand;
    split_count= p.split_count - 10;
  }

let get_cash (p: t) : player_money =
  p.cash

let get_hand (p: t) : card list =
  p.hand

let get_split_hand (p: t) : card list =
  p.split_hand

let set_cash (p: t) (money: int) : t =
  {
    id = p.id;
    hand = p.hand;
    count = p.count;
    cash = money;
    split_hand = p.split_hand;
    split_count= p.split_count;
  }

let new_round (p: t) : t = 
  {
    id=p.id;
    hand=[];
    count=0;
    cash=p.cash;
    split_hand = [];
    split_count= 0;
  }

let add_to_hand (p: t) (c: card) : t = 
  {
    id= p.id;
    hand= c :: p.hand;
    count= p.count;
    cash = p.cash;
    split_hand = p.split_hand;
    split_count= p.split_count;
  }

let add_to_split_hand (p: t) (c: card) : t = 
  {
    id= p.id;
    hand= p.hand;
    count= p.count;
    cash = p.cash;
    split_hand = c :: p.split_hand;
    split_count= p.split_count;
  }

let inc_count (p: t) (count: int) : t =
  {
    id= p.id;
    hand= p.hand;
    count= p.count + count;
    cash = p.cash;
    split_hand = p.split_hand;
    split_count= p.split_count;
  }

let inc_split_count (p: t) (count: int) : t =
  {
    id= p.id;
    hand= p.hand;
    count= p.count;
    cash = p.cash;
    split_hand = p.split_hand;
    split_count= p.split_count + count;
  }

let split_hand (p: t) : t =
  assert (List.length p.hand = 2);
  let card1 = List.nth p.hand 0 in 
  let card2 = List.nth p.hand 1 in 
  assert (val_of card1 = val_of card2);
  {
    id= p.id;
    hand= [card1];
    count= Deck.val_of card1;
    cash = p.cash;
    split_hand = [card2];
    split_count= Deck.val_of card2;
  }