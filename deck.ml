type suit =
  | Heart
  | Diamond
  | Spade
  | Club

type value = int

type card = {
  suit: suit; value: value
}

let (--) (i : int) (j : int) : int list =
  let rec from i j l =
    if i>j then l
    else from i (j-1) (j::l)
  in from i j []

(** AF: The list [{suit=Heart; value=3}; {suit=Diamond; value=1}]
      represents the deck containing a 3 of Hearts and an Ace of Diamonds. 
      The value 1 represents an Ace, and the values 11, 12, and 13 represent
      Jack, Queen, and King respectively. 
    RI: The list has at most 52 cards. Any card with a Number n value must meet
      1 <= n <= 13. There can be no identical cards in the list. *)
type t = card list

let rec generate_cards_of_suit 
    (st : suit) 
    (loop : int list) 
    (acc: card list) : card list = 
  match loop with
  | [] -> acc
  | h::t -> generate_cards_of_suit st t ({suit=st;value=h} :: acc)

let new_deck : t = 
  let hearts = generate_cards_of_suit Heart (1--13) [] in
  let diamonds = generate_cards_of_suit Diamond (1--13) [] in  
  let spades = generate_cards_of_suit Spade (1--13) [] in  
  let clubs = generate_cards_of_suit Club (1--13) [] in
  hearts@diamonds@spades@clubs

let shuffle (old_deck : t) : t =
  let rec helper (old_deck : t) (new_deck : t) : t = 
    match old_deck with
    | [] -> new_deck
    | _ -> begin 
        let chosen = (List.length old_deck) |> Random.int |> List.nth old_deck in
        helper (List.filter (fun c -> c <> chosen) old_deck) (chosen :: new_deck) 
      end in

  helper old_deck []

let choose (deck : t) : (card * t) option = 
  match deck with
  | [] -> None
  | h :: t -> Some (h, t)

let length (deck : t) : int = 
  List.length deck

let to_list (deck : t) : card list = 
  deck