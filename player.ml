open Deck

type player_id = int

module Player = struct
  type t = player_id * (card list)

  let new_player (id : player_id) : t =
    (id, [])

  let draw (deck : Deck.t) ((p_id, cards) : t) : (Deck.t * t) =
    match Deck.choose (deck) with
    | None -> (deck, (p_id, cards))
    | Some (card, new_deck) -> (new_deck, (p_id, card :: cards))

  let has_in_hand (zuit : suit) (zalue : value) ((p_id, cards) : t) : bool = 
    List.mem {suit = zuit; value = zalue} cards

  let remove_from_hand (zuit : suit) (zalue : value) ((p_id, cards) : t) : t = 
    let new_hand = List.filter (fun x -> x <> {suit = zuit; value = zalue}) cards in
    (p_id, new_hand)

  let get_hand ((p_id, cards) : t) : card list = 
    cards

end