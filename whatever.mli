type suit

type value

type card

module type Deck = sig
  type t

  val new_deck : t

  val shuffle : t -> t

  val choose : t -> (card * t) option 
end