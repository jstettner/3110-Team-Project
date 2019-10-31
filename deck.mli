type suit =
  | Heart
  | Diamond
  | Spade
  | Club

type value = int

type card = {
  suit: suit; value: value
}

module type Deck = sig
  type t

  val new_deck : t

  val shuffle : t -> t

  val choose : t -> card * t
end