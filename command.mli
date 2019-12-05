exception Malformed

type command =
  | Hit
  | Stand

val parse : string -> command
