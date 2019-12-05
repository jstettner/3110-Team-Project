exception Malformed

type command =
  | Hit
  | Stand
  | Double

val parse : string -> command
