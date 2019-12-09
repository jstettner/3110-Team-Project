exception Malformed

type command =
  | Hit
  | Stand
  | Double
  | Split

val parse : string -> command
