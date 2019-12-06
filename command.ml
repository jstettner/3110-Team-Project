exception Malformed

type command =
  | Hit
  | Stand
  | Double

let parse str =
  match str with 
  | "Hit" | "hit" | "h" -> Hit
  | "Stand" | "stand" | "s" -> Stand
  | "Double Down" | "double down" | "Double" | "Down" | "double" | "down" | "d" -> 
    Double
  | _ -> raise Malformed