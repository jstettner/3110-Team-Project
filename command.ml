exception Malformed

type command =
  | Hit
  | Stand

let parse str =
  match str with 
  | "Hit" | "hit" | "h" -> Hit
  | "Stand" | "stand" | "s" -> Stand
  | _ -> raise Malformed