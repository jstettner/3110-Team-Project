exception Malformed

type command =
  | Hit
  | Stand
  | Double
  | Split

let parse str =
  match str with 
  | "Hit" 
  | "hit" 
  | "h" -> Hit
  | "Stand" 
  | "stand" 
  | "s" -> Stand
  | "Double Down" 
  | "double down" 
  | "Double" 
  | "dd"
  | "Down" 
  | "double" 
  | "down" 
  | "d" -> 
    Double
  | "Split"
  | "spl"
  | "split"
  | "sp" -> Split
  | _ -> raise Malformed