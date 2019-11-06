exception Malformed

type command =
  | Hit
  | Stand

let match_cmd str =
  match str with 
  | "Hit" -> Hit
  | "Stand" -> Stand
  | _ -> raise Malformed