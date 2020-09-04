type word = string

type command = 
  | Create of word
  | Quit
  | Pass
  | Swap of Game.letter
  | Steal of int * word * word

exception Empty

exception Malformed

exception SingleChar

type check =
  | Valid
  | Invalid of word list

(** [remove x acc] removes elements from the given list if [x] equals the 
    element, returning a new list [acc] with elements not equal to [x]. *)
let rec remove x acc = function
  | [] -> acc
  | h::t -> if h = x then remove x acc t else remove x (h::acc) t

let parse_number str = 
  match int_of_string str with
  | exception (Failure s) -> 0
  | x -> x

(** [check_str s] is [true] if the string [s] is an integer.*)
let check_int s = 
  try (int_of_string s |> string_of_int) = s
  with Failure _ -> false

let parse str = 
  match List.rev (String.split_on_char ' ' str |> remove "" []) with
  | [] -> raise(Empty)
  | h :: t -> 
    if h = "quit" && t = [] then Quit else 
    if h = "create" && List.length t = 1 then begin
      if String.length (List.hd t) > 1 then 
        Create (String.uppercase_ascii (List.hd t)) else raise SingleChar end
    else 
    if h = "pass" && t = [] then Pass else
    if h = "swap" && List.length t = 1 then 
      Swap (String.uppercase_ascii (List.hd t)) else
    if h = "steal" && List.length t = 3 && check_int (List.hd t) then 
      let id = int_of_string (List.hd t) in 
      Steal (id, List.nth t 1 |> String.uppercase_ascii, 
             List.nth t 2 |> String.uppercase_ascii)
    else
      raise Malformed 

let parse_check str = 
  match List.rev (String.split_on_char ' ' str |> remove "" []) with
  | [] -> raise Empty 
  | h :: t -> 
    if h = "invalid" && t != []
    then Invalid (List.map String.uppercase_ascii t)
    else if h = "valid" && t = []
    then Valid
    else raise Malformed