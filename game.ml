open Yojson.Basic.Util
open Lwt
open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

type points = int
type letter = string

type alphabet =  { vowels: (letter * points) list; 
                   consonants: (letter * points) list }

type t = (letter * points) list

type all_letters_in_json = (letter * points) list

(** [points_as_int e] is a letter-points pair pased from json with points as 
    integer.*)
let points_as_int e = (fst e, snd e |> to_int)

let from_json j : alphabet = { 
  vowels = j |> member "vowels" |> to_assoc |> List.map points_as_int;
  consonants = j|> member "consonants" |> to_assoc |> List.map points_as_int;
}

let all_letters a : all_letters_in_json = a.vowels @ a.consonants 

let set_length s = List.length s

let empty() = []

(** [rand_l a b] is a random letter in the list [a], with 
    index between 0 (inclusive) and the bound [b] (exclusive). *)
let rand_l a b = (List.nth a (Random.self_init(); Random.int b))

let combo_set_var a lim =  
  let vow_num = ((lim + 1) / 3) in
  let vow_lst = List.init vow_num (fun x -> rand_l a.vowels 5) in
  let cons_num = lim - vow_num in
  let cons_lst = List.init cons_num (fun x -> if x < 4 then 
                                        rand_l a.consonants 21 
                                      else rand_l a.consonants 18) in
  vow_lst @ cons_lst

(** [to_list a k] is a list of the keys of the association list [a] except for 
    key [k], iff [k] is in [a]. *)
let rec to_list a k  = 
  match a with 
  | [] -> []
  | (char, point)::t -> if char != k then (char)::to_list t k else to_list t k

let swap_letter a l set = 
  let ul = String.uppercase_ascii l in 
  let new_a_v = to_list a.vowels l in
  let new_a_c = to_list a.consonants l in 
  let new_a = new_a_v @ new_a_c in
  let l' = rand_l new_a (List.length new_a) in 
  let points = try List.assoc l' a.consonants with 
    | Not_found -> List.assoc l' a.vowels in 
  let new_set = List.remove_assoc ul set in 
  ((l',points)::new_set)

let rec print_letters s = 
  match s with 
  | [] -> ()
  | (k,v)::t -> print_endline k; print_letters t

let generate_new_set l swappair set = swappair :: (List.remove_assoc l set)

let print_list a m rep= if not rep then begin 
    if m = 1 then 
      print_endline "\nYour Letters: \n"
    else print_endline "\nThe Pool: \n";
    List.iter 
      (fun (k,v) -> 
         ANSITerminal.(print_string [Bold;blue] ("  " ^ k ^ "     "))) a;
    print_string 
      ("\n--------------------------------------------------------------------"
       ^ "---------\n");
    List.iter (fun (k,v) -> 
        print_string ""; print_int v; print_string " pts   ") a;
    print_endline "\n" 
  end else ()

let rec get_points a l = match a with 
  | [] -> failwith "not in letter set"
  | (l', p) :: t -> if l = l' then p else get_points t l 

let get_letters game = List.map fst game

let create_combo_word lst = 
  List.fold_left (fun a k -> k ^ a) "" (get_letters lst)

let json_lookup url = 
  (Client.get (Uri.of_string ("http://www.anagramica.com/lookup/" ^ url 
                              ^ "?callback=<callback>")) >>=
   fun(resp,body) -> 
   body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string)

(** [json_anagram url] makes an API call and returns a promise of the Yojson. *)
let json_anagram url = 
  (Client.get (Uri.of_string ("http://www.anagramica.com/all/:" ^ url)) >>=
   fun(resp,body) -> 
   body |> Cohttp_lwt.Body.to_string >|= Yojson.Basic.from_string)

(** [all_anagrams json] is a string list from the Yojson anagrams 
    of the combo set. *)
let all_anagrams json = 
  json |> member "all" |> Yojson.Basic.Util.to_list |> List.map(to_string)

(** [all_found json] is the int 1 or 0, 
    returning 1 if the word looked up is in the dictionary,
    returning 0 if the word looked up does not exist. *)
let all_found json = 
  json |> member "found" |> Yojson.Basic.Util.to_int

let make_found url = json_lookup url >>= fun y -> all_found y |> Lwt.return

let make_a_lst url = json_anagram url >>= fun y -> all_anagrams y |> Lwt.return

let rec remove_letter s c = 
  match c with 
  | [] -> s 
  | h::t -> remove_letter (List.remove_assoc h s) t

let add_in_pool game l a = 
  if l = "" 
  then match rand_l a 26 with 
    | (l',p) -> (l',p)::game
  else ((l,get_points a l)::game)

let all_to_t a = a