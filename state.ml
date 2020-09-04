open Command
open Game

type player_id = int

type player = {   
  player_words:(Command.word * Game.points) list;
  total_points: Game.points;
  player_letter_set: Game.t;
  current_letter: string;
  swaps: float;
  stolen: (player_id * Command.word) list;
}

type t = {
  turns_left: int;
  player_list: (player_id  * player) list;
  current_player: player_id;
  total_players: int;
  mode: string;
  set: Game.t;
  alpha: Game.all_letters_in_json
}

type result = Legal of t | Illegal of string

(** [random_letter ()] is a random uppercase letter from the English alphabet.*)
let random_letter set = if (set_length set) < 4 || (set_length set) > 7
  then List.nth ["A";"E";"I";"O";"U"] (Random.self_init(); Random.int 5)
  else Char.escaped (Char.chr ((Random.self_init(); Random.int 26) + 65))

(** [init_player] initializes a player. *)
let init_player set = {
  player_words = [];
  total_points = 0;
  player_letter_set = set;
  current_letter = random_letter set;
  swaps = 0.;
  stolen = [];
}

let init_state set num turn mode a = {
  turns_left= turn * num; 
  player_list = List.init num (fun i -> ((i + 1), init_player set));
  current_player = 1;
  total_players = num;
  mode = mode;
  set= set;
  alpha = a
}

let turns state = 
  state.turns_left

(* [state_alpha state] is the list of the alphabet being used in [state]*)
let state_alpha state = 
  state.alpha

let current_player state = 
  state.current_player

let current_player_stolen state = 
  (List.assoc state.current_player state.player_list).stolen

let current_player_wordlist state =  
  (List.assoc state.current_player state.player_list).player_words

let current_player_points state = 
  (List.assoc state.current_player state.player_list).total_points

let current_player_letter_set state =
  (List.assoc state.current_player state.player_list).player_letter_set

let current_player_letter state = 
  (List.assoc state.current_player state.player_list).current_letter

let player_count state = 
  state.total_players

let get_pool state = state.set

let next_player state = 
  if (not (state.current_player >= state.total_players))
  then state.current_player + 1 
  else 1

let previous_player state = 
  if not (state.current_player = 1)
  then state.current_player - 1 
  else state.total_players

let prev_player_points state = 
  (List.assoc (previous_player state) state.player_list).total_points

(* [remove_dupl lst acc] is [lst] with duplications removed. 
   Requires: [acc] is the empty list. *)
let rec remove_dupl lst acc = 
  match lst with
  | [] -> acc
  | h::t -> if List.mem h acc then remove_dupl t acc else remove_dupl t (h::acc)

(**[word_to_cl n] is a char list of the input [word].*)
let word_to_cl n = List.init (String.length n) (String.get n)

(**[cl_to_ll cl] is a string list of char list [cl], with all letters in 
   uppercase.*)
let cl_to_ll cl = List.map (fun x -> Char.escaped x) cl 
                  |> List.map String.uppercase_ascii

let calculate_bonus_points word base = 
  let length = String.length word in 
  if length >= 3 && length < 5 
  then base |> float_of_int |> (fun x -> x*. 1.2) |> int_of_float
  else if length >= 5 
  then base |> float_of_int |> (fun x -> x*. 1.5) |> int_of_float
  else base

let calculate_base_points word st = 
  let a = state_alpha st in
  List.fold_left 
    (fun x y -> x + Game.get_points a y) 0 (word |> word_to_cl |> cl_to_ll) 

let calculate_word_points word st : Game.points = 
  let base = calculate_base_points word st in calculate_bonus_points word base

(* [start_message_help st stolen pt] prints information about (a) steal 
   action(s) as recorded in [stolen]. Total [pt] lost and the player id(s) 
   of the stealer(s) are printed. *)
let rec start_message_help st stolen pt = 
  match stolen with 
  | [] -> 
    print_string ("You've lost a total of " ^ string_of_int pt ^ " points.\n")
  | (id',word)::t -> 
    print_string ("Player "^string_of_int id'^ " stole your word '" ^ 
                  word ^ "' last round! "); 
    let pt' = pt + (calculate_word_points word st) in 
    start_message_help st t pt'

let start_message st = 
  let id = st.current_player in 
  let p_l = st.player_list in
  let p = List.assoc id p_l in 
  start_message_help st p.stolen 0

(** [remove_invalid next_player inv_words state] is a player with all invalid 
    words removed from his words list*)
let rec remove_invalid next_player inv_words state = 
  match inv_words with
  | [] -> next_player
  | h :: t -> (if List.mem_assoc h (next_player.player_words) 
               then (let new_next_pwlst = 
                       List.remove_assoc h (next_player.player_words) in 
                     remove_invalid ({next_player with 
                                      player_words = new_next_pwlst; 
                                      total_points = 
                                        next_player.total_points - 
                                        calculate_word_points h state;
                                      player_letter_set = 
                                        next_player.player_letter_set;
                                      current_letter = ""}) 
                       t state)
               else remove_invalid next_player t state)

(* [caculate_swap_points state] is the number of points lost from a swap in
   [state].*)
let calculate_swap_points state = 
  let id = state.current_player in 
  let player = List.assoc id state.player_list in 
  let swaps = player.swaps in 
  (-.(5. +. (1.5**swaps))) |> Float.round |> int_of_float

(* [update_swap state newset v] is the player [v] after a swap has been 
   performed. [v] takes set [newset]. *)
let update_swap state newset v =  {
  player_words = v.player_words;
  total_points = v.total_points + (calculate_swap_points state);
  player_letter_set = newset;
  current_letter = random_letter (get_pool state);
  swaps =  v.swaps +. 1.;
  stolen = v.stolen
}

(* [update_steal state newset word v id2] is the player [v] after a steal has
   been performed by player of id [id2] on [word]. [v] takes set [newset]. *)
let update_steal state newset word v id2 =  {
  player_words = 
    (let words = String.uppercase_ascii word in
     let p = List.mem_assoc words v.player_words in 
     if p then List.remove_assoc words v.player_words else 
       List.remove_assoc words v.player_words);
  total_points = v.total_points + (calculate_word_points word state);
  player_letter_set = newset;
  current_letter = random_letter (get_pool state);
  swaps =  v.swaps;
  stolen = v.stolen @ [id2, word]
}

(* [update_create state newset word v] is the player [v] after creating [word].
   [v] takes set [newset]. *)
let update_create state newset word v =  {
  player_words = 
    (let words = String.uppercase_ascii word in 
     v.player_words @ [(words,calculate_word_points word state)]);
  total_points = v.total_points + (calculate_word_points word state);
  player_letter_set = newset;
  current_letter = random_letter (get_pool state);
  swaps =  v.swaps;
  stolen = v.stolen
}

(* [update_create state newset v] is the player [v] after passing. [v] takes set 
   [newset]. *)
let update_pass state newset v = {
  player_words = v.player_words;
  total_points = v.total_points;
  player_letter_set = newset;
  current_letter = random_letter (get_pool state);
  swaps =  v.swaps;
  stolen = v.stolen
}

(* [update_check state newset word v] is the player [v] after invalidating 
   [word]. [v] takes set [newset]. *)
let update_check state newset word v = {
  player_words = 
    (let words = String.uppercase_ascii word in
     let p = List.mem_assoc words v.player_words in 
     if p then List.remove_assoc words v.player_words else 
       List.remove_assoc words v.player_words);
  total_points = v.total_points - (calculate_word_points word state);
  player_letter_set = newset;
  current_letter =  "";
  swaps = v.swaps;
  stolen = v.stolen;
}

(** [update_player_list state newset players word action id1 id2] is the 
    player_list as a result of [action] being executed on the player with [id1] 
    in [state]. If [id2] is not "", [action] was exected by [id2] and not [id1]. 
    Player [id1] takes the letter set [newset]. 
      If [action] = "steal" or "check", [word] is removed from [id]'s word list. 
         [action] = "swap", [word] is the letter swapped out of [id]'s. 
         [action] = "create", [word] is added to [id]'s word list. 
         [action] = "pass", [word] is "". *)
let rec update_player_list state newset players word action id1 id2 = 
  match players with 
  | [] -> []
  | (k,v)::t -> if k = id1 then 
      let player = (
        if action = "swap" then update_swap state newset v
        else if action = "steal" then update_steal state newset word v id2
        else if action = "create" then update_create state newset word v
        else if action = "check" then update_check state newset word v
        else (* Pass *) update_pass state newset v)
      in (k,player)::(update_player_list state newset t word action id1 id2)
    else (k,v)::(update_player_list state newset t word action id1 id2)

(**[remove x lst acc] is [lst] with the first occurance of [x] removed. *)
let rec remove x lst acc = match lst with
  | [] -> acc
  | h::t -> if h = x then acc @ t else remove x t (h::acc)

(**[check_illegal ll combo_l] is [true] iff [ll] contains letter(s) that is not
   in the combo or more occurances of some letter than what is offered in the 
   combo. *)
let rec check_illegal ll combo_l = 
  match ll with 
  | [] -> false
  | h :: t -> if not (List.mem h combo_l) then true
    else check_illegal t (remove h combo_l [])

(**[check_letter_used st word] is [true] iff [word] contains the player's 
   current letter in [st]. *)
let check_letter_used st word = String.contains (String.uppercase_ascii word)
    (String.get (current_player_letter st) 0) 

(** [string_to_sl s i] is the string list of [s], where [i] is the
    length of the string subtracted by 1. All in uppercase. *)
let rec string_to_sl s i = let ups = String.uppercase_ascii s in
  if i>(-1) then 
    String.make 1 (String.get ups i) ::string_to_sl ups (i-1) else [] 

let create_pl_combo_word playerwl = 
  let pwlkey = List.map fst playerwl in
  let str = List.fold_left (fun a k -> k ^ a) "" (pwlkey) in
  let str_list = string_to_sl str (String.length str - 1) in 
  let without_dups = remove_dupl str_list [] in
  let finish = List.fold_left (fun a k -> k ^ a) "" (without_dups) in
  if String.length finish < 10 then finish else ""

let create word state s = 
  let combo = (if state.mode = "pool" then 
                 (current_player_letter state)::
                 (Game.get_letters (get_pool state))
               else Game.get_letters (current_player_letter_set state)) in
  if word = "" then Illegal "Please enter a word."
  else if (s=false) && (check_illegal (word |> word_to_cl |> cl_to_ll) combo) 
  then Illegal "This word cannot be constructed with the current letter set. \n"
  else if state.mode = "pool" && not(check_letter_used state word)
  then Illegal ("The word '" ^ word ^ "' does not contain your letter.\n")
  else 
    let player = state.current_player in 
    let player_l = state.player_list in 
    let new_set = (List.assoc player player_l).player_letter_set in
    let new_player_l = 
      update_player_list state new_set player_l word "create" player (-1) in
    let used_letters_l = string_to_sl word ((String.length word)-1) in
    Legal {
      state with 
      turns_left = state.turns_left - 1;
      player_list = new_player_l;
      current_player = next_player state;
      total_players = state.total_players;
      set = if state.mode = "pool" && s=false
        then remove_letter state.set used_letters_l 
        else state.set
    } 

let pass state = if state.mode = "normal" then
    Legal { state with
            turns_left = state.turns_left - 1;
            current_player = next_player state;
          } 
  else let player = state.current_player in 
    let player_l = state.player_list in 
    let new_set = (List.assoc player player_l).player_letter_set in
    Legal { state with
            turns_left = state.turns_left - 1;
            player_list = 
              update_player_list state new_set player_l "" "pass"
                player (-1);
            current_player = next_player state;
            set = 
              Game.add_in_pool state.set (current_player_letter state) 
                (state_alpha state)
          } 

let swap l state json = 
  let alphabet = from_json json in 
  let set = current_player_letter_set state in
  let player = state.current_player in 
  let player_l = state.player_list in
  let new_set = swap_letter alphabet l set in 
  Legal { state with
          turns_left = state.turns_left - 1;
          player_list = 
            update_player_list state new_set player_l l "swap" player (-1);
          current_player = next_player state;
        }

let steal w nw p st = 
  let wup = String.uppercase_ascii w in
  let nwup = String.uppercase_ascii nw in 
  let p' = current_player st in
  let player_l = st.player_list in 
  let player = List.assoc p player_l in
  let words = player.player_words in 
  let new_set = player.player_letter_set in
  if not (List.mem_assoc wup words) then 
    Illegal ("The word '" ^ wup ^ "' is not in player " ^ string_of_int p ^ 
             "'s word list.")
  else if not (check_letter_used st nw) 
  then Illegal ("The word '" ^ nwup ^ "' does not contain your letter.")
  else if not ((String.length nwup) = ((String.length wup) + 1)) 
  then Illegal ("You cannot use letters in the pool to steal a word.")
  else 
    Legal {st with player_list = 
                     update_player_list st new_set player_l wup "steal" p p'}

(** [winner_check_helper players winners winner_p] is the list of winners in 
    game of state [state] and the highest number of point achieved by a player 
    in that game.*)
let rec winner_check_helper players winners winner_p = 
  match players with 
  | [] -> (winners,winner_p)
  | (id,p)::t -> if p.total_points > winner_p 
    then winner_check_helper t (id::[]) p.total_points
    else if p.total_points = winner_p && (not (List.mem id winners))
    then winner_check_helper t (id::winners) winner_p 
    else winner_check_helper t winners winner_p 

let winner_check state =
  let state' = {state with current_player = 1;} in
  let p_list = state.player_list in 
  let win_id = state'.current_player in 
  let win_p = (List.assoc win_id p_list).total_points in
  winner_check_helper p_list (win_id::[]) win_p

(* =====Below is for check phase====== *)

let rec invalid word_lst game state =
  match word_lst with 
  | [] -> state 
  | (h::t) -> let id = next_player state in 
    let player_l = state.player_list in 
    let player = List.assoc id player_l in 
    let new_set =  player.player_letter_set in 
    let word = String.uppercase_ascii h in
    let player_l' = 
      update_player_list state new_set player_l word "check" id (-1) in 
    invalid t game {state with player_list = player_l'}

let valid game state = 
  {state with current_player = state.current_player + 1}

let next_player_state game state = 
  {state with current_player = next_player state}

let print_player_word_list state id = 
  let wl = (List.assoc id state.player_list).player_words in
  if wl = [] then print_string "No words yet.\n" else
    List.iter (fun (k,v)-> print_string k; print_newline ();) wl

let print_player_letter st = 
  print_string ("\nCurrent player's letter: " );
  ANSITerminal.(print_string [Bold;blue] ((current_player_letter st) ^ "\n\n"))

(** [print_all_player_word_list_helper st acc] is a helper function 
    that prints all player[id]'s word list. *)
let rec print_all_player_word_list_helper st acc : unit = 
  if (acc > List.length st.player_list) 
  then () 
  else begin print_string ("Player " ^ string_of_int acc ^ ": "); 
    print_player_word_list st acc;
    print_all_player_word_list_helper st (acc + 1) end

let print_all_player_word_list st = print_all_player_word_list_helper st 1

