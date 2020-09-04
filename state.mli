(** 
   This module represents the state of an the game as it is being played,
   tracking values like the number of turns left and maintaining the state
   of the player as words are entered and points are assigned.
*)

open Game
open Command

(** The abstract type of the player. *)
type player

(** The abstract type of the game state. *)
type t

(** The type of returning result of a command action. *)
type result = Legal of t | Illegal of string

(** The type of a player id. *)
type player_id = int

(** [init_state set num turn mode a] is the initial state with the available 
    letter set [set], [num] players, [turn] number of turns, and alphabet set 
    [a] for the game mode [mode]. *)
val init_state : Game.t -> int -> int -> string -> Game.all_letters_in_json -> t

(** [turns state] is the turns left in game state [state]. *)
val turns: t  -> int

(** [current_player state] is the player_id whose turn is active in [state]. *)
val current_player: t -> int

(** [current_player_stolen state] is a list of stolen words and the id of the
    player who stole that word for the current player in [state].*)
val current_player_stolen: t -> (player_id * Command.word) list

(** [current_player_wordlist state] is the word-points list of the current 
    player in [state]. *)
val current_player_wordlist: t -> (Command.word * Game.points) list

(** [current_player_points state] is the total points of the current player in
    [state]. *)
val current_player_points: t -> Game.points

(** [current_player_letter_set state] is the current player's letter set
    in [state]. *)
val current_player_letter_set: t -> Game.t

(** [current_player_letter st] is the current player's letter at state [st]. *)
val current_player_letter: t -> string 

(** [player_count state] is the number of players in [state].*)
val player_count: t -> int

(** [get_pool state] is the current pool of letters in [state]. *)
val get_pool: t -> Game.t

(** [next_player state] gives the [id] of the player whose turn is next in 
    [state]. *)
val next_player: t -> player_id

(** [prev_player_points state] gives the points of the player whose turn 
    happened last in [state]. *)
val prev_player_points: t -> player_id

(* [calculate_bonus_points base word] is [base] points added with bonus
   points depending on the length of [word]. *)
val calculate_bonus_points: Command.word -> Game.points -> Game.points 

(** [calculate_base_points word st] is the points of [word] based on point
    values in [st]. *)
val calculate_base_points: Command.word -> t -> Game.points

(** [calculate_word_points word st] is the points of [word] based on point
    values in [st] with bonus points added. *)
val calculate_word_points: Command.word -> t -> Game.points

(** [start_message st] prints available messages for the current player in
    [state]. *)
val start_message: t -> unit

(** [create_pl_combo_word g] make the combo list [g] a string. *)
val create_pl_combo_word : (Command.word * Game.points) list -> string

(** [create word state s] is the result after the player in [game] attempts
    to create the [word]. [s] is whether or not the word was stolen. *)
val create: Command.word -> t -> bool -> result

(** [pass state] is the result after the player in [state] passes their turn.*)
val pass: t -> result

(** [swap l state json] is the result after the player in [state] swaps their 
    letter [l] based on [json]. *)
val swap: Game.letter -> t -> Yojson.Basic.t -> result

(** [steal w nw p st] is the result after the current player steals word [w] 
    from player [p] to construct word [nw] in [state]. *)
val steal: Command.word -> Command.word -> player_id -> t -> result

(** [winner_check state] is the list of winners in game of state [state] and
    the highest number of point achieved by a player in that game.*)
val winner_check: t -> (player_id list * Game.points)

(**[invalid word_lst game state] is the updated state where the next player's 
    words list is checked as valid. *)
val invalid: Command.word list -> Game.t -> t -> t

(** [invalid word_lst game state] is the updated state where the next player's 
    words list is already valid.*)
val valid : Game.t -> t -> t

(** [next_player_state state] gives the state moved on to the 
    player whose turn is next. *)
val next_player_state : Game.t -> t -> t

(** [print_player_word_list state id] prints player[id]'s word list.*)
val print_player_word_list: t -> player_id -> unit

(** [player_count st] prints the players current letter. *)
val print_player_letter: t -> unit

(** [print_all_player_word_list state id] prints all player[id]'s word list.*)
val print_all_player_word_list: t -> unit
