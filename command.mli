(**
   Parsing of player commands.
*)

(** The type [word] represents the word that follows the command. *)
type word = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a word. *)
type command = 
  | Create of word
  | Quit
  | Pass
  | Swap of Game.letter
  | Steal of int * word * word

(** The type [check] represents a player command when checking other's words 
    list that is decomposed into a verb and possibly a word. *)
type check =
  | Valid
  | Invalid of word list

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** Raised when the player attempts to create a single letter word. *)
exception SingleChar

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The next word, if any, become the word.
    Examples: 
    - [parse "    create   word  "] is [Create ["word"]]
    - [parse "quit"] is [Quit]
    - [parse "pass"] is [Pass].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [SingleChar] if the [str] contains only one letter.
    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "pass", "quit", nor "create",
    or if the verb is "quit" or "pass" and there is a word following,
    or if the verb is "create" and there is no word following. *)
val parse : string -> command

(** [parse_number] is similar to [parse]; parses number of players at start. *)
val parse_number : string -> int

(** [parse_check] is similar to [parse]; parses commands in check phase. 
    Examples:
    - [parse_check "invalid aa bb cc"] is [Invalid ["aa"; "bb"; "cc"]]
    - [parse_check "valid"] is [Valid]. *)
val parse_check : string -> check