(* Lambda Calculus *)

type term =
    Var of string
  | Abs of string * term
  | App of term * term

(* Some wrapper functions for term constructors. *)

let var s   = Var(s)
let abs s t = Abs(s, t)
let app t u = App(t, u)

let rec term_to_string = function
    Var(s)    -> s
  | Abs(s, t) -> "\\" ^ s ^ ". " ^ (term_to_string t)
  | App(t, u) -> "(" ^ (term_to_string t) ^ ") (" ^ (term_to_string u) ^ ")
