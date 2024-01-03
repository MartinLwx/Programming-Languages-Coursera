(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	  val r = g f1 f2 
    in
      case p of
          Wildcard          => f1 ()
        | Variable x        => f2 x
        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
        | ConstructorP(_,p) => r p
        | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Returns a string list that has only the strings in the argument that start
* with an uppercase letter *)
fun only_capitals xs =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs


(* Returns the longest string in the string list. If the list is empty, return
* "". In the case of a tie, return the string closest to the beginning of the
* list *)
fun longest_string1 xs =
  List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) "" xs


(* This function is exactly like longest_string1 except in the case of ties it
* returns the string cloest to the end of the list *)
fun longest_string2 xs =
  List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) "" xs


(* This function will look a lot like longest_string1 and longest_string2 but is
* more general because it takes a function as an argument *)
fun longest_string_helper f xs =
  List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" xs


(* The longest_string3 and longest_string4 are defined with val-bindings and
* partial applications of longest_string_helper *)
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >=y)


(* Returns the longest string in the list that begins withe an uppercase letter,
* or "" if there are no such things. *)
val longest_capitalized = longest_string1 o only_capitals


(* Returns a string in reverse order *)
val rev_string = implode o List.rev o explode


(* Returns v at the first time that f(x) returns SOME v for x in the list *)
fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => case (f x) of
                      NONE => first_answer f xs'
                    | SOME v => v


(* If f(x) returns NONE for any element, then the result for all_answers is NONE
* Else the calls to the first argument will have produced SOME lst1, SOME lst2,
* ... and the result of all_answers is SOME lst where lst is lst1, lst2, ...,
* lstn appended together(order doesn't matter) *)
(* Note all_answers f [] should evaluate to SOME [] *)
fun all_answers f xs =
let
  fun helper acc rest =
    case rest of
         [] => SOME acc
       | x::xs' => case (f x) of
                        NONE => NONE
                      | SOME v => helper (acc @ v) xs'
in
  helper [] xs
end


(* Use g to define a function count_wildcards that takes a pattern and returns
* how many Wildcard patterns it contains. *)
fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p


(* Use g to define a function count_wild_and_variable_lengths that takes a
* pattern and returns the number of Wildcard patterns it contains plus the sum
* of the string lengths of all the variables in the variable patterns it
* contains. *)
fun count_wild_and_variable_lengths p =
  g (fn _ => 1) String.size p


(* Use g to define a function count_some_var that takes a string and a pattern
* (as a pair) and returns the number of times the string appears as a variable
* in the pattern. We care only about variable names; the constructor names are
* not relevant. *)
fun count_some_var (s, p) =
  g (fn _ => 0) (fn another_s => if s = another_s then 1 else 0) p


(* Write a function check_pat that takes a pattern and returns true if and only
* if all the variables appearing in the pattern are distinct from each other
* (i.e., use different strings). The constructor names are not relevant. *)
fun check_pat p =
  let
    (* Returns a list of all the strings it uses for variables. *)
    fun get_all_variables current_p =
      case current_p of
           Variable x         => [x]
         | TupleP ps          => List.foldl (fn (p, acc) => 
                                            get_all_variables(p) @ acc) [] ps
         | ConstructorP(_, p) => get_all_variables p
         | _                  => []

    (* Decides if it has repeats. *)
    fun has_repeats str_list =
      case str_list of
           [] => false
         | x::xs' => List.exists (fn t => t = x) xs' orelse has_repeats xs'
  in
    not(has_repeats(get_all_variables p))
  end


(* Write a function match that takes a valu * pattern and returns a (string *
* valu) list option, namely NONE if the pattern does not match and SOME lst
* where lst is the list of bindings if it does. Note that if the value matches
* but the pattern has no patterns of the form Variable s, then the result is
* SOME [] *)
(* Hints:
* Sample solution has one case expression with 7 branches.
* The branch for tuples uses all_answers and ListPair.zip. *)
fun match (v, p) =
  case (v, p) of
       (_, Wildcard)                                 => SOME []
     | (_, Variable s)                               => SOME [(s, v)]
     | (Unit, UnitP)                                 => SOME []
     | (Const(x), ConstP(y))                         => if x = y then SOME [] else NONE
     | (Tuple vs, TupleP ps)                         => all_answers match (ListPair.zip(vs, ps))
     | (Constructor(s1, v), ConstructorP(s2, p))     => if s1 = s2 then match(v, p) else NONE
     | (_, _)                                        => NONE


(* Write a function first_match that takes a value and a list of patterns and returns a
(string * valu) list option, namely NONE if no pattern in the list matches or
SOME lst where lst is the list of bindings for the first pattern in the list
that matches. Use first_answer and a handle-expression. *)
fun first_match value pattern_list =
  (first_answer (fn p => SOME(match(value, p))) pattern_list) handle NoAnswer => NONE
