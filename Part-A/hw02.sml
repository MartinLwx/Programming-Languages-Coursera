(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* Returns NONE if target is not in the list, else return SOME lst where lst is
* identical to the argument list except the string is not in. You may assume the
* string is in the list at most once *)
fun all_except_option(target, str_list) =
  case str_list of
       [] => NONE
      | x::xs' => 
          if same_string(target, x)
          then SOME xs'
          else
            let
              val res = all_except_option(target, xs')
            in
              case res of 
                   NONE => NONE
                 | SOME v => SOME(x::v)
            end


(* put your solutions for problem 2 here *)
(* Returns a string list which has all the strings that are in some list in
  * substitutions that also has s, but s itself should not be in the result *)
(* Assume each list in substitutions has no repeats. *)
fun get_substitutions1(substitutions, s) =
  case substitutions of
       [] => []
     | x::xs' =>
         let
           val list1 = all_except_option(s, x)      (* string list option *)
           val list2 = get_substitutions1(xs', s)   (* string list *)
         in
           case list1 of
                NONE => list2
              | SOME v => v @ list2
         end


(* get_substitutions1, Tail-recursive version. *)
fun get_substitutions2(substitutions, s) =
  let 
    fun helper(substitutions, acc) =
      case substitutions of
           [] => acc
         | x::xs' => 
             let
               val list1 = all_except_option(s, x)      (* string list option *)
             in
               case list1 of
                    NONE => helper(xs', acc)
                  | SOME v => helper(xs', acc @ v)
             end
  in
    helper(substitutions, [])
  end


(* Returns a list of full names. The result is all the full names you can
* produce by substituting for the first name(and only the first name) using
* substitutions *)
(* The answer should begin with the origin name(then have 0 or more other names) *)
(* Do not eliminate duplicates from the answer. *)
fun similar_names(substitutions, full_name) =
  let 
    val {first: string, middle: string, last: string} = full_name
    val choices = get_substitutions2(substitutions, first)
    fun helper(choices, name) =
      case choices of
           [] => []
         | x::xs' => [{first=x, middle=middle, last=last}] @ helper(xs', first)
  in
    [full_name] @ helper(choices, first)
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* Takes a card and returns its color. *)
fun card_color a_card =
  case a_card of
       (Spades, _)   => Black
     | (Clubs, _)    => Black
     | (Diamonds, _) => Red
     | (Hearts, _)   => Red


(* Takes a card and returns its value. *)
fun card_value a_card =
  case a_card of
       (_, Ace)    => 11
     | (_, Jack)   => 10
     | (_, Queen)  => 10
     | (_, King)   => 10
     | (_, Num(v)) => v


(* Returns a list that has all the elements of cards except a_card. *)
(* If a_card is in the list more than once, remove only the first one. *)
fun remove_card(cards, a_card, e) =
  case cards of
       [] => raise e
     | x::xs' => if a_card = x
                 then xs'
                 else x::remove_card(xs', a_card, e)


(* Takes a list of cards and returns true if all the cards in the list are the
* same coor *)
fun all_same_color(cards) =
  case cards of
       [] => true
     | _::[] => true
     | x::(y::xs') =>
         card_color(x) = card_color(y) andalso all_same_color(y::xs')


(* Returns the sum of their values. *)
fun sum_cards cards =
  let
    fun helper(cards, acc) =
      case cards of
           [] => acc
         | x::xs' => helper(xs', acc + card_value(x))
  in
    helper(cards, 0)
  end


(* Compute the score. *)
fun score(cards, goal) =
let
  val score_sum = sum_cards cards
  val preliminary_score = if score_sum > goal
                          then 3 * (score_sum - goal)
                          else goal - score_sum
in
  if all_same_color cards
  then preliminary_score div 2
  else preliminary_score
end


(* Returns the score at the end of the game after processing the moves in the
* move list in order. *)
(* Use a locally defined recursive helper function that takes several arguments
* that together represent the current state of the game *)
fun officiate(cards, moves, goal) =
let
  fun play(held_cards, cards, moves) =
    case moves of
         (* The game ends if there are no more moves. *)
         [] => score(held_cards, goal)
       | m::ms' => 
           case m of
                Discard(t) => play(remove_card(held_cards, t, IllegalMove), cards, ms')
              | Draw =>
                  case cards of
                       (* If the player draws and the card-list is (already)
                       * empty, the game is over. *)
                       [] => score(held_cards, goal)
                     | c::cs' => 
                         let
                           val new_held_cards = held_cards @ [c]
                         in
                           if sum_cards(new_held_cards) > goal
                           then score(new_held_cards, goal)
                           else play(new_held_cards, cs', ms')
                         end
in
  (* The game starts with the held-cards being the empty list. *)
  play([], cards, moves)
end
