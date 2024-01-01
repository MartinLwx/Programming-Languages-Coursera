(* A date is an SML value of type int * int * int. i.e., year, month and day*)


(* Let's define some utils first *)
fun year(date: int * int * int) =
  #1 date

fun month(date: int * int * int) =
  #2 date

fun day(date: int * int * int) =
  #3 date


(* Evaluates to true if the first argument is a date that comes before the second argument. 
  * If the two dates are the same, the result is false. *)
fun is_older(lhs: int * int * int, rhs: int * int * int) =
  year lhs < year rhs
  orelse year lhs = year rhs andalso month lhs < month rhs
  orelse
    year lhs = year rhs
    andalso month lhs = month rhs
    andalso day lhs < day rhs


(* Returns how many dates in the list are in the given month. *)
fun number_in_month(dates: (int * int * int) list, target_month: int) =
  if null dates
  then 0
  else
    let
      val remains = number_in_month(tl dates, target_month)
    in
      if month(hd dates) = target_month then 1 + remains else remains
    end


(* Returns the number of dates in the list of dates that are in any of the
  * months in the list of months. Assume the list of months has no number repeated
  * *)
fun number_in_months(dates: (int * int * int) list, target_months: int list) =
  if null target_months
  then 0
  else
    number_in_month(dates, hd target_months) + number_in_months(dates, tl target_months)


(* Returns a list holding the dates from the argument list of dates that are in
  * the month *)
fun dates_in_month(dates: (int * int * int) list, target_month: int) =
  if null dates
  then []
  else
    if month(hd dates) = target_month
    then hd dates :: dates_in_month(tl dates, target_month)
    else dates_in_month(tl dates, target_month)


(* Returns a list holding the dates from the argument list of dates that are in
  * any of the months in the list of months. Assume the list of months has no
  * number repeated *)
fun dates_in_months(dates: (int * int * int) list, target_months: int list) =
  if null target_months
  then []
  else
    dates_in_month(dates, hd target_months) @ dates_in_months(dates, tl target_months)


(* Returns the n-th element of the list where eht head of the list is 1st. Do
  * not worry about the case where the list has too few elements *)
fun get_nth(strings: string list, n: int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)


(* Returns a string of the form January 20, 2013 *)
fun date_to_string(date: int * int * int) =
let
  val months = ["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December" ]
in
  get_nth(months, month date) ^ " " ^ Int.toString(day date) ^ ", " ^ Int.toString(year date)
end


(* Returns an int n s.t. the first n elements of the list add to less than sum,
  * but the first n + 1 elements of the list add to sum or more *)
(* Assumptions:
  * sum > 0.
  * all elements in arr are positive.
  * the entire list sums to more than the passed in value. *)
fun number_before_reaching_sum(sum: int, arr: int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum(sum - hd arr, tl arr)


(* Returns what month that day is in *)
fun what_month(day: int) =
let
  val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
in
  number_before_reaching_sum(day, months) + 1
end


(* Returns an int list [m1, m2, ..., mn] where m1 is the month of day1, m2 is
* the month of day1 + 1, ..., and mn is the month of day day2. *)
fun month_range(day1: int, day2: int) =
  if day1 = day2
  then [what_month day2]
  else
    if day1 < day2
    then what_month day1 :: month_range(day1 + 1, day2)
    else []


(* Takes a list of dates and evaluates to an (int * int * int) option *)
fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else
    let
      val next_oldest = oldest(tl dates) (* This may returns NONE *)
    in
      if 
        isSome(next_oldest) andalso is_older(hd dates, valOf(next_oldest))
        orelse not(isSome(next_oldest))
      then SOME (hd dates)
      else next_oldest
    end
