(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* 	do not use the # character
	do not need to write down explicit types - use pattern matching
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(x : string, xs: string list) =
	let fun all_except_list (xs) = 
		case xs of
			[] => []
			| x'::xs' => 
				if x' = x
					then xs'
					else x'::all_except_list(xs')
		in
			case all_except_list(xs) of
				[] => NONE
				| y::ys => SOME (all_except_list(xs))
		end
		
fun get_substitutions(substitutions: string list list, s: string) =
	case substitutions of
		[] => []
		| head::tail =>
			case all_except_option(s, head) of
							NONE => get_substitutions(tail, s)
							| SOME mylist => 
								if mylist = head
								then get_substitutions(tail, s)
								else mylist @ get_substitutions(tail, s)
	
fun get_substitutions2(substitutions: string list list, s: string) =
	case substitutions of
		[] => []
		| head::tail =>
			let fun aux(xs, acc) =
				case xs of
					[] => acc
					| x::xs' => aux(xs, x::acc)				
				in
					case all_except_option(s, head) of
							NONE => get_substitutions(tail, s)
							| SOME mylist => 
								if mylist = head
								then
									get_substitutions(tail, s)
								else
									aux(mylist,[])
				end
	
(*
Write a function similar_names, which takes a string list list of substitutions  and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using subtitutions and get_substitutions2
*)
fun similar_names(substitutions: string list list, full_name: {first:string,middle:string,last:string}) =
	(*let val similar_first = get_substitutions2(substitutions, first)*)
	let val {first=x,middle=y,last=z} = full_name
	in
		(*get_substitutions2(substitutions, x)*)
		full_name
	end
	
fun record(full_name: {first:string,middle:string,last:string}) =
	(*let val similar_first = get_substitutions2(substitutions, first)*)
	let val {first=x,middle=y,last=z} = full_name
	in
		(*get_substitutions2(substitutions, x)*)
		x
	end

	
	
(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
