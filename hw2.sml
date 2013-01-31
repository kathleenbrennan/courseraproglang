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
	
(* BUG:  This is returning too short of a list when there are more than one list that match, fix it *)
fun get_substitutions2(substitutions: string list list, s: string) =
	case substitutions of
		[] => []
		| head::tail =>
			let fun aux(xs, acc) =
				case xs of
					[] => acc
					| x::xs' => aux(xs', x::acc)				
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
				
	
fun similar_names(substitutions: string list list, full_name: {first:string,middle:string,last:string}) =
	let 
		val {first=x,middle=y,last=z} = full_name
		(* NOTE: The accumulator is reversing the order of the list, the homework didn't say whether that mattered *)
		fun aux(xs, acc) =
					case xs of
						[] => acc
						| x'::xs' => aux(xs', {first=x',middle=y,last=z}::acc)	
	in
		let val similar_firsts = get_substitutions(substitutions, x) 
		in 
			case similar_firsts of
				[] => []
				| x'::xs => full_name::aux(similar_firsts,[])
		end
	end

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here 

datatype mytype = TwoInts of int * int 
                | Str of string 
                | Pizza

fun f x = 
    case x of 
	Pizza => 3 
      | Str s => 8
      | TwoInts(i1,i2) => i1 + i2
	  
fun zip3 list_triple =
    case list_triple of 
	([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch	  
	  *)

fun card_color(card) =
	let val (mysuit, myrank) = card
	in 
		case mysuit of
			Clubs => Black
			| Spades => Black
			| _ => Red
	end
	
fun card_value(card) =
	let val (mysuit, myrank) = card
	in 
		case myrank of
			Ace => 11
			| Num i => i
			| _ => 10
	end