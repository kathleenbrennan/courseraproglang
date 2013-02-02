(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(x, xs) =
	let fun all_except_list (xs) = 
		case xs of
			[] => []
			| x'::xs' => 
				if same_string(x', x)
					then xs'
					else x'::all_except_list(xs')
		in
			case all_except_list(xs) of
				[] => NONE
				| y::ys => SOME (all_except_list(xs))
		end
		
fun get_substitutions(substitutions, s) =
	case substitutions of
		[] => []
		| head::tail =>
			case all_except_option(s, head) of
							NONE => get_substitutions(tail, s)
							| SOME mylist => 
								if mylist = head
								then get_substitutions(tail, s)
								else mylist @ get_substitutions(tail, s)
	
(* BUG:  This is returning incorrect results when there are more than one list that match, fix it *)
fun get_substitutions2(substitutions,s) =
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
				
	
fun similar_names(substitutions, full_name) =
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
				[] => full_name::[]
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

(* put your solutions for problem 2 here *)

fun card_color(c) =
	let val (mysuit, myrank) = c
	in 
		case mysuit of
			Clubs => Black
			| Spades => Black
			| _ => Red
	end
	
fun card_value(c) =
	let val (mysuit, myrank) = c
	in 
		case myrank of
			Ace => 11
			| Num i => i
			| _ => 10
	end

fun remove_card(cs, c, e) =
	(* todo: change to use accumulator *)
	let fun all_except_list (xs) = 
			case xs of
				[] => []
				| x'::xs' => 
					if x' = c
						then xs'
						else x'::all_except_list(xs')
	in
		let val all_except = all_except_list(cs) 
			in
				if all_except = cs
				then raise IllegalMove
				else all_except
			end
	end
	
fun all_same_color(cs) =
	case cs of
	[] => true
	| x::[] => true
	| x::y::[] => card_color(x) = card_color(y)
	| x::y::xs => card_color(x) = card_color(y) andalso all_same_color(y::xs) 
	
fun sum_cards(cs) = 
	let fun sum_values (xs,acc) =
		case xs of
			[] => acc
			| x::xs' => sum_values(xs', card_value(x)+acc)
		in
			sum_values(cs,0)
		end

fun score(cs, goal) =
	let val sum = sum_cards(cs)
	val prelim_score = if(sum > goal)
						then 3 *(sum - goal)
						else (goal - sum)	
	in
		if(all_same_color(cs))
		then prelim_score div 2
		else prelim_score
	end

fun officiate(cards, moves, goal) =
	let fun make_move(hcs, cs, mvs) =
		case (hcs, cs, mvs) of
			(* no moves left *)	
			(_, _, []) => score(cs, goal)  
			
			(* card list empty *)
			| (_, [], Draw::mv_tl) => score(cs, goal)
			
			(* card list empty after drawing *)
			| (_, cs_hd::[], Draw::mv_tl) => score(cs, goal)
			
			(* card list not empty *)
			| (_, cs_hd::cs_tl, Draw::mv_tl) => 
				let val new_hcs = cs_hd::hcs (* append head of card list as drawn card to held cards *)
				in
					if sum_cards(new_hcs) > goal
						then score(new_hcs, goal)
						else make_move(new_hcs, cs_tl, mv_tl) (* recurse to next move with larger held-cards and smaller card-list *)
				end
			(* on discarding, remove discarded from held cards and recurse, card list unchanged *)
			| (_, _, Discard i::mv_tl) => 
				make_move(remove_card(cs, i, IllegalMove), cs, mv_tl)
	in
		make_move([], cards, moves)
	end


	
	
	
