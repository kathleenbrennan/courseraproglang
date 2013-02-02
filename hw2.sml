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
	
(* BUG:  This is returning too short of a list when there are more than one list that match, fix it *)
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

(*
rite a function officiate, which \runs a game." It takes a card list (the card-list) a move list
(what the player \does" at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally dened recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
 The game starts with the held-cards being the empty list.
 The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
 If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
 If the player draws and the card-list is empty, the game is over. Else if drawing causes the sum of
the held-cards to exceed the goal, the game is over. Else play continues with a larger held-cards
and a smaller card-list.
*)

fun officiate(cards, moves, goal) =
	let fun make_move(hcs, cs,mvs) =
		case (hcs, cs, mvs) of
			(* no moves left *)	
			(_, _, []) => score(cs, goal) 
			(* some moves left, card-list empty 
			| (_, [], mv_hd::mv_tl) => 
				case mv_hd of
					Draw => score(cs, goal)
					Discard i => 7777(*raise IllegalMove  can't discard when cards are empty *)*)
			(* some moves left, card-list not empty *)
			| (_, card_hd::card_tl, mv_hd::mv_tl) => 7777
				(*case mv_hd of
					Draw =>
						if sum_cards(cs) > goal 
						then score(cs, goal)
						else make_move(card_tl, mv_tl)
					| Discard i =>
						remove_card(cs, card_hd, IllegalMove)
						make_move(card_tl, mv_tl)
						*)
	
	in
		make_move([], cards, moves)
	end


			
	

	
	
	
