fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(x, xs) =
	let fun all_except_list (xs) = 
		case xs of
			[] => ([],true)
			| x'::xs' => 
				if same_string(x', x)
					then (xs',true)
					else 
						case all_except_list(xs') of
						
					(*(xs', false orelse all_except_list(xs'))  todo: fix condition *)
		in
			case all_except_list(xs) of
				(_, false) => NONE
				| ([], true) => SOME([])
				(*| (hd::tl, true) => SOME(hd::tl)*)
				
		end