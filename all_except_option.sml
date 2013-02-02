fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(x, xs) =
	let fun all_except_list (xs,found,acc) = 
		case (xs,found) of
			(_,true) => (acc,true)
			| ([], false) => (acc, false)	
			| (x'::[], false) => 
				if same_string(x', x) 
				then (acc,true)
				else (acc, false)
			| (x'::xs',false) => 
				if same_string(x', x)
					then (xs',true)
					else all_except_list(xs',false,x'::acc) 
						
					(*(xs', false orelse all_except_list(xs'))  todo: fix condition *)
		in
			case all_except_list(xs,false,[]) of
				(_, false) => NONE
				| ([], true) => SOME([])
				| (hd::tl, true) => SOME(hd::tl)
				
		end
