(* year/month/day *)
fun is_older (date1 : int*int*int, date2: int*int*int) =
	if #1 date1 < #1 date2 
	then true
	else 
		if #2 date1 < #2 date2
		then true
		else #3 date1 < #3 date2
			
(* year/month/day *)		
fun number_in_month (dates : (int*int*int) list, month: int) = 
	if null dates
	then 0
	else
		if (#2 (hd dates) =  month)
			then 1 + number_in_month(tl dates, month)
			else number_in_month(tl dates, month)
	
					
fun number_in_months (dates : (int*int*int) list, months: int list) =
	if null months then 0
	else
		number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month: int) = 
	if null dates
	then []
	else
		if (#2 (hd dates) =  month)
			then hd dates :: [] @ dates_in_month(tl dates, month) 
			else dates_in_month(tl dates, month) 

fun dates_in_months (dates : (int*int*int) list, months: int list) = 
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months) 
	
fun get_nth(strings: string list, n: int) = 
	if null strings orelse n = 0 then ""
	else 
		if n = 1 then hd strings
		else get_nth(tl strings, n-1);
	
(* year/month/day *)
fun date_to_string (date: (int*int*int)) =
	let val month = get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date)
	in month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end;	

fun number_before_reaching_sum(sum : int, numbers: int list) =
	let fun loop(index : int, workingnumbers: int list, running_sum: int) =
		if running_sum >= sum then index 
		else	
			let fun add_to_head(number: int, numbers: int list) = 
				if null numbers then number
				else hd numbers + number
			in
				loop(index+1, tl workingnumbers, add_to_head(running_sum, tl workingnumbers))
			end
	in
		loop(0, numbers, hd numbers)					
	end
	
fun what_month(dayofyear: int) = 
	number_before_reaching_sum(dayofyear, [31,28,31,30,31,30,31,31,30,31,30,31]) + 1

fun month_range(day1: int, day2: int) = 
	(*assume day2 is greater than day1*)
	if(day1=day2)
		then what_month(day1) :: [] 
	else
		what_month(day1) :: month_range(day1+1, day2)
	
fun oldest(dates : (int*int*int) list) =
	if null dates then NONE
	else
		let val tl_oldest = oldest(tl dates)
		in
			if isSome tl_oldest
				andalso is_older(valOf tl_oldest, hd dates)
			then tl_oldest
			else SOME(hd dates)
		end
	
