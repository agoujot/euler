open String
let s l = List.sort Stdlib.compare l
let p = [1;2;3;4;5;6;7;8;9]
let rec printl l =
	match l with
	| [] -> print_endline ""
	| i::s -> print_int i; print_string "; "; printl s
let rec s_l (s, i, l)=
	if i = length s
	then l
	else s_l (s, i+1, l@[int_of_char(s.[i]) - 48]) 
let rec di (c, a, so) =
	if c = 10000
	then print_int so
	else
		if a = c
		then di (c+1, 12, so)
		else
			if c mod a = 0
			then 
				let b = c/a in
				let str = string_of_int a ^ string_of_int b ^ string_of_int c in
				let li = s_l(str, 0, []) in
				let sl = s li in
				if sl = p
				then di(c+1, 12, so+c)
				else di(c, a+1, so)
			else di(c, a+1, so)
let a = di(2000, 12, 0)
