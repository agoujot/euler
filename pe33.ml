open List
open Lib
let rec p(a, b) =
	if a = 100
	then p(10, b+1)
	else
		if b = 100
		then 0
		else let c = string_of_int a in let d = string_of_int b in let e = float_of_int(a) in let f = float_of_int(b) in let conv x = float_of_int(int_of_char(x)) -. 48. in
			if (c.[0] = d.[0] && c.[0] <> '0' &&  e/.f = conv c.[1] /. conv d.[1] && e/.f < 1.)
			|| (c.[0] = d.[1] && c.[0] <> '0' &&  e/.f = conv c.[1] /. conv d.[0] && e/.f < 1.)
			|| (c.[1] = d.[0] && c.[1] <> '0' &&  e/.f = conv c.[0] /. conv d.[1] && e/.f < 1.)
			|| (c.[1] = d.[1] && c.[1] <> '0' &&  e/.f = conv c.[0] /. conv d.[0] && e/.f < 1.)
			then let () = print_int a in let () = print_int b in let () =  print_string ";" in p(a+1, b)
			else p(a+1, b)
let a = p(10, 10)
