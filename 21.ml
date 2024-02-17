open Lib
let rec s(i, so) =
	if i = 10000
	then so
	else 
		let sa = su(div(i, 2, [1])) in
		let sb = su(div(sa, 2, [1])) in
		if sb = i && sa <> i
		then s(i+1, so+i)
		else s(i+1, so)
let a = s(0, 0)
let _ = print_int a
