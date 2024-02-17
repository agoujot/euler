open Lib
let rec s (n, so) =
	if n = 1000000
	then so
	else
		let b = ctb(n) in
		if pal(b, 0) && pal(string_of_int(n), 0)
		then s(n+1, so+n)
		else s(n+1, so)
let a = s(0, 0)
let _ = print_int a
