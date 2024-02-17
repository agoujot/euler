open Lib
let rec so (x, s) =
	if x = 2000000
	then s
	else
		if prime (x, 2) = 1
		then so (x+1, s+x)
		else so (x+1, s)
let a = so (2, 0)
let () = print_int a
