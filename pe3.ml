open Lib
let rec test (x, l, h) =
	if x = 1
	then h
	else
		if x mod l = 0 && prime(l, 2) = 1
		then test (x/l, l/l+1, l)
		else test (x, l+1, h)
let a = test (600851475143, 2, 1)
let b = print_int a
