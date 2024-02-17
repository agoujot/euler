open Lib
let rec t (x, c, n) =
	if c = 10001
	then n
	else
    	if prime(x, 2) = 1
    	then t (x+1, c+1, x)
    	else t (x+1, c, n)
let a = t (2, 0, 1)
let b = print_int a
