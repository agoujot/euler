let rec prime (x, l) =
	if l > int_of_float (sqrt (float_of_int x))
	then 1
	else
		if x mod l = 0 && prime(l, 2) = 1
		then 0
		else prime (x, l+1)
let rec so (x, s) =
	if x = 2000000
	then s
	else
		if prime (x, 2) = 1
		then so (x+1, s+x)
		else so (x+1, s)
let a = so (2, 0)
let () = print_int a
