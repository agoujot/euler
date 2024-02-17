let rec d (x, sq, su) = 
	if x = 101.
	then sq**2. -. su
	else d (x+.1., sq+.x, su+.x**2.)
let a = d (1., 0., 0.)
let () = print_float a
