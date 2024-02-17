open List
let rec doit (p, c, a, s, n, ni) =
	if p = 1000.
	then [n; ni]
	else
		if c = p
		then 
			if s > n
			then doit(p+.1., 1., a, 0, s, int_of_float(p))
			else doit(p+.1., 1., a, 0, n, ni)
		else
			if a = p -. c
			then doit(p, c+.1., 1., s, n, ni)
			else
				let b = p -. c -. a in
				if a**2. +. b**2. = c**2.
				then doit(p, c, a+.1., s+1, n, ni)
				else doit(p, c, a+.1., s, n, ni)
let a = doit(1., 1., 1., 0, 0, 0)
let b = print_int (nth a 0); print_int (nth a 1)
