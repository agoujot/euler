let rec tri (a, b, c) =
	if a = 1001.
	then tri(1., b+.1., c)
	else
		if b = 1001.
		then tri(a, 1., c+.1.)
		else
			if c = 1001.
			then 0.0
			else
				if a +. b +. c = 1000. && a**2. +. b**2. = c**2.
				then a *. b *. c
				else tri(a+.1., b, c)
let a = tri(1., 1., 1.)
let _ = print_float a
