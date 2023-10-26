let rec inl (x, l)  =
	match l with
	| [] -> false
	| i::s -> i=x || inl(x, s)
let rec cyc (r, d, p) =
	if r = 0
	then 0
	else
		let vs = r*10 in
		let nr = vs mod d in
		if not (inl(nr, p))
		then cyc(nr, d, [nr]@p)
		else List.length p
let cy x = cyc (1 mod x, x, [1 mod x])
let rec di i =
	if i = 1000
	then 0
	else max (cy i) (di(i+1))
let a = di 1
let rec dia i =
        if i = 1000
        then 1000
        else
                if cy i = a
                then i
                else dia(i+1)
let _ = print_int a; print_string " "; print_int(dia 1); print_endline
