let rec pal (s, i) =
	if i = String.length(s)
	then true
	else
    	if s.[i] = s.[String.length(s)-1-i]
	    then pal (s, i+1)
	    else false
let rec ctb x =
	if x = 0
	then ""
	else
		let r = x mod 2 in
		let q = x/2 in
		ctb q ^ string_of_int r 
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
