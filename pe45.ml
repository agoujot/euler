let t x = x*(x+1)/2
let p x = x*(3*x-1)/2
let h x = x*(2*x-1)
let rec tb x =
	if x > 89600
	then []
	else [t x]@tb(x+1)
let rec pb x =
	if x > 51800
	then []
	else [p x]@pb(x+1)
let rec hb x =
	if x > 44600
	then []
	else [h x]@hb(x+1)
let tri = Array.of_list(tb 286)
let pen = Array.of_list(pb 166)
let hex = Array.of_list(hb 144)
let lt = Array.length tri -1
let lp = Array.length pen -1
let lh = Array.length hex -1
let rec bsearch (x, i, j, ar) =
	if i = j
	then if ar.(i) = x
		then true
		else false
	else
		let m = (i+j)/2 in
		if x < ar.(m)
		then bsearch (x, i, m, ar)
		else
			if x = ar.(m)
			then true
			else bsearch (x, m+1, j, ar)
let rec di i =
	if i = 89600
	then print_endline "not enough :("
	else
		if bsearch(tri.(i-286), 0, lp, pen) && bsearch(tri.(i-286), 0, lh, hex)
		then (print_int tri.(i-286); print_endline "yippee :)")
		else di(i+1)
let _ = di 286
