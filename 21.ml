let rec su l = 
	match l with
    | [] -> 0
	| i::s -> i + su s

let rec div(x, i, l) = 
	if float_of_int i > sqrt(float_of_int x)
	then l
	else
		if x mod i = 0
		then div(x, i+1, l@[i; x/i])
		else div(x, i+1, l)
let rec s(i, so) =
	if i = 10000
	then so
	else 
		let sa = su(div(i, 2, [1])) in
		let sb = su(div(sa, 2, [1])) in
		if sb = i && sa <> i
		then s(i+1, so+i)
		else s(i+1, so)
let a = s(0, 0)
let _ = print_int a
