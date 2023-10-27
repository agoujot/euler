open List
let rec factors (x, l, f)=
	if x = 1
	then f
	else
		if x mod l = 0 && prime(l, 2) = 1
		then factors(x/l, l, [l]@f)
		else factors(x, l+1, f)
and prime (x, l)=
	if l > int_of_float (sqrt (float_of_int x))
	then 1
	else
		if x mod l = 0 && prime(l, 2) = 1
		then 0
		else prime (x, l+1)
and red(fa, fb, i) =
	if i = length fa || i = length fb
	then [ti(fa); ti(fb)]
	else 
		if inl(nth fa i, fb)
		then red(rem((nth fa i), fa, []), rem((nth fa i), fb, []), i)
		else red(fa, fb, i+1)
and inl (x, l) =
  match l with
  | [] -> false
  | i::s -> i = x || inl (x, s)
and ti(l) =
	match l with
	| [] -> 1
	| i::s -> i*ti(s)
and rem(x, l, n) =
	match l with
	| [] -> n
	| i::s -> if i = x then n@s else rem(x, s, n@[i])
let rec p(a, b) =
	if a = 100
	then p(10, b+1)
	else
		if b = 100
		then 0
		else let c = string_of_int a in let d = string_of_int b in let e = float_of_int(a) in let f = float_of_int(b) in let conv x = float_of_int(int_of_char(x)) -. 48. in
			if (c.[0] = d.[0] && c.[0] <> '0' &&  e/.f = conv c.[1] /. conv d.[1] && e/.f < 1.)
			|| (c.[0] = d.[1] && c.[0] <> '0' &&  e/.f = conv c.[1] /. conv d.[0] && e/.f < 1.)
			|| (c.[1] = d.[0] && c.[1] <> '0' &&  e/.f = conv c.[0] /. conv d.[1] && e/.f < 1.)
			|| (c.[1] = d.[1] && c.[1] <> '0' &&  e/.f = conv c.[0] /. conv d.[0] && e/.f < 1.)
			then let () = print_int a in let () = print_int b in let () =  print_string ";" in p(a+1, b)
			else p(a+1, b)
let a = p(10, 10)
