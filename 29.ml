let rec inl (x, l) =
	match l with
	| [] -> false
	| i::s -> i = x || inl(x, s)
let rec prime (x, l) =
  if l > int_of_float (sqrt(float_of_int x))
  then true
  else
    if x mod l = 0 && prime(l, 2)
    then false
    else prime (x, l+1)
let rec factors (x, l, f)=
  if x = 1
  then f
  else
    if x mod l = 0 && prime(l, 2)
    then factors(x/l, l, [l]@f)
    else factors(x, l+1, f)
let rec p(l, n) =
	match n with
	| 0 -> []
	| x -> l@p(l, n-1)
let tri l = List.sort compare l
let rec di(a, b) =
	match (a, b) with
	| (101, _) -> []
	| (_, 101) -> di(a+1, 2)
	| (_, _) -> begin
		let v = tri(p(factors(a, 2, []), b))
		and w = di(a, b+1) in
		if inl(v, w)
		then w
		else [v]@w
		end
let a = di(2, 2)
let _ = print_int(List.length a)
