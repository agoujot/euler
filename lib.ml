open List
open Printf
open String

let rec prime (x, l)=
  if l > int_of_float (sqrt(float_of_int x))
  then true
  else
    if x mod l = 0
    then false
    else prime (x, l+1)

let pris n =
	if n = 1
	then []
	else 
		if prime(n, 2)
		then [n]@pris(n-1)
		else pris(n-1)

let rec printl l = 
  match l with
  | [] -> false
  | i::s -> printf "%d, " i; printl s

let rec factors (x, l, f)=
  if x = 1
  then f
  else
    if x mod l = 0 && prime(l, 2)
    then factors(x/l, l, [l]@f)
    else factors(x, l+1, f)

let rec div(x, i, l) =
        if float_of_int i > sqrt(float_of_int x)
        then l
        else
                if x mod i = 0
                then
			if i = x/i
			then div(x, i+1, l@[i])
			else div(x, i+1, l@[i; x/i])
                else div(x, i+1, l)

let rec pal (s, i) =
  if i = String.length(s)
  then true
  else
    if s.[i] = s.[String.length(s)-1-i]
    then pal (s, i+1)
    else false

let rec inl (x, l) =
  match l with
  | [] -> false
  | i::s -> i = x || inl (x, s)

let rec ctb x =
	if x = 0
	then ""
	else
		let r = x mod 2 in
		let q = x/2 in
		ctb q ^ string_of_int r

let rec cfb (st, i, s) = 
	if i = String.length st
	then s
	else
		if st.[i] = '1'
		then cfb (st, i+1, s+int_of_float(2.**float_of_int((String.length st) - i - 1)))
		else cfb (st, i+1, s)

let rec pb (s, i, a) =
	let l = String.length s in
	if i = -1 && a = 0
	then pb ("1" ^ s, 0, a-1)
 	else
		if a = 0
		then s
		else
			if s.[i] = '0'
			then pb ((String.sub s 0 i) ^ "1" ^ (String.sub s (i+1) (l-i-1)), i, a-1)
			else pb ((String.sub s 0 i) ^ "0" ^ (String.sub s (i+1) (l-i-1)), i-1, a)

let rec fb (sa, sb, i, s) = 
	if i = -1
	then s
	else
		if sb.[i] = '1'
		then fb (sa, sb, i+1, s+cfb(sa, 0, 0))
		else fb (sa, sb, i+1, s)
let rec inv (s, i) =
	let l = String.length s in
	if i = l
	then s
	else 
		let begs = String.sub s 0 i in
		let ends = String.sub s (i+1) (l-i-1) in 
		inv (begs ^ (if s.[i] = '0' then "1" else "0") ^ ends, i+1) 

let rec su l  =
	match l with
	| [] -> 0
	| i::s -> i + su s

let rec ti l =
	match l with
	| [] -> 1
	| i::s -> i * ti s

let rec rem(x, l, r) =
	match l with
	| [] -> r
	| i::s -> if i = x
		then r@s
		else rem(x, s, r@[i])

let rec red(fa, fb, i) =
	if i = length fa || i = length fb
	then [float_of_int(ti(fa)); float_of_int(ti(fb))]
	else
		if inl(nth fa i, fb)
		then red(rem((nth fa i), fa, []), rem((nth fa i), fb, []), i)
		else red(fa, fb, i+1)

let rec bsearch (x, i, j) =
        if i = j
        then if ar.(i) = x
                then true
                else false
        else
                let m = (i+j)/2 in
                if x < ar.(m)
                then bsearch (x, i, m)
                else
                        if x = ar.(m)
                        then true
                        else bsearch (x, m+1, j)

let rec s_l (s, i, l)=
	if i = length s - 1
	then l
	else s_l (s, i+1, l@[int_of_char(s.[1]) - 30])

let tri l = sort Stdlib.compare l
