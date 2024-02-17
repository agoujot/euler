open Lib
let rec p(l, n) =
	match n with
	| 0 -> []
	| x -> l@p(l, n-1)
let tri l = List.sort compare l
let rec di(a, b) =
	match (a, b) with
	| (101, _) -> []
	| (_, 101) -> di(a+1, 2)
	| (_, _) -> 
 		begin
		let v = tri(p(factors(a, 2, []), b))
		and w = di(a, b+1) in
		if List.mem v w
		then w
		else [v]@w
		end
let a = di(2, 2)
let _ = print_int(List.length a)
