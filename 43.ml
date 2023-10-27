let p = [|2;3;5;7;11;13;17|]
let so = [|0|]
let rec ins (s, c, i) =
	if i = String.length s
	then false
	else
		if s.[i] = c
		then true
		else ins(s, c, i+1)
let rec c(n, i) =
	if i = 8
	then true
	else
		if int_of_string(String.sub n i 3) mod p.(i-1) = 0
		then c(n, i+1)
		else false
let rec b(i, j, n) =
	if i = 10
	then 
		begin
		if c(n, 1)
		then so.(0) <- so.(0) + int_of_string(n)
		end
	else
		if j < 10
		then 
			begin
			b(i, j+1, n);
			if not (ins(n, (string_of_int(j)).[0], 0)) 
			then b(i+1, 0, n^string_of_int(j))
			end
let a = b(0, 0, "")
let _ = print_int so.(0)
