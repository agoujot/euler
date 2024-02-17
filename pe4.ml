open Lib
let rec b (l1, l2, h) =
	if l1 = 1000
	then 
		if l1*l2 > h && pal (string_of_int(l1*l2), 0) = true
		then b (100, l2 + 1, l1*l2)
		else b (100, l2 + 1, h)
	else
		if l2 = 1000
		then h
		else 
		  if l1*l2 > h && pal (string_of_int(l1*l2), 0) = true
		  then b (l1+1, l2, l1*l2)
		  else b(l1+1, l2, h)
let a = b(100, 100, 0)
let c = print_int a
