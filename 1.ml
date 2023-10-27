let rec som x = 
	if x = 0
	then 0
	else
		if (x mod 3 = 0 || x mod 5 = 0) 
		then x+som(x-1) 
		else som (x-1)
let a = print_jnt som(999)
