let rec prime (x, l)=
  if l > int_of_float (sqrt(float_of_int x))
  then true
  else
    if x mod l = 0 && prime(l, 2)
    then false
    else prime (x, l+1)
let rec di(a, b, i, ma, mb, mi) =
	if a > 1000
	then ma*mb
	else
		if b = 1001
		then di(a+1, -1000, 0, ma, mb, mi)
		else 
			let n = int_of_float(float_of_int(i)**2.) + a * i + b in
			if n > 0 && prime(n, 2)
			then 
				if i > mi
				then di(a, b, i+1, a, b, i)
				else di(a, b, i+1, ma, mb, mi)
			else di(a, b+1, 0, ma, mb, mi)
let a = di(-999, -1000, 0, 0, 0, 0)
let _ = print_int a
