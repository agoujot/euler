let rec f (a, b, s) = 
  if b >= 4000000 
  then s
  else
    if b mod 2 = 0
    then f(b, a+b, s+b)
    else f(b, a+b, s)
let a = f (1, 1, 0)
let p = print_int a