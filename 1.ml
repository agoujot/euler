let rec som (x, y) = 
  if x = 0 
  then y 
  else
    if (x mod 3 = 0 || x mod 5 = 0) 
    then som (x-1, y + x) 
    else som (x-1, y)
let a = som(999, 0)
let b = print_int a