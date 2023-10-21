let rec countf (a, b, c) =
  if a mod b = 0
  then countf (a/b, b, c+1)
  else c
let rec comparec (l, b, maxi) =
  if l = 21
  then maxi
  else
    if countf (l, b, 0) > maxi
    then comparec(l+1, b, countf(l, b, 0))
    else comparec(l+1, b, maxi)
let rec ppcm (tot, b) =
  if b = 9
  then tot
  else 
    match b with
    | 1 -> ppcm(tot*.(2.**float_of_int(comparec(1, 2, 0))), 2)
    | 2 -> ppcm(tot*.(3.**float_of_int(comparec(1, 3, 0))), 3)
    | 3 -> ppcm(tot*.(5.**float_of_int(comparec(1, 5, 0))), 4)
    | 4 -> ppcm(tot*.(7.**float_of_int(comparec(1, 7, 0))), 5)
    | 5 -> ppcm(tot*.(11.**float_of_int(comparec(1, 11, 0))), 6)
    | 6 -> ppcm(tot*.(13.**float_of_int(comparec(1, 13, 0))), 7)
    | 7 -> ppcm(tot*.(17.**float_of_int(comparec(1, 17, 0))), 8)
    | 8 -> ppcm(tot*.(19.**float_of_int(comparec(1, 19, 0))), 9)
let a = ppcm(1., 1)
let b = print_float a (*14549535 55040*)