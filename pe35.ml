open String
open Lib
let rec ci (s, i) =
	let l = length s in
	if i = l
	then true
	else
		let ns = (sub s i (l-i)) ^ (sub s 0 i) in
		if prime (int_of_string(ns), 2)
		then ci (s, i+1)
		else false 
let rec di (i, c) =
	if i = 1000000
	then c
	else 
		if ci(string_of_int(i), 0)
		then di(i+1, c+1)
		else di(i+1, c)
let a = di(2, 0)
let _ = print_int a
