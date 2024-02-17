function prime(n)
	d = 2;
	while d <= math.sqrt(n) do
		if n % d == 0 then return false end;
		d = d + 1
	end
	return true
end
assert(prime(2));
assert(prime(3));
assert(not prime(4))
function sieve(n)
	s = {};
	for x = 2, n do
		if prime(x) then table.insert(s, x) end
	end
	return s
end
function pt(t)
	for i = 1, #t do 
		print(t[i])
	end
end
