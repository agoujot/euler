function prime(n)
	local d = 2;
	while d <= math.sqrt(n) do
		if n % d == 0 then return false end;
		d = d + 1
	end
	return true
end
assert(prime(2));
assert(prime(3));
assert(not prime(4));
function sieve(n)
	local s = {};
	for x = 2, n do
		if prime(x) then table.insert(s, x) end
	end
	return s
end
function pf(n)
	local si = sieve(n);
	local f = {};
	local x = n;
	for _, p in pairs(si) do
		if x == 1 then break end
		if x % p == 0 then 
			if f[p] == nil 
			then f[p] = 1 
			else f[p] = f[p] + 1 end; x = x // p
		end
	end
	return f
end
function cf(n)
	local si = sieve(n);
	local c = 0;
	local x = n;
	for _, p in pairs(si) do
		if x == 1 then break end
		if x % p == 0 then c = c + 1; x = x // p
		end
	end
	return c
end
function pt(t)
	for k, v in pairs(t) do
		print(k, v)
	end
end
function get(st, i)
	return st:byte(i)-48
end
function pandigital(st)
	local t = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
	for i = 1, #st do 
		t[get(st, i)+1] = t[get(st, i)+1] + 1
	end
	local ok = t[1] == 0;
	for i = 2, 10 do 
		if t[i] ~= 1 then ok = false end
	end
	return ok
end
function uniq(t)
	local r = {};
	local seen = {};
	for _, v in pairs(t) do
		if not seen[v] 
		then seen[v] = true; table.insert(r, v)
		end
	end
	return r
end
