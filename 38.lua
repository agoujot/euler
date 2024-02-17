function get(st, i)
	return st:byte(i)-48
end
function pt(t) for i = 1, #t do print(t[i]) end end
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
most = 0;
assert(pandigital("123456789"))
for x = 1, 10000 do
	s = "";
	for n = 1, 8 do
		s = s .. (x * n);
		if #s >= 9 then break end
	end
	if #s == 9 and pandigital(s) then 
		if tonumber(s) > most then most = tonumber(s) end
	end
end
print(most)
