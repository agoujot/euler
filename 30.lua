function get(st, i)
	return st:byte(i)-48
end
function test(n)
	local s = 0;
	local x = n; 
	while x > 0 do
		s = s + (x % 10)^5;
		x = x // 10
	end
	return s == n
end
so = 0;
for n = 2, 10^6 do
	if test(n) then so = so + n end
end
print(so)
