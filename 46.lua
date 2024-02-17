require "lib"
si = sieve(10^5);
function test(n)
	if prime(n) then return false else
		for _, p in pairs(si) do
			if p >= n then break end;
			local d = n - p;
			if d % 2 == 0 then 
				local sq = math.sqrt(d/2);
				if math.floor(sq) == sq then 
					return false 
				end
			end
		end
	end
	return true
end
for n=3, 10^5, 2 do
	if test(n) then print(n); break end
end
