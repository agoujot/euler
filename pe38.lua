require "lib";
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
