num = 1
t = 'string'
s = 'other string'
u = [[
multi-line strings
another line
]]
undefined = nil

while num<50 do
	num = num+1
end

if num>40 then
	print('over 40')
	elseif s ~= 'walternate' then -- ~= not equal
		io.write('not over 40')
		else
			-- variables are global by default.
			thisIsGloal = 5 --Camel case is common
			-- make variable local
			local line = io.read() -- reads next stdin line.
			-- String concatenation use the .. operator
			 print('Hello'..line)
		 end

 foo = unknowVariable -- Now foo = nil

 aBoolValue=false
 -- Only nil and false are falsy; 0 and '' are true!
 if not aBoolValue then print('not false') end

 -- 'or' and 'and' are short-circuited
 -- This is similar to the a?b:c in JS
 ans1 = false and 'yes'
 print(ans1) -- false
 ans2 = true and 'yes' 
 print(ans2) -- true

 ans3 =  false or 'no'
 print(ans3) -- 'no'

 counter = 0
 v = {'1','2','3'}
 print(v[0]) -- nil, which means the list indices start at 1!
 print('#v',#v) -- 3
 print('#_G',#_G) -- 0

 for i=0, #v do
	counter = counter+1
	print(v[i])
end
print('counter',counter)

metafraction = {}
metafraction.__add = function(t1,t2)
	print('adding two metaable')
	return  t1.a+t2.a
end

t1 = {a=1}
t2 = {a=1223}

setmetatable(t1, metafraction)
print(getmetatable(t1))

print(t1+t2)

-- print(counter.__metatable)
print(getmetatable(counter))