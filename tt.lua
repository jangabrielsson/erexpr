a = {1, 2, 3}
a = {b=1, d=2, f=3}

local gg = function() return pairs(a) end

local f,t,k,v = gg()

repeat
  k,v = f(t,k)
  if k then print(k,v) end
until not k