--%%name:ER
--%%offline:true
--%%file:Lisp.lua,lisp
--%%file:erparser.lua,parser
--%%file:er2lisp.lua,transform

fibaro.EventRunner = fibaro.EventRunner or { debugFlags = {} }
local ER = fibaro.EventRunner 
local debugFlags = ER.debugFlags
local fmt = string.format
local function printf(...) print(string.format(...)) end

local YES = "✅"
local NO = "❌"

local function encode(...)
  local args = {...}
  for i,e in ipairs(args) do args[i] = (type(e)=='table' and json.encodeFast(e)) or e end
  return table.unpack(args)
end

local _marshalBool={['true']=true,['True']=true,['TRUE']=true,['false']=false,['False']=false,['FALSE']=false}
local function marshallFrom(v) 
  if v == nil then return nil end
  local fc = v:sub(1,1)
  if fc == '[' or fc == '{' then local s,t = pcall(json.decode,v); if s then return t end end
  if tonumber(v) then return tonumber(v)
  elseif _marshalBool[v ]~=nil then return _marshalBool[v ] end
  if v=='nil' then 
    return nil 
  end
  local test = v:match("^[0-9%$s]")
  if not test then return v end
  local s,t = pcall(toTime,v,true); return s and t or v 
end

function QuickApp:main()

  local lisp = Lisp()
  lisp:setVar('objcall',(function(obj,method,...)
    return obj[method](obj,...)
  end))
  lisp:setVar("globalVar",function(name) return marshallFrom(fibaro.getGlobalVariable(name)) end)
  lisp.setfs.globalVar = function(env, val, obj)fibaro.setGlobalVariable(obj,json.encodeFast(val)) return val end
  lisp:setVar("quickVar",function(name) return quickApp:getVariable(name) end)
  lisp.setfs.quickVar = function(env, val, obj) quickApp:setVariable(obj,val) return val end
  lisp:setVar("storeVar",function(name) return quickApp:internalStorageGet(name) end)
  lisp.setfs.storeVar = function(env, val, obj) quickApp:internalStorageSet(obj,val) return val end
  lisp.env.nonVarHandler = function(name) return _G[name] end

  local function eval(str,opts) return ER.ereval(lisp,str,opts) end

  local function runTests(tests)
    for i=1,#tests,3 do
      local src,expected,opts = tests[i],tests[i+1],tests[i+2]
      local res = {eval(src,opts)}
      if not table.equal(res,expected) then
        print("❌ Test failed:")
        print("Source:   ",src)
        print("Expected: ",json.encode(expected))
        print("Got:      ",json.encode(res))
      else
        print(fmt("%s '%s' =",YES,src),encode(table.unpack(res)))
      end
    end
  end

  api.post("/globalVariables", {name="Bar",value="77"})
  T1 = {a={b=42}}
  T2 = {2,3,4,5}
  XX = 1
  YY = 2
  C2,C3,C4,C5,C6,C7 = 2,3,4,5,6,7
  function MV3() return 5,6,7 end
  function Foo() return 5 end
  function A1(x,y) return x+y end
  Obj3 = {}
  function Obj3:foo(n) return n end

  local tests = {
    "local a = 8; return a",{8},nil,
    "local a,b,c = 2,3,4; return a+b+c",{9},{sexpr=true},
    "return T1.a",{ {b=42} },{sexpr=false},
    "return 42",{42},nil,
    "a = 42; return a",{42},nil,
    "return a",{42},nil,
    "return a,a",{42,42},nil,
    "return",{nil},nil,
    "return a+1",{43},nil,
    "return a*2",{84},nil,
    "return 6+a*2",{90},nil,
    "return a-2",{40},nil,
    "return a/2",{21},nil,
    "return a%5",{2},nil,
    "return a^2",{1764},nil,
    "return -a",{-42},nil,
    "return 01:00",{3600},nil,
    "return ! false",{true},nil,
    "return ! true",{false},nil,
    "return false | true",{true},nil,
    "return true | false",{true},nil,
    "return true & true",{true},nil,
    "return false & true",{false},nil,
    "return true & false",{false},nil,
    "return false | false | true",{true},nil,
    "return true & true & false",{false},nil,
    "return nil==nil",{true},nil,
    "return nil~=nil",{false},nil,
    "return false==false",{true},nil,
    "return true==true",{true},nil,
    "return 42==42",{true},nil,
    "return '42'=='42'",{true},nil,
    "return -77",{-77},{},
    "return 0- -77",{77},{},
    "return false | true",{true},nil,
    "return false & true",{false},nil,
    "return !(7 == 7)",{false},nil,
    "return 6 > 5",{true},{},
    "return 5 > 6",{false},{},
    "return 6 >= 5",{true},{},
    "return 5 <= 6",{true},{},
    "return T2[1]",{2},{},
    "return T2[5]",{nil},{},
    "a=2;return a+-1",{1},{},
    "a=2;return -a+3",{1},{},
    "a=2;return a-1",{1},{},
    "a=2;return 1-a",{-1},{},
    "a=2;return 4/a",{2},{},
    "a=4;return a/2",{2},{},
    "a=4;return a*2",{8},{},
    "a=4;return 2*a",{8},{},
    "a=2;return a+1+1",{4},nil,
    "a=2;return (a+1)+1",{4},nil,
    "a=2;return (a+1)+1,9",{4,9},{newEnv=true},
    "a=8;return (a+1)*6",{54},nil,
    "a=8;return a % 3",{2},nil,
    "a=2;return a ^ 3",{8},nil,
    "return 7,8",{7,8},nil,
    "return MV3()",{5,6,7},nil,
    "return 5+6*2",{17},nil,
    "a=99; return a",{99},nil,  -- Surving toplevel environment
    "return a",{99},nil,
    "local a,b,c = 2,3,4; return a+b+c",{9},{sexpr=true},
    "function foo(a,b) return a+b,6 end; return foo(8,9)",{17,6},nil,
    "return T1.a.b",{42},{sexpr=false},
    "return T1['a'].b",{42},nil,
    "b = {}; return b",{{}},{},
    "a=1; b = {[a+1]=9}; return b",{{[2]=9}},{sexpr=false},
    "a=1; return {[a+1]=9}",{{[2]=9}},{},
    "return {a=9,b=8}",{{a=9,b=8}},{},
    "return 4+6",{10},{},
    "if true then return 42 end",{42},{},
    "if false then return 17 else return 42 end",{42},{},
    "if false then return 17 elseif true then return 42 end",{42},{},
    "b=1; while b<=3 do b=b+1; end; return b",{4},{newEnv=true},
    "c=0; for a=1,3 do c+=1 end; return c",{3},{newEnv=true},
    "b=1; while b<=3 do b=b+1; break end; return b",{2},{newEnv=true},
    "b=0; repeat b=b+1 until b>3; return b",{4},{newEnv=true},
    -- "b=0 do b=1; break; b=2 end; return b",{1},{tree=false,newEnv=true},
    "a=1; a+=4; return a",{5},{tree=false},
    "a=1; a-=4; return a",{-3},{},
    "T1.a.b = 5; return T1.a.b",{5},{},
    "a,b=C2+1,C2+2;return a+b",{7},{},
    "T1.a.b=88;return T1",{{a={b=88}}},{},
    "T1.a.b=C2;return T1",{{a={b=2}}},{},
    "a,T1.a.b=C2+1,C2;return T1.a.b+a",{5},{},
    "a,b,c=C5,C6,C7;return a+b+c",{18},{},
    "a,b,c=MV3();return a+b+c",{18},{},
    "a,b,c=C5,MV3();return a+b+c",{16},{},
    "local a,b,c = 5,MV3(); return a+b+c",{16},{},
    "function bar(a,b) return a+b end; return 5",{5},{sexpr=false},
    "return bar(2,3)",{5},{},
    "return (function(a,b) return a*b end)(2,3)",{6},{},
    "return (function(...) a = {...} return a end)(2,3)",{{2,3}},{tree=false},
    "return (function(a,...) b = ({...})[1] return a+b end)(2,3)",{5},{},
    -- "do local a=7; return a+3 end",{10},{},
    -- "do local a=8; return a+3 end",{11},{},
    -- "do local a=7; wait(2); return a+3 end",{10},{},
    -- "do local a=8; wait(2); return a+3 end",{11},{},
    "function gg(x) if x==0 then return 1 else  return x*gg(x-1) end end return 55",{55},{},
    "return gg(5)",{120},{},
    -- "return {O1,O1}:value",{{true,true}},nil,
    -- "O1:value = false; return 42",{42},nil,
    -- "{O1,O1}:value = false; return 42",{42},nil,
    -- "O1:value=42; return O1:value",{42},{},
    "$Bar={d=8}; return $Bar.d",{8},{},
    "$$Bar=42; return $$Bar",{42},{},
    "$$$Bar=42; return $$$Bar",{42},{},
    "return Foo()",{5},{},
    "return #foo",{{type='foo'}},{},
    "return 1 & true & 3",{3},{},
    "return 1 & 2 & false",{false},{},
    "return A1(5,6)",{11},{},
    -- "return wday('wed-thu')",{true},nil,
    -- "return wday('fri')",{false},nil,
    -- "return day('28')",{true},nil,
    -- "return day('lastw-last')",{true},nil, -- lastw is last day-6 in month, last is last day
    -- "return month('jul-sep')",{true},nil,
    -- "return date('* 10-12 * 8 *')",{true},nil, --min,hour,days,month,wday
    "local a=9; case || false >> a=18 || true >> a=19 end; return a",{19},nil,
    -- "log(a); return true",{true},nil,
    "return Obj3:foo(8)",{8},nil,
    -- "return nil ?? 66",{66},nil,
    -- "return false ?? 66",{false},nil,
    -- "return {2,3,4}:sum",{9},nil,
    -- "return {2,3,4}:average",{3},nil,
    -- "return {1,false,true,'g',6,0}:bin",{{1,0,1,1,1,0}},nil,
    -- "return {true,false}:mostlyTrue",{false},nil,
    -- "return {true,false,true}:mostlyTrue",{true},nil,
    -- "return {true,false,false}:mostlyTrue",{false},nil,
    -- "return {true,false}:mostlyFalse",{false},nil,
    -- "return {true,false,false}:mostlyFalse",{true},nil,
    -- "return {true,false,true}:mostlyFalse",{false},nil,
    -- "return {a = 8, b = 9}:leaf",{{9,8}},nil,  -- sometimes wrong due to flipped order
    -- "do local a = 0; for k,v in pairs(T2) do a += v end; return a end",{14},nil,
    -- "local a = 0; for k,v in ipairs(T2) do a += k end; return a",{10},nil,
    -- "hh = {1,2,3,1}; return hh:bin:sum",{4},{tree=false},
    "local bb = 2; local j = {3,4+bb,5}; return j",{ {3,6,5} },nil,
    "local bb = 2; local j = {3,[bb]=4,5}; return j",{ {3,5} },{sexpr=false},
    "return {[YY]=8,9}",{ {9,8} },nil,
  }

  runTests(tests)
end


function QuickApp:onInit()
  quickApp = self
  self:main()
end