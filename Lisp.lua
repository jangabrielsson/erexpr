--%%offline:true

local parse
local fmt = string.format
local function skipSpace(str) return 'space',nil,str:match("^[ \t\n]*(.*)") end
local function getNum(str,sign) local n,r = str:match("^(%d+%.?%d*)(.*)") return 'number',tonumber(n)*(sign or 1),r end
local function getAtom(str) return 'atom',str:match("^(.[%w!$%%&*/:<=>%?^_~%+%-]*)(.*)") end
local tokenTab = {}
for c in (" \t\n"):gmatch(".") do tokenTab[c] = skipSpace end
for c in ("0123456789"):gmatch(".") do tokenTab[c] = getNum end
for c in ("!$%&*/:<=>?^_~abcdefghijklmnopqrstuvxyzwABCDEFGHIJKLMNOPQRSTUVXYZW"):gmatch(".") do tokenTab[c] = getAtom end
tokenTab['('] = function(str) return 'lpar','(',str:sub(2) end
tokenTab[')'] = function(str) return 'rpar',')',str:sub(2) end
tokenTab['.'] = function(str) return 'dot','.',str:sub(2) end
tokenTab[','] = function(str) return 'comma','.',str:sub(2) end
tokenTab['"'] = function(str) return 'string',str:match('^"([^"]*)"(.*)') end
tokenTab["'"] = function(str) return 'string',str:match("^'([^']*)'(.*)") end
for c in ("+"):gmatch(".") do tokenTab[c] = function(str) return 'atom',str:sub(1,1),str:sub(2) end end
tokenTab['-'] = function(str) if str:sub(2,2):match("%d") then return getNum(str:sub(2),-1) else return 'atom','-',str:sub(2) end end
tokenTab[';'] = function(str) return 'space',nil,str:match("^[^%\n]*(.*)") end

local function nextToken(str)
  if str == "" then return 'eof',nil,str end
  local tf = tokenTab[str:sub(1,1)]
  if tf then
    local typ,tkn,str = tf(str)
    if typ=='space' then return nextToken(str) end
    return typ,tkn,str
  end
  error("Unknown token: "..str:sub(1,1))
end

local function createTokenizer(str)
  local function nxt()
    local typ, tkn
    typ, tkn,str = nextToken(str)
    local r = {type=typ, value=tkn}
    setmetatable(r, {__tostring = function() return (r.value or r.type) end})
    return r
  end
  local self,lt = {},nxt()
  function self.peek() return lt end
  function self.isType(t) return lt.type == t end
  function self.next() local v = lt; lt = nxt() return v end
  function self.mustBeType(t) 
    assert(self.isType(t), "Expected '"..t.."', got '"..(lt.value or lt.type).."'")
    self.next()
  end
  return self
end

function parse(tkns) 
  local t = tkns.next()
  if t.type=='atom' then
    if t.value == 't' then return true
    elseif t.value == 'false' then return false
    elseif t.value == 'nil' then return {'quote',nil}
    else  return t.value end
  elseif t.type=='number' then
    return t.value
  elseif t.type=='string' then
    return t.value
  elseif t.type=='lpar' then
    local lst = {}
    while not (tkns.isType('rpar') or tkns.isType('dot')) do
      local sexpr = parse(tkns)
      table.insert(lst,sexpr)
    end
    if tkns.isType('dot') then
      tkns.next()
      lst.cdr = parse(tkns)
    end
    tkns.mustBeType('rpar')
    return lst
  else 
    error("Unexpected token: "..tkns.peek().type)
  end
end

local function parser(str) return parse(createTokenizer(str)) end

local function execute(expr,cont,env,...)
  local i = 0
  repeat
    -- i=i+1
    -- print("STEP",i,expr)
    expr,cont = expr(cont,env,...)
  until expr == nil
end

local function MAP(expr,orExpr) return expr end

local function docify(c)
  if type(c) ~= 'table' then return tostring(c) end
  if c.__doc then 
    return docify(c.__doc)
  else
    local r = {}
    for k,v in pairs(c) do r[k] = docify(v) end
    return r
  end
end

local compile
local FUNCTION = 'func'..'tion'
local gopts = {}

local function CONT(cont,doc,src) 
  return setmetatable({ __continuation = true, __doc = doc, __expr = gopts.sourceMap and src or nil },{
    __call = function(t,...) return cont(...) end,
    __tostring = function(t) return (doc and json.encodeFast(docify(doc)) or "") end
  })
end

local function VAR(name)
  local src = nil
  if type(name)=='table' and name.__var then src = name; name = name.__var end
  return CONT(function(cont,env) return cont(env:getVar(name)) end,{'var',name},src)
end

local function CFUN(f,params,ne)
  return setmetatable({ __cfun = true, __params = params, __ne = ne },{
    __call = function(_,cont,env,...) 
      local args = {...}
      return f(cont,env,...)
    end,
    __tostring = function(_) return "cfun" end
  })
end

local function isCont(...) end
local function isCFun(f) return type(f)=='table' and f.__cfun end
local function getSymbol(e)
  local src = type(e)=='table' and e.__doc or nil
  if not src then return e elseif src[1]=='var' then return src[2] else return e end
end

local function logCall(fun,params,res)
  local p = {table.unpack(params,2)}
  print(fmt("CALL (%s %s) => %s", getSymbol(fun), json.encodeFast(p):sub(2,-2), res))
end

local function QUOTE(e) return CONT(function (cont,env) return cont(e,env) end,{'cont',e},e) end

local function evalExprs(exprs,cont) -- returns last value
  local i = 1
  local function c(...)
    i = i+1
    if i > #exprs then 
      return cont(...) 
    else return exprs[i],c end
  end
  if #exprs == 0 then return cont() 
  else return exprs[i],c end
end

local function evalArgs(args,cont) -- returns all values
  local params,i = {},1
  local function c(res,...)
    params[i] = res
    i = i+1
    if i > #args then 
      for _,v in ipairs({...}) do params[i] = v; i = i+1 end
      return cont(table.unpack(params)) 
    else return args[i],c end
  end
  if #args == 0 then return cont() 
  else return args[i],c end
end

local function reduceArgs(fun,start,exprs,cont) 
  local i,acc = 1,start
  local function c(v)
    i = i+1
    acc = fun(acc,v)
    if i > #exprs then 
      return cont(v) 
    else return exprs[i],c end
  end
  if #exprs == 0 then return cont() 
  else return exprs[i],c end
end

local function FUNC(params,body,ne,src)
  local rest = nil
  if params[#params-1] == '&rest' then
    rest = params[#params]
    params = {table.unpack(params,1,#params-2)}
  end
  return CONT(function(cont,env)
    local function fun(cont,env,...)
      local args = {...}
      local tail = env:getVar('__frame')
      if tail and fun == tail[1] and tail[2] == cont then -- Tail call
        env:unwindStack(tail[3])                          -- Unwind stack
        for i,_ in ipairs(params) do env:setLocal(params[i], args[i]) end -- Set vars
        if rest then local r = {table.unpack(args,#params+1)} env:setLocal(rest, r) end -- bind rest
        return body(cont,env)
      else
        local vars = {}
        for i,v in ipairs(params) do vars[v] = { args[i] } end -- Create new locals
        if rest then local r = {table.unpack(args,#params+1)} env:setLocal(rest, r) end -- bind rest
        if env:pushFrame(vars) then return end        -- max stack depth exceeded
        local ncont = function(...) env:popFrame() return cont(...) end
        env:setLocal('__frame',{fun,ncont,env:getStackPoint()}) -- Save fun,cont,sp
        return body(ncont,env)
      end
    end
    return QUOTE(CFUN(fun,params,ne)),cont 
  end,{'fun',body},src)
end

local function FUNCALL(fun,args,src)
  return CONT(function(cont,env)
    return fun,function(f)
      if type(f) == FUNCTION then
        return evalArgs(args, function(...)
          local stat = {pcall(f,...)}
          --logCall(fun,args,stat[1] and stat[2] or ("Error: "..tostring(stat[2])))
          if stat[1] then return cont(table.unpack(stat,2)) 
          else return env.error("Error in FUNCTION call: "..tostring(stat[2]).." "..getSymbol(tostring(fun))) end
        end)
      elseif isCFun(f) then
        if f.__ne then
          return f(cont,env,table.unpack(args))
        else
          return evalArgs(args, function(...) 
            return f(cont,env,...) 
          end)
        end
      else
        return env.error("Attempt to call a NON-FUNCTION: "..tostring(f).." "..getSymbol(tostring(fun)))
      end
    end
  end,{'call',fun,table.unpack(args)},src)
end

local function NUM(num) return CONT(function(cont,env) return cont(num) end,{'num',num}) end
local function BOOL(bool) return CONT(function(cont,env) return cont(bool) end,{'bool',bool}) end
local function NIL() return CONT(function(cont,env) return cont(nil) end,{'nil'}) end

local function LOCALFRAME(expr,src)
  return CONT(function(cont,env) 
    local function ncont(...) env:popFrame() return cont(...) end
    env:pushFrame({})
    env:setLocal('__localframe',{cont,env:getStackPoint()})
    return expr,ncont
  end,{'frame',expr},src)
end

local function LET(vars,exprs,body,src)
  return CONT(function(cont,env)
    return evalArgs(exprs, function(...) 
      local vals,locals = {...},{}
      for i=1,#vars do locals[vars[i]] = {vals[i]} end
      env:pushFrame(locals)
      return evalExprs(body,function(...) env:popFrame() return cont(...) end)
    end)
  end,{'let',vars,exprs,body},src)
end

local spec = {}

function spec.lambda(expr, ctx)
  local params,body = expr[2],{'progn',table.unpack(expr,3)}
  return FUNC(params,compile(body, ctx),nil,expr)
end

function spec.nlambda(expr, ctx)
  local params,body = expr[2],{'progn',table.unpack(expr,3)}
  return FUNC(params,compile(body, ctx),true,expr)
end

spec['if'] = function(expr, ctx)
  local test = compile(expr[2],ctx)
  local tthen = compile(expr[3],ctx)
  local telse = expr[4] and compile(expr[4],ctx)
  return CONT(function(cont,env)
    return test,function(v)
      if v then 
        return tthen,cont
      elseif telse then
        return telse,cont
      else
        return cont(false)
      end
    end
  end,{'if',test,tthen,telse},expr)
end

spec['progn'] = function(expr, ctx)
  local body = {table.unpack(expr,2)}
  if #body == 0 then return QUOTE(nil) 
  elseif #body == 1 then return compile(body[1],ctx) 
  else
    for i=1,#body do body[i] = compile(body[i],ctx) end
    return CONT(function(cont,env) 
      return evalExprs(body,cont) 
    end,{'progn',table.unpack(body)},expr)
  end
end

spec['let'] = function(expr, ctx) -- (let ((x 10) (y 20)) body)
  local body = {table.unpack(expr,3)}
  local vexpr = expr[2]
  local vars,exprs = {},{}
  for i,v in ipairs(body) do body[i] = compile(v,ctx) end
  for _,v in ipairs(expr[2] or {}) do
    vars[#vars+1] = v[1]
    exprs[#exprs+1] = compile(v[2],ctx)
  end
  return LET(vars,exprs,body,expr)
end

spec['eval'] = function(expr, ctx)
  local val = compile(expr[2],ctx)
  return CONT(function(cont,env) 
    return val,function(r) return r,cont end
  end,{'eval',expr},expr)
end

spec['and'] = function(expr, ctx)
  local args = {} for i=2,#expr do args[#args+1] = compile(expr[i],ctx) end
  if #args == 2 then 
    return CONT(function(cont,env)
      return args[1],function(av)
        if not av then return cont(av) end
        return args[2],cont
      end
    end,{'and',expr[2],expr[3]},expr)
  else
    return CONT(function(cont,env)
      return reduceArgs(function(acc,v) return acc and v end, true, args, cont)
    end,{'and',table.unpack(expr,2)},expr)
  end
end

spec['or'] = function(expr, ctx)
  local args = {} for i=2,#expr do args[#args+1] = compile(expr[i],ctx) end
  if #args == 2 then 
    return CONT(function(cont,env)
      return args[1],function(av)
        if av then return cont(av) end
        return args[2],cont
      end
    end,{'or',expr[2],expr[3]},expr)
  else
    return CONT(function(cont,env)
      return reduceArgs(function(acc,v) return acc or v end, true, args, cont)
    end,{'and',table.unpack(expr,2)},expr)
  end
end

spec['_loop'] = function(expr, ctx) -- basic (internal) loop, used by loop and looo for macros
  local body = {table.unpack(expr,2)}
  for i=1,#body do body[i] = compile(body[i],ctx) end
  local loop
  loop = CONT(function(cont,env)
    return evalExprs(body,function() return loop,cont end)
  end)
  return LOCALFRAME(loop,expr)
end

spec['quote'] = function(expr, ctx)
  local expr = expr[2] 
  return CONT(function(cont,env) return cont(expr) end,{'quote',expr})
end

spec['return'] = function(expr, ctx)
  local res = {table.unpack(expr,2)}
  for i=1,#res do res[i] = compile(res[i],ctx) end
  return CONT(function(cont,env)
    local ret = env:getVar('__frame') -- {fun,cont,sp}
    if ret then
      return evalArgs(res,function(...) env:unwindStack(ret[3]) return ret[2](...) end)
    end
  end,{'return',table.unpack(expr,2)},expr)
end

spec['break'] = function(expr, ctx)
  return CONT(function(cont,env)
    local brk = env:getVar('__localframe')
    if brk then env:unwindStack(brk[2]) return brk[1](true)
    else return env.error("No loop to break from") end
  end,{'break'},expr)
end

spec['breakif'] = function(expr, ctx)
  local cond = compile(expr[2],ctx)
  return CONT(function(cont,env)
    return cond,function(c)
      if c then 
        local brk = env:getVar('__localframe')
        if brk then 
          env:unwindStack(brk[2]) return brk[1](true)
        else return env.error("No loop to break from") end
      else return cont(true) end
    end
  end,{'break'},expr)
end

local iops = {
  ['+']=function(a,b) return a+b end,
  ['-']=function(a,b) return a-b end,
  ['*']=function(a,b) return a*b end,
  ['/']=function(a,b) return a/b end
}
spec.inc = function(expr,ctx)
  local op,name,val = expr[2],expr[3],compile(expr[4] or 1,ctx)
  local opf = iops[op] or error("Unknown inc op: "..tostring(op))
  return CONT(function(cont,env)
    local v = env:getVar(name) or 0
    return val,function(f)
      f = opf(v,f)
      env:setVar(name,f)
      return cont(f)
    end
  end,{'inc',op,name,val},expr)
end
spec['inc+'] = function(expr,ctx) return spec.inc({'inc','+',expr[2],expr[3]},ctx) end
spec['inc-'] = function(expr,ctx) return spec.inc({'inc','-',expr[2],expr[3]},ctx) end
spec['inc*'] = function(expr,ctx) return spec.inc({'inc','*',expr[2],expr[3]},ctx) end
spec['inc/'] = function(expr,ctx) return spec.inc({'inc','/',expr[2],expr[3]},ctx) end

spec.wait = function(expr,ctx)
  local val = compile(expr[2] or 0,ctx)
  return CONT(function(cont,env)
    return val,function(t)
      local ref = setTimeout(function() 
        local expr,c = cont(t)
        execute(expr,c,env)
      end,
      t*1000)
      return env.suspend(ref)
    end
  end,{'wait',expr},expr)
end

local function getsetf(lv,ctx)
  if type(lv) == 'string' then 
    return function(cont, env, value) env:setVar(lv, value) return cont(value) end
  elseif type(lv) == 'table' then 
    if lv.__var then 
      return function(cont, env, value) env:setVar(lv.__var, value) return cont(value) end
    end
    local set = ctx.lisp.setfs[lv[1]]
    if set then
      if #lv == 2 then
        local obj = compile(lv[2],ctx)
        setf = function(cont, env, value) 
          return obj(function(o) 
            set(env,value,o,lv) 
            return cont(value)
          end,env)
        end
      elseif #lv == 3 then
        local obj = compile(lv[2],ctx)
        local key = compile(lv[3],ctx)
        setf = function(cont, env, value) 
          return obj(function(o) 
            return key(function(k)
              set(env,value,o,k,lv) 
              return cont(value)
            end,env)
          end,env)
        end
      else
        error("Too many args to set function: "..tostring(lv[1]))
      end
    else
      error("No such set func".."tion: "..tostring(lv[1]))
    end
  end
  return setf
end

spec.psetf = function(expr,ctx) -- (psetf (var1 var2 varn ...) e1 e2 ... en)
  local lvs = expr[2]
  local rvs = table.pack(table.unpack(expr,3))
  for i=1,#rvs do rvs[i] = compile(rvs[i],ctx) end
  local setfs = {} for _,lv in ipairs(lvs) do setfs[#setfs+1] = getsetf(lv,ctx) end
  return CONT(function(cont,env)
    return evalArgs(rvs, function(...)
      local vals,i,last = {...},0,nil
      local expr
      local function c(v) last = v return expr,c end
      function expr(c,env) 
        i = i+1
        if i > #setfs then return cont(last) end
        return setfs[i](c,env,vals[i],lvs[i])
       end
      return expr,c
    end)
  end,{'psetf',table.unpack(expr,2)},expr)
end

spec.setf = function(expr,ctx)
  local name = expr[2]
  local setf = getsetf(name,ctx)
  local value = compile(expr[3],ctx)
  return CONT(function(cont,env)
    return value,function(v)
      return setf(cont, env, v, name)
    end
  end,{'setf',name,value},expr)
end

spec.setglob = function(expr,ctx)
  local name = expr[2]
  local value = compile(expr[3],ctx)
  return CONT(function(cont,env)
    return value,function(f)
      env:setGlobal(name, f)
      return cont(f)
    end
  end,{'setglob',name,value},expr)
end

local gcount = 100
local function gensym(str) gcount = gcount+1; return fmt("__%s_%d__",str,gcount) end

local macro = {}

function macro.dotimes(expr,ctx) -- (dotimes (var count) . body)
  local var = expr[2][1]
  local count = expr[2][2]
  local c = gensym('count')
  return MAP({{'lambda', {var,c}, {'_loop', {'inc+',var},{'breakif', {'>=', var, c}},table.unpack(expr,3)}},-1,count},expr)
end

function macro.dolist(expr,ctx) -- (dolist (elm list) . body)
  local var = expr[2][1]
  local listn = expr[2][2]
  local list,idx = gensym('list'),gensym('idx')
  return MAP({{'lambda', {idx,list,var}, {'_loop', {'setf', var, {'aref',list,idx}}, {'breakif',{'=',var,nil}}, {'inc+',idx}, table.unpack(expr,3)}},1,listn},expr)
end

macro['loop'] = function(expr,ctx) -- (loop for x from 12 to 11 do ..., loop forin k v in ipairs(x) do ...
  local ftyp = expr[2]
  if not (ftyp ==  'for' or ftyp == 'forin') then return {'_loop', table.unpack(expr,2)} end -- simple loop

  if ftyp == 'forin' then
    local k,v,tab,offs = expr[3],expr[4],expr[6],0
    if v=='in' then v = '_'; tab = expr[5]; offs = -1 end
    local f,t = gensym('forinf'),gensym('forint')
    local body = {table.unpack(expr,7+offs)}
    return MAP({'let', {k,v,f,t}, 
        {'psetf',{f,t,k,v},tab},
        {'_loop', {'psetf',{k,v},{f,t,k}},{'breakif',{'not',k}},table.unpack(body)}}
        ,expr)
  end

  local var,start,stopv,by,offs = expr[3],expr[5],expr[7],1,0
  if expr[8] == 'by' then by = expr[9]; offs = 2 end
  local stop,step = gensym('for'),gensym('for')
  local body = {table.unpack(expr,9+offs)}
  body[#body+1] = {'setf', var, {'+', var, step}}
  return MAP({{'lambda', {var,stop,step}, {'_loop', MAP({'breakif', {'>',var,stop}},expr), table.unpack(body)}},start,stopv,by},expr)
end

macro.defun = function(expr,ctx)
  local name = expr[2]
  local fun = MAP({'lambda',table.unpack(expr,3)},expr)
  return MAP({'setglob',name,fun},expr)
end

macro.defspec = function(expr,ctx)
  local name = expr[2]
  local fun = MAP({'nlambda',table.unpack(expr,3)},expr)
  return MAP({'setf',name,fun},expr)
end

function compile(expr,ctx)
  local typ = type(expr)
  if typ == 'table' then -- Call
    if expr.__var then 
      return VAR(expr)
    end
    if #expr == 0 then return QUOTE({}) end
    local op = expr[1]
    if spec[op] then
      return spec[op](expr, ctx) 
    elseif ctx.lisp.macro[op] then
      local expansion = ctx.lisp.macro[op](expr, ctx)
      return compile(expansion,ctx)
    end
    local fop,args = compile(op,ctx),{}
    for i=2,#expr do
      args[#args+1] = compile(expr[i],ctx)
    end
    return FUNCALL(fop,args,expr)
  elseif typ == 'number' then 
    return NUM(expr)
  elseif typ == 'boolean' then 
    return BOOL(expr)
  elseif typ == 'string' then
    return VAR(expr)
  elseif typ == 'nil' then
    return NIL()
  else
    error("Unknown string expr: "..tostring(expr))
  end
end

local function createEnvironment(vars,nonVarHandler)
  local self = { error = function(msg) fibaro.error(__TAG,msg) end, nonVarHandler = nonVarHandler }
  vars = vars or {}
  local topvars = vars
  function self:getVar(name) local v = vars[name] if v then return v[1] elseif self.nonVarHandler then return self.nonVarHandler(name) end end -- 1500 0.333
  function self:setLocal(name,val) rawset(vars,name,{val}) end
  function self:setVar(name,val) local v = vars[name] if v then v[1] = val else rawset(topvars,name,{val}) end end
  function self:setGlobal(name,val) 
    local v = rawget(topvars,name) if v then v[1] = val else rawset(topvars,name,{val}) end 
  end
  function self:dump(pp)
    for k,v in pairs(vars) do
      print(k,"=",v[1])
    end
  end
  local fd = 1
  function self:pushFrame(locals)
    if fd > 2000 then self.error("Max stack depth (2000) exceeded") return true end
    vars = setmetatable(locals, { __index = vars })
    fd = fd + 1
    locals.__fd  = fd
    --print(fd)
  end
  function self:popFrame() 
    vars = getmetatable(vars).__index
    fd = vars.__fd or 1
  end
  function self:getStackPoint() return vars end
  function self:unwindStack(sp) vars = sp; fd = vars.__fd or 1 end
  function self:copy(v,nvh)
    local e = createEnvironment(v or vars, nvh or self.nonVarHandler)
    return e
  end
  return self
end

local function Error(info,tostr)
  return setmetatable(info,{ __tostring = function (i) return tostr(i) end})
end

local err = {}
err.type = function(e)
  return fmt("Type error: argument %d (%s) expected to be of type '%s'", e.n, tostring(e.expr), e.expected)
end

local function checkArgs(e1,t1,e2,t2)
  if type(e1) ~= t1 then error(Error({type = 'typeErr', n=1, expr=e1, expected=t1}, err.type)) end
  if e2 and type(e2) ~= t2 then error(Error({type = 'typeErr', n=2, expr=e2, expected=t2}, err.type)) end
end

Lisp = {}
class 'Lisp'
function Lisp:__init(opts)
  self.opts = opts or {}
  self.macro,self.setfs = {},{}
  self.env = createEnvironment()
  self.env:setVar('true', true)
  self.env:setVar('false', false)
  self.env:setVar('not', function(a) return not a end)
  self.env:setVar('+', function(a,b) checkArgs(a,'number',b,'number') return a+b end)
  self.env:setVar('*', function(a,b) checkArgs(a,'number',b,'number') return a*b end)
  self.env:setVar('/', function(a,b) checkArgs(a,'number',b,'number') return a/b end)
  self.env:setVar('-', function(a,b) return b==nil and -a or a-b end)
  self.env:setVar('=', function(a,b) return a==b end)
  self.env:setVar('!=', function(a,b) return a~=b end)
  self.env:setVar('>', function(a,b) return a>b end)
  self.env:setVar('>=', function(a,b) return a>=b end)
  self.env:setVar('<', function(a,b) return a<b end)
  self.env:setVar('<=', function(a,b) return a<=b end)
  self.env:setVar('%', function(a,b) checkArgs(a,'number',b,'number') return a % b end)
  self.env:setVar('^', function(a,b) checkArgs(a,'number',b,'number') return a ^ b end)
  self.env:setVar('aref', function(tab,idx) return tab[idx] end)
  self.env:setVar('print', print)
  self.env:setVar('pairs', pairs)
  self.env:setVar('ipairs', ipairs)
  self.env:setVar('table', function(...)
    local t,args = {},{...}
    for i=1,#args,2 do t[args[i]] = args[i+1] end
    return t
  end)
  self.env.error = function(msg) fibaro.error(__TAG,msg) end
  for k,v in pairs(macro) do self.macro[k] = v end
  self.setfs.car = function(env, val, obj) obj[1] = val; return val end
  self.setfs.aref = function(env, val, obj, key) obj[key] = val; return val end
end

function Lisp:setVar(name,val) self.env:setVar(name,val) end
function Lisp:getVar(name) return self.env:getVar(name) end

function Lisp:_compile(expr)
  gopts = self.opts
  if type(expr) == 'string' then
    expr = parser(expr)
  end
  local ctx = {lisp = self}
  return compile(expr,ctx)
end

function Lisp:_eval(c,vars,...)
  env = self.env:copy()
  local res,printRes = {},false
  local function cont(...) 
    if printRes then print(...) end
    res = {...} 
    env:popFrame()
    return nil
  end
  local function suspend(ref) 
    printRes = true
  end
  env.suspend = suspend
  env:pushFrame(vars or {}) -- create new local environment
  env:setLocal('__frame',{nil,cont,env:getStackPoint()}) 
  execute(c,cont,env,...)
  return table.unpack(res)
end

function Lisp:eval(expr)
  return self:_eval(self:_compile(expr))
end

function Lisp:funFromSymbol(sym)
  local fun = self.env:getVar(sym)
  if not fun then error("No such function: "..tostring(sym)) end
  if not isCFun(fun) then error("Not a Lisp function: "..tostring(sym)) end
  return function(...)
    return self:_eval(fun,nil,...)
  end
end

local function encode(...)
  local args = {...}
  for i,e in ipairs(args) do args[i] = (type(e)=='table' and json.encodeFast(e)) or e end
  return table.unpack(args)
end

function Lisp.runWith(file)
  local f = io.open(file,'r')
  if not f then error("No such test file: "..tostring(file)) end
  local str = f:read('*a')
  f:close()
  local lisp = Lisp()
  lisp:eval("(progn " .. str .. ")")
  return function(...)
    for _,expr in ipairs({...}) do
      local t0 = os.clock()
      local res = {lisp:eval(expr)}
      local t1 = os.clock()
      res[#res+1] = fmt("[%.3fs]",t1-t0) 
      print("RUN:",expr,":",encode(table.unpack(res)))
    end
  end
end

-- Lisp.runWith("tests/loops.lsp")('(test5)')
-- Lisp.runWith("tests/loops.lsp")('(test7 2 8 2)')
-- Lisp.runWith("tests/loops.lsp")('(test2 2 8 2)')
-- Lisp.runWith("tests/loops.lsp")('(test3 (quote (2 8 2)))')
-- Lisp.runWith("tests/scope.lsp")('(test1)','(test2)')
-- Lisp.runWith("tests/fact.lsp")('(fact 10)','(fact2 10)')
-- Lisp.runWith("tests/fact.lsp")('(fact2 10)')
-- Lisp.runWith("tests/constructs.lsp")('(test1)','(test2)','(test3)','(test4)','(test5)')
-- Lisp.runWith("tests/constructs.lsp")('(test7)')
-- Lisp.runWith("tests/constructs.lsp")('(test8)')
-- Lisp.runWith("tests/scope.lsp")('(test3)')