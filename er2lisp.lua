fibaro.EventRunner = fibaro.EventRunner or { debugFlags = {} }
local ER = fibaro.EventRunner 
local debugFlags = ER.debugFlags
local fmt = string.format

local srcMap = {}
local function MAP(expr,node) 
  if type(node)=='table' and node._dbg then
    srcMap[expr] = { from = node._dbg.from, to = node._dbg.to }
    --print(json.encode(expr),node._dbg.from,node._dbg.to)
  end
  return expr 
end --source mapping

local function expandprogn(expr)
  if type(expr)=='table' and expr[1]=='progn' then return table.unpack(expr,2) end
  return expr
end
local trans = {}
local transform
local function transList(lst)
  local res = {} for _,node in ipairs(lst) do res[#res+1] = transform(node) end
  return res
end
function trans.block(node)
  if node.locals and next(node.locals) then
    local vars = {}; for v,_ in pairs(node.locals) do vars[#vars+1] = {v} end
    return MAP({'let', vars, table.unpack(transList(node.statements))},node)
  end
  return MAP({'progn',table.unpack(transList(node.statements))},node)
end
trans['break'] = function(node) return MAP({'break'},node) end
function trans.call(node) return MAP({transform(node.fun),table.unpack(transList(node.args))},node) end
function trans.name(node) 
  if node.vt=='ev' then return MAP({__var = node.value},node) -- special hack for source maping
  elseif node.vt=='gv' then return MAP({'globalVar',{'quote',node.value:sub(2)}},node)
  elseif node.vt=='qv' then return MAP({'quickVar',{'quote',node.value:sub(3)}},node)
  elseif node.vt=='sv' then return MAP({'storeVar',{'quote',node.value:sub(4)}},node)
  else error(fmt("Unknown var type %s",node.vt)) end
end
function trans.num(node) return node.value end
function trans.str(node) return MAP({'quote',node.value},node) end
local opMap = {
  add='+',sub='-',mul='*',div='/',mod='%',pow='^',neg='-', ['not'] = 'not', ['or'] = 'or', ['and'] = 'and',
  eq='=',neq='!=',lt='<',gt='>',lte='<=',gte='>=',betw='between'
}
local function getOp(op) return opMap[op] or error(fmt("Unknown op %s",op)) end
function trans.unop(node) 
  if not (node.a or node.b) then return MAP({getOp(node.op),transform(node.exp)},node) end
  return MAP({getOp(node.op), node.a or transform(node.exp),node.b or transform(node.exp)},node) 
end
function trans.binop(node) return MAP({getOp(node.op), transform(node.exp1),transform(node.exp2)},node) end
function trans.seqop(node) return MAP({getOp(node.op), table.unpack(transList(node.exprs))},node) end
function trans.getprop(node) return MAP({'getprop',transform(node.obj),node.prop},node) end
function trans.objcall(node) return MAP({'objcall',transform(node.obj),{'quote',node.fun},table.unpack(transList(node.args))},node) end
function trans.const(node) return MAP({'quote',node.value},node) end
trans['return'] = function(node) return MAP({'return', table.unpack(transList(node.exp or {}))},node) end
function trans.table(node) 
  if node.const then return MAP({'quote', node.value},node) end
  local res = { 'table' }
  for _,item in ipairs(node.value) do
    local key = item.key and {'quote',item.key} or transform(item.expr)
    table.insert(res,key)
    table.insert(res,transform(item.value))
  end
  return MAP(res,node)
end
function trans.aref(node) 
  local key = type(node.idx)=='table' and transform(node.idx) or type(node.idx)=='number' and node.idx or {'quote',node.idx}
  return MAP({'aref', transform(node.tab), key},node)
end
local function assign(vars,exprs)
  if #vars == 1 then return {'setf', vars[1], exprs[1]} end
  return {'psetf', vars, table.unpack(exprs)}
 end

function trans.assign(node)
  local vars,exprs = transList(node.vars),transList(node.exprs)
  return MAP(assign(vars,exprs),node)
end
trans['local'] = function(node) return assign(node.names,transList(node.exprs)) end

function trans.ruledef(node) return MAP({'defrule',transform(node.head),expandprogn(transform(node.body))},node) end
function trans.loop(node) return MAP({'loop', table.unpack(transList(node.statements))},node) end
function trans.breakif(node) return MAP({'breakif', transform(node.cond)},node) end
function trans.incvar(node) return MAP({'inc', getOp(node.op), node.name, transform(node.value)},node) end
function trans.forin(node) local res = {'forin', node.names, transform(node.exp[1]), transform(node.body)} end
trans['for'] = function(node)
  return MAP({'loop', 'for', transform(node.var),'from', transform(node.start), 'to', transform(node.stop), 'by', transform(node.step), 'do',expandprogn(transform(node.body))},node)
end
trans['while'] = function(node)
  return MAP({'loop', {'breakif',{'not',transform(node.cond)}}, expandprogn(transform(node.body))},node)
end
trans['repeat'] = function(node)
  local e = {'loop', expandprogn(transform(node.body))}
  e[#e+1] = {'breakif', transform(node.cond)}
  return MAP(e,node)
end
local function compIf(first,tail,...)
  if first.cond then
    return {'if', transform(first.cond), transform(first.body), tail and compIf(tail,...)}
  else return transform(first.body) end
end
trans['if'] = function(node) return MAP(compIf(table.unpack(node.args)),node) end
local function transformRest(params)
  if params[#params] == '...' then params[#params] = '&rest' params[#params+1] = '_vararg' end
  return params
end
function trans.functiondef(node) return MAP({'defun',node.name[1],transformRest(node.fun.params),expandprogn(transform(node.fun.body))},node) end
function trans.functionexpr(node) return MAP({'lambda',transformRest(node.fun.params),expandprogn(transform(node.fun.body))},node) end
function trans.varargstable(node) return '_vararg' end

function transform(ast)
  local t = trans[ast.type]
  if t then return t(ast) else error(fmt("No transformer for %s",ast.type)) end
end

local function er2lisp(str,opts)
  local ast = ER.er2ast(str,opts)
  srcMap = {}
  local lsp =  transform(ast)
  return lsp,ER.createSourceMap(str,srcMap)
end

local function ereval(lisp,str,opts)
  opts = opts or {}
  local l = er2lisp(str,opts)
  if opts.sexpr then print("LISP:",json.encode(l)) end
  local cl = lisp:_compile(l)
  return lisp:_eval(cl,{},opts)
end

----------------- src map ---------------------------
local function calcLineOffs(lines, pos)
  local line,offs,llength = 1,0,0
  for i, l in ipairs(lines) do
    local len = #l + 1
    if pos <= offs + len then
      line = i; llength = #l break
    end
    offs = offs + len
  end
  return line, pos - offs, llength
end

local function formatError(type,msg,src,from,to)
  src = src or ""
  local lines,buff = {},{}
  for line in src:gmatch("([^\n]*)\n?") do lines[#lines+1]=line end
  local startline,startoffs,startLength = calcLineOffs(lines, from)
  local endline,endoffs,endLength = calcLineOffs(lines, to)
  if startline == endline then
    local line = lines[startline] or ""
    local ptr = string.rep('&nbsp;',startoffs-1)..fmt("<font color='orange'>%s</font>",string.rep('^',endoffs-startoffs+1))
    buff[#buff+1] = fmt("%s: %s at pos %d",type,msg,from)
    if startline > 1 then buff[#buff+1] = lines[1] end
    buff[#buff+1] = line
    buff[#buff+1] = ptr
    return table.concat(buff,"<br>")
  end
end

function ER.createSourceMap(src,srcMap)
  local self = { src = src, map = srcMap }
  function self:formatError(type,msg,ptr)
    local s = self.map[ptr]
    if s then 
      return formatError(type,msg,self.src,s.from,s.to)
    else return fmt("%s: %s",type,msg) end
  end
  return self
end

ER.er2lisp = er2lisp
ER.ereval = ereval
ER.formatError = formatError