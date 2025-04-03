#
#    nim2glsl - nim-syntax to glsl-shader transpiler
#        (c) Copyright 2025 Joel Lienhard
#
#    See the file "LICENSE.txt", included in this
#    distribution, for details about the copyright.
#


import "$nim"/compiler/[ast, parser, idents, options, lineinfos, renderer]
import std/[cmdline, sequtils, strutils, strformat, tables, options, sets, paths, parseopt]
import fusion/matching


var
  inPath: string
  outCode = ""
  outCodeLines = 1'u16


func `[]`(node: PNode, i: HSlice[int, BackwardsIndex]): seq[PNode] {.inline.} =
  node.sons[i]

template error(msg: string, info: TLineInfo) =
  stderr.writeLine("$1($2, $3) [nim2glsl] Error: $4" % [inPath, $info.line, $info.col, msg])
  quit 1

template error(msg: string, node: PNode) = error(msg, node.info)

template assertEmpty(node: PNode, errMsg: string) =
  if node.kind != nkEmpty:
    error errMsg, node

proc expectKind(node: PNode, kind: TNodeKind) =
  if node.kind != kind:
    error &"expected node of kind `{kind}`, got `{node.kind}`", node


proc addLine(s: string, fromNode: PNode) =
  let line = fromNode.info.line
  if line > outCodeLines:
    outCode &= '\n'.repeat(line-outCodeLines) & ' '.repeat(fromNode.info.col)
    outCodeLines = line
  outCode &= s


type
  BasicType = enum tFloat="float", tInt="int", tBool="bool"

  TypeKind = enum tBasic, tVec, tMat, tArray

  Type = ref object
    case kind: TypeKind
    of tBasic: typ: BasicType

    of tVec:
      vecType: BasicType
      vecSize: range[2..4]

    of tMat:
      matType: BasicType
      matSize: tuple[x,y: range[2..4]]

    of tArray:
      arrType: Type
      arrSize: string  # string because could be a const instead of lit

func newVec(typ: BasicType, size: range[2..4]): Type =
  Type(kind: tVec, vecType: typ, vecSize: size)

const typePrefix = [tFloat: "", tInt: "i", tBool: "b"]

converter toType(typ: BasicType): Type =
  Type(kind: tBasic, typ: typ)

func `$`(typ: Type): string =
  case typ.kind
  of tBasic: $typ.typ

  of tVec:
    &"{typePrefix[typ.vecType]}vec{typ.vecSize}"

  of tMat:
    typePrefix[typ.matType] & "mat" & (block:
      let s = typ.matSize
      if s.x == s.y: $typ.matSize.x
      else: &"{s.x}x{s.y}"
    )
  of tArray:
    &"{typ.arrType}[{typ.arrSize}]"

proc `==`(a,b: Type): bool =
  if a.kind != b.kind: false
  else:
    case a.kind
    of tBasic: a.typ == b.typ
    of tVec: a.vecSize == b.vecSize and a.vecType == b.vecType
    of tMat: a.matSize == b.matSize and a.matType == b.matType
    of tArray: a.arrSize == b.arrSize and a.arrType == b.arrType

var typeLookup: Table[string, Type]
for typ in BasicType:
  typeLookup[$typ] = typ
  for size in 2..4:
    for sizeY in 2..4:
      let typ = Type(
        kind: tMat,
        matType: typ,
        matSize: (size, sizeY))
      typeLookup[$typ] = typ
    let typ = newVec(typ, size)
    typeLookup[$typ] = typ

proc parseType(node: PNode): Type =
  case node.kind
  of nkIdent:
    let name = node.ident.s
    if name in typeLookup:
      return typeLookup[name]
    else:
      error &"unknown type `{name}`", node

  of nkBracketExpr:
    if (
      len(node) == 3 and
      node[0].kind == nkIdent and
      node[0].ident.s == "array"
    ):
      let typ = parseType(node[2])
      case node[1].kind
      of nkIdent:  return Type(kind: tArray, arrType: typ, arrSize: node[1].ident.s)
      of nkIntLit: return Type(kind: tArray, arrType: typ, arrSize: $node[1].intVal)
      else: discard

  else: discard
  error "invalid type-annotation", node

proc swizzleString(node: PNode): Option[string] =
  if (
    node.kind == nkIdent and
    (let s = node.ident.s;
     len(s) in 1..4 and
     (s.allIt(it in "xyzw") or s.allIt(it in "rgba")))
  ):
    some(s)
  else:
    none(string)

var monoFunctions: Table[string, Type] = typeLookup  # name -> return type

type Context = Table[string, Type]

proc inferType(node: PNode, ctx: Context): Option[Type]
proc getReturnType(name, firstArg: PNode, ctx: Context): Option[Type] =
  name.expectKind nkIdent
  let name = name.ident.s
  result = none(Type)

  if name in monoFunctions:
    return some(monoFunctions[name])

  if Some(@argType) ?= inferType(firstArg, ctx):
    case name
    of "sin", "cos", "tan", "asin", "acos", "atan":
      return some(tFloat.toType)

    of "abs", "min", "max", "mix", "clamp",
       "normalize", "cross":
          return some(argType)

    of "dot":
      if argType.kind == tVec:
        return some(argType.vecType.toType)

    else: discard

proc inferType(node: PNode, ctx: Context): Option[Type] =
  result = none(Type)

  case node.kind
  of nkIdent:
    let name = node.ident.s
    if name in ctx:
      result = some(ctx[name])

  of nkIntLit:   result = some(tInt.toType)
  of nkFloatLit: result = some(tFloat.toType)

  of nkCall, nkCommand:
    if node[0].kind == nkDotExpr:
      result = getReturnType(node[0][1], node[0][0], ctx)
    else:
      result = getReturnType(node[0], node[1], ctx)

  of nkDotExpr:
    if (
      (Some(@s) ?= swizzleString(node[1])) and
      (Some(@typ) ?= inferType(node[0], ctx)) and typ.kind == tVec
    ):
      result = some:
        if len(s) > 1: newVec(typ.vecType, len(s))
        else: typ.vecType
    else:
      result = getReturnType(node[1], node[0], ctx)

  of nkBracketExpr:
    if (Some(@typ) ?= inferType(node[0], ctx)) and typ.kind == tArray:
      result = some(typ.arrType)

  of nkPrefix:
    let op = node[0].ident.s
    let node = node[1]
    result =
      case op
      of "not", "!": some(tBool.toType)
      of "+", "-": inferType(node, ctx)
      else: none(Type)

  of nkInfix:
    let op = node[0].ident.s
    let lhs = inferType(node[1], ctx)
    let rhs = inferType(node[2], ctx)

    case op
    of "and", "&&", "or", "||":
      result = some(tBool.toType)
    of "+", "-":
      if (Some(@lhs) ?= lhs) and (Some(@rhs) ?= rhs) and lhs == rhs:
        result = some(lhs)
    of "*", "/":
      if (Some(@lhs) ?= lhs) and (Some(@rhs) ?= rhs):
        result = 
          if lhs == rhs: some(lhs)
          elif lhs.kind == tBasic: some(rhs)
          elif rhs.kind == tBasic: some(lhs)
          elif lhs.kind == tMat and rhs.kind == tVec:
            some(newVec(lhs.matType, lhs.matSize.y))
          else: none(Type)
    else: discard

  of nkPar: result = inferType(node[0], ctx)
  else: discard

template ifStartsWith(s, start: string, body: untyped): bool =
  if s.startsWith(start):
    let rest {.inject.} = s[len(start)..^1]
    body

proc genExpr(node: PNode)
proc genCall(procNameNode: PNode, args: seq[PNode]) =
  procNameNode.expectKind nkIdent
  let procName = procNameNode.ident.s
  addLine procName&'(', procNameNode
  for i, arg in args:
    genExpr(arg)
    if i != high(args):
      outCode &= ", "
  outCode &= ')'

proc genExpr(node: PNode) =
  case node.kind
  of nkIdent:
    addLine node.ident.s, node

  of nkIntLit:   addLine $node.intVal,   node
  of nkFloatLit: addLine $node.floatVal, node

  of nkCall, nkCommand:
    if node[0].kind == nkDotExpr:
      genCall(node[0][1], node[0][0] & node[1..^1])
    else:
      genCall(node[0], node[1..^1])

  of nkDotExpr:
    if Some(@s) ?= swizzleString(node[1]):
      genExpr(node[0])
      addLine '.'&s, node[1]
    else:
      genCall(node[1], @[node[0]])

  of nkBracketExpr:
    genExpr(node[0])
    outCode &= '['
    genExpr(node[1])
    outCode &= ']'

  of nkPrefix:
    node[0].expectKind nkIdent
    addLine node[0].ident.s, node[0]
    genExpr(node[1])

  of nkInfix:
    node[0].expectKind nkIdent
    genExpr(node[1])
    outCode &= &" {node[0].ident.s} "
    genExpr(node[2])

  of nkPar:
    addLine "(", node
    genExpr(node[0])
    outCode &= ')'

  else:
    error &"unsupported language feature ({node.kind})", node

proc getDefsType(defs: Pnode, ctx: var Context): Type =
  if defs[^2].kind != nkEmpty:
    parseType(defs[^2])
  elif Some(@typ) ?= inferType(defs[^1], ctx):
    typ
  else:
    error "can't infer type, please annotate explicitly", defs

proc genDefs(
  defs: PNode|seq[PNode],
  ctx: var Context,
  seperator: string,
  isConst = false,
) =
  for defs in defs:
    var typStr: string
    if isConst: typStr = "const "
    let typAno = defs[^2]
    let typ =
      if typAno.kind != nkEmpty:
        case typAno.kind
        of nkOutTy:
          typStr &= "out "
          parseType(typAno[0])
        of nkCommand, nkPrefix:
          typStr &= typAno[0].ident.s & ' '
          parseType(typAno[1])
        else:
          parseType(typAno)
      elif Some(@typ) ?= inferType(defs[^1], ctx):
        typ
      else:
        error "can't infer type. please annotate explicitly", defs
    typStr &= $typ
    for def in defs[0 ..< ^2]:
      let name = 
        case def.kind
        of nkPragmaExpr: error "pragmas not supported", def
        of nkIdent: def.ident.s
        else: error "malformed var-section", def  # should not happen
      ctx[name] = typ
      addLine &"{typStr} {name}", defs
      if defs[^1].kind != nkEmpty:
        outCode &= " = "
        genExpr(defs[^1])
      outCode &= seperator

proc genStmt(node: PNode, ctx: Context) =
  var ctx = ctx

  case node.kind
  of nkStmtList:
    for node in node: genStmt(node, ctx)

  of nkLetSection:
    error "There are no let-bindings in glsl, use `var` instead", node
  of nkVarSection, nkConstSection:
    genDefs(node, ctx,
      seperator = ";",
      isConst = node.kind == nkConstSection
    )

  of nkAsgn:
    genExpr(node[0])
    outCode &= " = "
    genExpr(node[1])
    outCode &= ';'

  of nkReturnStmt:
    addLine "return ", node
    genExpr(node[0])
    outCode &= ';'

  of nkIfStmt:
    proc genIfStmt(node: PNode, prefix = "") =
      addLine prefix&"if(", node
      genExpr(node[0])
      outCode &= ") {"
      genStmt(node[1], ctx)
      outCode &= "}"
    assert node[0].kind == nkElifBranch
    genIfStmt(node[0])
    for node in node[1..^1]:
      if node.kind == nkElifBranch:
        genIfStmt(node, "else ")
      else:
        assert node.kind == nkElse
        addLine "else {", node
        genStmt(node[0], ctx)
        outCode &= '}'

  of nkWhileStmt:
    addLine "while(", node
    genExpr(node[0])
    outCode &= ") {"
    genStmt(node[1], ctx)
    outCode &= "}"

  of nkForStmt:
    if len(node) != 3 or node[0].kind != nkIdent:
      error "multiple for vars not supported", node
    let varName = node[0].ident.s
    var comp = ""
    if node[1].kind == nkInfix:
      let op = node[1][0].ident.s
      if op == "..<": comp = "<"
      elif op == "..": comp = "<="
    if comp == "":
      error "only loops over ranges with `..` or `..<` operator supported", node[1]
    var ctx = ctx
    ctx[varName] = tInt
    addLine &"for(int {varName} = ", node
    genExpr(node[1][1])
    outCode &= &"; {varName} {comp} "
    genExpr(node[1][2])
    outCode &= &"; {varName}++) {{"
    genStmt(node[2], ctx)
    outCode &= '}'

  of nkBreakStmt: addLine "break;", node
  of nkContinueStmt: addLine "continue;", node

  else:
    genExpr(node)
    outCode &= ';'


proc genToplevelDefs(node: PNode) =
  assert node.kind == nkStmtList

  var ctx: Context

  for node in node:
    case node.kind
    of nkLetSection:
      error "There are no let-bindings in glsl, use `var` instead", node

    of nkVarSection, nkConstSection:
      genDefs(node, ctx,
        seperator = ";",
        isConst = node.kind == nkConstSection
      )

    of nkProcDef:
      let procName = node[0].ident.s
      let returnType =
        if node[3][0].kind == nkEmpty: "void"
        else:
          let typ = parseType(node[3][0])
          monoFunctions[procName] = typ
          $typ
      addLine &"{returnType} {procName}(", node
      var ctx = ctx  # copy to add params to just local context
      if len(node[3]) > 1:
        genDefs(node[3][1..^1], ctx, seperator = ", ")
        outCode.setLen len(outCode)-2  # remove last comma
      outCode &= ") {"
      genStmt(node[6], ctx)
      outCode &= '}'

    of nkCommand:
      addLine renderTree(node)&';', node

    of nkPragma:
      if (
        len(node) != 1 or
        node[0].kind != nkExprColonExpr or
        len(node[0]) != 2 or
        node[0][0].kind != nkIdent
      ):
        error "directive needs to be of the form `{.ident: expr.}`", node

      addLine &"#{node[0][0].ident.s} {renderTree(node[0][1])}", node

    else:
      error "invalid top-level def", node


var 
  outPath = ""
  toStdout = false

proc optError(s: string) =
  echo s
  quit 1

var p = initOptParser(commandLineParams())
while true:
  p.next()
  if p.kind == cmdEnd: break

  if (
    p.kind == cmdShortOption and p.key == "o" or
    p.kind == cmdLongOption and p.key == "outPath"
  ):
    outPath = p.val

  elif p.kind == cmdLongOption and p.key == "stdout":
    toStdout = true

  elif p.kind in {cmdShortOption, cmdLongOption}:
    optError &"unknown option `{p.key}`"

  elif p.kind == cmdArgument:
    if inPath != "":
      optError "can only transpile one file"
    else:
      inPath = p.key

if inPath == "":
  optError "no file given"

if outPath == "":
  outPath = $Path(inPath).changeFileExt("glsl")

elif toStdout:
  optError "can't write to stdout and a file"

let inFilename = Path(inPath).lastPathPart

genToplevelDefs(parseString(readFile(inPath), newIdentCache(), newConfigRef(), inPath))
if toStdout:
  echo outCode
else:
  writeFile(outPath, outCode)