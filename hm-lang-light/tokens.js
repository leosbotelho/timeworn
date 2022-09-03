import P from "parsimmon"

const tokens = {
  name: () =>
    P.regexp(/[A-Za-z0-9_'#@-]+/),

  recordFieldName: () =>
    P.regexp(/[A-Za-z0-9_]+/),

  capId: () =>
    P.regexp(/[A-Z][a-zA-Z0-9_]*/),

  lowId: () =>
    P.regexp(/[a-z][a-zA-Z0-9_]*/),

  doubleColon: () =>
    P.string("::"),

  comma: () =>
    P.string(","),

  lParen: () =>
    P.string("("),

  rParen: () =>
    P.string(")"),

  lCurly: () =>
    P.string("{"),

  rCurly: () =>
    P.string("}"),

  lSquare: () =>
    P.string("["),

  rSquare: () =>
    P.string("]"),

  arrow: () =>
    P.string("->"),

  fatArrow: () =>
    P.string("=>"),

  squigglyArrow: () =>
    P.string("~>"),

  _: () =>
    P.regexp(/\s*/m),

  __: () =>
    P.regexp(/\s+/m)
}

export default tokens