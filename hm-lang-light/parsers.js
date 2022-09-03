import P from "parsimmon"

import {fst, snd} from "./tuple-select"

const flatten = xss => 
  [].concat.apply([], xss)

/*
 * Based on:
 * https://github.com/xodio/hm-parser
 */

const parsers = {
  // declaration -> name _ "::" (_ classConstraints _ "=>"):? _ type
  declaration: $ => 
    P.seq(
      P.seqMap(
        $.name.skip($._),
        $.doubleColon,

        fst
      ),

      P.seqMap(
        $.classConstraints.wrap($._, $._),
        $.fatArrow,

        fst
      ).times(0,1),

      P.seqMap(
        $._, 
        $.type,

        snd
      )
    )
      .map(xs => ({
        name: xs[0],
        constraints: flatten(xs[1]),
        type: xs[2]
      })),

  type: $ =>
    P.alt(
      $.method,
      $.uncurriedFunction,
      $.function, 
      $.thunk,
      $.list,
      $.record,
      $.constrainedType,
      $.typeConstructor,
      $.typeVariable
    ),

  // typeVariable -> lowId
  typeVariable: $ =>
    $.lowId
      .map(s => [
        {type: "typeVariable",
         text: s},

        []
      ]),

  classConstraints: $ =>
    P.alt(
      $.constraint.map(x => [x]),
      $.wrappedConstraints
    ),

  // constraint -> typeConstructor
  constraint: $ =>
    $.typeConstructor
      .map(x => [
        {type: "constraint",
         text: x[0]["text"]},

        x[1]
      ]),

  // wrappedConstraints -> "(" _ constraint (_ "," _ constraint):* _ ")"
  wrappedConstraints: $ =>
    P.seq(
      $.constraint,

      P.seqMap(
        $.comma.wrap($._, $._),
        $.constraint,

        snd
      ).many()
    )
      .wrap($._, $._)
      .wrap($.lParen, $.rParen)

      .map(xs => [xs[0]].concat(xs[1])),

  // typeConstructor -> capId (__ typeConstructorArg):*
  typeConstructor: $ =>
    P.seq(
      $.capId,

      P.seqMap(
        $.__,
        $.typeConstructorArg,

        snd
      ).many()
    )
      .map(xs => [
        {type: "typeConstructor",
         text: xs[0]},

        xs[1]
      ]),

  // wrappedTypeConstructor -> "(" _ typeConstructor _ ")"
  wrappedTypeConstructor: $ =>
    $.typeConstructor
      .wrap($._, $._)
      .wrap($.lParen, $.rParen),

  typeConstructorArg: $ =>
    P.alt(
      $.list,
      $.record,
      $.typeVariable,
      $.list,
      $.wrappedConstrainedType,
      $.nullaryTypeConstructor,
      $.wrappedTypeConstructor,
      $.wrappedFunction,
      $.wrappedThunk
    ),

  // nullaryTypeConstructor -> capId
  nullaryTypeConstructor: $ =>
    $.capId
      .map(s => [
        {type: "typeConstructor",
         text: s},

        []
      ]),

  constrainedTypeArg: $ =>
    P.alt(
      $.list,
      $.record,
      $.nullaryTypeConstructor,
      $.typeVariable,
      $.wrappedTypeConstructor,
      $.wrappedConstrainedType,
      $.wrappedFunction,
      $.wrappedThunk
    ),

  // constrainedType -> lowId (__ constrainedTypeArg):+
  constrainedType: $ =>
    P.seq(
      $.lowId,

      P.seqMap(
        $.__,
        $.constrainedTypeArg,

        snd
      ).atLeast(1)
    )
      .map(xs => [
        {type: "constrainedType",
         text: xs[0]},

        xs[1]
      ]),

  // wrappedConstrainedType -> "(" _ constrainedType _ ")"
  wrappedConstrainedType: $ =>
    $.constrainedType
      .wrap($._, $._)
      .wrap($.lParen, $.rParen),

  // list -> "[" _ type _ "]"
  list: $ =>
    $.type
      .wrap($._, $._)
      .wrap($.lSquare, $.rSquare)

      .map(x => [
        {type: "list",
         text: ""},

        [x]
      ]),

  // function -> functionArg (_ "->" _ functionArg):+
  function: $ =>
    P.seq(
      $.functionArg,

      P.seqMap(
        $.arrow.wrap($._, $._),
        $.functionArg,

        snd
      ).atLeast(1)
    )
      .map(xs => [
        {type: "function",
         text: ""},

        [xs[0]].concat(xs[1])   
      ]),

  // wrappedFunction -> "(" _ function _ ")"
  wrappedFunction: $ =>
    $.function
      .wrap($._, $._)
      .wrap($.lParen, $.rParen),

  functionArg: $ =>
    P.alt(
      $.wrappedFunction,
      $.wrappedUncurriedFunction, 
      $.wrappedThunk,
      $.typeConstructor,
      $.record, 
      $.constrainedType, 
      $.typeVariable, 
      $.list
    ),

  // uncurriedFunction -> uncurriedFunctionParams _ "->" _ type
  uncurriedFunction: $ =>
    P.seqMap(
      $.uncurriedFunctionParams,
      $.arrow.wrap($._, $._),
      $.type,

      (l, _, r) =>
        [l, r]
    )
      .map(xs => [
        {type: "uncurriedFunction",
         text: ""},

        [xs[0], xs[1]]
      ]),

  // wrappedUncurriedFunction -> "(" _ uncurriedFunction _ ")"
  wrappedUncurriedFunction: $ =>
    $.uncurriedFunction
      .wrap($._, $._)
      .wrap($.lParen, $.rParen),

  // uncurriedFunctionParams -> "(" _ type (_ "," _ type):+ _ ")"
  uncurriedFunctionParams: $ =>
    P.seq(
      $.type,

      P.seqMap(
        $.comma.wrap($._, $._),
        $.type,
        
        snd
      ).atLeast(1)
    )
      .wrap($._, $._)
      .wrap($.lParen, $.rParen)

      .map(xs => [
        {type: "uncurriedFunctionParams",
         text: ""},

        [xs[0]].concat(xs[1])
      ]),

  method: $ =>
    P.seq(
      P.seqMap(
        P.alt(
          $.typeConstructor,
          $.constrainedType,
          $.typeVariable
        ),
        $.squigglyArrow.wrap($._, $._),

        fst
      ),

      $.type
    )
      .map(xs => [
        {type: "method",
         text: ""},

        [xs[0], xs[1]]
      ]),

  // thunk -> thunkParenthesis (_ "->" _ functionArg):+
  thunk: $ =>
    P.seq(
      $.thunkParenthesis,

      P.seqMap(
        $.arrow.wrap($._, $._),
        $.functionArg,

        snd
      ).atLeast(1)
    )
      .map(xs => [
        {type: "function",
         text: ""},

        [xs[0]].concat(xs[1])
      ]),

  // wrappedThunk -> "(" _ thunk _ ")"
  wrappedThunk: $ =>
    $.thunk
      .wrap($._, $._)
      .wrap($.lParen, $.rParen),

  // thunkParenthesis -> "(" _ ")"
  thunkParenthesis: $ =>
    P.seq(
      $.lParen,
      $._,
      $.rParen
    )
      .map(_ => [
        {type: "thunk",
         text: ""},

        []
      ]),

  // record -> "{" _ recordField (_ "," _ recordField):* _ "}"
  record: $ =>
    P.seq(
      $.recordField,

      P.seqMap(
        $.comma.wrap($._, $._),
        $.recordField,

        snd
      ).many()

    )
      .wrap($._, $._)
      .wrap($.lCurly, $.rCurly)

      .map(xs => [
        {type: "record",
         text: ""},

        [xs[0]].concat(xs[1])
      ]),

  // recordField -> recordFieldName _ "::" _ type
  recordField: $ =>
    P.seqMap(
      $.recordFieldName,
      $.doubleColon.wrap($._, $._),
      $.type,

      (l, _, r) =>
        [l, r]
    )
      .map(xs => [
        {type: "recordField",
         text: xs[0]},

        [xs[1]]
      ])
}

export default parsers