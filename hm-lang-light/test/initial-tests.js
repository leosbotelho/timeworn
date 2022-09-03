import Tape    from "tape"

import HMLang  from "../index"
import _runP   from "../run-parser"

const runP     = _runP (HMLang)

/*
 * Tests as in:
 * https://github.com/kedashoe/hindley-milner-parser-js/blob/master/test/test.js
 */

Tape.test("parse", t => {
  t.deepEqual(
    runP ("declaration") ("hello :: a -> Maybe a"), {
        name: "hello",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
  })

  t.deepEqual(
    runP ("declaration") ("x :: [a] -> Integer"), {
        name: "x",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "list", text: ""},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Integer"},[]]]]
  })

  t.deepEqual(
    runP ("declaration") ("hello :: a -> { x :: String, y :: a }"), {
        name: "hello",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "record", text: ""},[
                    [{type: "recordField", text: "x"},[
                        [{type: "typeConstructor", text: "String"},[]]]],
                    [{type: "recordField", text: "y"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
  })

  t.deepEqual(
    runP ("declaration") ("Maybe#chain :: Maybe a ~> (a -> Maybe b) -> Maybe b"), {
        name: "Maybe#chain",
        constraints: [],
        type: 
            [{type: "method", text: ""},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeConstructor", text: "Maybe"},[
                            [{type: "typeVariable", text: "b"},[]]]]]],
                    [{type: "typeConstructor", text: "Maybe"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
  })

  t.deepEqual(
    runP ("declaration") ("hello :: Foo a => a -> String"), {
        name: "hello",
        constraints: [
            [{type: "constraint", text: "Foo"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
  })

  t.deepEqual(
    runP ("declaration") ("reduce_ :: Foldable f => ((a, b) -> a) -> a -> f b -> a"), {
        name: "reduce_",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "a"},[]]]]
  })

  t.deepEqual(
    runP ("declaration") ("hello :: (Foo f, Bar a) => (a -> f b) -> [a] -> [Either (Maybe a) b]"), {
        name: "hello",
        constraints: [
            [{type: "constraint", text: "Foo"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Bar"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "list", text: ""},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "list", text: ""},[
                    [{type: "typeConstructor", text: "Either"},[
                        [{type: "typeConstructor", text: "Maybe"},[
                            [{type: "typeVariable", text: "a"},[]]]],
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
  })

  t.deepEqual(
    runP ("declaration") ("sum :: Foldable f => f FiniteNumber -> FiniteNumber"), {
        name: "sum",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "FiniteNumber"},[]]]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
  })

  t.deepEqual(
    runP ("declaration") ("promap :: Profunctor p => (a -> b, c -> d, p b c) -> p a d"), {
        name: "promap",
        constraints: [
            [{type: "constraint", text: "Profunctor"},[
                [{type: "typeVariable", text: "p"},[]]]]],
        type: 
            [{type: "uncurriedFunction", text: ""},[
                [{type: "uncurriedFunctionParams", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "c"},[]],
                        [{type: "typeVariable", text: "d"},[]]]],
                    [{type: "constrainedType", text: "p"},[
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "c"},[]]]]]],
                [{type: "constrainedType", text: "p"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "d"},[]]]]]]
  })

  t.deepEqual(
    runP ("declaration") ("maybe_ :: (() -> b) -> (a -> b) -> Maybe a -> b"), {
        name: "maybe_",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "thunk", text: ""},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
  })

  t.end()
})

Tape.test("name", t => {
  t.deepEqual(runP ("name") ("foo"),"foo")
  t.deepEqual(runP ("name") ("foo'"),"foo'")
  t.deepEqual(runP ("name") ("Maybe#@@type"),"Maybe#@@type")

  t.end()
})

Tape.test("classConstraints", t => {
  t.deepEqual(runP ("classConstraints") ("(Eq a, Foo b, Bar b)"),[
        [{type: "constraint", text: "Eq"},[
            [{type: "typeVariable", text: "a"},[]]]],
        [{type: "constraint", text: "Foo"},[
            [{type: "typeVariable", text: "b"},[]]]],
        [{type: "constraint", text: "Bar"},[
            [{type: "typeVariable", text: "b"},[]]]]])

  t.deepEqual(runP ("classConstraints") ("(Eq a, Foo b)"),[
        [{type: "constraint", text: "Eq"},[
            [{type: "typeVariable", text: "a"},[]]]],
        [{type: "constraint", text: "Foo"},[
            [{type: "typeVariable", text: "b"},[]]]]])

  t.deepEqual(runP ("classConstraints") ("Eq a"),[
        [{type: "constraint", text: "Eq"},[
            [{type: "typeVariable", text: "a"},[]]]]])

  t.end()
})

Tape.test("typeVariable", t => {
  t.deepEqual(runP ("typeVariable") ("a"),
        [{type: "typeVariable", text: "a"},[]])
  t.end()
})

Tape.test("thunk", t => {
  t.deepEqual(runP ("thunk") ("() -> Maybe a"),
        [{type: "function", text: ""},[
            [{type: "thunk", text: ""},[]],
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeVariable", text: "a"},[]]]]]])

  t.deepEqual(runP ("thunk") ("() -> a"),
        [{type: "function", text: ""},[
            [{type: "thunk", text: ""},[]],
            [{type: "typeVariable", text: "a"},[]]]])

  t.end()
})

Tape.test("constrainedType", t => {
  t.deepEqual(runP ("constrainedType") ("f a"),
        [{type: "constrainedType", text: "f"},[
            [{type: "typeVariable", text: "a"},[]]]])

  t.deepEqual(runP ("constrainedType") ("p a b"),
        [{type: "constrainedType", text: "p"},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeVariable", text: "b"},[]]]])

  t.deepEqual(runP ("constrainedType") ("f Integer"),
        [{type: "constrainedType", text: "f"},[
            [{type: "typeConstructor", text: "Integer"},[]]]])

  t.end()
})

Tape.test("record", t => {
  t.deepEqual(runP ("record") ("{ foo :: Integer, bar :: Maybe [a] }"),
        [{type: "record", text: ""},[
            [{type: "recordField", text: "foo"},[
                [{type: "typeConstructor", text: "Integer"},[]]]],
            [{type: "recordField", text: "bar"},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "list", text: ""},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]]])

  t.deepEqual(runP ("record") ("{ foo :: Integer }"),
        [{type: "record", text: ""},[
            [{type: "recordField", text: "foo"},[
                [{type: "typeConstructor", text: "Integer"},[]]]]]])

  t.end()
})

Tape.test("uncurriedFunction", t => {
  t.deepEqual(runP ("uncurriedFunction") ("(a, b) -> Integer"),
        [{type: "uncurriedFunction", text: ""},[
            [{type: "uncurriedFunctionParams", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]],
            [{type: "typeConstructor", text: "Integer"},[]]]])

  t.deepEqual(runP ("uncurriedFunction") ("(Bool, [a], Maybe a) -> Either String a"),
        [{type: "uncurriedFunction", text: ""},[
            [{type: "uncurriedFunctionParams", text: ""},[
                [{type: "typeConstructor", text: "Bool"},[]],
                [{type: "list", text: ""},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]],
            [{type: "typeConstructor", text: "Either"},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeVariable", text: "a"},[]]]]]])

  t.end()
})

Tape.test("method", t => {
  t.deepEqual(runP ("method") ("Foo ~> Integer"),
        [{type: "method", text: ""},[
            [{type: "typeConstructor", text: "Foo"},[]],
            [{type: "typeConstructor", text: "Integer"},[]]]])

  t.deepEqual(runP ("method") ("Maybe a ~> b -> Boolean"),
        [{type: "method", text: ""},[
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]]])

  t.end()
})

Tape.test("function", t => {
  t.deepEqual(runP ("function") ("(a -> b) -> [a] -> [b]"),
        [{type: "function", text: ""},[
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]],
            [{type: "list", text: ""},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "list", text: ""},[
                [{type: "typeVariable", text: "b"},[]]]]]])

  t.deepEqual(runP ("function") ("(a -> b) -> a -> b"),
        [{type: "function", text: ""},[
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]],
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeVariable", text: "b"},[]]]])

  t.deepEqual(runP ("function") ("f a -> b"),
        [{type: "function", text: ""},[
            [{type: "constrainedType", text: "f"},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "typeVariable", text: "b"},[]]]])

  t.deepEqual(runP ("function") ("a -> Boolean"),
        [{type: "function", text: ""},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeConstructor", text: "Boolean"},[]]]])

  t.deepEqual(runP ("function") ("Maybe a -> a"),
        [{type: "function", text: ""},[
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "typeVariable", text: "a"},[]]]])

  t.deepEqual(runP ("function") ("a -> b"),
        [{type: "function", text: ""},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeVariable", text: "b"},[]]]])
  t.end()
})

Tape.test("list", t => {
  t.deepEqual(runP ("list") ("[[Integer]]"),
        [{type: "list", text: ""},[
            [{type: "list", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]]]]]])

  t.deepEqual(runP ("list") ("[a]"),
        [{type: "list", text: ""},[
            [{type: "typeVariable", text: "a"},[]]]])

  t.deepEqual(runP ("list") ("[Maybe a]"),
        [{type: "list", text: ""},[
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeVariable", text: "a"},[]]]]]])

  t.deepEqual(runP ("list") ("[a -> Bool]"),
        [{type: "list", text: ""},[
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Bool"},[]]]]]])

  t.end()
})

Tape.test("typeConstructor", t => {
  t.deepEqual(runP ("typeConstructor") ("Maybe (Either a (Maybe b))"),
        [{type: "typeConstructor", text: "Maybe"},[
            [{type: "typeConstructor", text: "Either"},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]]])

  t.deepEqual(runP ("typeConstructor") ("Maybe (Either a b)"),
        [{type: "typeConstructor", text: "Maybe"},[
            [{type: "typeConstructor", text: "Either"},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]]]])

  t.deepEqual(runP ("typeConstructor") ("Maybe (f a)"),
        [{type: "typeConstructor", text: "Maybe"},[
            [{type: "constrainedType", text: "f"},[
                [{type: "typeVariable", text: "a"},[]]]]]])
  
  t.deepEqual(runP ("typeConstructor") ("Maybe Integer"),
        [{type: "typeConstructor", text: "Maybe"},[
            [{type: "typeConstructor", text: "Integer"},[]]]])

  t.deepEqual(runP ("typeConstructor") ("Either Integer Bool"),
        [{type: "typeConstructor", text: "Either"},[
            [{type: "typeConstructor", text: "Integer"},[]],
            [{type: "typeConstructor", text: "Bool"},[]]]])
  
  t.deepEqual(runP ("typeConstructor") ("Triple a (Maybe Bool) Integer"),
        [{type: "typeConstructor", text: "Triple"},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeConstructor", text: "Bool"},[]]]],
            [{type: "typeConstructor", text: "Integer"},[]]]])

  t.deepEqual(runP ("typeConstructor") ("Either a (f b)"),
        [{type: "typeConstructor", text: "Either"},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "constrainedType", text: "f"},[
                [{type: "typeVariable", text: "b"},[]]]]]])

  t.deepEqual(runP ("typeConstructor") ("Either a b"),
        [{type: "typeConstructor", text: "Either"},[
            [{type: "typeVariable", text: "a"},[]],
            [{type: "typeVariable", text: "b"},[]]]])
  
  t.deepEqual(runP ("typeConstructor") ("Maybe a"),
        [{type: "typeConstructor", text: "Maybe"},[
            [{type: "typeVariable", text: "a"},[]]]])
  
  t.deepEqual(runP ("typeConstructor") ("A b"),
        [{type: "typeConstructor", text: "A"},[
            [{type: "typeVariable", text: "b"},[]]]])
  
  t.deepEqual(runP ("typeConstructor") ("Bool"),
        [{type: "typeConstructor", text: "Bool"},[]])
  
  t.deepEqual(runP ("typeConstructor") ("A"),
        [{type: "typeConstructor", text: "A"},[]])
  t.end()
})