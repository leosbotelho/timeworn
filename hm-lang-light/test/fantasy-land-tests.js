import Tape    from "tape"

import HMLang  from "../index"
import _runP   from "../run-parser"

const parse    = _runP (HMLang) ("declaration")

/*
 * Type expressions pursuant to:
 * https://github.com/fantasyland/fantasy-land
 */

 Tape.test("fantasy-land", t => {

    t.deepEqual(
        parse ("equals :: Setoid a => a ~> a -> Boolean"), {
        name: "equals",
        constraints: [
            [{type: "constraint", text: "Setoid"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]]]]
    })

    t.deepEqual(
        parse ("lte :: Ord a => a ~> a -> Boolean"), {
        name: "lte",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]]]]
    })

    t.deepEqual(
        parse ("compose :: Semigroupoid c => c i j ~> c j k -> c i k"), {
        name: "compose",
        constraints: [
            [{type: "constraint", text: "Semigroupoid"},[
                [{type: "typeVariable", text: "c"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "c"},[
                    [{type: "typeVariable", text: "i"},[]],
                    [{type: "typeVariable", text: "j"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "constrainedType", text: "c"},[
                        [{type: "typeVariable", text: "j"},[]],
                        [{type: "typeVariable", text: "k"},[]]]],
                    [{type: "constrainedType", text: "c"},[
                        [{type: "typeVariable", text: "i"},[]],
                        [{type: "typeVariable", text: "k"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("id :: Category c => () -> c a a"), {
        name: "id",
        constraints: [
            [{type: "constraint", text: "Category"},[
                [{type: "typeVariable", text: "c"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "thunk", text: ""},[]],
                [{type: "constrainedType", text: "c"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("concat :: Semigroup a => a ~> a -> a"), {
        name: "concat",
        constraints: [
            [{type: "constraint", text: "Semigroup"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("empty :: Monoid m => () -> m"), {
        name: "empty",
        constraints: [
            [{type: "constraint", text: "Monoid"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "thunk", text: ""},[]],
                [{type: "typeVariable", text: "m"},[]]]]
    })

    t.deepEqual(
        parse ("invert :: Group g => g ~> () -> g"), {
        name: "invert",
        constraints: [
            [{type: "constraint", text: "Group"},[
                [{type: "typeVariable", text: "g"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "typeVariable", text: "g"},[]],
                [{type: "function", text: ""},[
                    [{type: "thunk", text: ""},[]],
                    [{type: "typeVariable", text: "g"},[]]]]]]
    })

    t.deepEqual(
        parse ("filter :: Filterable f => f a ~> (a -> Boolean) -> f a"), {
        name: "filter",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeConstructor", text: "Boolean"},[]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("map :: Functor f => f a ~> (a -> b) -> f b"), {
        name: "map",
        constraints: [
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("contramap :: Contravariant f => f a ~> (b -> a) -> f b"), {
        name: "contramap",
        constraints: [
            [{type: "constraint", text: "Contravariant"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "a"},[]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("ap :: Apply f => f a ~> f (a -> b) -> f b"), {
        name: "ap",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "constrainedType", text: "f"},[
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "typeVariable", text: "b"},[]]]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("of :: Applicative f => a -> f a"), {
        name: "of",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("alt :: Alt f => f a ~> f a -> f a"), {
        name: "alt",
        constraints: [
            [{type: "constraint", text: "Alt"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "a"},[]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("zero :: Plus f => () -> f a"), {
        name: "zero",
        constraints: [
            [{type: "constraint", text: "Plus"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "thunk", text: ""},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("reduce :: Foldable f => f a ~> ((b, a) -> b, b) -> b"), {
        name: "reduce",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "uncurriedFunction", text: ""},[
                            [{type: "uncurriedFunctionParams", text: ""},[
                                [{type: "typeVariable", text: "b"},[]],
                                [{type: "typeVariable", text: "a"},[]]]],
                            [{type: "typeVariable", text: "b"},[]]]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("traverse :: (Applicative f, Traversable t) => t a ~> (TypeRep f, a -> f b) -> f (t b)"), {
        name: "traverse",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Traversable"},[
                [{type: "typeVariable", text: "t"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "t"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeConstructor", text: "TypeRep"},[
                            [{type: "typeVariable", text: "f"},[]]]],
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "constrainedType", text: "f"},[
                                [{type: "typeVariable", text: "b"},[]]]]]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "constrainedType", text: "t"},[
                            [{type: "typeVariable", text: "b"},[]]]]]]]]]]
    })

    t.deepEqual(
        parse ("chain :: Chain m => m a ~> (a -> m b) -> m b"), {
        name: "chain",
        constraints: [
            [{type: "constraint", text: "Chain"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "constrainedType", text: "m"},[
                            [{type: "typeVariable", text: "b"},[]]]]]],
                    [{type: "constrainedType", text: "m"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("chainRec :: ChainRec m => ((a -> c, b -> c, a) -> m c, a) -> m b"), {
        name: "chainRec",
        constraints: [
            [{type: "constraint", text: "ChainRec"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "uncurriedFunction", text: ""},[
                [{type: "uncurriedFunctionParams", text: ""},[
                    [{type: "uncurriedFunction", text: ""},[
                        [{type: "uncurriedFunctionParams", text: ""},[
                            [{type: "function", text: ""},[
                                [{type: "typeVariable", text: "a"},[]],
                                [{type: "typeVariable", text: "c"},[]]]],
                            [{type: "function", text: ""},[
                                [{type: "typeVariable", text: "b"},[]],
                                [{type: "typeVariable", text: "c"},[]]]],
                            [{type: "typeVariable", text: "a"},[]]]],
                        [{type: "constrainedType", text: "m"},[
                            [{type: "typeVariable", text: "c"},[]]]]]],
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("extend :: Extend w => w a ~> (w a -> b) -> w b"), {
        name: "extend",
        constraints: [
            [{type: "constraint", text: "Extend"},[
                [{type: "typeVariable", text: "w"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "function", text: ""},[
                        [{type: "constrainedType", text: "w"},[
                            [{type: "typeVariable", text: "a"},[]]]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "constrainedType", text: "w"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("extract :: Comonad w => w a ~> () -> a"), {
        name: "extract",
        constraints: [
            [{type: "constraint", text: "Comonad"},[
                [{type: "typeVariable", text: "w"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "thunk", text: ""},[]],
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("bimap :: Bifunctor f => f a c ~> (a -> b, c -> d) -> f b d"), {
        name: "bimap",
        constraints: [
            [{type: "constraint", text: "Bifunctor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "typeVariable", text: "b"},[]]]],
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "c"},[]],
                            [{type: "typeVariable", text: "d"},[]]]]]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "d"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("promap :: Profunctor p => p b c ~> (a -> b, c -> d) -> p a d"), {
        name: "promap",
        constraints: [
            [{type: "constraint", text: "Profunctor"},[
                [{type: "typeVariable", text: "p"},[]]]]],
        type: 
            [{type: "method", text: ""},[
                [{type: "constrainedType", text: "p"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "typeVariable", text: "b"},[]]]],
                        [{type: "function", text: ""},[
                            [{type: "typeVariable", text: "c"},[]],
                            [{type: "typeVariable", text: "d"},[]]]]]],
                    [{type: "constrainedType", text: "p"},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "d"},[]]]]]]]]
  })

    t.end()
 })