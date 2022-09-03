import Tape    from "tape"

import HMLang  from "../index"
import _runP   from "../run-parser"

const parse    = _runP (HMLang) ("declaration")

/*
 * Type expressions pursuant to:
 * https://github.com/sanctuary-js/sanctuary
 */

Tape.test("sanctuary", t => {

    t.deepEqual(
        parse ("create :: { checkTypes :: Boolean, env :: Array Type } -> Module"), {
        name: "create",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "record", text: ""},[
                    [{type: "recordField", text: "checkTypes"},[
                        [{type: "typeConstructor", text: "Boolean"},[]]]],
                    [{type: "recordField", text: "env"},[
                        [{type: "typeConstructor", text: "Array"},[
                            [{type: "typeConstructor", text: "Type"},[]]]]]]]],
                [{type: "typeConstructor", text: "Module"},[]]]]
    })

    t.deepEqual(
        parse ("env :: Array Type"), {
        name: "env",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "Array"},[
                [{type: "typeConstructor", text: "Type"},[]]]]
    })

    t.deepEqual(
        parse ("unchecked :: Module"), {
        name: "unchecked",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "Module"},[]]
    })

    t.deepEqual(
        parse ("type :: Any -> { namespace :: Maybe String, name :: String, version :: NonNegativeInteger }"), {
        name: "type",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Any"},[]],
                [{type: "record", text: ""},[
                    [{type: "recordField", text: "namespace"},[
                        [{type: "typeConstructor", text: "Maybe"},[
                            [{type: "typeConstructor", text: "String"},[]]]]]],
                    [{type: "recordField", text: "name"},[
                        [{type: "typeConstructor", text: "String"},[]]]],
                    [{type: "recordField", text: "version"},[
                        [{type: "typeConstructor", text: "NonNegativeInteger"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("is :: Type -> Any -> Boolean"), {
        name: "is",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Any"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("show :: Any -> String"), {
        name: "show",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Any"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("equals :: Setoid a => a -> a -> Boolean"), {
        name: "equals",
        constraints: [
            [{type: "constraint", text: "Setoid"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("lt :: Ord a => a -> a -> Boolean"), {
        name: "lt",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("lte :: Ord a => a -> a -> Boolean"), {
        name: "lte",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("gt :: Ord a => a -> a -> Boolean"), {
        name: "gt",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
  })

    t.deepEqual(
        parse ("gte :: Ord a => a -> a -> Boolean"), {
        name: "gte",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("min :: Ord a => a -> a -> a"), {
        name: "min",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("max :: Ord a => a -> a -> a"), {
        name: "max",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("id :: Category c => TypeRep c -> c"), {
        name: "id",
        constraints: [
            [{type: "constraint", text: "Category"},[
                [{type: "typeVariable", text: "c"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "typeVariable", text: "c"},[]]]]
    })

    t.deepEqual(
        parse ("concat :: Semigroup a => a -> a -> a"), {
        name: "concat",
        constraints: [
            [{type: "constraint", text: "Semigroup"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("empty :: Monoid a => TypeRep a -> a"), {
        name: "empty",
        constraints: [
            [{type: "constraint", text: "Monoid"},[
                [{type: "typeVariable", text: "a"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("invert :: Group g => g -> g"), {
        name: "invert",
        constraints: [
            [{type: "constraint", text: "Group"},[
                [{type: "typeVariable", text: "g"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "g"},[]],
                [{type: "typeVariable", text: "g"},[]]]]
    })

    t.deepEqual(
        parse ("filter :: Filterable f => (a -> Boolean) -> f a -> f a"), {
        name: "filter",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("reject :: Filterable f => (a -> Boolean) -> f a -> f a"), {
        name: "reject",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("takeWhile :: Filterable f => (a -> Boolean) -> f a -> f a"), {
        name: "takeWhile",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("dropWhile :: Filterable f => (a -> Boolean) -> f a -> f a"), {
        name: "dropWhile",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("map :: Functor f => (a -> b) -> f a -> f b"), {
        name: "map",
        constraints: [
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("flip :: Functor f => f (a -> b) -> a -> f b"), {
        name: "flip",
        constraints: [
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("bimap :: Bifunctor f => (a -> b) -> (c -> d) -> f a c -> f b d"), {
        name: "bimap",
        constraints: [
            [{type: "constraint", text: "Bifunctor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "c"},[]],
                    [{type: "typeVariable", text: "d"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "d"},[]]]]]]
    })

    t.deepEqual(
        parse ("mapLeft :: Bifunctor f => (a -> b) -> f a c -> f b c"), {
        name: "mapLeft",
        constraints: [
            [{type: "constraint", text: "Bifunctor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]]]]
    })

    t.deepEqual(
        parse ("promap :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d"), {
        name: "promap",
        constraints: [
            [{type: "constraint", text: "Profunctor"},[
                [{type: "typeVariable", text: "p"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "c"},[]],
                    [{type: "typeVariable", text: "d"},[]]]],
                [{type: "constrainedType", text: "p"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "p"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "d"},[]]]]]]
    })

    t.deepEqual(
        parse ("alt :: Alt f => f a -> f a -> f a"), {
        name: "alt",
        constraints: [
            [{type: "constraint", text: "Alt"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("zero :: Plus f => TypeRep f -> f a"), {
        name: "zero",
        constraints: [
            [{type: "constraint", text: "Plus"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "f"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("reduce :: Foldable f => (b -> a -> b) -> b -> f a -> b"), {
        name: "reduce",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("traverse :: (Applicative f, Traversable t) => TypeRep f -> (a -> f b) -> t a -> f (t b)"), {
        name: "traverse",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Traversable"},[
                [{type: "typeVariable", text: "t"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "f"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "t"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "constrainedType", text: "t"},[
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("sequence :: (Applicative f, Traversable t) => TypeRep f -> t (f a) -> f (t a)"), {
        name: "sequence",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Traversable"},[
                [{type: "typeVariable", text: "t"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "f"},[]]]],
                [{type: "constrainedType", text: "t"},[
                    [{type: "constrainedType", text: "f"},[
                        [{type: "typeVariable", text: "a"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "constrainedType", text: "t"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("ap :: Apply f => f (a -> b) -> f a -> f b"), {
        name: "ap",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("lift2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c"), {
        name: "lift2",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "c"},[]]]]]]
    })

    t.deepEqual(
        parse ("lift3 :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d"), {
        name: "lift3",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]],
                    [{type: "typeVariable", text: "d"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "d"},[]]]]]]
    })

    t.deepEqual(
        parse ("apFirst :: Apply f => f a -> f b -> f a"), {
        name: "apFirst",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("apSecond :: Apply f => f a -> f b -> f b"), {
        name: "apSecond",
        constraints: [
            [{type: "constraint", text: "Apply"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("of :: Applicative f => TypeRep f -> a -> f a"), {
        name: "of",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "f"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("chain :: Chain m => (a -> m b) -> m a -> m b"), {
        name: "chain",
        constraints: [
            [{type: "constraint", text: "Chain"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "constrainedType", text: "m"},[
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("join :: Chain m => m (m a) -> m a"), {
        name: "join",
        constraints: [
            [{type: "constraint", text: "Chain"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "m"},[
                    [{type: "constrainedType", text: "m"},[
                        [{type: "typeVariable", text: "a"},[]]]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("chainRec :: ChainRec m => TypeRep m -> (a -> m (Either a b)) -> a -> m b"), {
        name: "chainRec",
        constraints: [
            [{type: "constraint", text: "ChainRec"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "m"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "constrainedType", text: "m"},[
                        [{type: "typeConstructor", text: "Either"},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "typeVariable", text: "b"},[]]]]]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("extend :: Extend w => (w a -> b) -> w a -> w b"), {
        name: "extend",
        constraints: [
            [{type: "constraint", text: "Extend"},[
                [{type: "typeVariable", text: "w"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "constrainedType", text: "w"},[
                        [{type: "typeVariable", text: "a"},[]]]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("duplicate :: Extend w => w a -> w (w a)"), {
        name: "duplicate",
        constraints: [
            [{type: "constraint", text: "Extend"},[
                [{type: "typeVariable", text: "w"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "w"},[
                    [{type: "constrainedType", text: "w"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("extract :: Comonad w => w a -> a"), {
        name: "extract",
        constraints: [
            [{type: "constraint", text: "Comonad"},[
                [{type: "typeVariable", text: "w"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "w"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("contramap :: Contravariant f => (b -> a) -> f a -> f b"), {
        name: "contramap",
        constraints: [
            [{type: "constraint", text: "Contravariant"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("I :: a -> a"), {
        name: "I",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("K :: a -> b -> a"), {
        name: "K",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("T :: a -> (a -> b) -> b"), {
        name: "T",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("curry2 :: ((a, b) -> c) -> a -> b -> c"), {
        name: "curry2",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]]]]
    })

    t.deepEqual(
        parse ("curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d"), {
        name: "curry3",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "c"},[]]]],
                    [{type: "typeVariable", text: "d"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]],
                [{type: "typeVariable", text: "d"},[]]]]
    })

    t.deepEqual(
        parse ("curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e"), {
        name: "curry4",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "c"},[]],
                        [{type: "typeVariable", text: "d"},[]]]],
                    [{type: "typeVariable", text: "e"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]],
                [{type: "typeVariable", text: "d"},[]],
                [{type: "typeVariable", text: "e"},[]]]]
    })

    t.deepEqual(
        parse ("curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f"), {
        name: "curry5",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "uncurriedFunction", text: ""},[
                    [{type: "uncurriedFunctionParams", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]],
                        [{type: "typeVariable", text: "c"},[]],
                        [{type: "typeVariable", text: "d"},[]],
                        [{type: "typeVariable", text: "e"},[]]]],
                    [{type: "typeVariable", text: "f"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]],
                [{type: "typeVariable", text: "d"},[]],
                [{type: "typeVariable", text: "e"},[]],
                [{type: "typeVariable", text: "f"},[]]]]
    })

    t.deepEqual(
        parse ("compose :: Semigroupoid s => s b c -> s a b -> s a c"), {
        name: "compose",
        constraints: [
            [{type: "constraint", text: "Semigroupoid"},[
                [{type: "typeVariable", text: "s"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "s"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "constrainedType", text: "s"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "s"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "c"},[]]]]]]
    })

    t.deepEqual(
        parse ("pipe :: Foldable f => f (Any -> Any) -> a -> b"), {
        name: "pipe",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeConstructor", text: "Any"},[]],
                        [{type: "typeConstructor", text: "Any"},[]]]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("pipeK :: (Foldable f, Chain m) => f (Any -> m Any) -> m a -> m b"), {
        name: "pipeK",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Chain"},[
                [{type: "typeVariable", text: "m"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeConstructor", text: "Any"},[]],
                        [{type: "constrainedType", text: "m"},[
                            [{type: "typeConstructor", text: "Any"},[]]]]]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("on :: (b -> b -> c) -> (a -> b) -> a -> a -> c"), {
        name: "on",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "c"},[]]]]
    })

    t.deepEqual(
        parse ("PairType :: Type -> Type -> Type"), {
        name: "PairType",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Type"},[]]]]
    })

    t.deepEqual(
        parse ("Pair :: a -> b -> Pair a b"), {
        name: "Pair",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Pair"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("fst :: Pair a b -> a"), {
        name: "fst",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Pair"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("snd :: Pair a b -> b"), {
        name: "snd",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Pair"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("swap :: Pair a b -> Pair b a"), {
        name: "swap",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Pair"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Pair"},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("MaybeType :: Type -> Type"), {
        name: "MaybeType",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Type"},[]]]]
    })

    t.deepEqual(
        parse ("Maybe :: TypeRep Maybe"), {
        name: "Maybe",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "TypeRep"},[
                [{type: "typeConstructor", text: "Maybe"},[]]]]
    })

    t.deepEqual(
        parse ("Nothing :: Maybe a"), {
        name: "Nothing",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "Maybe"},[
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("Just :: a -> Maybe a"), {
        name: "Just",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("isNothing :: Maybe a -> Boolean"), {
        name: "isNothing",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("isJust :: Maybe a -> Boolean"), {
        name: "isJust",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("fromMaybe :: a -> Maybe a -> a"), {
        name: "fromMaybe",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("maybeToNullable :: Maybe a -> Nullable a"), {
        name: "maybeToNullable",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Nullable"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("maybe :: b -> (a -> b) -> Maybe a -> b"), {
        name: "maybe",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "b"},[]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("justs :: (Filterable f, Functor f) => f (Maybe a) -> f a"), {
        name: "justs",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "Maybe"},[
                        [{type: "typeVariable", text: "a"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("mapMaybe :: (Filterable f, Functor f) => (a -> Maybe b) -> f a -> f b"), {
        name: "mapMaybe",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Maybe"},[
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("encase :: (a -> b) -> a -> Maybe b"), {
        name: "encase",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("encase2 :: (a -> b -> c) -> a -> b -> Maybe c"), {
        name: "encase2",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "c"},[]]]]]]
    })

    t.deepEqual(
        parse ("encase3 :: (a -> b -> c -> d) -> a -> b -> c -> Maybe d"), {
        name: "encase3",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]],
                    [{type: "typeVariable", text: "d"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "d"},[]]]]]]
    })

    t.deepEqual(
        parse ("maybeToEither :: a -> Maybe b -> Either a b"), {
        name: "maybeToEither",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("EitherType :: Type -> Type -> Type"), {
        name: "EitherType",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Type"},[]],
                [{type: "typeConstructor", text: "Type"},[]]]]
    })

    t.deepEqual(
        parse ("Either :: TypeRep Either"), {
        name: "Either",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "TypeRep"},[
                [{type: "typeConstructor", text: "Either"},[]]]]
    })

    t.deepEqual(
        parse ("Left :: a -> Either a b"), {
        name: "Left",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("Right :: b -> Either a b"), {
        name: "Right",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("isLeft :: Either a b -> Boolean"), {
        name: "isLeft",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("isRight :: Either a b -> Boolean"), {
        name: "isRight",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("fromEither :: b -> Either a b -> b"), {
        name: "fromEither",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("either :: (a -> c) -> (b -> c) -> Either a b -> c"), {
        name: "either",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "c"},[]]]]
    })

    t.deepEqual(
        parse ("lefts :: (Filterable f, Functor f) => f (Either a b) -> f a"), {
        name: "lefts",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "Either"},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("rights :: (Filterable f, Functor f) => f (Either a b) -> f b"), {
        name: "rights",
        constraints: [
            [{type: "constraint", text: "Filterable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Functor"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "Either"},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("tagBy :: (a -> Boolean) -> a -> Either a a"), {
        name: "tagBy",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("encaseEither :: (Error -> l) -> (a -> r) -> a -> Either l r"), {
        name: "encaseEither",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Error"},[]],
                    [{type: "typeVariable", text: "l"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "r"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "l"},[]],
                    [{type: "typeVariable", text: "r"},[]]]]]]
    })

    t.deepEqual(
        parse ("encaseEither2 :: (Error -> l) -> (a -> b -> r) -> a -> b -> Either l r"), {
        name: "encaseEither2",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Error"},[]],
                    [{type: "typeVariable", text: "l"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "r"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "l"},[]],
                    [{type: "typeVariable", text: "r"},[]]]]]]
    })

    t.deepEqual(
        parse ("encaseEither3 :: (Error -> l) -> (a -> b -> c -> r) -> a -> b -> c -> Either l r"), {
        name: "encaseEither3",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Error"},[]],
                    [{type: "typeVariable", text: "l"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]],
                    [{type: "typeVariable", text: "r"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeVariable", text: "c"},[]],
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "l"},[]],
                    [{type: "typeVariable", text: "r"},[]]]]]]
    })

    t.deepEqual(
        parse ("eitherToMaybe :: Either a b -> Maybe b"), {
        name: "eitherToMaybe",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Either"},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("and :: Boolean -> Boolean -> Boolean"), {
        name: "and",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Boolean"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("or :: Boolean -> Boolean -> Boolean"), {
        name: "or",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Boolean"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("not :: Boolean -> Boolean"), {
        name: "not",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Boolean"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("complement :: (a -> Boolean) -> a -> Boolean"), {
        name: "complement",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("ifElse :: (a -> Boolean) -> (a -> b) -> (a -> b) -> a -> b"), {
        name: "ifElse",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("when :: (a -> Boolean) -> (a -> a) -> a -> a"), {
        name: "when",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("unless :: (a -> Boolean) -> (a -> a) -> a -> a"), {
        name: "unless",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "a"},[]]]]
    })

    t.deepEqual(
        parse ("allPass :: Foldable f => f (a -> Boolean) -> a -> Boolean"), {
        name: "allPass",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeConstructor", text: "Boolean"},[]]]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("anyPass :: Foldable f => f (a -> Boolean) -> a -> Boolean"), {
        name: "anyPass",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "function", text: ""},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeConstructor", text: "Boolean"},[]]]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("slice :: Integer -> Integer -> Array a -> Maybe (Array a)"), {
        name: "slice",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("at :: Integer -> Array a -> Maybe a"), {
        name: "at",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("head :: Array a -> Maybe a"), {
        name: "head",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("last :: Array a -> Maybe a"), {
        name: "last",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("tail :: Array a -> Maybe (Array a)"), {
        name: "tail",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("init :: Array a -> Maybe (Array a)"), {
        name: "init",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("take :: Integer -> Array a -> Maybe (Array a)"), {
        name: "take",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("takeLast :: Integer -> Array a -> Maybe (Array a)"), {
        name: "takeLast",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("drop :: Integer -> Array a -> Maybe (Array a)"), {
        name: "drop",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("dropLast :: Integer -> Array a -> Maybe (Array a)"), {
        name: "dropLast",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("size :: Foldable f => f a -> Integer"), {
        name: "size",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Integer"},[]]]]
    })

    t.deepEqual(
        parse ("append :: (Applicative f, Semigroup (f a)) => a -> f a -> f a"), {
        name: "append",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Semigroup"},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("prepend :: (Applicative f, Semigroup (f a)) => a -> f a -> f a"), {
        name: "prepend",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Semigroup"},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("joinWith :: String -> Array String -> String"), {
        name: "joinWith",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("elem :: (Setoid a, Foldable f) => a -> f a -> Boolean"), {
        name: "elem",
        constraints: [
            [{type: "constraint", text: "Setoid"},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeVariable", text: "a"},[]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("find :: Foldable f => (a -> Boolean) -> f a -> Maybe a"), {
        name: "find",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("foldMap :: (Monoid m, Foldable f) => TypeRep m -> (a -> m) -> f a -> m"), {
        name: "foldMap",
        constraints: [
            [{type: "constraint", text: "Monoid"},[
                [{type: "typeVariable", text: "m"},[]]]],
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "TypeRep"},[
                    [{type: "typeVariable", text: "m"},[]]]],
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "m"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeVariable", text: "m"},[]]]]
    })

    t.deepEqual(
        parse ("unfoldr :: (b -> Maybe (Pair a b)) -> b -> Array a"), {
        name: "unfoldr",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeConstructor", text: "Maybe"},[
                        [{type: "typeConstructor", text: "Pair"},[
                            [{type: "typeVariable", text: "a"},[]],
                            [{type: "typeVariable", text: "b"},[]]]]]]]],
                [{type: "typeVariable", text: "b"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("range :: Integer -> Integer -> Array Integer"), {
        name: "range",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "Integer"},[]]]]]]
    })

    t.deepEqual(
        parse ("groupBy :: (a -> a -> Boolean) -> Array a -> Array (Array a)"), {
        name: "groupBy",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "Array"},[
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("reverse :: (Applicative f, Foldable f, Monoid (f a)) => f a -> f a"), {
        name: "reverse",
        constraints: [
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]],
            [{type: "constraint", text: "Monoid"},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("sort :: (Ord a, Applicative m, Foldable m, Monoid (m a)) => m a -> m a"), {
        name: "sort",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "a"},[]]]],
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "m"},[]]]],
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "m"},[]]]],
            [{type: "constraint", text: "Monoid"},[
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("sortBy :: (Ord b, Applicative m, Foldable m, Monoid (m a)) => (a -> b) -> m a -> m a"), {
        name: "sortBy",
        constraints: [
            [{type: "constraint", text: "Ord"},[
                [{type: "typeVariable", text: "b"},[]]]],
            [{type: "constraint", text: "Applicative"},[
                [{type: "typeVariable", text: "m"},[]]]],
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "m"},[]]]],
            [{type: "constraint", text: "Monoid"},[
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "constrainedType", text: "m"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("zip :: Array a -> Array b -> Array (Pair a b)"), {
        name: "zip",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "Pair"},[
                        [{type: "typeVariable", text: "a"},[]],
                        [{type: "typeVariable", text: "b"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("zipWith :: (a -> b -> c) -> Array a -> Array b -> Array c"), {
        name: "zipWith",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeVariable", text: "a"},[]],
                    [{type: "typeVariable", text: "b"},[]],
                    [{type: "typeVariable", text: "c"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "b"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "c"},[]]]]]]
    })

    t.deepEqual(
        parse ("prop :: String -> a -> b"), {
        name: "prop",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("props :: Array String -> a -> b"), {
        name: "props",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeVariable", text: "b"},[]]]]
    })

    t.deepEqual(
        parse ("get :: (Any -> Boolean) -> String -> a -> Maybe b"), {
        name: "get",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Any"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("gets :: (Any -> Boolean) -> Array String -> a -> Maybe b"), {
        name: "gets",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Any"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "b"},[]]]]]]
    })

    t.deepEqual(
        parse ("singleton :: String -> a -> StrMap a"), {
        name: "singleton",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("insert :: String -> a -> StrMap a -> StrMap a"), {
        name: "insert",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeVariable", text: "a"},[]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("remove :: String -> StrMap a -> StrMap a"), {
        name: "remove",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("keys :: StrMap a -> Array String"), {
        name: "keys",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("values :: StrMap a -> Array a"), {
        name: "values",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("pairs :: StrMap a -> Array (Pair String a)"), {
        name: "pairs",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "Pair"},[
                        [{type: "typeConstructor", text: "String"},[]],
                        [{type: "typeVariable", text: "a"},[]]]]]]]]
    })

    t.deepEqual(
        parse ("fromPairs :: Foldable f => f (Pair String a) -> StrMap a"), {
        name: "fromPairs",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "Pair"},[
                        [{type: "typeConstructor", text: "String"},[]],
                        [{type: "typeVariable", text: "a"},[]]]]]],
                [{type: "typeConstructor", text: "StrMap"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("negate :: ValidNumber -> ValidNumber"), {
        name: "negate",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "ValidNumber"},[]],
                [{type: "typeConstructor", text: "ValidNumber"},[]]]]
    })

    t.deepEqual(
        parse ("add :: FiniteNumber -> FiniteNumber -> FiniteNumber"), {
        name: "add",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
    })

    t.deepEqual(
        parse ("sum :: Foldable f => f FiniteNumber -> FiniteNumber"), {
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
        parse ("sub :: FiniteNumber -> FiniteNumber -> FiniteNumber"), {
        name: "sub",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
    })

    t.deepEqual(
        parse ("mult :: FiniteNumber -> FiniteNumber -> FiniteNumber"), {
        name: "mult",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
    })

    t.deepEqual(
        parse ("product :: Foldable f => f FiniteNumber -> FiniteNumber"), {
        name: "product",
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
        parse ("div :: NonZeroFiniteNumber -> FiniteNumber -> FiniteNumber"), {
        name: "div",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "NonZeroFiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
    })

    t.deepEqual(
        parse ("pow :: FiniteNumber -> FiniteNumber -> FiniteNumber"), {
        name: "pow",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]],
                [{type: "typeConstructor", text: "FiniteNumber"},[]]]]
    })

    t.deepEqual(
        parse ("mean :: Foldable f => f FiniteNumber -> Maybe FiniteNumber"), {
        name: "mean",
        constraints: [
            [{type: "constraint", text: "Foldable"},[
                [{type: "typeVariable", text: "f"},[]]]]],
        type: 
            [{type: "function", text: ""},[
                [{type: "constrainedType", text: "f"},[
                    [{type: "typeConstructor", text: "FiniteNumber"},[]]]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "FiniteNumber"},[]]]]]]
    })

    t.deepEqual(
        parse ("even :: Integer -> Boolean"), {
        name: "even",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("odd :: Integer -> Boolean"), {
        name: "odd",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Integer"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("parseDate :: String -> Maybe ValidDate"), {
        name: "parseDate",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "ValidDate"},[]]]]]]
    })

    t.deepEqual(
        parse ("parseFloat :: String -> Maybe Number"), {
        name: "parseFloat",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Number"},[]]]]]]
    })

    t.deepEqual(
        parse ("parseInt :: Radix -> String -> Maybe Integer"), {
        name: "parseInt",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Radix"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "Integer"},[]]]]]]
    })

    t.deepEqual(
        parse ("parseJson :: (Any -> Boolean) -> String -> Maybe a"), {
        name: "parseJson",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "function", text: ""},[
                    [{type: "typeConstructor", text: "Any"},[]],
                    [{type: "typeConstructor", text: "Boolean"},[]]]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeVariable", text: "a"},[]]]]]]
    })

    t.deepEqual(
        parse ("regex :: RegexFlags -> String -> RegExp"), {
        name: "regex",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "RegexFlags"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "RegExp"},[]]]]
    })

    t.deepEqual(
        parse ("regexEscape :: String -> String"), {
        name: "regexEscape",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("test :: RegExp -> String -> Boolean"), {
        name: "test",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "RegExp"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Boolean"},[]]]]
    })

    t.deepEqual(
        parse ("match :: NonGlobalRegExp -> String -> Maybe { match :: String, groups :: Array (Maybe String) }"), {
        name: "match",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "NonGlobalRegExp"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "record", text: ""},[
                        [{type: "recordField", text: "match"},[
                            [{type: "typeConstructor", text: "String"},[]]]],
                        [{type: "recordField", text: "groups"},[
                            [{type: "typeConstructor", text: "Array"},[
                                [{type: "typeConstructor", text: "Maybe"},[
                                    [{type: "typeConstructor", text: "String"},[]]]]]]]]]]]]]]
    })

    t.deepEqual(
        parse ("groups :: Array (Maybe String)"), {
        name: "groups",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "Array"},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("matchAll :: GlobalRegExp -> String -> Array { match :: String, groups :: Array (Maybe String) }"), {
        name: "matchAll",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "GlobalRegExp"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "record", text: ""},[
                        [{type: "recordField", text: "match"},[
                            [{type: "typeConstructor", text: "String"},[]]]],
                        [{type: "recordField", text: "groups"},[
                            [{type: "typeConstructor", text: "Array"},[
                                [{type: "typeConstructor", text: "Maybe"},[
                                    [{type: "typeConstructor", text: "String"},[]]]]]]]]]]]]]]
    })

    t.deepEqual(
        parse ("groups :: Array (Maybe String)"), {
        name: "groups",
        constraints: [],
        type: 
            [{type: "typeConstructor", text: "Array"},[
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("toUpper :: String -> String"), {
        name: "toUpper",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("toLower :: String -> String"), {
        name: "toLower",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("trim :: String -> String"), {
        name: "trim",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("stripPrefix :: String -> String -> Maybe String"), {
        name: "stripPrefix",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("stripSuffix :: String -> String -> Maybe String"), {
        name: "stripSuffix",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Maybe"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("words :: String -> Array String"), {
        name: "words",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("unwords :: Array String -> String"), {
        name: "unwords",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("lines :: String -> Array String"), {
        name: "lines",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("unlines :: Array String -> String"), {
        name: "unlines",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]],
                [{type: "typeConstructor", text: "String"},[]]]]
    })

    t.deepEqual(
        parse ("splitOn :: String -> String -> Array String"), {
        name: "splitOn",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.deepEqual(
        parse ("splitOnRegex :: GlobalRegExp -> String -> Array String"), {
        name: "splitOnRegex",
        constraints: [],
        type: 
            [{type: "function", text: ""},[
                [{type: "typeConstructor", text: "GlobalRegExp"},[]],
                [{type: "typeConstructor", text: "String"},[]],
                [{type: "typeConstructor", text: "Array"},[
                    [{type: "typeConstructor", text: "String"},[]]]]]]
    })

    t.end()
})