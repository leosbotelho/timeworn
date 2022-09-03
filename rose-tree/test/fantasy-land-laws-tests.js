/*
 * References:
 * - Fantasy Land
 * - fantasy-laws source
 * - http://www.tomharding.me/fantasy-land/
 * - http://hackage.haskell.org/
 */

import Tape from "tape" 

import laws from "fantasy-laws"
import jsc  from "jsverify"

import {create, env} from "sanctuary"
import $             from "sanctuary-def"

import {equals}      from "sanctuary-type-classes"

import _Tree from "../index"
import Rose  from "../type"

/*
 * Bootstrap
 *  Sanctuary
 */

// hasMethod :: String -> a -> Boolean
const hasMethod = name => x => x != null && typeof x[name] === "function"

// hack
// NakedArbitrary :: Type
const NakedArbitrary = $.NullaryType
  ("rose-tree/NakedArbitrary")
  ("")
  (x => hasMethod ("generator") (x) && hasMethod ("shrink") (x) && hasMethod ("show") (x))

// info: Sanctuary type checking currently degrades performance quite a bit
// checkTypes :: Boolean
const checkTypes = true

const _create = checkTypes => env => create =>
  create({
    checkTypes,
    env
  })

const {Tree : TreeType} = Rose

const create_ = _create (checkTypes) 
                        (env.concat ([TreeType ($.Unknown),
                                      NakedArbitrary]))
 
const S   = create_ (create)
const def = create_ ($.create)

// for `Compose f g a` usage
const __uncheckedTraverse = true

const {Node, Tree} = _Tree ({S, $, def, Rose, __uncheckedTraverse})

/*
 * Bootstrap
 *  JSVerify
 */

// _blessGeneratae :: a -> arbitrary a
const _blessGeneratae = x => jsc.bless ({
  generator: _ => x
})

// type BasicArb = Integer

// basicArb :: arbitrary BasicArb
const basicArb   = jsc.integer // could be any `Setoid` arbitrary

// basicArbFn :: arbitrary (b -> BasicArb)
const basicArbFn = jsc.fn (basicArb)

// makeTreeArb :: arbitrary a -> arbitrary (RoseTree a)
const makeTreeArb = a => S.prop ("TreeArb") 
  (jsc.letrec (tie => ({
    "TreeArb": jsc.record ({
      datum: a, 
      children:
         jsc.small (jsc.array (tie ("TreeArb")))
    })
      .smap(
        ({datum : x, children : txs}) => Node (x) (txs),

        ({datum, children}) => ({datum, children}),

        S.show
      )
  })))

// TreeArb :: arbitrary (RoseTree BasicArb)
const TreeArb   = makeTreeArb (basicArb)

// TreeFnArb :: arbitrary (RoseTree (b -> BasicArb))
const TreeFnArb = makeTreeArb (basicArbFn)

// TreeArbFn :: arbitrary (b -> RoseTree BasicArb)
const TreeArbFn = jsc.fn (TreeArb)

// _uncurry2 :: (a -> b -> c) -> (a, b) -> c
const _uncurry2 = f => (x, y) =>
  f (x) (y)

// reducerFn :: (b, a) -> b
const reducerFn = _uncurry2 (S.add)

// reducerFnArb :: arbitrary ((b, a) -> b)
const reducerFnArb = _blessGeneratae (reducerFn)

// TraversableArbs :: { 
//   F :: arbitrary (F' a), 
//   G :: arbitrary (G' a), 
//   t :: arbitrary (F' a -> G' a),
//   u_Naturality  :: arbitrary (RoseTree (F' a)),
//   u_Composition :: arbitrary (RoseTree (F' (G' a)))
// }
const TraversableArbs = (() => {

  const _F = S.Maybe
  const  F = _blessGeneratae (_F)

  const makeF_a = a =>
    a.smap(
      x   => S.Just (x),
      S.maybeToNullable,

      S.show
    ) 

  const _G = Array
  const  G = _blessGeneratae (_G)

  const t  = _blessGeneratae (
    S.maybe ([]) (S.of (Array))
  )

  const u_Naturality = makeTreeArb (
    makeF_a (basicArb)
  )

  const u_Composition = makeTreeArb (
    makeF_a (jsc.array (basicArb))
  )

  return {
    F, G, t, u_Naturality, u_Composition
  }
})()

/*
 * Tests
 *  Fantasy Land `laws`
 */

const _ = (...xs) => xs

const testLaws = t => laws =>
  S.flip (S.compose) (S.pairs) (S.map (x =>

    t.doesNotThrow (
      S.prop (S.fst (x)) (laws) (...S.snd (x)), 
      S.fst (x)
    )))

/* Main */

Tape.test ("Setoid", t => {

  const _laws = laws.Setoid

  testLaws (t) (_laws) ({
    reflexivity:  _ (TreeArb),
    symmetry:     _ (TreeArb, TreeArb),
    transitivity: _ (TreeArb, TreeArb, TreeArb)
  })

  t.end ()
})

Tape.test ("Functor", t => {

  const _laws = laws.Functor (equals)

  testLaws (t) (_laws) ({
    identity:    _ (TreeArb),
    composition: _ (TreeArb, basicArbFn, basicArbFn)
  })

  t.end ()
})

Tape.test ("Apply", t => {

  const _laws = laws.Apply (equals)

  testLaws (t) (_laws) ({
    composition: _ (TreeFnArb, TreeFnArb, TreeArb)
  })

  t.end ()
})

Tape.test ("Applicative", t => {

  const _laws = laws.Applicative (equals, Tree)

  testLaws (t) (_laws) ({
    identity:     _ (TreeArb),
    homomorphism: _ (basicArbFn, TreeArb),
    interchange:  _ (TreeFnArb,  TreeArb)
  })

  t.end ()
})

Tape.test ("Chain", t => {

  const _laws = laws.Chain (equals)

  testLaws (t) (_laws) ({
    associativity: _ (TreeArb, TreeArbFn, TreeArbFn)
  })

  t.end ()
})

Tape.test ("Monad", t => {

  const _laws = laws.Monad (equals, Tree)

  testLaws (t) (_laws) ({
    leftIdentity:  _ (TreeArbFn, TreeArb),
    rightIdentity: _ (TreeArb)
  })

  t.end ()
})

Tape.test ("Foldable", t => {

  const _laws = laws.Foldable (equals)

  testLaws (t) (_laws) ({
    associativity: _ (reducerFnArb, basicArb, TreeArb)
  })

  t.end ()
})

Tape.test ("Traversable", t => {

  const _laws = laws.Traversable (equals)

  const {F, G, t : f, u_Naturality, u_Composition} = TraversableArbs

  testLaws (t) (_laws) ({
    naturality:  _ (F, G, f, u_Naturality),
    identity:    _ (F, TreeArb),
    composition: _ (F, G, u_Composition)
  })

  t.end ()
})