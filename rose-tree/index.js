/*
 * Reference implementation:
 * https://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.Tree.html#line-148
 */

export default ({S, $, def, Rose, __uncheckedTraverse}) => {

  /* Dependencies */

  const {TypeVariable, Array2, Array : _Array} = $

  const {Tree : TreeType, Forest} = Rose

  const {curry2, flip, compose,
         map, ap, chain, join,
         prepend, concat, reduce,
         show, and, equals} = S

  // For usage in Node$traverse; due to Sanctuary's engineering constraints
  // i.e: We need `Compose f g a` to prove fantasy-laws articulation
  //      of the `Traversable` composition law
  const {lift2 : _lift2, traverse : _traverse} = __uncheckedTraverse ? S["unchecked"] : S

  // util
  const twice = join (compose)

  /* Main */

  // TreeTypeRep :: TypeRep Tree
  const Tree = {
    "@@type": "rose-tree/RoseTree",

    "fantasy-land/of": x =>
      Node (x) ([])
  }

  // a :: Type
  const a = TypeVariable ("a")
  // b :: Type
  const b = TypeVariable ("b")

  // Node :: a -> [Tree a] -> Tree a
  const Node =
  def ("Node")
      ({})
      ([a, Forest (a), TreeType (a)])
      (x => txs => (t => ({
        constructor: Tree,
        ...t,

        "@@show": () => `RoseTree ${show (x)} ${show (txs)}`,

        "fantasy-land/equals": ({datum : y, children : tys}) =>
          and (equals (x) (y)) (equals (txs) (tys)),

        "fantasy-land/map": f =>
          Node (f (x)) (twice (map) (f) (txs)),

        "fantasy-land/ap": function({datum : f, children : tfs}) {
          return Node (f (x))
                      (concat (twice (map) (f) (txs))

                              (map (flip (ap) (this)) (tfs)))
        },

        "fantasy-land/chain":    Node$chain    (t),

        // :: Foldable f => f a ~> ((b, a) -> b, b) -> b
        "fantasy-land/reduce": function(f, acc) {

          return reduce (curry2 (f)) (acc) (flatten (this))
        },

        "fantasy-land/traverse": Node$traverse (t)

      })) ({datum: x, children: txs}))

  // Node$chain :: Chain m => m a ~> (a -> m b) -> m b
  const Node$chain  = ({datum : x, children : txs}) => f =>
    (({datum : x_, children : txs_}) =>
      Node (x_) (concat (txs_)
                        (map (chain (f)) (txs)))
    ) (f (x))

  // Node$traverse :: Applicative f, Traversable t => t a ~> (TypeRep f, a -> f b) -> f (t b)
  const Node$traverse = ({datum : x, children : txs}) => (typeRep, f) =>
    _lift2 (Node) (f (x))
                  (twice (_traverse (typeRep)) (f) (txs))

  // pre-order
  // flatten :: RoseTree a -> [a]
  const flatten =
  def ("flatten")
      ({})
      ([TreeType (a), _Array (a)])

      (({datum : x, children : txs}) =>
        prepend (x) (chain (flatten) (txs)))

  // depth-first
  // foldTree :: (a -> [b] -> b) -> RoseTree a -> b
  const foldTree =
  def ("foldTree")
      ({})
      ([a, _Array (b), b, TreeType (a), b])

      (f => ({datum : x, children : txs}) =>
        f (x) (map (foldTree (f)) (txs)))

  // breadth-first
  // unfoldTree :: (b -> (a, [b])) -> b -> RoseTree a
  const unfoldTree =
  def ("unfoldTree")
      ({})
      ([b, Array2 (a) (_Array (b)), b, TreeType (a)])

      (f => b =>
        (([a, bs]) =>
          Node (a) (unfoldForest (f) (bs))
        ) (f (b)))

  // breadth-first
  // unfoldForest :: (b -> (a, [b])) -> [b] -> [RoseTree a]
  const unfoldForest =
  def ("unfoldForest")
      ({})
      ([b, Array2 (a) (_Array (b)), _Array (b), Forest (a)])

      (f => map (unfoldTree (f)))

  Tree["Node"] = Node

  /*
   * Exports
   */
  return {
    Node,
    Tree,
    flatten,
    foldTree,
    unfoldTree,
    unfoldForest
  }
}