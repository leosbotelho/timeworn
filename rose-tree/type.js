import {Array as ArrayType, UnaryType} from "sanctuary-def"

import type from "sanctuary-type-identifiers"

// TreeTypeId :: String
const TreeTypeId = "rose-tree/RoseTree"

// data Tree a = Node a [Tree a]

// Tree :: Type -> Type
const Tree = UnaryType
  (TreeTypeId)
  ("")
  (x    => type (x) === TreeTypeId)
  (tree => [tree.datum])

// type Forest a = [Tree a]

// Forest :: Type -> Type
const Forest = a => ArrayType (Tree (a))


export default {Tree, Forest}