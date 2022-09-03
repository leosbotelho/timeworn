JavaScript fast, lightweight parsers for [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) type expressions using [Parsimmon](https://github.com/jneen/parsimmon) for parsers formulation.

## Features

 - Is able to parse all [Sanctuary](https://github.com/sanctuary-js/sanctuary) and [Fantasy Land](https://github.com/fantasyland/fantasy-land) type declarations
 - Only one self-contained dependency
 - Clear and extensible
 - Fast combinator parsing
 - About 1.8k LoC total
 - More comprehensive tests

## Examples

```javascript
import HMLang from "hm-lang-light"
import _runP  from "hm-lang-light/run-parser"

const runP    = _runP (HMLang)

runP ("declaration")
     ("ap :: Applicative m => m (a -> b) -> m a -> m b")

HMLang.declaration
  .tryParse("reverse :: (Applicative f, Foldable f, Monoid (f a)) => f a -> f a")

// Method - with complex type signature
runP ("declaration")
     ("traverse :: (Applicative f, Traversable t) => t a ~> (TypeRep f, a -> f b) -> f (t b)")

// Record syntax sugar
runP ("declaration")
     ("hello :: a -> { x :: String, y :: a }")

// `FlexibleContexts` enabled by design, also supporting record syntax
HMLang.constraint
  .parse("M (a -> b) {c :: Int}") // {status: true, ...}
```

See [tests](./test) for more examples.

## License

This project is licensed under the MIT License - see the [LICENSE](./LICENSE.md) file for details.
