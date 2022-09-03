{ lib }:

let

  inherit (builtins)
    foldl' elemAt
  ;

  inherit (lib.generators)
    toPretty
  ;

  otherwise = _: true;

  matchByPreds = cases: x:
    let err = throw "Handler missing for case `${toPretty {} x}`";
        nul = { r = err; c = true; };

        f = acc: case:
          if !acc.c
            then acc
            else
              let p = elemAt case 0;
                  h = elemAt case 1;

              in if p x then { r = h x; c = false; } else acc
        ;

    in (foldl' f nul cases).r
  ;

in {
  inherit
    otherwise
    matchByPreds
  ;
}
