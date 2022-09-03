args@{ lib, pass }:

let

  inherit (builtins)
    getAttr hasAttr foldl'
  ;

  inherit (lib)
    optional concatLists attrVals mapAttrs getAttrs
  ;

  inherit (lib.generators)
    toPretty
  ;

  inherit (import ./keys.nix args)
    hasProperKeyOptionsStructure tagKeyWithIdHash normalizeInputKey
    extractKeyRcRule coerceToDeploymentKeys
  ;

  inherit (import ./systemd-tmpfiles-rules.nix { inherit lib; })
    isSystemdTmpfilesRule resolveSystemdTmpfilesRules
  ;

  hydrateKeysAndAdhocPersistenceInput = users:
    let f = n:
      let user = getAttr n users;
      in {
        inherit (user) home group;
        user = n;
      }
    ;

    in mapAttrs (n: g: g (f n))
  ;


  massageKeysAndAdhocPersistenceInput =
    let
      nul = { keys = []; persistence = []; };
      f = {keys, persistence}: x:
        let r0 = hasProperKeyOptionsStructure x;
            r1 = r0 && hasAttr "rCTarget" x;
            r2 = isSystemdTmpfilesRule x;

            y = if r0 then tagKeyWithIdHash (normalizeInputKey x) else x;
            z = optional (r0) y;
            w =
              optional (r0 && r1) (extractKeyRcRule y) ++ optional r2 x;

        in
          if !r0 && !r2
            then
              throw
                ("\nInvalid `keys and adhoc persistence` input:\n"
                +"${toPretty {} x}")
            else {
              persistence = persistence ++ w;
              keys = keys ++ z;
            }
      ;

    in foldl' f nul
  ;

  resolveKeysAndAdhocPersistence = xs:
    let ys = massageKeysAndAdhocPersistenceInput xs;

    in {
      deploymentKeys = coerceToDeploymentKeys ys.keys;
      systemdTmpfilesRules = resolveSystemdTmpfilesRules ys.persistence;
    }
  ;

  arbitrateKeysAndAdhocPersistence = users: xs: lineup:
    resolveKeysAndAdhocPersistence (
      concatLists (
        attrVals
          lineup
          (hydrateKeysAndAdhocPersistenceInput users (getAttrs lineup xs))
      )
    )
  ;

in {
  inherit
    hydrateKeysAndAdhocPersistenceInput
    massageKeysAndAdhocPersistenceInput
    resolveKeysAndAdhocPersistence
    arbitrateKeysAndAdhocPersistence
  ;
}
