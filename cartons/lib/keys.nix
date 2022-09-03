{ lib, pass }:

let

  rCBase = "/keys/by-hash";

  inherit (builtins)
    isAttrs hasAttr hashString removeAttrs length attrNames attrValues
    concatMap
  ;

  inherit (lib)
    listToAttrs groupBy head concatStringsSep all partition optionalAttrs
    intersectLists subtractLists
  ;

  inherit (lib.generators)
    toPretty
  ;

  unlines = concatStringsSep "\n";

  nixopsKeyOptions = [
    "text" "keyFile" "destDir" "user" "group" "permissions"
  ];

  exclusiveKeyOptions = [
    "name" "passName" "rCTarget" "idHash"
  ];

  keyAttrs = nixopsKeyOptions ++ exclusiveKeyOptions;

  hardcodedNixopsKeyOptionsDefaults = {
    user = "root";
    group = "root";
    permissions = "0600";
    destDir = "/run/keys";
  };

  boolToInt = b: if b == true then 1 else 0;

  hasOnlyOneKeyContentAttr = x:
    let r0 = hasAttr "text" x;
        r1 = hasAttr "keyFile" x;
        r2 = hasAttr "passName" x;

        f = boolToInt;

    in (f r0 + f r1 + f r2) == 1
  ;

  hasOnlyOneOrNoneKeyPersistenceAttr = x:
    let f = boolToInt;
        r = f (hasAttr "destDir" x) + f (hasAttr "rCTarget" x);

    in r < 2
  ;

  hasProperKeyOptionsStructure = x:
    let ns = attrNames x;

    in     isAttrs x
        && hasOnlyOneKeyContentAttr x
        && hasOnlyOneOrNoneKeyPersistenceAttr x
        && subtractLists (intersectLists ns keyAttrs) ns == []
  ;

  tagKeyWithIdHash = key:
    let idHash = hashString "sha256" (key.name or key.passName);
    in  key // optionalAttrs (hasAttr "rCTarget" key) { inherit idHash; };

  keyDestDir = key:
    if hasAttr "destDir" key
      then key.destDir
      else if hasAttr "rCTarget" key
             then rCBase
             else hardcodedNixopsKeyOptionsDefaults.destDir
  ;

  coerceToDeploymentKey = key: {
    name = if hasAttr "rCTarget" key then key.idHash else key.name;

    value = removeAttrs key exclusiveKeyOptions
      // optionalAttrs (!hasAttr "keyFile" key) {
        text = key.text or (pass key.passName);
      }
    ;
  };

  keyOwnershipAndPermissions =
    with hardcodedNixopsKeyOptionsDefaults;
    key: {
      user = key.user or user;
      group = key.group or group;
      permissions = key.permissions or permissions;
    }
  ;

  partitionUniqueDeploymentKeys = deploymentKeysList:
    let
      collisionIndexesCsv = key: "${key.name},${key.value.destDir}";

      hasNoKeyCollisions = group:
        if length group < 2
          then true
          else
            let f = keyOwnershipAndPermissions;
                f0 = f (head group).value;

            in all (k: k.value.destDir == rCBase && f k.value == f0) group
      ;

    in partition hasNoKeyCollisions
                 (attrValues (groupBy collisionIndexesCsv deploymentKeysList))
  ;

  prettyKeysCollisions = keysCollisions:
    concatStringsSep "\n\n"
                     (map (ys: unlines (map (toPretty {}) ys)) keysCollisions)
  ;

  coerceToDeploymentKeys = keysWithIdHash:
    let
      r = partitionUniqueDeploymentKeys
            (map coerceToDeploymentKey keysWithIdHash)
      ;

    in if r.wrong == []
         then listToAttrs (concatMap (xs: [ (head xs) ]) r.right)
         else abort ''

           Keys collisions:

           ${prettyKeysCollisions r.wrong}
         ''
  ;

  extractKeyRcRule = key: {
    inherit (key)
      rCTarget destDir idHash
    ;
  };

  normalizeInputKey = inputKey:
    let err0 = throw "destDir cannot be ${rCBase} without rCTarget";
        err1 = throw "idHash is an internal attr";
        destDir = keyDestDir inputKey;

    in if hasAttr "idHash" inputKey
         then err1
         else
           if destDir == rCBase && !hasAttr "rCTarget" inputKey
             then err0
             else inputKey // { inherit destDir; }
  ;

in {
  inherit
    rCBase
    nixopsKeyOptions
    exclusiveKeyOptions
    hasProperKeyOptionsStructure
    tagKeyWithIdHash
    keyDestDir
    keyOwnershipAndPermissions
    partitionUniqueDeploymentKeys
    prettyKeysCollisions
    coerceToDeploymentKey
    coerceToDeploymentKeys
    extractKeyRcRule
    normalizeInputKey
  ;
}
