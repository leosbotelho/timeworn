{ lib }:

let

  inherit (import ./match.nix { inherit lib; })
    matchByPreds
  ;

  inherit (builtins)
    length attrNames hasAttr
  ;

  inherit (lib)
    concatStringsSep take drop range init tail splitString optionals
    intersectLists subtractLists concatMap
  ;

  inherit (lib.generators)
    toPretty
  ;

  paths' =
    xs:
      map (n: concatStringsSep "/" (take n xs)) (range 1 (length xs));

  paths = path:
    tail (paths' (splitString "/" path));

  possibleAttrs = ns: x:
    let xns = attrNames x;
    in  subtractLists (intersectLists xns ns) xns == []
  ;

  isRcRule = x:
       (hasAttr "rCTarget" x && hasAttr "idHash" x && hasAttr "destDir" x)
    && possibleAttrs [ "idHash" "rCTarget" "destDir" ] x
  ;

  resolveRcRule = { rCTarget, destDir, idHash }: [
    "r ${rCTarget} - - - - -"
    "C ${rCTarget} - - - - ${destDir}/${idHash}"
  ];

  mkdirRuleAttrs = [
    "mkdir" "user" "group" "permissions"
  ];

  isMkdirRule = x:
    hasAttr "mkdir" x && possibleAttrs mkdirRuleAttrs x
  ;

  resolveMkdirRule = {
    mkdir, permissions ? "0700", user ? "-", group ? "-"
  }: [
    "d ${mkdir} ${permissions} ${user} ${group} - -"
  ];

  isBackupAclsRule = x:
       possibleAttrs [ "backupAclsFoot" "backupAclsHead" ] x
    && (hasAttr "backupAclsFoot" x || hasAttr "backupAclsHead" x)
  ;

  resolveBackupAclsRule = { backupAclsFoot ? "", backupAclsHead ? "" }:
    let

      hasAclsFoot = backupAclsFoot != "";
      hasAclsHead = backupAclsHead != "";

      trail =
        if hasAclsFoot && hasAclsHead
          then
            init (
              drop (length (paths backupAclsFoot) - 1) (paths backupAclsHead))
          else
            optionals hasAclsFoot [ backupAclsFoot ]
      ;

      trailAcls = map (p: "a+ ${p} - - - - u:backup:--x,m::rwx") trail;

      headAcls =
        optionals hasAclsHead
          [
            "A+ ${backupAclsHead} - - - - u:backup:r-x,m::rwx"
          ]
      ;

    in trailAcls ++ headAcls
  ;

  isSystemdTmpfilesRule = x:
    isRcRule x || isMkdirRule x || isBackupAclsRule x
  ;

  resolveSystemdTmpfilesRule =
    matchByPreds [
      [ isRcRule resolveRcRule ]
      [ isMkdirRule resolveMkdirRule ]
      [ isBackupAclsRule resolveBackupAclsRule ]
    ]
  ;

  resolveSystemdTmpfilesRules = concatMap resolveSystemdTmpfilesRule;

in {
  inherit
    paths
    isRcRule
    resolveRcRule
    isMkdirRule
    resolveMkdirRule
    isBackupAclsRule
    resolveBackupAclsRule
    isSystemdTmpfilesRule
    resolveSystemdTmpfilesRule
    resolveSystemdTmpfilesRules
  ;
}
