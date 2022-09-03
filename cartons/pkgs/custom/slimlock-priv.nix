{ pkgs, uid, gid, env, slimlockPath }:

let

  inherit (builtins)
    concatStringsSep
  ;

  envp = concatStringsSep "," (map (s: ''"${s}"'') env);

in pkgs.writeCBin "slimlock-priv" ''
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <grp.h>

int main(int argc, char **argv) {
  unsigned uid = ${toString uid};
  unsigned gid = ${toString gid};

  int status = 0;

  gid_t groups_list[] = { gid };

  char slimlock_path[] = "${toString slimlockPath}";
  char *slimlock_args[] = { slimlock_path, 0 };
  char *_envp[] = {
    ${envp}, 0
  };

  if(argc != 1) {
    fprintf(stderr, "Invalid arguments\n");
    return 1;
  }

  status = setgid(gid);
  if(status < 0) {
    fprintf(stderr, "Failed to set gid=%u: %s\n", gid, strerror(errno));
    return 1;
  }

  status = setgroups(1, groups_list);
  if(status != 0) {
    fprintf(stderr, "Failed to setgroups\n");
    return 1;
  }

  status = setuid(uid);
  if(status < 0) {
    fprintf(stderr, "Failed to set uid=%u: %s\n", uid, strerror(errno));
    return 1;
  }

  execve(slimlock_path, slimlock_args, _envp);

  return 0;
}
''
