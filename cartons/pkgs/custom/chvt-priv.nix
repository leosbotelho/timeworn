{ pkgs, uid, gid, chvtPath }:

pkgs.writeCBin "chvt-priv" ''
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

  char *tty_arg = argv[1];

  char chvt_path[] = "${toString chvtPath}";
  char *chvt_args[] = { chvt_path, argv[1], 0 };
  char *_envp[] = {0};

  if(argc != 2) {
    fprintf(stderr, "Invalid arguments\n");
    return 1;
  }

  if(strlen(tty_arg) > 1 || (*tty_arg < '0' || *tty_arg > '9')) {
    fprintf(stderr, "Invalid tty\n");
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

  execve(chvt_path, chvt_args, _envp);

  return 0;
}
''
