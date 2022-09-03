{ lib, ... }:

{
  security.pam.services.su.text =
    lib.mkDefault (lib.mkBefore ''
      auth sufficient pam_rootok.so
      auth requisite  pam_succeed_if.so uid > 0
    '');
}
