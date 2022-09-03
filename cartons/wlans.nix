let

  inherit (builtins.extraBuiltins) pass;

in {
  wlan0 = {
    "${pass "wlan/0/ssid"}" = {
      pskRaw = pass "wlan/0/psk";
    };
  };
}
