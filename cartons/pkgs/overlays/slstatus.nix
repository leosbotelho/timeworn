self: super:

let

  conf = builtins.readFile ../config/slstatus/config-slstatus.h;

in {
  slstatus = super.slstatus.override {
    inherit conf;
  };
}
