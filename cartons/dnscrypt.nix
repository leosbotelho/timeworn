{
  services.dnscrypt-proxy = {
    enable = true;
    localPort = 43;
  };

  services.unbound = {
    enable = true;
    forwardAddresses = [ "127.0.0.1@43" ];
  };

  networking.nameservers = [ "127.0.0.1" ];
}
