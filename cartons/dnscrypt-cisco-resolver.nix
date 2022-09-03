{
  imports = [ ./dnscrypt.nix ];

  # Logs and no dnssec; still better than my ISP's
  services.dnscrypt-proxy.resolverName = "cisco";

  # Due to resolver constraints
  services.unbound.enableRootTrustAnchor = false;
}
