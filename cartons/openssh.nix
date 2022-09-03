# This is very sensitive

{ lib, config, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    permitRootLogin = "yes";
    passwordAuthentication = true;
    challengeResponseAuthentication = true;
    listenAddresses = [
      { addr = "127.0.0.1"; port = 22; }
    ];
  };

  security.pam.services.sshd.text = lib.mkOverride 0 ''
    # Account management.
    account required pam_unix.so

    # Authentication management.
    auth required ${pkgs.googleAuthenticator}/lib/security/pam_google_authenticator.so nullok no_increment_hotp noskewadj
    auth required pam_permit.so

    # Password management.
    password sufficient pam_unix.so nullok sha512

    # Session management.
    session required pam_env.so envfile=${config.system.build.pamEnvironment}

    session required pam_unix.so
    session required pam_loginuid.so

    session optional ${pkgs.systemd}/lib/security/pam_systemd.so
  '';

  services.openssh.extraConfig = ''
    AuthenticationMethods publickey,keyboard-interactive:pam

    Match User root
      AuthenticationMethods publickey
  '';

  environment.systemPackages = with pkgs; [
    googleAuthenticator
  ];

  programs.ssh = {
    hostKeyAlgorithms = [ "ssh-ed25519" "ssh-rsa" ];
    pubkeyAcceptedKeyTypes = [ "ssh-ed25519" "ssh-rsa" ];
  };
}
