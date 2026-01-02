{pkgs, lib, config, ...}: {
  programs.yubikey-manager.enable = true;
  programs.i3lock = {
    enable = true;
    u2fSupport = true;
  };

  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  security.pam = {
    u2f = {
      enable = true;
      settings = {
        cue = true;
        cue_prompt = "🫆 your yubikey...";
        interactive = false;
        origin = "pam://yubikey";

        # generated with pamu2fcfg -n -o pam://yubikey
        authfile = pkgs.writeText "u2f-mappings" (
          lib.concatStrings [
            "${config.settings.username}"
            ":jUXBROBcZSVAZFY3HlaV7rATBtJh5NHDbCbTXoo34OCTH+Xwoe151V8aD+QLqn1BCbHOWQ+K0AmNCQte9rbGJw==,v8ZsU+Wh//vPLattzh9oJVRujhWFgtqCi4IzUxHGaF6//b5ZHfpwuzoTZax2+jFvA39L5UJNJyzGCHY8IPQWwg==,es256,+presence" # yk 4
            ":lpdS0rjLAhJLHni9oo+sKfCLXyr5Mm38O6jnJDDAPpsnFYNqZDXZ2JyYR3L3SMvfN8FWypx10j4gzvJpIB2XfQ==,Dij2i9ksdooVFMbL/EA9IuILkHzzd5xsG+ayv9h8gF0p+XTFU4+TVRXEGbc/A9mHKsecxgACoFeR4I7OE8carw==,es256,+presence" # yk 5c
            ":vyat/VKLd4PDeMMYEezh7cAMLYC5dpYLxDIrIkCxyG9DnQcnH3ZJNpcs4o9k7xLFFx6QjR4QYVU8CwBbwHtE8A==,yqJXxFyWdYsxESACMiPCckOFLss/Z0UKTGSaJim3J1/OGTP/P1k/yT209mdn+T7mV3oaKDFLA1/KsKrpfvTxdQ==,es256,+presence" # yk 5c nfc
          ]
        );
      };
    };
    services = {
      sudo.u2fAuth = true;
      login.u2fAuth = false;
      i3lock.u2fAuth = true;
    };
  };

  environment.systemPackages = with pkgs; [
    pam_u2f
    pamtester
  ];
}
