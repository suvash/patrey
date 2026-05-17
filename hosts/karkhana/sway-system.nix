{pkgs, ...}: {
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # Fixes most GTK3 app theming issues
  };

  # Required for screen sharing, file pickers, and other portal-based features.
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-gtk];
    config.common.default = "*";
  };

  # enable D-bus
  services.dbus.enable = true;

  # symlink intel dri card to /dev/dri/intel-card
  services.udev.extraRules = ''
    SUBSYSTEM=="drm", KERNEL=="card*", DRIVERS=="i915", SYMLINK+="dri/intel-card"
  '';

  # Required for Home Manager's Sway module and for graphical privilege prompts.
  security.polkit.enable = true;

  # Without this, swaylock will NOT accept your password.
  # But should be enabled above already
  # security.pam.services.swaylock = {};

  # Login manager: greetd + tuigreet
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --time --asterisks --cmd sway";
        user = "greeter";
      };
    };
  };
  # Expose sway as a selectable session in tuigreet
  environment.etc."greetd/environments".text = "sway\n";
  # tuigreet needs its own user
  users.users.greeter = {};

  # Must be a system service so that network daemons can reach it over D-Bus.
  services.gnome.gnome-keyring.enable = true;

  environment.sessionVariables = {
    # Tell wlroots (and thus sway) to only use the Intel card
    WLR_DRM_DEVICES = "/dev/dri/intel-card";
    # Tells Electron/Chromium apps to use Wayland natively.
    NIXOS_OZONE_WL = "1";
    # Force SDL2 apps to use Wayland
    SDL_VIDEODRIVER = "wayland";
    # Force Qt5 apps to use Wayland (comment out if it causes issues)
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
  };

  # bare minimum needed at the system level.
  # Everything else is managed via Home Manager.
  environment.systemPackages = with pkgs; [
    grim # screenshot capture
    slurp # interactive region selection (pairs with grim)
    wl-clipboard # wl-copy / wl-paste — clipboard for Wayland
    mako # notification daemon
    brightnessctl # screen brightness control (laptops)
  ];

  # Modern audio stack; provides PulseAudio and JACK compatibility layers.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # fonts
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-color-emoji
    font-awesome
    ubuntu-classic
  ];

  # overload keys
  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = ["*"]; # apply to all keyboards
      settings = {
        main = {
          capslock = "overload(control, esc)";
        };
      };
    };
  };
}
