{
  config,
  lib,
  ...
}: let
  inherit
    (lib)
    filterAttrs
    mapAttrs'
    mkIf
    mkOption
    nameValuePair
    optional
    optionals
    types
    ;

  cfg = config.services.nfsMounts;
  enabled = filterAttrs (_: m: m.enable) cfg;

  mkOptions = m:
    ["_netdev" "nofail" "x-systemd.mount-timeout=${toString m.mountTimeout}s"]
    ++ optional m.readOnly "ro"
    ++ optional (m.nfsVersion != null) "nfsvers=${m.nfsVersion}"
    ++ optionals m.automount (
      ["noauto" "x-systemd.automount"]
      ++ optional (m.idleTimeout > 0) "x-systemd.idle-timeout=${toString m.idleTimeout}"
    )
    ++ m.extraOptions;
in {
  options.services.nfsMounts = mkOption {
    default = {};
    description = ''
      NFS shares to mount, keyed by an arbitrary name. Each entry becomes a
      `fileSystems` entry, with lazy automounting and boot-safe defaults.
    '';
    example = {
      media = {
        server = "192.168.1.10";
        export = "/export/media";
        nfsVersion = "4.2";
      };
    };
    type = types.attrsOf (types.submodule ({name, ...}: {
      options = {
        enable = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to mount this share.";
        };

        server = mkOption {
          type = types.str;
          example = "192.168.1.10";
          description = "NFS server hostname or IP address.";
        };

        export = mkOption {
          type = types.str;
          example = "/export/media";
          description = "Exported path on the server.";
        };

        mountPoint = mkOption {
          type = types.path;
          default = "/mnt/nfs/${name}";
          description = "Local directory to mount the share on. Created automatically.";
        };

        readOnly = mkOption {
          type = types.bool;
          default = true;
          description = "Mount read-only.";
        };

        nfsVersion = mkOption {
          type = types.nullOr (types.enum ["3" "4" "4.0" "4.1" "4.2"]);
          default = null;
          description = "Pin the NFS version. Null lets client and server negotiate.";
        };

        automount = mkOption {
          type = types.bool;
          default = true;
          description = "Mount lazily on first access instead of at boot.";
        };

        idleTimeout = mkOption {
          type = types.ints.unsigned;
          default = 60;
          description = "Seconds of inactivity before unmounting (automount only). 0 disables.";
        };

        mountTimeout = mkOption {
          type = types.ints.positive;
          default = 10;
          description = "Seconds to wait for the mount before giving up.";
        };

        extraOptions = mkOption {
          type = types.listOf types.str;
          default = [];
          example = ["soft" "timeo=50"];
          description = "Additional mount options.";
        };
      };
    }));
  };

  config = mkIf (enabled != {}) {
    boot.supportedFilesystems = ["nfs"];

    fileSystems =
      mapAttrs'
      (_: m:
        nameValuePair m.mountPoint {
          device = "${m.server}:${m.export}";
          fsType = "nfs";
          options = mkOptions m;
        })
      enabled;
  };
}
