{ config, ... }:
{
  services.gitea = {
    enable = true;
    user = "git";
    group = "git";
    settings = {
      server = {
        ROOT_URL = "https://git.suva.sh/";
        HTTP_ADDR = "127.0.0.1";
        HTTP_PORT = 3000;
        SSH_DOMAIN = "src.suva.sh";
      };
      service = {
        DISABLE_REGISTRATION = true;
      };
      session.COOKIE_SECURE = true;
    };
  };

  users.users.git = {
    isSystemUser = true;
    useDefaultShell = true;
    group = "git";
    home = config.services.gitea.stateDir;
  };
  users.groups.git = { };

  services.cloudflared = {
    tunnels."lle-suva-sh".ingress = {
      "git.suva.sh" = "http://localhost:${toString 3000}";
    };
  };
}
