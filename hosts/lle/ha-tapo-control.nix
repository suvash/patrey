{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
  fetchPypi,
  home-assistant,
}:

let
  # The tapo_control manifest pins pytapo==3.4.18. Older nixpkgs
  # revisions carry an earlier pytapo (e.g. 3.4.13), which fails
  # buildHomeAssistantComponent's manifest version check. Rather than
  # requiring a full nixpkgs bump just for this one library, bump
  # pytapo in isolation. pytapo's declared dependencies are identical
  # between 3.4.13 and 3.4.18 (requests, urllib3, pycryptodome, rtp,
  # python-kasa - verified against PyPI metadata for both releases),
  # so inheriting everything but src/version from whatever's already
  # pinned is safe. If a future tapo_control release needs an even
  # newer pytapo and this override starts failing, check
  # https://pypi.org/pypi/pytapo/json for the current dependency list
  # before just bumping the version number below.
  pytapo = home-assistant.python3Packages.pytapo.overridePythonAttrs (_: rec {
    version = "3.4.18";
    src = fetchPypi {
      pname = "pytapo";
      inherit version;
      hash = "sha256-N8s4L8quSWlChU4BSKnLDqY6WboJbcuYLNaFwPEeNnI=";
    };
  });
in
buildHomeAssistantComponent rec {
  owner = "JurajNyiri";
  domain = "tapo_control";
  version = "7.1.26";

  src = fetchFromGitHub {
    owner = "JurajNyiri";
    repo = "HomeAssistant-Tapo-Control";
    tag = version;
    hash = "sha256-f5fQMOnUdLnXRW6FiGwnN3aef05RQOBENKO3K29lqAw=";
  };

  # Must mirror custom_components/tapo_control/manifest.json "requirements".
  # pytapo is the version-pinned override above; python-kasa is pulled
  # straight from home-assistant.python3Packages so we match the exact
  # interpreter/package set Home Assistant itself is built against.
  dependencies = [
    pytapo # requirement: pytapo==3.4.18
    home-assistant.python3Packages.python-kasa # requirement: python-kasa[speedups]==0.10.2
  ];

  meta = {
    description = "Home Assistant custom component to control Tapo cameras (and Kasa/Tapo smart devices) directly, without the cloud";
    homepage = "https://github.com/JurajNyiri/HomeAssistant-Tapo-Control";
    changelog = "https://github.com/JurajNyiri/HomeAssistant-Tapo-Control/releases/tag/${version}";
    license = lib.licenses.mit;
    maintainers = [ ]; # add yourself here if you plan to maintain this locally
  };
}

