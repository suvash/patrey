{inputs}: final: _prev: let
  inherit (final) lib;
  flakeLock = builtins.fromJSON (builtins.readFile ../flake.lock);
  llamaCppInputName =
    flakeLock.nodes.root.inputs."llama-cpp"
    or (throw "flake.lock is missing the llama-cpp input; run `nix flake update llama-cpp`");
  llamaCppLock =
    flakeLock.nodes.${llamaCppInputName}
    or (throw "flake.lock is missing node ${llamaCppInputName}; run `nix flake update llama-cpp`");
  llamaCppRef = llamaCppLock.original.ref or (throw "llama-cpp input must point at a bNNNN tag");
  llamaCppLockedRev = llamaCppLock.locked.rev or (throw "llama-cpp input must be locked to a git revision");
  llamaCppInputRev = inputs.llama-cpp.rev or (throw "llama-cpp input must expose a git revision");
  llamaCppImportNpmLock = final.master.importNpmLock;
  llamaCppUiRoot = "ui/apps/chat";
  llamaCppVersion =
    if llamaCppLockedRev != llamaCppInputRev
    then throw "llama-cpp flake.lock rev ${llamaCppLockedRev} does not match input rev ${llamaCppInputRev}; run `nix flake update llama-cpp`"
    else if builtins.match "b[0-9]+" llamaCppRef != null
    then lib.removePrefix "b" llamaCppRef
    else throw "llama-cpp input must point at a bNNNN tag, got ${llamaCppRef}";
  llamaCppCommit =
    if inputs.llama-cpp ? shortRev
    then inputs.llama-cpp.shortRev
    else builtins.substring 0 7 llamaCppInputRev;
  llamaCppNodeModules = llamaCppImportNpmLock.buildNodeModules {
    npmRoot = "${inputs.llama-cpp}/${llamaCppUiRoot}";
    nodejs = final.master.nodejs;
  };
in {
  llama-cpp-from-input = args:
    (final.master.llama-cpp.override args).overrideAttrs (oldAttrs: {
      version = llamaCppVersion;
      src = inputs.llama-cpp;
      npmRoot = llamaCppUiRoot;
      npmDepsHash = null;
      npmDeps = llamaCppNodeModules;
      dontLinkNodeModules = true;
      nativeBuildInputs =
        builtins.filter
        (input: (input.name or "") != "npm-config-hook")
        oldAttrs.nativeBuildInputs
        ++ [llamaCppImportNpmLock.hooks.linkNodeModulesHook];
      # Both PWA generators copy the favicons. Copies from the Nix store retain
      # their read-only mode, so allow the second generator to overwrite them.
      postPatch = (oldAttrs.postPatch or "") + ''
        substituteInPlace ${llamaCppUiRoot}/scripts/brand-favicons.ts \
          --replace-fail "{ copyFileSync, mkdirSync }" "{ chmodSync, copyFileSync, mkdirSync }" \
          --replace-fail \
            'copyFileSync(require.resolve(specifier), resolve(outDir, fileName));' \
            'copyFileSync(require.resolve(specifier), resolve(outDir, fileName)); chmodSync(resolve(outDir, fileName), 0o644);'
      '';
      # Build in the full source tree: Chat compiles ui/lib/ui and ui/lib/voice
      # directly, using the dependencies installed for the app.
      preConfigure = ''
        linkNodeModulesHook
        pushd ${llamaCppUiRoot}
        LLAMA_BUILD_NUMBER=b${llamaCppVersion} npm run build
        test -s dist/index.html
        popd
      '';
      # The nixpkgs recipe pins these independently of version/src.
      cmakeFlags =
        builtins.filter
        (flag: !(lib.hasPrefix "-DLLAMA_BUILD_NUMBER" flag || lib.hasPrefix "-DLLAMA_BUILD_COMMIT" flag))
        oldAttrs.cmakeFlags
        ++ [
          (lib.cmakeFeature "LLAMA_BUILD_NUMBER" llamaCppVersion)
          (lib.cmakeFeature "LLAMA_BUILD_COMMIT" llamaCppCommit)
          (lib.cmakeBool "LLAMA_BUILD_UI" false)
          (lib.cmakeBool "LLAMA_USE_PREBUILT_UI" false)
        ];
    });
}
