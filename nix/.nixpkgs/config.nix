{
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; {

    shell-env = pkgs.buildEnv {
      name = "shell-env";
      paths = [
        alacritty
        ffmpeg
        ghostscript
        gitAndTools.gitFull
        gitAndTools.hub
        gitAndTools.tig
        graphviz
        imagemagick
        jq
        pandoc
        silver-searcher
        sloccount
        stow
        tmux
        tree
        wget
        xz
        youtube-dl
        z3
        zsh
      ];
    };

    neovim-env = pkgs.buildEnv {
      name = "neovim-env";
      paths = [
        neovim
        python35Packages.neovim
        python27Packages.neovim
      ];
    };

    python-env = pkgs.buildEnv {
      name = "python-env";
      paths = [
        python35
        python27
      ];
    };

    javascript-env = pkgs.buildEnv {
      name = "javascript-env";
      paths = [
        yarn
        nodejs
      ];
    };

  };
}
