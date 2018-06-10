{
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; {

    shell-env = pkgs.buildEnv {
      name = "shell-env";
      paths = [
        ffmpeg
        ghostscript
        gitAndTools.gitFull
        gitAndTools.hub
        gitAndTools.tig
        graphviz
        imagemagick
        jq
        neovim
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

  };
}
