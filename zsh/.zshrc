# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;28\a\033]6;1;bg;green;brightness;32\a\033]6;1;bg;blue;brightness;34\a'

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

# Source fasd
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache
alias z='fasd_cd -d'     # cd, same functionality as j in autojump

# base16-shell
# BASE16_SHELL=$HOME/.config/base16-shell/
# [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# Others
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt histignorealldups sharehistory appendhistory autocd extendedglob correct_all
bindkey -e

# Aliases
alias vim='nocorrect nvim '
alias please='sudo `fc -ln -1`'

alias v='nocorrect nvim '
alias vf='f -e nvim '
alias o='open '

alias ll='ls -l'
alias la='ls -la'

alias bi='brew install '
alias br='brew remove '
alias bs='brew search '
alias bci='brew cask install '
alias bcr='brew cask remove '
alias bcs='brew cask search '

alias vrc='vim ~/.config/nvim/init.vim'
alias zrc='vim ~/.zshrc'
# alias hrc='vim ~/.hammerspoon/init.lua'
alias krc='vim ~/.khdrc'
alias crc='vim ~/.chunkwmrc'
alias hrc='vim ~/.hyper.js'
