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

# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;43\a\033]6;1;bg;green;brightness;48\a\033]6;1;bg;blue;brightness;59\a'

# Others
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt histignorealldups sharehistory appendhistory autocd extendedglob correct_all
bindkey -e

# Aliases
alias vim='nocorrect nvim '
alias please='sudo `fc -ln -1`'

alias v='vim '
alias o='open '

alias vrc='vim ~/.config/nvim/init.vim'
alias zrc='vim ~/.zshrc'
alias hrc='vim ~/.hammerspoon/init.lua'
