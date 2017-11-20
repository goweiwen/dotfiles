# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;28\a\033]6;1;bg;green;brightness;32\a\033]6;1;bg;blue;brightness;34\a'

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

# Source autoenv
source "$HOME/.zsh-autoenv/autoenv.zsh"

# Source fasd
fasd_cache="$HOME/.fasd-init"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init auto >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i'     # cd with interactive selection

# Setup virtualenv
source /usr/local/anaconda3/bin/virtualenvwrapper.sh

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
alias emacs='/usr/local/Cellar/emacs-mac/emacs-25.2-z-mac-6.5/bin/emacs'
alias emacsclient='/usr/local/Cellar/emacs-mac/emacs-25.2-z-mac-6.5/bin/emacsclient'

alias jn='jupyter notebook'
alias v='nocorrect nvim'
alias vf='f -e nvim'
alias o='open'

alias ll='ls -l'
alias la='ls -la'

alias bi='brew install'
alias br='brew remove'
alias bs='brew search'
alias bl='brew list'
alias bci='brew cask install'
alias bcr='brew cask remove'
alias bcs='brew cask search'
alias bcl='brew cask list'

alias vrc='vim ~/.config/nvim/init.vim'
alias zrc='vim ~/.zshrc'
alias hrc='vim ~/.hammerspoon/init.lua'
alias crc='vim ~/.chunkwmrc'

# added by travis gem
[ -f /Users/weiwen/.travis/travis.sh ] && source /Users/weiwen/.travis/travis.sh

export PATH="$HOME/.yarn/bin:$PATH"
