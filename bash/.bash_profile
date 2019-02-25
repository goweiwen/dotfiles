# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]]; then
	. ~/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.lean/bin:$PATH"

alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'


bind "set completion-ignore-case on"
bind "set completion-map-case on"
bind "set mark-symlinked-directories on"
# bind "set show-all-if-ambiguous on"
set -o noclobber
shopt -s autocd 2> /dev/null
shopt -s checkwinsize 2> /dev/null
shopt -s cmdhist 2> /dev/null
shopt -s globstar 2> /dev/null
shopt -s histappend 2> /dev/null
shopt -s nocaseglob 2> /dev/null

export HISTSIZE=500000
export HISTFILESIZE=100000
export HISTCONTROL="erasedups:ignoreboth"
export HISTIGNORE="ls:exit:bg:fg:history:clear"
export HISTTIMEFORMAT='%F %T '

export PROMPT_COMMAND='history -a'

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'
bind Space:magic-space

export EDITOR=nvim
export BROWSER=google-chrome-stable
export PAGER=less

export PS1="[\e[34m\u\e[39m@\e[35m\h\e[39m] \e[36m\W\e[39m \$ "

if [ -f "/usr/share/doc/mcfly/mcfly.bash" ]; then
  . /usr/share/doc/mcfly/mcfly.bash
fi

if [ -f "/usr/share/z/z.sh" ]; then
  . /usr/share/z/z.sh
fi

if [ -d "$HOME/.anaconda3/bin" ]; then
  export PATH="$PATH:$HOME/.anaconda3/bin"
fi
