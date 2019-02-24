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

HISTSIZE=500000
HISTFILESIZE=100000
HISTCONTROL="erasedups:ignoreboth"
HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
HISTTIMEFORMAT='%F %T '

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[C": forward-char'
bind '"\e[D": backward-char'
bind Space:magic-space

EDITOR=nvim
BROWSER=qutebrowser

export PS1="\W \$ "
