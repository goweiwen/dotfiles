# Git
alias g='git'

# Add
alias gia='git add'
alias giap='git add --patch'

# Reset
alias gir='git reset'
alias girp='git reset --patch'
alias gic='git clean'

# Branch
alias gb='git branch'
alias gbc='git checkout -b'
alias gbm='git branch --move'
alias gbd='git branch -d'
alias gbD='git branch -D'

# Commit
alias gc='git commit --verbose'
alias gca='git commit --verbose --amend --reuse-message HEAD'

# Checkout
alias gco='git checkout'

# Cherry Pick
alias gcp='git cherry-pick --ff'

# Fetch
alias gf='git fetch'
alias gfc='git clone'
alias gfm='git pull'
alias gfr='git pull --rebase'

# Grep
alias gg='git grep'
alias ggi='git grep --ignore-case'
alias ggl='git grep --files-with-matches'
alias ggL='git grep --files-without-match'
alias ggv='git grep --invert-match'
alias ggw='git grep --word-regexp'

# Log
alias gl='git log --topo-order --all --graph --oneline'
alias glo='git log --topo-order --oneline'
alias grl='git reflog'

# Merge
alias gm='git merge'

# Push
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpa='git push --all'
alias gpt='git push --tags'
alias gpc='git push --set-upstream origin "$(git-branch-current 2> /dev/null)"'

# Rebase
alias gr='git rebase'
alias gra='git rebase --abort'
alias gri='git rebase --interactive --autosquash'
alias grc='git rebase --continue'
alias grs='git rebase --skip'

# Remote
alias gR='git remote'
alias gRl='git remote --verbose'
alias gRa='git remote add'
alias gRd='git remote rm'
alias gRm='git remote rename'

# Stash
alias gs='git stash'
alias gsa='git stash apply'
alias gsd='git stash drop'
alias gsl='git stash list'
alias gss='git stash save'

# Submodule
alias gS='git submodule'
alias gSa='git submodule add'
alias gSi='git submodule init'
alias gSI='git submodule update --init --recursive'
alias gSu='git submodule foreach git pull origin master'

# Tag
alias gt='git tag'
#
# Status
alias gws='git status --short'
alias gwss='git status'
alias gwd='git diff'
alias gwdw='git diff --color-words'

# Blame
alias gwb='git blame'
