autoload -Uz compinit promptinit
compinit
promptinit

export LANG=en_US.UTF-8

alias fere="git fetch && git rebase origin/master"
alias puom="git push origin master"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PLAN9=/Users/tofunth/9/plan9port
export PATH=$PATH:$PLAN9/bin
export ACME=/Users/tofunth/prog/acme
export PATH=$PATH:$ACME/bin
