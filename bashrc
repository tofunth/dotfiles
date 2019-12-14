# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Source bash aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

DIR="$HOME/.local"
export DIR

# User specific environment
if ! [[ "$PATH" =~ *"$DIR/bin"* ]]
then
    PATH="$DIR/bin:$PATH"
fi
export PATH

if ! [[ "LD_LIBRARY_PATH" =~ *"$DIR/lib"* ]]
then
    LD_LIBRARY_PATH="$DIR/lib:$LD_LIBRARY_PATH"
fi
export LD_LIBRARY_PATH

if ! [[ "C_INCLUDE_PATH" =~ *"$DIR/include"* ]]
then
    C_INCLUDE_PATH="$DIR/include:$C_INCLUDE_PATH"
fi
export C_INCLUDE_PATH

if ! [[ "C_PLUS_INCLUDE_PATH" =~ *"$DIR/include"* ]]
then
    C_PLUS_INCLUDE_PATH="$DIR/include:$C_PLUS_INCLUDE_PATH"
fi
export C_PLUS_INCLUDE_PATH

# Set up bash history

# history format
export HISTTIMEFORMAT="%h %d %H:%M:%S "
export HISTSIZE=100000
export HISTFILESIZE=100000

# command appending to history file
shopt -s histappend

# every commands to history file
PROMPT_COMMAND='history -a'

# ignore command starting with space and duplicating command to its previous
export HISTCONTROL=ignorespace:ignoredups

# list of commands to ignore
export HISTIGNORE="ls:ps:history:htop:top"

# store multiple command into one entry
shopt -s cmdhist

# get current branch in git repo
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo ""
    fi
}

# get current branch in git repo
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo ""
    fi
}

# get current branch in git repo
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo ""
    fi
}

# get current status of git repo
function parse_git_dirty {
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${renamed}" == "0" ]; then
        bits=">${bits}"
    fi
    if [ "${ahead}" == "0" ]; then
        bits="*${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="+${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
        bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
        bits="x${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
        bits="!${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
        echo " ${bits}"
    else
        echo ""
    fi
}

export PS1="\[\e[32m\]\u\[\e[m\]\[\e[32m\]@\[\e[m\]\[\e[32m\]\h\[\e[m\]:\[\e[34m\]\w\[\e[m\]\[\e[35m\]\`parse_git_branch\`\[\e[m\]\\$ "

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
