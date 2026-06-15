#!/usr/bin/env bash

# ----- ENVIRONMENT -----
## default editor
export EDITOR=nvim
export VISUAL=$EDITOR
export PSQL_EDITOR="vim -u NONE"
export KUBE_EDITOR=$EDITOR

## appearance
export LSCOLORS="ExGxbEaECxxEhEhBaDaCaD"

### support colors in less
export LESS_TERMCAP_mb=$(tput bold; tput setaf 93)
export LESS_TERMCAP_md=$(tput bold; tput setaf 93)
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_se=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 11; tput setab 27)
export LESS_TERMCAP_ue=$(tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 6)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

## bash history
HISTSIZE=25000
HISTFILESIZE=10000
HISTCONTROL=ignoredups:erasedups

## nvm
export NVM_DIR=~/.nvm

## aws-vault config
export AWS_VAULT_BACKEND=file
export AWS_ASSUME_ROLE_TTL=4h
export AWS_SESSION_TTL=4h

## make fzf use ripgrep
if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden'
  export FZF_DEFAULT_OPTS='-m --color="dark,fg:6"'
  export FZF_COMPLETION_DIR_COMMANDS="cd pushd rmdir tree"
fi
# ----- ENVIRONMENT -----

# ----- PATH -----
## reset path
PATH="/usr/local/bin:$(getconf PATH)"

## add homebrew to path
eval "$(/opt/homebrew/bin/brew shellenv)"

## stuff to add to path
RVM_HOME=$HOME/.rvm/bin
BREW_PYTHON_HOME=$(brew --prefix python@3)/bin
GO_BIN="$(go env GOPATH)/bin"

## set path
PATH="$(brew --prefix)/Cellar:/usr/local/sbin:$HOME/.local/bin:$BREW_PYTHON_HOME:$GO_BIN:$RVM_HOME:$PATH"
# ----- PATH -----

# ----- SHELL OPTIONS -----
# ----- SHELL OPTIONS -----

# ----- ALIASES -----
# ----- ALIASES -----

# ----- PROMPT -----

## Place current directory in bash title/tab
PROMPT_COMMAND='echo -ne "\033]0; ${PWD##*/}\007"'

## share history between shell sessions
PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# ----- PROMPT -----

# ----- FUNCTIONS -----
# ----- FUNCTIONS -----

# Load all the shell dotfiles
for file in ~/.{bash_prompt,aliases,functions,work,secrets}; do
  [ -r "$file" ] && . "$file"
done;
unset file;

# Enable Homebrew bash completion
[[ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]] && . "$(brew --prefix)/etc/profile.d/bash_completion.sh"

# For bash completion
# . ~/.bin/.tmuxinator.bash;

# Append to bash history instead of overwriting
shopt -s histappend;

# Autocorrect typos in paths on cd command
shopt -s cdspell;

# Case-insensitive globbing
shopt -s nocaseglob;

# Need this so SSH passphrase isn't constantly asked for in tmux sessions
ssh-add -A 2>/dev/null;

# For nvm installed via homebrew
# . $(brew --prefix nvm)/nvm.sh;

command -v fzf >/dev/null 2>&1 && eval "$(fzf --bash)"
[ -n "$BASH" ] && complete -F _fzf_complete_git -o default -o bashdefault git

# For nvm installed via creationix install.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# ----- EVERYTHING BELOW THIS LINE WAS AUTOMATICALLY ADDED AND SHOULD BE MOVED TO .bashrc.local -----
