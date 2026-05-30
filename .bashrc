#!/usr/bin/env bash

# Ignore all of this if bash is running non-interactively (like when a script is run)
[[ -n $PS1 ]] || return

# ----- formerly .exports -----
# default editor
export EDITOR=nvim;
export PSQL_EDITOR="vim -u NONE";
export FCEDIT=$(brew --prefix)/bin/vim;
export KUBE_EDITOR=$EDITOR;

# nvm
export NVM_DIR=~/.nvm;

# CLI colors
export CLICOLOR=1;

# Colored grep output
export GREP_OPTIONS="--color=auto";

# Place current directory in bash title/tab
export PROMPT_COMMAND='echo -ne "\033]0; ${PWD##*/}\007"';

#share bash history across sessions/tabs
export HISTSIZE=25000;
export HISTFILESIZE=10000;
export HISTCONTROL=ignoredups:erasedups;
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND";

# aws-vault config
export AWS_VAULT_BACKEND=file;
export AWS_ASSUME_ROLE_TTL=4h
export AWS_SESSION_TTL=4h

# make fzf use ripgrep
if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files --hidden'
  export FZF_DEFAULT_OPTS='-m --color="dark,fg:6"'
  export FZF_COMPLETION_DIR_COMMANDS="cd pushd rmdir tree"
fi
# ----- formerly .exports -----

# Load all the shell dotfiles
for file in ~/.{path,bash_prompt,aliases,functions,work,secrets}; do
  [ -r "$file" ] && . "$file"
done;
unset file;

# Enable Homebrew bash completion
[[ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]] && . "$(brew --prefix)/etc/profile.d/bash_completion.sh"

# Append to bash history instead of overwriting
shopt -s histappend;

# Autocorrect typos in paths on cd command
shopt -s cdspell;

# Case-insensitive globbing
shopt -s nocaseglob;

# Need this so SSH passphrase isn't constantly asked for in tmux sessions
ssh-add -A 2>/dev/null;

command -v fzf >/dev/null 2>&1 && eval "$(fzf --bash)"
[ -n "$BASH" ] && complete -F _fzf_complete_git -o default -o bashdefault git

# For nvm installed via creationix install.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
