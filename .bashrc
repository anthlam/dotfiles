#!/usr/bin/env bash

# Ignore all of this if bash is running non-interactively (like when a script is run)
[[ -n $PS1 ]] || return

# ----- ENVIRONMENT -----
## default editor
export EDITOR=nvim;
export VISUAL=$EDITOR;
export PSQL_EDITOR="vim -u NONE";
export KUBE_EDITOR=$EDITOR;

## appearance
export LSCOLORS="ExGxbEaECxxEhEhBaDaCaD";
### Support colors in less
export LESS_TERMCAP_mb=$(tput bold; tput setaf 93);
export LESS_TERMCAP_md=$(tput bold; tput setaf 93);
export LESS_TERMCAP_me=$(tput sgr0);
export LESS_TERMCAP_se=$(tput sgr0);
export LESS_TERMCAP_so=$(tput bold; tput setaf 11; tput setab 27);
export LESS_TERMCAP_ue=$(tput sgr0);
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 6);
export LESS_TERMCAP_mr=$(tput rev);
export LESS_TERMCAP_mh=$(tput dim);
export LESS_TERMCAP_ZN=$(tput ssubm);
export LESS_TERMCAP_ZV=$(tput rsubm);
export LESS_TERMCAP_ZO=$(tput ssupm);
export LESS_TERMCAP_ZW=$(tput rsupm);

## Place current directory in bash title/tab
export PROMPT_COMMAND='echo -ne "\033]0; ${PWD##*/}\007"';

## share bash history across sessions/tabs
export HISTSIZE=25000;
export HISTFILESIZE=10000;
export HISTCONTROL=ignoredups:erasedups;
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND";

## nvm
export NVM_DIR=~/.nvm;

## aws-vault config
export AWS_VAULT_BACKEND=file;
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
## Append to bash history instead of overwriting
shopt -s histappend;

## cd changes (autocd and dirspell require Bash >= 4)
shopt -s cdspell;
shopt -s autocd   2>/dev/null || true;
shopt -s dirspell 2>/dev/null || true;

## adjusts alignment after each command
shopt -s checkwinsize;

## globbing changes
shopt -s nocaseglob;
shopt -s extglob
# ----- SHELL OPTIONS -----

# ----- ALIASES -----
## Navigation
alias ..="cd .."
alias ...="cd ../.."
alias ~="cd ~"
alias -- -="cd -"

## File management
alias la="ls -alhT"       # ls all, long format, human-readable size, type identifiers, colorized, complete time
alias lats="ls -alhtrT"     # la sorted by increasing time modified
alias mv="mv -iv"
alias cp="cp -Riv"
alias mkdir="mkdir -vp"

## git shortcuts
alias gdmb="git remote prune origin | grep '] origin/' | sed 's/^.*origin\///g' | xargs -L1 -J % git branch -D %"  # Delete branches that have been deleted on the remote repository (works with squash&merge repos)

## ctags
alias actags="rm tags; ctags --exclude=.git --exclude=vendor --exclude=node_modules --exclude=coverage --exclude=public -R .;"

## Reloading profile
alias sop="source ~/.bash_profile;"

## Better silver searcher default
alias ag="ag --hidden --path-to-ignore ~/.ignore"

## Tmux
alias tls="tmux ls"
alias tk="tmux kill-server"

## Disk Usage
alias hdu1="du -hd1 | sort -rh"
alias hdf="df -ah"

## Kubernetes
alias kc="kubectl"

## replacement for GREP_OPTIONS
grep --color=auto < /dev/null &>/dev/null &&
  alias grep='grep --color=auto'

## cross-platform color support for ls (replaces CLICOLOR)
if ls --color=auth &>/dev/null; then
  alias ls='ls -F --color=auto'
else
  alias ls='ls -F -G'
fi
# ----- ALIASES -----

# ----- PROMPT -----
## Requires a terminal that supports 256 colors
## Requires a Powerline-patched font

PROMPT="❱"
MORE_PROMPT="..."
PROMPT_SEPERATOR='|'
PROMPT_ERROR='✘'
PROMPT_ROOT='⚡'
PROMPT_JOBS='⚙'
GIT_BRANCH=''
GIT_CLEAN='✔'
GIT_STAGED='●'
GIT_UNSTAGED='Δ'
GIT_UNTRACKED='✚'

BLACK=$(tput setaf 0)
RED=$(tput setaf 160)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 11)
ORANGE=$(tput setaf 208)
BLUE=$(tput setaf 27)
PURPLE=$(tput setaf 93)
MAGENTA=$(tput setaf 126)
CYAN=$(tput setaf 6)
WHITE=$(tput setaf 7)
PINK=$(tput setaf 207)
BOLD=$(tput bold)
RESET=$(tput sgr0)

## Build git file changes portion on dirty git repos
parse_git_dirty() {
  local details=''
  local status=$(git status -b --porcelain 2> /dev/null || git status --porcelain 2> /dev/null)

  if [[ -n "${status}" ]]; then
    local untracked_count="$(grep -Ec '^\?\? .+' <<< "${status}")"
    local unstaged_count="$(grep -Ec '^.[^ ?#] .+' <<< "${status}")"
    local staged_count="$(grep -Ec '^[^ ?#]. .+' <<< "${status}")"

    [[ "${staged_count}" -gt 0 ]] && details+=" ${RESET}${BOLD}${GREEN}${GIT_STAGED}${staged_count}"
    [[ "${unstaged_count}" -gt 0 ]] && details+=" ${RESET}${BOLD}${YELLOW}${GIT_UNSTAGED}${unstaged_count}"
    [[ "${untracked_count}" -gt 0 ]] && details+=" ${RESET}${BOLD}${RED}${GIT_UNTRACKED}${untracked_count}"
    [[ "${staged_count}" -eq 0 ]] && [[ "${unstaged_count}" -eq 0 ]] && [[ "${untracked_count}" -eq 0 ]] && details+=" ${RESET}${BOLD}${GREEN}${GIT_CLEAN}"
  fi

  if [[ -n "${details}" ]]; then
    echo -n "${details}"
  fi
}

## Get the current git branch
parse_git_branch() {
  local branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
  echo -n "${RESET}${BOLD}${MAGENTA}${branch}"
}

## Build the git branch portion of the prompt
prompt_branch_icon() {
  if [[ -n "$(git branch --no-color 2> /dev/null)" ]]; then
    echo -n "${RESET}${BOLD}${MAGENTA}${GIT_BRANCH}"
  fi
}

## Build the git portion of the prompt
prompt_git() {
  echo -n " \$(prompt_branch_icon)\$(parse_git_branch)\$(parse_git_dirty)"
}

## Put current time in 12 hour format at beginning of prompt
prompt_timestamp() {
  echo -n "${RESET}${BOLD}${ORANGE}\t"
}

## Build user and host portion of the prompt
prompt_context() {
  local left_sq_bracket="${RESET}${BOLD}${GREEN}["
  local right_sq_bracket="${RESET}${BOLD}${GREEN}]"
  local at_symbol="${RESET}${BOLD}${PINK}@"
  local user_name="${RESET}${BOLD}${PURPLE}\u"
  local host_name="${RESET}${BOLD}${BLUE}\H"

  if [[ -n ${SSH_CLIENT} ]]; then
    echo -n " ${left_sq_bracket}${user_name}${at_symbol}${host_name}${right_sq_bracket}"
  else
    echo -n " ${left_sq_bracket}${user_name}${right_sq_bracket}"
  fi
}

## Build current working directory portion of the prompt
prompt_cwd() {
  echo -n " ${RESET}${BOLD}${CYAN}\w"
}

## Add prompt character
prompt_char(){
  echo -n "${RESET}\n${PROMPT} "
}

build_prompt() {
  prompt_timestamp
  prompt_context
  prompt_cwd
  prompt_git
  prompt_char
}

export PS1="${RESET}\n$(build_prompt)"
export PS2="${MORE_PROMPT} "
# ----- PROMPT -----

# Load all the shell dotfiles
for file in ~/.{functions,work,secrets}; do
  [ -r "$file" ] && . "$file"
done;
unset file;

# Enable Homebrew bash completion
[[ -r "$(brew --prefix)/etc/profile.d/bash_completion.sh" ]] && . "$(brew --prefix)/etc/profile.d/bash_completion.sh"

# Need this so SSH passphrase isn't constantly asked for in tmux sessions
ssh-add -A 2>/dev/null;

command -v fzf >/dev/null 2>&1 && eval "$(fzf --bash)"
[ -n "$BASH" ] && complete -F _fzf_complete_git -o default -o bashdefault git

# For nvm installed via creationix install.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# ----- EVERYTHING BELOW HERE WAS AUTOMATICALLY ADDED, PROBABLY BY SOME STUPID WORK BULLSHIT -----
