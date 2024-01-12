#!/usr/bin/env bash

# Load all the shell dotfiles
for file in ~/.{path,bash_prompt,exports,aliases,functions,work,secrets}; do
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

# For Layer0
eval "$(direnv hook bash)"

# For nvm installed via homebrew
# . $(brew --prefix nvm)/nvm.sh;

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# For nvm installed via creationix install.sh
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
