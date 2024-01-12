# Anth's dotfiles

## Usage

### The easy way (untested)
- From your `$HOME`, run
  ```
  curl https://raw.githubusercontent.com/anthlam/dotfiles/master/.macos | bash
  ```

### The hard way
Run the following from your `$HOME` directory.

- Clone repo to your local.
  ```
  git clone git@github.com:anthlam/dotfiles.git "${HOME}/code/me/dotfiles"
  ```
- Review files and customize as you see fit, add any files you don't need or
don't want to the `.dotignore` file.
- Install [ Homebrew ](https://brew.sh/).
- Run bootstrap script to copy everything in the repo to your `$HOME` directory.
  ```
  $ /bin/bash -c "${HOME}/code/me/dotfiles/bootstrap.sh --force"
  ```
- Brew install
  ```
  $ brew bundle --file="${HOME}/.brewfile"
  ```
- Complete instructions listed in Caveats of homebrew install.
- Change shell to the bash version installed by homebrew.
  ```
  $ sudo -s
  $ echo "/usr/local/bin/bash" >> /etc/shells
  $ chsh -s /usr/local/bin/bash
  $ exec "${SHELL}"
  ```
- (Optional) Install spacemacs
  ```
  $ ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/
  $ git clone https://github.com/syl20bnr/spacemacs $HOME/.emacs.d
  $ (cd $HOME/.emacs.d && git checkout develop)
  ```
- (Optional) Install language/tool specific packages
  - `brew install python postgresql elixir`
  - [Install NVM](https://github.com/creationix/nvm#installation)
  - [Install RVM](https://rvm.io/rvm/install)
  - Install desired node versions using `nvm`
  - Install desired ruby versions using `rvm`

## TODO
- Document `.secrets` setup/usage.
- Document `.work` setup/usage.
