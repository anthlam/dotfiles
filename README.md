# Anth's dotfiles

## Usage

### Quick Install
From your `$HOME` directory, run:
```bash
curl https://raw.githubusercontent.com/anthlam/dotfiles/master/.macos | bash
```

### Manual Install
If you want to review and customize before installing:

1. Clone the repo:
   ```bash
   git clone git@github.com:anthlam/dotfiles.git "${HOME}/code/me/dotfiles"
   ```

2. Review and customize the `.macos` script and dotfiles as needed.

3. Run the setup script:
   ```bash
   bash "${HOME}/code/me/dotfiles/.macos"
   ```

The `.macos` script will:
- Install Homebrew
- Generate SSH keys for GitHub
- Symlink all dotfiles to your `$HOME` directory
- Install all packages from `.brewfile`
- Set up neovim configuration
- Set up fzf key bindings and fuzzy completion
- Install and configure Spacemacs
- Install nvm and latest Node.js LTS
- Configure macOS system preferences

### Optional Post-Install

- Install language/tool specific packages:
  - `brew install python postgresql elixir`
  - [Install RVM](https://rvm.io/rvm/install)
  - Install desired ruby versions using `rvm`

## TODO
- Document `.secrets` setup/usage.
- Document `.work` setup/usage.
