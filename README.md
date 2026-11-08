# Anth's dotfiles

## Usage

### Install

1. Clone the repo:
   ```bash
   git clone git@github.com:anthlam/dotfiles.git "${HOME}/code/me/dotfiles"
   ```

2. Review and customize the `install` script and dotfiles as needed.

3. Run the setup script from the repo:
   ```bash
   cd "${HOME}/code/me/dotfiles" && bash install
   ```

The `install` script will:
- Install Homebrew
- Generate SSH keys for GitHub
- Symlink dotfiles and config files (nvim, tmux, ghostty, Claude Code) into your `$HOME` directory
- Install the latest bash and make it your shell
- Install all packages from `.brewfile`
- Set up fzf key bindings and fuzzy completion
- Install and configure Spacemacs
- Install nvm and latest Node.js LTS
- Configure macOS system preferences

### Optional Post-Install

- Install language/tool specific packages:
  - `brew install python postgresql elixir`
  - [Install RVM](https://rvm.io/rvm/install)
  - Install desired ruby versions using `rvm`

## Machine-Specific Configuration

`.bashrc.local` in this repo is a template. Copy it to `~/.bashrc.local` (a copy, not a symlink) and add anything specific to that machine: work aliases, paths, API keys and other secrets.

```bash
cp "${HOME}/code/me/dotfiles/.bashrc.local" "${HOME}/.bashrc.local"
chmod 600 "${HOME}/.bashrc.local"
```

`.bashrc` sources it, so the same dotfiles repo works on personal and work machines. Keep the real file out of the repo.

Git identity is separate: `.gitconfig` includes `.gitconfig.work` under `~/code/work/` and `.gitconfig.personal` under `~/code/me/`.
