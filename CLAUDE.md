# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal dotfiles repository for macOS that manages shell configuration, editor setup, and development environment. Files from this repo are synced to `$HOME` using rsync.

## Architecture

### Installation System

The dotfiles use a symlink-based deployment system via `.macos`:
- Files are symlinked from repo to `$HOME` directory (using `ln -sf` for idempotency)
- `.dotignore` controls which files are excluded from the repo (like `.git/`, `.DS_Store`, etc.)
- The `.macos` script handles initial setup: Homebrew, SSH keys, dotfile linking, package installation, and macOS preferences

### Shell Configuration Loading Order

1. `.bash_profile` → sources `.bashrc`
2. `.bashrc` → loads all modular dotfiles in this order:
   - `.path` - PATH configuration
   - `.bash_prompt` - prompt customization
   - `.exports` - environment variables
   - `.aliases` - command shortcuts
   - `.functions` - custom bash functions
   - `.work` - work-specific config (optional, gitignored)
   - `.secrets` - sensitive config (optional, gitignored)

### Key Configuration Files

- **`.path`**: Manages PATH setup, includes Homebrew, Python, Go, and RVM paths
- **`.exports`**: Sets editor (nvim), NVM_DIR, history settings, AWS vault config, fzf/ripgrep integration
- **`.aliases`**: Navigation shortcuts, file management, git shortcuts, editor aliases
- **`.functions`**: Helper functions (git branch cleanup, tmux management, weather, fzf completion)
- **`.bashrc`**: Enables bash completion, histappend, cdspell, nocaseglob, fzf integration, loads nvm/rvm
- **`.vimrc`**: Neovim/Vim config with vim-plug, ALE linting/fixing, fzf integration, custom keybindings
- **`.gitconfig`**: Git aliases, diff/merge settings, conditional includes for work/personal configs
- **`.brewfile`**: Homebrew package definitions (terminal tools, editors, CLI utilities)

## Common Commands

### Installation/Setup
```bash
# Initial setup (run once on new machine)
bash "${HOME}/code/me/dotfiles/.macos"

# The script is idempotent - safe to re-run if needed
```

### Homebrew Management
```bash
# Install all packages from .brewfile
brew bundle --file="${HOME}/.brewfile"

# Update .brewfile with currently installed packages
brewup  # Custom function defined in .functions
```

### Shell/Editor
```bash
# Reload shell configuration
source ~/.bash_profile

# Install/update vim plugins (run inside vim/nvim)
:PlugInstall
:PlugUpdate
```

### Git Workflow
The repo uses git aliases defined in `.gitconfig:15-58`:
- `git ac "message"` - add all and commit
- `git ls` / `git lr` - formatted log with graph
- `git br` - branches sorted by commit date
- `git r` / `git r1` / `git rs` - various reset shortcuts

## Important Notes

### Environment-Specific Files
- `.work` and `.secrets` are optional files loaded by `.bashrc:4` but excluded from git
- Git config uses conditional includes (`.gitconfig:70-74`) for work/personal settings
- Never commit `.work` or `.secrets` files

### Path Management
- `.path` is loaded first and resets PATH to clean state before building it up
- Homebrew paths must be configured for Apple Silicon (`/opt/homebrew`) vs Intel (`/usr/local`)
- Python path uses `$(brew --prefix python@3)` for version resilience

### Editor Configuration
- Default editor is `nvim` (set in `.exports:4`)
- Vim configuration auto-installs vim-plug on first run (`.vimrc:8-13`)
- ALE handles linting and auto-fixing on save for JS/TS/Ruby

### fzf Integration
- Configured to use ripgrep as backend (`.exports:39-43`)
- Custom bash completion for git commands (`.functions:71-76`)
- Vim keybinding: `Ctrl-f` for file search, `<leader>f` for content search
