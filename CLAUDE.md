# CLAUDE.md

Personal macOS dotfiles. The repo lives at `~/code/me/dotfiles`; `install` symlinks its files into `$HOME`, so edits here are live.

## Shell

- `.bashrc` is the single shell config, in marked sections: ENVIRONMENT, PATH, SHELL OPTIONS, ALIASES, PROMPT, FUNCTIONS. Add to the matching section.
- Everything below the "AUTOMATICALLY ADDED" banner is installer debris; move it to `~/.bashrc.local`, which `.bashrc` sources just above that banner.
- `.bash_profile` and `.profile` only source `.bashrc`.

## Deployment

- Dotfiles are symlinked by the `symlink` loop in `install`. A new top-level dotfile needs an entry in that `dotfiles` array.
- `.dotignore` lists repo files that stay out of `$HOME`.
- `claude/settings.json` is symlinked to `~/.claude/settings.json`; edits take effect immediately.
- `install` touches sudo, Homebrew, and macOS defaults. Reading it is safe; running it is the user's call.

## Git identity

`.gitconfig` selects `.gitconfig.work` under `~/code/work/` and `.gitconfig.personal` under `~/code/me/` via `includeIf gitdir`. Identity changes go in those two files, not `.gitconfig`.

## Machine-specific config

The tracked `.bashrc.local` is a template. The real one is a copy at `~/.bashrc.local` (not a symlink), holding secrets and work-only config. Keep those out of the repo.
