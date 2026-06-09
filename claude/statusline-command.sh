#!/usr/bin/env bash

# Claude Code statusLine command
# Single line, fully color-coded with truecolor RGB:
#   repo name (bold cyan) | branch icon + git branch (bold magenta) |
#   context used/total + % (yellow) | session usage % (red) | model (gray)
# Elements separated by dim gray pipes.

input=$(cat)

# --- truecolor helpers ---
RESET='\033[0m'
BOLD='\033[1m'
DIM='\033[2m'

# Bold cyan (truecolor) for repo name
bold_cyan()  { printf '\033[1;38;2;0;210;210m%s\033[0m' "$1"; }
# Bold magenta (truecolor) for branch
bold_magenta() { printf '\033[1;38;2;210;0;210m%s\033[0m' "$1"; }
# Yellow (truecolor) for context
yellow() { printf '\033[38;2;220;200;0m%s\033[0m' "$1"; }
# Red (truecolor) for session usage
red() { printf '\033[38;2;220;60;60m%s\033[0m' "$1"; }
# Gray (truecolor) for model
gray() { printf '\033[38;2;150;150;150m%s\033[0m' "$1"; }
# Dim gray pipe separator
pipe() { printf '\033[2;38;2;100;100;100m | \033[0m'; }

# --- repo name ---
repo=$(echo "$input" | jq -r '.workspace.repo | if . then .name else empty end')
if [[ -z "$repo" ]]; then
    # Fall back to basename of project_dir
    proj=$(echo "$input" | jq -r '.workspace.project_dir // empty')
    [[ -n "$proj" ]] && repo=$(basename "$proj")
fi

# --- git branch ---
cwd=$(echo "$input" | jq -r '.cwd')
branch=""
if git -C "$cwd" rev-parse --git-dir >/dev/null 2>&1; then
    branch=$(git -C "$cwd" rev-parse --abbrev-ref HEAD 2>/dev/null)
fi

# --- context window ---
total=$(echo "$input" | jq -r '.context_window.context_window_size // empty')
used_tokens=$(echo "$input" | jq -r '.context_window.total_input_tokens // empty')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Format token counts: show as K (thousands)
fmt_tokens() {
    local n="$1"
    if [[ -z "$n" || "$n" == "null" ]]; then echo "?"; return; fi
    printf '%dk' "$(( n / 1000 ))"
}

ctx_str=""
if [[ -n "$used_tokens" && -n "$total" ]]; then
    used_fmt=$(fmt_tokens "$used_tokens")
    total_fmt=$(fmt_tokens "$total")
    if [[ -n "$used_pct" && "$used_pct" != "null" ]]; then
        pct_fmt=$(printf '%.0f' "$used_pct")
        ctx_str="${used_fmt}/${total_fmt} (${pct_fmt}%)"
    else
        ctx_str="${used_fmt}/${total_fmt}"
    fi
fi

# --- session usage (5-hour rate limit) ---
session_str=""
five_pct=$(echo "$input" | jq -r '.rate_limits.five_hour.used_percentage // empty')
if [[ -n "$five_pct" ]]; then
    session_pct=$(printf '%.0f' "$five_pct")
    session_str="${session_pct}% session"
fi

# --- model name ---
model=$(echo "$input" | jq -r '.model.display_name // empty')

# --- assemble ---
out=""

if [[ -n "$repo" ]]; then
    out+="$(bold_cyan "$repo")"
fi

if [[ -n "$branch" ]]; then
    [[ -n "$out" ]] && out+="$(pipe)"
    out+="$(bold_magenta "⎇ $branch")"
fi

if [[ -n "$ctx_str" ]]; then
    [[ -n "$out" ]] && out+="$(pipe)"
    out+="$(yellow "$ctx_str")"
fi

if [[ -n "$session_str" ]]; then
    [[ -n "$out" ]] && out+="$(pipe)"
    out+="$(red "$session_str")"
fi

if [[ -n "$model" ]]; then
    [[ -n "$out" ]] && out+="$(pipe)"
    out+="$(gray "$model")"
fi

printf "%s\n" "$out"
