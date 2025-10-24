" Disable Vi compatibility (this is nvim's default)
set nocompatible

" Enable completion where available.
" This setting must be set before ALE is loaded.
let g:ale_completion_enabled = 1

" install vim-plug automatically
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" install plugins, check if nvim present to determine install location
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

Plug 'tpope/vim-fugitive'                | "git wrapper"
Plug 'airblade/vim-gitgutter'            | "git diff in gutter"

Plug 'vim-airline/vim-airline'           | "status/tabline"
Plug 'vim-airline/vim-airline-themes'    | "vim-airline themes"

Plug 'ctrlpvim/ctrlp.vim'                | "old fuzzy finder"
Plug 'junegunn/fzf', { 'do': { -> fzf#install() }}
Plug 'junegunn/fzf.vim'                  | "new fuzzy finder"
Plug 'tpope/vim-surround'                | "parenthesizing"
Plug 'bronson/vim-trailing-whitespace'   | "highlights trailing whitespace"

Plug 'dense-analysis/ale'                | "syntax checker"
Plug 'pangloss/vim-javascript'           | "JS indentation and syntax"
Plug 'leafgarland/typescript-vim'        | "typescript syntax highlighting"

Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }  | "golang support"

Plug 'vim-ruby/vim-ruby'                 | "ruby syntax highlighting"

Plug 'gertjanreynaert/cobalt2-vim-theme' | "Cobalt2 color scheme"

call plug#end()

" turn on filetype detection, indent scripts, and filetype plugins
" keep this after plugins block
filetype plugin indent on

" turn on syntax highlighting
syntax on
set encoding=utf-8

" appearance
colors cobalt2
set visualbell
set number
set ruler
set colorcolumn=90
highlight ColorColumn ctermbg=8
set showcmd
set cursorline

" Default white space settings
set nowrap
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set nojoinspaces

" Go-specific white space settings (tabs with width 8)
autocmd FileType go setlocal tabstop=8
autocmd FileType go setlocal shiftwidth=8
autocmd FileType go setlocal noexpandtab  " Use actual tabs
autocmd FileType go setlocal softtabstop=0

" searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" system clipboard support
set clipboard=unnamed

" misc
set backspace=indent,eol,start
set noswapfile
set scrolloff=5
set nostartofline
set nrformats+=alpha

" vim-airline configuration
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline_theme='simple'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_inactive_collapse=1
let g:airline#extensions#tabline#enabled = 0
let g:airline#extensions#ale#enabled = 1
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'v'  : 'V',
    \ 'V'  : 'V-L',
    \ '^V' : 'V-B',
    \ 'c'  : 'CMD',
    \ 's'  : 'S',
    \ 'S'  : 'S-L',
    \ '^S' : 'S-B',
    \ 't'  : 'T',
    \ }

" Ctags configuration
set tags=./.tags,.tags

" CtrlP configuration
let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:20,results:50'
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden=1

if executable('rg')
  " use rg over grep
  set grepprg=rg\ --vimgrep\ --smart-case\ --follow\ --hidden

  " use rg in CtrlP for listing files
  let g:ctrlp_user_command = 'rg %s --files --hidden --color=never --glob ""'

  " rg is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" fzf configuration
let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.7 } }

" ALE configuration
let g:ale_lint_on_save = 1
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'typescript': ['tsserver', 'eslint'],
\   'ruby': ['standardrb',]
\}
let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['prettier', 'eslint'],
\   'typescript': ['prettier', 'eslint'],
\   'ruby': ['standardrb']
\}

" change leader key
let mapleader = "\<Space>"

" my keymap (don't put whitespace in front of pipes, or it all breaks)
inoremap jk <Esc>|                                 "exit insert mode
nnoremap sa :w<CR>|                                "save file from normal mode
nnoremap <leader>js :%!python3 -m json.tool<CR>|   "pretty print json"
nnoremap <C-h> <C-w>h|                             "switch split left
nnoremap <C-j> <C-w>j|                             "switch split down
nnoremap <C-k> <C-w>k|                             "switch split up
nnoremap <C-l> <C-w>l|                             "switch split right
map <leader>x :bd<CR>|                             "delete buffer
nnoremap <C-c> :bp\|bd #<CR>|                      "delete buffer, keep split
map <leader>c :!git ctags<CR><CR>|                 "re/create ctags
nmap <silent> <leader><Space> :nohlsearch<CR>|     "clear search highlighting
nnoremap <silent> <C-f> :Files<CR>|                "search files with fzf"
nnoremap <silent> <leader>f :Rg<CR>|               "search inside files with ripgrep"
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>|     "grep word under cursor
"ctags go to definition
"ctags go back
"nav buffers in single pane
