let mapleader = "\<Space>"
set encoding=utf-8
let g:ale_completion_enabled = 1

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'                | "git wrapper"
Plug 'airblade/vim-gitgutter'            | "git diff in gutter"

Plug 'vim-airline/vim-airline'           | "status/tabline"
Plug 'vim-airline/vim-airline-themes'    | "vim-airline themes"

Plug 'ctrlpvim/ctrlp.vim'                | "fuzzy finder"
Plug 'tpope/vim-surround'                | "parenthesizing"
Plug 'bronson/vim-trailing-whitespace'   | "highlights trailing whitespace"

Plug 'dense-analysis/ale'                | "syntax checker"
Plug 'pangloss/vim-javascript'           | "JS indentation and syntax"
Plug 'leafgarland/typescript-vim'        | "typescript syntax highlighting"

Plug 'gertjanreynaert/cobalt2-vim-theme' | "Cobalt2 color scheme"

call plug#end()

filetype plugin indent on
syntax enable

" appearance
colors cobalt2
set visualbell

" for vim-airline
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline_theme='simple'
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_inactive_collapse=1
let g:airline#extensions#tabline#enabled = 0
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

" rulers and such
set number
set ruler
set colorcolumn=90
highlight ColorColumn ctermbg=8
set showcmd
set cursorline

" tabs and white space
set nowrap
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set nojoinspaces

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


" for Ctags
set tags=./.git/tags,tags

" for CtrlP
let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:20,results:50'
let g:ctrlp_max_files = 0
let g:ctrlp_show_hidden=1

" for Silver Searcher
if executable('ag')
  " use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " use ag in CtrlP for listing files.
  " works with agignore file in $HOME
  let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0

  " bind K to grep word under cursor
  nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

  " bind \ (backward slash) to grep shortcut
  command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
  nnoremap \ :Ag<SPACE>
endif

"for ALE
let g:ale_lint_on_save = 1
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'typescript': ['tsserver', 'eslint']
\}
let g:ale_linters_explicit = 1
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'javascript': ['prettier', 'eslint'],
\   'typescript': ['prettier', 'eslint']
\}

" my keymap (the pipe is not part of the command, it just enables inline comments)
inoremap fd <Esc>|                                  "exit insert mode
nnoremap sa :w<CR>|                                 "save file from normal mode
nnoremap <leader>js :%!python -m json.tool<CR>|     "pretty print json"
nnoremap <C-h> <C-w>h|                              "switch split left
nnoremap <C-j> <C-w>j|                              "switch split down
nnoremap <C-k> <C-w>k|                              "switch split up
nnoremap <C-l> <C-w>l|                              "switch split right
map <leader>x :bd<CR>|                              "delete buffer
map <leader>c :!git ctags<CR><CR>|                  "re/create ctags
nmap <silent> <leader><Space> :FixWhitespace<CR>|   "delete trailing whitespace
nmap <silent> <CR> :silent noh<CR>|                 "clear search highlighting
nnoremap <C-c> :bnext\|bdelete #<CR>|               "delete buffer without deleting window
"ctags go to definition
"ctags go back
"nav buffers in single pane
