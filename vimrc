"-------------------------------------------------------------------------------
" BASIC CUSTOMIZATIONS
"-------------------------------------------------------------------------------

set nocompatible

let mapleader      = ' '
let maplocalleader = ' '

" turn on syntax highlighting
syntax on

" turn line numbers on
"set number " fixed style
"set nu rnu " relative style

" enable mouse rolling
if has("mouse")
    set mouse=a
endif

" hide buffer when switching, thus undo history is retained
set hidden

" make vim try to detect file types
filetype on
filetype plugin on
filetype indent on

" reload file outside vim
set autoread

" encoding utf 8
set encoding=utf-8
set fileencoding=utf-8

" greatly enhance search
runtime macros/matchit.vim

" set unix line ending
set fileformat=unix

" Backspace works in Insert mode (e.g. not inserting a ^?), but won't delete
" over line breaks, or automatically-inserted indentation, or the place where
" insert mode started:
" ref: http://vim.wikia.com/wiki/Backspace_and_delete_problems
set backspace=indent,eol,start

" search settings
set incsearch
set hlsearch

" case-related search
set ignorecase " case insensitive searching
set smartcase " when a search pattern includes uppcase, this search is sensitive

" remove .ext~ files but not the swapfiles
set nobackup
set writebackup
set noswapfile

" longer timeout for <Leader> key
set timeoutlen=8000

" display matching files
set wildmenu

" suggestion for normal mode commands
set wildmode=longest:full,full

" minimal menu bar
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" keep cursor visible within 5 lines
set scrolloff=5

" highlight the cursor
"set cursorline

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return '[PASTE] '
    endif
    return ''
endfunction

" indentation
set expandtab " force to use spaces for indentation
set autoindent " press Enter, start the new line at the same indent as the previous line
set smartindent " treat *.py with 'tab' favor
set shiftwidth=4 " block indent/unindent blocks using < and >
set tabstop=4 " 4-space indent
set softtabstop=4 " see multiple spaces as tabstops

" color column
set colorcolumn=80

" more natural splitting
set splitbelow
set splitright

" set GUI vim tab label: tab number + filename + sign
set guitablabel=\[%N\]\ %t\ %M

" set GUI font
if has('gui_running')
    set guifont=Inconsolata\ Regular\ 13
endif

" Disable format option with command 'o' in normal mode not to have it
" continue in commenting
autocmd BufRead,BufNewFile * set formatoptions-=o

" Remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" Always show the status line
set laststatus=2

" Format the status line
set statusline=%m                                   "modified
set statusline+=%t                                  "tail
set statusline+=\ [%{strlen(&fenc)?&fenc:'none'},   "file encoding
set statusline+=\ %{&ff}]                           "file format
set statusline+=\ %y                                "filetype
set statusline+=\ %r                                "read only flag
set statusline+=\ %h                                "help file flag
set statusline+=%=                                  "left/right separator
set statusline+=%{HasPaste()}                       "paste-mode
set statusline+=%l:%c                               "line:column
set statusline+=\ (%P)                              "percentage

"-------------------------------------------------------------------------------
" Color settings
"-------------------------------------------------------------------------------
if &term =~ '256color'
    " disable Background Color Erase (BCE) so that color schemes
    "   " render properly when inside 256-color tmux and GNU screen.
    "     " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
    set t_ut=
endif

set background=dark
highligh MatchParen ctermfg=red ctermbg=blue
highligh Visual ctermfg=red ctermbg=green
highligh Search ctermfg=red ctermbg=lightyellow
highlight StatusLine ctermfg=black ctermbg=darkcyan
highlight ColorColumn ctermfg=red ctermbg=yellow
highlight CursorLine cterm=NONE ctermfg=None ctermbg=darkred

" Highlight trailing whitespaces and starting tabs
highlight ExtraWhitespace ctermbg=lightblue
au ColorScheme * highlight ExtraWhitespace
au BufEnter * match ExtraWhitespace /\s\+$\|\t/
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$\|\t/
au InsertLeave * match ExtraWhiteSpace /\s\+$\|\t/

" highlight active pane with column
augroup BgHighlight
    autocmd!
    autocmd WinEnter * set colorcolumn=80
    autocmd WinLeave * set colorcolumn=0
augroup END

"-------------------------------------------------------------------------------
" PLUGIN MANAGER
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
"-------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')
" call plug#begin('~/.local/share/nvim/plugged')

Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'majutsushi/tagbar'
Plug 'mkitt/tabline.vim'
Plug 'Chiel92/vim-autoformat'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
""Plug 'ternjs/tern_for_vim', { 'do': 'npm install' }
""Plug 'vim-latex/vim-latex'
Plug 'w0rp/ale' " async lint

Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'leshill/vim-json', { 'for': 'json' }
Plug 'tpope/vim-surround' " allow easy editing surrounding tags
Plug 'tpope/vim-fugitive' " integrate Git
Plug 'tpope/vim-eunuch' " Some useful shell commands
Plug 'mattn/emmet-vim' " this plugin is useful for html, xml editing (more advanced than ragtag)
Plug 'othree/javascript-libraries-syntax.vim' " syntax libraris for different js projects
Plug 'Valloric/YouCompleteMe', { 'do': './install.py' }
Plug 'nathanaelkane/vim-indent-guides'
""Plug 'Konfekt/FastFold'
Plug 'python-mode/python-mode', {'branch': 'develop', 'for': 'python'} " better python support than builtin one
Plug 'vim-scripts/bash-support.vim'
""Plug 'xolox/vim-session'
""Plug 'xolox/vim-misc'
""Plug 'SirVer/ultisnips'
""Plug 'honza/vim-snippets'
Plug 'vimwiki/vimwiki'
Plug 'sheerun/vim-polyglot'
" Initialize plugin system
call plug#end()

"-------------------------------------------------------------------------------
" PLUGINs
"-------------------------------------------------------------------------------

" NERDtree
" automatically open NERDTree when starting vim with a directory
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

" tagbar
let g:tagbar_autofocus = 1  " jump to tagbar right after opening
let g:tagbar_sort = 0  " sort items according to their order in file

" tabline
hi TabLine      ctermfg=Black  ctermbg=Brown cterm=NONE
hi TabLineFill  ctermfg=Black  ctermbg=Brown cterm=NONE
hi TabLineSel   ctermfg=White  ctermbg=DarkBlue  cterm=NONE

" autoformat
let g:formatter_yapf_style = 'pep8'

" fzf
" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
            \ call fzf#vim#grep(
            \   'git grep --line-number '.shellescape(<q-args>), 0,
            \   { 'dir': systemlist('git rev-parse --show-toplevel')[0] }, <bang>0)
let g:fzf_layout = { 'down': '30%' } " set fzf layout

" Git
" File files in project (including non-gitted)
function! s:find_git_root()
    return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction

command! ProjectFiles execute 'Files' s:find_git_root()

" JS libraries syntax
let g:used_javascript_libs = 'angularjs,jasmine,gulp'

" vim-latex
set shellslash
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_MultipleCompileFormats='pdf,bibtex,pdf'
let g:Tex_ViewRule_pdf = 'evince'
let g:tex_conceal = ""
let g:Tex_GotoError=0
let g:Tex_IgnoredWarnings =
            \'Underfull'."\n".
            \'Overfull'."\n".
            \'specifier changed to'."\n".
            \'You have requested'."\n".
            \'Missing number, treated as zero.'."\n".
            \'There were undefined references'."\n".
            \'Package lcg Warning:'."\n".
            \'Citation %.%# undefined'
let g:Tex_IgnoreLevel = 8

" Python-mode
let g:pymode_rope = 0
let g:pymode_lint_on_write = 0

" Async Lint Engine
let g:ale_enabled = 0 " disable ALE by default
let g:ale_linters = {
            \'python': ['flake8', 'pylint'],
            \}
" vim-session
"" let g:session_directory='~/progstuffs/vim-session'
"" let g:session_autosave='no'

" snippets
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-f>"
let g:UltiSnipsJumpForwardTrigger="<c-l>"
let g:UltiSnipsJumpBackwardTrigger="<c-h>"

" If you want :UltiSnipsEdit to split your window.
"" let g:UltiSnipsEditSplit="vertical"

" Bash support
let g:BASH_MapLeader  = '\'

" Cpp
set tags=./tags;/

"-------------------------------------------------------------------------------
" BASIC KEY BINDINGS
"-------------------------------------------------------------------------------
" better splitting motion
nnoremap <C-h> <C-W>h
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-l> <C-W>l

nnoremap <C-Left> <C-W>h
nnoremap <C-Down> <C-W>j
nnoremap <C-Up> <C-W>k
nnoremap <C-Right> <C-W>l

" convenient macro-ing
nnoremap Q @q

" Quickfix stuffs
nnoremap ]q :cnext<CR>
nnoremap [q :cprev<CR>
nnoremap ]l :lnext<CR>
nnoremap [l :lprev<CR>

" Buffer stuffs
nnoremap ]b :bnext<CR>
nnoremap [b :bprev<CR>

" Toggle paste mode on and off
map <leader>pp :setlocal paste!<cr>

"-------------------------------------------------------------------------------
" PLUGINS KEY BINDINGS
"-------------------------------------------------------------------------------

" NERDTree shortcuts
nnoremap <silent> <Leader>nn :NERDTreeToggle<CR>
nnoremap <silent> <Leader>nf :NERDTreeFind<CR>

" Tagbar
nnoremap <silent> <Leader>tt :TagbarToggle<CR>

" autoformat
noremap <F3> :Autoformat<CR>

" ALE
nnoremap <silent> <Leader>aa :ALEToggle<CR>
nmap ]a <Plug>(ale_next_wrap)
nmap [a <Plug>(ale_previous_wrap)

"-------------------------------------------------------------------------------
" FZF KEY BINDINGS
"-------------------------------------------------------------------------------
" Buffers shortcut
nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>rr :Rg<CR>
nnoremap <silent> <Leader>gf :GFiles<CR>
nnoremap <silent> <Leader>gg :GGrep<CR>
nnoremap <silent> <Leader>bb :Buffers<CR>
nnoremap <silent> <Leader>pf :ProjectFiles<CR>

"-------------------------------------------------------------------------------
" DWIM KEY BINDINGS
"-------------------------------------------------------------------------------
" Files  word under cursor
nnoremap <silent> ,ff :Files <C-R><C-W><CR>
xnoremap <silent> ,ff :<C-W>Files <C-R><C-*><CR>
" rg word under cursor
nnoremap <silent> ,rr :Rg <C-R><C-W><CR>
xnoremap <silent> ,rr :<C-W>Rg <C-R><C-*><CR>
" GFiles  word under cursor
nnoremap <silent> ,gf :GFiles <C-R><C-W><CR>
xnoremap <silent> ,gf :<C-W>GFiles <C-R><C-*><CR>
" GGrep  word under cursor
nnoremap <silent> ,gg :GGrep <C-R><C-W><CR>
xnoremap <silent> ,gg :<C-W>GGrep <C-R><C-*><CR>
" search word under cursor
nnoremap <silent> ,ss /<C-R><C-W><CR>
" BLines word under cursor
nnoremap <silent> ,bl :BLines <C-R><C-W><CR>
xnoremap <silent> ,bl :<C-W>BLines <C-R><C-*><CR>
" Grep and pipe the results to quickfix
set grepprg=rg\ --vimgrep\ --no-heading\ --smart-case
nnoremap <silent> ,rg :grep <C-R><C-W><CR>
xnoremap <silent> ,rg :<C-W>grep <C-R><C-*><CR>

