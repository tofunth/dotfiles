"-------------------------------------------------------------------------------
" BASIC CUSTOMIZATIONS
"-------------------------------------------------------------------------------
set nocompatible

let mapleader      = ' '
let maplocalleader = ' '

" longer timeout for <Leader> key
set timeoutlen=8000

syntax on
set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix

" hide buffer when switching, thus undo history is retained
set hidden

filetype on
filetype plugin on
filetype indent on

" better search
runtime macros/matchit.vim

" enable mouse rolling
if has("mouse")
    set mouse=a
endif

if has("gui_macvim")
    set guifont=Menlo\ Regular:h13
endif

" reload file outside vim
set autoread

" Backspace works in Insert mode (e.g. not inserting a ^?), but won't delete
" over line breaks, or automatically-inserted indentation, or the place where
" insert mode started:
" ref: http://vim.wikia.com/wiki/Backspace_and_delete_problems
set backspace=indent,eol,start

" search settings
set incsearch
set hlsearch

" case insensitive searching
set ignorecase

" when a search pattern includes uppcase, this search is sensitive
set smartcase

" remove .ext~ files but not the swapfiles
set nobackup
set writebackup
set noswapfile

" display matching files
set wildmenu

" suggestion for normal mode commands
set wildmode=longest:full,full

" minimal menu bar
" let g:netrw_browse_split=4  " open in prior window
" let g:netrw_altv=1          " open splits to the right
" let g:netrw_liststyle=3     " tree view
" let g:netrw_list_hide=netrw_gitignore#Hide()
" let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

" keep cursor visible within 5 lines
set scrolloff=5

" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return '[PASTE] '
    endif
    return ''
endfunction

" indentation
set expandtab     " force to use spaces for indentation
set autoindent    " press Enter, start the new line at the same indent as the previous line
set smartindent   " treat *.py with 'tab' favor
set shiftwidth=4  " block indent/unindent blocks using < and >
set tabstop=4     " 4-space indent
set softtabstop=4 " see multiple spaces as tabstops

" color column
set colorcolumn=80

" more natural splitting
set splitbelow
set splitright

" Disable format option with command 'o' in normal mode not to have it
" continue in commenting
autocmd BufRead,BufNewFile * set formatoptions-=o

" Remove trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

" " Always show the status line
set laststatus=2
"
" " Format the status line
set statusline=%m\                                " modified
set statusline+=%{HasPaste()}                     " paste-mode
set statusline+=%t                                " tail
set statusline+=\ [%{strlen(&fenc)?&fenc:'none'}, " file encoding
set statusline+=\ %{&ff}]                         " file format
set statusline+=\ %y                              " filetype
set statusline+=\ %r                              " read only flag
set statusline+=\ %h                              " help file flag
set statusline+=%=                                " left/right separator
set statusline+=%l:%c                             " line:column
set statusline+=\ (%P)                            " percentage

" disable folding
set nofoldenable
"-------------------------------------------------------------------------------
" Color settings
"-------------------------------------------------------------------------------
if &term =~ '256color'
    " disable Background Color Erase (BCE) so that color schemes
    "   " render properly when inside 256-color tmux and GNU screen.
    "     " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
    set t_ut=
endif

set t_Co=256
set bg=dark

"-------------------------------------------------------------------------------
" PLUGIN MANAGER
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
"-------------------------------------------------------------------------------
call plug#begin('~/.vim/plugged')
" call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'godlygeek/tabular'    " Quick tabularize
Plug 'tpope/vim-surround'   " allow easy editing surrounding tags
Plug 'tpope/vim-fugitive'   " integrate Git
Plug 'tomtom/tcomment_vim'  " Toggle comment
" Plug 'mattn/emmet-vim'
Plug 'sheerun/vim-polyglot'
Plug 'vim-latex/vim-latex'
" Initialize plugin system
call plug#end()

"-------------------------------------------------------------------------------
" Theme
"-------------------------------------------------------------------------------
set bg=dark

"-------------------------------------------------------------------------------
" NERD tree
"-------------------------------------------------------------------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
noremap <silent> <Leader>nn :NERDTreeToggle<CR>
noremap <silent> <Leader>nf :NERDTreeFind<CR>

"-------------------------------------------------------------------------------
" vim-latex
"-------------------------------------------------------------------------------
set shellslash
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

" convenient macro-ing
nnoremap Q @q

" Quickfix
nnoremap ]q :cnext<CR>
nnoremap [q :cprev<CR>
nnoremap ]l :lnext<CR>
nnoremap [l :lprev<CR>

" Buffer stuffs
nnoremap ]b :bnext<CR>
nnoremap [b :bprev<CR>

" Toggle paste mode on and off
noremap <C-p> :setlocal paste!<cr>
inoremap <C-p> :setlocal paste!<cr>

" Fix syntax highlighting by refreshing it
noremap <silent> <Leader>hh :syntax sync fromstart<CR>

" Zoom
function! s:zoom()
    if winnr('$') > 1
        tab split
    elseif len(filter(map(range(tabpagenr('$')), 'tabpagebuflist(v:val + 1)'),
                \ 'index(v:val, '.bufnr('').') >= 0')) > 1
        tabclose
    endif
endfunction
noremap <silent> <leader>zz :call <sid>zoom()<cr>

"-------------------------------------------------------------------------------
" Buffers
"-------------------------------------------------------------------------------
noremap <silent> <Leader>bb :Buffers<CR>
noremap <silent> <Leader>be :bufdo execute<CR>

"-------------------------------------------------------------------------------
" Fuzzy stuffs
"-------------------------------------------------------------------------------
noremap <silent> <Leader>ff :Files<CR>
noremap <silent> ,ff :Files <C-R><C-W><CR>
noremap <silent> <Leader>gf :GFiles<CR>
noremap <silent> ,gf :GFiles <C-R><C-W><CR>

"-------------------------------------------------------------------------------
" Git
"-------------------------------------------------------------------------------
noremap <silent> <Leader>gs :Git<CR>
noremap <silent> <Leader>gb :Gblame<CR>
