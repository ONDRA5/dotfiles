runtime! archlinux.vim
source ~/.config/nvim/plugconfig/coc.vim

"use system clipboard
set clipboard+=unnamedplus

" Vertically center document when entering the insert mode
autocmd InsertEnter * norm zz

" Fix splits
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

"Plugins
call plug#begin('~/vim/plugged')

" Visual stuff
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'dkasak/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'neovimhaskell/haskell-vim'
" Miscellanous
Plug 'dhruvasagar/vim-table-mode'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

" nvim settings
	" Numbered lines
	set number
	" Cursor lines
	set cursorline
	set cursorcolumn
	" Enable syntax highlighting
	syntax on
	" Mouse support
	set mouse=a
	" Case insensitive search
	set ignorecase
	" j/k will go through wrapped lines as if they were normal lines
	noremap <silent> <expr> j (v:count == 0 ? 'gj' : j)
	noremap <silent> <expr> k (v:count == 0 ? 'gk' : k)
	" TAB will switch tabs
	nnoremap <silent> <TAB> :tabn<CR>
	" SHIFT-TAB will go back
	nnoremap <silent> <S-TAB> :tabp<CR>
	" Ctr-t for new tab
	noremap <silent> <C-t> :tabnew<CR>
	" Miscellaneous
	set wrap
	set paste

	
" Theme
colorscheme gruvbox
let g:airline_theme = 'gruvbox'
let g:airline#extensions#wordcount#enabled = 1
let g:airline#extensions#hunks#non_zero_only = 1
let g:airline_powerline_fonts = 1

" Hexokinase configuration (the thing that shows color codes a colors
set termguicolors
let g:Hexokinase_refreshEvents = ['InsertLeave']
let g:Hexokinase_optInPatterns = [
\     'full_hex',
\     'triple_hex',
\     'rgb',
\     'rgba',
\     'hsl',
\     'hsla',
\     'colour_names'
\ ]
let g:Hexokinase_highlighters = ['backgroundfull']
 " Reenable hexokinase on enter
 autocmd VimEnter * HexokinaseTurnOn

" Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
" let g:haskell_classic_highlighting = 1   " to disable opinionated highlighting
