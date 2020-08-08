runtime! archlinux.vim

"use system clipboard
set clipboard+=unnamedplus

" Vertically center document when entering the insert mode
autocmd InsertEnter * norm zz

"Plugins
call plug#begin('~/vim/plugged')

" Visual stuff
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'dkasak/gruvbox'
Plug 'vim-airline/vim-airline'
" Miscellanous
Plug 'dhruvasagar/vim-table-mode'
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
