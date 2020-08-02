" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages.
runtime! archlinux.vim

" use system clipboard
set clipboard+=unnamedplus

" Vertically center document when entering the insert mode
autocmd InsertEnter * norm zz

" Plugins
" Documentation https://github.com/junegunn/vim-plug/blob/master/README.md
" install with :PluginInstall
" update with :PlugUpgrade
call plug#begin('~/vim/plugged')

" Visual stuff
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'dkasak/gruvbox'
Plug 'vim-airline/vim-airline'
" Center text
Plug 'junegunn/goyo.vim'
" Code completetion
Plug 'neoclide/coc.nvim',{'branch': 'release'}
" Working with tags
Plug 'alvan/vim-closetag'
Plug 'tpope/vim-surround'
" Syntax highlighting
Plug 'neovimhaskell/haskell-vim'
Plug 'yuezk/vim-js'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'rust-lang/rust.vim'
Plug 'vim-pandoc/vim-pandoc-syntax'
" Miscellanous
Plug 'dhruvasagar/vim-table-mode'
call plug#end()

" Theme and airline
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

" Table mode
let g:table_mode_delete_row_map = "<leader>tdr"


" ------Vim Auto Closetag------
" filenames like *.xml, *.html, *.xhtml, ...
" These are the file extensions where this plugin is enabled.
let g:closetag_filenames = '*.html,*.xhtml,*.jsx,*.js,*.tsx'

" filenames like *.xml, *.xhtml, ...
" This will make the list of non-closing tags self-closing in the specified files.
let g:closetag_xhtml_filenames = '*.xml,*.xhtml,*.jsx,*.js,*.tsx'

" filetypes like xml, html, xhtml, ...
" These are the file types where this plugin is enabled.
let g:closetag_filetypes = 'html,xhtml,jsx,js,tsx'

" filetypes like xml, xhtml, ...
" This will make the list of non-closing tags self-closing in the specified files.
let g:closetag_xhtml_filetypes = 'xml,xhtml,jsx,js,tsx'

" integer value [0|1]
" This will make the list of non-closing tags case-sensitive (e.g. `<Link>` will be closed while `<link>` won't.)
let g:closetag_emptyTags_caseSensitive = 1

" Disables auto-close if not in a "valid" region (based on filetype)
let g:closetag_regions = {
    \ 'typescript.tsx': 'jsxRegion,tsxRegion',
    \ 'javascript.jsx': 'jsxRegion',
    \ }

" Shortcut for closing tags, default is '>'
let g:closetag_shortcut = '>'

" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'

" Haskell
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
" let g:haskell_classic_highlighting = 1   " to disable opinionated highlighting

" highlited line
set cursorline
set cursorcolumn

" j/k will navigate wrapped lines as normal lines
noremap <silent> <expr> j (v:count == 0 ? 'gj' : j)
noremap <silent> <expr> k (v:count == 0 ? 'gk' : k)

" numbered lines
set number

" Enable syntax highlighting
syntax on
filetype plugin indent on

" mouse scrolling
set mouse=a

" case insensitive search
set ignorecase

" miscellaneous
set wrap
set paste

" Goyo
noremap <leader>g :Goyo<CR>
