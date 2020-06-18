" All system-wide defaults are set in $VIMRUNTIME/archlinux.vim (usually just
" /usr/share/vim/vimfiles/archlinux.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vimrc), since archlinux.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing archlinux.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages.
runtime! archlinux.vim


" Plugins
" Documentation https://github.com/junegunn/vim-plug/blob/master/README.md
" install with :PluginInstall
" update with :PlugUpgrade
call plug#begin('~/vim/plugged')

" Plug 'ap/vim-css-color'
Plug 'neoclide/coc.nvim',{'branch': 'release'}
Plug 'ap/vim-css-color',{'as':'colors'}
Plug 'morhetz/gruvbox'
" Plug 'dracula/vim',{'as':'dracula'}
call plug#end()

colorscheme gruvbox
"let g:dracula_colorterm = 0
" colorscheme dracula

" If you prefer the old-style vim functionalty, add 'runtime! vimrc_example.vim'
" Or better yet, read /usr/share/vim/vim80/vimrc_example.vim or the vim manual
" and configure vim to your own liking!

" do not load defaults if ~/.vimrc is missing
"let skip_defaults_vim=1

" highlited line
set cursorline

" numbered lines
set number

" Enable syntax highlighting
syntax enable

" mouse scrolling
set mouse=a

" case insensitive search
set ignorecase

" miscellaneous
set wrap
set paste
