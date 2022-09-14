let g:mapleader = ","
set fileencodings=utf-8,gb2312,gb18030,gbk,ucs-bom,cp936,latin1


filetype plugin indent on                                   " enable filetype dectection and ft specific plugin/indent
syntax on                                                   " enable syntax hightlight and completion

"--------
" Vim UI
"--------
set background=dark
" set background=light

" highlight current line
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline cursorcolumn
set cursorline cursorcolumn
highlight clear SignColumn                                  " SignColumn should match background
highlight clear LineNr                                      " Current line number row will have same background color in relative mode

" search
set incsearch
set hlsearch 
"set highlight 	                                            " conflict with highlight current line
set ignorecase
set smartcase
set wildmenu " Show list instead of just completing
set wildmode=list:longest,full " Command <Tab> completion, list matches, then longest common part, then all.
set listchars=tab:›\ ,trail:•,extends:#,nbsp:.              " Highlight problematic whitespace

"set cmdheight=2							" 
"set laststatus=2
"
"

" editor settings
"
set nocompatible
set history=1000
set nofoldenable                                              " disable folding"
set confirm                                                   " prompt when existing from an unsaved file
set backspace=indent,eol,start                                " More powerful backspacing
set linespace=0							                      " no extra spaces between rows
set t_Co=256                                                  " Explicitly tell vim that the terminal has 256 colors "
set term=screen-256color
set mouse=a                                                   " use mouse in all modes
set report=0                                                  " always report number of lines changed                "
set nowrap                                                    " dont wrap lines
set scrolloff=5                                               " 5 lines above/below cursor when scrolling
set number                                                    " show line numbers
set showmatch                                                 " show matching bracket (briefly jump)
set showcmd                                                   " show typed command in status bar
set title                                                     " show file in titlebar
set matchtime=2                                               " show matching bracket for 0.2 seconds
set matchpairs+=<:>                                           " specially for html
set relativenumber
set autoread
set autowrite                                                 " 切换buffer时自动write  
set t_ti= t_te=                                               " 退出vim后，内容显示在终端屏幕，方便复制
set magic                                                     " 正着表达式匹配形式
set autochdir
set iskeyword+=_,$,@,%,#,-                                    " 带有如下符号的单词不要被换行切割
set path+=/usr/include                                        " gf 
set clipboard+=unnamed                                        " 共享剪切板
"
"------------tab/buffer------------------
" Default Indentation
set autoindent
set smartindent                                               " indent when
set tabstop=4                                                 " tab width
set softtabstop=4                                             " backspace
set shiftwidth=4                                              " indent width
" set textwidth=79
" set smarttab
set expandtab                                                 " expand tab to space
set showmode 							                      " Display the current mode
"noremap <left> :bp<CR>
"noremap <right> :bn<CR>

map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove

" ---------------netrw-------------------
" use the previous window to open file
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 20

" ------------------FileType------------------------
autocmd FileType php setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120
autocmd FileType ruby setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120
autocmd FileType php setlocal tabstop=4 shiftwidth=4 softtabstop=4 textwidth=120
autocmd FileType coffee,javascript setlocal tabstop=2 shiftwidth=2 softtabstop=2 textwidth=120
autocmd FileType python setlocal tabstop=4 shiftwidth=4 softtabstop=4 textwidth=120

"-----------------
" Plugin settings
"-----------------
"