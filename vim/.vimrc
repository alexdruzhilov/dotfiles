" Environment {

    " Basics {
        set nocompatible        " Must be first line
    " }

    " Setup Bundle Support {
        " The next three lines ensure that the ~/.vim/bundle/ system works
        filetype off
        set rtp+=~/.vim/bundle/Vundle.vim
        call vundle#rc()
    " }

" }

" Bundles {

    " Deps {
        Plugin 'gmarik/vundle'
    " }

    " General {
        Plugin 'flazz/vim-colorschemes'
        Plugin 'altercation/vim-colors-solarized'
        Plugin 'scrooloose/nerdtree'
        Plugin 'jistr/vim-nerdtree-tabs'
        Plugin 'bling/vim-airline'
        Plugin 'ctrlpvim/ctrlp.vim'              " Navigation by partial file name
        Plugin 'gcmt/wildfire.vim'               " Quick select closest text object by hitting <ENTER>
        Plugin 'mhinz/vim-signify'               " Shows VCS diff in sign column
        Plugin 'osyo-manga/vim-over'             " Substitute preview
        Plugin 'mhinz/vim-startify'              " Great start screen with navigation
        Plugin 'Lokaltog/vim-easymotion'         " Text navigation
        Plugin 'mileszs/ack.vim'                 " Search tool
        Plugin 'benmills/vimux'                  " Easily interact with tmux from vim
    " }

    " Programming {
        Plugin 'tpope/vim-fugitive'
        Plugin 'scrooloose/syntastic'
        Plugin 'majutsushi/tagbar'
        Plugin 'tomtom/tcomment_vim'
    " }

    " Snippets & AutoComplete {
        Plugin 'Valloric/YouCompleteMe'
        Plugin 'SirVer/ultisnips'
        Plugin 'honza/vim-snippets'
    " }

    " Erlang {
        Plugin 'jimenezrick/vimerl.git'
    " }

    " Javascript {
        " Plugin 'elzr/vim-json'                  " Better JSON support
        Plugin 'pangloss/vim-javascript'        " Improved Javascript indentation and syntax support
        Plugin 'kchmck/vim-coffee-script'       " Coffee script support
    " }
" }

" General {
    set background=dark         " Assume a dark background
    filetype plugin indent on   " Automatically detect file types
    syntax on                   " Syntax highlighting
    set mouse=a                 " Automatically enable mouse usage
    set mousehide               " Hide the mouse cursor while typing
    scriptencoding utf-8

    set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
    set clipboard=unnamed
    set shortmess+=filmnrxoOtT          " Abbrev. of messages (avoids 'hit enter')
    set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
    set virtualedit=onemore             " Allow for cursor beyond last character
    set history=1000                    " Store a ton of history (default is 20)
    set nospell                         " Spell checking off
    set hidden                          " Allow buffer switching without saving
" }

" UI {
    set t_Co=256
    colorscheme solarized
    highlight Normal ctermbg=NONE
    highlight nonText ctermbg=NONE
    highlight clear SignColumn      " SignColumn should match background
    highlight clear LineNr          " Current line number row will have same background color in relative mode

    set tabpagemax=15               " Only show 15 tabs
    set showmode                    " Display the current mode
    set cursorline                  " Highlight current line
    set ruler                       " Show the ruler
    set laststatus=2                " Always show statusline
    set backspace=indent,eol,start  " Backspace for dummies
    set linespace=0                 " No extra spaces between rows
    set nu                          " Line numbers on
    set showmatch                   " Show matching brackets/parenthesis
    set incsearch                   " Find as you type search
    set hlsearch                    " Highlight search terms
    set winminheight=0              " Windows can be 0 line high
    set ignorecase                  " Case insensitive search
    set smartcase                   " Case sensitive when uc present
    "set wildmenu                    " Show list instead of just completing
    "set wildmode=list:longest,full  " Command <Tab> completion, list matches, then longest common part, then all.
    "set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
    set scrolljump=5                " Lines to scroll when cursor leaves screen
    set scrolloff=3                 " Minimum lines to keep above and below cursor
    set nofoldenable                " Disable auto folding code
    set list
    set listchars=tab:›\ ,trail:•,extends:#,nbsp:. " Highlight problematic whitespace
" }

" Formatting {
    set nowrap                      " Do not wrap long lines
    set autoindent                  " Indent at the same level of the previous line
    set shiftwidth=4                " Use indents of 4 spaces
    set expandtab                   " Tabs are spaces, not tabs
    set tabstop=4                   " An indentation every four columns
    set softtabstop=4               " Let backspace delete indent
    set nojoinspaces                " Prevents inserting two spaces after punctuation on a join (J)
    set splitright                  " Puts new vsplit windows to the right of the current
    set splitbelow                  " Puts new split windows to the bottom of the current

    " autocmd FileType c,cpp,java,go,php,javascript,puppet,python,rust,twig,xml,yml,perl,sql,erlang autocmd BufWritePre <buffer> call StripTrailingWhitespace()
    " autocmd FileType haskell,puppet,ruby,yml setlocal expandtab shiftwidth=2 softtabstop=2
" }

" Key (re)Mappings {
    let mapleader=','

    " Easier moving in tabs and windows
    map <C-J> <C-W>j<C-W>_
    map <C-K> <C-W>k<C-W>_
    map <C-L> <C-W>l<C-W>_
    map <C-H> <C-W>h<C-W>_

    nmap <silent> <leader>/ :nohlsearch<CR>

    " Visual shifting (does not exit Visual mode)
    vnoremap < <gv
    vnoremap > >gv

    " Allow using the repeat operator with a visual selection (!)
    " http://stackoverflow.com/a/8064607/127816
    vnoremap . :normal .<CR>

    " For when you forget to sudo.. Really Write the file.
    cmap w!! w !sudo tee % >/dev/null

    " Adjust viewports to the same size
    map <Leader>= <C-w>=

    " Map <Leader>ff to display all lines with keyword under cursor
    " and ask which one to jump to
    nmap <Leader>ff [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
" }

" Plugins {

    " NerdTree {
        map <C-e> <plug>NERDTreeTabsToggle<CR>
        map <leader>e :NERDTreeFind<CR>j

        let NERDTreeShowBookmarks=1
        let NERDTreeIgnore=['^\.git$']
        let NERDTreeChDirMode=2
        let NERDTreeMouseMode=2
        let NERDTreeShowHidden=1
    " }

    " vimerl {
        let erlang_skel_header = {"author": "Aleksandr Druzhilov", "owner" : "adruzhilov@unison.com", "year"  : "2015"}
    " }

    " vim-airline {
        let g:airline_theme = 'solarized'
        let g:airline_left_sep='›'  " Slightly fancier than '>'
        let g:airline_right_sep='‹' " Slightly fancier than '<'
    " }

    " ctrlp {
        let g:ctrlp_working_path_mode = 'raw'
        let g:ctrlp_custom_ignore = {
                        \ 'dir':  '\.git$\|\.hg$\|\.svn$\|test_rel\|lib',
                        \ 'file': 'rebar$\|\.beam$' }
    " }

    " fugitive {
        nnoremap <silent> <leader>gs :Gstatus<CR>
        nnoremap <silent> <leader>gd :Gdiff<CR>
        nnoremap <silent> <leader>gc :Gcommit<CR>
        nnoremap <silent> <leader>gb :Gblame<CR>
        nnoremap <silent> <leader>gl :Glog<CR>
        nnoremap <silent> <leader>gp :Git push<CR>
        nnoremap <silent> <leader>gr :Gread<CR>
        nnoremap <silent> <leader>gw :Gwrite<CR>
        nnoremap <silent> <leader>ge :Gedit<CR>
        " Mnemonic _i_nteractive
        nnoremap <silent> <leader>gi :Git add -p %<CR>
        nnoremap <silent> <leader>gg :SignifyToggle<CR>
    " }

    " tagbar {
        nnoremap <silent> <leader>tt :TagbarToggle<CR>
    " }

    " sessionman {
        nmap <leader>sl :SessionList<CR>
        nmap <leader>ss :SessionSave<CR>
        nmap <leader>sc :SessionClose<CR>
    " }

    " startify {
        nmap <leader>sl :SLoad<CR>
        nmap <leader>ss :SSave<CR>
        nmap <leader>sd :SDelete<CR>
        nmap <leader>sc :SClose<CR>
        let g:startify_list_order = [
            \ ['    Sessions'],
            \ 'sessions',
            \ ['    Most recently used files'],
            \ 'files',
            \ ['    Most recently used files in current directory'],
            \ 'dir',
            \ ['    Bookmarks'],
            \ 'bookmarks']
        let g:startify_bookmarks = [ '~/.vimrc' ]
        let g:startify_enable_special = 0
        let g:startify_custom_header = [
            \'            ██╗   ██╗███╗   ██╗██╗███████╗ ██████╗ ███╗   ██╗',
            \'            ██║   ██║████╗  ██║██║██╔════╝██╔═══██╗████╗  ██║',
            \'            ██║   ██║██╔██╗ ██║██║███████╗██║   ██║██╔██╗ ██║',
            \'            ██║   ██║██║╚██╗██║██║╚════██║██║   ██║██║╚██╗██║',
            \'            ╚██████╔╝██║ ╚████║██║███████║╚██████╔╝██║ ╚████║',
            \'             ╚═════╝ ╚═╝  ╚═══╝╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═══╝'
            \]
        let g:startify_session_persistence = 1
    " }

    " Snippets {
        let g:UltiSnipsSnippetsDir = '~/.vim/bundle/vim-snippets/UltiSnips'
    " }

    " YouCompleteMe {
        let g:ycm_collect_identifiers_from_tags_files = 1
        let g:ycm_autoclose_preview_window_after_completion = 1
        let g:ycm_autoclose_preview_window_after_insertion = 1
        let g:ycm_key_list_select_completion = ['<TAB>', '<Down>', '<C-j>']
        let g:ycm_key_list_previous_completion = ['<S-TAB>', '<Up>', '<C-k>']

        " remap Ultisnips for compatibility for YCM
        " let g:UltiSnipsExpandTrigger = '<C-j>'
        " let g:UltiSnipsJumpForwardTrigger = '<C-j>'
        " let g:UltiSnipsJumpBackwardTrigger = '<C-k>'
    " }
" }

