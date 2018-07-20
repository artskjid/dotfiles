call plug#begin('~/.local/share/nvim/plugged')
Plug 'jremmen/vim-ripgrep'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'vim-airline/vim-airline'
Plug 'KabbAmine/yowish.vim'
call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General UI
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set number
set cursorline
colorscheme yowish
set background=dark
let g:airline#extensions#tabline#enabled = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General Keys
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader=","
nnoremap <F1> :e $MYVIMRC<CR>
nnoremap <Left> :bprevious<CR>
nnoremap <Right> :bnext<CR>
nnoremap <C-o> :<C-u>FZF<CR>
nnoremap <F2> :grep<Space>
nnoremap <F10> :Redir :!git blame %<CR>
autocmd FileType json nnoremap <F8> :%!jq .<CR>

map <C-n> :cnext<CR>zv
map <C-p> :cprevious<CR>zv<Paste>
nnoremap <leader>a :cclose<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Others
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set grepprg=rg\ --vimgrep
set grepformat=%f:%l:%c:%m

" redirect cmd output to a scratch buffer
function! Redir(cmd)
	for win in range(1, winnr('$'))
		if getwinvar(win, 'scratch')
			execute win . 'windo close'
		endif
	endfor
	if a:cmd =~ '^!'
		execute "let output = system('" . substitute(a:cmd, '^!', '', '') . "')"
	else
		redir => output
		execute a:cmd
		redir END
	endif
	vnew
	let w:scratch = 1
	setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile
	call setline(1, split(output, "\n"))
endfunction

command! -nargs=1 Redir silent call Redir(<f-args>)
