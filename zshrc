# env variable settings
export ZDOTDIR=$HOME                        # default:$HOME
export EDITOR='nvim'
export VISUAL='nvim'
export fpath=($ZDOTDIR/.zsh $fpath)         # add own functions folder to fpath 

export YX_DOTDIR=$HOME/dot
export YX_ORGDIR=$HOME/org

# line edit
bindkey -e
export KEYTIMEOUT=1

# history settings
if [ -z $HISTFILE ]; then
    HISTFILE=$HOME/.zsh_history
fi
export HISTSIZE=10000                       # Maximum events for internal history
export SAVEHIST=10000                       # Maximum events in history file
setopt INC_APPEND_HISTORY                   # Write to the history file immediately,
setopt SHARE_HISTORY                        # Share history between all sessions.
setopt APPEND_HISTORY                       # append to history file
setopt HIST_SAVE_NO_DUPS                    # Do not write a duplicate event to the history file.

# dir stack
setopt AUTO_PUSHD                           # Push the current directory visited on the stack.
setopt PUSHD_IGNORE_DUPS                    # Do not store duplicates in the stack.
setopt PUSHD_SILENT                         # Do not print the directory stack after pushd or popd

# 别名
alias :q=exit
alias ..='cd ..'
alias ls='ls --color=auto'
alias ll='ls -la'
alias grep='grep --color=auto'
alias vim='nvim'
alias em='emacs --no-desktop -nw "$@"'
alias ec='TERM=xterm-256color emacsclient -s server -t "$@"'


# alias for git
alias ga='git add'
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gc='git commit -v'
alias gca='git commit -v --amend'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --staged'
alias gs='git status -sb'
alias gstl='git stash list'
alias gsta='git stash push'
alias gstaa='git stash apply'
alias gstd='git stash drop'
alias gstp='git stash pop'
alias gstc='git stash clear'

autoload -U colors && colors
autoload -U compinit; compinit

unsetopt MENU_COMPLETE
setopt AUTO_MENU
setopt ALWAYS_TO_END
setopt COMPLETE_IN_WORD
setopt NOBEEP
setopt CORRECT_ALL

zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$ZDOTDIR/.zcompcache"
zstyle ':completion:*' menu select
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' file-sort change

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# 插件管理器
[ -f $YX_DOTDIR/miniplug/miniplug.zsh ] && source $YX_DOTDIR/miniplug/miniplug.zsh
# plugin managed
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug plugin 'agkozak/zsh-z'
miniplug plugin 'Tarrasch/zsh-bd'
miniplug plugin 'sindresorhus/pure'
miniplug load

# 插件配置
# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
zstyle :prompt:pure:git:stash show yes

# emacs vterm shell side configuration
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi
