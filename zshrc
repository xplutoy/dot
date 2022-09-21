# env variable settings
export ZDOTDIR=$HOME                        # default:$HOME
export EDITOR='nvim'
export VISUAL='nvim'
export fpath=($ZDOTDIR/.zsh $fpath)         # add own functions folder to fpath 

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

# 补全
autoload -U compinit; compinit
unsetopt MENU_COMPLETE
setopt AUTO_MENU
setopt ALWAYS_TO_END
setopt COMPLETE_IN_WORD

# 路径


# misc
setopt CORRECT_ALL
#
# zstyle
zstyle :prompt:pure:git:stash show yes

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 插件管理器
[ -f ~/.miniplug.zsh ] && source ~/.miniplug.zsh

# plugin
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug plugin 'agkozak/zsh-z'
miniplug plugin 'Tarrasch/zsh-bd'

# theme
# miniplug plugin 'sindresorhus/pure'
miniplug load

# 插件配置
# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
