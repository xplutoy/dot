# env variable settings
export ZDOTDIR=$HOME                        # default:$HOME
export fpath=($ZDOTDIR/.zsh $fpath)         # add own functions folder to fpath 

# history settings
setopt INC_APPEND_HISTORY                   # Write to the history file immediately,
setopt SHARE_HISTORY                        # Share history between all sessions.
setopt APPEND_HISTORY                       # append to history file
setopt HIST_SAVE_NO_DUPS                    # Do not write a duplicate event to the history file.

# 别名
alias :q=exit
alias ..='cd ..'
alias ls='ls --color=auto'
alias ll='ls -la'
alias grep='grep --color=auto'
alias vim='nvim'
#
# zstyle
zstyle ':completion:*' menu select
zstyle :prompt:pure:git:stash show yes

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 插件管理器
[ -f ~/.miniplug.zsh ] && source ~/.miniplug.zsh

# plugin
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'
miniplug plugin 'agkozak/zsh-z'

# theme
# miniplug plugin 'sindresorhus/pure'
miniplug load

# 插件配置
# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
