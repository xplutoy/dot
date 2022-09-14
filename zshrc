# 别名
alias ls='ls --color=auto'
alias ll='ls -la'
alias vim='nvim'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# 插件管理器
[ -f ~/.miniplug.zsh ] && source ~/.miniplug.zsh

# plugin
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'

# theme
# miniplug plugin 'sindresorhus/pure'
miniplug load

# 插件配置
# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
zstyle :prompt:pure:git:stash show yes
