[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.miniplug.zsh ] && source ~/.miniplug.zsh

# autoload -Uz cominit
# miniplug plugin 'Aloxaf/fzf-tab'

# plugin
miniplug plugin 'zsh-users/zsh-syntax-highlighting'
miniplug plugin 'zsh-users/zsh-autosuggestions'
miniplug plugin 'zsh-users/zsh-completions'

# theme
# miniplug plugin 'sindresorhus/pure'
miniplug load

# pure theme
fpath+=($HOME/.local/share/miniplug/sindresorhus/pure)
autoload -U promptinit; promptinit
prompt pure
zstyle :prompt:pure:git:stash show yes

