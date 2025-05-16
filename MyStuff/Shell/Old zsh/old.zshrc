export PATH=/home/plucky/:/home/linuxbrew/.linuxbrew/Cellar/oh-my-posh/24.1.0/bin/:/home/linuxbrew/.linuxbrew/bin/oh-my-posh/:/home/plucky/.local/kitty.app/bin/kitty:/home/plucky/.local/kitty.app/bin/kitten/:/home/linuxbrew/.linuxbrew/share:/home/plucky/.rbenv/shims/ruby:/home/linux/tree-sitter/:/Users/plucky/.pyenv/shims/:/Users/plucky/.rbenv/shims:/home/linuxbrew/.linuxbrew/bin/cargo:/Users/plucky/.local/bin:/Users/plucky/.pyenv/versions/3.13.0/bin/:/home/linuxbrew/.linuxbrew/bin/:/home/linuxbrew/.linuxbrew/sbin/:/home/linuxbrew/.linuxbrew/lib/ruby/gems/3.4.0/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin:/sbin/:/snap/bin/:/home/plucky/.cargo/bin/

# if this is macOS then add line below
# eval "$(/opt/homebrew/bin/brew shellenv)"

ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

if [ ! -d "$ZINIT_HOME" ]; then
    mkdir -p "$(dirname $ZINIT_HOME)"
    git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

source "${ZINIT_HOME}/zinit.zsh"

set rtp+=/opt/homebrew/opt/fzf

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

zinit snippet OMZP::command-not-found

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'

# eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"

alias myip="curl http://ipecho.net/plain; echo"
alias la='colorls -all'
alias lb='cd ..'
alias lc='colorls -lA --sd'
alias ld='colorls -ltr'
alias lf='colorls -lath'
alias lg='colorls --gs'
alias lh='colorls'
alias ll='colorls -loa --sd'
alias lt='colorls --tree'
alias lk='batcat'

eval "$(oh-my-posh init zsh --config ~/.config/ohmyposh/p10k.toml)"

# ----------------------------------------------------------

FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

# pyenv config
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

ZSH_DISABLE_COMPFIX="true"

Default_USER=$(whoami)

# User configuration

eval "$(rbenv init - zsh)"

FPATH=~/.rbenv/completions:"$FPATH"

export MANPATH="/usr/local/share/man/:usr/share/man:$MANPATH"

export XDG_DATA_DIRS="/home/linuxbrew/.linuxbrew/share:$XDG_DATA_DIRS"

fpath+=${ZDOTDIR:-~}/.zsh_functions

autoload -U compinit
compinit

zinit cdreplay -q

source $(dirname $(gem which colorls))/tab_complete.sh
