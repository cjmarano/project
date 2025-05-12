# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export PATH=/home/plucky/:/home/linuxbrew/.linuxbrew/Cellar/oh-my-posh/24.1.0/bin/:/home/linuxbrew/.linuxbrew/bin/oh-my-posh/:/home/plucky/.local/kitty.app/bin/kitty:/home/plucky/.local/kitty.app/bin/kitten/:/home/linuxbrew/.linuxbrew/share:/home/plucky/.rbenv/shims/ruby:/home/linux/tree-sitter/:/Users/plucky/.pyenv/shims/:/Users/plucky/.rbenv/shims:/home/linuxbrew/.linuxbrew/bin/cargo:/Users/plucky/.local/bin:/Users/plucky/.pyenv/versions/3.13.0/bin/:/home/linuxbrew/.linuxbrew/bin/:/home/linuxbrew/.linuxbrew/sbin/:/home/linuxbrew/.linuxbrew/lib/ruby/gems/3.4.0/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin:/sbin/:/snap/bin/:/home/plucky/.cargo/bin/

# if this is macOS then add line below
# eval "$(/opt/homebrew/bin/brew shellenv)"

ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

if [ ! -d "$ZINIT_HOME" ]; then
    mkdir -p "$(dirname $ZINIT_HOME)"
    git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
fi

source "${ZINIT_HOME}/zinit.zsh"

zinit ice depth=1; zinit light romkatv/powerlevel10k

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

autoload -U compinit && compinit

zinit cdreplay -q

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

HISTSIZE=1000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls --color $realpath'


alias ls='ls --color'
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

# not an fzf option
# eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"
