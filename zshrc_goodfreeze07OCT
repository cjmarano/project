export PATH=/Users/charles.marano/.cache/emacs/:/Users/charles.marano/.cache/emacs/tree-sitter/:/Users/charles.marano/tree-sitter/:/opt/homebrew/opt/ruby/bin/:/opt/homebrew/lib/ruby/gems/3.4.0/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/bin/:/Users/charles.marano/.pyenv/shims/:/Users/charles.marano/.pyenv/shims/pip/:/Users/charles.marano/.rbenv/shims/:/Users/charles.marano/.cargo/bin/:/Users/charles.marano/.local/bin/:/opt/homebrew/bin/:/opt/homebrew/sbin/:/usr/local/jamf/bin/:/usr/local/sbin/:/usr/local/bin/:/usr/sbin/:/usr/bin/:/bin/:/sbin/:/Library/Apple/usr/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/lib/python3.13/site-packages/:/opt/homebrew/opt/python-lsp-server/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/lib/python3.13/site-packages/:/users/charles.marano/.pyenv/bin/

# if this is macOS then add line below
eval "$(/opt/homebrew/bin/brew shellenv)"

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
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'ls --color $realpath'

# alias ls='ls --color'
# alias la='colorls -all'
alias lb='cd ..'
# alias lc='colorls -lA --sd'
# alias ld='colorls -ltr'
alias lf='colorls -lath'
alias lg='colorls --gs'
# alias lh='colorls'
alias ll='colorls -loa --sd'
# alias lt='colorls --tree'
alias lz='eza -la  --time=modified --color=always --icons=always'
alias ly='eza -lha --color=always --icons=always --group-directories-first'
alias lx='eza -la --time=created --color=always --icons=always --group-directories-first'
alias myip="curl http://ipecho.net/plain; echo"
alias bu='emacs -q -l ~/project/Prot/init.el &'
alias ec='emacsclient -n $1'
alias ez="emacsclient --create-frame $1"
# pyenv config
export PYENV_ROOT="$HOME/.pyenv"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

FPATH=~/.rbenv/completions:"$FPATH"

export EDITOR=emacs

# Turn off all beeps
unsetopt BEEP
# Turn off autocomplete beeps
# unsetopt LIST_BEEP

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
 source "$GHOSTTY_RESOURCES_DIR"/shell-integration/zsh/ghostty-integration
fi

# Shell Integration.
eval "$(fzf --zsh)"
eval "$(zoxide init --cmd cd zsh)"

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}
