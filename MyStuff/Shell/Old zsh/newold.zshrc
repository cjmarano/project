export PATH=/Users/charles.marano/.cache/emacs/:/Users/charles.marano/.cache/emacs/tree-sitter/:/Users/charles.marano/tree-sitter/:/opt/homebrew/opt/ruby/bin/:/opt/homebrew/lib/ruby/gems/3.3.6/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/bin/:/Users/charles.marano/.pyenv/shims/:/Users/charles.marano/.pyenv/shims/pip/:/Users/charles.marano/.rbenv/shims/:/Users/charles.marano/.cargo/bin/:/Users/charles.marano/.local/bin/:/opt/homebrew/bin/:/opt/homebrew/sbin/:/usr/local/jamf/bin/:/usr/local/sbin/:/usr/local/bin/:/usr/sbin/:/usr/bin/:/bin/:/sbin/:/Library/Apple/usr/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/lib/python3.13/site-packages/:/opt/homebrew/opt/python-lsp-server/bin/:/Users/charles.marano/.pyenv/versions/3.13.1/lib/python3.13/site-packages/:/Users.charles.marano/.rbenv/bin/:/users/charles.marano/.pyenv/bin/

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

export EDITOR="emacsclient -c -a emacs"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

if [[ -f "/opt/homebrew/bin/brew" ]] then
  # If you're using macOS, you'll want this enabled
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

zstyle ':omz:update' mode auto      # update automatically without asking

# Uncomment the following line if pasting URLs and other text is messed up.
DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting) 

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Example aliases
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

source $(dirname $(gem which colorls))/tab_complete.sh
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
source /opt/homebrew/opt/chruby/share/chruby/auto.sh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export ZSH_HIGHLIGHT_HIGHLIGHTERS_DIR="/opt/homebrew/share/zsh-syntax-highlighting/highlighters"

# pyenv config
export PYENV_ROOT="$HOME/.pyenv"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

ZSH_DISABLE_COMPFIX="true"

Default_USER=$(whoami)

# User configuration

FPATH=~/.rbenv/completions:"$FPATH"

autoload -U compinit
compinit

# Turn off all beeps
unsetopt BEEP
# Turn off autocomplete beeps
# unsetopt LIST_BEEP

source ~/powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /Users/charles.marano/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# export FZF_DEFAULT_OPTS=' -- height=40% -- preview="bat -- color=always {}" -- preview-window=right:60%:wrap'
