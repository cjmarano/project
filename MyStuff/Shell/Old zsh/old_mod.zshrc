



eval "$(oh-my-posh init zsh --config ~/.config/ohmyposh/p10k.toml)"

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

fpath+=${ZDOTDIR:-~}/.zsh_functions

source $(dirname $(gem which colorls))/tab_complete.sh
