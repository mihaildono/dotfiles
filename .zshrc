# important variable for OMZ
export ZSH="$HOME/.oh-my-zsh"

# this is overriden by pure prompt
ZSH_THEME="robbyrussell"
fpath=( "$HOME/.zfunctions" $fpath )

# Display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="dd.mm.yyyy"

# Match names regardless of capitalization, but try to match exact first
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

plugins=(
    command-not-found
    zsh-autosuggestions
    last-working-dir
)

alias ..="cd .."
alias l="ls -la"

alias gbr="git branch"
alias g="git"
alias gs="git status -s"
alias gd="git diff"
alias gps="git push"
alias gp="git pull"
alias gcm="git commit"
alias ga="git add"
alias gco="git checkout"
alias gcl="git clone"
alias gl="git log --pretty=format:'%C(yellow)%h %Cred%ar %Cblue%an%Cgreen%d %Creset%s' --date=short"
alias grbi="git rebase -i"
alias grb="git rebase"

# add jump
eval "$(jump shell zsh)"

# add autosuggestions plugin
source ~/.oh-my-zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=3"

# init pure prompt
autoload -U promptinit; promptinit
prompt pure

# add ruby rbenv path vars
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# nvm initialization
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# place this after nvm initialization!
# calls nvm use automatically if it encounters a .nvmrc file and sets the correct version
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc
