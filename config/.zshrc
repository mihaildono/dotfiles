# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Theme
ZSH_THEME="powerlevel10k/powerlevel10k"

# Variables
export ZSH="$HOME/.oh-my-zsh"
export TERM="xterm-256color" # This sets up colors properly
# add bin to path (add additional commands to PATH in ~/bin folder)
# export PATH="/home/$USER/bin:$PATH"
export PATH="/usr/local/sbin:$PATH" # This is for MacOS

# android
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_SDK_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools

# Aliases
# directory
alias ..="cd .."
alias l="ls -la"

# git
alias gf="git fetch"
alias gfm="git fetch origin main:main"
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
alias gr='git reset $(git merge-base master $(git branch --show-current))'
alias gbrd='git branch | grep -v "main" | grep -v "master" | grep -v "live" | xargs git branch -D
' # delete merged branches


# General settings
COMPLETION_WAITING_DOTS="true"     # Display red dots whilst waiting for completion.
DISABLE_MAGIC_FUNCTIONS="true"     # No auto insert backslashes
ZSH_AUTOSUGGEST_MANUAL_REBIND=1    # Performance boost
HIST_STAMPS="%d/%m/%y %T"          # Show date of command
DISABLE_UNTRACKED_FILES_DIRTY=true # Not show untracked files

plugins=(
    zsh-autosuggestions
    autojump
    last-working-dir
    command-not-found
)

# Custom
# Match names regardless of capitalization, but try to match exact first
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

# NVM config
# export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" ||# printf %s "${XDG_CONFIG_HOME}/nvm")"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

# node version manager
eval "$(fnm env --use-on-cd)"

# Bottom line settings
source $ZSH/oh-my-zsh.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# fnm
export PATH="/Users/mihailgeorgiev/Library/Application Support/fnm:$PATH"
eval "`fnm env`"

# python
eval "$(pyenv init --path)"

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)