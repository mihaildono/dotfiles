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

# Aliases
# directory
alias ..="cd .."
alias l="ls -la"

# git
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

# General settings
COMPLETION_WAITING_DOTS="true"     # Display red dots whilst waiting for completion.
DISABLE_MAGIC_FUNCTIONS="true"     # No auto insert backslashes
ZSH_AUTOSUGGEST_MANUAL_REBIND=1    # Performance boost
HIST_STAMPS="%d/%m/%y %T"          # Show date of command
DISABLE_UNTRACKED_FILES_DIRTY=true # Not show untracked files

# persist history
HISTSIZE=50000              # How many lines of history to keep in memory
HISTFILE=~/.zsh_history     # Where to save history to disk
SAVEHIST=50000              # Number of history entries to save to disk
setopt appendhistory        # Append history to the history file (no overwriting)
setopt sharehistory         # Share history across terminals
setopt incappendhistory     # Immediately append to the history file, not just when a term is killed

plugins=(
    zsh-syntax-highlighting
    zsh-autosuggestions
    autojump
    last-working-dir
    command-not-found
    zsh-nvm
)

# Plugin settings
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=3" # change color of suggestion
export NVM_LAZY_LOAD=true
export NVM_COMPLETION=true

# add bin to path (add additional commands to PATH in ~/bin folder)
# export PATH="/home/$USER/bin:$PATH"
# This is for MacOS
export PATH="/usr/local/sbin:$PATH"

# Custom
# Match names regardless of capitalization, but try to match exact first
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'

# Bottom line settings
source $ZSH/oh-my-zsh.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
