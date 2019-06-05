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

plugins=(
    command-not-found
    zsh-autosuggestions
    last-working-dir
)

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
