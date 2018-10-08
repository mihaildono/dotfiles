status --is-interactive; and . (jump shell fish | psub)

set PATH ~/.rbenv/bin ~/.rbenv/shims $PATH

# set -Ux EDITOR emacsclient -c
# set EDITOR emacsclient -c
# set VISUAL emacsclient -c

alias .. "cd .."
alias ... "cd ../.."
alias .... "cd ../../.."
alias ..... "cd ../../../.."
alias ...... "cd ../../../../.."
alias ....... "cd ../../../../../.."

abbr -a ka "killall"
abbr -a k9 "kill -9"

abbr -a e "emacs"

abbr -a v "nvim"

alias tmux "tmux -2"
abbr -a t "tmux"
abbr -a tl "tmux ls"
abbr -a ta "tmux attach -t"
abbr -a ts "tmux new -s"
abbr -a tk "tmux kill-session -t"

abbr -a be "bundle exec"
abbr -a ber "bundle exec rails"

abbr -a gbr "git branch"
abbr -a g "git"
abbr -a gs "git status -s"
abbr -a gd "git diff"
abbr -a gps "git push"
abbr -a gp "git pull"
abbr -a gcm "git commit"
abbr -a ga "git add"
abbr -a gco "git checkout"
abbr -a gcl "git clone"
abbr -a gl "git log --pretty=format:'%C(yellow)%h %Cred%ar %Cblue%an%Cgreen%d %Creset%s' --date=short"
abbr -a grbi "git rebase -i"
abbr -a grb "git rebase"


function fish_prompt --description 'Write out the prompt'
    # Just calculate these once, to save a few cycles when displaying the prompt
    if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
    end

    if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
    end

    if not set -q __git_cb
    set __git_cb ":"(set_color brown)(git branch ^/dev/null | grep \* | sed 's/* //')(set_color normal)""
    end

    switch $USER

    case root

    if not set -q __fish_prompt_cwd
        if set -q fish_color_cwd_root
            set -g __fish_prompt_cwd (set_color $fish_color_cwd_root)
        else
            set -g __fish_prompt_cwd (set_color $fish_color_cwd)
        end
    end

    printf '%s@%s:%s%s%s%s# ' $USER $__fish_prompt_hostname "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" $__git_cb

    case '*'

    if not set -q __fish_prompt_cwd
        set -g __fish_prompt_cwd (set_color $fish_color_cwd)
    end

    printf '%s@%s:%s%s%s%s$ ' $USER $__fish_prompt_hostname "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_normal" $__git_cb

    end
end

