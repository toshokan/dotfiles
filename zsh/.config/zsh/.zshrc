# Emacs keys
bindkey -e
bindkey '^i' complete-word
setopt NO_FLOW_CONTROL
WORDCHARS=${WORDCHARS//[-.\/\_]}
bindkey '^[[H' beginning-of-line # HOME
bindkey '^[[F' end-of-line # END
bindkey '^[[3~' delete-char # DELETE

# Beeping
setopt NO_BEEP

# History
setopt SHARE_HISTORY EXTENDED_HISTORY HIST_IGNORE_DUPS HIST_FIND_NO_DUPS HIST_REDUCE_BLANKS HIST_IGNORE_SPACE HIST_NO_STORE HIST_VERIFY

# Prompts
PROMPT="%F{cyan}[%~] > %f"
RPROMPT="%F{yellow} < %m%f%F{cyan} | %f%(?.%F{green}.%F{red})%T%f"

# Completion
setopt GLOB AUTO_MENU ALWAYS_LAST_PROMPT COMPLETE_ALIASES AUTO_REMOVE_SLASH COMPLETE_IN_WORD AUTO_LIST LIST_TYPES LIST_PACKED
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select=4
zstyle ':completion:*' max-errors 3 numeric
zstyle ':completion:*' completer _expand _complete _match _correct _approximate _history _prefix _ignored
zstyle ':completion:*:ignored:*' single-ignored show
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:files' ignored-patterns '*?.o' '*?~'
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]-}={[:upper:][:lower:]_}' 'r:|[-_./]=* r:|=*'

# General
setopt AUTO_CD EXTENDED_GLOB MULTIOS CORRECT AUTO_PUSHD

ls() {
    command ls --color=auto "${@}"
}

grep() {
    command grep --color=auto "${@}"
}

alias wakiri="wol -v 70:85:c2:89:62:a1"
alias wakgri="wol -v 00:1c:c4:65:f8:b3"
