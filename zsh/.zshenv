ZDOTDIR=$HOME/.config/zsh
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

rust_path=(~/.cargo/bin)
ruby_path=(~/.gem/ruby/2.6.0/bin)
local_path=(~/.local/bin)
env_path=(~/bin/)
opt_path=(/opt/bin)

typeset -Ux path=($env_path
	          $rust_path
	          $ruby_path
	          $opt_path
	          $path)

# Fix grey Java GUIs
typeset -x _JAVA_AWT_WM_NONREPARENTING=1

typeset -x EDITOR="emacsclient -t"
typeset -x VISUAL="emacsclient -c"
typeset -x ALTERNATE_EDITOR=""
typeset -x SUDO_EDITOR=${EDITOR}
E() {
    typeset -x SUDO_EDITOR=${EDITOR}
    sudoedit "${@}"
}

V() {
    typeset -x SUDO_EDITOR=${VISUAL}
    sudoedit "${@}"
}

ke() {
    if [[ $(pgrep emacs) ]]; then
	$(emacsclient -e '(save-buffers-kill-terminal)')
    else
	echo "GNU Emacs is not running..."
	return 1
    fi
}
e() {
    EDITOR ${@}
}

v() {
    VISUAL ${@}
}

kt() {
    killall Telegram 2>&1 >/dev/null
}
EDITOR() {
    ${=EDITOR} ${@}
}

VISUAL() {
    ${=VISUAL} ${@}
}

typeset -x TERMINAL="alacritty"
TERMINAL() {
    ${=TERMINAL} ${@}
}

typeset -x BROWSER="firefox"
BROWSER() {
    ${=BROWSER} ${@}
}

EMAIL() {
    thunderbird ${@}
}

SCREEN_LOCK_CMD() {
    physlock -md
}

SUSPEND_CMD() {
    sudo pm-suspend
}

LOCK_SUSPEND_CMD() {
    SCREEN_LOCK_CMD & SUSPEND_CMD
}

SCREENSHOT_FULLSCREEN() {
    scrot
}

SCREENSHOT_SELECT() {
    sleep 0.5
    scrot --quality 100 -s
}

FILE_BROWSER() {
    pcmanfm ${@}
}
