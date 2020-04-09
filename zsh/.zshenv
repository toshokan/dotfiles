ZDOTDIR=${HOME}/.config/zsh
HISTFILE=${ZDOTDIR}/histfile
HISTSIZE=100000
SAVEHIST=100000

function source_if_exists() {
    if [[ -f $1 ]]; then
	source $1
    fi
}

source_if_exists "${ZDOTDIR}/.zshenv.local"

rust_path=(~/.cargo/bin)
local_path=(~/.local/bin)
env_path=(~/bin/)
opt_path=(/opt/bin)

typeset -Ux path=($env_path
	          $rust_path
	          $opt_path
		  $path_local
	          $path)

# Fix grey Java GUIs
typeset -x _JAVA_AWT_WM_NONREPARENTING=1

typeset -x GTK2_RC_FILES="${HOME}/.config/.gtkrc-2.0"
typeset -x LESSHISTFILE=-
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

TERMINAL() {
    ${=TERMINAL} ${@}
}

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

fork_detach() {
    nohup ${@} 2>/dev/null >/dev/null & disown
}
