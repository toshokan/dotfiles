source ~/.zshenv 
if [[ -n "$SSH_CONNECTION" ]]
then
    export PINENTRY_USER_DATA="USE_CURSES=1"
fi
