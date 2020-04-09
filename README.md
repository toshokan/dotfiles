# toshokan's dotfiles
This is a collection of dotfiles, intended primarily for GNU/Linux
systems.  The largest item is my GNU Emacs config, which changes
rapidly and is laid out using `org-mode`, and compiled dynamically
with `org-babel` when required.

Feel free to look around!

## usage
These dotfiles are meant to be used with [GNU
Stow](https://www.gnu.org/software/stow/), a wonderful symlink
manager.  For example, if this repository is cloned to your home
directory, use `stow emacs` to symlink the contents of the `emacs`
directory (currently `.emacs.d`) into your home directory so that
Emacs can find it.


