#!/bin/bash

packages=`ls -I setup.sh`

do_stow() {
    stow --no-folding $1
}

for p in $packages; do
    do_stow $p
done
