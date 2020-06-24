#!/bin/bash

if [[ -z $* ]]; then
    packages=`ls -I setup.sh -I README.md`
else
    packages=$*
fi

do_stow() {
    stow --no-folding $1
}

for p in $packages; do
    do_stow $p
done
