#!/bin/bash

if [[ -z $* ]]; then
    echo "first"
    packages=`ls -I setup.sh`
else
    packages=$*
fi

do_stow() {
    stow --no-folding $1
}

for p in $packages; do
    do_stow $p
done
