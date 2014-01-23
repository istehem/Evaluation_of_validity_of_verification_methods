#!/bin/bash

SCRIPT_PATH=$(dirname $0)

if [ $1 ] && [ -f $1 ]; then
    if [ ! -d $SCRIPT_PATH/tex ]; then
          mkdir $SCRIPT_PATH/tex
    fi
    file=${1##*/}
    echo $(pwd)/$1 | python $SCRIPT_PATH/bin/state_transitions.py --format=pdf > $SCRIPT_PATH/tex/${file%.*}.tex
else
    echo "usage: $0 path_to_history_file"
fi

