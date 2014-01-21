#!/bin/bash

SCRIPT_PATH=$(dirname $0)

if [ $1 ] && [ -f $1 ]; then
    if [ ! -d $SCRIPT_PATH/pdf ]; then
          mkdir $SCRIPT_PATH/pdf
    fi  
    echo $(pwd)/$1 | python $SCRIPT_PATH/bin/status_per_length.py --format=pdf > $SCRIPT_PATH/pdf/status_per_length.pdf
else
    echo "usage: $0 path_to_history_file"
fi 
