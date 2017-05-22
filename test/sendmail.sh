#!/bin/bash

SUBJECT=$1
TO=$2
#FILE=$3

mutt -d 1 -s "$SUBJECT" -a $FILE -- $TO << "Text" 

