#!/bin/bash

SUBJECT=$1
TO=$2
#FILE=$3

#mutt -d 1 -s "$SUBJECT" -a $FILE -- $TO << "Text" 

mutt -d 1 -s "Sending Figure" -a SitesOutputs_Static3.txt.steadystate.png -- charles.santana@gmail.com << "Testing text" 

#mutt -d 1 -s "$SUBJECT" -- $TO << "Text" 
