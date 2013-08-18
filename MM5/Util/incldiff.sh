#!/bin/sh
if [ ! "$1" ]; then
 echo "usage: incldiff.sh arg1 arg2"
 exit 0
fi
if [ ! "$2" ]; then
 echo "usage: incldiff.sh arg1 arg2"
 exit 0
fi
if [ ! -s "$1" ]; then
 echo "$1 either does not exist or has size 0!"
 exit 0
fi
if [ ! -s "$2" ]; then
 echo "$2 does not exist. Creating $2"
 cp $1 $2
 rm $1
 exit 0
fi
fred=`diff $1 $2`
if [ ! "$fred" ]; then
 echo "No differences detected between $1 and $2"
 rm $1
else
 echo "Differences detected between $1 and $2"
 echo "Replacing $2"
 cp $1 $2
 rm $1
fi
