#!/bin/sh

mv $1 /tmp/${1##*/}.orig
echo '/* '  > $1
cat COPYING >> $1
echo '*/ '  >> $1
echo ''  >> $1
cat /tmp/${1##*/}.orig >>$1
