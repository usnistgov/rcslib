#!/bin/sh

set -x
ipcrm  `ipcs -s | gawk '{if($1 != "0x00000000" && $1 != "") printf("-s %s\n",$2);}' | grep -vi sh | grep -vi si | grep -vi se`
ipcrm  `ipcs -m | gawk '{if($1 != "0x00000000" && $1 != "") printf("-m %s\n",$2);}' | grep -vi sh | grep -vi si | grep -vi se`
