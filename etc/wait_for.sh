#!/bin/bash 

#set -x

if test "x$1" = "x" -o "x$2" = "x" ; then
    echo "usage: <timeout_in_seconds> <file_to_wait_for>";
    exit 1;
fi

echo -n  "Waiting for $2 ";

#----------------------------------------
#    Function: check_for_file_or_process()
#----------------------------------------
#prog=${2##*/}		# last string before '/'
file="$2"
check_for_file()
{
    if test -f "$file" ; then
	echo " done.";
	exit 0;
    fi

}


#-------------------------------------------
#  1.) 
#-------------------------------------------
check_for_file
tries=0;
while true ; do
    echo "waiting for $2 ( ${tries} of ${1} ) ..."
    sleep 1;
    let tries++;
    if test ${tries} -gt $1 ; then
	echo " timedout.";
	exit 2;
    fi
    check_for_file
done

echo " done."

cat $2


