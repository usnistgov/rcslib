#! /bin/sh

set -x ;

\rm -rf /tmp/svn_trash_*;
\rm -rf /tmp/tni_*;
\rm -rf /tmp/*.nml;
\rm -ff /tmp/nml*.log;
\rm -rf /tmp/UDP_with*.log;
\rm -rf /tmp/TCP_with*.log;
\rm -rf /tmp/STCP_with*.log;
\rm -rf /tmp/SHMEM*.log;
\rm -rf /tmp/mpb_*;
\rm -f /tmp/psgrepi.sh;
\rm -f /tmp/killnt.sh;


if svn info ; then
    tdir=/tmp/svn_trash_$$; 
    mkdir $tdir;
    find . -name \*.mine -exec mv '{}' $tdir \;
    find . -name \*.r\[0-9\]\[0-9\]\[0-9\] -exec mv '{}' $tdir \;
    find . -name \*.r\[0-9\]\[0-9\] -exec mv '{}' $tdir \;
    find . -name RCS_VERSION.java -exec mv '{}' $tdir \;
    find . -name rcs_svn_vers.hh -exec mv '{}' $tdir \;
    svn update
    svn status;
fi

