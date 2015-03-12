#! /bin/sh

#set -x
echo "Running $0 $*"

\rm -f diag_sources.mak;
\rm -f diag_sources.mak.mine;
\rm -f diag_sources.mak.r*;
echo "diag_sources= \\" > diag_sources.mak;
diag_sources=`find . -name \*.java | grep -v depend`;

for j in ${diag_sources} ; do
        if test "x${j}" = "xCogeGen/CodeGen" ; then
	    continue;
	fi
        if test "x${j}" = "xCogeGen/CodeGenCmdLine" ; then
	    continue;
	fi
	echo $j | sed  's#[.]/##' |  awk '{printf("\t%s \\\n",$1);}' >>diag_sources.mak;
done;
echo "" >>diag_sources.mak;
echo "" >>diag_sources.mak;

for j in ${diag_sources} ;  do
        if test "x${j}" = "xCogeGen/CodeGen" ; then
	    continue;
	fi
        if test "x${j}" = "xCogeGen/CodeGenCmdLine" ; then
	    continue;
	fi
	obj=`echo $j | sed 's#[.]java##g' | sed 's#[.]#/#g' | sed  ' s#//##' | awk '{printf("%s.o",$1);}'`;
	deps=`grep import $j | grep 'diagapplet.'| sed 's#diagapplet[.]##g' | sed 's#;##g' | sed 's#[.]#/#g' | sed  ' s#//##' | awk '{printf("%s.o ",$2);}'`;
	echo "${obj}: ${deps} " >> diag_sources.mak;
	echo "" >>diag_sources.mak;
	echo "" >>diag_sources.mak;
done;
echo "" >>diag_sources.mak;
echo "" >>diag_sources.mak;
