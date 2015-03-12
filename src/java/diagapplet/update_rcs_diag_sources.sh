#! /bin/sh

#set -x
echo "Running $0 $*"

\rm -f rcs_diag_sources.mak;
\rm -f rcs_diag_sources.mak.mine;
\rm -f rcs_diag_sources.mak.r*;
echo "rcs_diag_sources= \\" > rcs_diag_sources.mak;
rcs_diag_sources=`find ../rcs -name \*.java | grep -v '\.depend'`;

for j in ${rcs_diag_sources} ; do
	echo $j |  awk '{printf("\t%s \\\n",$1);}' >>rcs_diag_sources.mak;
done;
echo "" >>rcs_diag_sources.mak;
echo "" >>rcs_diag_sources.mak;

for j in ${rcs_diag_sources} ;  do
	obj=`echo $j | sed 's#[.]java##g' | sed 's#[.]#/#g' | sed  ' s#///#../#' | awk '{printf("%s.o",$1);}'`;
	deps=`grep import $j | grep 'rcs.' | sed 's#;##g' | sed 's#[.]#/#g' | sed  ' s#///#../#' | awk '{printf(" ../%s.o ",$2);}'`;
	echo "${obj}: ${deps} " >> rcs_diag_sources.mak;
	echo "" >>rcs_diag_sources.mak;
	echo "" >>rcs_diag_sources.mak;
done;
echo "" >>rcs_diag_sources.mak;
echo "" >>rcs_diag_sources.mak;


\rm -f diag_sources.mak;
echo "diag_sources= \\" > diag_sources.mak;
diag_sources=`find . -name \*.java`;

for j in ${diag_sources} ; do
	echo $j |  awk '{printf("\t%s \\\n",$1);}' >>diag_sources.mak;
done;
echo "" >>diag_sources.mak;
echo "" >>diag_sources.mak;

for j in ${diag_sources} ;  do
	obj=`echo $j | sed 's#[.]java##g' | sed 's#[.]#/#g' | sed  ' s#//##' | awk '{printf("%s.o",$1);}'`;
	deps=`grep import $j | grep 'diagapplet.' | sed 's#;##g' | sed 's#[.]#/#g' | sed  ' s#//##' | awk '{printf(" ../%s.o ",$2);}'`;
	echo "${obj}: ${deps} " >> diag_sources.mak;
	echo "" >>diag_sources.mak;
	echo "" >>diag_sources.mak;
done;
echo "" >>diag_sources.mak;
echo "" >>diag_sources.mak;
