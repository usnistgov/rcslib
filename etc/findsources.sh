#! /bin/sh

echo "This script should not be used anymore."
exit 9

if [ ! -d etc -o ! -d src  ] ; then
    echo "This script is being run from the wrong directory.";
    exit -1
fi

if [ ! -w etc ] ; then
    echo "Can not run this script without write access to etc directory."
    exit -1
fi


if [ -f etc/default_sources ] ; then
    rm etc/default_sources
fi

touch etc/default_sources

for i in src/os_intf src/cms src/node src/print src/posemath src/stg_clss src/nt_xdr ; do
    find $i  -xdev -type f -a  -name \*.cc  -a  -not -exec grep NOT_A_DEFAULT_RCSLIB_SOURCE {}  \; -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/default_sources
    find $i   -xdev -type f -a -name \*.c  -a  -not -exec grep NOT_A_DEFAULT_RCSLIB_SOURCE {}  \; -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/default_sources
done

if [ -f etc/nondefault_sources ] ; then
    rm etc/nondefault_sources
fi

touch etc/nondefault_sources

find src  -xdev -type f -a -name \*.cc  -a  -exec grep NOT_A_DEFAULT_RCSLIB_SOURCE {}  \; -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.c -a  -exec grep NOT_A_DEFAULT_RCSLIB_SOURCE {}  \; -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/nondefault_sources

find src/java  -xdev -type f -a -name \*.java  >>etc/nondefault_sources
echo src/java/rcs/update_ver >>etc/nondefault_sources
echo src/java/rcs/RCS_VERSION.java.perm >>etc/nondefault_sources
find src   -xdev -type f -a -name Make\* -a -not -name \*~ -a -not -name \*# >>etc/nondefault_sources
find  src  -xdev -type f -a  -name \*.nml -o -name \*.nml2  >> etc/nondefault_sources

find src  -xdev -type f -a -name \*.tex  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.lyx  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.htm  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.html  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.txt -a -not -regex ./plat.\*  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.run  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.sh  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.ini  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.tbl  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.var  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.tcl  >>etc/nondefault_sources
find src  -xdev -type f -a -name \*.tool_default  >>etc/nondefault_sources
find  src  -xdev -type f -a  -name \*.gen  >>etc/nondefault_sources


find etc  -xdev -type f -a -name \*.tex  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.lyx  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.htm  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.html  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.txt -a -not -regex ./plat.\*  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.run  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.sh  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.ini  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.tbl  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.var  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.tcl  >>etc/nondefault_sources
find etc  -xdev -type f -a -name \*.tool_default  >>etc/nondefault_sources
find  etc -xdev -type f -a  -name \*.gen  >>etc/nondefault_sources

echo Makefile.am.head 2>/dev/null  >>etc/nondefault_sources
echo INSTALL.configure 2>/dev/null  >>etc/nondefault_sources

if [ -f nmlclean.log ] ; then rm nmlclean.log ; fi


if [ -f etc/headers ] ; then
    rm etc/headers
fi

touch etc/headers

find src  -name \*.h  -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/headers
find src  -name \*.hh -print | grep -v NOT_A_DEFAULT_RCSLIB_SOURCE >>etc/headers


if [ -f Makefile.am ] ; then
    rm Makefile.am
fi

touch Makefile.am

if [ -f Makefile.am.head ] ; then
    cat Makefile.am.head >>Makefile.am
fi

echo  >>Makefile.am
echo -n "librcs_la_SOURCES= src/rcsvers.c" >>Makefile.am
cat etc/default_sources | awk '{printf("\\\n\t%s",$1);} \
END {printf("\n");}' >>Makefile.am
echo  >>Makefile.am
echo -n "EXTRA_DIST=etc" >>Makefile.am
cat etc/nondefault_sources | awk '{printf("\\\n\t%s",$1);} \
END {printf("\n");}' >>Makefile.am
echo  >>Makefile.am
echo -n "noinst_HEADERS=" >>Makefile.am
cat etc/headers | awk '{printf("\\\n\t%s",$1);} \
END {printf("\n");}' >>Makefile.am
echo  >>Makefile.am

if [ -f Makefile.am.tail ] ; then
    cat Makefile.am.tail >>Makefile.am
fi
