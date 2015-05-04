#!/bin/sh

set -x;

git pull  || exit 1
git add -A
git commit -a -m "pre set_version_number check in" || exit 1

if test "x${AWK}" = "x" ; then
    AWK=awk;
fi

${AWK} --version
${AWK} --version | grep GNU || echo "BAD awk version, install gawk, remove mawk or set AWK environment variable."
${AWK} --version | grep GNU || exit 1


( cd src/java/rcs ; env DEBUG=1 ./update_ver )

cp Makefile.am /tmp/Makefile.am.$$
cat /tmp/Makefile.am.$$ | awk '{ if ( $2 == "-version-number" ) printf("%s %s %d:%d:%d\n",$1,$2, strftime("%Y",systime()),strftime("%m",systime()), strftime("%d",systime())); else print $0;  }' | awk '{ if ( $2 == "-release" ) printf("%s %s %4.4d-%2.2d-%2.2d\n",$1,$2, strftime("%Y",systime()),strftime("%m",systime()), strftime("%d",systime())); else print $0;  }' > /tmp/Makefile.am_new_version.$$

diff /tmp/Makefile.am.$$ /tmp/Makefile.am_new_version.$$
cp /tmp/Makefile.am_new_version.$$ ./Makefile.am


cp configure.ac /tmp/configure.ac.$$
cat /tmp/configure.ac.$$ | awk '{ if ( $1 == "AC_INIT(Real-Time" ) printf("AC_INIT(Real-Time Control System Library, %s, shackle@nist.gov, rcslib)\n",strftime("%Y.%m.%d",systime())); else print $0; }' > /tmp/configure.ac_new_version.$$

diff /tmp/configure.ac.$$ /tmp/configure.ac_new_version.$$
cp /tmp/configure.ac_new_version.$$ ./configure.ac

cp src/rcsvers.hh /tmp/rcsvers.hh.$$
cat /tmp/rcsvers.hh.$$ |  awk '{if ( $1 == "#define" ) { if ( $2 == "RCS_VERSION" ) printf("#define RCS_VERSION \"%s\"\n",strftime("%Y.%m.%d",systime())); else if ( $2 == "RCS_MAJOR_VERSION" ) printf("#define RCS_MAJOR_VERSION (%s)\n",strftime("%Y",systime()));  else if ( $2 == "RCS_MINOR_VERSION" ) printf("#define RCS_MINOR_VERSION (%d)\n",strftime("%m",systime())); else if ( $2 == "RCS_VERSION_NUMBER" ) printf("#define RCS_VERSION_NUMBER %d\n",strftime("%Y%m%d",systime()));  else  print $0 ; } else print $0 ; }' > /tmp/rcsvers.hh_new_version.$$

diff /tmp/rcsvers.hh.$$ /tmp/rcsvers.hh_new_version.$$
cp /tmp/rcsvers.hh_new_version.$$ src/rcsvers.hh

if test "x${1}" = "x" ; then
    new_version=`date +%Y.%m.%d`;
else
    new_version="${1}";
fi

old_version=`cat doc/getrcs.html | awk '{ if ( $1 == "<!--RCS_VERSION=" && $3 == "-->" ) print $2; }'`;

echo "new_version=${new_version}"
echo "old_version=${old_version}"

if test "x${new_version}" != "x${old_version}" -a  "x${old_version}" != "x" -a   "x${new_version}" != "x" ; then
    cp doc/getrcs.html /tmp/getrcs.html.$$
    cat /tmp/getrcs.html.$$ | sed s#rcslib-${old_version}\.tar#rcslib-${new_version}.tar#g > /tmp/getrcs.html_step_1.$$
    cat /tmp/getrcs.html_step_1.$$ | sed s#rcslib-${old_version}/src#rcslib-${new_version}/src#g > /tmp/getrcs.html_step_2.$$
    cat /tmp/getrcs.html_step_1.$$ | sed s#rcslib-${old_version}/etc#rcslib-${new_version}/etc#g > /tmp/getrcs.html_step_2.$$
    cat /tmp/getrcs.html_step_2.$$ | sed s#rcslib-${old_version}\$#rcslib-${new_version}# > /tmp/getrcs.html_new_version.$$
    cat /tmp/getrcs.html_step_2.$$ | sed "s#RCS_VERSION= ${old_version}#RCS_VERSION= ${new_version}#" > /tmp/getrcs.html_step_3.$$
    cat /tmp/getrcs.html_step_3.$$ | sed "s#<p>Last Modified: .*</p>\$#<p>Last Modified: ${new_version} </p>#" > /tmp/getrcs.html_step_4.$$
    cat /tmp/getrcs.html_step_4.$$ | sed "s#cd rcslib-.*\$#<p>cd rcslib-${new_version}#" > /tmp/getrcs.html_step_5.$$
    cat /tmp/getrcs.html_step_5.$$ | sed "s#<p><p><p>cd#<p>cd#" > /tmp/getrcs.html_step_6.$$
    cat /tmp/getrcs.html_step_6.$$ | sed "s#<p><p>cd#<p>cd#" > /tmp/getrcs.html_new_version.$$

    diff /tmp/getrcs.html.$$ /tmp/getrcs.html_new_version.$$
    cp /tmp/getrcs.html_new_version.$$ doc/getrcs.html
else
    if test "x${force}" = "x" ; then
	set +x;
	echo 
	echo "ERROR: Old version is the same as new version".
	exit 1;
    fi
fi

\rm -rf .last* lib/* bin/* include/* plat .multiplat* 2>/dev/null >/dev/null 
sudo \rm -rf .last* lib/* bin/* include/* plat .multiplat* 2>/dev/null >/dev/null
autoreconf
etc/clean_build.sh
etc/multiplatbuild.sh
git add -A
git commit -a -m "set new_version = ${new_version}" || exit 1
git push || exit 1

set +x;

echo 
echo "$0 succeeded version=${new_version}"
