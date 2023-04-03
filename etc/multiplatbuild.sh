#!/bin/sh 

if test "x${DEBUG}" != x ; then
    set -x
fi

( \rm -rf ~/.tmp/*;\rm -rf /tmp/mpb_*; \rm -rf /tmp/tni_*; \rm -rf /dev/shm/mpb_* ; \rm -rf /dev/shm/tni_*; \rm -f /tmp/nml_codegen_auto_*.gen; \rm -f /tmp/jdk_*.txt; ) >/dev/null 2>/dev/null

unalias ls >/dev/null 2>/dev/null
unalias rm >/dev/null 2>/dev/null
unalias mv >/dev/null 2>/dev/null
unalias cp >/dev/null 2>/dev/null
unalias cat >/dev/null 2>/dev/null
unalias cd >/dev/null 2>/dev/null

if test "x${PLAT}" != "x" ; then
    PLAT1=${PLAT}
fi

if test "x${HOME}" = "x" ; then
    HOME=~
fi


CXXFLAGS="${CXXFLAGS} ${RCSLIB_CXXFLAGS} " 
if test -f ${HOME}/.rcslib_cxxflags ; then
    for f in `cat ${HOME}/.rcslib_cxxflags`; do 
	CXXFLAGS="${CXXFLAGS} ${f}";
    done
fi


CFLAGS="${CFLAGS} ${RCSLIB_CFLAGS} " 
if test -f ${HOME}/.rcslib_cflags ; then
    for f in `cat ${HOME}/.rcslib_cflags`; do 
	CFLAGS="${CFLAGS} ${f}";
    done
fi

echo "CXXFLAGS=${CXXFLAGS}"
echo "CFLAGS=${CFLAGS}"

my_home=${HOME}

uname_s=`uname -s`;

if test "${CC}x" = "x" -o "x${CC}" = "xgcc" ; then
    case $UNAME_M in
	i686) CPUFLAGS="-march=i686";;
	i586) CPUFLAGS="-march=i586";;
    esac

    if test "x${CPUFLAGS}" != "x" ; then
	if echo "x${CFLAGS}" | grep -v "mcpu" >/dev/null 2>/dev/null ; then
	    CFLAGS="${CFLAGS} ${CPUFLAGS}";
	fi
	if echo "x${CXXFLAGS}" | grep -v "mcpu" >/dev/null 2>/dev/null ; then
	    CXXFLAGS="${CXXFLAGS} ${CPUFLAGS}";
	fi
    fi

    if test "x${NO_WALL_FLAG}" = "x" ; then
	
	echo "x${CFLAGS}x" | grep "Wall" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CFLAGS="${CFLAGS}  -Wall";
	    export CFLAGS;
	fi
	
	echo "x${CXXFLAGS}x" | grep "Wall" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CXXFLAGS="${CXXFLAGS}  -Wall";
	    export CXXFLAGS;
	fi
    fi
    
    if test "x${NO_WERROR_FLAG}" = "x" -a "x${uname_s}" = "xLinux" -a "x${USER}" = "xshackle" ; then
	
	echo "x${CFLAGS}x" | grep "Werror" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CFLAGS="${CFLAGS}  -Werror";
	    export CFLAGS;
	fi
	
	echo "x${CXXFLAGS}x" | grep "Werror" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CXXFLAGS="${CXXFLAGS}  -Werror";
	    export CXXFLAGS;
	fi
    fi
    if test "x${NO_DEBUG_FLAG}" = "x" -a "x${uname_s}" = "xLinux" -a "x${USER}" = "xshackle" ; then
	
	echo "x${CFLAGS}x" | grep "g" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CFLAGS="${CFLAGS}  -g";
	    export CFLAGS;
	fi
	
	echo "x${CXXFLAGS}x" | grep "g" > /dev/null ;
	if test "x$?" != "x0" ; then
	    CXXFLAGS="${CXXFLAGS}  -g";
	    export CXXFLAGS;
	fi
    fi
fi


echo "CXXFLAGS=${CXXFLAGS}";
echo "CFLAGS=${CFLAGS}";
    
if test "x${temp_dir}" = "x" ; then
    mkdir /tmp/mpb_$$ && touch /tmp/mpb_$$/.touch_$$ && temp_dir="/tmp/mpb_$$";
fi

if test "x${temp_dir}" = "x" ; then
    mkdir ${HOME}/.tmp && mkdir ${HOME}/.tmp/mpb_$$ && touch ${HOME}/.tmp/mpb_$$/.touch_$$ && temp_dir="${HOME}/.tmp/mpb_$$";
fi

if test \! -d ${temp_dir} ; then
    (mkdir  ${temp_dir} || true );
    (mkdir -p ${temp_dir} || true );
fi

echo temp_dir=${temp_dir}

startdir=`pwd`

test_cmd=test

( touch ${HOME}/testot1.tst ) >/dev/null 2>/dev/null
sleep 1
( touch ${HOME}/testot2.tst ) >/dev/null 2>/dev/null

if ( /bin/test ${HOME}/testot1.tst -ot ${HOME}/testot2.tst ) >/dev/null 2>/dev/null ; then
    if ( test ${HOME}/testot1.tst -ot ${HOME}/testot2.tst ) >/dev/null 2>/dev/null ; then
	test_cmd=test
    else
	test_cmd=/bin/test
    fi
fi

( \rm -f ${HOME}/testot1.tst ) >/dev/null 2>/dev/null
( \rm -f ${HOME}/testot2.tst ) >/dev/null 2>/dev/null

#echo ${test_cmd}

if ${test_cmd} \! -d ${temp_dir} ; then
    mkdir ${temp_dir};
fi

if ${test_cmd} "x${rcsdir}" = "x" ; then

    progdir=`echo $0 | sed 's#/multiplatbuild.sh##' | sed 's#multiplatbuild.sh##'`
    if ${test_cmd} "x${progdir}" != "x" ; then
       if ( cd ${progdir} && cd .. && ${test_cmd} -f src/rcs.hh ) ; then
	   cd ${progdir} 
	   cd ..
	   rcsdir=`pwd`
       fi
    fi

fi

if ${test_cmd} "x${rcsdir}" = "x" ; then

    if ${test_cmd}  -f src/rcs.hh ; then
	rcsdir=`pwd`
    elif ${test_cmd}  -f ../src/rcs.hh ; then
	rcsdir=`cd ..; pwd`
    elif ${test_cmd}  -f ../../src/rcs.hh ; then
	rcsdir=`cd ../..; pwd`
    elif ${test_cmd}  -f ../../../src/rcs.hh ; then 
	rcsdir=`cd ../../.. ; pwd`
    elif ${test_cmd}  -f ../../../../src/rcs.hh ; then 
	rcsdir=`cd ../../../.. ; pwd`
    fi
fi

if ${test_cmd} "x${rcsdir}" = "x" ; then
    echo "Can not determine rcsdir." >&2
    exit 1
fi

if test -f "${rcsdir}/lastconfig/make.log" ; then
    \rm -f "${rcsdir}/lastconfig/make.log";
fi

make_log="${temp_dir}/make_${pid}.log";

if test -d "${rcsdir}/lastconfig/" -a -w "${rcsdir}/lastconfig/" ; then
    make_log="${rcsdir}/lastconfig/make.log";
fi



( \rm -f ${rcsdir}/src/rcs_svn_vers.hh.* ) >/dev/null 2>/dev/null;
( \rm -f ${rcsdir}/src/java/rcs/RCS_VERSION.java.r* ) >/dev/null 2>/dev/null;
( \rm -f ${rcsdir}/src/java/rcs/RCS_VERSION.java.mine ) >/dev/null 2>/dev/null;
( \rm -f ${rcsdir}/src/java/rcs/RCS_VERSION.java.[0-9]* ) >/dev/null 2>/dev/null;


which svnversion
if test  "x$?" = "x0" -a -w ${rcsdir}/src/rcsvers.c -a -f ${rcsdir}/src/rcsvers.c -a -d ${rcsdir}/.svn -a -f ~/.DO_RCSLIB_SVN_CHECK ; then 
    ( cd ${rcsdir} ; \
	svn_n=`svnversion -n .`; \
	svn_ns=`svnversion -n . | sed 's#:.*##' | sed s#M##`; \
	last_svn_n=`cat ${rcsdir}/.last_rcs_svn | sed 's#:.*##' | sed s#M##`; \
	echo "last_svn_n=${last_svn_n} svn_n=${svn_n} svn_ns=${svn_ns}"; \
	if test  "x${svn_ns}" != "x${last_svn_n}" -o \! -f   ${rcsdir}/src/rcs_svn_vers.hh; then \
	 if test "x${SSH_AGENT_PID}" != "x" -a "x${SSH_AUTH_SOCK}" != "x" ; then \
	  svn propset svn_n "${svn_n}" "${rcsdir}/src/rcs.hh";  \
	  svn propset svn_n "${svn_n}" "${rcsdir}/src/rcs_svn_vers.hh";  \
	  svn propset svn_n "${svn_n}" "${rcsdir}/src/rcs/RCS_VERSION.java";  \
	  svn propset svn_n "${svn_n}" "${rcsdir}/src/rcs/RCS_VERSION.java.perm" "${rcsdir}/src/rcs_svn_vers.hh" "${rcsdir}/src/rcs/RCS_VERSION.java" "${rcsdir}/src/rcs/RCS_VERSION.java.perm" ;  \
          svn ci -m "change svn_n prop to ${svn_n}" "${rcsdir}/src/rcs.hh"; \
	 fi ; \
	 echo "${svn_n}" > "${rcsdir}/.last_rcs_svn"; \
	 ##mv ${rcsdir}/src/rcsvers.c /tmp; \
	 ##cat /tmp/rcsvers.c | sed "s/#define RCS_SVN_VERS.*/#define RCS_SVN_VERS \"SVN_VER=${svn_n}\"/" > src/rcsvers.c; \
	 ##echo "#define RCS_SVN_VERS \"SVN_VER=${svn_n}\"" >src/rcs_svn_vers.hh; \
	 \rm -f "${rcsdir}/.lastbuild/rcsvers.lo"; \
	 \rm -f "${rcsdir}/.lastbuild/rcsvers.o"; \
	 \rm -f "${rcsdir}/.lastbuild/.libs/rcsvers.o"; \
	 \rm -f "${rcsdir}/.lastbuild/src/rcsvers.c"; \
	 \rm -f "${rcsdir}/src/rcs_svn_vers.hh.r*"; \
	 \rm -f "${rcsdir}/src/rcs_svn_vers.hh.R*"; \
	 \rm -f "${rcsdir}/src/rcs_svn_vers.hh.mine"; \
	 \rm -f "${rcsdir}/src/java/rcs/RCS_VERSION.java.r*"; \
	 \rm -f "${rcsdir}/src/java/rcs/RCS_VERSION.java.R*"; \
	 \rm -f "${rcsdir}/src/java/rcs/RCS_VERSION.java.mine"; \
	fi; \
	)
fi

find ${rcsdir}/src/ -name rcs\*ver\*.r[0-9][0-9][0-9] -exec \rm -f \{\} \; ;
find ${rcsdir}/src/ -name rcs\*ver\*.mine -exec \rm -f \{\} \; ;
find ${rcsdir}/src/test -name nml_test_format\*.r[0-9][0-9][0-9] -exec \rm -f \{\} \; ;
find ${rcsdir}/src/test -name nml_test_format\*.mine -exec \rm -f \{\} \; ;
find ${rcsdir}/src/test -name otherheader\*.r[0-9][0-9][0-9] -exec \rm -f \{\} \; ;
find ${rcsdir}/src/test -name otherheader\*.mine -exec \rm -f \{\} \; ;


if ${test_cmd} -f ${HOME}/tailtest.txt ; then
    \rm -f ${HOME}/tailtest.txt >/dev/null 2>/dev/null;
fi

cat >${HOME}/tailtest.txt <<EOF
1
2
3
EOF

if ${test_cmd} "x${TAIL1}" = "x" ; then 
    if cat ${HOME}/tailtest.txt | tail -n 1 >/dev/null 2>/dev/null ; then
	if ${test_cmd}  3 = `cat ${HOME}/tailtest.txt | tail -n 1 2>/dev/null` ; then
	    TAIL1='tail -n 1'
	fi
    fi
fi

if ${test_cmd} "x${TAIL1}" = "x" ; then 
    if cat ${HOME}/tailtest.txt | tail -1 >/dev/null 2>/dev/null ; then
	if ${test_cmd}  3 = `cat ${HOME}/tailtest.txt | tail -1 2>/dev/null` ; then
	    TAIL1='tail -1'
	fi
    fi
fi

if ${test_cmd} "x${TAIL1}" = "x" ; then 
    TAIL1='tail -n 1'
fi

if ${test_cmd} -f ${HOME}/tailtest.txt ; then
    \rm -f ${HOME}/tailtest.txt >/dev/null 2>/dev/null;
fi

if ${test_cmd}  "x${CC}" = "x" ; then
    if  ( qcc -v ) >/dev/null 2>/dev/null ; then 
	CC=qcc;
    elif ( gcc -v ) >/dev/null 2>/dev/null ; then
	CC=gcc;
    elif ( /usr/bin/gcc -v ) >/dev/null 2>/dev/null ; then
	CC=/usr/bin/gcc;
    elif ( cc -v ) >/dev/null 2>/dev/null ; then
	CC=cc;
    fi
fi

if ${test_cmd} "x${CC_V_CMD}" = "x" ; then
    if ${CC} -v 2>/dev/null >/dev/null ; then
	    CC_V_CMD="${CC} -v";
    fi
fi

if ${test_cmd} "x${CC_V_CMD}" = "x" ; then
    if ${CC} --version 2>/dev/null >/dev/null ; then
	    CC_V_CMD="${CC} --version";
    fi
fi

if ${test_cmd} "x${CC_V_CMD}" = "x" ; then
    if ${CC} -V 2>/dev/null >/dev/null ; then
	    CC_V_CMD="${CC} -V";
    fi
fi

if ${test_cmd} "x${CC_V_CMD}" = "x" ; then
    if ${CC} -V 2>&1 | grep -i version >/dev/null ; then
	    CC_V_CMD="${CC} -V";
    fi
fi

if ${test_cmd} "x${CC_V_CMD}" = "x" ; then
    CC_V=`echo ${CC} | ${TAIL1}`
else
    if ${CC_V_CMD} 2>&1 | grep -i version >/dev/null 2>/dev/null ; then
	CC_V=`${CC_V_CMD} 2>&1 | grep -i version | grep -v "GNU ld version" | ${TAIL1}`
    else
	CC_V=`${CC_V_CMD} 2>&1 | ${TAIL1}`
    fi
fi

CC_NAME=`echo ${CC}_${CC_V} | sed 'y#., ()-:;/!?|*&<>{}#__________________#' | sed 'y|#|_|' | sed 's#gcc_gcc#gcc#g' | sed 's#${CC}_${CC}#${CC}#g' | awk '{printf("%.50s",$1);}' `
echo "CC_NAME=${CC_NAME}"

UNAME_S=`uname -s | sed 'y#., ()-:;/|!?*&<>{}#__________________#' | sed 'y|#|_|' | awk '{printf("%.30s",$1);}'`
UNAME_R=`uname -r | sed 'y#., ()-:;/|!?*&<>{}#__________________#' | sed 'y|#|_|' | awk '{printf("%.30s",$1);}'`
UNAME_M=`uname -m | sed 'y#., ()-:;/|!?*&<>{}#__________________#' | sed 'y|#|_|' | awk '{printf("%.30s",$1);}'`

CPUFLAGS=


host_arg_found=no;
disable_java_arg=
for arg in $* ; do
    if ${test_cmd} "x${host_arg_found}" = "xyes" ; then
	host_arg=${arg};
	break;
    fi
    if ${test_cmd} "x${arg}" = "x--host" ; then 
	host_arg_found=yes;
    fi
done

if ${test_cmd} -f ${HOME}/.rebuild_rcslib_configure ; then
    REBUILD_RCSLIB_CONFIGURE=yes
fi

( \
if test "x${DEBUG}" != x ; then \
    set -x; \
fi; \
if ${test_cmd} "x${REBUILD_RCSLIB_CONFIGURE}" = "xyes" ; then
    if ${test_cmd} ${rcsdir}/configure -ot ${rcsdir}/aclocal.m4 -o \
	${rcsdir}/configure -ot ${rcsdir}/acinclude.m4 ; then
	
	(cd ${rcsdir}; aclocal || (echo "aclocal failed." >&2 ; exit 1 ) || exit 1) || exit 1;
    fi

    if ${test_cmd} ${rcsdir}/Makefile.in -ot ${rcsdir}/Makefile.am ; then
	  ls -l ${rcsdir}/Makefile.in
	  ls -l ${rcsdir}/Makefile.am
	 (cd ${rcsdir} ; automake || (echo "automake failed." >&2 ; exit 1 ) || exit 1) || exit 1;
	 ls -l ${rcsdir}/Makefile.in
	 touch ${rcsdir}/Makefile.in
	 ls -l ${rcsdir}/Makefile.in
	 
    fi

    if ${test_cmd} ${rcsdir}/rcs_config.h.in  -ot ${rcsdir}/configure.ac ; then
	ls -l ${rcsdir}/rcs_config.h.in
	ls -l ${rcsdir}/configure.ac
	(cd ${rcsdir}; autoheader || (echo "autoheader failed." >&2 ; exit 1 ) || exit 1) || exit 1;
	touch ${rcsdir}/rcs_config.h.in
	ls -l ${rcsdir}/rcs_config.h.in
    fi

    if ${test_cmd} ${rcsdir}/configure -ot ${rcsdir}/aclocal.m4 -o \
	${rcsdir}/configure -ot ${rcsdir}/acinclude.m4 -o \
	${rcsdir}/configure -ot ${rcsdir}/Makefile.in -o \
	${rcsdir}/configure -ot ${rcsdir}/configure.ac -o \
	${rcsdir}/configure -ot ${rcsdir}/rcs_config.h.in ; then
	
	ls -l ${rcsdir}/configure
	ls -l ${rcsdir}/aclocal.m4
	ls -l ${rcsdir}/Makefile.in
	ls -l ${rcsdir}/configure.ac
	ls -l ${rcsdir}/rcs_config.h.in

	(cd ${rcsdir} ; autoconf || (echo "autoconf failed." ; exit 1 ) || exit 1) || exit 1;
	ls -l ${rcsdir}/configure
	touch ${rcsdir}/configure
	ls -l ${rcsdir}/configure	
    fi
fi
) || exit 1;


if ${test_cmd}  "x${CC}" = "xmingw32-gcc" -a "x${host_arg_found}" = "xno" -a "x${UNAME_S}" = "xLinux" ; then
    host_arg='i386-pc-mingw32';
    host_arg_to_add="--host ${host_arg} --disable-shared --disable-ALLJAVA";
fi

if ${test_cmd}  "x${CC}" = "xi386-mingw32msvc-gcc" -a "x${host_arg_found}" = "xno" -a "x${UNAME_S}" = "xLinux" ; then
    host_arg='i386-pc-mingw32';
    host_arg_to_add="--host ${host_arg} --disable-ALLJAVA";
fi

if ${test_cmd} "x${MULTIPLAT_CROSS_HOST}" != "x" ; then
    host_arg="${MULTIPLAT_CROSS_HOST}";
    host_arg_to_add="--host ${host_arg} --disable-ALLJAVA";
fi

if ${test_cmd} "x${host_arg}" != "x" ; then
    UNAME_S=${host_arg}_cross_compiled_under_${UNAME_S};
fi

if ${test_cmd} "x${UNAME_S}" = "xLinux" ; then
    if ${test_cmd} "x`${rcsdir}/etc/havertlinux.sh | head -n 1`" = "xYES" ; then
	UNAME_S=Linux_with_rtlinux;
    elif ${test_cmd} "x`${rcsdir}/etc/havertai.sh | head -n 1`" = "xYES" ; then
	UNAME_S=Linux_with_rtai;
    fi
fi

LIBC_NAME=NOT_GLIBC

if ${test_cmd} -f /usr/include/features.h -a "x${host_arg}" = "x" ; then
    if grep _GLIBC /usr/include/features.h >/dev/null 2>/dev/null ; then
	GLIBC_MAJOR_VERSION=`grep __GLIBC__  /usr/include/features.h | grep '#define'  | grep -v PREREQ | head -n 1 | awk '{printf("%s",$3);}'`
	GLIBC_MINOR_VERSION=`grep __GLIBC_MINOR__  /usr/include/features.h | grep '#define' | grep -v PREREQ | head -n 1 | awk '{printf("%s",$3);}'`
	LIBC_NAME=GLIBC_${GLIBC_MAJOR_VERSION}_${GLIBC_MINOR_VERSION}
    else 
	LIBC_NAME=NOT_GLIBC
    fi
else
    LIBC_NAME=NOT_GLIBC
fi

if ${test_cmd} "x${host_arg}" = "x" ; then
    rel_install_dir=.multiplatinstalls/${UNAME_S}/${UNAME_R}/${UNAME_M}/${LIBC_NAME}/${CC_NAME}
else
    rel_install_dir=.multiplatinstalls/`echo ${host_arg}| sed y#-#/#`/${CC_NAME}
fi

if test "x${rcslib_config_name}" != "x" ; then
    rel_install_dir=${rel_install_dir}/${rcslib_config_name}
fi

install_dir=${rcsdir}/${rel_install_dir}

echo "install_dir=${install_dir}" >&2
if ${test_cmd} \! -d ${install_dir} ; then 
    mkdir -p ${install_dir} || (echo "mkdir -p ${install_dir}  failed." >&2 ; exit 127 ) || exit 127
fi

if ${test_cmd} "x${host_arg}" = "x" ; then
    rel_build_dir=.multiplatbuilds/${UNAME_S}/${UNAME_R}/${UNAME_M}/${LIBC_NAME}/${CC_NAME}
else
    rel_build_dir=.multiplatbuilds/`echo ${host_arg} | sed y#-#/#`/${CC_NAME}
fi

if test "x${rcslib_config_name}" != "x" ; then
    rel_build_dir=${rel_build_dir}/${rcslib_config_name}
fi

build_dir=${rcsdir}/${rel_build_dir}

echo "build_dir=${build_dir}" >&2
if ${test_cmd} \! -d ${build_dir} ; then
    mkdir -p ${build_dir} || (echo "mkdir -p ${build_dir} failed." >&2 ; exit 127 )
fi

cd ${build_dir} || (echo "cd ${build_dir} failed." >&2 ; exit 127 ) || exit 127

if ${test_cmd} -f ${build_dir}/Makefile ; then
    if ${test_cmd} ${build_dir}/Makefile -ot ${rcsdir}/configure -o \
	${build_dir}/libtool -ot ${rcsdir}/configure -o \
	${build_dir}/rcs_config.h -ot ${rcsdir}/configure -o \
	'(' -d ${build_dir}/.deps -a ${build_dir}/.deps -ot ${rcsdir}/configure ')' ; then
	( \
	if test "x${DEBUG}" != x ; then \
	set -x ; \
	ls -l ${build_dir}/Makefile ; \
	ls -l ${build_dir}/libtool ; \
	ls -l ${rcsdir}/configure ; \
	ls -ld ${build_dir}/.deps; \
	ls -l ${build_dir}/rcs_config.h; \
        fi
	)
	( if test "x${DEBUG}" != x ; then set -x ; fi; cd ${build_dir} ; \rm  -r -f Makefile* .dep* .conf* conf* libtool src *.{lo,o,la,a,so,loT} rcs_config.h gmem.out stamp-h1 >/dev/null 2>/dev/null)
    fi
fi

if ${test_cmd} "x${host_arg}" = "x" ; then
    PLAT=`(cd ${rcsdir}; platname_safe_mode=yes; export platname_safe_mode;   . ${rcsdir}/etc/platname) 2>/dev/null`
else
    PLAT=${UNAME_S}
fi

echo rcsdir=${rcsdir}
echo host_arg=${host_arg}
echo PLAT=${PLAT}
echo UNAME_S=${UNAME_S}

PLAT=`echo ${PLAT} | sed s#/.\*\$\## | sed y#.,\ \(\)-:\;/\|\!\?\*\&\<\>\{\}#__________________# | sed y\|#\|_\| | sed s#__#_#g | cut -c 1-71 `

echo PLAT=${PLAT}

LONG_PLAT=${PLAT}
if test "x${LONG_PLAT}" != "x" ; then
    PLAT=`echo ${LONG_PLAT} | awk '{printf("%.60s",$1);}'`
fi

echo PLAT=${PLAT}
export PLAT

mkdir -p ${rcsdir}/plat || (echo "mkdir -p ${rcsdir}/plat failed." >&2 ; exit 127 ) || exit 127

\rm -f ${HOME}/linktest* >/dev/null 2>/dev/null

pid=$$

 ( cd ${temp_dir} ; touch linktestfile${pid} ;  ln -s "linktestfile${pid}" "linktestfile${pid}.link" ); >/dev/null 2>/dev/null

#ls -l ${HOME}/link*

if ${test_cmd} -h ${HOME}/linktestfile${pid}.link -a \! -h ${HOME}/linktestfile${pid} ; then
    LINK_FLAG=-h;
else
    LINK_FLAG=-L;
fi

\rm -f ${HOME}/linktest* >/dev/null 2>/dev/null

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${PLAT} ; then
    rm ${rcsdir}/plat/${PLAT} || ( echo "Can not remove old symbolic link." >&2 ; exit 127 ) || exit 127
fi

if test "x${PLAT}" != "x" ; then

    if test -d ${rcsdir}/plat/${PLAT} ; then
	(cd ${rcsdir}/plat && (mkdir -p ".moved.${pid}" ;  mv "${PLAT}" ".moved.${pid}" ) ) || (echo "Can not move old plat out of the way." >&2 ; exit 127 ) || exit 127
    fi

    (  cd ${rcsdir}/plat && ln -s "../${rel_install_dir}" "${PLAT}" || exit 127 ) || ( echo "Can not setup symbolic link. PLAT=${PLAT} install_dir=${install_dir} " >&2 ; exit 127 ) || exit 127

fi


if test "x${PLAT1}" != "x"  -a "x${PLAT1}" != "x${PLAT}" ; then

    if test -d ${rcsdir}/plat/${PLAT1} ; then
	(cd ${rcsdir}/plat && (mkdir -p ".moved.${pid}" ;  mv "${PLAT1}" ".moved.${pid}" ) ) || (echo "Can not move old plat out of the way." >&2 ; exit 127 ) || exit 127
    fi

    (  cd ${rcsdir}/plat && ln -s "../${rel_install_dir}" "${PLAT1}" || exit 127 ) || ( echo "Can not setup symbolic link. PLAT1=${PLAT1} install_dir=${install_dir} " >&2 ; exit 127 ) || exit 127

fi


if test "x${host_arg}" = "x" ; then

    ( cd ${rcsdir} ;  PLAT=; rm ${HOME}/.platname* ; mkdir_cmd=true;   . ${rcsdir}/etc/platname >/dev/null; 2>/dev/null;
	if ${test_cmd} "x${ORIG_PLAT}" != "x" -a "x${ORIG_PLAT}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${ORIG_PLAT} ; then
		rm ${rcsdir}/plat/${ORIG_PLAT} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${ORIG_PLAT} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
		mv ${rcsdir}/plat/${ORIG_PLAT} ${rcsdir}/plat/.moved.${pid}/
	    fi
	    (cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${ORIG_PLAT}" )
	fi
	if ${test_cmd} "x${ETC_RELEASE_PLAT}" != "x" -a "x${ETC_RELEASE_PLAT}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${ETC_RELEASE_PLAT} ; then
	    rm ${rcsdir}/plat/${ETC_RELEASE_PLAT} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${ETC_RELEASE_PLAT} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
		mv ${rcsdir}/plat/${ETC_RELEASE_PLAT} ${rcsdir}/plat/.moved.${pid}/
	    fi
	    (cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${ETC_RELEASE_PLAT}" )
	fi
	if ${test_cmd} "x${DEBIAN_PLAT}" != "x" -a "x${DEBIAN_PLAT}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${DEBIAN_PLAT} ; then
		rm ${rcsdir}/plat/${DEBIAN_PLAT} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${DEBIAN_PLAT} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
		mv ${rcsdir}/plat/${DEBIAN_PLAT} ${rcsdir}/plat/.moved.${pid}/
	    fi
	(cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${DEBIAN_PLAT}"; )
	fi
	if ${test_cmd} "x${REDHAT_PLAT}" != "x" -a "x${REDHAT_PLAT}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${REDHAT_PLAT} ; then
		rm ${rcsdir}/plat/${REDHAT_PLAT} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${REDHAT_PLAT} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
	    mv ${rcsdir}/plat/${REDHAT_PLAT} ${rcsdir}/plat/.moved.${pid}/
	    fi
	    (cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${REDHAT_PLAT}" )
	fi
	if ${test_cmd} "x${OLD_PLAT}" != "x" -a "x${OLD_PLAT}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${OLD_PLAT} ; then
		rm ${rcsdir}/plat/${OLD_PLAT} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${OLD_PLAT} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
		mv ${rcsdir}/plat/${OLD_PLAT} ${rcsdir}/plat/.moved.${pid}/
	    fi
	    (cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${OLD_PLAT}" )
	fi
	if ${test_cmd} "x${PLATBASE}" != "x" -a "x${PLATBASE}" != "x${PLAT}" ; then
	    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/${PLATBASE} ; then
		rm ${rcsdir}/plat/${PLATBASE} ;
	    elif ${test_cmd} -d  ${rcsdir}/plat/${PLATBASE} ; then
		( mkdir -p ${rcsdir}/plat/.moved.${pid}/ ) >/dev/null 2>/dev/null;
		mv ${rcsdir}/plat/${PLATBASE} ${rcsdir}/plat/.moved.${pid}/
	    fi
	    (cd ${rcsdir}/plat/ ; ln -s "../${rel_install_dir}" "${PLATBASE}" )
	fi
	)>/dev/null 2>/dev/null;

fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/.lastbuild ; then
    \rm -f ${rcsdir}/.lastbuild >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastbuild ; then
    (cd ${rcsdir} ; mv .lastbuild  .lastbuild.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${build_dir} ; then 
    (cd ${rcsdir} ; ln -s "${rel_build_dir}" .lastbuild)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/.lastinstall ; then
    \rm -f ${rcsdir}/.lastinstall >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall ; then
    (cd ${rcsdir} ; mv .lastinstall  .lastinstall.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${install_dir} ; then 
    (cd ${rcsdir} ; ln -s "${rel_install_dir}" .lastinstall)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/lib ; then
    \rm -f ${rcsdir}/lib >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/lib ; then
    ( cd ${rcsdir}; mv lib lib.${pid} ) >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/lib ; then 
    (cd ${rcsdir} ; ln -s ".lastinstall/lib" .)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/bin ; then
    \rm -f ${rcsdir}/bin >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/bin ; then
    ( cd ${rcsdir}; mv bin bin.${pid} ) >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/bin ; then 
    (cd ${rcsdir} ; ln -s ".lastinstall/bin" .)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/include ; then
    \rm -f ${rcsdir}/include >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/include ; then
    ( cd ${rcsdir}; mv include include.${pid} ) >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/include ; then 
    (cd ${rcsdir} ; ln -s ".lastinstall/include" .)
fi


if ${test_cmd} -f "${HOME}/.${HOST}.${PLAT}.rcslib_configure_options" ; then
    CONFIG_OPTS=`cat ${HOME}/.${HOST}.${PLAT}.rcslib_configure_options`
fi

( \rm -f ${temp_dir}/do_rcslib_post_install_*.sh ) >/dev/null 2>/dev/null

chmod u+w ${temp_dir}
chmod u+w ${temp_dir}/*
touch ${temp_dir}/make${pid}.sh
chmod u+w ${temp_dir}/make${pid}.sh

cat >${temp_dir}/do_rcslib_post_install_${pid}.sh <<EOF
#!/bin/sh

if ${test_cmd} \! -d ${rcsdir}/plat/java/lib ; then
    mkdir -p ${rcsdir}/plat/java/lib 2>/dev/null
fi

(cd ${rcsdir}/plat/java/lib && \
    for jarfile in *.jar endjarfiles  ; do
	if ${test_cmd} "x${jarfile}" != "xendjarfiles" -a ${LINK_FLAG} ${jarfile} ; then
	    rm ${jarfile};
	fi
    done
) >/dev/null 2>/dev/null

if ${test_cmd} -d ${install_dir}/bin ; then

( cd ${install_dir}/bin && \
    for jarfile in *.jar endjarfiles; do \
	if ${test_cmd}  "x${jarfile}" != "xendjarfiles"  -a "x${jarfile}" != "x" ; then \
	    (cd ${rcsdir}/plat/java/lib && \
		ln -s "../../../${rel_install_dir}/bin/${jarfile}" .; ); \
	fi; \
    done; \
) >/dev/null 2>/dev/null
fi

if ${test_cmd} -d ${install_dir}/lib ; then

(cd ${install_dir}/lib && \
    if ${test_cmd} -f librcs.a -a ! -f librcsd.a ; then \
	if ${test_cmd} ${LINK_FLAG} librcsd.a ; then \
	    rm librcsd.a; \
	fi; \
	ln -s librcs.a librcsd.a; \
    fi \
) >/dev/null 2>/dev/null

fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/.lastinstall ; then
    \rm -f ${rcsdir}/.lastinstall >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall ; then
    (cd ${rcsdir} ; mv .lastinstall  .lastinstall.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${install_dir} ; then 
    (cd ${rcsdir} ; ln -s "${rel_install_dir}" .lastinstall)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/lib ; then
    \rm -f ${rcsdir}/lib >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/lib ; then
    (cd ${rcsdir} ; mv lib .lib.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/lib ; then
    (cd ${rcsdir} ; ln -s ".lastinstall/lib" .)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/bin ; then
    \rm -f ${rcsdir}/bin >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/bin ; then
    (cd ${rcsdir} ; mv bin .bin.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/bin ; then
    (cd ${rcsdir} ; ln -s ".lastinstall/bin" .)
fi

if ${test_cmd} ${LINK_FLAG} ${rcsdir}/include ; then
    \rm -f ${rcsdir}/include >/dev/null 2>/dev/null ;
fi

if ${test_cmd} -d ${rcsdir}/include ; then
    (cd ${rcsdir} ; mv include .include.${pid} ) >/dev/null 2>/dev/null;
fi

if ${test_cmd} -d ${rcsdir}/.lastinstall/include ; then
    (cd ${rcsdir} ; ln -s ".lastinstall/include" .)
fi

if ${test_cmd} -f ${install_dir}/lib/librcsrtai.a ; then 
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/realtime ; then
	\rm -f ${rcsdir}/plat/realtime;
    fi
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/rtai ; then
	\rm -f ${rcsdir}/plat/rtai;
    fi
    if ${test_cmd} -d ${rcsdir}/plat/realtime ; then
	(cd  ${rcsdir}/plat; mv realtime ".realtime.${pid}" )
    fi
    if ${test_cmd} -d ${rcsdir}/plat/rtai ; then
	(cd  ${rcsdir}/plat; mv rtai ".rtai.${pid}" )
    fi
    if ${test_cmd} -d ${rcsdir}/plat/.multiplatbuild_rtai ; then
	( \rm -r -f ${rcsdir}/plat/.multiplatbuild_rtai ) >/dev/null 2>/dev/null
    fi
    ( cd ${rcsdir}/plat ; \
	mkdir .multiplatbuild_rtai; \
	ln -s .multiplatbuild_rtai rtai ; \
	ln -s .multiplatbuild_rtai realtime ; \
	cd .multiplatbuild_rtai; \
	mkdir lib; \
	ln -s "../../${rel_install_dir}/include" .; \
	cd lib; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtai.a" .; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtai.a" librcs.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtai.a" librcsd.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtai.a" libposemath.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtai.a" libpm.a ; \
	)

elif ${test_cmd} -f ${install_dir}/lib/librcsrtl.a ; then 
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/realtime ; then
	\rm -f ${rcsdir}/plat/realtime;
    fi
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/rtlinux ; then
	\rm -f ${rcsdir}/plat/rtlinux;
    fi
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/rtlinux_3_0 ; then
	\rm -f ${rcsdir}/plat/rtlinux_3_0;
    fi
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/rtlinux_3_2 ; then
	\rm -f ${rcsdir}/plat/rtlinux_3_2;
    fi
    if ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/rtlinux_2_2 ; then
	\rm -f ${rcsdir}/plat/rtlinux_2_2;
    fi
    if ${test_cmd} -d ${rcsdir}/plat/realtime ; then
	(cd  ${rcsdir}/plat; mv realtime ".realtime.${pid}" )
    fi
    if ${test_cmd} -d ${rcsdir}/plat/rtlinux ; then
	(cd  ${rcsdir}/plat; mv rtlinux ".rtlinux.${pid}" )
    fi
    if ${test_cmd} -d ${rcsdir}/plat/.multiplatbuild_rtlinux ; then
	( \rm -r -f ${rcsdir}/plat/.multiplatbuild_rtlinux ) >/dev/null 2>/dev/null
    fi
    ( cd ${rcsdir}/plat ; \
	mkdir .multiplatbuild_rtlinux; \
	ln -s .multiplatbuild_rtlinux rtlinux ; \
	if test \! -d rtlinux_3_0 ; then ln -s .multiplatbuild_rtlinux rtlinux_3_0 ; fi ; \
	if test \! -d rtlinux_3_2 ; then ln -s .multiplatbuild_rtlinux rtlinux_3_2 ; fi ; \
	if test \! -d rtlinux_2_2 ; then ln -s .multiplatbuild_rtlinux rtlinux_2_2 ; fi ; \
	ln -s .multiplatbuild_rtlinux realtime ; \
	cd .multiplatbuild_rtlinux; \
	mkdir lib; \
	ln -s "../../${rel_install_dir}/include" .; \
	cd lib; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtl.a" .; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtl.a" librcs.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtl.a" librcsd.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtl.a" libposemath.a ; \
	ln -s "../../../${rel_install_dir}/lib/librcsrtl.a" libpm.a ; \
	)
elif ${test_cmd} -f ${install_dir}/lib/libposemath.a ; then
     if ${test_cmd} ${LINK_FLAG} ${install_dir}/lib/libpm.a; then
	\rm -f ${install_dir}/lib/libpm.a;
     fi
     (cd ${install_dir}/lib ; \
         ln -s libposemath.a libpm.a; ) 2>/dev/null >/dev/null
fi


if ${test_cmd} -f "${rcsdir}/${rel_build_dir}/plat/java/lib/rcs.jar" -o -f "${rcsdir}/${rel_build_dir}/plat/java/lib/rcs/RCS_VERSION.class" ; then
        if ${test_cmd} \! -d ${rcsdir}/plat/java ; then
           (cd ${rcsdir}/plat ; ln -s ../${rel_build_dir}/plat/java . ) ; 
        elif ${test_cmd} ${LINK_FLAG} ${rcsdir}/plat/java ; then
           (cd ${rcsdir}/plat/; rm java ;  ln -s ../${rel_build_dir}/plat/java . ) ; 
        else
           (cd ${rcsdir}/plat; mv java .java_${pid} ; ln -s ../${rel_build_dir}/plat/java . ) ;
        fi
fi

if ${test_cmd} -f ${home}/.tmp/make${pid}.sh ; then
 rm ${home}/.tmp/make${pid}.sh
fi

EOF

chmod a+x ${temp_dir}/do_rcslib_post_install_${pid}.sh

ORIG_CFLAGS=${CFLAGS}
ORIG_CXXFLAGS=${CXXFLAGS}

if test "x${CFLAGS}" != "x" ; then
    if echo "${CFLAGS}" | grep -- -Werror ; then
	CFLAGS=`echo " ${CFLAGS}" | sed s/-Werror//`
	if test "x${CXXFLAGS}" != "x" ; then
	    CXXFLAGS=`echo " ${CXXFLAGS}" | sed s/-Werror//`;
	fi
	ADD_WERROR=yes;
    fi
fi

if test "x${MAKE_CMD}" = "x" ; then
	MAKE_CMD=make;
fi
    


if ${test_cmd} $# -lt 1 ; then

    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}

    if ${test_cmd} \! -f Makefile  ; then
	( if test "x${DEBUG}" != x ; then set -x ; fi;  ${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} ${CONFIG_OPTS} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127 
	\rm -f ${build_dir}/*.{a,la,so}
	\rm -f ${build_dir}/.libs/*.{a,la,so}

	if test -f "${rcsdir}/rcs_config.h" -a -d "${rcsdir}/lastconfig/" -a -w "$rcsdir}/lastconfig/" ; then
	    cp -p "${rcsdir}/rcs_config.h"  "${rcsdir}/lastconfig/";
	fi

	if test -f "${rcsdir}/config.log" -a -d "${rcsdir}/lastconfig/" ; then
	    cp -p "${rcsdir}/config.log"  "${rcsdir}/lastconfig/";
	fi
    fi
    touch ${rcsdir}/src/rcsvers.c
    make_args="${MULTIPLAT_MAKEFLAGS}"
    echo make_args=${make_args}
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then 
	echo "set -x" >> ${temp_dir}/make${pid}.sh; 
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != "x"; then 
	ls -l  ${temp_dir}/make${pid}.sh
	cat  ${temp_dir}/make${pid}.sh
    fi
    \rm -f ${temp_dir}/MAKE_FAILED
    (( if test "x${DEBUG}" != x ; then set -x ; fi; /bin/sh ${temp_dir}/make${pid}.sh  2>&1) | tee -a  "${make_log}") || (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 127;
    fi
    make_args="${MULTIPLAT_MAKEFLAGS} install"
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then 
	echo "set -x" >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh	
    if test "x${DEBUG}" != "x"; then 
	ls -l  ${temp_dir}/make${pid}.sh
	cat  ${temp_dir}/make${pid}.sh
    fi
    \rm -f ${temp_dir}/MAKE_FAILED;
    (( if test "x${DEBUG}" != x ; then set -x ; fi; /bin/sh ${temp_dir}/make${pid}.sh  2>&1) | tee -a  "${make_log}") || (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 127;
    fi
    mkdir -p ${install_dir}/src
    cp "${builddir}"/*.h "${install_dir}/src" 2>/dev/null
    cp "${builddir}"/*.log "${install_dir}/src"
    (cd ${rcsdir} ; 
	find src -type f -name \*.c -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.cc -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.cpp -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.h -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.hh -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.hpp -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.ads -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.adb -exec cp \{\} "${install_dir}/src" \; ;
	find src -type f -name \*.java -exec cp \{\} "${install_dir}/src" \; ;
	);

elif ${test_cmd}  "x${1}" = "xconfig" ; then

    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}



    if ${test_cmd} "x${2}" = "x--help" ; then
	set +x;
    fi
    if ${test_cmd} $# -gt 9  ; then
	echo "TOO many arguments"
	echo "TOO many arguments" >&2
	exit 1
    fi
    if test -d ~/.tmp; then
	\rm -rf ~/.tmp;
	mkdir ~/.tmp;
    fi
    if ${test_cmd} -f Makefile -a "x${2}" != "x--help" ; then
	${MAKE_CMD} distclean || exit 1
    fi
    if ${test_cmd} "x${2}" != "x--help" ; then
	echo $2 $3 $4 $5 $6 $7 $8 $9 > "${HOME}/.${HOST}.${PLAT}.rcslib_configure_options"
	cp ${HOME}/.${HOST}.${PLAT}.rcslib_configure_options ${build_dir}
	set > ${build_dir}/.config_set_env.${pid}
    fi
    config_command="${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} $2 $3 $4 $5 $6 $7 $8 $9"
    if ${test_cmd} "x${2}" != "x--help" ; then
	echo ${config_command} > ${home}/.rcslib_config_command.${HOST}
	echo ${config_command} > ${build_dir}/.config_command.${pid}
	if test "x${DEBUG}" != "x"; then 
	    ls -l ${build_dir}/.config_command.${pid}
	    cat ${build_dir}/.config_command.${pid}
	fi
    fi
    ( if test "x${DEBUG}" != x ; then set -x ; fi; ${config_command} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127
    if ${test_cmd} "x${2}" = "x--help" ; then
	exit 1;
    fi
    earg_list="";
    for carg in ${config_command} ; do
	if echo "${carg}" | grep -- "--enable" ; then
	    if test "x${carg}" != "x--enable-shared" -a "x${carg}" != "x--enable-static" ; then 
		carg_converted=`echo ${carg} | sed s/-/_/g`
		earg_list="${earg_list} ${carg_converted}";
	    fi
	fi
    done
    grep set_ ${build_dir}/config.log | sed s/-/_/g >${temp_dir}/config_sets${pid}.log
    if test "x${earg_list}" != "x"; then
	for earg in ${earg_list} ; do
	    if test "x${earg}" != "x" ; then
		if grep -i -- "set_${earg}" ${temp_dir}/config_sets${pid}.log; then
		    echo "${earg} found set in config.log" .
		else
		    echo "set_${earg} not found in config.log"
		    echo "set_${earg} not found in config.log" >&2
		    mv ${build_dir}/Makefile ${build_dir}/Makefile.trashed.${pid}
		    exit 1
		fi
	    fi
	done
    fi
    earg_list="";
    for carg in ${config_command} ; do
	if test "x${carg}" != "x--disable-shared" -a "x${carg}" != "x--disable-static" ; then 
	    if echo "${carg}" | grep -- "--disable" ; then
		carg_converted=`echo ${carg} | sed s/--dis/--en/ | sed s/-/_/g`
		earg_list="${earg_list} ${carg_converted}";
	    fi
	fi
    done
    if test "x${earg_list}" != "x"; then
	for earg in ${earg_list} ; do
	    if test "x${earg}" != "x" ; then
		if grep -i -- "set_${earg}" ${temp_dir}/config_sets${pid}.log; then
		    echo "${earg} found set in config.log" .
		else
		    echo "${earg} not found in config.log"
		    echo "${earg} not found in config.log" >&2
		    mv ${build_dir}/Makefile ${build_dir}/Makefile.trashed.${pid}
		    exit 1
		fi
	    fi
	done
    fi

		    
    \rm -f ${build_dir}/*.{a,la,so}
    \rm -f ${build_dir}/.libs/*.{a,la,so}

elif ${test_cmd}  "x${1}" = "xmake" ; then


    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}



    if ${test_cmd} \! -f Makefile; then
	cp ${HOME}/.${HOST}.${PLAT}.rcslib_configure_options ${build_dir}/.${HOST}.${PLAT}.rcslib_configure_options.${pid}
	set > ${build_dir}/.config_set_env.${pid}
	config_command="${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} ${CONFIG_OPTS}"
	echo ${config_command} > ${build_dir}/.config_command.${pid}
	( if test "x${DEBUG}" != x ; then set -x ; fi;  ${config_command} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127
	\rm -f ${build_dir}/*.{a,la,so}
	\rm -f ${build_dir}/.libs/*.{a,la,so}
    fi
    touch ${rcsdir}/src/rcsvers.c

    for arg in $* ; do
	if echo "$arg" | grep "=" >/dev/null ; then
	    eval ${arg}
	fi
    done

    if test "x${prefix}" != "x" -a -f ${build_dir}/librcs.la ; then
	rm ${build_dir}/librcs.la;
    fi

    make_args="${MULTIPLAT_MAKEFLAGS} ${2} ${3} ${4} ${5} ${6} ${7} ${8}"
    echo make_args=${make_args}
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then 
	echo "set -x" >> ${temp_dir}/make${pid}.sh;
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != "x"; then 
	ls -l  ${temp_dir}/make${pid}.sh
	cat  ${temp_dir}/make${pid}.sh
    fi
    \rm -f ${temp_dir}/MAKE_FAILED
    (( if test "x${DEBUG}" != x ; then set -x ; fi; /bin/sh ${temp_dir}/make${pid}.sh 2>&1 )| tee -a  "${make_log}") || (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 127;
    fi
    if test "x${DEBUG}" != x ; then
	set -x
	set
    fi
    mkdir -p ${build_dir}/src
    cp "${builddir}"/*.h "${build_dir}/src" 
    cp "${builddir}"/*.log "${build_dir}/src" 
    ( cd ${rcsdir} ;
	find src -type f -name \*.c -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.cc -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.cpp -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.h -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.hh -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.hpp -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.ads -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.adb -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.java -exec cp \{\} "${build_dir}/src" \; ;
	)

    if test "x${prefix}" != "x" ; then
	prefix_dir="${prefix}"
	install_dir="${prefix}"
	mkdir -p ${prefix}/src
	mkdir -p ${prefix}/plat/java/lib
	if test -d ${rcsdir}/plat/java/lib -a -d ${prefix}/plat/java/lib ; then
	    cp ${rcsdir}/plat/java/lib/*.jar ${prefix}/plat/java/lib
	    if test -d ${rcsdir}/plat/java/lib/rcs ; then
		cp -r ${rcsdir}/plat/java/lib/rcs ${prefix}/plat/java/lib;
	    fi
	    if test -d ${rcsdir}/plat/java/lib/diagapplet ; then
		cp -r ${rcsdir}/plat/java/lib/diagapplet ${prefix}/plat/java/lib;
	    fi
	fi
	if test -d ${build_dir}/plat/java/lib  -a -d ${prefix}/plat/java/lib ; then
	    cp ${build_dir}/plat/java/lib/*.jar ${prefix}/plat/java/lib
	    if test -d ${build_dir}/plat/java/lib/rcs ; then
		cp -r ${build_dir}/plat/java/lib/rcs ${prefix}/plat/java/lib;
	    fi
	    if test -d ${build_dir}/plat/java/lib/diagapplet ; then	    
		cp -r ${build_dir}/plat/java/lib/diagapplet ${prefix}/plat/java/lib;
	    fi
	fi
	cp "${build_dir}"/*.h "${prefix_dir}/src" 
	cp "${build_dir}"/*.log "${prefix_dir}/src"
	( cd ${rcsdir} ; 
	    find src -type f -name \*.c -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.cc -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.cpp -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.h -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.hh -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.hpp -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.ads -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.adb -exec cp \{\} "${prefix_dir}/src" \; ;
	    find src -type f -name \*.java -exec cp \{\} "${prefix_dir}/src" \; ;
	    )
    fi

elif ${test_cmd}  "x${1}" = "xcheck" ; then

    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}




    if ${test_cmd} \! -f Makefile; then
	cp ${HOME}/.${HOST}.${PLAT}.rcslib_configure_options ${build_dir}/.${HOST}.${PLAT}.rcslib_configure_options.${pid}
	set > ${build_dir}/.config_set_env.${pid}
	config_command="${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} ${CONFIG_OPTS}"
	echo ${config_command} > ${build_dir}/.config_command.${pid}
	( if test "x${DEBUG}" != x ; then set -x ; fi;  ${config_command} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127
	\rm -f ${build_dir}/*.{a,la,so}
	\rm -f ${build_dir}/.libs/*.{a,la,so}
    fi
    touch ${rcsdir}/src/rcsvers.c
    make_args="${MULTIPLAT_MAKEFLAGS}"
    echo make_args=${make_args}
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then 
	echo "set -x" >> ${temp_dir}/make${pid}.sh;
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != "x"; then 
	ls -l  ${temp_dir}/make${pid}.sh
	cat  ${temp_dir}/make${pid}.sh
    fi
    \rm -f ${temp_dir}/MAKE_FAILED
    (( if test "x${DEBUG}" != x ; then set -x ; fi;  /bin/sh ${temp_dir}/make${pid}.sh 2>&1) | tee -a  "${make_log}") || (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 127;
    fi
    make_args="${MULTIPLAT_MAKEFLAGS} install"
    echo make_args=${make_args}
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then
	echo "set -x" >> ${temp_dir}/make${pid}.sh;
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh
    \rm -f ${temp_dir}/MAKE_FAILED
    (( if test "x${DEBUG}" != x ; then set -x ; fi; /bin/sh ${temp_dir}/make${pid}.sh 2>&1 ) | tee -a  "${make_log}") || (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 127;
    fi
    ( ${temp_dir}/do_rcslib_post_install_${pid}.sh || true )
    make_args="${MULTIPLAT_MAKEFLAGS} check"
    echo make_args=${make_args}
    echo "#! /bin/sh" > ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != x ; then
	echo "set -x" >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CFLAGS}" != "x" ; then
	echo "CFLAGS=\"${ORIG_CFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" ; then
	echo "CXXFLAGS=\"${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
	echo export CXXFLAGS >> ${temp_dir}/make${pid}.sh
    fi
    if test "x${ORIG_CXXFLAGS}" != "x" -o "x${ORIG_CFLAGS}" != "x" ; then
	echo ${MAKE_CMD} ${make_args} "\"CFLAGS=${ORIG_CFLAGS}\"" "\"CXXFLAGS=${ORIG_CXXFLAGS}\"" >> ${temp_dir}/make${pid}.sh
    else
	echo ${MAKE_CMD} ${make_args} " || exit 1 " >> ${temp_dir}/make${pid}.sh
    fi
    echo "sts=\$?" >> ${temp_dir}/make${pid}.sh
    echo "if test \"x\${sts}\" != \"x0\" ; then" >> ${temp_dir}/make${pid}.sh 
    echo "touch ${temp_dir}/MAKE_FAILED;" >> ${temp_dir}/make${pid}.sh 
    echo "fi" >> ${temp_dir}/make${pid}.sh 
    echo "echo \$0 returned \${sts}" >> ${temp_dir}/make${pid}.sh
    echo "exit \${sts}" >> ${temp_dir}/make${pid}.sh
    chmod a+x ${temp_dir}/make${pid}.sh
    if test "x${DEBUG}" != "x"; then 
	ls -l  ${temp_dir}/make${pid}.sh
	cat  ${temp_dir}/make${pid}.sh
    fi
    \rm -f ${temp_dir}/MAKE_FAILED;
    (( if test "x${DEBUG}" != x ; then set -x ; fi; /bin/sh ${temp_dir}/make${pid}.sh 2>&1 ) | tee -a  "${make_log}")|| (echo " ${MAKE_CMD} ${make_args} failed." >&2 ; exit 127 ) || exit 127
    if test -f ${temp_dir}/MAKE_FAILED ; then
	exit 1;
    fi
    mkdir -p ${build_dir}/src
    cp "${builddir}"/*.h "${build_dir}/src" 
    cp "${builddir}"/*.log "${build_dir}/src" 
    ( cd ${rcsdir} ;
	find src -type f -name \*.c -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.cc -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.cpp -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.h -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.hh -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.hpp -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.ads -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.adb -exec cp \{\} "${build_dir}/src" \; ;
	find src -type f -name \*.java -exec cp \{\} "${build_dir}/src" \; ;
    )

elif ${test_cmd}  "x${1}" = "xdistclean" ; then
    if test -d ${install_dir} ; then
	\rm -r -f ${install_dir} ; 
    fi
    if test -d ${build_dir} ; then
	\rm -r -f ${build_dir} ; 
    fi
    if test -d ${rcsdir}/plat/${PLAT} ; then
	\rm -r -f ${rcsdir}/plat/${PLAT};
    fi
    
    \rm -rf ${rcsdir}/.lastinstall;
    \rm -rf ${rcsdir}/.lastbuild;
    \rm -rf ${rcsdir}/lib;
    \rm -rf ${rcsdir}/bin;
    \rm -rf ${rcsdir}/include;
    exit 0;

elif ${test_cmd}  "x${1}" = "xclean" ; then
    if test -d ${install_dir} ; then
	\rm -r -f ${install_dir} ; 
    fi

    if test -d ${rcsdir}/plat/${PLAT} ; then
	\rm -r -f ${rcsdir}/plat/${PLAT};
    fi
    
    \rm -rf ${rcsdir}/.lastinstall;
    \rm -rf ${rcsdir}/lib;
    \rm -rf ${rcsdir}/bin;
    \rm -rf ${rcsdir}/include;

    if test -d ${build_dir} ; then
	make -C ${build_dir} clean ; 
    fi
    exit 0;

elif ${test_cmd}  "x${1}" = "xdist" ; then


    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}

    if ${test_cmd} \! -f Makefile  ; then
	( if test "x${DEBUG}" != x ; then set -x ; fi;  ${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} ${CONFIG_OPTS} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127 
	\rm -f ${build_dir}/*.{a,la,so}
	\rm -f ${build_dir}/.libs/*.{a,la,so}
    fi
    SAFE_PATH="/bin:/usr/bin";
    if test -d /tmp/distcheckhome ; then
	\rm -r -f /tmp/distcheckhome
    fi
    mkdir /tmp/distcheckhome
    ( ( if test "x${DEBUG}" != x ; then set -x ; fi;  env -i "PATH=${SAFE_PATH}" "HOME=/tmp/distcheckhome" ${MAKE_CMD} dist  2>&1 ) | tee dist.${pid}.log) || (echo "${MAKE_CMD} dist failed." >&2 ; exit 127 ) || exit 127 
    if ${test_cmd} "x${build_dir}" != "x${rcsdir}" ; then
	( if test "x${DEBUG}" != x ; then set -x ; fi; cd ${build_dir} ;
	    for archive in rcs*.tar.gz ; do
		echo "archive=${archive}"
		if ${test_cmd} "x${archive}" != "x" ; then
		    if ${test_cmd} \! -f "${rcsdir}/${archive}" -a -f "./${archive}" ; then 
			cp "${archive}" "${rcsdir}";
		    elif ${test_cmd} -f "./${archive}" -a -f "${rcsdir}/${archive}" -a "./${archive}" -nt "${rcsdir}/${archive}"; then
			mv "${rcsdir}/${archive}" ${temp_dir};
			cp "${archive}" "${rcsdir}";
		    fi
	       fi
	   done
       )
    fi


elif ${test_cmd}  "x${1}" = "xdistcheck" ; then


    echo CFLAGS="${CFLAGS}"
    echo CXXFLAGS="${CXXFLAGS}"
    echo CPPFLAGS="${CPPFLAGS}"
    echo LDFLAGS="${LDFLAGS}"
    echo CC="${CC}"
    echo CXX="${CXX}"
    echo MULTIPLAT_MAKEFLAGS="${MULTIPLAT_MAKEFLAGS}"
    echo MAKE_CMD=${MAKE_CMD}



    \rm -rf /tmp/tni_*
    \rm -rf /tmp/*.nml
    \rm -rf /tmp/*.log
    \rm -rf /tmp/jdk_*
    \rm -rf /tmp/kill*sh
    \rm -rf /tmp/jre_*
    \rm -rf /tmp/getrcs.html*
    \rm -rf /tmp/distcheckhome*
    if ${test_cmd} \! -f Makefile  ; then
	( if test "x${DEBUG}" != x ; then set -x ; fi; ${rcsdir}/configure --prefix=${install_dir} ${host_arg_to_add} ${CONFIG_OPTS} ) || (echo "configure failed." >&2 ; exit 127 ) || exit 127 
	\rm -f ${build_dir}/*.{a,la,so}
	\rm -f ${build_dir}/.libs/*.{a,la,so}
    fi
    SAFE_PATH="/bin:/usr/bin";
    if test -d /tmp/distcheckhome ; then
	\rm -r -f /tmp/distcheckhome
    fi
    mkdir /tmp/distcheckhome
    (( if test "x${DEBUG}" != x ; then set -x ; fi ; cd /tmp/distcheckhome/ ; "${rcsdir}/configure" ; pwd ; env -i "PATH=${SAFE_PATH}" "HOME=/tmp/distcheckhome" ${MAKE_CMD} distcheck  2>&1 ) | tee distcheck.${pid}.log) || (echo "${MAKE_CMD} distcheck failed." >&2 ; exit 127 ) || exit 127 
    if ${test_cmd} "x${build_dir}" != "x${rcsdir}" ; then
	( if test "x${DEBUG}" != x ; then set -x ; fi;  cd /tmp/distcheckhome ;
	    for archive in rcs*.tar.gz ; do
		echo "archive=${archive}";
		if ${test_cmd} "x${archive}" != "x" ; then
		    if ${test_cmd} \! -f "${rcsdir}/${archive}" -a -f "./${archive}" ; then 
			cp "${archive}" "${rcsdir}";
		    elif ${test_cmd} -f "./${archive}" -a -f "${rcsdir}/${archive}" -a "./${archive}" -nt "${rcsdir}/${archive}"; then
			mv "${rcsdir}/${archive}" ${temp_dir};
			cp "${archive}" "${rcsdir}";
		    fi
	       fi
	   done
       )
    fi
else
    echo "First argument (${1}) not recognized. Use one of distcheck, dist, clean, distclean, check, make or config."
    exit 1 
fi


${temp_dir}/do_rcslib_post_install_${pid}.sh

echo "Any files created by this script in ${temp_dir} are temporary and may be deleted."

if test -f "${rcsdir}/.lastbuild/rcs_config.h" -a -d "${rcsdir}/lastconfig/" ; then
    cp -p "${rcsdir}/.lastbuild/rcs_config.h"  "${rcsdir}/lastconfig/";
fi

if test -f "${rcsdir}/.lastbuild/config.log" -a -d "${rcsdir}/lastconfig/" ; then
    cp -p "${rcsdir}/.lastbuild/config.log"  "${rcsdir}/lastconfig/";
fi

if test -f "${rcsdir}/lastconfig/make.log" -a -d "${temp_dir}" ; then
    cp -p "${rcsdir}/lastconfig/make.log" "${temp_dir}/make_${pid}.log";
fi

echo "rcsdir=${rcsdir}" >&2 
echo "install_dir=${install_dir}" >&2 
echo "build_dir=${build_dir}" >&2 
echo "PLAT=${PLAT}" >&2

if test -f "${HOME}/.rcslib_split_debug" ; then
    (cd "${install_dir}/lib" ; "${rcsdir}/etc/splitdebug.sh" `find . -name lib\*so\* -not -name \*a -not -name \*.debug -not -name \*.h -not -name libtool  -perm /111 -type f` );
    (cd "${install_dir}/bin" ; "${rcsdir}/etc/splitdebug.sh" `find . -not -name \*.sh -not -name \*.bash -not -name \*.jar -not -name \*.debug -not -name \*.h -not -name libtool -perm /111 -type f` );
fi


exit 0







