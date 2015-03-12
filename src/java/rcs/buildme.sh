#! /bin/sh

set -x
pwd

if test "x${JAVAC}" = x ; then
    JAVAC=javac
fi

if test "x${JAR}" = x ; then
    JAR=jar
fi

topdir=`cd ../../.. ; pwd`


mkdir ${topdir}/plat
mkdir ${topdir}/plat/java/
mkdir ${topdir}/plat/java/lib

javalibdir=${topdir}/plat/java/lib

mkdir ${javalibdir}/rcs
mkdir ${javalibdir}/rcs/nml
mkdir ${javalibdir}/rcs/posemath
mkdir ${javalibdir}/rcs/utils

( cd ${javalibdir}/rcs ; rm *.class )
( cd ${javalibdir}/rcs/nml ; rm *.class )
( cd ${javalibdir}/rcs/posemath ; rm *.class )
( cd ${javalibdir}/rcs/utils ; rm *.class )

./update_ver

${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd utils
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ../nml
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ../posemath
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ..

(set -x ;  cd ${javalibdir}; ${JAR} -cf0 rcs.jar rcs )





