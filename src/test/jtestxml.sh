#! /bin/sh

set -x

if [ x${PLAT} = x ] ; then
    PLAT=`../../etc/platname`
fi
export PLAT

\rm -f *.log
\rm -f *.xml
\rm -f *.xml.~*~

RCS_DEBUG_FLAG=0xFFFFFFFF
export RCS_DEBUG_FLAG
XMLOUT_LOG_FILE=xmlout.log
export XMLOUT_LOG_FILE
XMLSCHEMAOUT_LOG_FILE=xmlschemaout.log
export XMLSCHEMAOUT_LOG_FILE
CGTESTSVR1_LOG_FILE=cgtestsvr1.log
export CGTESTSVR1_LOG_FILE
CGTESTSVR2_LOG_FILE=cgtestsvr2.log
export CGTESTSVR2_LOG_FILE
CGTESTREAD_LOG_FILE=cgtestread.log
export CGTESTREAD_LOG_FILE
CGTESTWRITE_LOG_FILE=cgtestwrite.log
export CGTESTWRITE_LOG_FILE
#DEBUG_MEMORY=1
#export DEBUG_MEMORY


killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

if test "x${REBUILD}" = "xALL" ;  then

    make PLAT=${PLAT} -C ..  || (echo FAILED;exit 255) || exit 255 
    make -C ../java/rcs/nml PLAT=java || (echo FAILED;exit 255) || exit 255 
    rm ../../plat/java/lib/rcs.jar
    make -C ../java/rcs rcs.jar PLAT=java || (echo FAILED;exit 255) || exit 255 
    make -C ../java/diagapplet/CodeGen PLAT=java || (echo FAILED;exit 255) || exit 255 

fi

if test "x${REBUILD}" = "xLOCAL" -o "x${REBUILD}" = "xALL" ; then

    java -jar ../../../rcslib/plat/java/lib/CodeGen.jar update_with_name=true display_on=false cgtester.hh || (echo FAILED;exit 255) || exit 255 
    rm ../../plat/${PLAT}/bin/cgtest*
    rm ../../plat/${PLAT}/bin/xml*
    rm ./*.class
    javac -classpath ../../plat/java/lib/rcs.jar *.java || (echo FAILED;exit 255) || exit 255 
    make PLAT=${PLAT}  headers sources depend xmlout xmlschemaout cgtestsvr1 cgtestsvr2 cgtestread cgtestwrite || (echo FAILED;exit 255) || exit 255 

fi


if test "x${XSDVALID}" = "x" ; then

if test -x ../../../xsdvalid-21/xsdvalid ; then
    XSDVALID=../../../xsdvalid-21/xsdvalid ;  
elif test -x ../../../../xsdvalid-21/xsdvalid ; then
    XSDVALID=../../../../xsdvalid-21/xsdvalid ;  
elif test -x ~/xsdvalid-21/xsdvalid ; then
    XSDVALID=~/xsdvalid-21/xsdvalid ;
fi

fi

../../../rcslib/plat/${PLAT}/bin/xmlout || (echo FAILED xmlout;exit 255) || exit 255 
\rm -f xmlschemaout.xsd
../../../rcslib/plat/${PLAT}/bin/xmlschemaout || (echo FAILED xmlschemaout;exit 255) || exit 255  

if test "x${XSDVALID}" != "x" ; then
${XSDVALID} -s xmlschemaout.xsd xmlout.xml || (echo FAILED xsdvalid xmlout.xml;exit 255) || exit 255  
fi

java -classpath  ../../plat/java/lib/rcs.jar:. jxmlout >jxmlout.xml || (echo FAILED jxmlout;exit 255) || exit 255  

if test "x${XSDVALID}" != "x" ; then
${XSDVALID} -s xmlschemaout.xsd jxmlout.xml || (echo FAILED xsdvalid jxmlout.xml ;exit 255) || exit 255  
fi

killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

../../plat/${PLAT}/bin/cgtestsvr1 &
jobtokill=$!
../../plat/${PLAT}/bin/cgtestsvr2 &
jobtokill2=$!

java -classpath ../../plat/java/lib/rcs.jar:. exwrite >exwrite.log 2>exwrite.err|| (echo FAILED exwrite;exit 255) || exit 255 
java -classpath ../../plat/java/lib/rcs.jar:. exread >exread.log 2>exread.err|| (echo FAILED exread;exit 255) || exit 255 

../../plat/${PLAT}/bin/cgtestwrite  || (echo FAILED;exit 255) || exit 255 
java -classpath ../../plat/java/lib/rcs.jar:. exread >exread.log 2>exread.err || (echo FAILED exread;exit 255) || exit 255 
java -classpath ../../plat/java/lib/rcs.jar:. exwrite >exread.log 2>exread.err || (echo FAILED exwrite;exit 255) || exit 255 
../../plat/${PLAT}/bin/cgtestread || (echo FAILED;exit 255) || exit 255  

kill -INT $jobtokill
kill -INT $jobtokill2

sleep 1

kill -KILL $jobtokill
kill -KILL $jobtokill2


killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2


grep '!DBGMEM!' *.log
grep '!ERROR!' *.log && echo FAILED && exit 255  

echo SUCCEEDED
exit 0


