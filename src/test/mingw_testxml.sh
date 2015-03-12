#! /bin/sh

set -x
PLAT=mingw32
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


make -C ..  PLAT=mingw32 || exit 255
#make -C ../java PLAT=java || exit 255
java -jar ../../../rcslib/plat/java/lib/CodeGen.jar update_with_name=true display_on=false cgtester.hh || exit 255
make  PLAT=mingw32 xmlout xmlschemaout cgtestsvr1 cgtestsvr2 cgtestread cgtestwrite || exit 255
\rm -f xmlschemaout.xsd xmlout.xml
wine ../../../rcslib/plat/${PLAT}/bin/xmlout.exe || exit 255
wine ../../../rcslib/plat/${PLAT}/bin/xmlschemaout.exe || exit 255 


if test "x${XSDVALID}" = "x" ; then

if test -x ../../../xsdvalid-21/xsdvalid ; then
    XSDVALID=../../../xsdvalid-21/xsdvalid ;  
elif test -x ../../../../xsdvalid-21/xsdvalid ; then
    XSDVALID=../../../../xsdvalid-21/xsdvalid ;  
elif test -x ~/xsdvalid-21/xsdvalid ; then
    XSDVALID=~/xsdvalid-21/xsdvalid ;
fi

fi

if test "x${XSDVALID}" != "x" ; then

    ${XSDVALID} -s xmlschemaout.xsd xmlout.xml || exit 255 

fi

killall -INT wine

killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

killall -INT cgtestsvr1.exe
killall -INT cgtestsvr2.exe
sleep 2
killall -KILL cgtestsvr1.exe
killall -KILL cgtestsvr2.exe

killall -KILL wine

wine ../../../rcslib/plat/${PLAT}/bin/cgtestsvr1.exe >wine_cgtestsvr1.out 2>wine_cgtestsvr1.err &
jobtokill=$!
wine ../../../rcslib/plat/${PLAT}/bin/cgtestsvr2.exe >wine_cgtestsvr2.out 2>wine_cgtestsvr2.err  &
jobtokill2=$!

sleep 5
wine ../../../rcslib/plat/${PLAT}/bin/cgtestwrite.exe  && \
sleep 2 && \
wine ../../../rcslib/plat/${PLAT}/bin/cgtestread.exe

kill -INT $jobtokill
kill -INT $jobtokill2

sleep 1

kill -KILL $jobtokill
kill -KILL $jobtokill2


killall -INT cgtestsvr1.exe
killall -INT cgtestsvr2.exe
sleep 2
killall -KILL cgtestsvr1.exe
killall -KILL cgtestsvr2.exe


killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2



