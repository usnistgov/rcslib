#! /bin/sh

set -x


if test "x${PLAT}" = "x" ; then
    echo "Please set PLAT environment variable."
    exit 1
fi

if test "x${RCSLIB_DIR}" = "x" ; then
    echo "Please set RCSLIB_DIR environment variable."
    exit 1
fi

LD_LIBRARY_PATH="${RCSLIB_DIR}/lib:${LD_LIBRARY_PATH}";

export LD_LIBRARY_PATH;

\rm -f *.log
\rm -f cgtest*.xml
\rm -f CMS_HEADER_*.xml
\rm -f EXAMPLE_*.xml
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
CGTESTWRITEIFREAD_LOG_FILE=cgtestwriteifread.log
export CGTESTWRITEIFREAD_LOG_FILE
#DEBUG_MEMORY=1
#export DEBUG_MEMORY

# make PLAT=${PLAT} -C .. FORCE_DEPEND=1 depend || (echo FAILED;exit 255) || exit 255
#make PLAT=${PLAT} -C ..  DISABLE_DEFAULT_OPTIMIZE=1   || (echo FAILED;exit 255) || exit 255
#make PLAT=${PLAT} -C ../java PLAT=java || (echo FAILED;exit 255) || exit 255


if test -x ${RCSLIB_DIR}plat/${PLAT}/bin/CodeGen ; then
    ${RCSLIB_DIR}plat/${PLAT}/bin/CodeGen update_with_name=true cgtester.hh dla_length_init=2 || (echo FAILED;exit 255) || exit 25

else 
    java -jar ${RCSLIB_DIR}plat/java/lib/CodeGenCmdLine.jar update_with_name=true generate_symbol_lookups=true cgtester.hh dla_length_init=2 || (echo FAILED;exit 255) || exit 255
fi


ENABLE_RCS_XML=1;
export ENABLE_RCS_XML;

LOCAL_CFLAGS=-DENABLE_RCS_XML;
export LOCAL_CFLAGS;

#make PLAT=${PLAT} headers sources depend ENABLE_RCS_XML=1 || (echo FAILED;exit 255) || exit 255
make PLAT=${PLAT} DISABLE_DEFAULT_OPTIMIZE=1 ENABLE_RCS_XML=1  xmlout xmlschemaout cgtestsvr1 cgtestsvr2 cgtestread cgtestwrite cgtestwriteifread || (echo FAILED;exit 255) || exit 255
${RCSLIB_DIR}plat/${PLAT}/bin/xmlout || (echo FAILED;exit 255) || exit 255
\rm -f xmlschemaout.xsd
${RCSLIB_DIR}plat/${PLAT}/bin/xmlschemaout || (echo FAILED;exit 255) || exit 255 

PATH=${PATH}:${HOME}/xvalid-3_8_1/bin/;

export PATH;
which xsdvalid;

if test -x `which xsdvalid` ; then
    xsdvalid -s xmlschemaout.xsd xmlout.xml || (echo FAILED;exit 255) || exit 255 
else
    echo "XML Schema Validation skipped : xsdvalid  not installed."
fi

which xmllint
if test -x `which xmllint` ; then
    xmllint --noout --schema "http://www.w3.org/2001/XMLSchema.xsd" xmlschemaout.xsd || (echo FAILED;exit 255) || exit 255 
    xmllint --noout --schema xmlschemaout.xsd xmlout.xml || (echo FAILED;exit 255) || exit 255 
else
    echo "XML Schema Validation skipped : xmllint not installed."
fi

killall -INT cgtestsvr1
killall -INT cgtestsvr2
sleep 2
killall -KILL cgtestsvr1
killall -KILL cgtestsvr2

${RCSLIB_DIR}plat/${PLAT}/bin/cgtestsvr1 &
jobtokill=$!
${RCSLIB_DIR}plat/${PLAT}/bin/cgtestsvr2 &
jobtokill2=$!

sleep 5
${RCSLIB_DIR}plat/${PLAT}/bin/cgtestwrite || (echo FAILED;exit 255) || exit 255
sleep 2 
${RCSLIB_DIR}plat/${PLAT}/bin/cgtestread  || (echo FAILED;exit 255) || exit 255

${RCSLIB_DIR}plat/${PLAT}/bin/cgtestwriteifread X00 || (echo FAILED;exit 255) || exit 255
${RCSLIB_DIR}plat/${PLAT}/bin/cgtestwriteifread 0X0 || (echo FAILED;exit 255) || exit 255 

echo "Next two cgtestwriteifreads should fail since no read was done."

${RCSLIB_DIR}plat/${PLAT}/bin/cgtestwriteifread X00 && echo FAILED && exit 255
${RCSLIB_DIR}plat/${PLAT}/bin/cgtestwriteifread 0X0 && echo FAILED && exit 255

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


grep '!ERROR!' *.log || echo "Everything worked!!!"

