ma#! /bin/sh

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

mkdir ${javalibdir}/diagapplet
mkdir ${javalibdir}/diagapplet/utils
mkdir ${javalibdir}/diagapplet/plotter
mkdir ${javalibdir}/diagapplet/CodeGen

( cd ${javalibdir}/diagapplet ; rm *.class )
( cd ${javalibdir}/diagapplet/utils ; rm *.class )
( cd ${javalibdir}/diagapplet/plotter ; rm *.class )
( cd ${javalibdir}/diagapplet/CodeGen ; rm *.class )

pwd

cd utils
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ../plotter
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ../CodeGen
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java
cd ..
${JAVAC} ${JAVAC_FLAGS} -classpath ${CLASSPATH}:${javalibdir} -d ${javalibdir} *.java

cp CodeGen/CodeGenCmdLineJarInfo.txt ${javalibdir}
cp CodeGen/CodeGenJarInfo.txt ${javalibdir}

( set -x ;  cd ${javalibdir} ; ${JAR} -cmf0 CodeGenCmdLineJarInfo.txt CodeGenCmdLine.jar rcs diagapplet/utils/FakeFastListPanel.class diagapplet/utils/FastListPanelItem.class diagapplet/utils/FastListPanelInterface.class diagapplet/utils/CountButtonInterface.class diagapplet/utils/URLLoadInfoPanelInterface.class diagapplet/CodeGen/ModuleInfoInterface.class diagapplet/CodeGen/StructureTypeInfo.class diagapplet/CodeGen/EnumTypeInfo.class diagapplet/CodeGen/DiagNMLMessageDictionaryInterface.class diagapplet/CodeGen/DiagNMLMessageDictionaryCreatorInterface.class diagapplet/CodeGen/ModuleInfo.class diagapplet/CodeGen/DiagNMLmsg.class diagapplet/CodeGen/DiagNMLMessageDictionary.class diagapplet/CodeGen/DiagNMLMessageDictionaryCreator.class diagapplet/CodeGen/CodeGenCmdLine.class diagapplet/CodeGen/DefinedValue.class diagapplet/CodeGen/BufferInfo.class diagapplet/CodeGen/ChannelInfo.class diagapplet/CodeGen/ServerInfo.class diagapplet/CodeGen/CodeGenCommonInterface.class diagapplet/CodeGen/CodeGenBellRingerInterface.class diagapplet/CodeGen/CodeGenTextAreaInterface.class diagapplet/CodeGen/CodeGenListInterface.class diagapplet/CodeGen/CodeGenCheckBoxInterface.class diagapplet/CodeGen/CodeGenTextFieldInterface.class diagapplet/CodeGen/CodeGenCommon.class ;  ${JAR} -cmf0 CodeGenJarInfo.txt CodeGen.jar rcs diagapplet/utils diagapplet/CodeGen; )





