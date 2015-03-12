#! /bin/sh

set -x

if test "x${GCJ}" = "x" ; then
    GCJ=gcj
fi

${GCJ} --version || exit $?

if test "x${GCJ_FLAGS}" = "x" ; then 
    GCJ_FLAGS="-O3 -fno-store-check -fno-bounds-check";
fi

if test "x${OBJ_EXT}" = "x" ; then
    OBJ_EXT=.o
fi

if test "x${EXE_DIR}" = "x" ; then
    EXE_DIR=.
fi

if test "x${OBJ_DIR}" = "x" ; then
    OBJ_DIR=.
fi


JFILES="../../rcs/utils/URL_and_FileLoader.java ../../rcs/utils/StrToInt.java ../../rcs/utils/StrToLong.java ../../rcs/nml/NMLConnectionInterface.java ../../rcs/nml/NMLConnectionCreatorInterface.java ../../rcs/nml/NMLFormatConvertErrCallbackInterface.java  ../../rcs/nml/NMLMsgDict.java ../../rcs/nml/NMLmsg.java ../../rcs/nml/NML_ERROR.java ../../rcs/nml/NML_TEXT.java ../../rcs/nml/NML_DISPLAY.java  ../../rcs/nml/errlogMsgDict.java  ../utils/FakeFastListPanel.java ../utils/FastListPanelItem.java ../utils/FastListPanelInterface.java ../utils/CountButtonInterface.java ../utils/CountListInterface.java ../utils/URLLoadInfoPanelInterface.java ModuleInfoInterface.java info_array_elem_info.java StructTypeInfoTokenizer.java StructureTypeInfo.java EnumTypeInfo.java DiagNMLMsgDictInterface.java DiagNMLMsgDictCreatorInterface.java DiagNMLFormatConvertErrCallback.java StructTypeInfoTokenizerInterface.java ModuleInfo.java CodeGenCmdLine.java DefinedValue.java BufferInfo.java ChannelInfo.java ServerInfo.java CodeGenCommonInterface.java CodeGenBellRingerInterface.java CodeGenTextAreaInterface.java CodeGenListInterface.java CodeGenCheckBoxInterface.java CodeGenTextFieldInterface.java SplitInfoToken.java StringFuncs.java C_Generator.java Ada_Generator.java CodeGenCommon.java"

OFILES=

for jfile in ${JFILES} ; do
    shortjfile=${jfile##*/}
    ofile=${shortjfile/.java/${OBJ_EXT}}
    longofile=${OBJ_DIR}/${ofile}
    ls -l ${longofile}
    ls -l ${jfile}
    test '!' -f ${longofile}
    sts=$?
    test ${jfile} -nt ${longofile};
    sts=$?
    if test '!' -f ${longofile} -o ${jfile} -nt ${longofile} ; then
	${GCJ} ${GCJ_FLAGS} -I../.. -c -o ${longofile} ${jfile} || exit $?;
    fi
    OFILES="${OFILES} ${longofile}";
done

test -f "${EXE_DIR}/nml_codegen${EXE_EXT}";
sts=$?;

if test -f "${EXE_DIR}/nml_codegen${EXE_EXT}" ; then
    ls -l "${EXE_DIR}/nml_codegen${EXE_EXT}";
    rm -f "${EXE_DIR}/nml_codegen${EXE_EXT}";
fi

${GCJ} --main=diagapplet.CodeGen.CodeGenCmdLine -o ${EXE_DIR}/nml_codegen${EXE_EXT} ${OFILES}
