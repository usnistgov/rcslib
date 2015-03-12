
# Set the main RCS Library Directory
ifndef RCSLIB_DIR
RCSLIB_DIR=../..
endif

ifdef PLAT
PLAT_INC_FLAG=-I$(RCSLIB_DIR)/plat/$(PLAT)/include
PLAT_LIB_FLAG=-L$(RCSLIB_DIR)/plat/$(PLAT)/lib
endif

ifeq ($(MSYSTEM),MINGW32)
EXTRA_LIBS:=$(EXTRA_LIBS) -lws2_32
endif

CPPFLAGS:=$(CPPFLAGS) -I $(RCSLIB_DIR)/include -I /usr/local/include  $(PLAT_INC_FLAG)  -I ../../src -I ../../src/os_intf  -I ../../src/posemath -I ../../src/cms -I ../../src/node -I ../../src/print -I ../../src/stg_clss 


LIBS:=$(LIBS) -L$(RCSLIB_DIR)/lib -L/usr/local/lib -L$(RCSLIB_DIR)/.libs -L$(RCSLIB_DIR) $(PLAT_LIB_FLAG) -lrcs $(EXTRA_LIBS)

ifndef JAVA
JAVA=java
endif

ifndef JAVAC
JAVAC=javac
endif

ifndef CODEGEN_CMD
CODEGEN_CMD=$(JAVA) -classpath $(RCSLIB_DIR)/bin/CodeGenCmdLine.jar:$(RCSLIB_DIR)/plat/java/lib/CodeGenCmdLine.jar diagapplet.CodeGen.CodeGenCmdLine  generate_for_all_langs=true
endif

ifndef NMLCFG
NMLCFG=env "PATH=$(RCSLIB_DIR)/bin:$(RCSLIB_DIR):$(PATH)" nmlcfg$(EXE_EXT)
endif

ifndef CXX
CXX=g++
endif

CPPFLAGS:= $(CPPFLAGS) -I $(RCSLIB_DIR)/include -I $(RCSLIB_DIR)/plat/$(PLAT)/include

ifndef HOSTNAME
HOSTNAME:=$(shell hostname)
endif

LIBS:= $(LIBS) -L$(RCSLIB_DIR)/lib -L$(RCSLIB_DIR)/plat/$(PLAT)/lib -L$(RCSLIB_DIR) -lrcs

CXXFLAGS:= $(CXXFLAGS) -g

all: nml_ex1_write nml_ex1_svr nml_ex1_read nml_ex1_j_write.class nml_ex1_j_read.class ex1.nml

ex1.nml  : ex1.cfg
	$(NMLCFG) -D HOSTNAME=$(HOSTNAME) $^ -o $@

nml_ex1_n.cc EXAMPLE_MSG.java nml_ex1_MsgDict.java : nml_ex1.hh
	$(CODEGEN_CMD) $^

nml_ex1_write: nml_ex1_n.o nml_ex1_write.o
	$(CXX) $(CXXFLAGS) $^ $(LIBS) -o $@

nml_ex1_read: nml_ex1_n.o nml_ex1_read.o
	$(CXX) $(CXXFLAGS) $^ $(LIBS) -o $@

nml_ex1_svr: nml_ex1_n.o nml_ex1_svr.o
	$(CXX) $(CXXFLAGS) $^ $(LIBS) -o $@

nml_ex1_j_write.class nml_ex1_j_read.class EXAMPLE_MSG.class nml_ex1_MsgDict.class : nml_ex1_j_write.java EXAMPLE_MSG.java nml_ex1_MsgDict.java nml_ex1_j_read.java
	$(JAVAC) -classpath .:$(RCSLIB_DIR)/plat/java/lib/rcs.jar $^


.PHONY: all

clean: 
	-\rm -f nml_ex1_n.o nml_ex1_write.o nml_ex1_read.o nml_ex1_svr.o nml_ex1_j_write.class EXAMPLE_MSG.class nml_ex1_MsgDict.class nml_ex1_j_write.class  EXAMPLE_MSG.java nml_ex1_MsgDict.java nml_ex1_n.cc nml_ex1_write nml_ex1_read nml_ex1_svr nml_ex1_n_codegen_protos.hh nml_ex1_c_n.c nml_ex1_c_n.h nml_ex1_n_ada.adb nml_ex1_n_ada.ads   nml_ex1_j_read.class ex1.nml nml_w_diag_example.tar.gz



.PHONY: clean

nml_w_diag_example.tar.gz: clean
	tar -czf $@ `find . -maxdepth 1 -type f -not -name \*.tar\*`



