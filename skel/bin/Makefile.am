INCLUDES = -I$(RCSLIB_DIR)/include

EXTRA_DIST = run

bin_PROGRAMS = skelNmlSvr subMain subShell supMain supShell skelLog

skelNmlSvr_SOURCES = ../src/skelNmlSvr.cc ../src/skel.hh ../src/skelIntf.hh ../src/supIntf.hh ../src/subIntf.hh
skelNmlSvr_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm

subMain_SOURCES = ../src/subMain.cc
subMain_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm

subShell_SOURCES = ../src/subShell.cc ../src/getopt.h ../src/getopt.c
subShell_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm

supMain_SOURCES = ../src/supMain.cc
supMain_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm

supShell_SOURCES = ../src/supShell.cc ../src/getopt.h ../src/getopt.c
supShell_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm

skelLog_SOURCES = ../src/skelLog.cc
skelLog_LDADD = -L../lib -lskel -L$(RCSLIB_DIR)/lib -lrcs -lposemath -lm
