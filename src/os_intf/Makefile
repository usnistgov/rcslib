ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES

ifndef NO_MULTIPLAT_BUILD_WARN
$(warning "Using the multiplatbuild.sh script that uses autoconf generated configure script. To use the old static RCS Library Makefiles either set PLAT=<yourplat> or USE_OLD_RCSLIB_MAKEFILES=1.")
endif

all:
	../../etc/multiplatbuild.sh

config:
	../../etc/multiplatbuild.sh config $(CONFIG_ARGS)

clean distclean check distcheck dist install:
	../../etc/multiplatbuild.sh $@

.PHONY: all config clean distclean check distcheck install


else

# Makefile for the Operating System interface  modules of the
# RCS Library

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif


# LIBRCS_OBJS Makefile definitions
LIBRCS_OBJS =

include Makefile.lib

HEADERS=$(OS_INTF_HEADERS)

ifneq ($(findstring vxworks, $(PLAT)), )

SRCS =	_sem.c \
	sem.cc \
	_shm.c \
	shm.cc \
	_table.c \
	table.cc \
	_timer.c \
	timer.cc \
	gtimer.cc \
	inetnull.cc \
	inetfile.cc \
	inettest.c \
	inifile.cc \
	_inifile.c \
	dbg_mem.cc \
	fileops.c

OBJS := $(LIBRCS_OBJS)

else

SRCS =	_sem.c \
	sem.cc \
	_shm.c \
	shm.cc \
	_table.c \
	table.cc \
	_timer.c \
	timer.cc \
	inetfile.cc \
	inettest.c \
	inetnull.cc \
	inifile.cc \
	_inifile.c \
	unix_sem.c \
	dbg_mem.cc \
	fileops.c

OBJS := $(LIBRCS_OBJS)

endif


LIBS =

#Application Makefile for inclusion
include ../Makefile.rcs

#
# Explicit targets for the BINS and their rules go here
#



# Targets and rules for libraries

# Targets for specific binaries

ifneq ($(findstring vxworks, $(PLAT)), )

timetest: $(DEVP_LIB_DIR)/timetest.o

else

timetest: $(DEVP_BIN_DIR)/timetest


$(DEVP_BIN_DIR)/timetest: 	\
	$(DEVP_LIB_DIR)/timetest.o \
	$(DEVP_LIB_DIR)/librcsd.a
	$(CPLUSPLUS) $^ $(CFLAGS) $(CPLUSPLUSFLAGS) $(CPLUSPLUSLINK) -o $@

endif

.PHONY: timetest

ifneq ($(findstring vxworks, $(PLAT)), )

inettest: $(DEVP_LIB_DIR)/inettest.o

else
$(DEVP_BIN_DIR)/inettest:  \
	$(DEVP_LIB_DIR)/inettest.o \
	$(DEVP_LIB_DIR)/inetfile.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) -lwww  $(CFLAGS) $(CPLUSPLUSFLAGS)  $(CPLUSPLUSLINK) -o $@

inettest: $(DEVP_BIN_DIR)/inettest

endif

$(DEVP_BIN_DIR)/tesleep: \
	$(DEVP_LIB_DIR)/_timer.o \
	$(DEVP_LIB_DIR)/rcs_prnt.o \
	$(DEVP_LIB_DIR)/linklist.o \
	$(DEVP_LIB_DIR)/tesleep.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^  $(CFLAGS) $(CPLUSPLUSFLAGS)  $(CPLUSPLUSLINK) -o $@


tesleep: $(DEVP_BIN_DIR)/tesleep

.PHONY: timertest inettest tesleep

$(DEVP_BIN_DIR)/inifile_test: \
	$(DEVP_LIB_DIR)/inifile_test.o \
	$(DEVP_LIB_DIR)/librcsd.a
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^  $(CFLAGS) $(CPLUSPLUSFLAGS)  $(CPLUSPLUSLINK) -o $@

inifile_test: $(DEVP_BIN_DIR)/inifile_test

.PHONY: inifile_test

$(DEVP_LIB_DIR)/_sem.o : _sem.c unix_sem.c vxworks_sem.c win32_sem.c

clean_objs:
	rm $(DEVP_OBJS);

.PHONY: clean_objs

endif
# ifndef USE_OLD_RCSLIB_MAKEFILES
