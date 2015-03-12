ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES

$(warning "Using the multiplatbuild.sh script that uses autoconf generated configure script. To use the old static RCS Library Makefiles either set PLAT=<yourplat> or USE_OLD_RCSLIB_MAKEFILES=1.")

all:
	../../etc/multiplatbuild.sh

config:
	../../etc/multiplatbuild.sh config $(CONFIG_ARGS)

clean distclean check distcheck dist install:
	../../etc/multiplatbuild.sh $@

.PHONY: all config clean distclean check distcheck install


else

#
# Makefile for the RCS C++ Storage Class Utilities
#

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif

# LIBRCS_OBJS Makefile
LIBRCS_OBJS =
include Makefile.lib

# Declare which files are to be released
HEADERS = $(STG_CLSS_HEADERS) 

SRCS =	linklist.cc

OBJS = $(LIBRCS_OBJS)

LIBS =

BINS= 


#Application Makefile for inclusion
include ../Makefile.rcs

# Targets and rules for libraries

# Targets for specific binaries

$(DEVP_BIN_DIR)/lltest: $(DEVP_LIB_DIR)/lltest.o \
	$(DEVP_LIB_DIR)/linklist.o
	$(CPLUSPLUS) $^ $(CFLAGS) $(LOCAL_CFLAGS) $(CPLUSPLUSFLAGS) -o $@

.PHONY: lltest

lltest: $(DEVP_BIN_DIR)/lltest


endif
# ifndef USE_OLD_RCSLIB_MAKEFILES

