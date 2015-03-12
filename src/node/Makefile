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

# Makefile for the NODE modules of the
# RCS Library

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif

# LIBRCS_OBJS Makefile definitions
LIBRCS_OBJS =
include Makefile.lib

# Declare which files are to be released
HEADERS = $(NODE_HEADERS)


SRCS =	cmd_msg.cc \
	stat_msg.cc \
	nml_mod.cc \
	nml_oi.cc

OBJS := $(LIBRCS_OBJS)

LIBS =

LOCAL_CFLAGS =

#Application Makefile for inclusion
include ../Makefile.rcs



endif
#ifndef USE_OLD_RCSLIB_MAKEFILES
