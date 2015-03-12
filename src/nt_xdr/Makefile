ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES

$(warning "Using the multiplatbuild.sh script that uses autoconf generated configure script. To use the old static RCS Library Makefiles either set PLAT=<yourplat> or USE_OLD_RCSLIB_MAKEFILES=1.")

all:
	../../etc/multiplatbuild.sh

config:
	../../etc/multiplatbuild.sh config $(CONFIG_ARGS)

clean distclean check distcheck install:
	../../etc/multiplatbuild.sh $@

.PHONY: all config clean distclean check distcheck install


else


# Makefile for the Operating System interface  modules of the
# RCS Library


ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif

# LIBRCS_OBJS Makefile definitions
ifeq ($(PLAT),mingw32)
include Makefile.lib
else
LIBRCS_NT_XDR_OBJS =
endif


# Declare which files are to be released
HEADERS = xdr.h \
	xdrtypes.h


SRCS =  xdr_floa.c \
	xdr_rec.c \
	xdr_stdi.c \
	xdr.c \
	xdr_arra.c \
	xdr_mem.c \
	xdr_refe.c

OBJS := $(LIBRCS_NT_XDR_OBJS)


LIBS =

#Application Makefile for inclusion
include ../Makefile.rcs

endif
# ifndef USE_OLD_RCSLIB_MAKEFILES
