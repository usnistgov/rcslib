
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

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif

LIBRCS_OBJS=
include Makefile.lib


# Makefile for compiling pose math library

# Note: posemath.cc and _posemath.c implement the C++ and C versions of
# the posemath.h definitions, respectively.
# sincos.h and sincos.c are C only.
# mathprnt.cc and _mathprnt.c implement the C++ and C versions of the
# mathprnt.h definitions, respectively.
#
# Debug can be compiled in to the posemath.cc and _posemath.c files.
#
# The resulting .o files are linked into libpm.a (no debug), or libpmdb.a
# (with debug).

MODULE = posemath

ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))
BUILD_REALTIMEONLY=1
endif

ifeq (rtai,$(PLAT))
BUILD_REALTIMEONLY=1
endif

HEADERS = $(POSEMATH_HEADERS)

ifdef BUILD_REALTIMEONLY


CSRCS = \
	_posemath.c \
	sincos.c

COBJS = \
	_posemath.o \
	sincos.o

else


CSRCS = \
	_mathprnt.c \
	_posemath.c \
	sincos.c \
	testpmc.c \
	testcirc.c


COBJS = _posemath.o \
	_posemathdb.o\
	sincos.o \
	_mathprnt.o

endif

CPPSRCS = mathprnt.cc \
	posemath.cc

CPPOBJS = posemath.o \
	posemathdb.o \
	mathprnt.o


ifeq (rtlinux,$(findstring rtlinux,$(PLAT)))


SRCS = $(CSRCS)

OBJS = $(COBJS)


else

ifeq (rtai,$(PLAT))

SRCS = $(CSRCS)

OBJS = $(COBJS)

else


OBJS = $(COBJS) $(CPPOBJS)

SRCS = $(CSRCS) $(CPPSRCS)

endif

endif


ifneq (vxworks, $(findstring vxworks, $(PLAT)))


ifeq (rtlinux,$(findstring rtlinux,$(PLAT)))

LIBS = libpm.a

else
# rtlinux 

ifeq (rtai,$(PLAT))

LIBS = libpm.a

else
#rtai 

LIBS=	libpm.a	libpmdb.a

BINS = 

# testpmc testpmcpp

endif
# ! rtlinux

endif
# ! rtai

else
# vxworks

LIBS =


endif
# vxworks


# get all the platform and application definitions
include ../Makefile.rcs

ifeq (linux,$(findstring linux,$(PLAT)))
LOCAL_CFLAGS += -ffast-math
endif

# debug overrides

ifndef BUILD_REALTIMEONLY

$(DEVP_LIB_DIR)/_posemathdb.o:	_posemath.c
	-( $(COMPILER_SETUP); \
	$(CC) $< -c -w $(LOCAL_CFLAGS) \
	-DPM_PRINT_ERROR \
	 -I$(DEVP_INCLUDE_DIR) -I$(RELEASE_INCLUDE_DIR) -I$(RCS_INCLUDE) \
	$(CFLAGS) $(LOCAL_CFLAGS) -o $@ )

$(DEVP_LIB_DIR)/posemathdb.o: posemath.cc
	-( $(COMPILER_SETUP); \
	$(CPLUSPLUS) $< -c -w $(LOCAL_CFLAGS) $(LOCAL_CPLUSPLUSFLAGS) \
	-DPM_PRINT_ERROR \
	-I$(DEVP_INCLUDE_DIR) -I$(RELEASE_INCLUDE_DIR) -I$(RCS_INCLUDE) \
	 $(CFLAGS) $(LOCAL_CFLAGS) $(CPLUSPLUSFLAGS) -o $@ )

endif

# Libraries

ifeq (rtlinux,$(findstring rtlinux,$(PLAT)))

$(DEVP_LIB_DIR)/libpm.a: \
	$(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(AR) cr $@ $(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(RANLIB) $@

else

ifeq (rtai,$(PLAT))

$(DEVP_LIB_DIR)/libpm.a: \
	$(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(AR) cr $@ $(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(RANLIB) $@

else

$(DEVP_LIB_DIR)/libpm.a: \
	$(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/posemath.o \
	$(DEVP_LIB_DIR)/_mathprnt.o \
	$(DEVP_LIB_DIR)/mathprnt.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(AR) cr $@ $(DEVP_LIB_DIR)/_posemath.o \
	$(DEVP_LIB_DIR)/posemath.o \
	$(DEVP_LIB_DIR)/_mathprnt.o \
	$(DEVP_LIB_DIR)/mathprnt.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(RANLIB) $@

endif
endif

ifndef BUILD_REALTIMEONLY

$(DEVP_LIB_DIR)/libpmdb.a: \
	$(DEVP_LIB_DIR)/_posemathdb.o \
	$(DEVP_LIB_DIR)/posemathdb.o \
	$(DEVP_LIB_DIR)/_mathprnt.o \
	$(DEVP_LIB_DIR)/mathprnt.o \
	$(DEVP_LIB_DIR)/sincos.o
	-$(AR) cr $@ $^
	-$(RANLIB) $@

endif

# C test code

ifndef BUILD_REALTIMEONLY

$(DEVP_LIB_DIR)/testpmc.o:	testpmc.c
	-( $(COMPILER_SETUP); \
	$(CC) $< -c $(LOCAL_CFLAGS) -DPM_LOOSE_NAMESPACE \
	 -I$(DEVP_INCLUDE_DIR) -I$(RELEASE_INCLUDE_DIR) -I$(RCS_INCLUDE) \
	$(CFLAGS) $(LOCAL_CFLAGS) -o $@ )

$(DEVP_BIN_DIR)/testpmc: \
	$(DEVP_LIB_DIR)/testpmc.o \
	$(DEVP_LIB_DIR)/libpmdb.a
	-( $(COMPILER_SETUP); \
	$(CC) $^ $(CLINK) -lm -o $@ )

testpmc: $(DEVP_BIN_DIR)/testpmc

.PHONY: testpmc

$(DEVP_BIN_DIR)/testcirc: \
	$(DEVP_LIB_DIR)/testcirc.o \
	$(DEVP_LIB_DIR)/libpm.a
	-( $(COMPILER_SETUP); \
	$(CC) $(CFLAGS) $(LOCAL_CFLAGS) $^ \
	$(CLINK) -lm \
	-o $@ )

# C++ test code


$(DEVP_BIN_DIR)/testpmcpp: \
	$(DEVP_LIB_DIR)/testpmcpp.o \
	$(DEVP_LIB_DIR)/libpmdb.a
	-( $(COMPILER_SETUP); \
	$(CPLUSPLUS)   $(CPLUSPLUSFLAGS) $^ \
	$(CPLUSPLUSLINK) -lm  \
	-o $@ )


endif

endif
# ifndef USE_OLD_RCSLIB_MAKEFILES
