ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES

all:
	../../etc/multiplatbuild.sh $(MPB_ARGS)

config:
	../../etc/multiplatbuild.sh config $(CONFIG_ARGS)

clean distclean check distcheck dist install:
	../../etc/multiplatbuild.sh $@

.PHONY: all config clean distclean check distcheck install


else

# Makefile for the CMS (Communication Management System) modules of the
# RCS Library

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif

ifndef PLAT

include ../Makefile.rcs

else

ifeq (rtlinux_09J, $(findstring rtlinux_09J, $(PLAT)))

make_dirs clean headers depend all install sources:
	-@echo "nothing to be done for" $@ "for this platform," $(PLAT)

.PHONY: make_dirs clean headers depend all install sources
else

# Uncomment the next line and re-compile to do perforance profiling.
#LOCAL_CFLAGS = -pg

ifdef DEBUG_RCS_MAKE
$(warning LOCAL_CFLAGS=$(LOCAL_CFLAGS) )
endif

# LIBRCS_OBJS Makefile definitions
LIBRCS_OBJS =
include Makefile.lib

ifdef DEBUG_RCS_MAKE
$(warning LOCAL_CFLAGS=$(LOCAL_CFLAGS) )
endif

# Declare which files are to be released
ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))
BUILD_REALTIMEONLY=1
endif

ifeq (rtai,$(PLAT))
BUILD_REALTIMEONLY=1
endif

ifdef BUILD_REALTIMEONLY

HEADERS = rtlnml.h

SRCS = rtlnml.c

OBJS = $(CMS_OBJS)

else

HEADERS = $(CMS_HEADERS)

VXWORKS_SRCS = \
	bbdmem.cc \
	bbd.c \
	bus_lock.c \
	dma.c

ifdef HAVE_RTL
LINUX_SRCS = \
	rtlmem.cc \
	rtlnml.c
endif

ifdef HAVE_RTAI
LINUX_SRCS = \
	rtlmem.cc \
	rtlnml.c
endif



COMMON_SRCS =	\
	cms.cc \
	cms_datetime.cc \
	cmsdiag.cc \
	nmldiag.cc \
	cms_in.cc \
	cms_up.cc \
	cms_xup.cc \
	cms_pup.cc \
	cms_xml_up.cc \
	cms_cfg.cc \
	autokey.c \
	nml.cc \
	nmlmsg.cc \
	globmem.cc \
	spinlock.c \
	memsem.cc \
	shmem.cc \
	cms_srv.cc \
	nml_srv.cc \
	phantom.cc \
	nmltest.cc \
	perfsvr.cc \
	nmlperf.cc \
	perftype.cc \
	_physmem.c \
	physmem.cc \
	tcp_srv.cc \
	tcpmem.cc \
	stcpmem.cc \
	nmlclean.cc \
	tcp_opts.cc \
	udp_opts.cc \
	udp_srv.cc \
	udpmem.cc \
	sendmsgt.c \
	recvmsgt.c \
	recvn.c \
	sendn.c \
	sokintrf.c \
	locmem.cc \
	filemem.cc \
	cms_dup.cc \
	sendline.c \
	recvline.c \
	stcpsvr.cc \
	stcpopts.cc \
	tcpproxy.cc \
	cms_pm.cc \
	crypt2.cc \
	nmlpwd.cc  \
	cmssvrp.cc \
	nmlqr.cc \
	nmlcfg.cc \
	nmlcms_c.cc \
	posemath_c_n.c \
	nmlcfgsvr.cc \
	nmlcfgsvr_clntcalls.cc

# Rashmi - I added the nmlcfgsvr lines above


ifeq ($(findstring linux, $(PLAT)),linux)
SRCS = $(COMMON_SRCS) $(LINUX_SRCS)
else

ifeq ($(findstring vxworks, $(PLAT)),vxworks)
SRCS = $(COMMON_SRCS) $(VXWORKS_SRCS)
else
SRCS = $(COMMON_SRCS)
endif
endif


OBJS := $(CMS_OBJS)

endif

LIBS =

ifneq ($(findstring vxworks, $(PLAT)), )

BINS =

else

BINS =

endif

ifdef CMS_CFG_USE_RTLMEM
LOCAL_CFLAGS += -DCMS_CFG_USE_RTLMEM
endif


#Application Makefile for inclusion
include ../Makefile.rcs

ifdef DEBUG_RCS_MAKE
$(warning DEVP_HEADERS=$(DEVP_HEADERS) )
$(warning DEVP_OBJS=$(DEVP_OBJS) )
$(warning LOCAL_CFLAGS=$(LOCAL_CFLAGS) )
endif


ifeq (rtlinux,$(findstring rtlinux,$(PLAT)))

LOCAL_CFLAGS := $(LOCAL_CFLAGS) -finline-functions -Winline -fno-implement-inlines

endif


memsemtest: $(DEVP_BIN_DIR)/memsemtest

 $(DEVP_BIN_DIR)/memsemtest: $(DEVP_LIB_DIR)/memsemtest.o \
	$(DEVP_LIB_DIR)/memsem.o \
	$(DEVP_LIB_DIR)/_timer.o
	$(CPLUSPLUS) $^ $(CFLAGS) $(LOCAL_CFLAGS) $(CPLUSPLUSFLAGS) -o $@

ifneq (vxworks, $(findstring  vxworks,$(PLAT)))
ifneq (rtlinux, $(findstring rtlinux, $(PLAT)))

install: $(RELEASE_BIN_DIR)/nmltest$(EXE_EXT) \
	 $(RELEASE_BIN_DIR)/nmlperf$(EXE_EXT) \
	$(RELEASE_BIN_DIR)/nmlcfg$(EXE_EXT) \
	$(RELEASE_BIN_DIR)/nmlclean$(EXE_EXT) \
	$(RELEASE_BIN_DIR)/perfsvr$(EXE_EXT)

clean:
	\rm -f $(DEVP_BIN_DIR)/nmltest$(EXE_EXT)
	\rm -f $(DEVP_BIN_DIR)/nmlperf$(EXE_EXT)
	\rm -f $(DEVP_BIN_DIR)/nmlclean$(EXE_EXT)
	\rm -f $(DEVP_BIN_DIR)/perfsvr$(EXE_EXT)
	\rm -f $(DEVP_BIN_DIR)/tcpproxy$(EXE_EXT)

endif
endif

#
# Explicit targets for the BINS and their rules go here
#

ifeq (,$(findstring vxworks, $(PLAT)))

nmltest.tc: $(DEVP_BIN_DIR)/nmltest.tc$(EXE_EXT)

nmlclean.tc: $(DEVP_BIN_DIR)/nmlclean.tc$(EXE_EXT)


nmlperf.tc: $(DEVP_BIN_DIR)/nmlperf.tc$(EXE_EXT)

nmlwatch.tc: $(DEVP_BIN_DIR)/nmlwatch.tc$(EXE_EXT)

perfsvr.tc: $(DEVP_BIN_DIR)/perfsvr.tc$(EXE_EXT)

nmltest: $(DEVP_BIN_DIR)/nmltest$(EXE_EXT)

nmlclean: $(DEVP_BIN_DIR)/nmlclean$(EXE_EXT)

nmlperf: $(DEVP_BIN_DIR)/nmlperf$(EXE_EXT)

perfsvr: $(DEVP_BIN_DIR)/perfsvr$(EXE_EXT)

tcpproxy: $(DEVP_BIN_DIR)/tcpproxy$(EXE_EXT)

nmlpwd: $(DEVP_BIN_DIR)/nmlpwd$(EXE_EXT)

$(DEVP_BIN_DIR)/nmltest$(EXE_EXT): \
	$(DEVP_LIB_DIR)/nmltest.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@

$(DEVP_BIN_DIR)/nmlclean$(EXE_EXT): \
	$(DEVP_LIB_DIR)/nmlclean.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@


$(DEVP_BIN_DIR)/tcpproxy$(EXE_EXT): \
	$(DEVP_LIB_DIR)/tcpproxy.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@


$(DEVP_BIN_DIR)/nmlperf$(EXE_EXT): \
	$(DEVP_LIB_DIR)/nmlperf.o \
	$(DEVP_LIB_DIR)/perftype.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@


$(DEVP_BIN_DIR)/perfsvr$(EXE_EXT): \
	$(DEVP_LIB_DIR)/perfsvr.o \
	$(DEVP_LIB_DIR)/perftype.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@

$(DEVP_BIN_DIR)/nmlpwd$(EXE_EXT): \
	$(DEVP_LIB_DIR)/nmlpwd.o \
	$(DEVP_LIB_DIR)/crypt2.o
	$(COMPILER_SETUP); \
	$(CPLUSPLUS)  $^ -L$(DEVP_LIB_DIR) $(RCS_LINK_FLAG)  $(CLINK) $(CPLUSPLUSLINK) -o $@

else

nmltest: $(DEVP_LIB_DIR)/nmltest.o

nmlclean: $(DEVP_LIB_DIR)/nmlclean.o

tcpproxy: $(DEVP_LIB_DIR)/tcpproxy.o

nmlperf: $(DEVP_LIB_DIR)/nmlperf.o $(DEVP_LIB_DIR)/perftype.o

perfsvr: $(DEVP_LIB_DIR)/perfsvr.o $(DEVP_LIB_DIR)/perftype.o

nmlpwd: $(DEVP_LIB_DIR)/nmlpwd.o

endif

# Targets and rules for libraries

# Targets for specific binaries

#Phony Targets

$(DEVP_BIN_DIR)/nmlcfg$(EXE_EXT): \
	$(DEVP_LIB_DIR)/nmlcfg.o \
	$(DEVP_LIB_DIR)/$(RCSLIB_LIBRARY_NAME)
	$(COMPILER_SETUP); \
	$(CPLUSPLUS) $(LOCAL_CFLAGS) $^ $(CFLAGS) $(CPLUSPLUSFLAGS) $(CLINK) $(CPLUSPLUSLINK) $(SEARCH_PATH_FLAG) $(RCS_LIB_SEARCH_PATH) -o $@

nmlcfg: $(DEVP_BIN_DIR)/nmlcfg$(EXE_EXT)

.PHONY: nmlcfg

ifdef EXE_EXT
nmlcfg$(EXE_EXT): $(DEVP_BIN_DIR)/nmlcfg$(EXE_EXT)

.PHONY: nmlcfg$(EXE_EXT)
endif

TTYTEST_EXTRA_LIBS=

ifdef ENABLE_READLINE
TTYTEST_EXTRA_LIBS += -lreadline
endif


$(DEVP_BIN_DIR)/ttytest$(EXE_EXT): \
	$(DEVP_LIB_DIR)/ttytest.o \
	$(DEVP_LIB_DIR)/_timer.o \
	$(DEVP_LIB_DIR)/rcs_prnt.o \
	$(DEVP_LIB_DIR)/linklist.o \
	$(DEVP_LIB_DIR)/ttyintf.o
	$(COMPILER_SETUP); \
	$(CXX) $(LOCAL_CFLAGS) $^ $(CFLAGS) $(TTYTEST_EXTRA_LIBS) -o $@

ttytest: $(DEVP_BIN_DIR)/ttytest$(EXE_EXT)

.PHONY: ttytest

ifdef EXE_EXT
ttytest$(EXE_EXT): $(DEVP_BIN_DIR)/ttytest$(EXE_EXT)

.PHONY: ttytest$(EXE_EXT)
endif



clean_objs:
	rm $(DEVP_OBJS);

.PHONY: clean_objs

endif
# rtlinux_09J


$(DEVP_LIB_DIR)/rtlnml.o: rtlnml.c
	$(CC) rtlnml.c -c  $(CFLAGS) -O2 -fno-implement-inlines -finline-functions -Winline -o $@


$(DEVP_BIN_DIR)/xsd2nmlh$(EXE_EXT): cms_xml_up.cc ../stg_clss/linklist.cc  ../print/rcs_prnt.cc  ../os_intf/_timer.c 
	-if test -f $@ ; then \rm $@ >/dev/null 2>/dev/null ; fi             
	$(CPLUSPLUS) -g -O0 -DXML_SCHEMA_TO_HEADER  -I. -I.. -I../stg_clss -I../print -I../os_intf  $(CFLAGS) $(CPLUSPLUSFLAGS) $^ -o $@

xsd2nmlh: $(DEVP_BIN_DIR)/xsd2nmlh$(EXE_EXT)

ifdef EXE_EXT

xsd2nmlh$(EXE_EXT): $(DEVP_BIN_DIR)/xsd2nmlh$(EXE_EXT)

.PHONY: xsd2nmlh$(EXE_EXT)

endif

.DELETE_ON_ERROR: $(DEVP_BIN_DIR)/xsd2nmlh
.PHONY: xsd2nml

endif
# ifdef PLAT

.PHONY: nmltest nmlcfg nmlperf perfsvr nmlclean tcpproxy nmlpwd

$(DEVP_BIN_DIR)/nmlsizes: nmlsizes.cc
	$(CPLUSPLUS) -g  -I. -I.. -I../stg_clss -I../print -I../os_intf -I../posemath -I../node  $(CFLAGS) $(CPLUSPLUSFLAGS) $^ -o $@

nmlsizes: $(DEVP_BIN_DIR)/nmlsizes
	-rm ./nmlsizes
	-ln -s $(DEVP_BIN_DIR)/nmlsizes nmlsizes

.PHONY: nmlsizes


endif
# ifndef USE_OLD_RCSLIB_MAKEFILES


