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
HEADERS= $(PRINT_HEADERS)

SRCS =	rcs_prnt.cc sepwords.cc

OBJS = $(PRINT_OBJS)

LIBS =

#Application Makefile for inclusion
include ../Makefile.rcs

# Targets and rules for libraries

# Targets for specific binaries
printest: $(DEVP_BIN_DIR)/printest
printest.tc: $(DEVP_BIN_DIR)/printest.tc

$(DEVP_BIN_DIR)/printest: \
	$(DEVP_LIB_DIR)/printest.o \
	$(DEVP_LIB_DIR)/librcs.a
	$(CPLUSPLUS) $^ $(CFLAGS) $(CPLUSPLUSFLAGS) $(CPLUSPLUSLINK) -o $@

$(DEVP_BIN_DIR)/printest.tc: \
	$(DEVP_LIB_DIR)/printest.o \
	$(DEVP_LIB_DIR)/librcs.a
	proof $(CPLUSPLUS) $^ $(CFLAGS) $(CPLUSPLUSFLAGS) $(CPLUSPLUSLINK) -o $@
.PHONY: printest printest.tc

endif
# ifndef USE_OLD_RCSLIB_MAKEFILES
