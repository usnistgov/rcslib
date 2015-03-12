# Makefile for the missing_ce directory or the
# RCS Library
# The library contains implementations of ANSI C function clones that are
# appear to be missing from Winows CE

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=../..
endif


# LIBRCS_OBJS Makefile definitions
LIBRCS_OBJS =

include Makefile.lib

# Declare which files are to be released
HEADERS = rcs_ce.h

SRCS =	rcs_ce.cc \
	cetest.cc

OBJS = rcs_ce.o

BINS = cetest

#Application Makefile for inclusion
include ../Makefile.rcs

#
# Explicit targets for the BINS and their rules go here
#



$(DEVP_BIN_DIR)/cetest: \
	$(DEVP_LIB_DIR)/cetest.o \
	$(DEVP_LIB_DIR)/rcs_ce.o
	$(CPLUSPLUS)  $^  $(CFLAGS) $(CPLUSPLUSFLAGS)  $(CPLUSPLUSLINK) -o $@

cetest: $(DEVP_BIN_DIR)/cetest

.PHONY : cetest
