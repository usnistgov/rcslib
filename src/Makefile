

ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES


all:
	../etc/multiplatbuild.sh

config:
	../etc/multiplatbuild.sh config $(CONFIG_ARGS)

distclean check distcheck dist install:
	../etc/multiplatbuild.sh $@

clean:
	../etc/multiplatbuild.sh $@

.PHONY: all config clean distclean check distcheck install


else


# Makefile for rcslib/src directory
# This makefile calls makes in subdirectories and includes the target info to
# create the library from the object files.

ifndef RCSLIB_MAIN_DIR
RCSLIB_MAIN_DIR=..
endif

include ../etc/determineplat.def

SRCS =  rcsvers.c

ifneq (java, $(findstring java, $(PLAT)))
ifneq (rtlinux, $(findstring rtlinux, $(PLAT)))
ifneq (rtai, $(PLAT))

HEADERS = rcs.hh \
	rcs_defs.hh \
	rcsvers.hh


ifeq (linux, $(findstring linux, $(PLAT)))
ifneq (linux_2_0, $(findstring linux_2_0, $(PLAT)))

ifndef HAVE_RTLINUX
HAVE_RTLINUX = $(strip $(shell ../etc/havertlinux.sh) )
endif

ifndef HAVE_RTAI
HAVE_RTAI = $(strip $(shell ../etc/havertai.sh) )
endif

ifneq (YES, $(HAVE_RTLINUX))
ifneq (YES, $(HAVE_RTAI))
NO_RTL=1
endif
endif

endif
endif

# Include definitions of LIBRCS_OBJS
include print/Makefile.lib
include os_intf/Makefile.lib
include cms/Makefile.lib
include node/Makefile.lib
include stg_clss/Makefile.lib
include posemath/Makefile.lib

ifeq ($(PLAT),mingw32)
include nt_xdr/Makefile.lib

all : nt_xdr_make_all
headers : nt_xdr_make_headers

.PHONY: nt_xdr_make_all

nt_xdr_make_all:
	$(MAKE) PLAT=$(PLAT) -C nt_xdr all

.PHONY: nt_xdr_make_all

nt_xdr_make_headers:
	$(MAKE) PLAT=$(PLAT) -C nt_xdr headers

endif


ifeq ($(PLAT), CenterLine)
LIBRCS_OBJS := $(LIBRCS_OBJS) CenterLine_Fix.o rcsvers.o
endif

LIBRCS_OBJS := $(LIBRCS_OBJS)

# Declare which files are to be released.


ifneq (vxworks, $(findstring vxworks, $(PLAT)))

LIBS = librcs.a librcsd.a

else
# vxworks

LIBS = librcs.o librcsd.o 
# librcsw.o

endif
# vxworks

else
# rtai 

HEADERS =  \
	rcs_defs.hh \
	rcsvers.hh

include posemath/Makefile.lib
include cms/Makefile.lib

LIBS =librcs.a librcsd.a

LIBRCS_OBJS := $(LIBRCS_OBJS) rcsvers.o


endif
# rtai

else
# rtlinux 

HEADERS =  \
	rcs_defs.hh \
	rcsvers.hh

include posemath/Makefile.lib
include cms/Makefile.lib

LIBS =librcs.a librcsd.a

LIBRCS_OBJS := $(LIBRCS_OBJS) rcsvers.o


endif
# rtlinux


endif
#java



OBJS =

BINS =

NO_AUTO_DEPENDS = 1

# Include Generic definitions
include Makefile.rcs

ifdef DEBUG_RCS_MAKE
$(warning DEVP_HEADERS=$(DEVP_HEADERS) )
$(warning DEVP_OBJS=$(DEVP_OBJS) )
endif

ifneq (vxworks, $(findstring vxworks, $(PLAT)))

ifndef DYNAMIC_LIB_EXTENSION
ifeq (darwin, $(findstring darwin, $(PLAT)))

DYNAMIC_LIB_EXTENSION=dylib

else
# darwin

DYNAMIC_LIB_EXTENSION=so

endif
# darwin

endif
# DYNAMIC_LIB_EXTENSION

ifdef DYNAMIC_LINK_FLAG

ifneq ($(RCS_VERSION),)

LIBS = librcs.a librcsd.a librcs.$(DYNAMIC_LIB_EXTENSION)

else
# RCS_VERSION

LIBS = librcs.a librcsd.a librcs.$(DYNAMIC_LIB_EXTENSION) librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)

endif
# RCS_VERSION

RELEASE_LIBS = $(LIBS:%=$(RELEASE_LIB_DIR)/%)
DEVP_LIBS = $(LIBS:%=$(DEVP_LIB_DIR)/%)

endif
# DYNAMIC_LINK_FLAG

endif
# vxworks


ifneq ($(RCS_VERSION),)

ifdef DYNAMIC_LINK_FLAG

ifndef RCSLIB_DYNAMIC_LINK_FLAG
RCSLIB_DYNAMIC_LINK_FLAG=$(DYNAMIC_LINK_FLAG)
endif

all: $(DEVP_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION) $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)

install: $(RELEASE_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION) $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)

endif

else

ifdef DYNAMIC_LINK_FLAG

ifndef RCSLIB_DYNAMIC_LINK_FLAG
RCSLIB_DYNAMIC_LINK_FLAG=$(DYNAMIC_LINK_FLAG)
endif

all: $(DEVP_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION)

install: $(RELEASE_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION)

endif

endif


ifeq (java, $(findstring java, $(PLAT)))

delete clean depend sources headers:
	(cd java; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  COMPILE_TOGETHER=1 $@;)

else
# ! java

ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))

delete clean depend sources headers release_headers:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)


else
# ! rtlinux


ifeq (rtai, $(PLAT))

delete clean depend sources headers:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)


else
# ! rtai  


ifeq (win32, $(findstring win32, $(PLAT)))

delete clean sources headers:
	(cd nt_xdr; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd node; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)

else
# ! win

delete clean:
	$(MAKE) PLAT=$(PLAT) -C stg_clss $@
	$(MAKE) PLAT=$(PLAT) -C print $@
	$(MAKE) PLAT=$(PLAT) -C posemath $@
	$(MAKE) PLAT=$(PLAT) -C os_intf $@
	$(MAKE) PLAT=$(PLAT) -C cms $@
	$(MAKE) PLAT=$(PLAT) -C node $@

# ! win
endif

# ! rtai
endif

# ! rtlinux
endif

# ! java
endif


# Everyone except the RCS administrators (Will Shackleford, and Fred Proctor)
# should probably remove last three commands. Which only send out mail
# and archive the source code and library.


clean: remove_old_rcs_libs

remove_old_rcs_libs:
	-\rm -f $(DEVP_LIB_DIR)/librcs*.$(DYNAMIC_LIB_EXTENSION)*

.PHONY: remove_old_rcs_libs




ifdef AT_NIST

ifeq (java, $(findstring java, $(PLAT)))


install:
	(cd java; $(MAKE) PLAT=$(PLAT) $(MFLAGS) COMPILE_TOGETHER=1 $@;)
	($(MAKE) PLAT=$(PLAT) $(MFLAGS) archive)



# java
else


ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))


install:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	($(MAKE) PLAT=$(PLAT) $(MFLAGS) archive)


# rtlinux
else

ifeq (rtai, $(PLAT))


install:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	($(MAKE) PLAT=$(PLAT) $(MFLAGS) archive)


# rtai
else

ifeq (lynx, $(findstring lynx, $(PLAT)))

install:
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd node; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd pcio; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd pmac; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(if [ ! -f $(RELEASE_LIB_DIR)/libwww.$(DYNAMIC_LIB_EXTENSION) ] ; then \
		cd w3c; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS); \
	fi;)

# lynx
else

ifeq (win, $(findstring win, $(PLAT)))

install:
	(cd nt_xdr; $(MAKE) PLAT=$(PLAT) $@  $(MFLAGS);)
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd node; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# win
else

install:
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) install $(MFLAGS)  ;)
	(cd print; $(MAKE) PLAT=$(PLAT) install $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) install $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) install $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) install $(MFLAGS);)
	(cd node; $(MAKE) PLAT=$(PLAT) install  $(MFLAGS);)
	(if [ ! -f $(RELEASE_LIB_DIR)/libwww.$(DYNAMIC_LIB_EXTENSION) ] ; then \
		cd w3c; $(MAKE) PLAT=$(PLAT) headers sources depend all install $(MFLAGS); \
	fi;)


# win
endif

# lynx
endif

# java
endif

# rtlinux
endif

# rtai
endif

# AT_NIST
else



ifeq (java, $(findstring java, $(PLAT)))

install:
	(cd java; $(MAKE) PLAT=$(PLAT) $(MFLAGS) COMPILE_TOGETHER=1 $@;)

# java
else


ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))

install:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# rtlinux
else

ifeq (rtai, $(PLAT))

install:
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# rtai
else

ifeq (lynx, $(findstring lynx, $(PLAT)))

install:
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd node; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd pcio; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd pmac; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# lynx
else

ifeq (win, $(findstring win, $(PLAT)))

install:
	(cd nt_xdr; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd node; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# win
else

install:
	(cd stg_clss; $(MAKE) PLAT=$(PLAT) $(MFLAGS)  $@;)
	(cd print; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd os_intf; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd posemath; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd cms; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)
	(cd node; $(MAKE) PLAT=$(PLAT) $@ $(MFLAGS);)

# win
endif

# lynx
endif

# rtlinux
endif


# rtai
endif

# java
endif


# AT_NIST
endif

post_install_only: sendmail
	($(MAKE) PLAT=$(PLAT) $(MFLAGS)  archive)

.PHONY: post_install_only



ifneq (vxworks, $(findstring vxworks, $(PLAT)))

backup_libs:
	@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y`; fi
	-@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y/%m`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y/%m`; fi
	-@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`; fi
	-cp $(RELEASE_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION) $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`

else

backup_libs:
	@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y`; fi
	-@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y/%m`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y/%m`; fi
	-@if(test ! -d $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`) ; then mkdir -p  $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`; fi
	-cp $(RELEASE_LIB_DIR)/librcs.o $(RELEASE_LIB_DIR)/`date +20%y/%m/%d`


endif


.PHONY: backup_libs

archive:
	-($(MAKE) PLAT=$(PLAT) $(MFLAGS)  archive_plat)
	-($(MAKE) PLAT=$(PLAT) $(MFLAGS)  archive_src)

ifdef PLAT
ifndef HOST_PLAT
HOST_PLAT=$(shell uname -s)
endif
endif



ifeq (Linux, $(findstring Linux, $(HOST_PLAT)))

TAR_X_ARGS = chXf
TAR_ARGS = chf
TAR_A_ARGS = rf
TAR_EXTENSION = tar
COMPRESS_COMMAND = gzip
COMPRESSED_TAR_EXTENSION  = tar.gz


else

ifeq (linux, $(findstring linux, $(HOST_PLAT)))

TAR_X_ARGS = chXf
TAR_ARGS = chf
TAR_A_ARGS = rf
TAR_EXTENSION = tar
COMPRESS_COMMAND = gzip
COMPRESSED_TAR_EXTENSION = tar.gz

else

TAR_A_ARGS = uhFf
TAR_X_ARGS = chFXf
TAR_ARGS = chFf
TAR_EXTENSION = tar
COMPRESS_COMMAND = compress
COMPRESSED_TAR_EXTENSION = tar.Z

endif
endif

archive_tools:
	-@echo Creating prog_tools.$(TAR_EXTENSION)
	cd $(RCSLIB_MAIN_DIR); tar $(TAR_ARGS) $(ARCHIVE_DIRECTORY)/prog_tools.$(TAR_EXTENSION) etc
	-@echo Compressing prog_tools.$(TAR_EXTENSION)
	-$(COMPRESS_COMMAND) -f  $(ARCHIVE_DIRECTORY)/prog_tools.$(TAR_EXTENSION)
	-@echo Make Archive date-specific  Subdirectory
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`; fi
	-@echo Copy files to date-specific Archive subdirectory $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`
	-@cp $(ARCHIVE_DIRECTORY)/prog_tools.$(COMPRESSED_TAR_EXTENSION) $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`

archive_src:
	-@echo Creating tar exclude file
	-@\rm -f $(USER_DIR)/exclude_from_tar.txt
	-@mkdir -p $(ARCHIVE_DIRECTORY)
	-@\rm -f $(ARCHIVE_DIRECTORY)/rcs_src.$(TAR_EXTENSION)
	-@\rm -f $(ARCHIVE_DIRECTORY)/rcs_src.$(COMPRESSED_TAR_EXTENSION)
	-@cd $(USER_DIR); find src -name "."\?\?\* -print >exclude_from_tar.txt 2> archive_src.err
	-@cd $(USER_DIR); find src -name "install"\?\?\* -print >> exclude_from_tar.txt 2>> archive_src.err
	-@cd $(USER_DIR); find src -name "update"\?\?\* -print >> exclude_from_tar.txt 2>> archive_src.err
	-@cd $(USER_DIR); find src -name "new_version" -print >> exclude_from_tar.txt 2>> archive_src.err
	-@cd $(USER_DIR); find src -name "setver" -print >> exclude_from_tar.txt 2>> archive_src.err
	-@cd $(USER_DIR); find src -name "RCS" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name "SCCS" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name "TC.Cache" -print >> exclude_from_tar.txt 2>> archive_src.err
	-@cd $(USER_DIR); find src -name "old"\* -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); echo src/demos >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); echo src/extras >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); echo src/dict >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); echo src/bit3 >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); echo src/java/native >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".html" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \#\*\# -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".class" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".o" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".dsp" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*"~" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".$(TAR_EXTENSION)" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*".txt" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \?\?\*"$(COMPRESSED_TAR_EXTENSION)" -print >> exclude_from_tar.txt
	-@cd $(USER_DIR); find src -name \?\?\*",v" -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name \*foo\* -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name core -print >> exclude_from_tar.txt  2>> archive_src.err
	-@cd $(USER_DIR); find src -name a.out -print >> exclude_from_tar.txt  2>> archive_src.err
	-@echo Creating Source tar file
	-cd $(USER_DIR); tar $(TAR_X_ARGS) exclude_from_tar.txt   $(ARCHIVE_DIRECTORY)/rcs_src.$(TAR_EXTENSION)   src  2>> archive_src.err
	-@echo adding $(RCSLIB_MAIN_DIR)/etc  directory
	-cd $(RCSLIB_MAIN_DIR); tar $(TAR_A_ARGS) $(ARCHIVE_DIRECTORY)/rcs_src.$(TAR_EXTENSION) etc/*.def etc/addnewline.sed etc/platname etc/README.TXT  2>> archive_src.err
	-@echo compressing Source tar file
	-$(COMPRESS_COMMAND) -f $(ARCHIVE_DIRECTORY)/rcs_src.$(TAR_EXTENSION)
	-@echo Make Archive date-specific  Subdirectory
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`; fi
	-@echo Copy files to date-specific Archive subdirectory $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`
	-@cp $(ARCHIVE_DIRECTORY)/rcs_src.$(COMPRESSED_TAR_EXTENSION) $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`



ifneq (java,$(findstring java,$(PLAT)))

archive_plat:
	-@echo Creating $(PLAT) tar file
	-(cd $(APPDIR); \
	tar $(TAR_ARGS) $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(TAR_EXTENSION) \
	plat/$(PLAT)/include  plat/$(PLAT)/lib/*.a plat/$(PLAT)/lib/*.lib plat/$(PLAT)/lib/*.dll plat/$(PLAT)/lib/*.$(DYNAMIC_LIB_EXTENSION) plat/$(PLAT)/lib/*-$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION) plat/$(PLAT)/lib/*.jar \
	`find plat/$(PLAT) -name \*.class -print` \
	`find plat/$(PLAT) -name \*.java -print` \
	plat/$(PLAT)/lib/librcs.o  plat/$(PLAT)/bin/*.dll \
	plat/$(PLAT)/bin/*.lib plat/$(PLAT)/bin/*.exe \
	plat/$(PLAT)/bin/*.com plat/$(PLAT)/*.mak plat/$(PLAT)/*.mdp \
	plat/$(PLAT)/*.prj plat/$(PLAT)/*.dsw plat/$(PLAT)/*.dsp \
	plat/$(PLAT)/*.plg plat/$(PLAT)/*.ncb plat/$(PLAT)/*.opt \
	plat/$(PLAT)/bin/nmlcfg* plat/$(PLAT)/bin/nmlcean* plat/$(PLAT)/bin/xsd* \
	etc/$(PLAT).def; )
	-@echo Compressing $(PLAT) tar file
	$(COMPRESS_COMMAND) -f $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(TAR_EXTENSION)
	-@echo Make Archive date-specific  Subdirectory
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`; fi
	-@echo Copy files to date-specific Archive subdirectory $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`
	-@cp $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(COMPRESSED_TAR_EXTENSION) $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`
	-@echo Make zip archive
	-\rm -r -f /tmp/rcslib
	-mkdir /tmp/rcslib
	-cd /tmp/rcslib
	-cp $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(COMPRESSED_TAR_EXTENSION) /tmp/rcslib
	-$(UNCOMPRESS_COMMAND)

else

archive_plat:
	-@echo Creating $(PLAT) tar file
	-(cd $(APPDIR); \
	tar $(TAR_ARGS) $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(TAR_EXTENSION) \
	`find plat/$(PLAT) -name \*.class -print` \
	`find plat/$(PLAT) -name \*.java -print` \
	`find plat/$(PLAT) -name \*.jar -print` \
	etc/RCS-Design* etc/diagnostics* etc/CodeGen* etc/run_make \
	etc/FinishUp etc/JDK* etc/exec_in_dir etc/run etc/java* etc/generic.def; )
	-@echo Compressing $(PLAT) tar file
	$(COMPRESS_COMMAND) -f $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(TAR_EXTENSION)
	-@echo Make Archive date-specific  Subdirectory
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m`; fi
	-@if(test ! -d $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`) ; then mkdir -p  $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`; fi
	-@echo Copy files to date-specific Archive subdirectory $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`
	-@cp $(ARCHIVE_DIRECTORY)/rcs_$(PLAT).$(COMPRESSED_TAR_EXTENSION) $(ARCHIVE_DIRECTORY)/`date +20%y/%m/%d`


# java
endif

top_level_headers: $(DEVP_HEADERS)
	echo HEADERS = $(HEADERS)
	echo DEVP_HEADERS = $(DEVP_HEADERS)


.PHONY : top_level_headers


ifeq (java, $(findstring java, $(PLAT)))

all:
	(cd java; $(MAKE) PLAT=$(PLAT) $(MFLAGS) COMPILE_TOGETHER=1 $@;)

else

ifeq (rtlinux, $(findstring rtlinux, $(PLAT)))

librcsbuild:

else

ifeq (rtai, $(PLAT))

librcsbuild:

else

librcsbuild: make_dirs $(DEVP_LIB_DIR)/rcsvers.o $(DEVP_LIBS)

endif

endif

ifneq (vxworks, $(findstring vxworks, $(PLAT)))

ifneq (dos, $(findstring dos, $(PLAT)))
ifneq (win, $(findstring win, $(PLAT)))

ifneq (rtlinux, $(findstring rtlinux, $(PLAT)))
ifneq (rtai, $(PLAT))

ifneq (mingw32, $(PLAT))

all:
	(cd cms; $(MAKE) PLAT=$(PLAT) $(MFLAGS) PLAT=$(PLAT)  nmlcfg nmlclean nmltest;)

else

all:
	(cd cms; $(MAKE) PLAT=$(PLAT) $(MFLAGS) PLAT=$(PLAT)  nmlcfg nmlclean nmltest;)
	-(cd $(DEVP_DIR) ; \rm -f rcslib_$(PLAT).zip;  \
	zip rcslib_$(PLAT).zip include/* src/* lib/librcs.a \
	bin/nmlcfg.exe bin/nmltest.exe bin/xsd2nmlh.exe; ) 

endif
# mingw32

endif
# rtai

endif
# rtlinux

#win
endif

endif
#dos

# java
endif

# vxworks
endif

.PHONY:

# Rules for specific libraries and binaries

$(DEVP_LIB_DIR)/librcs.o: $(DEVP_LIB_DIR)/librcsd.o
	-cp $(DEVP_LIB_DIR)/librcsd.o $(DEVP_LIB_DIR)/librcs.o
	-$(STRIP) $(STRIP_DEBUG_OPTION) $(DEVP_LIB_DIR)/librcs.o

$(DEVP_LIB_DIR)/librcsd.o: $(DEVP_LIB_DIR)/rcsvers.o
	-\rm -f $@
	$(COMPILER_SETUP); \
	$(LD) -r $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o $(CPLUSPLUSLINK) -o $@
	-chmod 664 $@

LIBRCSW_OBJS = $(subst inetnull.o,inetfile.o, $(LIBRCS_OBJS))

$(DEVP_LIB_DIR)/librcsw.o: \
	 $(LIBRCSW_OBJS:%=$(DEVP_LIB_DIR)/%) \
	$(DEVP_LIB_DIR)/libwww.o
	-\rm -f $@
	$(COMPILER_SETUP); \
	$(LD) -r $(LIBRCSW_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o $(DEVP_LIB_DIR)/libwww.o $(CPLUSPLUSLINK) -o $@
	-chmod 664 $@


$(DEVP_LIB_DIR)/librcsd.a: $(DEVP_LIB_DIR)/rcsvers.o $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%)
	-\rm -f $@
	$(COMPILER_SETUP); \
	$(AR) cr $@ $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o 
	$(COMPILER_SETUP); \
	$(RANLIB) $@
	-chmod 664 $@


ifdef RCSLIB_DYNAMIC_LINK_FLAG

$(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION): $(DEVP_LIB_DIR)/librcsd.a  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o
	-\rm -f $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
	-$(DYNAMIC_LIB_BUILD_TOOL) $(RCSLIB_DYNAMIC_LINK_FLAG)  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o -o $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)

ifdef RCS_VERSION
ifneq ($(RCS_VERSION),)
$(DEVP_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION): $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
	-\rm -f $@;
	-(cd $(DEVP_LIB_DIR); ln -s librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION) librcs.$(DYNAMIC_LIB_EXTENSION);)

ifneq ($(RELEASE_LIB_DIR),$(DEVP_LIB_DIR))

$(RELEASE_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION): $(RELEASE_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
	-(cd $(RELEASE_LIB_DIR); \rm -f librcs.$(DYNAMIC_LIB_EXTENSION); ln -s librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION) librcs.$(DYNAMIC_LIB_EXTENSION));


$(RELEASE_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION): $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
	-cp $^ $@


endif

endif

endif


librcs.$(DYNAMIC_LIB_EXTENSION): $(DEVP_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION)

.PHONY: librcs.$(DYNAMIC_LIB_EXTENSION)

endif

ifdef NO_STRIP_FOR_LIBS

$(DEVP_LIB_DIR)/librcs.a: $(DEVP_LIB_DIR)/librcsd.a $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o
	-\rm -f $@
	-cp $(DEVP_LIB_DIR)/librcsd.a $@

else
# lynx

$(DEVP_LIB_DIR)/librcs.a: $(DEVP_LIB_DIR)/librcsd.a $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o
	-\rm -f $@
	-cp $(DEVP_LIB_DIR)/librcsd.a $@
	-$(STRIP) -x $@
	-$(AR) -ts  $(DEVP_LIB_DIR)/librcs.a

# ! lynx
endif

linktest: linktest.cc
	g++ $^ -I$(RELEASE_INCLUDE_DIR) $(RELEASE_LIB_DIR)/librcs.a -o $@
	rm $@


ifneq (vxworks, $(findstring vxworks, $(PLAT)))


ifeq (lynx, $(findstring lynx, $(PLAT)))
librcs: $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%)
	-\rm -f $(DEVP_LIB_DIR)/librcsd.a
	$(COMPILER_SETUP); \
	$(AR) cr $(DEVP_LIB_DIR)/librcsd.a  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o  
	$(COMPILER_SETUP); \
	$(RANLIB)  $(DEVP_LIB_DIR)/librcsd.a
	-chmod 664  $(DEVP_LIB_DIR)/librcsd.a
	-\rm -f $(DEVP_LIB_DIR)/librcs.a
	-cp $(DEVP_LIB_DIR)/librcsd.a $(DEVP_LIB_DIR)/librcs.a

else
# ! lynx

ifdef RCSLIB_DYNAMIC_LINK_FLAG



librcs: $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%)
	-\rm -f $(DEVP_LIB_DIR)/librcsd.a
	$(COMPILER_SETUP); \
	$(AR) cr $(DEVP_LIB_DIR)/librcsd.a  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o  
	$(COMPILER_SETUP); \
	$(RANLIB)  $(DEVP_LIB_DIR)/librcsd.a
	-chmod 664  $(DEVP_LIB_DIR)/librcsd.a
	-\rm -f $(DEVP_LIB_DIR)/librcs.a
	-cp $(DEVP_LIB_DIR)/librcsd.a $(DEVP_LIB_DIR)/librcs.a
	-$(STRIP) -x  $(DEVP_LIB_DIR)/librcs.a
	-$(AR) -ts  $(DEVP_LIB_DIR)/librcs.a
	-\rm -f $(DEVP_LIB_DIR)/librcs.$(DYNAMIC_LIB_EXTENSION)
	-\rm -f $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
	-$(DYNAMIC_LIB_BUILD_TOOL) $(RCSLIB_DYNAMIC_LINK_FLAG) $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o  -o $(DEVP_LIB_DIR)/librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION)
ifneq ($(RCS_VERSION),)
	-(cd $(DEVP_LIB_DIR); ln -s librcs$(RCS_VERSION).$(DYNAMIC_LIB_EXTENSION) librcs.$(DYNAMIC_LIB_EXTENSION) );
endif

else

librcs: $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%)
	-\rm -f $(DEVP_LIB_DIR)/librcsd.a
	$(COMPILER_SETUP); \
	$(AR) cr $(DEVP_LIB_DIR)/librcsd.a  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o  
	$(COMPILER_SETUP); \
	$(RANLIB)  $(DEVP_LIB_DIR)/librcsd.a
	-chmod 664  $(DEVP_LIB_DIR)/librcsd.a
	-\rm -f $(DEVP_LIB_DIR)/librcs.a
	-cp $(DEVP_LIB_DIR)/librcsd.a $(DEVP_LIB_DIR)/librcs.a
	-$(STRIP) -x  $(DEVP_LIB_DIR)/librcs.a
	-$(AR) -ts  $(DEVP_LIB_DIR)/librcs.a

endif

# ! lynx
endif

else
# vxworks

librcs:  $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%)
	-\rm -f  $(DEVP_LIB_DIR)/librcs.o
	-\rm -f  $(DEVP_LIB_DIR)/librcsd.o
	$(COMPILER_SETUP); \
	$(LD) -r $(LIBRCS_OBJS:%=$(DEVP_LIB_DIR)/%) $(DEVP_LIB_DIR)/rcsvers.o  $(CPLUSPLUSLINK) -o $(DEVP_LIB_DIR)/librcsd.o
	-chmod 664  $(DEVP_LIB_DIR)/librcsd.o
	-\rm -f  $(DEVP_LIB_DIR)/librcsw.o
	$(COMPILER_SETUP); \
	$(LD) -r $(LIBRCSW_OBJS:%=$(DEVP_LIB_DIR)/%)$(DEVP_LIB_DIR)/rcsvers.o  $(CPLUSPLUSLINK) -o $(DEVP_LIB_DIR)/librcsw.o
	-chmod 664  $(DEVP_LIB_DIR)/librcsw.o
	-cp $(DEVP_LIB_DIR)/librcsd.o $(DEVP_LIB_DIR)/librcs.o
	-$(STRIP) $(STRIP_DEBUG_OPTION) $(DEVP_LIB_DIR)/librcs.o

# vxworks
endif

librcs: $(DEVP_LIB_DIR)/rcsvers.o

.PHONY: librcs archive archive_plat archive_src

test: $(DEVP_LIB_DIR)/test.o

memtest: $(DEVP_LIB_DIR)/memtest.o

ifneq (java, $(findstring java, $(PLAT)))

rcs_version: 	$(DEVP_LIB_DIR)/rcsvers.o

else
#java

rcs_version:
	-@echo Nothing to make for rcs_version.

#java
endif

ifdef COMPUTE_ALLFILES
ALLFILES := $(shell find . -type f -a '(' -name \*.c -o -name \*.cc -o -name \*.h -o -name \*.hh -o -name Make\* ')' )
endif

ifdef DEBUG_RCS_MAKE
$(warning ALLFILES=$(ALLFILES) )
endif

TAGS: $(ALLFILES)
	-find . -type f -a '(' -name \*.c -o -name \*.cc -o -name \*.h -o -name \*.hh ')' | etags -
all: TAGS

echoallfiles:
	echo ALLFILES=$(ALLFILES)

.PHONY: echoallfiles

ifneq (rtlinux, $(findstring rtlinux, $(PLAT)))

ifneq (rtai, $(PLAT))

$(DEVP_LIB_DIR)/rcsvers.o:  $(DEVP_DIR)/.stg_clssbuild $(DEVP_DIR)/.printbuild $(DEVP_DIR)/.posemathbuild $(DEVP_DIR)/.os_intfbuild $(DEVP_DIR)/.cmsbuild $(DEVP_DIR)/.nodebuild $(DEVP_DIR)/.nodeheaders $(DEVP_DIR)/.stg_clssheaders $(DEVP_DIR)/.printheaders $(DEVP_DIR)/.cmsheaders $(DEVP_DIR)/.os_intfheaders


else
#rtai

$(DEVP_LIB_DIR)/rcsvers.o: $(DEVP_DIR)/ $(DEVP_DIR)/.posemathbuild $(DEVP_DIR)/.cmsbuild

endif
#rtai

else
#rtlinux

$(DEVP_LIB_DIR)/rcsvers.o: $(ALLFILES) $(DEVP_DIR)/ $(DEVP_DIR)/.posemathbuild $(DEVP_DIR)/.cmsbuild

endif
#rtlinux

ifneq (java, $(findstring java, $(PLAT)))

headers: $(DEVP_DIR)/.headersbuild

$(DEVP_DIR)/.headersbuild: 
	touch $@

.PHONY: memtest sendmail rcs_version


install_libs_only:
	cp $(DEVP_LIBS) $(RELEASE_LIB_DIR)

.PHONY : install_libs_only


$(DEVP_DIR)/.stg_clssbuild : $(DEVP_DIR)/.headersbuild
	touch $@
	$(MAKE) PLAT=$(PLAT) -C stg_clss


$(DEVP_DIR)/.printbuild : $(DEVP_DIR)/.headersbuild 
	touch $@
	$(MAKE) PLAT=$(PLAT) -C print


$(DEVP_DIR)/.os_intfbuild : $(DEVP_DIR)/.headersbuild
	touch $@
	$(MAKE) PLAT=$(PLAT) -C os_intf


$(DEVP_DIR)/.posemathbuild : $(DEVP_DIR)/.headersbuild
	touch $@
	$(MAKE) PLAT=$(PLAT) -C posemath



$(DEVP_DIR)/.cmsbuild : $(DEVP_DIR)/.headersbuild
	touch $@
	$(MAKE) PLAT=$(PLAT) -C cms


$(DEVP_DIR)/.nodebuild : $(DEVP_DIR)/.headersbuild
	touch $@
	$(MAKE) PLAT=$(PLAT) -C node

.DELETE_ON_ERROR: $(DEVP_DIR)/.printbuild  $(DEVP_DIR)/.os_intfbuild  $(DEVP_DIR)/.posemathbuid  $(DEVP_DIR)/.cmsbuild  $(DEVP_DIR)/.nodebuild


.cleandepend: cleandependcommands

cleandependcommands:
	echo $@
	-\rm -f `find . -follow -name '.??*lists'`
	-\rm -f `find . -follow -name '.depend*'`
	-\rm -f `find $(DEVP_DIR) -follow -name '.??*build'`
	-\rm -f `find $(DEVP_DIR) -follow -name '.depend*'`
	touch .cleandepend

cleandepend: cleandependcommands

.PHONY: cleandepend cleandependcommands


ifndef BUILD_REALTIMEONLY

ifdef FORCE_DEPEND


depend: $(DEVP_DIR)/.dependbuild


$(DEVP_DIR)/.dependbuild:  $(DEVP_DIR)/.sourcesbuild $(DEVP_DIR)/.headersbuild
	$(MAKE) PLAT=$(PLAT) -C stg_clss depend
	$(MAKE) PLAT=$(PLAT) -C print depend
	$(MAKE) PLAT=$(PLAT) -C posemath depend
	$(MAKE) PLAT=$(PLAT) -C os_intf depend
	$(MAKE) PLAT=$(PLAT) -C cms depend
	$(MAKE) PLAT=$(PLAT) -C node depend
	-\rm .cleandepend
	touch $@


sources: $(DEVP_DIR)/.sourcesbuild

$(DEVP_DIR)/.sourcesbuild: 
	$(MAKE) PLAT=$(PLAT) -C stg_clss sources
	$(MAKE) PLAT=$(PLAT) -C print sources
	$(MAKE) PLAT=$(PLAT) -C posemath sources
	$(MAKE) PLAT=$(PLAT) -C os_intf sources
	$(MAKE) PLAT=$(PLAT) -C cms sources
	$(MAKE) PLAT=$(PLAT) -C node sources
	-\rm .cleandepend
	touch $@

endif

endif

clean: cleanbuild

cleanbuild:
	-\rm -f $(DEVP_DIR)/.??*build
	-\rm -f $(DEVP_DIR)/.??*headers
	-\rm -f $(DEVP_LIB_DIR)/rcsvers.o
	-\rm -f $(DEVP_INCLUDE_DIR)/rcs.hh
	-\rm -f $(DEVP_INCLUDE_DIR)/rcs_defs.hh
	-\rm -f $(DEVP_LIB_DIR)/librcs.a
	-\rm -f $(DEVP_LIB_DIR)/librcs*.so*



.PHONY: cleanbuild

endif


depend: .depend.lists


.DELETE_ON_ERROR: .depend.lists

.depend.lists: $(ALLFILES)
	-$(ECHO_MARK) if test -f .depend.lists ; then \rm .depend.lists >/dev/null 2>/dev/null ; fi
	-$(ECHO_MARK) touch .depend.lists
	-$(ECHO_MARK) find . -type f -a '(' -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.sourcesbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find . -type f -a '(' -name \*.h -o -name \*.hh ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.headersbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find cms -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.cmsbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find posemath -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.posemathbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find node -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.nodebuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find os_intf -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.os_intfbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find print -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null | awk '{printf("$$(DEVP_DIR)/.printbuild: %s\n",$$1);}' >>.depend.lists
	-$(ECHO_MARK) find stg_clss -type f -a '(' -name \*.h -o -name \*.hh -o \( -name Make\* -a -not -name \*~\* \)  -o -name \*.c -o -name \*.cc ')' 2>/dev/null  | awk '{printf("$$(DEVP_DIR)/.stg_clssbuild: %s\n",$$1);}' >>.depend.lists

# -- ifeq (YES, $(shell if test -f .depend.lists ; then echo YES ; fi))
# -- include .depend.lists
# -- endif

$(DEVP_DIR)/.os_intfheaders: $(OS_INTF_HEADERS:%=os_intf/%)
	touch $@
	$(MAKE) PLAT=$(PLAT) -C os_intf headers

$(DEVP_DIR)/.cmsheaders: $(CMS_HEADERS:%=cms/%)*
	touch $@
	$(MAKE) PLAT=$(PLAT) -C cms  headers


$(DEVP_DIR)/.stg_clssheaders : $(STG_CLSS_HEADERS:%=stg_clss/%)
	touch $@
	$(MAKE) PLAT=$(PLAT) -C stg_clss headers


$(DEVP_DIR)/.printheaders: $(PRINT_HEADERS:%=print/%)
	touch $@
	$(MAKE) PLAT=$(PLAT) -C print  headers


$(DEVP_DIR)/.posemathheaders: $(POSEMATH_HEADERS:%=posemath/%)
	touch $@
	$(MAKE) PLAT=$(PLAT) -C posemath  headers

$(DEVP_DIR)/.nodeheaders: $(NODE_HEADERS:%=node/%)
	touch $@
	$(MAKE) PLAT=$(PLAT) -C node  headers

.PHONY: makeallheaders

$(DEVP_DIR)/.headersbuild:$(DEVP_DIR)/.posemathheaders   $(DEVP_DIR)/.nodeheaders $(DEVP_DIR)/.stg_clssheaders $(DEVP_DIR)/.printheaders $(DEVP_DIR)/.cmsheaders $(DEVP_DIR)/.os_intfheaders

.DELETE_ON_ERROR: $(DEVP_DIR)/.headersbuild  $(DEVP_DIR)/.nodeheaders $(DEVP_DIR)/.stg_clssheaders $(DEVP_DIR)/.printheaders $(DEVP_DIR)/.cmsheaders $(DEVP_DIR)/.os_intfheaders

$(CMS_OBJS:%=$(DEVP_LIB_DIR)/%)	:
	-\rm -f $(DEVP_DIR)/.cmsbuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C cms
	touch $(DEVP_DIR)/.cmsbuild

$(NODE_OBJS:%=$(DEVP_LIB_DIR)/%):
	-\rm -f $(DEVP_DIR)/.nodebuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C node
	touch $(DEVP_DIR)/.nodebuild

$(PRINT_OBJS:%=$(DEVP_LIB_DIR)/%):
	-\rm -f $(DEVP_DIR)/.printbuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C print
	touch $(DEVP_DIR)/.printbuild

$(OS_INTF_OBJS:%=$(DEVP_LIB_DIR)/%):
	-\rm -f $(DEVP_DIR)/.os_intfbuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C os_intf
	touch $(DEVP_DIR)/.os_intfbuild

$(STG_CLSS_OBJS:%=$(DEVP_LIB_DIR)/%):
	-\rm -f $(DEVP_DIR)/.stg_clssbuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C stg_clss
	touch $(DEVP_DIR)/.stg_clssbuild 

$(POSEMATH_OBJS:%=$(DEVP_LIB_DIR)/%):
	-\rm -f $(DEVP_DIR)/.posemathbuild
	-@ echo need $@
	make PLAT=$(PLAT)  -C posemath $@
	touch $(DEVP_DIR)/.posemathbuild 

$(CMS_HEADERS:%=$(DEVP_INCLUDE_DIR)/%)	:
	-\rm -f $(DEVP_DIR)/.cmsheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C cms headers
	touch $(DEVP_DIR)/.cmsheaders

$(NODE_HEADERS:%=$(DEVP_INCLUDE_DIR)/%):
	-\rm -f $(DEVP_DIR)/.nodeheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C node headers
	touch $(DEVP_DIR)/.nodeheaders

$(PRINT_HEADERS:%=$(DEVP_INCLUDE_DIR)/%):
	-\rm -f $(DEVP_DIR)/.printheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C print headers
	touch $(DEVP_DIR)/.printheaders

$(OS_INTF_HEADERS:%=$(DEVP_INCLUDE_DIR)/%):
	-\rm -f $(DEVP_DIR)/.os_intfheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C os_intf headers
	touch $(DEVP_DIR)/.os_intfheaders

$(STG_CLSS_HEADERS:%=$(DEVP_INCLUDE_DIR)/%):
	-\rm -f $(DEVP_DIR)/.stg_clssheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C stg_clss headers
	touch $(DEVP_DIR)/.stg_clssheaders

$(POSEMATH_HEADERS:%=$(DEVP_INCLUDE_DIR)/%):
	-\rm -f $(DEVP_DIR)/.posemathheaders
	-@ echo need $@
	make PLAT=$(PLAT)  -C posemath headers
	touch $(DEVP_DIR)/.posemathheaders

all clean: remove_guess_warn_files

remove_guess_warn_files: 
	-@\rm -f .tcltkincflags_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.tcltkincflags_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .tcltklibflags_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.tcltklibflags_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .rtlinux_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.rtlinux_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .rtlinuxdir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.rtlinuxdir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .rtai_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.rtai_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .rtaidir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.rtaidir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .rtaidir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.rtaidir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .kernel_src_dir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.kernel_src_dir_guess_warn >/dev/null 2>/dev/null
	-@\rm -f .platname_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.platname_guess_warn >/dev/null 2>/dev/null
	-@\rm -f ~/.kernel_src_dir_printed >/dev/null 2>/dev/null
	-@\rm -f ~/.rtaidir_printed >/dev/null 2>/dev/null

.PHONY: remove_guess_warn_files




endif
# ifndef USE_OLD_MAKEFILES








