ifdef PLAT
USE_OLD_RCSLIB_MAKEFILES=1
endif

ifndef USE_OLD_RCSLIB_MAKEFILES

NO_MULTIPLAT_BUILD_WARN=1
NO_MAKE_WARN=1

include Makefile.extra_tests

else

ifeq (vxworks, $(findstring vxworks, $(PLAT)))
include Makefile.vx_tests
else
include Makefile.old_tests
endif


# USE_OLD_RCSLIB_MAKEFILES
endif
