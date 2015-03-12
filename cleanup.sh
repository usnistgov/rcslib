#!/bin/sh

\rm -f      src/test/nml_test_format_c_n.c
\rm -f      src/test/nml_test_format_n_ada.adb
\rm -f      src/test/nml_test_format_n_ada.ali
\rm -f      src/test/nml_test_format_n_ada.ads
\rm -f      src/test/otherheader_c_n.c
\rm -f      src/test/otherheader_n_ada.adb
\rm -f      src/test/otherheader_n_ada.ali
\rm -f      src/test/otherheader_n_ada.ads
\rm -f      src/test/otherheader_n.cc
\rm -f      src/test/nml_test_format_n.cc
\rm -f src/rcs_svn_vers.hh.r*
\rm -f src/rcs_svn_vers.hh.mine
make distclean || ( \rm -f rcs_config.h config.status nmlclean nmlcfgsvr rcs.jarMakefile libtool stamp-h1 xsd2nmlh CodeGenCmdLine.jar CodeGen.jar diagapplet.jar ; \rm -rf .deps .libs; )


