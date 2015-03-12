#! /bin/sh

if test -f Makefile ; then
    make distclean;
fi

chmod a+rw -R .lib* .dep* .multiplat* .last* plat lib/* include/* bin/* 2>/dev/null

\rm -rf .lib* .dep* .multiplat* .last* plat lib/* include/* bin/*
chmod a+rw -R /tmp/extra_test_dir /tmp/tni_* /tmp/distcheckhome *.xml /tmp/nml_test* /tmp/*.nml  /tmp/logstat* /tmp/rcsvers* /tmp/jdk* /tmp/jre* /tmp/mpb* /tmp/configure* /tmp/Makefile* /tmp/extra_tests* 2>/dev/null
\rm -rf /tmp/extra_test_dir /tmp/tni_* /tmp/distcheckhome /tmp/rcslib* /tmp/*.xml /tmp/*.nml /tmp/logstat* /tmp/rcsvers* /tmp/jdk* /tmp/jre* /tmp/mpb* /tmp/configure* /tmp/Makefile* /tmp/extra_tests*
sudo -n \rm -rf /tmp/extra_test_dir /tmp/tni_* /tmp/distcheckhome /tmp/rcslib* /tmp/*.xml /tmp/*.nml /tmp/logstat* /tmp/rcsvers* /tmp/jdk* /tmp/jre* /tmp/mpb* /tmp/configure* /tmp/Makefile* /tmp/extra_tests*


