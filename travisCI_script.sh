#!/bin/sh
(make distclean || true)
./configure
make
(rm -rf testtmp || true )
mkdir testtmp
( env tmpdir=`pwd`/testtmp make check || (cat ./test-suite.log && cat `find testtmp -name \*.log` && false ) )
 cd rcsjava_maven/rcslib && mvn -version && mvn -e compile &&  mvn test && mvn verify

