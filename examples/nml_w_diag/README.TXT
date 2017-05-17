# This directory contains some examples.

The examples send or recieve a message written in a format in nml_ex1.hh, a 
CodeGen tool creates some C++ and Java files for marshalling and unmarshalling 
data in the message. 
The communications parameters are stored in ex1.cfg they are parsed by nmlcfg 
and placed in ex1.nml
ex1.diag contains configuration parameters for the diagnostics tool, including
the list of buffers and the header files with their message definitions.

The examples require some Java classes that were not necessarilly built by
default. If you get errors from java that it can not load classes:

cd rcslib/src/java
make

The Java compiler/interpreter must be the official Sun version 1.4 or better
from http://java.sun.com 
The open source interpreters kaffe,gcj, blackdown etc that come by default
on several Linux distros are known not to work.


The examples must be built with GNU make, 
on Windows this means using cygwin(http://www.cygwin.com) 
or mingw (http://www.mingw.org)

to build everything run make setting the RCSLIB_DIR variable to the location
rcslib is installed. Depending on how things are installed other variables
you may need to set on the command line include CXX,CXXFLAGS, CPPFLAGS,LIBS,PLAT, CODEGEN_JAR,NMLCFG, JAVA, and JAVAC see GNUmakefile.

make RCSLIB_DIR=~/rcslib



to cleanup when done

make clean

To run the examples:

The programs may need environment variable LD_LIBRARY_PATH to be set to include the locatation of librcs.so, so for example in tcsh

setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:~/rcslib/lib

or in bash

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:~/rcslib/lib

The java programs need to be able to find the RCS java classes which I ussually accomplish by setting up a symbolic link. This can also be accomplished by setting CLASSPATH environment variable or adding -classpath options to the java commands

i.e.


The SERVER must be started first, leave it running in the background.

./nml_ex1_svr &


in more or less any order

./nml_ex1_write

./nml_ex1_read

java -cp  ~/rcslib/plat/java/lib/rcs.jar:. nml_ex1_j_write

java -cp  ~/rcslib/plat/java/lib/rcs.jar:. nml_ex1_j_read 


To run the diagnostics tool:

java -jar <pathtorcsjavalib>/diagapplet.jar ex1.diag

In the diag tool click the red "connected" checkbox (it should change to yellow), and change the drop down list below it from "Hierarchy" to "Auxilliary Channels".

To kill the server, bring it into the foreground and press Control-C.

ie

%1
^C

