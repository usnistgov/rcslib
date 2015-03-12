# ;; -*- mode: makefile; -*-

ifndef RCSDIR
RCSDIR=/usr/local/rcslib
endif

override CXXFLAGS += -I$(RCSDIR)/include
override LDFLAGS += -L$(RCSDIR)/lib -lrcs

all: nml_unbounded_array_test_write nml_unbounded_array_test_read nml_unbounded_array_test_svr ub.nml

clean:
	-\rm -f nml_unbounded_array_test_write ub.nml nml_unbounded_array_test_msg_n.cc nml_unbounded_array_test_write.o nml_unbounded_array_test_msg_n.o nml_unbounded_array_test_read.o nml_unbounded_array_test_read core.*


ub.nml : ub.cfg
	$(RCSDIR)/bin/nmlcfg -D HOST=$(HOSTNAME) $^ -o $@


.PHONY : all clean

nml_unbounded_array_test_msg_n.cc : nml_unbounded_array_test_msg.hh
	java -jar $(RCSDIR)/bin/CodeGenCmdLine.jar $^

nml_unbounded_array_test_write.o : nml_unbounded_array_test_msg.hh

nml_unbounded_array_test_write: nml_unbounded_array_test_write.o nml_unbounded_array_test_msg_n.o
	-killall -INT $@
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

nml_unbounded_array_test_read.o : nml_unbounded_array_test_msg.hh

nml_unbounded_array_test_read: nml_unbounded_array_test_read.o nml_unbounded_array_test_msg_n.o 
	-killall -INT $@
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@


nml_unbounded_array_test_svr.o : nml_unbounded_array_test_msg.hh

nml_unbounded_array_test_svr: nml_unbounded_array_test_svr.o nml_unbounded_array_test_msg_n.o 
	-killall -INT $@
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@
