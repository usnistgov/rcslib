

ifndef RCSLIB_DIR
RCSLIB_DIR:=$(shell cd ../.. ; pwd )
endif

CPPFLAGS += -I$(RCSLIB_DIR)/include

LIBS += -L$(RCSLIB_DIR)/lib -lrcs 

all : udp_direct.nml mynmlread mynmlwrite mynmlsvr udpdirectread udpdirectwrite

.PHONY: all

clean:
	-\rm -f udp_direct.nml mynmlread mynmlwrite mynmlsvr udpdirectread udpdirectwrite mymsg_n.cc *.o  mymsg_n_codegen_protos.hh


.PHONY: clean

mymsg_n.cc : mymsg.hh
	$(RCSLIB_DIR)/bin/CodeGen.sh $^


udp_direct.nml : udp_direct.cfg
	$(RCSLIB_DIR)/bin/nmlcfg $^ -o $@


mynmlsvr : mynmlsvr.o mymsg_n.o
	$(CXX) $(CXXFLAGS) $(LIBS) $^ -o $@

mynmlread : mynmlread.o mymsg_n.o
	$(CXX) $(CXXFLAGS) $(LIBS) $^ -o $@

mynmlwrite : mynmlwrite.o mymsg_n.o
	$(CXX) $(CXXFLAGS) $(LIBS) $^ -o $@
