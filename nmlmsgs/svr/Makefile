
RCSLIB_DIR ?= $(shell cd ../../.. ; pwd) 
$(info RCSLIB_DIR=$(RCSLIB_DIR))

CXXFLAGS += -I$(RCSLIB_DIR)/include -I../include

all : ../bin/nmlmsgs_svr


../lib/libnmlmsgs.so :
	$(MAKE) -C .. lib/libnmlmsgs.so


../bin/nmlmsgs_svr : nmlmsgs_svr.o ../lib/libnmlmsgs.so
	$(CXX) -L $(RCSLIB_DIR)/lib -lrcs -L ../lib -lnmlmsgs $(LDFLAGS) $< -o $@


clean:
	-\rm -f nmlmsgs_svr.o ../bin/nmlmsgs_svr

.PHONY: all clean





