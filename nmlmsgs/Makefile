

all: 


.PHONY: all


RCSLIB_DIR ?= $(shell cd .. ; pwd) 

ifdef VERBOSE
$(info RCSLIB_DIR=$(RCSLIB_DIR))
endif

CODEGENCMDLINE_JAR ?= $(shell find $(RCSLIB_DIR)/ -name CodeGenCmdLine.jar | head -n 1 )

ifdef VERBOSE
$(info CODEGENCMDLINE_JAR=$(CODEGENCMDLINE_JAR))
endif

RCS_JAR ?= $(shell find $(RCSLIB_DIR)/ -name rcs.jar | head -n 1 )

ifdef VERBOSE
$(info RCS_JAR=$(RCS_JAR))
endif

PIC_FLAG ?= -fPIC
JAVA ?= java
JAR ?= jar
JAVAC ?= javac
AR ?= ar
RANLIB ?= ranlib

src/%_n.cc : include/%.hh
	$(JAVA) -jar $(CODEGENCMDLINE_JAR) $^ -o $@

.static_lib_objs/%.o : src/%.cc
	$(CXX) $^ -c $(CXXFLAGS) -I$(RCSLIB_DIR)/include -Iinclude -o $@

.static_lib_objs :
	mkdir $@

.dynamic_lib_objs/%.o : src/%.cc
	$(CXX) $^ -c $(CXXFLAGS) $(PIC_FLAG) -I$(RCSLIB_DIR)/include -Iinclude -o $@

.dynamic_lib_objs :
	mkdir $@


MOAST_DIR ?= $(HOME)/moast
MOAST_INCLUDE_DIR ?= $(MOAST_DIR)/include

MOAST_HEADERS= \
	moastTypes.hh \
	moastLogRecordSuperCmd.hh \
	moastLogRecordSuperSts.hh \
	amMobJA.hh \
	primMobJA.hh \
	servoMobJA.hh \
	primDebug.hh \
	primSP.hh \
	sensorData.hh \
	navDataExt.hh \
	atrvTune.hh \
	nav200Dbg.hh \
	humanTracking.hh \
	humanTrackingDebug.hh

include/%.hh : $(MOAST_INCLUDE_DIR)/%.hh
	cp $< $@;


all: \
	$(addprefix include/,$(MOAST_HEADERS)) \
	.static_lib_objs \
	.dynamic_lib_objs \
	lib/libnmlmsgs.a \
	lib/libnmlmsgs.so \
	lib/nmlmsgs.jar \
	bin/nmlmsgs_svr 


NML_HEADERS= \
	fwCameraData_v2.hh \
	fwCameraLog_v2.hh \
	labjackData.hh \
	LadarCmd.hh \
	LadarData.hh \
	range3DMsg.hh \
	PointCloud.hh \
	log_data_format.hh \
	logPlaybackCmd.hh \
	logPlaybackSts.hh \
	logRecorderCmd.hh \
	logRecorderSts.hh \
	navdata.hh \
	navdata_v2.hh \
	playerPosData.hh \
	sickCmdSts.hh \
	sickData.hh \
	sickData_v3.hh \
	sickLoggern.hh \
	sicksafetyCmd.hh \
	sicksafetyData.hh \
	sicksafetySts.hh \
	SkyTrax_TCPMsg.hh \
	sonarData.hh \
	$(MOAST_HEADERS)

EXTRA_HEADERS= \
	moastNmlOffsets.hh \
	nmlOffsets.hh

.PRECIOUS : src/%.cc

.SECONDARY : src/%.cc %.cc

NML_CPP := $(addprefix src/,$(addsuffix _n.cc,$(basename $(notdir $(NML_HEADERS)))))

ifdef VERBOSE
$(info NML_CPP=$(NML_CPP))
endif

.SECONDARY :$(NML_CPP) 

NML_OBJS := $(addsuffix _n.o,$(basename $(notdir $(NML_HEADERS))))
#$(info OBJS=$(OBJS))

STATIC_OBJS := $(addprefix .static_lib_objs/,$(NML_OBJS))

DYNAMIC_OBJS := $(addprefix .dynamic_lib_objs/,$(NML_OBJS))

lib/libnmlmsgs.a : $(STATIC_OBJS)
	-@\rm -f $@
	$(AR) -cr $@ $^
	$(RANLIB) $@


lib/libnmlmsgs.so : $(DYNAMIC_OBJS)
	-@\rm -f $@
	$(CXX) -shared $^ -o $@

bin/nmlmsgs_svr:
	$(MAKE) -C svr ../$@

lib/nmlmsgs.jar : $(addprefix include/,$(NML_HEADERS))
	-@\rm -f $@
	-@ (mkdir src/java 2>/dev/null || /bin/true )
	( \
	 cd src/java; \
	 pwd; \
	 $(JAVA) -jar $(CODEGENCMDLINE_JAR) \
			select_from_all_files=true \
			generate_for_cpp=false \
			generate_for_java=true \
			java_package_name=nmlmsgs.nml \
			$(addprefix ../../,$^) \
			 -o nmlmsgs/nml/nmlmsgs_MsgDict.java ; )
	-@ ( mkdir src/java/build 2>/dev/null || /bin/true )
	( \
	 cd src/java; \
	 pwd; \
	 $(JAVAC) -classpath .:$(RCS_JAR) -d build nmlmsgs/nml/*.java; )
	( \
         cd src/java/build ; \
	 $(JAR) cf ../../../$@ nmlmsgs/nml/* ; \
	)

clean:	
	-\rm -rf src/java/nmlmsgs/nml \
		.static_lib_objs \
		.dynamic_lib_objs \
		src/*_n.cc \
		lib/nmlmsgs.jar \
		lib/libnmlmsgs* \
		bin/nmlmsgs_svr \
		$(RCSLIB_DIR)/lib/libnmlmsgs* \
		$(RCSLIB_DIR)/bin/nmlmsgs_svr \
		$(addprefix $(RCSLIB_DIR)/include/,$(NML_HEADERS)) 
	$(MAKE) -C svr $@

.PHONY: clean


#INSTALL_CMD ?= install

#install: all 
#	$(INSTALL_CMD) $(addprefix include/,$(NML_HEADERS)) $(RCSLIB_DIR)/include
#	$(INSTALL_CMD) $(addprefix include/,$(EXTRA_HEADERS)) $(RCSLIB_DIR)/include
#	$(INSTALL_CMD) bin/nmlmsgs_svr $(RCSLIB_DIR)/bin
#	$(INSTALL_CMD) lib/nmlmsgs.jar $(RCSLIB_DIR)/bin
#	$(INSTALL_CMD) lib/libnmlmsgs* $(RCSLIB_DIR)/lib

#.PHONY: install
