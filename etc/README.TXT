

Here's some info on how it works.
If I enter make without any arguments it will make the first target it encounters in the "Makefile" in the current directory.

This should be the line from Makefile.generic that looks like this.
The $(DEVP_0BJS) variable is just the variables $(OBJS) that users set in their
application makefiles with a path to the platform specific directory tacked on to each file. The same goes for $(DEVP_LIBS), and $(DEVP_BINS)

all: $(DEVP_OBJS) $(DEVP_LIBS) $(DEVP_BINS)


The make utility sees that  "all" it first needs to make the files listed in $(DEVP_OBJS). There is a rule in Makefile.generic for creating .o files from .cc files with the same name.

Then the make utility sees that to make the files listed in $(DEVP_BINS) there is no rule in Makefile.generic but there should be one for each particular file
in the application Makefile.

In our example it should find

$(DEVP_BIN_DIR)/rti: $(DEVP_LIB_DIR)/rti.o $(RCS_PLATLIB)/librcs.a
        $(CPLUSPLUS) $(CPLUSPLUSLINK) $^ -o $@


If the workspace directory were /users/shackle/my_workspace and we are compiling under Lynxos the variables expand like this

DEVP_BIN_DIR = /users/shackle/my_workspace/plat/lynxos/bin
DEVP_LIB_DIR = /users/shackle/my_workspace/plat/lynxos/lib
RCS_PLATLIB = /home/boole/rcslib/plat/lynxos/lib
CPLUSPLUS = g++                # From Makefile.lynxos
CPLUSPLUSLINK = /lib/librpc.a /lib/libbsd.a /lib/libg++.a # From Makefile.lynxos
$^ = (The dependancies of the current rule)
        = $(DEVP_LIB_DIR)/rti.o $(RCS_PLATLIB)/librcs.a
$@ = (The Target of the current rule)
        = $(DEVP_BIN_DIR)/rti


For us mere humans all the rule says is to make the rti program link rti.o and
librcs.a using g++. The variables just specify the details like which directories the various files are in etc. I could have included the directories directly rather than using the variables but that would mean I would have to write a different rule for each platform and if I moved the workspace or another user wanted to execute the same makefile from his/her own workspace then the makefile would have to be edited again.


Other variables of interest:

DEVP_LIB_DIR - Developers directory for libraries and object files.
DEVP_INCLUDE_DIR - Developers archive directory for header files
DEVP_SRC_DIR - Developers archive directory for souce files.
DEVP_BIN_DIR - Developers directory for executable binaries.

RELEASE_LIB_DIR - Application release directory for libraries and object files.
RELEASE_INCLUDE_DIR - Application release archive directory for header files
RELEASE_SRC_DIR - Application release archive directory for souce files.
RELEASE_BIN_DIR - Application release directory for executable binaries.

PLAT - Name of the platform that your compiling for.

RCS_PLATLIB - Location of the RCS library (Often called librcs.a)
RCS_PLATBIN - Location of utility programs associated with RCS library
RCS_INCLUDE - Location of header files associated with the RCS library

CFLAGS - Flags set in on of the Makefile.$(PLAT) files for compiling C
LOCAL_CFLAGS - May be set by the application makefile it will add additional flags for compiling C

CPLUSPLUSFLAGS - Flags set in on of the Makefile.$(PLAT) files for compiling C
LOCAL_CPLUSPLUSFLAGS - May be set by the application makefile it will add additional flags for compiling C
