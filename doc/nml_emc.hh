
#ifndef NML_EMC_HH
#define NML_EMC_HH

/* 
 To add a message, do the following:

 1.  #define a new NML_ id for the message, at the appropriate level
 (SERVO, TRAJ, etc.), in this file.
 2.  declare a derived class of NMLmsg, following the format for the
 existing ones, and stick in any arguments, in this file.
 3.  prototype the function called when that message is received,
 in this file.
 4.  In nml_emc.cc, modify the format function switch to include a
 label for the id in (1).
 5.  In nml_emc.cc, add an update method for the new derived class,
 calling cms->update for each argument.
 6.  Update the ini file with the type number and string.
 7.  Add the definition of the function prototyped in (3), in the
 style of the particular board/code which implements the level,
 e.g., servo.pmac.cc, hme.pmac.cc.
 8.  Modify the appropriate 'wrapper' for the level so that the function
 is called when the NML id is received, e.g., in hme.cc, seq.cc,
 canonlist.cc .
 9.  Make an entry in the manual pages, in man/manl .

 To add new WM data, do the following:

 1.  make an entry in the WM for the appropriate level (e.g., 
 NML_SERVO_WM .), in this file.
 2.  prototype the access function called to get the WM data out
 of the board/code which implements the level, in this file.
 3.  in nml_emc.cc, modify the update method for the WM of the level
 to include updates of the added WM data.
 4.  Add the definition of the function prototyped in (2), in the
 style of the particular board/code which implements the level.
 5.  Update the WM copying section of the hme main() loop to call
 the function every cycle.
 6.  Make an entry in the manual pages, in man/manl .

 */

/*	
  This file contains the ID's and C++ class declarations for the EMC
  NML messages and world model data; and function prototypes for the
  functions invoked when NML messages are received.

  Adding messages here means that you must add msg id's, classes,
  function prototypes in this file, and update the nmlformat() function,
  and add the class definition in nml.emc.cc .

  Modification history:

  22-Aug-1995  Fred Proctor added SERVO_WM::home_offset
  15-Aug-1995  Fred Proctor added NML_PARAMETER
  19-Jul-1995  Fred Proctor added io power state
  6-Jul-1995  Fred Proctor added spindle force commands
  11-May-1995 Will Shackleford added sequencer_state, sequencer_cmd_type, and 
  sequencer_queue_length to the NML_HME_WM.
  5-May-1995   Fred Proctor added home_side to io wm.
  28-Apr-1995  Will Shackleford changed NML_INTERP_UNION into an NMLmsg.
  28-Apr-1995  Will Shackleford changed NML_INTERP_LIST to use RCS_LINKED_LIST.
  11-Apr-1995  Fred Proctor added gear to io wm
  18-Mar-1995  Fred Proctor checked out before GM trip
  9-Mar-1995  Fred Proctor added gear changes
  8-Mar-1995  Fred Proctor added interpreter optional stop; io cycle
  start, stop
  3-Mar-1995  Fred Proctor added bias to gains structure; added units
  and servo scale;
  1-Mar-1995  Fred Proctor pulled out code from NML_INTERP_NODE,
  NML_INTERP_LIST classes and put in .cc file (compiler wants to
  inline code in header, an can't in general-- code should go in
  .cc files)
  27-Feb-1995  Fred Proctor removed unused cms args; added int arg to
  nml_servo_set_gains(); added gains to servo WM and associated access
  function servo_wm_gains(); created constructors for some types;
  changed NML_POSE to NML_ROTPOSE;
  26-Feb-1995  Fred Proctor removed traj cutter comp functions
  24-Feb-1995  Fred Proctor added NML id string table; WMSA_ESTOP_RESET
  as arg to WMSA_STATE; 
  23-Feb-1995  Fred Proctor added error string table
  18-Feb-1995  Fred Proctor inited error strings in interp wm to null;
  inited error_num to 0 in all wms; upped NML_JOINT_MAX to 6; added
  tool_file string to io wm; created NML_POSE type
  17-Feb-1995  Fred Proctor added #defines for return values for
  nml_interp_read().
  16-Feb-1995  Fred Proctor added 'errors' text field to interp wm
  11-Feb-1995  Fred Proctor added traj origin, rot origin back in--
  intent is to have implementations of these functions use the
  canonical interface, so single point of contact is used for
  external origin access (e.g., indication) and interpreter.
  Added io into hme WM.
  8-Feb-1995  Fred Proctor removed traj units, origin stuff
  1-Feb-1995  Fred Proctor added NML_ROTATION type. Added 
  NML_IO_READ_TOOL_TABLE.
  27-Jan-1995  Fred Proctor removed tool arg from load_tool; added 
  NML_IO_SHUTTLE_PALLETS
  25-Jan-1995  Fred Proctor added NML_TRAJ_ABS_JOG, NML_TRAJ_SLAVE;
  added a 'j' field in NML_INTERP_UNION
  30-Dec-1994  Fred Proctor added nml_io_execute_cycle(), ATR tool
  change stuff for IO, and io_wm_busy()
  27-Dec-1994  Fred Proctor added single block, block delete
  15-Dec-1994  Fred Proctor upped NML_TOOL_MAX to 128; added declaration
  for traj_wm_b() for B axis
  7-Dec-1994  Fred Proctor added #defines for default and max feed and
  traverse rates
  2-Dec-1994  Fred Proctor added feed, speed overrides in traj and io
  WMs
  29-Nov-1994  Fred Proctor added #defines for interp file and command
  string lens
  22-Oct-1994  Fred Proctor removed kinematics declaration, since it
  was already in here.
  18-Oct-1994  Sandor Szabo added kinematics declaration.  Kinematics found 
  in hme/minimill.cc
  6-Oct-1994  Fred Proctor added nml_wait_ready()
  29-Sep-1994  Fred Proctor added more INTERP commands
  19-Aug-1994  Fred Proctor added cutter radius comp and tool offset
  messages to traj
  6-Aug-1994   Fred Proctor added NML_TRAJ_ORIGIN, and origin to traj wm
  30-Jul-1994  Fred Proctor pulled WM data out of HME, put it in WMSA,
  and included WMSA.  This makes the HME an pure aggregate, with no
  stuff of its own.
  29-Jul-1994  Steve Legowik corrected enumeration syntax in WM types
  (see #ifndef LYNX)
  22-Jul-1994  Fred Proctor added HME_NOMODE and HME_NOSTATE, to 
  signify that HME WM is in unknown (startup) state
  22-Jul-1994  Fred Proctor added NML_TRAJ_PAUSE and NML_TRAJ_RESUME
  to implement a feed hold feature-- RCS may want to define these as
  implicit messages.  Added paused to NML_TRAJ_WM.
  18-Jul-1994  Fred Proctor added nml_io_init, exit (left out);
  still need to add them for interp and higher ones.  FIXME
  14-Jul-1994  Fred Proctor added HME stuff, for port of minimill
  controller to Monarch
  */

extern "C" {
#include <stdio.h>		/* NULL */
}

#include "rcs.hh"
#include "canon.hh"		/* CANON_PLANE, etc. */

/* error logger function */
void nml_log_error(unsigned long int error, ...);

/* Data primitives */

typedef unsigned int NML_PARAMETER;

/* units type */
typedef unsigned int NML_UNITS;

/* distance units */
#define NML_DISTANCE_UNITS_MM 1001
#define NML_DISTANCE_UNITS_CM 1002
#define NML_DISTANCE_UNITS_METERS 1003
#define NML_DISTANCE_UNITS_INCHES 1011
#define NML_DISTANCE_UNITS_FEET 1012

/* angular units */
#define NML_ANGULAR_UNITS_RADIANS 2001
#define NML_ANGULAR_UNITS_DEGREES 2002
#define NML_ANGULAR_UNITS_GRADS 2003

struct NML_CARTESIAN
{
  NML_CARTESIAN() {}
  NML_CARTESIAN(double _x, double _y, double _z) {x = _x; y = _y; z = _z;}
  double x;
  double y;
  double z;
  void update(CMS *);
};

struct NML_ROTATION
{
  NML_ROTATION() {}
  NML_ROTATION(double _a, double _b, double _c) {a = _a; b = _b; c = _c;}
  double a;
  double b;
  double c;
  void update(CMS *);
};

struct NML_ROTPOSE
{
  NML_ROTPOSE() {}
  NML_ROTPOSE(double _x, double _y, double _z,
	      double _a, double _b, double _c);
  NML_CARTESIAN cart;
  NML_ROTATION rot;
  void update(CMS *);
};

#define NML_JOINT_MAX 6		/* 6-axis support, for now */

struct NML_JOINT		/* avoid arrays, so we can use stack */
{
  NML_JOINT();
  NML_JOINT(double _j);
  double &operator [] (int n);

 private:
  double j[NML_JOINT_MAX], none;
};

struct NML_JOINT_FLAG		/* avoid arrays, so we can use stack */
{
  NML_JOINT_FLAG();
  NML_JOINT_FLAG(int _f);
  int &operator [] (int n);

 private:
  int f[NML_JOINT_MAX], none;
};

/* How we report errors in NML:
   Errors are written into the WM of each control node.
   Errors are cleared when new commands are received.
   Errors consists of an integer ID, and descriptive text.
   */
#define NML_ERROR_MAX 16	/* number of ints for error ids */

/* Registry of WM data types */

#define NML_SERVO_WM_TYPE ((NMLTYPE) 1001)
#define NML_TRAJ_WM_TYPE ((NMLTYPE) 1003)
#define NML_IO_WM_TYPE ((NMLTYPE) 1005)
#define NML_INTERP_WM_TYPE ((NMLTYPE) 1007)
#define NML_WMSA_WM_TYPE ((NMLTYPE) 1009)
#define NML_HME_WM_TYPE ((NMLTYPE) 1010)

/* NML types for World Model information */

struct NML_SERVO_GAINS
{
  NML_SERVO_GAINS() {}
  NML_SERVO_GAINS(double _bias, double _p, double _i, double _d,
		  double _vf, double _af)
  {
    bias = _bias, p = _p, i = _i, d = _d, vf = _vf, af = _af;
  }
  double bias;			/* additive output bias */
  double p;			/* proportional constant multiplier */
  double i;			/* integral constant multiplier */
  double d;			/* derivative constant multiplier */
  double vf;			/* velocity feedforward */
  double af;			/* acceleration feedforward */
};

/* structure for representing scaling from servo coordinates (encoder
   ticks, etc.) to servo external units reported in wm (mm, degrees, etc.) */
struct NML_SERVO_SCALE
{
  NML_SERVO_SCALE() {a = b = 1.0;} /* init to 1-- divides are done */
  NML_SERVO_SCALE(double _a, double _b) {a = _a, b = _b;}
  /* coefficients such that user units = a(sensor value)+b */
  double a;
  double b;
};

class NML_SERVO_WM : public NMLmsg
{
 public:
  NML_SERVO_WM() : NMLmsg(NML_SERVO_WM_TYPE, sizeof(NML_SERVO_WM)) {};
  void update(CMS *cms);

  double cycle;			/* # of seconds in a servo cycle */
  NML_UNITS units[NML_JOINT_MAX]; /* external units used for each axis */
  NML_SERVO_SCALE scale[NML_JOINT_MAX]; /* scale for internal->external */
  NML_SERVO_GAINS gains[NML_JOINT_MAX];	/* gains for each axis */
  NML_JOINT pos;		/* current position */
  NML_JOINT vel;		/* current velocity */
  NML_JOINT acc;		/* current acceleration */
  NML_JOINT delta;		/* diff between pos and commanded */
  NML_JOINT offset;		/* stick offsets here */
  NML_JOINT_FLAG home;		/* non-zero means corr. axis is homed */
  NML_JOINT home_offset;	/* offsets from trigger to call 0 */
  NML_JOINT_FLAG limit;		/* +/- means corr. axis at +/- limit */
  NML_JOINT homepos; /* abs. position when homed */
  NML_JOINT poslimit, neglimit;
  /* abs. position when at pos or neg limit */
  NML_JOINT_FLAG homing;	/* non-zero means homing */
  NML_JOINT_FLAG jogging;	/* non-zero means jogging */
};

/* prototypes of servo WM access functions */
extern const double servo_wm_cycle();
extern const NML_UNITS servo_wm_units(int axis); /* external units for axis */
extern const NML_SERVO_SCALE servo_wm_scale(int axis); /* conversion */
extern const NML_SERVO_GAINS servo_wm_gains(int axis); /* gains for axis */
extern const NML_JOINT servo_wm_pos();	/* current position */
extern const NML_JOINT servo_wm_vel();	/* current velocity */
extern const NML_JOINT servo_wm_acc();	/* current acceleration */
extern const NML_JOINT servo_wm_delta(); /* diff between pos and commanded */
extern const NML_JOINT servo_wm_offset(); /* stick offsets here */
extern const NML_JOINT_FLAG servo_wm_home(); /* non-zero means homed */
extern const NML_JOINT servo_wm_home_offset();
extern const NML_JOINT_FLAG servo_wm_limit();	/* +/- means at +/- limit */
extern const NML_JOINT servo_wm_homepos(); /* home pos, in servo units */
extern const NML_JOINT servo_wm_poslimit(); /* positive limit, in servo units */
extern const NML_JOINT servo_wm_neglimit(); /* neg limit, in servo units */
extern const NML_JOINT_FLAG servo_wm_homing();
extern const NML_JOINT_FLAG servo_wm_jogging();

extern NML_ROTPOSE servo_forward_kinematics(NML_JOINT &joint);
extern NML_JOINT servo_inverse_kinematics(NML_ROTPOSE &pose);
extern int nml_servo_set_home_offset(int axis, double offset);

/* system defaults for feed and traverse rate-- controller will
   come up with these values in effect */
extern double NML_TRAJ_DEFAULT_FEED_RATE;
extern double NML_TRAJ_MAX_FEED_RATE;
extern double NML_TRAJ_DEFAULT_TRAVERSE_RATE;
extern double  NML_TRAJ_MAX_TRAVERSE_RATE;

class NML_TRAJ_WM : public NMLmsg
{
 public:
  NML_TRAJ_WM() : NMLmsg(NML_TRAJ_WM_TYPE, sizeof(NML_TRAJ_WM)) {};
  void update(CMS *cms);

  double cycle;			/* # of seconds in a traj cycle */
  double neighborhood;		/* how close for completion */
  int inpos;			/* non-zero means we're there */
  int queue;			/* number of pending motions */
  NML_ROTPOSE pos;		/* current position */
  NML_ROTPOSE vel;		/* current velocity */
  NML_ROTPOSE acc;		/* current acceleration */
  NML_ROTPOSE delta;		/* diff between pos and commanded */
  NML_ROTPOSE offset;		/* stick offsets here */
  NML_ROTPOSE difference;	/* distance to go until end of current move */
  double feed;			/* current feedrate for linear moves */
  double traverse;		/* current traverse rate for free moves */
  double feed_override;		/* positive scale factor for feed */
  int feed_override_disabled;	/* non-zero means disabled */
  int paused;			/* non-zero means motion paused */
  NML_ROTPOSE origin;		/* offset from abs origin; base for moves */
  CANON_ORIGIN origin_type;	/* absolute, or part coords (ref'ed to
				   'origin' above) */
  double tool_offset;		/* tool length offset */
};

/* prototypes of traj WM access functions */
extern const double traj_wm_cycle();	
extern const double traj_wm_neighborhood(); 
extern const int traj_wm_inpos();	
extern const int traj_wm_queue();	
extern const NML_ROTPOSE traj_wm_pos(); 
extern const NML_ROTPOSE traj_wm_vel(); 
extern const NML_ROTPOSE traj_wm_acc(); 
extern const NML_ROTPOSE traj_wm_delta(); 
extern const NML_ROTPOSE traj_wm_offset(); 
extern const double traj_wm_feed();	
extern const double traj_wm_traverse();
extern const double traj_wm_feed_override();
extern const int traj_wm_feed_override_disabled();
extern const int traj_wm_paused();	
extern const NML_ROTPOSE traj_wm_origin(); 
extern const CANON_ORIGIN traj_wm_origin_type();
extern const double traj_wm_tool_offset();
extern const NML_ROTPOSE traj_wm_difference(); 

/* how many tools we allow */
#define NML_TOOL_FILENAME_LEN 80 /* number of chars in a file name */
#define NML_TOOL_MAX 68	/* number of tools, and associated offsets */
struct TOOL_TABLE
{
  int id;
  double length;
  double diameter;
};

/* allowable values for NML_IO_WM::status */
#define NML_IO_STATUS_DONE 1	/* cmd done */
#define NML_IO_STATUS_EXEC 2	/* cmd still running */
#define NML_IO_STATUS_ERROR_TOO_LONG 4 /* timed out */
#define NML_IO_STATUS_ERROR_INIT_COND 5	/* initial conditions not met */

/* AC power status */
#define NML_IO_POWER_ON 1	/* normal */
#define NML_IO_POWER_OFF 2	/* it's off */
#define NML_IO_POWER_LOW 3	/* it's going off */

/* Emergency stop */
#define NML_IO_ESTOP_ON 1	/* estop button depressed */
#define NML_IO_ESTOP_OFF 2	/* estop button released */

class NML_IO_WM : public NMLmsg
{
 public:
  NML_IO_WM() : NMLmsg(NML_IO_WM_TYPE, sizeof(NML_IO_WM)) {};
  void update(CMS *cms);

  int tool;			/* current tool */
  int tool_in_changer;		/* current tool in the loading claw */
  int spindle;			/* non-zero means spindle enabled */
  int pallet;			/* current pallet on table */
  double speed;			/* commanded spindle speed */
  double delta;			/* spindle position, from home */
  int mist;			/* non-zero means mist coolant on */
  int flood;			/* non-zero means flood coolant on */
  double speed_override;	/* scale factor for spindle speed */
  int speed_override_disabled;	/* non-zero means override disabled */
  int pendant_mode;		/* enabled, disabled */
  int coolant_mode;		/* on, off, auto */
  int shuttle_mode;		/* enabled, disabled */
  int pallet_mode;		/* auto, unclamp */
  int pocket;			/* current pocket of the tool magazine */
  int gear;			/* 1=low, 2=med, 3=high */
  NML_JOINT_FLAG home_side;	/* <0 = negative side, >0 = positive side */
  int busy;			/* non-zero means IO is active, and motion
				   should not be initiated */
  int status;			/* done, exec, errors */
  int power;			/* AC power state */
  int estop;			/* state of the ESTOP */
  int spindle_busy;		/* non-zero means spindle moving */
  int spindle_enable_state;	/* 1 = spindle enabled */

  /* tool_table contains offsets assigned to tools, for indices in range
   0..NML_TOOL_MAX. Index 0 will not be used for machines that start
   at slot (pocket) 1, but one can always reference 
   tool_table[NML_TOOL_MAX] and get the last tool, or
   tool_table[1] and get the tool in slot 1, or
   tool_table[0] and get the tool in slot 0, if such
   a slot exists. */
  TOOL_TABLE tool_table[NML_TOOL_MAX+1]; 
  char tool_file[NML_TOOL_FILENAME_LEN];
};

/* prototypes for io WM access functions */
extern const int io_wm_tool();	
extern const int io_wm_tool_in_changer();
extern const int io_wm_spindle();
extern const int io_wm_pallet();	
extern const double io_wm_speed();
extern const double io_wm_delta();
extern const int io_wm_mist();
extern const int io_wm_flood();
extern const double io_wm_speed_override();
extern const int io_wm_speed_override_disabled();
extern const int io_wm_pendant_mode();
extern const int io_wm_coolant_mode();
extern const int io_wm_shuttle_mode();
extern const int io_wm_pallet_mode();
extern const int io_wm_pocket();
extern const int io_wm_gear();
extern const NML_JOINT_FLAG io_wm_home_side();
extern const int io_wm_busy();
extern const int io_wm_status();
extern const int io_wm_power();
extern const int io_wm_estop();
extern const int io_wm_spindle_busy();
extern const int io_wm_spindle_enable_state();
extern const TOOL_TABLE io_wm_tool_table(int index);
extern const char *io_wm_tool_file();
/* FIXME-- io_wm_mode(),state() ? */

/* interpreter states */
#define INTERP_IDLE 1
#define INTERP_RUNNING 2
#define INTERP_PAUSED 3

#define NML_INTERP_FILE_LEN 80
#define NML_INTERP_COMMAND_LEN 80

/* return values from functions */
#define NML_INTERP_READ_OK 0
#define NML_INTERP_READ_ERROR -1
#define NML_INTERP_READ_EOF 1

class NML_INTERP_WM : public NMLmsg
{
 public:
  NML_INTERP_WM() : NMLmsg(NML_INTERP_WM_TYPE, sizeof(NML_INTERP_WM)) {};
  void update(CMS *cms);

  char file[NML_INTERP_FILE_LEN]; /* name of program being interpreted */
  char command[NML_INTERP_COMMAND_LEN];	/* text of line being interpreted */
  int line;			/* last line read (reads ahead) */
  int mode;			/* one of INTERP_IDLE, ... */
  NML_CARTESIAN offset;		/* work zero (part) origin */
  int single_block;		/* non-zero if single stepping */
  int block_delete;		/* non-zero means optional delete active */
  int optional_stop;		/* non-zero means optional stop active */
};

/* prototypes of interpreter WM access functions */
extern const char *interp_wm_file();
extern const char *interp_wm_command();
extern const int interp_wm_line();
extern const int interp_wm_mode();
extern const NML_CARTESIAN interp_wm_offset();
extern const int interp_wm_single_block();
extern const int interp_wm_block_delete();
extern const int interp_wm_optional_stop();

/* WMSA modes and states */

#define WMSA_NOMODE 0
#define WMSA_EDIT 1
#define WMSA_AUTO 2
#define WMSA_MDI 3
#define WMSA_MANUAL 4

#define WMSA_NOSTATE 0
#define WMSA_OFF 1		/* turn machine off */
#define WMSA_ON 2		/* turn machine on */
#define WMSA_ESTOP 3		/* estop the machine */
#define WMSA_ESTOP_RESET 4	/* reset the estop */

class NML_WMSA_WM : public NMLmsg
{
 public:
  NML_WMSA_WM() : NMLmsg(NML_WMSA_WM_TYPE, sizeof(NML_WMSA_WM))
  {
    mode = WMSA_NOMODE, state = WMSA_NOSTATE;
  };
  void update(CMS *cms);

  int mode;
  int state;
};

/* prototypes of WMSA WM access functions */
extern const int wmsa_wm_mode();
extern const int wmsa_wm_state();

#define HME_CODES_SIZE 9

class NML_HME_WM : public NMLmsg
{
 public:
  NML_HME_WM() : NMLmsg(NML_HME_WM_TYPE, sizeof(NML_HME_WM))
  {
    int t;
    for (t = 0; t < NML_ERROR_MAX; t++)
      error[t] = 0;
    error_num = 0;
  };
  void update(CMS *cms);

  /* need to incorporate SERVO, TRAJ, IO stuff as well, since
     HME is interface to this stuff in aggregated controller */
  NML_SERVO_WM servo;
  NML_TRAJ_WM traj;
  NML_INTERP_WM interp;	
  NML_WMSA_WM wmsa;
  NML_IO_WM io;

  unsigned long int error[NML_ERROR_MAX]; /* error status */
  int error_num;		/* cumulative errors */
  int report_error(unsigned long int _error);

  /* additional HME WM data; some is KT800-specific */
  int sequencer_state;
  NMLTYPE sequencer_cmd_type;
  int sequencer_queue_length;

  /* active G codes, etc. -- first element is line number,
     rest are -1 (invalid), or active code. */
  int active_g_codes[HME_CODES_SIZE];
  int active_m_codes[HME_CODES_SIZE];

  /* current line executing */
  int active_line;
};

/* Registry of NML data type identifiers */

/* NML messages to servo */

#define NML_SERVO_INIT_TYPE ((NMLTYPE) 101)
#define NML_SERVO_EXIT_TYPE ((NMLTYPE) 103)
#define NML_SERVO_SET_CYCLE_TYPE ((NMLTYPE) 105)
#define NML_SERVO_SET_GAINS_TYPE ((NMLTYPE) 107)
#define NML_SERVO_SET_LIMITS_TYPE ((NMLTYPE) 108)
#define NML_SERVO_SET_OFFSET_TYPE ((NMLTYPE) 109)
#define NML_SERVO_ENABLE_TYPE ((NMLTYPE) 111)
#define NML_SERVO_DISABLE_TYPE ((NMLTYPE) 113)
#define NML_SERVO_HOME_TYPE ((NMLTYPE) 115)
#define NML_SERVO_JOG_TYPE ((NMLTYPE) 117)
#define NML_SERVO_JOG_STOP_TYPE ((NMLTYPE) 119)
#define NML_SERVO_POS_TYPE ((NMLTYPE) 121)
#define NML_SERVO_INCR_JOG_TYPE ((NMLTYPE) 123)
#define NML_SERVO_ABS_JOG_TYPE ((NMLTYPE) 124)
#define NML_SERVO_SET_SCALE_TYPE ((NMLTYPE) 125)
#define NML_SERVO_HALT_TYPE ((NMLTYPE) 127)

/* NML messages to TRAJ */

#define NML_TRAJ_INIT_TYPE ((NMLTYPE) 201)
#define NML_TRAJ_EXIT_TYPE ((NMLTYPE) 203)
#define NML_TRAJ_SET_CYCLE_TYPE ((NMLTYPE) 205)
#define NML_TRAJ_SET_NEIGHBORHOOD_TYPE ((NMLTYPE) 207)
#define NML_TRAJ_SET_OFFSET_TYPE ((NMLTYPE) 209)
#define NML_TRAJ_SET_TRAVERSE_RATE_TYPE ((NMLTYPE) 211)
#define NML_TRAJ_SET_FEED_RATE_TYPE ((NMLTYPE) 213)
#define NML_TRAJ_SET_FEED_OVERRIDE_TYPE ((NMLTYPE) 214)
#define NML_TRAJ_STRAIGHT_TRAVERSE_TYPE ((NMLTYPE) 215)
#define NML_TRAJ_STRAIGHT_FEED_TYPE ((NMLTYPE) 217)
#define NML_TRAJ_ARC_FEED_TYPE ((NMLTYPE) 219)
#define NML_TRAJ_PAUSE_TYPE ((NMLTYPE) 221)
#define NML_TRAJ_RESUME_TYPE ((NMLTYPE) 223)
#define NML_TRAJ_SET_ORIGIN_TYPE ((NMLTYPE) 230)
#define NML_TRAJ_USE_ORIGIN_TYPE ((NMLTYPE) 231)
#define NML_TRAJ_USE_TOOL_LENGTH_OFFSET_TYPE ((NMLTYPE) 251)
#define NML_TRAJ_START_SEQUENCE_TYPE ((NMLTYPE) 260)
#define NML_TRAJ_STOP_SEQUENCE_TYPE ((NMLTYPE) 261)
#define NML_TRAJ_DWELL_TYPE ((NMLTYPE) 263)
#define NML_TRAJ_SLAVE_TYPE ((NMLTYPE) 264)
#define NML_TRAJ_HALT_TYPE ((NMLTYPE) 265)
#define NML_TRAJ_DISABLE_FEED_OVERRIDE_TYPE ((NMLTYPE) 266)

/* NML messages to IO */

#define NML_IO_INIT_TYPE ((NMLTYPE) 321)
#define NML_IO_EXIT_TYPE ((NMLTYPE) 323)
#define NML_IO_SPINDLE_ON_TYPE ((NMLTYPE) 303)
#define NML_IO_SPINDLE_OFF_TYPE ((NMLTYPE) 305)
#define NML_IO_SPINDLE_RETRACT_TYPE ((NMLTYPE) 307)
#define NML_IO_SPINDLE_ROTATE_TYPE ((NMLTYPE) 308)
#define NML_IO_MIST_ON_TYPE ((NMLTYPE) 309)
#define NML_IO_MIST_OFF_TYPE ((NMLTYPE) 311)
#define NML_IO_FLOOD_ON_TYPE ((NMLTYPE) 313)
#define NML_IO_FLOOD_OFF_TYPE ((NMLTYPE) 315)
#define NML_IO_SET_SPEED_OVERRIDE_TYPE ((NMLTYPE) 317)
#define NML_IO_READ_TOOL_TABLE_TYPE ((NMLTYPE) 320)
#define NML_IO_PREP_FOR_TOOL_TYPE ((NMLTYPE) 324)
#define NML_IO_LOAD_TOOL_TYPE ((NMLTYPE) 325)
#define NML_IO_UNLOAD_TOOL_TYPE ((NMLTYPE) 326)
#define NML_IO_SHUTTLE_PALLETS_TYPE ((NMLTYPE) 327)
#define NML_IO_EXECUTE_CYCLE_TYPE ((NMLTYPE) 330)
#define NML_IO_PENDANT_MODE_TYPE ((NMLTYPE) 331)
#define NML_IO_COOLANT_MODE_TYPE ((NMLTYPE) 332)
#define NML_IO_SET_POCKET_NUMBER_TYPE ((NMLTYPE) 333)
#define NML_IO_SHUTTLE_MODE_TYPE ((NMLTYPE) 334)
#define NML_IO_PALLET_MODE_TYPE ((NMLTYPE) 335)
#define NML_IO_MODE_TYPE ((NMLTYPE) 340)
#define NML_IO_STATE_TYPE ((NMLTYPE) 341)
#define NML_IO_CYCLE_START_TYPE ((NMLTYPE) 343)
#define NML_IO_CYCLE_STOP_TYPE ((NMLTYPE) 344)
#define NML_IO_PREP_FOR_GEAR_CHANGE_TYPE ((NMLTYPE) 345)
#define NML_IO_CHANGE_GEARS_TYPE ((NMLTYPE) 346)
#define NML_IO_ORIENT_SPINDLE_TYPE ((NMLTYPE) 347)
#define NML_IO_HALT_TYPE ((NMLTYPE) 348)
#define NML_IO_USE_SPINDLE_FORCE_TYPE ((NMLTYPE) 349)
#define NML_IO_USE_NO_SPINDLE_FORCE_TYPE ((NMLTYPE) 350)
#define NML_IO_ARC_SPINDLE_FORCE_TYPE ((NMLTYPE) 351)
#define NML_IO_SET_TOOL_NUMBER_TYPE ((NMLTYPE) 352)
#define NML_IO_SET_PALLET_NUMBER_TYPE ((NMLTYPE) 353)
#define NML_IO_DISABLE_SPEED_OVERRIDE_TYPE ((NMLTYPE) 354)

/* NML messages to INTERP */

#define NML_INTERP_INIT_TYPE ((NMLTYPE) 401)
#define NML_INTERP_EXIT_TYPE ((NMLTYPE) 403)
#define NML_INTERP_OPEN_TYPE ((NMLTYPE) 405)
#define NML_INTERP_READ_TYPE ((NMLTYPE) 407)
#define NML_INTERP_EXECUTE_TYPE ((NMLTYPE) 409)
#define NML_INTERP_RUN_TYPE ((NMLTYPE) 410)
#define NML_INTERP_PAUSE_TYPE ((NMLTYPE) 411)
#define NML_INTERP_RESUME_TYPE ((NMLTYPE) 413)
#define NML_INTERP_HALT_TYPE ((NMLTYPE) 415)
#define NML_INTERP_CLOSE_TYPE ((NMLTYPE) 417)
#define NML_INTERP_SINGLE_BLOCK_TYPE ((NMLTYPE) 419)
#define NML_INTERP_BLOCK_DELETE_TYPE ((NMLTYPE) 421)
#define NML_INTERP_STEP_TYPE ((NMLTYPE) 422)
#define NML_INTERP_OPTIONAL_STOP_TYPE ((NMLTYPE) 425) /* sets mode */
#define NML_INTERP_UNION_TYPE ((NMLTYPE) 426)
#define NML_INTERP_EXEC_OPTIONAL_STOP_TYPE ((NMLTYPE) 427) /* does it */

/* nml messages to WMSA */

#define NML_WMSA_INIT_TYPE ((NMLTYPE) 501)
#define NML_WMSA_EXIT_TYPE ((NMLTYPE) 503)
#define NML_WMSA_MODE_TYPE ((NMLTYPE) 505)
#define NML_WMSA_STATE_TYPE ((NMLTYPE) 507)
#define NML_WMSA_HALT_TYPE ((NMLTYPE) 508)

/* Generic NML messages */
#define NML_SET_PARAMETER_TYPE ((NMLTYPE) 600)
#define NML_GENERIC_TYPE ((NMLTYPE) 601)

/* Note:  no specific NML messages to HME-- HME aggregates WMSA,
   TRAJ, SERVO, and IO */

/* timed-out wait for read-ready status on NML channel.  Returns
   0 if OK, -1 if timed out. */
extern int nml_wait_ready(NML *, double timeout = 0.030, int times = 33);

/* NML formatting function prototype */
extern int nmlformat(NMLTYPE type, void *buffer, CMS *cms);

/* Application data types */

/* NML types for SERVO */

class NML_SERVO_INIT : public NMLmsg
{
 public:
  NML_SERVO_INIT() : NMLmsg(NML_SERVO_INIT_TYPE, sizeof(NML_SERVO_INIT)) {};
  void update();
};
extern int nml_servo_init();

class NML_SERVO_EXIT : public NMLmsg
{
 public:
  NML_SERVO_EXIT() : NMLmsg(NML_SERVO_EXIT_TYPE, sizeof(NML_SERVO_EXIT)) {};
  void update();
};
extern int nml_servo_exit();

class NML_SERVO_SET_CYCLE : public NMLmsg
{
 public:
  NML_SERVO_SET_CYCLE() : NMLmsg(NML_SERVO_SET_CYCLE_TYPE, sizeof(NML_SERVO_SET_CYCLE)) {};
  void update(CMS *cms);
  
  double cycle;
};
extern int nml_servo_set_cycle(double cycle);

class NML_SERVO_SET_GAINS : public NMLmsg
{
 public:
  NML_SERVO_SET_GAINS() : NMLmsg(NML_SERVO_SET_GAINS_TYPE, sizeof(NML_SERVO_SET_GAINS)) {};
  void update(CMS *cms);

  int axis;
  NML_SERVO_GAINS gains;
};
extern int nml_servo_set_gains(int axis, NML_SERVO_GAINS gains);

class NML_SERVO_SET_LIMITS : public NMLmsg
{
 public:
  NML_SERVO_SET_LIMITS() : NMLmsg(NML_SERVO_SET_LIMITS_TYPE, sizeof(NML_SERVO_SET_LIMITS)) {};
  void update(CMS *cms);

  int axis;
  double pos;
  double neg;
};
extern int nml_servo_set_limits(int axis, double pos, double neg);

class NML_SERVO_SET_OFFSET : public NMLmsg
{
 public:
  NML_SERVO_SET_OFFSET() : NMLmsg(NML_SERVO_SET_OFFSET_TYPE, sizeof(NML_SERVO_SET_OFFSET)) {};
  void update(CMS *cms);

  NML_JOINT offset;
};
extern int nml_servo_set_offset(NML_JOINT &offset);

class NML_SERVO_ENABLE : public NMLmsg
{
 public:
  NML_SERVO_ENABLE() : NMLmsg(NML_SERVO_ENABLE_TYPE, sizeof(NML_SERVO_ENABLE)) {};
  void update(CMS *cms);

  int axis;
};
extern int nml_servo_enable(int axis);

class NML_SERVO_DISABLE : public NMLmsg
{
 public:
  NML_SERVO_DISABLE() : NMLmsg(NML_SERVO_DISABLE_TYPE, sizeof(NML_SERVO_DISABLE)) {};
  void update(CMS *cms);

  int axis;
};
extern int nml_servo_disable(int axis);

class NML_SERVO_HOME : public NMLmsg
{
 public:
  NML_SERVO_HOME() : NMLmsg(NML_SERVO_HOME_TYPE, sizeof(NML_SERVO_HOME)) {};
  void update(CMS *cms);

  int axis;
};
extern int nml_servo_home(int axis);

class NML_SERVO_JOG : public NMLmsg
{
 public:
  NML_SERVO_JOG() : NMLmsg(NML_SERVO_JOG_TYPE, sizeof(NML_SERVO_JOG)) {};
  void update(CMS *cms);

  int axis;
  double speed;
};
extern int nml_servo_jog(int axis, double speed);

class NML_SERVO_JOG_STOP : public NMLmsg
{
 public:
  NML_SERVO_JOG_STOP() : NMLmsg(NML_SERVO_JOG_STOP_TYPE, sizeof(NML_SERVO_JOG_STOP)) {};
  void update(CMS *cms);

  int axis;
};
extern int nml_servo_jog_stop(int axis);

class NML_SERVO_POS : public NMLmsg
{
 public:
  NML_SERVO_POS() : NMLmsg(NML_SERVO_POS_TYPE, sizeof(NML_SERVO_POS)) {};
  void update(CMS *cms);

  NML_JOINT joint;
};
extern int nml_servo_pos(NML_JOINT &joint);

class NML_SERVO_INCR_JOG : public NMLmsg
{
 public:
  NML_SERVO_INCR_JOG() : NMLmsg(NML_SERVO_INCR_JOG_TYPE, sizeof(NML_SERVO_INCR_JOG)) {};
  void update(CMS *cms);

  int axis;
  double speed;
  double incr;
};
extern int nml_servo_incr_jog(int axis, double speed, double incr);

class NML_SERVO_ABS_JOG : public NMLmsg
{
 public:
  NML_SERVO_ABS_JOG() : NMLmsg(NML_SERVO_ABS_JOG_TYPE, sizeof(NML_SERVO_ABS_JOG)) {};
  void update(CMS *cms);

  int axis;
  double speed;
  double abspos;
};
extern int nml_servo_abs_jog(int axis, double speed, double abspos);

class NML_SERVO_SET_SCALE : public NMLmsg
{
 public:
  NML_SERVO_SET_SCALE() : NMLmsg(NML_SERVO_SET_SCALE_TYPE, sizeof(NML_SERVO_SET_SCALE)) {};
  void update(CMS *cms);

  int axis;
  NML_SERVO_SCALE scale;
};
extern int nml_servo_set_scale(int axis, NML_SERVO_SCALE scale);

class NML_SERVO_HALT : public NMLmsg
{
 public:
  NML_SERVO_HALT() : NMLmsg(NML_SERVO_HALT_TYPE, sizeof(NML_SERVO_HALT)) {};
  void update();
};
extern int nml_servo_halt();

/* NML types for TRAJ */

class NML_TRAJ_INIT : public NMLmsg
{
 public:
  NML_TRAJ_INIT() : NMLmsg(NML_TRAJ_INIT_TYPE, sizeof(NML_TRAJ_INIT)) {};
  void update();
};
/* Initializes any parameters for the traj level-- WM data such
   as traverse and feed rates, and feed override. Board-specific stuff
   would go here too. */
extern int nml_traj_init();

class NML_TRAJ_EXIT : public NMLmsg
{
 public:
  NML_TRAJ_EXIT() : NMLmsg(NML_TRAJ_EXIT_TYPE, sizeof(NML_TRAJ_EXIT)) {};
  void update();
};
/* called when traj level is no longer needed. Put any board-specific
   cleanup in here. */
extern int nml_traj_exit();

class NML_TRAJ_SET_CYCLE : public NMLmsg
{
 public:
  NML_TRAJ_SET_CYCLE() : NMLmsg(NML_TRAJ_SET_CYCLE_TYPE, sizeof(NML_TRAJ_SET_CYCLE)) {};
  void update(CMS *cms);
  
  double cycle;
};
/* sets the trajectory interpolation time */
extern int nml_traj_set_cycle(double cycle);

class NML_TRAJ_SET_NEIGHBORHOOD : public NMLmsg
{
 public:
  NML_TRAJ_SET_NEIGHBORHOOD() : NMLmsg(NML_TRAJ_SET_NEIGHBORHOOD_TYPE, sizeof(NML_TRAJ_SET_NEIGHBORHOOD)) {};
  void update(CMS *cms);
  
  double neighborhood;
};
/* sets the delta, in currently active units, within which motions
   will be deemed to be in position (as reported in inpos) */
extern int nml_traj_set_neighborhood(double neighborhood);

class NML_TRAJ_SET_OFFSET : public NMLmsg
{
 public:
  NML_TRAJ_SET_OFFSET() : NMLmsg(NML_TRAJ_SET_OFFSET_TYPE, sizeof(NML_TRAJ_SET_OFFSET)) {};
  void update(CMS *cms);

  NML_ROTPOSE offset;
};
/* set the post-added offset */
extern int nml_traj_set_offset(NML_ROTPOSE &offset);

class NML_TRAJ_SET_TRAVERSE_RATE : public NMLmsg
{
 public:
  NML_TRAJ_SET_TRAVERSE_RATE() : NMLmsg(NML_TRAJ_SET_TRAVERSE_RATE_TYPE, sizeof(NML_TRAJ_SET_TRAVERSE_RATE)) {};
  void update(CMS *cms);

  double rate;
};
/* set the vector speed used for all subsequent traverse moves, as called
   by nml_traj_straight_traverse. It does *not* affect the feed rate at
   which already-queued straight traverses will take place (the override
   will affect any queued motions) */
extern int nml_traj_set_traverse_rate(double rate);

class NML_TRAJ_SET_FEED_RATE : public NMLmsg
{
 public:
  NML_TRAJ_SET_FEED_RATE() : NMLmsg(NML_TRAJ_SET_FEED_RATE_TYPE, sizeof(NML_TRAJ_SET_FEED_RATE)) {};
  void update(CMS *cms);

  double rate;
};
/* set the vector speed used for all subsequent straight and arc
   feeds, as called by nml_traj_straight_feed or nml_traj_arc_feed.  It
   does *not* affect the feed rate at which already-queued straight feeds
   will take place (the override will affect any queued motions)
   */
extern int nml_traj_set_feed_rate(double rate);

class NML_TRAJ_SET_FEED_OVERRIDE : public NMLmsg
{
 public:
  NML_TRAJ_SET_FEED_OVERRIDE() : NMLmsg(NML_TRAJ_SET_FEED_OVERRIDE_TYPE, sizeof(NML_TRAJ_SET_FEED_OVERRIDE)) {};
  void update(CMS *cms);

  double override;
};
/* set the scale factor used to scale all feeds (straight, arc, and traverse)
   immediately, that is, the current move and any already queued or subsequent
   moves vector speed will be scaled by this factor as soon as the function
   is called. */
extern int nml_traj_set_feed_override(double override);

class NML_TRAJ_STRAIGHT_TRAVERSE : public NMLmsg
{
 public:
  NML_TRAJ_STRAIGHT_TRAVERSE() : NMLmsg(NML_TRAJ_STRAIGHT_TRAVERSE_TYPE, sizeof(NML_TRAJ_STRAIGHT_TRAVERSE)) {};
  void update(CMS *cms);

  NML_ROTPOSE pos;
};
/* move in a straight line at the feed rate set by the last call to
   nml_traj_set_traverse_rate */
extern int nml_traj_straight_traverse(NML_ROTPOSE &pos);

class NML_TRAJ_STRAIGHT_FEED : public NMLmsg
{
 public:
  NML_TRAJ_STRAIGHT_FEED() : NMLmsg(NML_TRAJ_STRAIGHT_FEED_TYPE, sizeof(NML_TRAJ_STRAIGHT_FEED)) {};
  void update(CMS *cms);

  NML_ROTPOSE pos;
};
/* move in a straight line at the feed rate set by the last call to
   nml_traj_set_feed_rate */
extern int nml_traj_straight_feed(NML_ROTPOSE &pos);

class NML_TRAJ_ARC_FEED : public NMLmsg
{
 public:
  NML_TRAJ_ARC_FEED() : NMLmsg(NML_TRAJ_ARC_FEED_TYPE, sizeof(NML_TRAJ_ARC_FEED)) {};
  void update(CMS *cms);

  double first_axis;
  double second_axis;
  double rotation;
  double axis_end_point;
  CANON_PLANE plane;
};
/* move in an arc at the feed rate set by the last call to
   nml_traj_set_feed_rate */
extern int nml_traj_arc_feed(double f, double s, double r, double aep,
			     CANON_PLANE plane);

class NML_TRAJ_PAUSE : public NMLmsg
{
 public:
  NML_TRAJ_PAUSE() : NMLmsg(NML_TRAJ_PAUSE_TYPE, sizeof(NML_TRAJ_PAUSE)) {};
  void update();
};
/* pause the current motion, until the next call to nml_traj_resume. This
   will affect the current motion, or any subsequent motions if none is
   running now but some are called for later. */
extern int nml_traj_pause();

class NML_TRAJ_RESUME : public NMLmsg
{
 public:
  NML_TRAJ_RESUME() : NMLmsg(NML_TRAJ_RESUME_TYPE, sizeof(NML_TRAJ_RESUME)) {};
  void update();
};
/* resume the current motion that was paused due to nml_traj_pause, and
   don't pause any subsequent motions if none is paused right now. */
extern int nml_traj_resume();

class NML_TRAJ_SET_ORIGIN : public NMLmsg
{
 public:
  NML_TRAJ_SET_ORIGIN() : NMLmsg(NML_TRAJ_SET_ORIGIN_TYPE, sizeof(NML_TRAJ_SET_ORIGIN)) {};
  void update(CMS *cms);

  NML_ROTPOSE origin;
};
extern int nml_traj_set_origin(NML_ROTPOSE &origin);

class NML_TRAJ_USE_ORIGIN : public NMLmsg
{
 public:
  NML_TRAJ_USE_ORIGIN() : NMLmsg(NML_TRAJ_USE_ORIGIN_TYPE, sizeof(NML_TRAJ_USE_ORIGIN)) {};
  void update(CMS *cms);

  CANON_ORIGIN origin;
};
extern int nml_traj_use_origin(CANON_ORIGIN origin);

class NML_TRAJ_USE_TOOL_LENGTH_OFFSET : public NMLmsg
{
 public:
  NML_TRAJ_USE_TOOL_LENGTH_OFFSET() :
    NMLmsg(NML_TRAJ_USE_TOOL_LENGTH_OFFSET_TYPE,
	   sizeof(NML_TRAJ_USE_TOOL_LENGTH_OFFSET)) {};
  void update(CMS *cms);

  double length;
};
extern int nml_traj_use_tool_length_offset(double length);

class NML_TRAJ_START_SEQUENCE : public NMLmsg
{
 public:
  NML_TRAJ_START_SEQUENCE() :
    NMLmsg(NML_TRAJ_START_SEQUENCE_TYPE,
	   sizeof(NML_TRAJ_START_SEQUENCE)) {};
  void update();
};
/* call this just prior to the beginning of the first move in any 
   queued move sequence */
extern int nml_traj_start_sequence();

class NML_TRAJ_STOP_SEQUENCE : public NMLmsg
{
 public:
  NML_TRAJ_STOP_SEQUENCE() :
    NMLmsg(NML_TRAJ_STOP_SEQUENCE_TYPE,
	   sizeof(NML_TRAJ_STOP_SEQUENCE)) {};
  void update();
};
/* call this just after the last move in any queued move sequence, to 
   signify that motion will stop after the last move. */
extern int nml_traj_stop_sequence();

class NML_TRAJ_DWELL : public NMLmsg
{
 public:
  NML_TRAJ_DWELL() :
    NMLmsg(NML_TRAJ_DWELL_TYPE,
	   sizeof(NML_TRAJ_DWELL)) {};
  void update(CMS *cms);

  double time;			/* in seconds */
};
/* wait at zero speed for the specified time, in seconds. This takes
   effect after the last queued motion, not immediately. */
extern int nml_traj_dwell(double time);

class NML_TRAJ_SLAVE : public NMLmsg
{
 public:
  NML_TRAJ_SLAVE() : NMLmsg(NML_TRAJ_SLAVE_TYPE, sizeof(NML_TRAJ_SLAVE)) {};
  void update(CMS *cms);

  int master;			/* master axis number, 1..NML_JOINT_MAX */
  int slave;			/* slave axis number */
  double ratio;			/* slave units/master units */
};
/* slave/unslave axes-- if master is 0, then unslave slave axis;
   else use master axis as source for driving slave axis, at
   ratio master units/slave units */
extern int nml_traj_slave(int master, int slave, double ratio);

class NML_TRAJ_HALT : public NMLmsg
{
 public:
  NML_TRAJ_HALT() : NMLmsg(NML_TRAJ_HALT_TYPE, sizeof(NML_TRAJ_HALT)) {};
  void update();
};
extern int nml_traj_halt();

class NML_TRAJ_DISABLE_FEED_OVERRIDE : public NMLmsg
{
 public:
  NML_TRAJ_DISABLE_FEED_OVERRIDE() : NMLmsg(NML_TRAJ_DISABLE_FEED_OVERRIDE_TYPE, sizeof(NML_TRAJ_DISABLE_FEED_OVERRIDE)) {};
  void update(CMS *cms);

  int disabled;
};
/* called when traj level is no longer needed. Put any board-specific
   cleanup in here. */
extern int nml_traj_disable_feed_override(int disabled);

/* NML messages to IO */

class NML_IO_INIT : public NMLmsg
{
 public:
  NML_IO_INIT() : NMLmsg(NML_IO_INIT_TYPE, sizeof(NML_IO_INIT)) {};
  void update();
};
extern int nml_io_init();

class NML_IO_EXIT : public NMLmsg
{
 public:
  NML_IO_EXIT() : NMLmsg(NML_IO_EXIT_TYPE, sizeof(NML_IO_EXIT)) {};
  void update();
};
extern int nml_io_exit();

class NML_IO_SPINDLE_ON : public NMLmsg
{
 public:
  NML_IO_SPINDLE_ON() : NMLmsg(NML_IO_SPINDLE_ON_TYPE, sizeof(NML_IO_SPINDLE_ON)) {};
  void update(CMS *cms);

  double speed;
};
extern int nml_io_spindle_on(double speed);

class NML_IO_SPINDLE_ROTATE : public NMLmsg
{
 public:
  NML_IO_SPINDLE_ROTATE() : NMLmsg(NML_IO_SPINDLE_ROTATE_TYPE, sizeof(NML_IO_SPINDLE_ROTATE)) {};
  void update(CMS *cms);

  double incr;			/* where, in degrees, from here = 0 */
  double speed;			/* how fast, in rpms */
};
extern int nml_io_spindle_rotate(double incr, double speed);

class NML_IO_SPINDLE_OFF : public NMLmsg
{
 public:
  NML_IO_SPINDLE_OFF() : NMLmsg(NML_IO_SPINDLE_OFF_TYPE, sizeof(NML_IO_SPINDLE_OFF)) {};
  void update();
};
extern int nml_io_spindle_off();

class NML_IO_SPINDLE_RETRACT : public NMLmsg
{
 public:
  NML_IO_SPINDLE_RETRACT() : NMLmsg(NML_IO_SPINDLE_RETRACT_TYPE, sizeof(NML_IO_SPINDLE_RETRACT)) {};
  void update();
};
extern int nml_io_spindle_retract();

class NML_IO_MIST_ON : public NMLmsg
{
 public:
  NML_IO_MIST_ON() : NMLmsg(NML_IO_MIST_ON_TYPE, sizeof(NML_IO_MIST_ON)) {};
  void update();
};
extern int nml_io_mist_on();

class NML_IO_MIST_OFF : public NMLmsg
{
 public:
  NML_IO_MIST_OFF() : NMLmsg(NML_IO_MIST_OFF_TYPE, sizeof(NML_IO_MIST_OFF)) {};
  void update();
};
extern int nml_io_mist_off();

class NML_IO_FLOOD_ON : public NMLmsg
{
 public:
  NML_IO_FLOOD_ON() : NMLmsg(NML_IO_FLOOD_ON_TYPE, sizeof(NML_IO_FLOOD_ON)) {};
  void update();
};
extern int nml_io_flood_on();

class NML_IO_FLOOD_OFF : public NMLmsg
{
 public:
  NML_IO_FLOOD_OFF() : NMLmsg(NML_IO_FLOOD_OFF_TYPE, sizeof(NML_IO_FLOOD_OFF)) {};
  void update();
};
extern int nml_io_flood_off();

class NML_IO_SET_SPEED_OVERRIDE : public NMLmsg
{
 public:
  NML_IO_SET_SPEED_OVERRIDE() : NMLmsg(NML_IO_SET_SPEED_OVERRIDE_TYPE, sizeof(NML_IO_SET_SPEED_OVERRIDE)) {};
  void update(CMS *cms);

  double override;
};
extern int nml_io_set_speed_override(double override);

class NML_IO_READ_TOOL_TABLE : public NMLmsg
{
 public:
  NML_IO_READ_TOOL_TABLE() : NMLmsg(NML_IO_READ_TOOL_TABLE_TYPE, sizeof(NML_IO_READ_TOOL_TABLE)) {};
  void update(CMS *cms);

  char file[NML_TOOL_FILENAME_LEN];
};
extern int nml_io_read_tool_table(char *file);

class NML_IO_PREP_FOR_TOOL : public NMLmsg
{
 public:
  NML_IO_PREP_FOR_TOOL() : NMLmsg(NML_IO_PREP_FOR_TOOL_TYPE,
				  sizeof(NML_IO_PREP_FOR_TOOL)) {};
  void update(CMS *cms);

  int tool;
};
extern int nml_io_prep_for_tool(int tool);

class NML_IO_LOAD_TOOL : public NMLmsg
{
 public:
  NML_IO_LOAD_TOOL() : NMLmsg(NML_IO_LOAD_TOOL_TYPE,
			      sizeof(NML_IO_LOAD_TOOL)) {};
  void update();
};
extern int nml_io_load_tool();

class NML_IO_UNLOAD_TOOL : public NMLmsg
{
 public:
  NML_IO_UNLOAD_TOOL() : NMLmsg(NML_IO_UNLOAD_TOOL_TYPE,
				sizeof(NML_IO_UNLOAD_TOOL)) {};
  void update();
};
extern int nml_io_unload_tool();

class NML_IO_SHUTTLE_PALLETS : public NMLmsg
{
 public:
  NML_IO_SHUTTLE_PALLETS() : NMLmsg(NML_IO_SHUTTLE_PALLETS_TYPE,
				sizeof(NML_IO_SHUTTLE_PALLETS)) {};
  void update();
};
extern int nml_io_shuttle_pallets();

class NML_IO_EXECUTE_CYCLE : public NMLmsg
{
 public:
  NML_IO_EXECUTE_CYCLE() : NMLmsg(NML_IO_EXECUTE_CYCLE_TYPE,
				sizeof(NML_IO_EXECUTE_CYCLE)) {};
  void update();
};
extern int nml_io_execute_cycle();

#define NML_IO_PENDANT_MODE_ENABLE 1
#define NML_IO_PENDANT_MODE_DISABLE 2
class NML_IO_PENDANT_MODE : public NMLmsg
{
 public:
  NML_IO_PENDANT_MODE() : NMLmsg(NML_IO_PENDANT_MODE_TYPE,
				sizeof(NML_IO_PENDANT_MODE)) {};
  void update(CMS *cms);

  int mode;
};
extern int nml_io_pendant_mode(int mode);

#define NML_IO_COOLANT_MODE_ON 1
#define NML_IO_COOLANT_MODE_OFF 2
#define NML_IO_COOLANT_MODE_AUTO 3
class NML_IO_COOLANT_MODE : public NMLmsg
{
 public:
  NML_IO_COOLANT_MODE() : NMLmsg(NML_IO_COOLANT_MODE_TYPE,
				sizeof(NML_IO_COOLANT_MODE)) {};
  void update(CMS *cms);

  int mode;
};
extern int nml_io_coolant_mode(int mode);

class NML_IO_SET_POCKET_NUMBER : public NMLmsg
{
 public:
  NML_IO_SET_POCKET_NUMBER() : NMLmsg(NML_IO_SET_POCKET_NUMBER_TYPE,
				sizeof(NML_IO_SET_POCKET_NUMBER)) {};
  void update(CMS *cms);

  int pocket;
};
extern int nml_io_set_pocket_number(int pocket);

class NML_IO_SET_TOOL_NUMBER : public NMLmsg
{
 public:
  NML_IO_SET_TOOL_NUMBER() : NMLmsg(NML_IO_SET_TOOL_NUMBER_TYPE,
				sizeof(NML_IO_SET_TOOL_NUMBER)) {};
  void update(CMS *cms);

  int tool;
};
extern int nml_io_set_tool_number(int tool);

class NML_IO_SET_PALLET_NUMBER : public NMLmsg
{
 public:
  NML_IO_SET_PALLET_NUMBER() : NMLmsg(NML_IO_SET_PALLET_NUMBER_TYPE,
				sizeof(NML_IO_SET_PALLET_NUMBER)) {};
  void update(CMS *cms);

  int pallet;
};
extern int nml_io_set_pallet_number(int pallet);

#define NML_IO_SHUTTLE_MODE_ENABLE 1
#define NML_IO_SHUTTLE_MODE_DISABLE 2
class NML_IO_SHUTTLE_MODE : public NMLmsg
{
 public:
  NML_IO_SHUTTLE_MODE() : NMLmsg(NML_IO_SHUTTLE_MODE_TYPE,
				sizeof(NML_IO_SHUTTLE_MODE)) {};
  void update(CMS *cms);

  int mode;
};
extern int nml_io_shuttle_mode(int mode);

#define NML_IO_PALLET_MODE_AUTO 1
#define NML_IO_PALLET_MODE_UNCLAMP 2
class NML_IO_PALLET_MODE : public NMLmsg
{
 public:
  NML_IO_PALLET_MODE() : NMLmsg(NML_IO_PALLET_MODE_TYPE,
				sizeof(NML_IO_PALLET_MODE)) {};
  void update(CMS *cms);
  
  int mode;
};
extern int nml_io_pallet_mode(int mode);

class NML_IO_MODE : public NMLmsg
{
 public:
  NML_IO_MODE() : NMLmsg(NML_IO_MODE_TYPE, sizeof(NML_IO_MODE)) {};
  void update(CMS *cms);

  int mode;
};
extern int nml_io_mode(int mode);

class NML_IO_STATE : public NMLmsg
{
 public:
  NML_IO_STATE() : NMLmsg(NML_IO_STATE_TYPE, sizeof(NML_IO_STATE)) {};
  void update(CMS *cms);

  int state;
};
extern int nml_io_state(int state);

class NML_IO_CYCLE_START : public NMLmsg
{
 public:
  NML_IO_CYCLE_START() : NMLmsg(NML_IO_CYCLE_START_TYPE, sizeof(NML_IO_CYCLE_START)) {};
  void update();
};
extern int nml_io_cycle_start();

class NML_IO_CYCLE_STOP : public NMLmsg
{
 public:
  NML_IO_CYCLE_STOP() : NMLmsg(NML_IO_CYCLE_STOP_TYPE, sizeof(NML_IO_CYCLE_STOP)) {};
  void update();
};
extern int nml_io_cycle_stop();

class NML_IO_PREP_FOR_GEAR_CHANGE : public NMLmsg
{
 public:
  NML_IO_PREP_FOR_GEAR_CHANGE() : NMLmsg(NML_IO_PREP_FOR_GEAR_CHANGE_TYPE, sizeof(NML_IO_PREP_FOR_GEAR_CHANGE)) {};
  void update();
};
extern int nml_io_prep_for_gear_change();

class NML_IO_CHANGE_GEARS : public NMLmsg
{
 public:
  NML_IO_CHANGE_GEARS() : NMLmsg(NML_IO_CHANGE_GEARS_TYPE, sizeof(NML_IO_CHANGE_GEARS)) {};
  void update(CMS *cms);

  int gear;
};
extern int nml_io_change_gears(int gear);

class NML_IO_ORIENT_SPINDLE : public NMLmsg
{
 public:
  NML_IO_ORIENT_SPINDLE() : NMLmsg(NML_IO_ORIENT_SPINDLE_TYPE, sizeof(NML_IO_ORIENT_SPINDLE)) {};
  void update(CMS *cms);

  double orientation;		/* degrees */
  int direction;		/* one of the CANON_DIRECTIONS */
};
extern int nml_io_orient_spindle(double orientation, int direction);

class NML_IO_HALT : public NMLmsg
{
 public:
  NML_IO_HALT() : NMLmsg(NML_IO_HALT_TYPE, sizeof(NML_IO_HALT)) {};
  void update();
};
extern int nml_io_halt();

class NML_IO_USE_SPINDLE_FORCE : public NMLmsg
{
 public:
  NML_IO_USE_SPINDLE_FORCE() : NMLmsg(NML_IO_USE_SPINDLE_FORCE_TYPE, sizeof(NML_IO_USE_SPINDLE_FORCE)) {};
  void update(CMS *cms);

  double force;			/* units=newtons */
  double direction;		/* orientation from 0, units=degrees */
};
extern int nml_io_use_spindle_force(double force, double direction);

class NML_IO_USE_NO_SPINDLE_FORCE : public NMLmsg
{
 public:
  NML_IO_USE_NO_SPINDLE_FORCE() : NMLmsg(NML_IO_USE_NO_SPINDLE_FORCE_TYPE, sizeof(NML_IO_USE_NO_SPINDLE_FORCE)) {};
  void update();
};
extern int nml_io_use_no_spindle_force();

class NML_IO_ARC_SPINDLE_FORCE : public NMLmsg
{
 public:
  NML_IO_ARC_SPINDLE_FORCE() : NMLmsg(NML_IO_ARC_SPINDLE_FORCE_TYPE, sizeof(NML_IO_ARC_SPINDLE_FORCE)) {};
  void update(CMS *cms);

  double force;			/* units=newtons */
  double start;			/* beginning orientation, units=degrees */
  double end;			/* ending orientation, units=degrees */
  double rate;			/* degrees/sec, + = CW, - = CCW */
};
extern int nml_io_arc_spindle_force(double force, double direction);

class NML_IO_DISABLE_SPEED_OVERRIDE : public NMLmsg
{
 public:
  NML_IO_DISABLE_SPEED_OVERRIDE() : NMLmsg(NML_IO_DISABLE_SPEED_OVERRIDE_TYPE, sizeof(NML_IO_DISABLE_SPEED_OVERRIDE)) {};
  void update(CMS *cms);

  int disabled;
};
/* called when traj level is no longer needed. Put any board-specific
   cleanup in here. */
extern int nml_io_disable_speed_override(int disabled);

/* NML types for INTERP */

class NML_INTERP_INIT : public NMLmsg
{
 public:
  NML_INTERP_INIT() : NMLmsg(NML_INTERP_INIT_TYPE, sizeof(NML_INTERP_INIT)) {};
  void update();
};
extern int nml_interp_init();

class NML_INTERP_EXIT : public NMLmsg
{
 public:
  NML_INTERP_EXIT() : NMLmsg(NML_INTERP_EXIT_TYPE, sizeof(NML_INTERP_EXIT)) {};
  void update();
};
extern int nml_interp_exit();

class NML_INTERP_OPEN : public NMLmsg
{
 public:
  NML_INTERP_OPEN() : NMLmsg(NML_INTERP_OPEN_TYPE, sizeof(NML_INTERP_OPEN)) {};
  void update(CMS *cms);

  char file[NML_INTERP_FILE_LEN];
};
extern int nml_interp_open(char *file);

class NML_INTERP_READ : public NMLmsg
{
 public:
  NML_INTERP_READ() : NMLmsg(NML_INTERP_READ_TYPE, sizeof(NML_INTERP_READ)) {};
  void update();
};
extern int nml_interp_read();

class NML_INTERP_PAUSE : public NMLmsg
{
 public:
  NML_INTERP_PAUSE() : NMLmsg(NML_INTERP_PAUSE_TYPE, sizeof(NML_INTERP_PAUSE)) {};
  void update();
};
extern int nml_interp_pause();

class NML_INTERP_RESUME : public NMLmsg
{
 public:
  NML_INTERP_RESUME() : NMLmsg(NML_INTERP_RESUME_TYPE, sizeof(NML_INTERP_RESUME)) {};
  void update();
};
extern int nml_interp_resume();

class NML_INTERP_EXECUTE : public NMLmsg
{
 public:
  NML_INTERP_EXECUTE() : NMLmsg(NML_INTERP_EXECUTE_TYPE, sizeof(NML_INTERP_EXECUTE)) {};
  void update(CMS *cms);

  char command[NML_INTERP_COMMAND_LEN];
};
extern int nml_interp_execute(char *command);

class NML_INTERP_RUN : public NMLmsg
{
 public:
  NML_INTERP_RUN() : NMLmsg(NML_INTERP_RUN_TYPE, sizeof(NML_INTERP_RUN)) {};
  void update(CMS *cms);

  char file[NML_INTERP_FILE_LEN];
};
extern int nml_interp_run(char *command);

class NML_INTERP_HALT : public NMLmsg
{
 public:
  NML_INTERP_HALT() : NMLmsg(NML_INTERP_HALT_TYPE, sizeof(NML_INTERP_HALT)) {};
  void update();
};
extern int nml_interp_halt();

class NML_INTERP_CLOSE : public NMLmsg
{
 public:
  NML_INTERP_CLOSE() : NMLmsg(NML_INTERP_CLOSE_TYPE, sizeof(NML_INTERP_CLOSE)) {};
  void update();
};
extern int nml_interp_close();

class NML_INTERP_SINGLE_BLOCK : public NMLmsg
{
 public:
  NML_INTERP_SINGLE_BLOCK() : NMLmsg(NML_INTERP_SINGLE_BLOCK_TYPE, sizeof(NML_INTERP_SINGLE_BLOCK)) {};
  void update(CMS *cms);

  int on;
};
extern int nml_interp_single_block(int on);

class NML_INTERP_BLOCK_DELETE : public NMLmsg
{
 public:
  NML_INTERP_BLOCK_DELETE() : NMLmsg(NML_INTERP_BLOCK_DELETE_TYPE, sizeof(NML_INTERP_BLOCK_DELETE)) {};
  void update(CMS *cms);

  int on;
};
extern int nml_interp_block_delete(int on);

class NML_INTERP_STEP : public NMLmsg
{
 public:
  NML_INTERP_STEP() : NMLmsg(NML_INTERP_STEP_TYPE, sizeof(NML_INTERP_STEP)) {};
  void update();
};
extern int nml_interp_step();

class NML_INTERP_OPTIONAL_STOP : public NMLmsg
{
 public:
  NML_INTERP_OPTIONAL_STOP() : NMLmsg(NML_INTERP_OPTIONAL_STOP_TYPE, sizeof(NML_INTERP_OPTIONAL_STOP)) {};
  void update(CMS *cms);

  int on;			/* non-zero means turn on, else off */
};
extern int nml_interp_optional_stop(int on);
class NML_INTERP_EXEC_OPTIONAL_STOP : public NMLmsg
{
 public:
  NML_INTERP_EXEC_OPTIONAL_STOP() : NMLmsg(NML_INTERP_EXEC_OPTIONAL_STOP_TYPE, sizeof(NML_INTERP_EXEC_OPTIONAL_STOP)) {};
  void update();
};
extern int nml_interp_exec_optional_stop();

/* NML types for WMSA */

class NML_WMSA_INIT : public NMLmsg
{
 public:
  NML_WMSA_INIT() : NMLmsg(NML_WMSA_INIT_TYPE, sizeof(NML_WMSA_INIT)) {};
  void update();
};
extern int nml_wmsa_init();

class NML_WMSA_EXIT : public NMLmsg
{
 public:
  NML_WMSA_EXIT() : NMLmsg(NML_WMSA_EXIT_TYPE, sizeof(NML_WMSA_EXIT)) {};
  void update();
};
extern int nml_wmsa_exit();

class NML_WMSA_MODE : public NMLmsg
{
 public:
  NML_WMSA_MODE() : NMLmsg(NML_WMSA_MODE_TYPE, sizeof(NML_WMSA_MODE)) {};
  void update(CMS *cms);

  int mode;
};
extern int nml_wmsa_mode(int mode);

class NML_WMSA_STATE : public NMLmsg
{
 public:
  NML_WMSA_STATE() : NMLmsg(NML_WMSA_STATE_TYPE, sizeof(NML_WMSA_STATE)) {};
  void update(CMS *cms);

  int state;
};
extern int nml_wmsa_state(int state);

class NML_WMSA_HALT : public NMLmsg
{
 public:
  NML_WMSA_HALT() : NMLmsg(NML_WMSA_HALT_TYPE, sizeof(NML_WMSA_HALT)) {};
  void update();
};
extern int nml_wmsa_halt();

#define NML_PARAMETER_LEN 80	/* length of chars in parameter */
class NML_SET_PARAMETER : public NMLmsg
{
 public:
  NML_SET_PARAMETER() : NMLmsg(NML_SET_PARAMETER_TYPE, sizeof(NML_SET_PARAMETER)) {};
  void update(CMS *cms);

  NML_PARAMETER type;
  char parameter[NML_PARAMETER_LEN];
};
extern int nml_set_parameter(NML_PARAMETER type, char *parameter);
/* extension stub */
extern int x_nml_set_parameter(NML_PARAMETER type, char *parameter);

/* Diagnostics */

#define NML_GENERIC_MESSAGE_LEN 80 /* chars in the generic arg */
class NML_GENERIC : public NMLmsg
{
 public:
  NML_GENERIC() : NMLmsg(NML_GENERIC_TYPE, sizeof(NML_GENERIC)) {};
  void update(CMS *cms);

  char message[NML_GENERIC_MESSAGE_LEN];
};
extern int nml_generic(char *message);

#define MAX_EMC_NML_COMMAND_SIZE 1000

/* INTERP LIST */
struct NML_INTERP_LIST_NODE {
  int line_number;
  char command[MAX_EMC_NML_COMMAND_SIZE];
};

class NML_INTERP_LIST
{
 public:
  NML_INTERP_LIST();
  ~NML_INTERP_LIST();
  
  int append(NMLmsg &);
  int append(NMLmsg *);
  NMLmsg *get();
  NMLmsg *get_last();
  void clear();
  void print();
  int len();
  int get_line_number();

 private:
  RCS_LINKED_LIST *linked_list_ptr;
  int line_number;
  NML_INTERP_LIST_NODE temp_node;
};

/* 
  PARAMETERS 
  
  Parameters are C++ constants mirroring the .ini file settings, 
  used with nml_set_parameter(). The numbers need only
  be unique among these parameters in this file. They need not match
  up with any numbers in the .ini file, although keeping the same
  names is a convention for readability.
*/

#define DEFAULT_FEED_RATE		101
#define MAX_FEED_RATE			102
#define DEFAULT_TRAVERSE_RATE		103
#define MAX_TRAVERSE_RATE		104
#define X_HOME_OFFSET			105
#define Y_HOME_OFFSET			106
#define Z_HOME_OFFSET			107
#define B_HOME_OFFSET			108

#endif				/* #ifndef NML_EMC_HH */

