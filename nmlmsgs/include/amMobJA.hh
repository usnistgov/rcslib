/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

/*!
  \file amMobJA.hh

  \brief NML declarations for autonomous mobility (AM) level mission package
  that generates sequences of point-to-point moves for the mission package,
  where points are the ending angular position of mission package.

  Synopsis:
  #include <rcs.hh>
  #include <posemath.h>
  #include "moastNmlOffsets.hh"
*/

#ifndef AM_MOB_JA_HH
#define AM_MOB_JA_HH

#include "moastTypes.hh"	// MAX_WAYPOINTS, MAX_ARC_SEGMENTS
#include "moastNmlOffsets.hh"

#define RAD(x)  (0.017453292519943 * (x))

#define MAX_AM_SEED_NODES 200

#define AM_MOB_JA_CMD_NAME "amMobJACmd"
#define AM_MOB_JA_STAT_NAME "amMobJAStat"
#define AM_MOB_JA_CFG_NAME "amMobJACfg"
#define AM_MOB_JA_SET_NAME "amMobJASet"

#define AM_MOB_JA_CMD_BASE        (AM_MOB_JA_BASE)
#define AM_MOB_JA_STAT_BASE       (AM_MOB_JA_BASE + 100)
#define AM_MOB_JA_CFG_BASE        (AM_MOB_JA_BASE + 200)
#define AM_MOB_JA_SET_BASE        (AM_MOB_JA_BASE + 300)
#define AM_MOB_JA_ERROR_BASE      (AM_MOB_JA_BASE + 400)

#define AM_MOB_JA_CMD_INIT_TYPE          (AM_MOB_JA_CMD_BASE + 1)
#define AM_MOB_JA_CMD_ABORT_TYPE         (AM_MOB_JA_CMD_BASE + 2)
#define AM_MOB_JA_CMD_HALT_TYPE          (AM_MOB_JA_CMD_BASE + 3)
#define AM_MOB_JA_CMD_SHUTDOWN_TYPE      (AM_MOB_JA_CMD_BASE + 4)
#define AM_MOB_JA_CMD_MOVE_WAYPOINT_TYPE (AM_MOB_JA_CMD_BASE + 5)
#define AM_MOB_JA_CMD_PREPARE_MOVE_TYPE  (AM_MOB_JA_CMD_BASE + 6)
#define AM_MOB_JA_CMD_SPIN_TYPE          (AM_MOB_JA_CMD_BASE + 7)
#define AM_MOB_JA_CMD_STOP_TYPE          (AM_MOB_JA_CMD_BASE + 8)
#define AM_MOB_JA_CMD_NOP_TYPE           (AM_MOB_JA_CMD_BASE + 9)

#define AM_MOB_JA_STAT_TYPE          (AM_MOB_JA_STAT_BASE + 1)

#define AM_MOB_JA_CFG_CELL_RESOLUTION_TYPE      (AM_MOB_JA_CFG_BASE + 1)
#define AM_MOB_JA_CFG_CYCLE_TIME_TYPE           (AM_MOB_JA_CFG_BASE + 2)
#define AM_MOB_JA_CFG_DEBUG_TYPE                (AM_MOB_JA_CFG_BASE + 3)
#define AM_MOB_JA_CFG_DUMP_WM_TYPE              (AM_MOB_JA_CFG_BASE + 4)
#define AM_MOB_JA_CFG_NOP_TYPE                  (AM_MOB_JA_CFG_BASE + 5)
#define AM_MOB_JA_CFG_PLAN_HORIZON_TYPE         (AM_MOB_JA_CFG_BASE + 6)
#define AM_MOB_JA_CFG_VEHICLE_MIN_TURN_RAD_TYPE (AM_MOB_JA_CFG_BASE + 7)
#define AM_MOB_JA_CFG_VEHICLE_WIDTH_TYPE        (AM_MOB_JA_CFG_BASE + 8)

#define AM_MOB_JA_SET_TYPE        (AM_MOB_JA_SET_BASE + 1)

#define AM_MOB_JA_ERROR_MOVE         (AM_MOB_JA_ERROR_BASE + 1)
#define AM_MOB_JA_ERROR_NONE         (AM_MOB_JA_ERROR_BASE + 2)
#define AM_MOB_JA_ERROR_NOT_INIT     (AM_MOB_JA_ERROR_BASE + 3)
#define AM_MOB_JA_ERROR_PLAN_HORIZON (AM_MOB_JA_ERROR_BASE + 4)
#define AM_MOB_JA_ERROR_PLAN_PRIM    (AM_MOB_JA_ERROR_BASE + 5)
#define AM_MOB_JA_ERROR_POSITION     (AM_MOB_JA_ERROR_BASE + 6)
#define AM_MOB_JA_ERROR_PRIM         (AM_MOB_JA_ERROR_BASE + 7)
#define AM_MOB_JA_ERROR_UNSPECIFIED  (AM_MOB_JA_ERROR_BASE + 8)
#define AM_MOB_JA_ERROR_WAYPOINT     (AM_MOB_JA_ERROR_BASE + 9)

/*! For AM_MOB_JA_ERROR_WAYPOINT:
  0. If there are too many arc segments: seed_length will be set to 0.

  1. If a waypoint is in a bad location (such as on an obstacle): seed_length
  will be set to 1 and seed[0] will contain the offending location.

  2. If there is not at least one arc segment: seed_length will be set to 2.

  3. If  two consecutive waypoints are too close together: seed_length
  will be set to 3, the first too-close point will be in seed[0], the
  second too-close point will be in seed[1], and the minimum distance
  allowed between waypoints will be in seed[2].x.
*/

// AmMobJACmdXXX

/***********************************************************************/

/*! AmMobJACmdInit is a task command class telling the AM level to initialize.

*/

class AmMobJACmdInit:public RCS_CMD_MSG {
public:
  AmMobJACmdInit():RCS_CMD_MSG(AM_MOB_JA_CMD_INIT_TYPE,
			       sizeof(AmMobJACmdInit)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACmdAbort is a task command class telling the AM level to abort.
*/

class AmMobJACmdAbort:public RCS_CMD_MSG {
public:
  AmMobJACmdAbort():RCS_CMD_MSG(AM_MOB_JA_CMD_ABORT_TYPE,
				sizeof(AmMobJACmdAbort)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACmdHalt is a task command class telling the AM level to halt.
*/

class AmMobJACmdHalt:public RCS_CMD_MSG {
public:
  AmMobJACmdHalt():RCS_CMD_MSG(AM_MOB_JA_CMD_HALT_TYPE,
			       sizeof(AmMobJACmdHalt)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACmdShutdown is a task command class telling the AM level to
shut down.
*/

class AmMobJACmdShutdown:public RCS_CMD_MSG {
public:
  AmMobJACmdShutdown():RCS_CMD_MSG(AM_MOB_JA_CMD_SHUTDOWN_TYPE,
				   sizeof(AmMobJACmdShutdown)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACmdMoveWaypoint is a task command class telling the AM level to
move the vehicle through a series of waypoints. The data in the command
is an array of waypoints, up to MAX_WAYPOINTS, in relative coordinates.

AmMobJA must do whatever is necessary to get the prim level to move
appropriately. This might require making a plan(s) or retrieving a
a plan(s).

In normal operation, it is expected that there will usually be only one
waypoint and that the plan will change before the vehicle reaches
the waypoint.

If computeHorizonCosts is true, while prim is busy accomplishing the
move(s), amMobJA must compute additional plans to reach all
the points on the planning horizon. These points are used by the
vehicle level planner to seed its planning graph in the same way
that the points computed by preparePlan are used.

This command is really two commands in one. It is not feasible to
use two separate commands because (a) if preparePlan were called while
moveWaypoint was in progress, it would wipe out amMobJA's knowledge
of the status of the move and (b) if amMobJA were to wait until
moveWaypoint was done and then get a preparePlan command, it would
be impossible to have continuous motion.

Because this is really two commands executing simultaneously, the
status must indicate the progress of both commands. To do
this, the seed_length in the status message should be updated on
every cycle (not only when RCS_DONE is received from prim) to indicate
the number of points for which the cost has been found. RCS_DONE
should be returned only if prim has actually reached the last
waypoint (not normally expected to happen) and amMobJA considers the
planning finished (which it may do without planning to all points on
its horizon).

The speed and neighborhood data members are used only for planning to
the periphery of the planning horizon.  Each waypoint has its own
speed and neighborhood which is to be used in planning and execution
for that waypoint.

*/

class AmMobJACmdMoveWaypoint:public RCS_CMD_MSG {
public:
  AmMobJACmdMoveWaypoint():RCS_CMD_MSG(AM_MOB_JA_CMD_MOVE_WAYPOINT_TYPE,
				       sizeof(AmMobJACmdMoveWaypoint)) {};
  void update(CMS *);

  bool computeHorizonCosts;
  double neighborhood;
  double speed;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(Waypoint, waypoint, MAX_WAYPOINTS);
  // will now automatically get 'int waypoint_length'
};

/***********************************************************************/

/*! AmMobJACmdPrepareMove is a command class telling the AM level to
    prepare for vehicle movement. The planner will compute the cost of
    moving from the vehicle's current location to an array of locations 
    on the vehicle's planning horizon, using the given neighborhood
    and speed constraints. The vehicle level planner will read these
    costs from the status message and use them for computing the final
    path to follow. A move waypoint command with a single waypoint
    will follow this command.
*/

/* Note that when using this command, the status returned (in the seed
   array) will contain the cost of reaching planned points. The XY location
   of the nth point is at (seed[n].x, seed[n].y) and the cost of that
   point is in seed[n].z. A cost of -1 means that the point is unreachable.
   The value located at seed[seed_length] gives the current location of the
   vehicle and should contain a cost of 0.
*/

class AmMobJACmdPrepareMove:public RCS_CMD_MSG {
public:
  AmMobJACmdPrepareMove():RCS_CMD_MSG(AM_MOB_JA_CMD_PREPARE_MOVE_TYPE,
				      sizeof(AmMobJACmdPrepareMove)) {};
  void update(CMS *);

  double neighborhood;
  double speed;
};

/***********************************************************************/

/*! AmMobJACmdNop is a command class that does nothing.
*/

class AmMobJACmdNop:public RCS_CMD_MSG {
public:
  AmMobJACmdNop():RCS_CMD_MSG(AM_MOB_JA_CMD_NOP_TYPE,
			       sizeof(AmMobJACmdNop)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACmdSpin is a command that causes the vehicle to spin in the
  direction indicated by the "direction", which should have one of the
  three values in the enum below.  The CW suffix means clockwise, the
  CCW suffix means counterclockwise and the SHORTEST suffix means take
  the direction with the shortest rotation. The vehicle should spin in
  the direction given until the given absolute angle is achieved. The
  command will cause the vehicle to stop at +- tolerance of the given
  angle.

  Note that clockwise would result in north-to-east rotation and
  counterclockwise would result in north-to-west rotation, consistent
  with an overhead view of the vehicle. Since the vehicle's Z axis
  points down, the yaw values increase for clockwise rotation and
  decrease with counterclockwise rotation. */

/* enum {PRIM_MOB_JA_CMD_ROTATE_CW, // force clockwise rotation
         PRIM_MOB_JA_CMD_ROTATE_CCW, // force counter-clockwise rotation
         PRIM_MOB_JA_CMD_ROTATE_SHORTEST}; // take shortest rotation
*/

class AmMobJACmdSpin:public RCS_CMD_MSG {
public:
  AmMobJACmdSpin():RCS_CMD_MSG(AM_MOB_JA_CMD_SPIN_TYPE,
			       sizeof(AmMobJACmdSpin)) {};
  void update(CMS *);
  int direction;
  double absAngle;
  double tolerance;
};

/***********************************************************************/

/*! AmMobJACmdStop is a command that causes the vehicle to stop motion.
*/

class AmMobJACmdStop:public RCS_CMD_MSG {
public:
  AmMobJACmdStop():RCS_CMD_MSG(AM_MOB_JA_CMD_STOP_TYPE,
			       sizeof(AmMobJACmdStop)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJAStat is the command status class for the AM controller.
The admin_state data member is inherited from RCS_STAT_MSG_V2.

The amMobJAError data member is for providing specific error
information on the status of a AmMobJACmdMoveWaypoint command.  For
other commands, it should be set to AM_MOB_JA_ERROR_NONE by AM Mob and
ignored by the receiver.

The seed array is a list of planning horizon costs. When requested,
the planner will compute the cost of reaching nodes on the planning
horizon. The node location is stored in the .x and .y fields and the
cost is stored in the .z field.

*/

class AmMobJAStat : public RCS_STAT_MSG_V2 {
public:
  AmMobJAStat() : RCS_STAT_MSG_V2(AM_MOB_JA_STAT_TYPE, sizeof(AmMobJAStat)) {};
  void update(CMS *);

  unsigned int amMobJAError;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN, seed, MAX_AM_SEED_NODES);
  // will now automatically get 'int seed_length'
};

/***********************************************************************/

/*! AmMobJACfgCellResolution is a configuration command class that
sets the AM controller's cell resolution.
*/

class AmMobJACfgCellResolution:public RCS_CMD_MSG {
public:
  AmMobJACfgCellResolution():RCS_CMD_MSG(AM_MOB_JA_CFG_CELL_RESOLUTION_TYPE,
			       sizeof(AmMobJACfgCellResolution)) {};
  void update(CMS *);

  //! resolution in meters
  double cellResolution;
};

/***********************************************************************/

/*! AmMobJACfgCycleTime is a configuration command class that
sets the AM controller's cycle time.
*/

class AmMobJACfgCycleTime:public RCS_CMD_MSG {
public:
  AmMobJACfgCycleTime():RCS_CMD_MSG(AM_MOB_JA_CFG_CYCLE_TIME_TYPE,
			       sizeof(AmMobJACfgCycleTime)) {};
  void update(CMS *);

  //! period in seconds
  double cycleTime;
};

/***********************************************************************/

/*!  AmMobJACfgDebug is a configuration command class that
sets the controller's debug level.
*/

class AmMobJACfgDebug:public RCS_CMD_MSG {
public:
  AmMobJACfgDebug():RCS_CMD_MSG(AM_MOB_JA_CFG_DEBUG_TYPE,
			       sizeof(AmMobJACfgDebug)) {};
  void update(CMS *);

  //! debug level
  int debug;
};

/***********************************************************************/

/*!  AmMobJACfgDumpWM is a configuration command class that
     turns on or off the output of the current world view

*/

class AmMobJACfgDumpWM:public RCS_CMD_MSG {
public:
  AmMobJACfgDumpWM():RCS_CMD_MSG(AM_MOB_JA_CFG_DUMP_WM_TYPE,
				 sizeof(AmMobJACfgDumpWM)) {};
  void update(CMS *);

  //! turn on or off output (true on, false off)
  bool turnOn;
};

/***********************************************************************/

/*! AmMobJACfgNop is a command class that does nothing.
*/

class AmMobJACfgNop:public RCS_CMD_MSG {
public:
  AmMobJACfgNop():RCS_CMD_MSG(AM_MOB_JA_CFG_NOP_TYPE,
			       sizeof(AmMobJACfgNop)) {};
  void update(CMS *);
};

/***********************************************************************/

/*! AmMobJACfgPlanHorizon is a configuration command class that
sets the AM controller's plan horizon.
*/

class AmMobJACfgPlanHorizon:public RCS_CMD_MSG {
public:
  AmMobJACfgPlanHorizon():RCS_CMD_MSG(AM_MOB_JA_CFG_PLAN_HORIZON_TYPE,
			       sizeof(AmMobJACfgPlanHorizon)) {};
  void update(CMS *);

  //! plan horizon in meters
  double planHorizon;
};

/***********************************************************************/

/*!  AmMobJACfgVehicleMinTurnRad is a configuration command class that
sets the controller's vehicle minimum turning radius.

vehicleMinTurnRad is a double (in meters) and should be non-negative.

*/

class AmMobJACfgVehicleMinTurnRad:public RCS_CMD_MSG {
public:
  AmMobJACfgVehicleMinTurnRad():
    RCS_CMD_MSG(AM_MOB_JA_CFG_VEHICLE_MIN_TURN_RAD_TYPE,
		sizeof(AmMobJACfgVehicleMinTurnRad)) {};
  void update(CMS *);

  //! minimum turning radius in meters
  double vehicleMinTurnRad;
};

/***********************************************************************/

/*!  AmMobJACfgVehicleWidth is a configuration command class that
sets the controller's vehicle width.

vehicleWidth is a double (in meters) and should be positive.

*/

class AmMobJACfgVehicleWidth:public RCS_CMD_MSG {
public:
  AmMobJACfgVehicleWidth():RCS_CMD_MSG(AM_MOB_JA_CFG_VEHICLE_WIDTH_TYPE,
				       sizeof(AmMobJACfgVehicleWidth)) {};
  void update(CMS *);

  //! width in meters
  double vehicleWidth;
};

/***********************************************************************/

/*! AmMobJASet is the configuration status class for AM mobility.
    It holds settings that change infrequently and in response
    to configuration commands.*/
/*

The configuration status includes two distinct sorts of data:
(1) data about the AM Mob system
    cellResolution
    cycleTime
    debug
    dumpWM
    planHorizon
(2) data about the vehicle being controlled.
    controlPoint
    vehicleMinTurnRad
    vehicleWidth

    Data about the AM Mob system comes initially from three sources:
(1) arguments to the command that starts the system,
(2) the inifile,
(3) default values.

Data about the vehicle comes initially from three sources:
(1) the inifile,
(2) servo Mob, which gets it from the simulation,
(3) default values.

The control point is actually a transformation which, when applied to
the location point (the point whose location is given by nav data and
is usually somewhere near the center of the vehicle) will give the
location of the point being controlled. This is usually a point on the
vehicle whose motion is easiest to predict given the motion of the
controlled wheels. For a 4-wheel vehicle with all wheels powered and
no wheels that steer, this is usually about the same as the center of
the vehicle. For a vehicle with two wheels that steer (ackermann
steering), this is usually the midpoint of the line between the
centers of the wheels that do not steer. Prim Mob and AM Mob need to
use the same control point.

Commands exist to change all the status data, but AM Mob will refuse
to change the cellResolution or the planHorizon while it is running
(since it is unable to do so).

It is usually not a good idea to change data about the vehicle (if the
data provided by servo or inifile is correct), but for testing purposes
and emergency maneuvers, it may be useful.

*/

class AmMobJASet : public RCS_STAT_MSG {
public:
  AmMobJASet():RCS_STAT_MSG(AM_MOB_JA_SET_TYPE, sizeof(AmMobJASet)) {};
  void update(CMS *);

  double cellResolution;
  PM_POSE controlPoint;	// (inverse of) pose of control pt wrt nav data frame
  double cycleTime;
  unsigned int debug;
  bool dumpWM;
  double planHorizon;
  double vehicleMinTurnRad;
  double vehicleWidth;
};

/***********************************************************************/

extern int amMobJA_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * amMobJA_symbol_lookup(long v);

/***********************************************************************/

#endif // AM_MOB_JA_HH
