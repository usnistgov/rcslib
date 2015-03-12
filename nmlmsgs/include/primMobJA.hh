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
  \file primMobJA.hh

  \brief NML declarations for vehicle prim level command, status, config
  and settings.
*/

#ifndef PRIM_MOB_JA_HH
#define PRIM_MOB_JA_HH

#include <stdio.h>
#include "moastTypes.hh"	// MAX_WAYPOINTS
#include "moastNmlOffsets.hh"

#define PRIM_MOB_JA_CMD_NAME "primMobJACmd"
#define PRIM_MOB_JA_STAT_NAME "primMobJAStat"
#define PRIM_MOB_JA_CFG_NAME "primMobJACfg"
#define PRIM_MOB_JA_SET_NAME "primMobJASet"

#define PRIM_MOB_JA_CMD_BASE        (PRIM_MOB_JA_BASE + 0)
#define PRIM_MOB_JA_STAT_BASE       (PRIM_MOB_JA_BASE + 100)
#define PRIM_MOB_JA_CFG_BASE        (PRIM_MOB_JA_BASE + 200)
#define PRIM_MOB_JA_SET_BASE        (PRIM_MOB_JA_BASE + 300)
#define PRIM_MOB_JA_ERROR_BASE      (PRIM_MOB_JA_BASE + 400)

#define PRIM_MOB_JA_CMD_NOP_TYPE              (PRIM_MOB_JA_CMD_BASE + 1)
#define PRIM_MOB_JA_CMD_INIT_TYPE             (PRIM_MOB_JA_CMD_BASE + 2)
#define PRIM_MOB_JA_CMD_ABORT_TYPE            (PRIM_MOB_JA_CMD_BASE + 3)
#define PRIM_MOB_JA_CMD_HALT_TYPE             (PRIM_MOB_JA_CMD_BASE + 4)
#define PRIM_MOB_JA_CMD_SHUTDOWN_TYPE         (PRIM_MOB_JA_CMD_BASE + 5)
#define PRIM_MOB_JA_CMD_MOVE_ARC_SEGMENT_TYPE (PRIM_MOB_JA_CMD_BASE + 6)
#define PRIM_MOB_JA_CMD_MOVE_WAYPOINT_TYPE    (PRIM_MOB_JA_CMD_BASE + 7)
#define PRIM_MOB_JA_CMD_MOVE_WAYPOSE_TYPE     (PRIM_MOB_JA_CMD_BASE + 8)
#define PRIM_MOB_JA_CMD_ROTATE_TYPE           (PRIM_MOB_JA_CMD_BASE + 9)
#define PRIM_MOB_JA_CMD_VEL_TYPE              (PRIM_MOB_JA_CMD_BASE + 10)
#define PRIM_MOB_JA_CMD_STOP_TYPE             (PRIM_MOB_JA_CMD_BASE + 11)
#define PRIM_MOB_JA_CMD_DOCK_TYPE             (PRIM_MOB_JA_CMD_BASE + 12)

#define PRIM_MOB_JA_STAT_TYPE       (PRIM_MOB_JA_STAT_BASE + 1)

#define PRIM_MOB_JA_CFG_NOP_TYPE             (PRIM_MOB_JA_CFG_BASE + 1)
#define PRIM_MOB_JA_CFG_CYCLE_TIME_TYPE      (PRIM_MOB_JA_CFG_BASE + 2)
#define PRIM_MOB_JA_CFG_MOVE_TYPE            (PRIM_MOB_JA_CFG_BASE + 3)
#define PRIM_MOB_JA_CFG_DEBUG_TYPE           (PRIM_MOB_JA_CFG_BASE + 4)

#define PRIM_MOB_JA_SET_TYPE        (PRIM_MOB_JA_SET_BASE + 1)

enum {PRIM_MOB_JA_ERROR_BAD_CFG = PRIM_MOB_JA_ERROR_BASE + 1};


class PrimMobJACmdNop:public RCS_CMD_MSG {
public:
  PrimMobJACmdNop():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_NOP_TYPE,
     sizeof(PrimMobJACmdNop)) {};
  void update(CMS *);
};

class PrimMobJACmdInit:public RCS_CMD_MSG {
public:
  PrimMobJACmdInit():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_INIT_TYPE,
     sizeof(PrimMobJACmdInit)) {};
  void update(CMS *);
};

class PrimMobJACmdHalt:public RCS_CMD_MSG {
public:
  PrimMobJACmdHalt():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_HALT_TYPE,
     sizeof(PrimMobJACmdHalt)) {};
  void update(CMS *);
};

class PrimMobJACmdAbort:public RCS_CMD_MSG {
public:
  PrimMobJACmdAbort():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_ABORT_TYPE,
     sizeof(PrimMobJACmdAbort)) {};
  void update(CMS *);
};

class PrimMobJACmdShutdown:public RCS_CMD_MSG {
public:
  PrimMobJACmdShutdown():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_SHUTDOWN_TYPE,
     sizeof(PrimMobJACmdShutdown)) {};
  void update(CMS *);
};

/*!
  The PrimMobJACmdMoveArc NML message contains an array of ArcSegments,
  up to MAX_ARC_SEGMENTS, in relative coordinates.
*/

class PrimMobJACmdMoveArcSegment:public RCS_CMD_MSG {
public:
  PrimMobJACmdMoveArcSegment():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_MOVE_ARC_SEGMENT_TYPE,
     sizeof(PrimMobJACmdMoveArcSegment)) {};
  void update(CMS *);

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(ArcSegment, arcList, MAX_ARC_SEGMENTS);
  // will now get 'int arcList_length'
};

/*!
  The PrimMobJACmdMoveWaypoint NML message contains an array of waypoints,
  up to MAX_WAYPOINTS, in relative coordinates.
*/

class PrimMobJACmdMoveWaypoint:public RCS_CMD_MSG {
public:
  PrimMobJACmdMoveWaypoint():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_MOVE_WAYPOINT_TYPE,
     sizeof(PrimMobJACmdMoveWaypoint)) {hasOrientation = 0;};
  void update(CMS *);
  bool prim_path;
  bool hasOrientation;		//!< non-zero means use the 'rpy' element of the waypoints for heading 

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(Waypoint, waypoint, MAX_WAYPOINTS);
  // will now get 'int waypoint_length'
};

class PrimMobJACmdRotate:public RCS_CMD_MSG {
public:
  PrimMobJACmdRotate():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_ROTATE_TYPE,
     sizeof(PrimMobJACmdRotate)) {};
  void update(CMS *);

  double theta;			// yaw angle, relative coords, [rad]
  double thetaTol;		// tolerance, [rad]
  /*!
    \a direction can take a value from \a PRIM_MOB_JA_CMD_ROTATE_CW,
    \a PRIM_MOB_JA_CMD_ROTATE_CCW or \a PRIM_MOB_JA_CMD_ROTATE_SHORTEST,
    where the \a CW suffix means clockwise, the \a CCW suffix means
    counterclockwise and the \a SHORTEST suffix means take the direction
    with the shortest rotation.

    Note that clockwise would result in north-to-east rotation and
    counterclockwise would result in north-to-west rotation, consistent
    with an overhead view of the vehicle. Since the vehicle's Z axis
    points down, the yaw values increase for clockwise rotation and
    decrease with counterclockwise rotation.
  */
  int direction;		
};

class PrimMobJACmdVel:public RCS_CMD_MSG {
public:
  PrimMobJACmdVel():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_VEL_TYPE,
     sizeof(PrimMobJACmdVel)) {};
  void update(CMS *);

  double v;			// trans speed
  double w;			// rot speed
};

class PrimMobJACmdStop:public RCS_CMD_MSG {
public:
  PrimMobJACmdStop():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_STOP_TYPE,
     sizeof(PrimMobJACmdStop)) {};
  void update(CMS *);
};

/*! Command to dock to a station. The command must contain a rectangle of 
free space, a rectangle that represents the docking station (obstacle), and the 
goal point for the dock. Note that the goal point is the location on the station
to dock with, not the location where the center of the vehicle will be when docked.
*/
class PrimMobJACmdDock:public RCS_CMD_MSG {
public:
  PrimMobJACmdDock():RCS_CMD_MSG
    (PRIM_MOB_JA_CMD_DOCK_TYPE,
     sizeof(PrimMobJACmdDock)) {};
  void update(CMS *);

  PM_CARTESIAN freeSpaceUpperRight;
  PM_CARTESIAN freeSpaceLowerLeft;

  PM_CARTESIAN stationUpperRight;
  PM_CARTESIAN stationLowerLeft;

  PM_CARTESIAN dockPoint;
};


/*!
  PrimMobJAStat is the class that holds the dynamic vehicle status,
  things that change frequently and in response to vehicle command
  messages.
 */

class PrimMobJAStat:public RCS_STAT_MSG_V2 {
public:
  PrimMobJAStat() : RCS_STAT_MSG_V2(PRIM_MOB_JA_STAT_TYPE, sizeof(PrimMobJAStat)) {};
  void update(CMS *);

  int command_type_sent;	// the last one sent down to servo
  int serial_number_sent;	// the last one sent down to servo
  int pathIndex;		// which one we're on, 0 = first one
};

class PrimMobJACfgCycleTime:public RCS_CMD_MSG {
public:
  PrimMobJACfgCycleTime():RCS_CMD_MSG
    (PRIM_MOB_JA_CFG_CYCLE_TIME_TYPE,
     sizeof(PrimMobJACfgCycleTime)) {};
  void update(CMS *);

  double cycleTime;
};

class PrimMobJACfgNop:public RCS_CMD_MSG {
public:
  PrimMobJACfgNop():RCS_CMD_MSG
    (PRIM_MOB_JA_CFG_NOP_TYPE,
     sizeof(PrimMobJACfgNop)) {};
  void update(CMS *);
};

class PrimMobJACfgMove:public RCS_CMD_MSG {
public:
  PrimMobJACfgMove():RCS_CMD_MSG
    (PRIM_MOB_JA_CFG_MOVE_TYPE,
     sizeof(PrimMobJACfgMove)) {};
  void update(CMS *);

  double vmax;
  double amax;
  double wmax;
  double alphamax;
  double vCutoffAngle;
  double wCutoffAngle;
};

#define PRIM_MOB_JA_DEBUG_MOVE     0x00000001
#define PRIM_MOB_JA_DEBUG_WAYPOINT 0x00000002
#define PRIM_MOB_JA_DEBUG_ARCS     0x00000004
#define PRIM_MOB_JA_DEBUG_ICR      0x00000008
#define PRIM_MOB_JA_DEBUG_ALL      0xFFFFFFFF

class PrimMobJACfgDebug:public RCS_CMD_MSG {
public:
  PrimMobJACfgDebug():RCS_CMD_MSG
    (PRIM_MOB_JA_CFG_DEBUG_TYPE,
     sizeof(PrimMobJACfgDebug)) {};
  void update(CMS *);

  int debug;			// debug mask
};

/*!
  PrimMobJASet is the class that holds the quasi-static vehicle settings,
  those that change infrequently and in response to vehicle configuration
  messages.
 */

class PrimMobJASet:public RCS_STAT_MSG {
public:
  PrimMobJASet():RCS_STAT_MSG(PRIM_MOB_JA_SET_TYPE, sizeof(PrimMobJASet)) {};
  void update(CMS *);

  double cycleTime;
  int debug;			// debug mask
  double vmax;			// max translational speed, [m/s]
  double amax;			// max translational acceleration, [m/s/s]
  double wmax;			// max rotational speed, [rad/s]
  double alphamax;		// max rotational acceleration, [rad/s/s]
  // value of angular deviation above which speed is set to zero,
  // leading to only rotation
  double vCutoffAngle;		// [rad]
  // value of angular deviation above which angular speed is clamped
  // to wmax
  double wCutoffAngle;		// [rad]
  PM_POSE controlPoint;	// (inverse of) pose of control point wrt nav data frame
};

extern int primMobJA_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * primMobJA_symbol_lookup(long type);

#endif // PRIM_MOB_JA_HH
