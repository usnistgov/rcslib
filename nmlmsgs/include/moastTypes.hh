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
  \file moastTypes.hh

  \brief Declarations for MOAST types and ranges

  \code CVS Status:
  $Author: proctor $
  $Revision: 1.22 $
  $Date: 2011/03/11 14:09:42 $
  \endcode

  \author Fred Proctor
*/

#ifndef MOAST_TYPES_HH
#define MOAST_TYPES_HH

#include <stdio.h> // FILE, fprintf
#include <math.h>
#include <stddef.h>		// sizeof, NULL
#include <string.h>		// strncpy

#include <rcs.hh>		// RCS_STAT_SOURCE_FILE_LEN
#include <posemath.h>		// PM_CARTESIAN

// safe string copy
#define strsafecpy(dst,src) ((dst)[sizeof(dst)-1] = 0) == 0 ? strncpy((dst),(src),sizeof(dst) < sizeof(src) ? sizeof(dst)-1 : sizeof(src)) : NULL

// handle lack of M_PI on some systems, typically found in math.h
#ifndef M_PI
#define M_PI		3.14159265358979323846	/* pi */
#endif
#ifndef M_PI_2
#define M_PI_2		1.57079632679489661923	/* pi/2 */
#endif

//! macros for dealing with RCS status structure elements
#if ! defined(state_match)
#define state_match(s,a) (s)->line = (s)->source_line = __LINE__, (s)->state == (a)
#define state_new(s) strncpy((s)->source_file, __FILE__, RCS_STAT_SOURCE_FILE_LEN), (s)->source_file[RCS_STAT_SOURCE_FILE_LEN-1] = 0
#define state_next(s,a) (s)->state = (a)
#define status_next(s,a) (s)->status = (a)
#define state_default(s) (s)->line = (s)->source_line = __LINE__
#endif // ! defined (state_match)

//! How long an NML buffer name can be
#define MOAST_NML_BUFFER_NAME_LEN 80

//! How long a file name can be
#define MOAST_FILE_NAME_LEN 100

//! How long a generic name can be
#define MOAST_NAME_LEN 80

//! How long a platform name can be
#define MOAST_PLATFORM_NAME_LEN 80

// Structures and classes that are shared among many NML channels
//! steering mechanisms types
typedef enum MOAST_STEER_TYPE {
  MOAST_INVALID_STEER_TYPE=-1,
  MOAST_SKID_STEER_TYPE,  
  MOAST_ACKERMAN_STEER_TYPE,
  MOAST_OMNI_STEER_TYPE,
  MOAST_SUBMARINE_STEER_TYPE,
  MOAST_ROTARY_WING_STEER_TYPE,
  MOAST_FIXED_WING_STEER_TYPE,
  MOAST_UNKNOWN_STEER_TYPE
}moastSteerType;

//! robot types
typedef enum MOAST_ROBOT_TYPE {
  MOAST_INVALID_ROBOT_TYPE=-1,
  MOAST_GROUND_VEHICLE_TYPE,  
  MOAST_NAUTIC_ROBOT_TYPE,
  MOAST_AERIAL_ROBOT_TYPE,
  MOAST_LEGGED_ROBOT_TYPE,
  MOAST_UNKNOWN_ROBOT_TYPE
}moastRobotType;

//! Flipper definitions
typedef enum MOAST_FLIPPER_TYPE {
  MOAST_FR_FLIPPER_TYPE, //front right
  MOAST_FL_FLIPPER_TYPE, //front left
  MOAST_RR_FLIPPER_TYPE, //rear right
  MOAST_RL_FLIPPER_TYPE  //rear left
} moastFlipperType;

////////////////////////////////////////////////////////////////////
/*! A point from a sensor may be represented with SensorPoint
    The default sensorLoc differs from the default dataLoc so that
    normalization using the distance between the two will not fail.
*/

class SensorPoint
{
public:
  // Constructors
  SensorPoint(){
    //    sensorLoc = PM_CARTESIAN(1,1,1);
    //    dataLoc = PM_CARTESIAN(0,0,0);
    sensorLoc.x = 1;
    sensorLoc.y = 1;
    sensorLoc.z = 1;
    
    dataLoc.x = 0;
    dataLoc.y = 0;
    dataLoc.z = 0;
    
    param1 = 0;
  };
  SensorPoint(PM_CARTESIAN sensorLocIn, PM_CARTESIAN dataLocIn){
    sensorLoc = sensorLocIn;
    dataLoc =  dataLocIn;
  };

  double param1;
  PM_CARTESIAN sensorLoc;
  PM_CARTESIAN dataLoc;
};

//! 2-D location structure
class Location2D
{
public:
  Location2D(){};
  //! the north location of the point
  double north;
  //! the east location of the point
  double east;

  Location2D(const Location2D &copyIn) // copy constructor to handle pass by value
  {
    north = copyIn.north;
    east = copyIn.east;
  };

  Location2D& operator=(const Location2D &rhs)
  {
    this->north = rhs.north;
    this->east = rhs.east;
    return *this;
  };

  Location2D& operator=(const PM_CARTESIAN &rhs)
  {
    this->north = rhs.x;
    this->east = rhs.y;
    return *this;
  };

  int operator==(const Location2D &rhsIn) const
  {
    if( this->north!=rhsIn.north || this->east!=rhsIn.east )
      return 0;
    else
      return 1;
  };

  // make a PM_CARTESIAN
  PM_CARTESIAN makePM_CARTESIAN(){
    PM_CARTESIAN retValue;
    retValue.x = north;
    retValue.y = east;
    retValue.z = 0;
    return retValue;
  };

  // sort by (increasing north, decreasing east)
  int operator<(const Location2D &rhs) const
  {
    if( this->north == rhs.north && this->east > rhs.east ) return 1; // make (0,4) come before (0,0)
    if( this->north < rhs.north ) return 1;
    return 0;
  };

  // compute distance to another point
  double dist(const Location2D &pointIn)
  {
    return( sqrt( (this->north-pointIn.north)*(this->north-pointIn.north) +
		  (this->east-pointIn.east)*(this->east-pointIn.east) ) );
  };
};

/*
  An ArcSegment is a path from an implicit start location to an end
  location, along a circular arc. In a 2-D problem, the Z values are
  ignored, and the normal vector is assumed to be +Z. 
*/
typedef struct {
  PM_CARTESIAN center;		// coords of center
  PM_CARTESIAN end;		// coords of end
  PM_CARTESIAN normal;		// normal vector
  // nominal angular displacement about normal from implicit start to end
  double theta;
  // tolerance along path, difference between outer and inner radius
  double annular_tol;
  // speed along the segment; acceleration is a system-wide parameter
  double speed;
  int isArc;			// 1 means arc, 0 means line, use 'end'
} ArcSegment;

//! define conventions for rotations
enum {MOAST_ROTATE_CW, // force clockwise rotation
      MOAST_ROTATE_CCW, // force counter-clockwise rotation
      MOAST_ROTATE_SHORTEST}; // take shortest rotation

//! Constrained interger structure
typedef struct {
  //! integer value that will be constrained
  int data;
  //! type of constraint, must be defined by routine that uses this structure
  int constraint;
  //! tolerance on constraint, must be defined by routine that uses this structure
  double tolerance;
} constrainedInt;

//! Constrained double structure
typedef struct {
  //! float value that will be constrained
  float data;
  //! type of constraint, must be defined by routine that uses this structure
  int constraint;
  //! tolerance on constraint, must be defined by routine that uses this structure
  double tolerance;
} constrainedFloat;

//! Constrained double structure
typedef struct {
  //! double value that will be constrained
  double data;
  //! type of constraint, must be defined by routine that uses this structure
  int constraint;
  //! tolerance on constraint, must be defined by routine that uses this structure
  double tolerance;
} constrainedDouble;

typedef struct {
  PM_CARTESIAN p;
  PM_RPY rpy;
  double neighborhood;
  double speed;
  unsigned char quality;
} Waypoint;

// the number of waypoints AM Mob can accept
#define MAX_WAYPOINTS 100
// the number of arc segments Prim Mob can accept
#define MAX_ARC_SEGMENTS 100
// the maximum number of vehicles that can be simulated in traffic
#define MAX_TRAFFIC_VEHICLES 10

#endif // MOAST_TYPES_HH

/*
  $Log: moastTypes.hh,v $
  Revision 1.22  2011/03/11 14:09:42  proctor
  Added a 'name' field to servoMisJA to be used to set some types of robot
  kinematics explicitly

  Revision 1.21  2010/07/29 21:34:36  dr_steveb
  Added taught points to prim mission. To do this, needed to expand the bigest filename field in moast (for debug) and needed to
  add a real status return to servo mission move commands. Also added a tollerance to servo move commands and a taught position
  flag to prim mission init.

  Revision 1.20  2010/01/20 17:52:43  proctor
  Took out snprintf decl for WIN32 - it's in ulapi now

  Revision 1.19  2010/01/15 18:54:41  proctor
  Added strsafecpy

  Revision 1.18  2009/12/10 20:46:03  proctor
  Took out broken check for RCS_STAT_MSG_V2 in moastTypes.hh, which was
  preventing the admin state symbol lookup from working.

  Revision 1.17  2009/06/05 19:18:28  proctor
  Changed test of RCS stat message string conversions to match new RCS define

  Revision 1.16  2009/06/05 17:33:56  proctor
  Replaced ad hoc rcs_xxx_to_string macros with rcslib's built-in version

  Revision 1.15  2009/05/20 17:59:14  proctor
  Added a PM_RPY element to the Waypoint structure, and a haveOrientation
  element to the PrimCmdMoveWaypoint message, to support waypoints with
  orientation (yaw). Added the code to handle this in primMobEngine.cc.

  Revision 1.14  2009/03/13 12:28:39  proctor
  Added a WIN32 snprintf macro

  Revision 1.13  2008/12/02 19:49:39  proctor
  Fixed lack of M_PI on Windows

  Revision 1.12  2007/12/18 15:28:03  cj_scrapper
  Adding platform name into servo mobility settings

  Revision 1.11  2007/12/13 13:17:26  dr_steveb
  Changes for vehicle level visibility graph planning.

  Revision 1.10  2007/11/07 19:56:30  dr_steveb
  Moved the LineSegment class from moastTypes into moastGeom

  Revision 1.9  2007/08/29 18:26:25  dr_steveb
  Added joystick control commands to the traffic cmd buffer and moved some constants to mostTypes

  Revision 1.8  2007/06/28 12:02:35  dr_steveb
  Changed location2d into a class from a structure

  Revision 1.7  2007/05/10 13:15:31  dr_steveb
  Moved rotation and arcSegment declaration from primMobJA to moastTypes so that multiple apps can use them.

  Revision 1.6  2007/04/25 15:08:57  dr_steveb
  Added traffic simulation files

  Revision 1.5  2006/03/09 15:28:46  proctor
  Took out AdminState stuff, since it's now in RCS

  Revision 1.4  2006/03/08 20:51:55  dr_steveb
  Added missing ';'

  Revision 1.3  2006/03/07 20:35:07  dr_steveb
  Changed administrative state declarations to an enum

  Revision 1.2  2006/03/07 20:29:09  dr_steveb
  Added administrative state definitions to file.

  Revision 1.1.1.1  2005/10/12 20:50:32  root
  initial import

  Revision 1.18  2005/10/03 13:27:48  kramer
  added setting MAX_ARC_SEGMENTS to 100

  Revision 1.17  2005/09/29 13:02:52  stephen
  Added param1 back into sensorPoint

  Revision 1.16  2005/09/29 01:45:01  stephen
  Changed SensorPoint constructor to be NML autogen friendly

  Revision 1.15  2005/09/27 21:32:22  kramer
  changed definition of SensorPoint

  Revision 1.14  2005/08/05 13:34:04  scrapper
  Added MOAST_WORLD_NAME_LEN to moastTypes.  Added worldName to all servo 'set' messages.

  Revision 1.13  2005/08/02 20:40:00  proctor
  Added 'speed' element to Waypoint type

  Revision 1.12  2005/07/26 20:29:52  stephen
  Added new methods for line class.

  Revision 1.11  2005/07/21 18:59:11  stephen
  Fixed merged conflicts and added visibility graph stuff.

  Revision 1.10  2005/07/19 19:04:56  stephen
  Fixed spelling

  Revision 1.9  2005/07/06 22:27:07  proctor
  Included <rcs.hh>, <posemath.h>

  Revision 1.8  2005/06/20 19:38:36  stephen
  Added symbolicData header file for high-level sensor processing.

  Revision 1.7  2005/06/20 17:15:16  proctor
  Added conditional declaration of state_match() and related

  Revision 1.6  2005/06/14 19:18:36  stephen
  Changed location and declaration of SensorPoint.

  Revision 1.5  2005/06/08 19:28:38  proctor
  Moved state_match, etc. macros into here

  Revision 1.4  2005/06/01 13:20:45  proctor
  Moved Waypoint structure into moastTypes.hh

  Revision 1.3  2005/06/01 12:45:44  stephen
  Added commands for vehicle level ied mission.

  Revision 1.2  2005/05/18 12:46:45  stephen
  Added MOAST_NML_FILE_NAME_LEN

  Revision 1.1  2005/05/16 15:45:13  proctor
  Initial checkin

*/
