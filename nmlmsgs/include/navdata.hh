/* navdata.hh */
/*
  The update() and format() commands will be automatically generated
  by the NIST auto-code-writer routine (JAVA-based).  These will be
  found in the file *_n.cc
*/
/*GEN_NMLOFFSETS_SKIP_THIS_FILE*/

#ifndef __INCnavdatahh
#define __INCnavdatahh

#include "rcs.hh"

#define NAV_DATA_TYPE 101
#define NAV_DATA_NAME    "navdata"
#define NAV_DATA_SIZE 1500

// Vehicle state from navigation subsystem
class NAV_DATA  : public NMLmsg
{
public:
  NAV_DATA();
  void update( CMS*);

  int time;                   // time tag [sec] relative to common origin
  PM_CARTESIAN tranRel;       // position [m], Relative coords, smooth
  PM_CARTESIAN tranAbs;       // position [m], Absolute coords, noisy
  int utmZone;                // UTM zone number for Absolute position
  double speed;               // [m/s] speed of vehicle, pos=forward, neg=reverse
  PM_CARTESIAN tranVel;       // velocity [m/s], Relative coords

  PM_QUATERNION rot;          // orientation, identity is north-facing, upright
  PM_ROTATION_VECTOR rotVel;  // orientation velocity [.s =rad/sec]

  float steeringCurvature;    // [1/m], pos is right turn
  float steeringCrabAngle;    // [rad], pos is right forward.

  PM_RPY rpy;                 // rad
  PM_RPY rpyRates;            // rad/sec

  // these items subject to chnage
  //  double accelX;              // [m/s/s] vehicle coords
  //  double accelY;              // [m/s/s] vehicle coords
  //  double headingVar;          // [rad]
  //  int gpsFom;                 // figure of merit
  //  int gpsTime;
  //  int gpsMode;
  //  int iruMode;
  //  int bit;
};
int navdata_format(NMLTYPE type, void *buf, CMS *cms);
#endif
