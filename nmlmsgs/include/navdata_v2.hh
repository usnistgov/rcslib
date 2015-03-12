/* 
This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST RCS intelligent mobility software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.
*/ 

/************************************************************************\
 * DISCLAIMER:                                                          *
 * This software was produced by the National Institute of Standards    *
 * and Technology (NIST), an agency of the U.S. government, and by      *
 * statute is not subject to copyright in the United States.            *
 * Recipients of this software assume all responsibility associated     *
 * with its operation, modification, maintenance, and subsequent        *
 * redistribution.                                                      *
 *                                                                      *
 * See NIST Administration Manual 4.09.07 b and Appendix I.             *
 * Author : Tsai Hong                                                   *
\************************************************************************/

/*
  The update() and format() commands will be automatically generated
  by the NIST auto-code-writer routine (JAVA-based).  These will be
  found in the file *_n.cc
*/
#ifndef __INCnavdata_v2hh
#define __INCnavdata_v2hh

#include <stdio.h>

#include "nmlOffsets.hh"
#include "rcs.hh"
#include "navdata.hh"

#define NAV_DATA_V2_TYPE NAVDATA_V2_BASE
#define NAV_DATA_V2_NAME    "navdata_v2"
#define NAV_DATA_V2_SIZE 1500

// Vehicle state from navigation subsystem
class NAV_DATA_V2  : public NMLmsg
{
public:
  NAV_DATA_V2();
    
  void update( CMS*);

  int time;                   // time tag [msec] relative to some origin
  double epoTime;               // time tag [sec] relative to the Epoch
                                // (00:00:00 UTC, January 1, 1970) 
  float gps_sun_secs;           // gps time with respect to 0 hour. i.e.
  int gps_sun_h;                //  int (gps_sun_sec / 60 / 60)
  int gps_sun_m;                //  fraction (gps_sun_sec / 60 / 60) * 60

  PM_CARTESIAN tranRel;       // position [m], Relative coords, smooth
  PM_CARTESIAN tranAbs;       // position [m], Absolute coords, noisy
  int utmZone;                // UTM zone number for Absolute position
  double speed;               // [m/s] speed of vehicle, pos=forward,
                              // neg=reverse 
  PM_CARTESIAN relVel;       // velocity [m/s], Relative coords
  PM_CARTESIAN absVel;       // velocity [m/s], Abs coords

  PM_QUATERNION rot;          // orientation, identity is north-facing, upright
  PM_ROTATION_VECTOR rotVel;  // orientation velocity [.s =rad/sec]

  float steeringCurvature;    // [1/m], pos is right turn
  float steeringCrabAngle;            // [rad], pos is right forward.

  /* absolute coord */
  PM_RPY rpy;                 // rad
  PM_RPY rpyRates;            // rad/sec

  /* relative coord */
  PM_RPY relRpy;                 // rad
  PM_RPY relRpyRates;            // rad/sec

  // min/max and avg estimated error of horiziontal component of GPS in meters.
  double gps_err;
  double gps_max_err;
  double gps_min_err;
  double gps_avg_err;
  double gps_northing;
  double gps_easting;
  // double gps_sun_secs;

  int posId; // set to rfid tag number, set to zero if not used.

  double gps_timeOfWeek;
  short  gps_leapSeconds;
  short  gps_rcvrWeekNumber;

  double odometer;
  // these items subject to chnage
  //  double accelX;              // [m/s/s] vehicle coords
  //  double accelY;              // [m/s/s] vehicle coords
  //  double headingVar;          // [rad]
  //  int gpsFom;                 // figure of merit
  //  int gpsTime;
  //  int gpsMode;
  //  int iruMode;
  //  int bit;


  void Print ()
  {
    fprintf (stderr, 
             "---------------------------\n"
             "Nav Data_V2:\n"
             "---------------------------\n");
    
    fprintf (stderr, "time = %d, epoTime = %f\n", time, epoTime);
    fprintf (stderr, "tranRel = (%f %f %f)\n",
             tranRel.x, tranRel.y, tranRel.z);
    fprintf (stderr, "tranAbs = (%f %f %f)\n",
             tranAbs.x, tranAbs.y, tranAbs.z);
    fprintf (stderr, "rpy = (%f %f %f)\n", rpy.r, rpy.p, rpy.y);
    fprintf (stderr, "rot = (%f %f %f %f)\n", rot.s, rot.x, rot.y, rot.z);
    fprintf (stderr, "relRpy = (%f %f %f)\n", relRpy.r, relRpy.p, relRpy.y);
    fprintf (stderr, "speed = %f \n", speed);
    fprintf (stderr, 
             "---------------------------\n");
  }

#ifndef JAVA_DIAG_APPLET

  NAV_DATA_V2(const NAV_DATA &nd): NMLmsg(NAV_DATA_V2_TYPE,sizeof(NAV_DATA_V2)),
    time(nd.time),epoTime(0.0),gps_sun_secs(0.0),gps_sun_h(0),gps_sun_m(0),
    tranRel(nd.tranRel),tranAbs(nd.tranAbs),utmZone(nd.utmZone),speed(nd.speed),
    relVel(nd.tranVel),absVel(nd.tranVel),rot(nd.rot),rotVel(nd.rotVel),
    steeringCurvature(nd.steeringCurvature),steeringCrabAngle(nd.steeringCrabAngle),
    rpy(nd.rpy),rpyRates(nd.rpyRates), relRpy(nd.rpy),relRpyRates(nd.rpyRates),
    gps_err(0.0),gps_max_err(0.0),gps_min_err(0.0),
    gps_avg_err(0.0),gps_northing(0.0),gps_easting(0.0),
    posId(0),gps_timeOfWeek(0.0),gps_leapSeconds(0),gps_rcvrWeekNumber(0),
    odometer(0.0) {};

  inline NAV_DATA_V2 & operator =(const NAV_DATA &nd)
  {
    time = nd.time;
    tranRel = nd.tranRel;
    tranAbs = nd.tranAbs;
    utmZone = nd.utmZone;
    speed = nd.speed;
    relVel = nd.tranVel;
    rot = nd.rot;
    rotVel = nd.rotVel;
    steeringCurvature = nd.steeringCurvature;
    steeringCrabAngle = nd.steeringCrabAngle;
    rpy = nd.rpy;
    rpyRates = nd.rpyRates;
    return (*this);
  }
#endif

};

int navdata_v2_format(NMLTYPE type, void *buf, CMS *cms);

#define NAVDATA_V2_ID "$Id: navdata_v2.hh 623 2008-11-10 16:01:29Z shackle $"



#endif
