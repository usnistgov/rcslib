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
  \file navDataExt.hh

  \brief  NML declarations for extended navigation data. Historically
  this is based on the original NAV_DATA class, with some local
  orientation added

  \code CVS Status:
  $Author: proctor $
  $Revision: 689 $
  $Date: 2006/10/03 12:44:47 $
  \endcode

  \author Fred Proctor
*/

#ifndef NAV_DATA_EXT_HH
#define NAV_DATA_EXT_HH

#include <rcs.hh>		// NMLmsg, PM_CARTESIAN,RPY,POSE
#include "moastNmlOffsets.hh"	// NAV_DATA_EXT_BASE

#define NAV_DATA_EXT_TYPE (NAV_DATA_EXT_BASE + 1)
#define NAV_DATA_EXT_NAME "navDataExt"

/*
  The mapping of the original NAV_DATA structure to NavDataExt
  is as follows:

  tranAbs -> tranAbs, the same
  tranRel -> tranRel, the same
  tranVel -> tranRelRate
  absVel -> tranAbsRate
  rpy -> rpyAbs
  rpyRates -> rpyAbsRate
  tranRpy -> rpyRel
  tranRpyRates -> rpyRelRate
*/

/*!
  \class NavDataExt
  \brief Navigation data, e.g., position in relative- and absolute coordinates.

  NavDataExt contains the vehicle's pose in various coordinate systems,
  transformations between those coordinate systems, and UTM data.
*/
class NavDataExt : public NMLmsg {
public:
  NavDataExt() : NMLmsg(NAV_DATA_EXT_TYPE, sizeof(NavDataExt)) {};
  void update(CMS *);

  //! seconds relative to common origin
  double time;			

  //! Universal Transverse Mercator (UTM) coordinates, 1 to 60
  char utmZone;
  //! Universal Transverse Mercator (UTM) coordinates, 'N' or 'S'
  char utmLetter;

  /*!
    \a tranAbs holds absolute position, in meters, where \a x is UTM northing,
    \a y is UTM easting, and \a z points positive downward. 
  */
  PM_CARTESIAN tranAbs;
  /*!
    \a rpyAbs holds absolute orientation, in radians, where \a roll
    is rotation about the UTM northing (X) direction, \a pitch is rotation
    about the UTM easting (Y) direction, and \a yaw is rotation about
    the downward-pointing Z direction. Positive changes in yaw correspond to
    clockwise rotation as viewed looking down.
  */
  PM_RPY rpyAbs;
  //! absolute velocity, time deriv of tranAbs, [m/s]
  PM_CARTESIAN tranAbsRate;
  //! time deriv of rpyAbs, [rad/s]
  PM_RPY rpyAbsRate;

  //! relative position, [m]
  PM_CARTESIAN tranRel;
  //! relative orientation, roll-pitch-yaw, [rad]
  PM_RPY rpyRel;
  //! relative velocity, time deriv of tranAbs, [m/s]
  PM_CARTESIAN tranRelRate;
  //! time deriv of rpyRel, [rad/s]
  PM_RPY rpyRelRate;

  /*!
    'truth' values of position and orientation, in some global coordinate
    frame, obtainable in simulation but never in real life 
  */
  PM_CARTESIAN tranTrue;
  PM_RPY rpyTrue;
  PM_CARTESIAN tranTrueRate;
  PM_RPY rpyTrueRate;

  /*!
    Transformation from absolute coords to relative coords, e.g.,

    (PM_POSE) posRel = absToRel * (PM_POSE) posAbs;

    This is computable from the above as follows:

    PM_POSE poseAbs;
    PM_POSE poseRel;

    poseAbs.tran = tranAbs;
    poseAbs.rot = rpyAbs; //! built-in conversion from rpy to quaternion
    poseRel.tran = tranRel;
    poseRel.rot = rpyRel; //! ditto

    absToRel = poseRel * inv(poseAbs);

    We omit the other possibilities, trueToRel, trueToAbs, ... since they
    can be obtained through calculation as above, and aren't often needed
  */
  PM_POSE absToRel;

  //! [m/s] speed of vehicle, pos=forward, neg=reverse
  double speed;
  double steeringCurvature;
  double steeringCrabAngle;
};

extern int navDataExt_format(NMLTYPE type, void *buf, CMS * cms);
extern const char *navDataExt_symbol_lookup(long type);

#endif // NAV_DATA_EXT_HH

/*
  Modification history:

  $Log: navDataExt.hh,v $
  Revision 1.3  2006/10/03 12:44:47  proctor
  Added comment on coordinate systems in navDataExt.hh

  Revision 1.2  2005/12/14 21:50:34  cj_scrapper
  Changed time from int to double.  Time is now reported in seconds to conform to SI Units

  Revision 1.1.1.1  2005/10/12 20:50:32  root
  initial import

  Revision 1.7  2005/07/24 16:51:12  kramer
  Fixed comment on units for rpyAbsRate.

  Revision 1.5  2005/07/12 21:42:42  proctor
  Added nested headers; baseToVeh in servoMisJA.hh; sensorToBase in
  servoSP.hh

  Revision 1.4  2005/05/10 13:44:33  stephen
  Changed message to be an NMLmsg instead of a stat message. Removed include files.

  Revision 1.3  2005/05/10 13:10:46  proctor
  Doxygen-ified some more comments

  Revision 1.2  2005/05/05 20:08:27  proctor
  Worked around code generator bug by using single-line bodies for the
  constructor, {}, instead of breaking the braces across a line.

  Revision 1.1  2005/05/05 15:56:12  proctor
  Changed naming convention from e.g. NAV_DATA_EXT to NavDataExt

*/
