#ifndef SONARDATA_HH
#define SONARDATA_HH

#include <stdio.h>
#include "rcs.hh"
#include "nmlOffsets.hh"        // SONARSAFETY_DATA_BASE
#include "timetracker.hh"

#define SONAR_DATA_TYPE (SONAR_DATA_BASE)

#define SONAR_RP_MAX 48


struct sonar_range_frame
{
  double sonar_datatime;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float, sonar_rp, SONAR_RP_MAX);
};

struct xyzrpy_struct
{
  double x,y,z,roll,pitch,yaw;
};

struct xyzrpy_struct_frame
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct xyzrpy_struct, pose, SONAR_RP_MAX);
};

struct position_2d 
{
  double datatime;

  /** Robot geometry in robot cs: pose gives the position2d and
      orientation, size gives the extent.  These values are filled in
      by playerc_position2d_get_geom(). */
  double pose[3];
  double size[2];

  /** Odometric pose (m, m, rad). */
  double px, py, pa;

  /** Odometric velocity (m/s, m/s, rad/s). */
  double vx, vy, va;

  /** Stall flag [0, 1]. */
  int stall;
};

class SONAR_DATA : public NMLmsg
{
public:
  SONAR_DATA();
  void update(CMS *);

  double timeStamp;
  double fov_deg; // field of view in degrees.
  double start_angle_deg;  // start angle in degrees.

  struct time_tracker tt;
  struct sonar_range_frame range_frame;
  struct xyzrpy_struct_frame xyz_frame;
  struct position_2d pos2d;
};


int sonarData_format(NMLTYPE type, void *buf, CMS *cms);

#define SONAR_DATA_ID "$Id: sonarData.hh 623 2008-11-10 16:01:29Z shackle $"
#define SONAR_DATA_REV "$Rev: 623 $"

static const char *SONAR_DATA_HEADER_FILE=__FILE__;

#endif
