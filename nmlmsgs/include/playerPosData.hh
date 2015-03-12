#ifndef PLAYERPOSDATA_HH
#define PLAYERPOSDATA_HH

#include <stdio.h>
#include "rcs.hh"
#include "nmlOffsets.hh"        // PLAYERPOSSAFETY_DATA_BASE
#include "timetracker.hh"

#define PLAYERPOS_DATA_TYPE (PLAYERPOS_DATA_BASE)

struct player_position_2d 
{
  double datatime;
  int index;
  char drivername[64];

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

class PLAYERPOS_DATA : public NMLmsg
{
public:
  PLAYERPOS_DATA();
  void update(CMS *);

  double timeStamp;

  struct time_tracker tt;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct player_position_2d,pos2d,10);
};


int playerPosData_format(NMLTYPE type, void *buf, CMS *cms);

#define PLAYERPOS_DATA_ID "$Id: playerPosData.hh 623 2008-11-10 16:01:29Z shackle $"
#define PLAYERPOS_DATA_REV "$Rev: 623 $"

static const char *PLAYERPOS_DATA_HEADER_FILE=__FILE__;

#endif
