#ifndef SICKDATA_HH
#define SICKDATA_HH

#include <stdio.h>
#include "rcs.hh"
#include "nmlOffsets.hh"        // SICKSAFETY_DATA_BASE
#include "timetracker.hh"

#define SICK_DATA_TYPE (SICK_DATA_BASE)

#define SICK_RP_MAX 402

#define SICK_DATA_ID "$Id: sickData.hh 894 2010-05-05 12:36:57Z shackle $"
#define SICK_DATA_REV "$Rev: 894 $"

#ifdef LOG_DATA_FORMAT
static const char *SICK_DATA_HEADER_FILE=__FILE__;
#endif

struct sick_range_frame
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float, sick_rp, SICK_RP_MAX);
};

class SICK_DATA : public NMLmsg
{
public:
  SICK_DATA();
  void update(CMS *);

  double timeStamp;
  double fov_deg; // field of view in degrees.
  double start_angle_deg;  // start angle in degrees.

  struct time_tracker tt;
  struct sick_range_frame range_frame;
};


int sickData_format(NMLTYPE type, void *buf, CMS *cms);

#define SICK_DATA_ID "$Id: sickData.hh 894 2010-05-05 12:36:57Z shackle $"


#endif
