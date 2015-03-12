

#ifndef RANGE_3D_MSG_HH
#define RANGE_3D_MSG_HH

#include "rcs.hh"
#include "nmlOffsets.hh"

enum enumRange3DMsgNmlType {
  RANGE_3D_MSG_TYPE=RANGE_3D_MSG_BASE+1,
};

#define RANGE_3D_IMG_COLS  4000
#define RANGE_3D_IMG_ROWS  32

class RANGE_3D_MSG: public NMLmsg {
public:
  RANGE_3D_MSG();

  void update(CMS *);
  double timeStamp; // time in seconds since UNIX epoch Jan 1, 1970.
  float range_m[RANGE_3D_IMG_ROWS][RANGE_3D_IMG_COLS];
  float xyz_m[RANGE_3D_IMG_ROWS][RANGE_3D_IMG_COLS][3];
};


extern int range3DMsg_format(NMLTYPE type, void *buf, CMS *cms);

#endif
