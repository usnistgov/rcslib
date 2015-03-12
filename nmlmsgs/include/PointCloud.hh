

#ifndef POINTCLOUD_HH

#include "rcs.hh" // NMLmsg ...
#include "nmlOffsets.hh" // POINTCLOUD_BASE

enum enumPointCloudNmlType {
  POINTCLOUD_MSG_TYPE=POINTCLOUD_BASE+1,
};

#define MAX_POINTCLOUD_POINTS (1024*1024)

class POINTCLOUD_MSG : public NMLmsg {
public:
  POINTCLOUD_MSG();
  void update(CMS *);

  double timeStamp;
  int rows;
  int cols;
  bool dense;

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,x,MAX_POINTCLOUD_POINTS);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,y,MAX_POINTCLOUD_POINTS);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,z,MAX_POINTCLOUD_POINTS);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,color,MAX_POINTCLOUD_POINTS);

  // the row and col for the given index  ie x[i] came from row[i],column[i] in
  // the original dense range image.
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,row,MAX_POINTCLOUD_POINTS);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,col,MAX_POINTCLOUD_POINTS);
};

extern int PointCloud_format(NMLTYPE type, void *buf, CMS *cms);

#endif
