

#ifndef PRIM_DEBUG_HH
#define PRIM_DEBUG_HH


#include "rcs.hh"
#include "moastNmlOffsets.hh"

enum PRIM_DEBUG_MSG_TYPES {
  PRIM_DEBUG_MSG_OUT_TYPE = (PRIM_DEBUG_BASE+1)
};
  
struct PathPoint
{
  double r;
  double theta;
  double yaw;
  double speed;
  int waypoints_index;
  PM_CARTESIAN nav;
};

struct PathPointList
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct PathPoint,pp,24);
};

struct CartList
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN,c,100);
};

enum PRIM_ALGORITHM_TYPE {
  A1 = 1,
  A2 = 2,
  A3 = 3,
  A4 = 4,
  A5 = 5
};


class PRIM_DEBUG_MSG_OUT: public NMLmsg {
public:
  PRIM_DEBUG_MSG_OUT();
  
  void update(CMS *);
  
  struct time_tracker tt;
  double pathLength;
  double minPathLength;
  double maxPathLength;
  double maxTranAccel;
  double maxTranSpeed;
  double maxRotSpeed;
  double maxRotAccel;
  double tranSpeed;
  double rotSpeed;
  double tranAccel;
  double rotAccel;
  double waypoint_speed;
  double speed_ratio;
  double r;
  double fabs_r;
  double min_fabs_r;
  double max_fabs_r;
  double theta_r;
  double min_theta_r;
  double max_theta_r;
  double expected_err;
  double expected_err_max;
  double cur_err;
  double cur_err_max;
  double dist_from_path;
  double dist_from_path_max;
  double theta;
  double yaw;
  double wp_move_start_time;
  double wp_start_speed;
  double time_since_start;
  double accel_max_speed;
  bool obstacle_in_path;
  double dist_to_obstacle; // Distance vehicle can move along path before hitting obstacle.
  double time_without_movement;
  double time_since_new_command;
  double obstacle_dist; // distance between outer edge of vehicle and obstacle when the obstacle will be considered in the path.
  double obstacle_point_sensed_time;
  double obstacle_point_time_since_sensed;
  double tranSpeedIn; // tran speed before final clipping
  double rotSpeedIn; // rot speed before final clipping
  double currentDirNextSegDot;
  bool using_cubic;
  enum PRIM_ALGORITHM_TYPE algorithm; //default=A4
  double cubicA;
  double cubicB;
  PM_CARTESIAN wp_start_point;
  PM_CARTESIAN s;
  PM_CARTESIAN next_point;
  PM_CARTESIAN r_center;
  PM_CARTESIAN ortho;
  PM_CARTESIAN maxTranSpeedPoint;
  PM_CARTESIAN maxRotSpeedPoint;
  PM_CARTESIAN maxTranAccelPoint;
  PM_CARTESIAN maxRotAccelPoint;
  PM_CARTESIAN minRMagPoint;
  PM_CARTESIAN maxRMagPoint;
  PM_CARTESIAN minTheraRPoint;
  PM_CARTESIAN maxTheraRPoint;
  PM_CARTESIAN minPathLengthPoint;
  PM_CARTESIAN maxPathLengthPoint;
  PM_CARTESIAN obstacle_point;
  CartList arcList;
  CartList pathList;
  PathPointList ppl;
  PM_CARTESIAN nextSegmenUnitVec;
  PM_CARTESIAN currentDirUnitVec;
  
};

extern int primDebug_format(NMLTYPE type, void *buf, CMS * cms);
extern const char *primDebug_symbol_lookup(long type);
extern const char *primDebug_enum_PRIM_ALGORITHM_TYPE_symbol_lookup(long v);

// generate_symbol_lookups=true
// generate_all_enum_symbol_lookups=true

#endif
