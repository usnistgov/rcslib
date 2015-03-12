

#ifndef NAV200DBG_HH
#define NAV200DBG_HH


#include "rcs.hh"
#include "moastNmlOffsets.hh"

enum NAV200DBG_MSG_TYPES {
  NAV200DBG_MSG_OUT_TYPE = (NAV200DBG_BASE+1)
};
  
class NAV200DBG_MSG_OUT: public NMLmsg {
public:
  NAV200DBG_MSG_OUT();
  
  void update(CMS *);
  
  double x_m,y_m,yaw_deg;
  int quality; // 0-100, or -1,-2 
  int max_quality;
  int min_quality;
  int num_reflectors;
  struct time_tracker atrv_encoder_pos_tt;
  struct time_tracker nav_200_tt;
  PM_CARTESIAN outputPos;
  PM_RPY outputRpy;
  PM_CARTESIAN nav200Pos;
  PM_RPY nav200Rpy;
  PM_CARTESIAN offset_nav200Pos;
  PM_CARTESIAN nav200_output_diff_Pos;
  double nav200_output_diff_yaw;
  double nav200_speed;
  PM_CARTESIAN atrvEncoderPos;
  PM_CARTESIAN atrvEncoderIncrement;
  PM_RPY atrvEncoderRpy;
  double atrvEncoder_nav200_diff_yaw;
  double time_since_last_atrv_move;
  double atrv_last_move_time;
  double atrv_encoder_tran_speed;
  double atrv_encoder_rot_speed;
  double atrv_encoder_time_diff;
  double atrv_encoder_dist_diff;
  double atrv_encoder_theta_diff;
  double atrv_encoder_pos_time;
  int atrv_encoder_pos_count;
};

extern int nav200Dbg_format(NMLTYPE type, void *buf, CMS * cms);


#endif
