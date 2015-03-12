
#ifndef ATRV_TUNE_HH
#define ATRV_TUNE_HH


#include "rcs.hh"
#include "moastNmlOffsets.hh"

//generate_symbol_lookups=true

struct motor_gains {

  // Feed forward gain output = cmd_vel*ffgain + ...
  float ffgain;

  // Proportional gain  output = err*pgain + ...  
  float pgain;

  // Integral gain output = cumerr * igain + ...
  float igain;

  // Derivativ gain output = cumerr * igain + ...
  float dgain;

  /* Scale multiplies wRight or wLeft from ServoMobJACmdSkid 
   * cmd_vel = scale * wRight 
   *   -- ticks/meter
   */
  float scale;
  
  // Which encoder counter to use fixes in software the mismatched wiring.
  int counter_index;

  // max_allowed error cmd_vel - act_vel or disable servo. (Set to zero for no limit)
  float max_allowed_error;

  // clip max DAC output or set to zero for no limit.
  int max_iout;

};


struct motor_state {

  // Commanded velocity taken from ServoMobJACmdSkid wLeft or wRight *scale.
  float cmd_vel;

  // Actual velocity count_diff/time_diff;
  float act_vel;

  // Following error cmd_vel - act_vel;
  float err;

  // Cumulative err  cumerr += err each cycle.
  float cumerr;

  // Cumulative err  cumerr += err each cycle.
  float old_err;

  // Cumulative err  cumerr += err each cycle.
  float diff_err;

  // Maximum value of err since last init.
  float max_recorded_err;

  // Current encoder position in ticks
  int count;

  // Difference between encoder position and encoder position last cycle count-old_count
  int count_diff;

  // Old encoder position in ticks.
  int old_count;

  // DAC output value.
  int iout;

  // Value sent to dac last cycle
  int last_iout;

  // Cycle to cycle iout_diff
  int iout_diff;

  struct motor_gains gains;
};

enum ATRV_MSG_TYPES {
  ATRV_TUNE_MSG_OUT_TYPE = (ATRV_TUNE_BASE+1),
  ATRV_TUNE_MSG_IN_TYPE,
  ATRV_TUNE_DAC_OVERRIDE_TYPE,
  ATRV_TUNE_CLEAR_TYPE,
  ATRV_TUNE_ENABLE_TYPE
};

struct atrv_global_settings_struct {
  double max_vel;
  double max_non_turning_vel_diff;
  int turning_cw_iout_ramp_min;
  int turning_cw_iout_ramp_max;
  int turning_ccw_iout_ramp_min;
  int turning_ccw_iout_ramp_max;
  double turning_iout_ramp_period;
};

struct atrv_dac_override_struct {
  bool  dac_ramp;
  double iout_override_max_time_to_use;
  double iout_ramp_delay_time;
  int iout_override[4];
};

class ATRV_TUNE_MSG_OUT : public NMLmsg {
public:
  ATRV_TUNE_MSG_OUT();
  void update(CMS *);
  
  double timeStamp;
  bool dac_override;
  bool enabled;
  bool turning;
  bool turning_cw;
  bool turning_ccw;
  struct motor_state states[4];
  struct time_tracker tt;
  struct atrv_global_settings_struct global_settings;
  struct atrv_dac_override_struct dac_override_info;
  double turning_start_time;
  double turning_time_used;
  double iout_override_time_used;
  double iout_override_start_time;
  int turning_ramp_cycles;
  int turning_iout_ramp_val;
};

class ATRV_TUNE_MSG_IN : public NMLmsg {
public:
  ATRV_TUNE_MSG_IN();
  void update(CMS *);

  struct atrv_global_settings_struct global_settings;
  struct motor_gains gains[4];
};

class ATRV_TUNE_DAC_OVERRIDE : public NMLmsg {
public:
  ATRV_TUNE_DAC_OVERRIDE();
  void update(CMS *);

  struct atrv_dac_override_struct dac_override_info;
};


class ATRV_TUNE_CLEAR : public NMLmsg {
public:
  ATRV_TUNE_CLEAR();
  void update(CMS *);
};

class ATRV_TUNE_ENABLE : public NMLmsg {
public:
  ATRV_TUNE_ENABLE();
  void update(CMS *);
};

extern int atrvTune_format(NMLTYPE type, void *buf, CMS * cms);

#endif
// end of ifdef ATRV_TUNE_HH
