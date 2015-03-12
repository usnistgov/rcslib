

#ifndef HUMAN_TRACKING_DEBUG_HH
#define HUMAN_TRACKING_DEBUG_HH


#include "rcs.hh"
#include "moastNmlOffsets.hh"
#include "humanTracking.hh"

enum HUMAN_TRACKING_DEBUG_MSG_TYPES {
  HUMAN_TRACKING_DEBUG_MSG_TYPE = (HUMAN_TRACKING_DEBUG_BASE+1),
  HUMAN_TRACKING_DEBUG_DBG_MSG_TYPE
};


struct debugPointInfo {
  PM_CARTESIAN sensorCart; // cartesian location in sensor coords.
  PM_CARTESIAN worldCart; 
  bool past_background;
  bool range_change;
  int leg_id;
  int human_id;
  int sensor_id;
};

struct debugLegInfo {
  PM_CARTESIAN center;
  int num_points;
};

struct pointsList {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct debugPointInfo,pts,5000);
};

struct debugHumanInfo {
  struct debugLegInfo legs[2];
  PM_CARTESIAN center;
  int id;
};

struct humansList {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct debugHumanInfo,humans,50);  
};

struct sensorDebugInfo{
  PM_CARTESIAN pos;
  PM_RPY rpy;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,name,40);
};

struct sensorsList {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct sensorDebugInfo,sensors,50);  
};  

class HUMAN_TRACKING_DEBUG_MSG: public NMLmsg {
public:
  HUMAN_TRACKING_DEBUG_MSG();
  void update(CMS *);

  double LEG_TOLERANCE;
  double HUMAN_TOLERANCE;
  struct pointsList pl;
  struct humansList hl;
  struct sensorsList sl;

};

extern int humanTrackingDebug_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * humanTrackingDebug_symbol_lookup(long type);

#endif
