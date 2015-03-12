

#ifndef HUMAN_TRACKING_HH
#define HUMAN_TRACKING_HH


#include "rcs.hh"
#include "moastNmlOffsets.hh"

enum HUMAN_TRACKING_MSG_TYPES {
  HUMAN_TRACKING_MSG_OUT_TYPE = (HUMAN_TRACKING_BASE+1),
  HUMAN_TRACKING_MSG_OUT2_TYPE,
};
  
struct trackedHumanInfo {
  float x,y,vx,vy; // x and y position and velocities
};

struct trackedHumanInfo2 : public trackedHumanInfo {
  float radius; // adds a calculated radius for this person
};

class HUMAN_TRACKING_MSG_OUT: public NMLmsg {
public:
  HUMAN_TRACKING_MSG_OUT();
  
  double timeStamp; // secs since 1970
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct trackedHumanInfo,humans,16);

  void update(CMS *);

};

class HUMAN_TRACKING_MSG_OUT2: public NMLmsg {
public:
  HUMAN_TRACKING_MSG_OUT2();
  
  double timeStamp; // secs since 1970
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct trackedHumanInfo2,humans,16);

  void update(CMS *);

};

extern int humanTracking_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * humanTracking_symbol_lookup(long type);

#endif
