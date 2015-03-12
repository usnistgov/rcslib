// Prevent Multiple Inclusion
#ifndef EXSTAT_HH
#define EXSTAT_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

enum exStatMsgType {
  EX_STATUS_TYPE=6000
};

// Define the NML Message Classes

// Status Class
class EX_STATUS : public RCS_STAT_MSG
{
public:

  // Normal Constructor
  EX_STATUS();

  // Constructor used by derived classes
  EX_STATUS(NMLTYPE t, size_t s) : RCS_STAT_MSG(t,s) {};

  // CMS Update Function
  void update(CMS *);

  // Place custom variables here.
  PM_CARTESIAN position;

};

extern int exStat_format(NMLTYPE, void *, CMS *);

#endif 	// EXSTAT_HH
