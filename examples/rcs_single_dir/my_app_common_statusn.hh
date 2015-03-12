

#ifndef MY_APP_COMMON_STATUSN_HH
#define MY_APP_COMMON_STATUSN_HH

#include "rcs.hh"

#define MY_APP_COMMON_STAT_MSG_TYPE (0)

// Status Class
class MY_APP_COMMON_STAT_MSG : public RCS_STAT_MSG
{
public:
	// Constructor used by derived classes
	MY_APP_COMMON_STAT_MSG(NMLTYPE t, size_t s) :  RCS_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  int common_int_var;

private:
	// Normal Constructor
	MY_APP_COMMON_STAT_MSG(): RCS_STAT_MSG(0,0) {};

};

#endif
