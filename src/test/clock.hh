#ifndef CLOCK_HH
#define CLOCK_HH

#include "rcs.hh"

#define MY_CLOCK_TYPE 1001

class MY_CLOCK: public NMLmsg
{
public:
  MY_CLOCK();
  void update(CMS *);
  
  CMS_TIME now;
};


#endif

