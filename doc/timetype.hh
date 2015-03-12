/***************************************************************************
* File: timetype.hh
*
* This C++ Header file defines the messages that will be used by the 
* wtimer psuedo controller. 
*
***************************************************************************/

#ifndef TIMETYPE_HH
#define TIMETYPE_HH

#include "rcs.hh"		// class NMLmsg, class CMS

#define NML_TIME_TYPE (1001)
#define NML_START_TIMER_TYPE (2001)
#define NML_STOP_TIMER_TYPE (2002)
#define NML_RESET_TIMER_TYPE (2003)

class NML_TIME: public NMLmsg
{
public:
  NML_TIME(): NMLmsg(NML_TIME_TYPE, sizeof(NML_TIME)) {};
  void update(CMS *);
  double time;
};


class NML_START_TIMER: public NMLmsg
{
public:
  NML_START_TIMER(): NMLmsg(NML_START_TIMER_TYPE, sizeof(NML_START_TIMER)) {};
  void update(CMS *);
};

 
class NML_STOP_TIMER: public NMLmsg
{
public:
  NML_STOP_TIMER(): NMLmsg(NML_STOP_TIMER_TYPE, sizeof(NML_STOP_TIMER)) {};
  void update(CMS *);
};

class NML_RESET_TIMER: public NMLmsg
{
public:
  NML_RESET_TIMER(): NMLmsg(NML_RESET_TIMER_TYPE, sizeof(NML_RESET_TIMER)) {};
  void update(CMS *);
};


int timetype_format(NMLTYPE, void *, CMS *);


#endif
