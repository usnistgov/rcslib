

#include "rcs.hh"
#include "timetype.hh"

int timetype_format(NMLTYPE type, void *buf, CMS *cms)
{
  switch(type) 
  {
  case NML_TIME_TYPE:
    ((NML_TIME *) buf)->update(cms);
    return 1;
  case NML_START_TIMER_TYPE:
    ((NML_START_TIMER *) buf)->update(cms);
    return 1;
  case NML_STOP_TIMER_TYPE:
    ((NML_STOP_TIMER *) buf)->update(cms);
    return 1;
  case NML_RESET_TIMER_TYPE:
    ((NML_RESET_TIMER *) buf)->update(cms);
    return 1;
    
  default:
    return 0;
  }
  return 0;
}


void NML_TIME::update(CMS *cms)
{
  cms->update(time);
}

 

void NML_RESET_TIMER::update(CMS *cms)
{
}

void NML_STOP_TIMER::update(CMS *cms)
{
}

void NML_START_TIMER::update(CMS *cms)
{
}
 
  
