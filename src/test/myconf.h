#ifndef MY_CONF_H
#define MY_CONF_H

#include <rcs.hh>

#define MY_CONF_TYPE 10001

class MY_CONF : public NMLmsg
{
 public:
  MY_CONF();
  void update(CMS *);

  double p_gain;
  double i_gain;
  double d_gain;
};

#endif

