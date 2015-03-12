

#ifndef MYMSG_HH
#define MYMSG_HH


#include "rcs.hh"

#define MYMSG_TYPE (2002)

class MYMSG: public NMLmsg
{
public:
  MYMSG();
  void update(CMS *);

  int int_in_mymsg;
};

extern int MYMSG_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
