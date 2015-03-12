#ifndef ECHO_TYPES_HH
#define ECHO_TYPES_HH

#include "rcs.hh"

#define ECHO_QUERY_TYPE 101
#define ECHO_REPLY_TYPE 102

class ECHO_QUERY: public NML_QUERY_MSG
{
public:
  ECHO_QUERY();
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,line,80);
};

class ECHO_REPLY: public NMLmsg
{
public:
  ECHO_REPLY();
  void update(CMS *);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,line,80);
};

extern int ECHO_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
