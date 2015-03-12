
#ifndef NML_UNBOUNDED_ARRAY_TEST_MSG_HH
#define NML_UNBOUNDED_ARRAY_TEST_MSG_HH

#include "rcs.hh"


struct s1
{
  int i;
  double d;
};

struct s2
{
  DECLARE_NML_UNBOUNDED_ARRAY(struct s1, s1_in_s2_ubP);
  DECLARE_NML_UNBOUNDED_ARRAY(char, c_ubP);
}

#define NML_UNBOUNDED_ARRAY_TEST_MSG_TYPE 101

class NML_UNBOUNDED_ARRAY_TEST_MSG: public NMLmsg
{
public:

  NML_UNBOUNDED_ARRAY_TEST_MSG();
  void update(CMS *);
  
  struct s2 s2_m;
  DECLARE_NML_UNBOUNDED_ARRAY(float, f_ubP);
  DECLARE_NML_UNBOUNDED_ARRAY(struct s2, s2_ubP);
};

/* Declare the NML Format function. */
int nml_unbounded_array_test_msg_format(NMLTYPE type, void *buf, CMS *cms);

#endif
