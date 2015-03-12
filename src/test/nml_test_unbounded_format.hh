
#ifndef NML_TEST_UNBOUNDED_FORMAT_HH
#define NML_TEST_UNBOUNDED_FORMAT_HH

// generate_symbol_lookups=true

#include "rcs.hh"


#define NML_TEST_UNBOUNDED_MSG_TYPE (2021)

struct ntu_s1
{
  int i;
  DECLARE_NML_UNBOUNDED_ARRAY(char,name_ua); //default=National Institute of Standards and Technology
  DECLARE_NML_UNBOUNDED_ARRAY(char,ntu_s1_char_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned char,ntu_s1_u_char_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(short,ntu_s1_short_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned short,ntu_s1_u_short_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(int,ntu_s1_int_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned int,ntu_s1_u_int_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(long,ntu_s1_long_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned long,ntu_s1_u_long_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(float,ntu_s1_float_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(double,ntu_s1_double_ua);
  int end_i;
};

class NML_TEST_UNBOUNDED_MSG : public NMLmsg{

public:
  NML_TEST_UNBOUNDED_MSG();
  void update(CMS *);
  
  int i;
  ntu_s1 s1;
  DECLARE_NML_UNBOUNDED_ARRAY(char,name_ua); //default=National Institute of Standards and Technology
  DECLARE_NML_UNBOUNDED_ARRAY(char,char_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned char,u_char_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(short,short_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned short,u_short_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(int,int_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned int,u_int_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(long,long_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(unsigned long,u_long_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(float,float_ua);
  DECLARE_NML_UNBOUNDED_ARRAY(double,double_ua);
  ntu_s1 s1_2;
  ntu_s1 s1_a[300];
  //DECLARE_NML_UNBOUNDED_ARRAY(ntu_s1,ntu_s1_ua);
  int end_i;
  int lastvar;
};


/* Declare the NML Format function. */
int nml_test_unbounded_format(NMLTYPE type, void *buf, CMS *cms);

#endif
