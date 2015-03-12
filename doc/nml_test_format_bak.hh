
#ifndef NML_TEST_FORMAT_HH
#define NML_TEST_FORMAT_HH

#include "rcs.hh"

#define TEST_MESSAGE_TYPE (101)
#define SIMPLER_MSG_TYPE (102)
#define QTEST_MSG_TYPE (103)
#define BOP_MSG_TYPE (104)

enum enumtest{ a, b};

typedef struct _teststruct {
  
  int i;
  char c;
  float f;
  double d;
  int ia[4];
  char ca[4];
  float fa[4];
  double da[4];
  double two_d_array[4][4];
  double three_d_array[4][4][4];
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,cda, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,ida, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fda, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double,dda, 8);
  
}teststruct;

 
class TEST_MESSAGE: public NMLmsg
{
public:
  TEST_MESSAGE();
  void update(CMS *);
  
  char c;
  double d;
  int i;
  float f;
  int ia[4];
  char ca[4];
  float fa[4];
  double da[4]; 
  double two_d_array[4][4];
  double three_d_array[4][4][4];
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,cda, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,ida, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fda, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double,dda, 8);
  teststruct s;
  teststruct sa[4];
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(teststruct,sda, 4);
  enum enumtest enumtestvar;
  long lastvar;
};

class SIMPLER_MSG : public NMLmsg
{
public:
  SIMPLER_MSG();
  void update(CMS *);
  
  int i;
  char cbuf[80];
};

class QTEST_MSG: public NMLmsg
{
public:
  QTEST_MSG();
  void update(CMS *);

  int priority;
  int pchanges_count;
  int count;
  int pid;
  char line[105];
  double time;
};

class BOP_MSG: public NMLmsg
{
public:
  BOP_MSG();
  void update(CMS *);
  
  unsigned long ula[4];
};


/* Declare the NML Format function. */
int nml_test_format(NMLTYPE type, void *buf, CMS *cms);

#endif 

  
