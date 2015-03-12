/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.

*/ 

/* This is a header file which is deliberately intended to test and
   exercise the NML CodeGenerator
   in addition to being an example.
*/

#ifndef NML_TEST_FORMAT_HH
#define NML_TEST_FORMAT_HH

// generate_symbol_lookups=true

#include "rcs.hh"

#include "otherheader.hh"


// These are not used anywhere we just want to make sure the CodeGenerator can ignore them.
template<class T> T cubed(const T &val)  { return val * val * val; }
template<class T> T abs(const T &val)  { return val >= (T) 0 ? val : -val; }
template<class T> T sign(const T &val)  { return val == (T) 0 ? 0 : val > (T) 0 ? (T) 1 : (T) -1; }

// This not used anywhere we just want to make sure the CodeGenerator can ignore them.
#define CONSTRAIN(A,B,C)  (((A) < (B)) ? (B) : ((A) > (C)) ? (C) : (A))


#ifndef JAVA_DIAG_APPLET
// CodeGen should know to ignore anything inside ifndef JAVA_DIAG_APPLET
struct struct_with_pointer
{
  void * vp;
};
#endif


#define MY_STAT_TYPE 1001 // all caps, underscores

class MyStat : public RCS_STAT_MSG { // mixed case, no underscores
public:
 MyStat() : RCS_STAT_MSG(MY_STAT_TYPE, sizeof(MyStat)) {
 };
  
  void update(CMS *);
};


#define MY_STAT_V2_TYPE 2002 // all caps, underscores

class MyStat2 : public RCS_STAT_MSG_V2 { // mixed case, no underscores
public:
 MyStat2() : RCS_STAT_MSG_V2(MY_STAT_V2_TYPE, sizeof(MyStat2)) {
 };
  
  void update(CMS *);
};


enum             {INIT, SIDEWALK, UNKNOWN, PARKINGPAVED,
                 BUILDING, TREES, DRIVEPAVED, BUILDINGCONNECTOR,
                 CONCRETE, STEPS, SUBSTATION, LAKE, UTILITYBOX,
                 UTILITYPOLE, SHRUB, TREE, SIGN, LIGHTPOLE, LAMP,
                 CATCHBASIN, CATCH_BASINCULVERT, HYDRANT, MANHOLE, PATH, LANE, GRASS, OBSTACLE, ROAD};

const int num_object_classes = 28;

#define NTFHH_BASE 100
#define TEST_MESSAGE_BASE_TYPE (NTFHH_BASE+0)
#define TEST_MESSAGE_TYPE (NTFHH_BASE+1)
#define SIMPLER_MSG_TYPE (NTFHH_BASE+2)
#define QTEST_MSG_TYPE (NTFHH_BASE+3)
#define BOP_MSG_TYPE (NTFHH_BASE+4)

enum enumtest{ a, b, aa, bb, ccc=99,dd=77, e=88};

enum enumtest3ftoh
{ 
  fff,
  ggg,
  hhh
};

typedef enum { zzz, yyy, xxx } enumtest_typedef;

typedef enum 
{
 www,
 vvv,
 uuu } 
enumtest_typedef2;

extern "C" { 

struct c_struct
{
  char csc;
  int csi;
};

}
// end of extern "C"


extern "C"
{ 

struct c_struct2
{
  char csc2;
  int csi2;
};

}
// end of extern "C"
#define FOUR (TWO*two)
const int two = 2;
#define TWO two

typedef struct _teststruct {

  struct c_struct cs;
  bool b;
  int i;
  char c;
  float f;
  double d;
  bool bool_array[FOUR];
  int ia[2];
  char ca[2];
  float fa[2];
  double da[2];
  double two_d_array[2][2];
  char  two_c_array[2][2];
  double three_d_array[2][2][2];
  char three_c_array[2][2][2];
  double d_pi; //default=3.14159265
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,cda, 2);
  double seventysevenpointseven; //default=77.7
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,ida, 2);
  double eightyeightpointeight; //default=88.8
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fda, 2);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double,dda, 2);
  float f_pi; //default=3.14159265
  char endtsbuf[16]; //default=endts
  PmCartesian pm_cart_test;
  PmCartesian pm_cart_test_array[4];

}teststruct;

typedef struct _teststruct_td2 {

  struct c_struct2 cs2;
  bool b;
  int i;
  char c;
  float f;
  double d;
  bool bool_array[2];
  int ia[2];
  char ca[2];
  float fa[2];
  double da[2];
  double two_d_array[2][2];
  double three_d_array[2][2][2];
  float f_pi; //default=3.14159265
  double d_pi; //default=3.14159265
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,cda, 2);
  double seventysevenpointseven; //default=77.7
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,ida, 2);
  double eightyeightpointeight; //default=88.8
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fda, 2);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double,dda, 2);
  char endtsbuf[16]; //default=endts
  
}
teststruct_td2;

class TEST_MESSAGE_BASE : public RCS_STAT_MSG_V2
{
public:
  TEST_MESSAGE_BASE();
  TEST_MESSAGE_BASE(NMLTYPE _t, size_t _s):RCS_STAT_MSG_V2(_t,_s),test_message_base_var(0) {};
  void update(CMS *);

  int test_message_base_var;
};

typedef unsigned short fwPixel;
struct fwLaserStruct {
  fwPixel rangep[4*3];
};
class TEST_MESSAGE: public TEST_MESSAGE_BASE
{
public:
  TEST_MESSAGE();
  void update(CMS *);

  char byte_to_messup_msg;

  long first_count;
  struct struct_from_other_header sfoh;
  bool b;
  char c;
  double d;
  int i;
  float f;

  long l;
  unsigned long ul;

  /* 
   * FIXME: long long are more trouble to support than they are worth.
   long long ll;
   unsigned long long ull;
   long long lla[3];
   unsigned long long ulla[3];
  */

  //  double big_double_array[10000];
  struct fwLaserStruct fw;
  enumtest_typedef etd;
  enumtest_typedef2 etd2;
  char big_array[1000];
  bool bool_array[2];
  int ia[2];
  char ca[2];
  float fa[2];
  double da[2]; 
  double two_d_array[2][2];
  double three_d_array[2][2][2];
  float f_pi; //default=3.14159265
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char,cda, 8);
  double seventysevenpointseven; //default=77.7
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int,ida, 8);
  double eightyeightpointeight; //default=88.8
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fda, 8);
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double,dda, 8);
  teststruct s;
  teststruct_td2 s_td2;
  teststruct sa[2];
  double d_pi; //default=3.14159265
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(teststruct,sda, 2);
  enum enumtest enumtestvar;
  enum enumtest enum_array[5];
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(enum enumtest,enumtest_dla,7);
  PM_CARTESIAN cart;
  PM_CARTESIAN cart_array[3];
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN,cart_dla,5);
  bool do_int_size_test;
  short smin;
  short smax;
  int i_smin;
  int i_smax;
  int imin;
  int imax;
  long l_imin;
  long l_imax;
  long lmin;
  long lmax;
  unsigned short usmax;
  unsigned int ui_usmax;
  unsigned int uimax;
  unsigned long ul_uimax;
  unsigned long ulmax;
  double d_ulmax;
  double d_lmin;
  double d_lmax;
  short s_array[3];
  int i_array[3];
  long l_array[3];
  unsigned short us_array[2];
  unsigned int ui_array[2];
  unsigned long ul_array[2];
  bool false_bool;
  bool true_bool;
  short sminusone;
  int iminusone;
  long lminusone;
  float fminusone;
  double dminusone;
  long last_count;
  teststruct teststruct_2d_array[2][2];
  long lastvar;
};

class SIMPLER_MSG : public NMLmsg
{
public:
  SIMPLER_MSG();
  void update(CMS *);
  
  int i;
  char cbuf[80];
  long lastvar;
};

#define ONE_HUNDRED 100

class QTEST_MSG: public NMLmsg
{
public:
  QTEST_MSG();
  void update(CMS *);

  int priority;
  int pchanges_count;
  int count;
  int pid;
  char line[ONE_HUNDRED+5];
  double time;
};

class BOP_MSG: public NMLmsg
{
public:
  BOP_MSG();
  void update(CMS *);
  
  unsigned long ula[2];
};


/* Declare the NML Format function. */
int nml_test_format(NMLTYPE type, void *buf, CMS *cms);

#endif 

  
