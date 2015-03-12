/*
 * Filename: perftype.hh
 *
 * Purpose:
 *  Defines the message class for messages that will be used by nmlperf and 
 * perfsvr.
 */

#ifndef PERFTYPE_HH
#define PERFTYPE_HH

#include "nmlmsg.hh"		/* NMLmsg */

#define NML_PERFORMANCE_TEST_MSG_TYPE       ((NMLTYPE) 0xff)

class NML_PERFORMANCE_TEST_MSG: public NMLmsg
{
 public:
  NML_PERFORMANCE_TEST_MSG(): NMLmsg(NML_PERFORMANCE_TEST_MSG_TYPE, sizeof(NML_PERFORMANCE_TEST_MSG)) {};
  void update(CMS *);
  int serial_number;
  int test_type;
  long array_length;

#ifndef JAVA_DIAG_APPLET
  union {
    char char_data;
    short short_data;
    int int_data;
    long long_data;
    float float_data;
    double double_data;
  };
 void compute_array_length();
 void *operator new(unsigned int);
 void operator delete(void *);
#endif

};

enum TEST_TYPE{
  CHAR_TEST,
  SHORT_TEST,
  INT_TEST,
  LONG_TEST,
  FLOAT_TEST,
  DOUBLE_TEST
};

extern "C" {
#include <stddef.h>		/* size_t  */
}

extern void set_real_test_msg_size(size_t);
extern void delete_test_msg_buffer();
extern size_t real_test_msg_size;
extern size_t test_msg_buffer_size;
extern void *test_msg_buffer;
extern int perf_types_format(NMLTYPE type, void *buffer, CMS *cms);



#endif				/* !PTYPES_HH */
