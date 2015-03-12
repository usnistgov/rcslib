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

class NML_PERFORMANCE_TEST_MSG:public NMLmsg
{
public:
  NML_PERFORMANCE_TEST_MSG ():NMLmsg (NML_PERFORMANCE_TEST_MSG_TYPE,
				      sizeof (NML_PERFORMANCE_TEST_MSG))
  {
  };
  void update (CMS *);
  int serial_number;
  int test_type;
  long array_length;

#ifndef JAVA_DIAG_APPLET
  union
  {
    char char_data;
    short short_data;
    int int_data;
    long long_data;
    float float_data;
    double double_data;
  };
  void compute_array_length ();
  void *operator      new (size_t);
  void operator      delete (void *);
#endif

};

enum TEST_TYPE
{
  CHAR_TEST,
  SHORT_TEST,
  INT_TEST,
  LONG_TEST,
  FLOAT_TEST,
  DOUBLE_TEST
};

extern "C"
{
#include <stddef.h>		/* size_t  */
}

extern void set_real_test_msg_size (long);
extern void delete_test_msg_buffer ();
extern unsigned int real_test_msg_size;
extern unsigned int test_msg_buffer_size;
extern void *test_msg_buffer;
extern int perf_types_format (NMLTYPE type, void *buffer, CMS * cms);



#endif /* !PTYPES_HH */
