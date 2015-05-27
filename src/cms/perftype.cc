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

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1


extern "C"
{
#include <stdio.h>
#include <stdlib.h>		/* malloc(), free() */
}

#include "perftype.hh"
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "cms.hh"		/* CMS */
#include "nml.hh"		/* NML */

unsigned int real_test_msg_size = 0;
unsigned int test_msg_buffer_size = 0;
void *test_msg_buffer = NULL;


int
perf_types_format (NMLTYPE type, void *buffer, CMS * cms)
{

  switch (type)
    {
    case NML_PERFORMANCE_TEST_MSG_TYPE:
      ((NML_PERFORMANCE_TEST_MSG *) buffer)->update (cms);
      break;
    default:
      //rcs_print_error ("perf_types_format: Unknown Type.(%ld) (mode = %d)\n",
      //		       (long) type, cms->mode);
      //return (-1);
      return 0;
    }
  return (1);
}


void
NML_PERFORMANCE_TEST_MSG::update (CMS * cms)
{
  cms->update (serial_number);
  cms->update (test_type);
  cms->update (array_length);
  switch (test_type)
    {
    case CHAR_TEST:
      cms->update (&char_data, (array_length > 0) ? array_length : 1);
      break;
    case SHORT_TEST:
      cms->update (&short_data, (array_length > 0) ? array_length : 1);
      break;
    case INT_TEST:
      cms->update (&int_data, (array_length > 0) ? array_length : 1);
      break;
    case LONG_TEST:
      cms->update (&long_data, (array_length > 0) ? array_length : 1);
      break;
    case FLOAT_TEST:
      cms->update (&float_data, (array_length > 0) ? array_length : 1);
      break;
    case DOUBLE_TEST:
      cms->update (&double_data, (array_length > 0) ? array_length : 1);
      break;
    }
}

void
NML_PERFORMANCE_TEST_MSG::compute_array_length ()
{
  long array_unit_size, base_size;
  switch (test_type)
    {
    case CHAR_TEST:
      array_unit_size = sizeof (char);
      break;
    case SHORT_TEST:
      array_unit_size = sizeof (short);
      break;
    case INT_TEST:
      array_unit_size = sizeof (int);
      break;
    case LONG_TEST:
      array_unit_size = sizeof (long);
      break;
    case FLOAT_TEST:
      array_unit_size = sizeof (float);
      break;
    case DOUBLE_TEST:
      array_unit_size = sizeof (double);
      break;
    default:
      rcs_print_error ("PERFTYPE: Invalid test type. (%d)\n", test_type);
      array_unit_size = 1;
      break;
    }
  base_size = sizeof (NML_PERFORMANCE_TEST_MSG) - sizeof (double);
  array_length = (size - base_size) / array_unit_size;
  if (array_length > 0)
    {
      size = array_length * array_unit_size + base_size;
    }
  else
    {
      rcs_print_error
	("NML_PERFORMANCE_TEST_MSG::compute_array_length() - array_length <= 0\n");
    }
}

void *
  NML_PERFORMANCE_TEST_MSG::operator
new (size_t size)
{
  if (test_msg_buffer_size > size && test_msg_buffer_size > real_test_msg_size
      && test_msg_buffer != NULL)
    {
      return test_msg_buffer;
    }
  if (test_msg_buffer != NULL)
    {
      free (test_msg_buffer);
      test_msg_buffer = NULL;
    }
  if (real_test_msg_size > size)
    {
      test_msg_buffer_size = real_test_msg_size;
      return (test_msg_buffer = calloc (real_test_msg_size, 1));
    }
  test_msg_buffer_size = size;
  return (test_msg_buffer = calloc (size, 1));
}

void
  NML_PERFORMANCE_TEST_MSG::operator
delete (void *ptr)
{
  if (ptr != test_msg_buffer && ptr != NULL)
    {
      free (ptr);
    }
}

void
delete_test_msg_buffer ()
{
  if (NULL != test_msg_buffer)
    {
      free (test_msg_buffer);
      test_msg_buffer = NULL;
    }
}

void
set_real_test_msg_size (long size)
{
  real_test_msg_size = size;
}
