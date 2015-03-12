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

/************************************************************************
File: filemem.hh
Purpose: Defines FILEMEM which is a derived class of CMS that provides
a means of logging messages to a file or of reading messages from
a pseudo-script.
**********************************************************************/

#ifndef FILEMEM_HH
#define FILEMEM_HH

#include <stdio.h>		// FILE

#include "cms.hh"		// class CMS

#define FILEMEM_INPUT_BUFFER_SIZE (2048)


class FILEMEM_NONPORT_INTERNALS;

class FILEMEM:public CMS
{
public:
  FILEMEM (const char *bufline,
	   const char *procline,
	   int set_to_server =0,
	   int set_to_master = 0);

  virtual ~ FILEMEM ();

  /* Overloaded CMS functions. */
  CMS_STATUS clear ();
  int check_if_read ();
  CMS_STATUS read ();
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
  CMS_STATUS write_if_read (void *data);

protected:
  char infile_name[80];
  char outfile_name[80];
  long input_file_pos;
  long max_output_messages;
  void close_input ();
  void reopen_input ();
  FILE *in;
  FILE *out;
  char input_buffer[FILEMEM_INPUT_BUFFER_SIZE];
  int input_is_stdin;
  int output_is_stdout;
  double last_read_time;
  double read_time;
  double last_write_time;
  double write_time;
  double wait_period;
  double wait_start;
  int write_cycle;
  int write_count;
  int read_cycle;
  int add_waits;

  class FILEMEM_NONPORT_INTERNALS *internals;

  long write_file_length;
  int lock_input ();
  int lock_output ();
  int unlock_input ();
  int unlock_output ();

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  FILEMEM(const FILEMEM &);
  FILEMEM &operator=(const FILEMEM &);

};



#endif
