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


#include "nml_test_format.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int 
main(int argc, char **argv)
{
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);

  if(argc < 6)
    {
      fprintf(stderr,"!ERROR! usage : bufname procname configfile timeout lastvar\n");
      exit(124);
    }
  
  NML *nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  if(0 == nml)
    {
      fprintf(stderr,"!ERROR! 0==nml\n");
      exit(123);
    }
  if(!nml->valid())
    {
      fprintf(stderr,"!ERROR! nml->valid() check failed.\n");
      delete nml;
      exit(122);
    }
  double timeout=strtod(argv[4],0);
  int t;
  if(TEST_MESSAGE_TYPE != (t = nml->blocking_read(timeout)))
    {
      fprintf(stderr,"!ERROR! nml->blocking_read(%f) returned %d\n",timeout, t);
      delete nml;
      exit(121);
    }
  TEST_MESSAGE *tst_msg = (TEST_MESSAGE *) nml->get_address();
  long expected_last_var=strtol(argv[5],0,0);
  if(tst_msg->lastvar != expected_last_var)
    {
      fprintf(stderr,"!ERROR! expected_last_var(%ld) != tst_msg->last_var(%ld)\n",
	      expected_last_var, tst_msg->lastvar);
      delete nml;
      exit(120);
    }
  delete nml;
  exit(0);
}
