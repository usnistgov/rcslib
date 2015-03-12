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
  printf("new NML(...,%s,%s,%s) created.\n",
	 argv[1],argv[2],argv[3]);
  fflush(stdout);

  double timeout=strtod(argv[4],0);
  int t;
  int count = 0;
  char *rep_count_env = getenv("REP_COUNT");
  int rep_count=1;
  if(rep_count_env)
    {
      rep_count = strtol(rep_count_env,0,0);
    }
  while(count < rep_count)
    {
      double start=etime();
      if(TEST_MESSAGE_TYPE != (t = nml->blocking_read(timeout)))
	{
	  fprintf(stderr,"!ERROR! nml->blocking_read(%f) returned %d after %3.3fs\n",timeout, t,(etime()-start));
	  if(t != 0 || rep_count <= count+1)
	    {
	      delete nml;
	      exit(121);
	    }
	  else
	    {
	      count++;
	      continue;
	    }
	}
      fprintf(stderr,"INFO: nml->blocking_read(%f) returned %d after %3.3fs\n",
	      timeout,t,(etime()-start));
      TEST_MESSAGE *tst_msg = (TEST_MESSAGE *) nml->get_address();
      long expected_last_var=strtol(argv[5],0,0);
      if(tst_msg->first_count != tst_msg->last_count)
	{
	  fprintf(stderr,"tst_msg->first_count(%ld) != tst_msg->last_count(%ld)\n", tst_msg->first_count, tst_msg->last_count);
	  delete nml;
	  exit(120);
	}
      if(tst_msg->lastvar != expected_last_var)
	{
	  fprintf(stderr,"!ERROR! expected_last_var(%ld) != tst_msg->last_var(%ld)\n",
		  expected_last_var, tst_msg->lastvar);
	  delete nml;
	  exit(119);
	}
      count++;
    }
  delete nml;
  exit(0);
}
