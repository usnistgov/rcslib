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
#include <signal.h>


int 
main(int argc, char **argv)
{
  NML *nml = 0;
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);

  if(argc < 4)
    {
      fprintf(stderr,"usage : bufname procname configfile\n");
      exit(127);
    }

  nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  if(0 == nml)
    {
      exit(126);
    }
  if(!nml->valid())
    {
      fprintf(stderr,"nml->valid() check failed.\n");
      delete nml;
      exit(125);
    }
  TEST_MESSAGE tmsg;
  tmsg.d = 55.5;
  tmsg.s.d = 44.4;
  tmsg.da[2] = 33.3;
  tmsg.sa[1].d = 22.2;
  int bad_count =0;
  bool var_found = false;
  double d_from_msg = nml->get_double_var(&tmsg,"d",var_found);
  printf("d_from_msg=%f\n",d_from_msg);
  if(d_from_msg != tmsg.d || !var_found)
    {
      bad_count++;
    }
  var_found = false;
  d_from_msg = nml->get_double_var(&tmsg,"s.d",var_found);
  printf("d_from_msg=%f\n",d_from_msg);
  if(d_from_msg != tmsg.s.d || !var_found)
    {
      bad_count++;
    }
  var_found=false;
  d_from_msg = nml->get_double_var(&tmsg,"da[2]",var_found);
  printf("d_from_msg=%f\n",d_from_msg);
  if(d_from_msg != tmsg.da[2] || !var_found)
    {
      bad_count++;
    }
  var_found=false;
  d_from_msg = nml->get_double_var(&tmsg,"sa[1].d",var_found);
  printf("d_from_msg=%f\n",d_from_msg);
  if(d_from_msg != tmsg.sa[1].d || !var_found)
    {
      bad_count++;
    }
  delete nml;
  if(bad_count> 0)
    {
      exit(1);
    }
  exit(0);
}
