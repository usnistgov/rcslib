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



#include "rcs.hh"
#include "nml_ex1.hh"

#include <stdio.h>

int
main(int , const char **)
{
  NML nml(ex_format,"ex1_buf","nml_ex1_read","ex1.nml");

  NMLTYPE t = nml.read();
  printf("read returned %ld\n",t);
  
  switch(t)
    {
    case 0:
      // Empty buffer
      break;

    case EXAMPLE_MSG_TYPE:
      {
	EXAMPLE_MSG *ex_msg = (EXAMPLE_MSG *) nml.get_address();
	printf("ex_msg->d = %f\n",ex_msg->d);
      }
      break;
      
    case -1:
      // Communicatios error
      break;
      
    default:
      // Some other unexpected type of message
      break;
    }
}
