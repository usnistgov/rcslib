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
main(int , char **)
{
  printf("sizeof(struct _teststruct)=%lu (0x%lX)\n",
	 sizeof(struct _teststruct),
	 sizeof(struct _teststruct));
  printf("sizeof(TEST_MESSAGE)=%lu (0x%lX)\n",
	 sizeof(TEST_MESSAGE),
	 sizeof(TEST_MESSAGE));
}
