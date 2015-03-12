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
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

/*
  testrcslib.cc

  Test program to exercise RCS Library functions. Returns 0 if OK,
  non-zero if not.
  
  $Author: shackle $
  $Revision: 430 $
  $Date: 2006-02-18 16:03:48 -0500 (Sat, 18 Feb 2006) $
*/

#include <stdio.h>		// fprintf(), stderr
#include "rcs.hh"		// RCS_CMD_CHANNEL, ...

#include "testcommon.cc"

int main(int argc, char * argv[])
{
  RCS_CMD_CHANNEL * cmdBuf;
  testInit initMsg;
  RCS_TIMER timer(1.0);
  int t;

  cmdBuf = new RCS_CMD_CHANNEL(testFormat, "cmd", "writer", "test.nml");
  if (! cmdBuf->valid()) {
    fprintf(stderr, "can't open cmd buffer as writer\n");
    return 1;
  }

  for (t = 0; t < 10; t++) {
    cmdBuf->write(initMsg);
    timer.wait();
  }
  
  delete cmdBuf;

  return 0;
}

/*
  Modification history:

  $Log$
  Revision 1.1  2005/09/12 20:47:53  proctor
  Initial checkin

*/
