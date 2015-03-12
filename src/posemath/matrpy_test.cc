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




#include "posemath.h"
#include "mathprnt.h"

using namespace std;
#include <iostream>

int
main(int argc, const char **argv)
{

  PM_RPY rpy1, rpy2;
  
  rpy1 = PM_RPY(-45 * TO_RAD, -90 *TO_RAD, 0);
  rpy2 = PM_ROTATION_MATRIX( rpy1 ); 

  cout << "rpy1=" << rpy1  << endl;
  cout << "rpy2=" << rpy2 << endl;
}


#if 0
/*
This is the email that prompted building this file for testing:

From Karl Murphy, 

Will-

Brian Womack at GDRS found a bug in _posemath.c in pmMatRpyConvert(). This occurs when pitch is -90 and the roll and yaw are non-zero.  To test this run.

  PM_RPY rpy1, rpy2;

  rpy1 = PM_RPY(-45 * TO_RAD, -90 *TO_RAD, 0);
  rpy2 = PM_ROTATION_MATRIX( rpy1 );

rpy2 is (0, -90, 0)  when it should be (-45, -90, 0)

Note that pitch= +- 90 are singularities and roll and pitch are not uniquely defined.  The code sets yaw to zero and roll to the sum (or difference) of the original values.


Below is the corrected code.

-karl



int pmMatRpyConvert(PmRotationMatrix m, PmRpy * rpy)
{
  rpy->p = atan2(-m.x.z, pmSqrt(pmSq(m.x.x) + pmSq(m.x.y)));

  if (fabs(rpy->p -PM_PI_2) < RPY_P_FUZZ)
  {
    rpy->r = atan2(m.y.x, m.y.y);
    rpy->p =PM_PI_2;            /* force it * - /
    rpy->y = 0.0;
  }
  else if (fabs(rpy->p +PM_PI_2) < RPY_P_FUZZ)
  {
    /************************************************
      bad code.  Replace this line...

       rpy->r = -atan2(m.y.z, m.y.y);

       with this line...
    ************************************************* - /
    rpy->r = -atan2(m.y.x, m.y.y);   /* new good code, uses m.y.x * - /



    rpy->p = -PM_PI_2;          /* force it * - /
    rpy->y = 0.0;
  }
  else
  {
    rpy->r = atan2(m.y.z, m.z.z);
    rpy->y = atan2(m.x.y, m.x.x);
  }

  return pmErrno = 0;
}
*/
#endif  


