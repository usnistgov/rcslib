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


#include <stdio.h>
#include "posemath.h"

/*
  testcirc.c

  Circle test code

  Modification history:

  10-Nov-1998  FMP took out unused angle and length vars
*/

int main()
{
  double incr;

  PmCircle circle;
  PmPose start, end;
  PmCartesian center, normal;
  int turn;
  PmPose out;

  printf("start: ");
  fflush(stdout);
  scanf("%lf %lf %lf", &start.tran.x, &start.tran.y, &start.tran.z);
  start.rot.s = 1.0;
  start.rot.x = 0.0;
  start.rot.y = 0.0;
  start.rot.z = 0.0;

  printf("end: ");
  fflush(stdout);
  scanf("%lf %lf %lf", &end.tran.x, &end.tran.y, &end.tran.z);
  end.rot.s = 1.0;
  end.rot.x = 0.0;
  end.rot.y = 0.0;
  end.rot.z = 0.0;

  printf("center: ");
  fflush(stdout);
  scanf("%lf %lf %lf", &center.x, &center.y, &center.z);

  printf("normal: ");
  fflush(stdout);
  scanf("%lf %lf %lf", &normal.x, &normal.y, &normal.z);

  printf("turns: ");
  fflush(stdout);
  scanf("%d", &turn);

  pmCircleInit(&circle, start, end, center, normal, turn);

  printf("radius: %f\n", circle.radius);
  printf("angle: %f\n", circle.angle);
  printf("spiral: %f\n", circle.spiral);
  printf("rTan: %f %f %f\n", circle.rTan.x, circle.rTan.y, circle.rTan.z);
  printf("rPerp: %f %f %f\n", circle.rPerp.x, circle.rPerp.y, circle.rPerp.z);
  printf("rHelix: %f %f %f\n", circle.rHelix.x, circle.rHelix.y, circle.rHelix.z);
  return 0;

  if (turn >= 0)
    {
      for (incr = 0.0; incr <= circle.angle; incr += circle.angle / 100.0)
        {
          pmCirclePoint(&circle, incr, &out);
          printf("%f %f %f\n", out.tran.x, out.tran.y, out.tran.z);
        }
    }
  else
    {
      for (incr = 0.0; incr >= circle.angle; incr += circle.angle / 100.0)
        {
          pmCirclePoint(&circle, incr, &out);
          printf("%f %f %f\n", out.tran.x, out.tran.y, out.tran.z);
        }
    }

  return 0;
}
