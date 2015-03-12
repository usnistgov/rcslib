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


/*
   testpmc.c

   Test code for C version of pose math library

   Modification history:

   10-Nov-1998  FMP braced const args to pose ctor; removed FILE *fp
   7-Feb-1997  FMP removed AXIS; mathQ -> pmQuat
   30-Jan-1997  FMP created
*/

#include <stdio.h>
#include "posemath.h"
#include "mathprnt.h"

#ifndef PI
#define PI PM_PI
#endif

#if 0
static void test_math_printf()
{
  VECTOR v = {1.0, 2.0, 3.0};
  double dbl = 3.14;
  QUATERNION quat = {1., 2., 3., 4.};
  MATRIX mat = { {1., 2., 3.},
                 {4., 5., 6.},
                 {7., 8., 9.} };
  POSE pose = { {1., 2., 3.}, {4., 5., 6., 7.}};
  char string[256];

  pmQuatNorm(quat, &quat);
  pmQuatNorm(pose.rot, &pose.rot);

  pmSprintf(string, "Hi there!");
  printf("%s\n", string);

  pmSprintf(string, "This is a real test:  vector = %4v \nfloat = %4f",
               v, dbl);
  printf("%s\n", string);
  pmPrintf("Hi this vector: %v\n%f", v, dbl);
  pmFprintf(stdout, " number = %f, vector = %v\n", dbl, v);
  pmPrintf("vector = %v \nformated vector = %6.3v\n", v, v);
  pmPrintf("quaternion = %q \nformated quaternion = %6.3q\n", quat, quat);
  pmPrintf("quaternion = %Q \nformated quaternion = %6.3Q\n", quat, quat);
  pmPrintf("matrix = %m \nformatted matrix = %6.3m\n", mat, mat);
  pmPrintf("pose = %p \nformatted pose = %6.3p\n", pose, pose);
  pmPrintf("Pose = %P \nFormatted Pose = %6.3P\n", pose, pose);
}
#endif


int testcase_pmrpyrot(PmRpy rpy)
{
  PmQuaternion q;
  PmRotationVector v1,v2;
  v1.x = v1.y = v1.z = v1.s = 0.0;
  v2.x = v2.y = v2.z = v2.s = 0.0;
  q.x = q.y = q.z = q.s = 0.0;
  

  printf("\n********************************\n");
  printf("rpy = (r = %f PI, p =  %f PI, y =  %f PI)\n",
         rpy.r/PI, rpy.p/PI, rpy.y/PI);
  pmRpyRotConvert(rpy,&v1);
  printf("direct convert : v1 = (s=%f, x= %f y=%f, z=%f)\n", v1.s, v1.x, v1.y,v1.z);
  pmRpyQuatConvert(rpy,&q);
  printf("quaternion intermediate : q = (s=%f, x= %f y=%f, z=%f)\n", q.s, q.x, q.y,q.z);
  pmQuatRotConvert(q,&v2);
  printf("convert via quat: v2 = (s=%f, x=%f, y=%f,  z=%f)\n", v2.s, v2.x, v2.y,v2.z);
  pmRotRpyConvert(v2,&rpy);
  printf("convert back direct: rpy = (r = %f PI, p =  %f PI, y =  %f PI)\n",
         rpy.r/PI, rpy.p/PI, rpy.y/PI);
  pmRotQuatConvert(v1,&q);
  pmQuatRpyConvert(q,&rpy);
  printf("convert back via quat: rpy = (r = %f PI, p =  %f PI, y =  %f PI)\n",
         rpy.r/PI, rpy.p/PI, rpy.y/PI);
  printf("********************************\n");
  return 0;
}

int test_pmrpyrot()
{
  PmRpy rpy;

  rpy.r = 0; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/4; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/4; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 0; rpy.y = PI/4;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/4; rpy.y = PI/4;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/4; rpy.p = PI/4; rpy.y = PI/4;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/4; rpy.p = PI/4; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/4; rpy.p = 0; rpy.y = PI/4;
  testcase_pmrpyrot(rpy);

  printf("\n\n+++++++++++++++++++++++++++++++++++++++++++\n\n");

  rpy.r = 0; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/3; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/3; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 0; rpy.y = PI/3;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/3; rpy.y = PI/3;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/3; rpy.p = PI/3; rpy.y = PI/3;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/3; rpy.p = PI/3; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/3; rpy.p = 0; rpy.y = PI/3;
  testcase_pmrpyrot(rpy);



  printf("\n\n+++++++++++++++++++++++++++++++++++++++++++\n\n");

  rpy.r = 0; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/2; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/2; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 0; rpy.y = PI/2;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = PI/2; rpy.y = PI/2;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/2; rpy.p = PI/2; rpy.y = PI/2;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/2; rpy.p = PI/2; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = PI/2; rpy.p = 0; rpy.y = PI/2;
  testcase_pmrpyrot(rpy);


  printf("\n\n+++++++++++++++++++++++++++++++++++++++++++\n\n");

  rpy.r = 0; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 2*PI; rpy.p = 0; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 2*PI; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 0; rpy.y = 2*PI;
  testcase_pmrpyrot(rpy);
  rpy.r = 0; rpy.p = 2*PI; rpy.y = 2*PI;
  testcase_pmrpyrot(rpy);
  rpy.r = 2*PI; rpy.p = 2*PI; rpy.y = 2*PI;
  testcase_pmrpyrot(rpy);
  rpy.r = 2*PI; rpy.p = 2*PI; rpy.y = 0;
  testcase_pmrpyrot(rpy);
  rpy.r = 2*PI; rpy.p = 0; rpy.y = 2*PI;
  testcase_pmrpyrot(rpy);

  return(0);
}

int fullcircle_test(PmRotationVector rv)
{
  PmRotationVector rv_out;
  PmEulerZyx zyx;
  double rotation_scaler;
  int deg;
  printf("rv: %f %f %f %f\n\n",rv.s,rv.x,rv.y,rv.z);

  for(deg = 0; deg <= 360; deg +=5)
    {
      rotation_scaler = deg*2*PI/360/rv.s;
      pmRotScalMult(rv,rotation_scaler, &rv_out);
      printf("rv_out: %f %f %f %f\n",rv_out.s, rv_out.x, rv_out.y, rv_out.z);
      pmRotZyxConvert(rv_out,&zyx);
      printf("zyx: %f %f %f\n",zyx.z,zyx.y, zyx.x);
    }
  return 0;
}


int main()
{
  /* test_math_printf(); */
  /* test_pmzyxrot(); */
  PmRotationVector rv;
  PmEulerZyx zyx;
  zyx.z = 0; zyx.y = PI/4; zyx.x = PI/4;
  pmZyxRotConvert(zyx,&rv);
  fullcircle_test(rv);

  return 0;
}
