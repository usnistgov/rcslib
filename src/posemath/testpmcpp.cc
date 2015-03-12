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
   testpmcpp.c

   Test code for C++ version of pose math library

   Modification history:

    7-Jun-1999 WPS ifdefed out some tests that caused ambiguous constructor
errors under RedHat Linux 6.0, The cause of this error was not understood. FIXME
   1-Sep-1998  FMP added testPose()
   31-Jan-1997  FMP changed unit to norm to reflect change in posemath;
   added assert stuff, quaternion
   29-Jan-1997  FMP adapted from C++ VECTOR code incorporated into
   posemath.c
*/

//#include "rcs_defs.hh"

#ifdef EXTERN_C_STD_HEADERS
extern "C" {
#endif
   
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>              // DBL_EPSILON

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#ifdef VXWORKS
#define NO_IOSTREAM
#endif

#ifdef WIN32
#define NO_IOSTREAM 1
#endif

#ifndef NO_IOSTREAM
using namespace std;
#include <iostream>
#endif

#include "posemath.h"
#include "mathprnt.h"

// error outputs-- assumes '==' works

static int numErrors = 0;
#define forceError() fprintf(stderr, "\tv--- forced error on line %d\n", __LINE__)
#define testAssert(cond) ((cond) ? 1 : (fprintf(stderr, "error on line %d\n", __LINE__),abort(),0))

// formatted output
static void testPrint()
{
  PM_CARTESIAN v(1.0, 2.0, 3.0);
  PM_QUATERNION quat(1., 2., 3., 4.);
  PM_ROTATION_MATRIX mat(1., 2., 3.,
             4., 5., 6.,
             7., 8., 9.);
  PM_POSE pose(1., 2., 3., 4., 5., 6., 7.);

  quat = norm(quat);
  pose.rot = norm(pose.rot);

#ifndef NO_IOSTREAM
  cout << "vector = " << v << endl;
  cout << "quat = " << quat << endl;
  cout << "mat = " << endl << mat;
  cout << "pose = " << pose << endl;
#endif
}

// PM_CARTESIAN tests
static void testCart()
{
  double d;
  PM_CARTESIAN v1(1, 2, 3);
  PM_CARTESIAN v2(1, 2, 3);
  PM_CARTESIAN *pv;

  // test arg ctor
  testAssert(v1 == v2);

  // test new arg ctor
  pv = new PM_CARTESIAN(4, 5, 6);
  testAssert(PM_CARTESIAN(4, 5, 6) == *pv);

  // test indexing
  d = v2[-1];                   // ERROR
  d = v2[3];                    // ERROR
  d = v2[2];
  v2[2] = v2[1];
  v2[1] = v2[0];
  v2[0] = d;
  testAssert(PM_CARTESIAN(3, 1, 2) == v2);

  // test assignment
  v1 = PM_CARTESIAN(0, 0, 0);
  *pv = PM_CARTESIAN(0, 0, 0);
  *pv = v1 = v2;
  testAssert(PM_CARTESIAN(3, 1, 2) == v1);
  testAssert(PM_CARTESIAN(3, 1, 2) == v2);
  testAssert(PM_CARTESIAN(3, 1, 2) == *pv);

  // test unary operators
  *pv = -v1;
  testAssert(PM_CARTESIAN(3, 1, 2) == v1);
  testAssert(PM_CARTESIAN(-3, -1, -2) == *pv);
  *pv = +v1;
  testAssert(PM_CARTESIAN(3, 1, 2) == v1);
  testAssert(PM_CARTESIAN(3, 1, 2) == *pv);

  // test +/-
  v1 = v1 + v2;
  testAssert(PM_CARTESIAN(6, 2, 4) == v1);
  testAssert(PM_CARTESIAN(3, 1, 2) == v2);
  v2 = v2 - v1;
  testAssert(PM_CARTESIAN(6, 2, 4) == v1);
  testAssert(PM_CARTESIAN(-3, -1, -2) == v2);

  // test scalar *, /
  v1 = v1 / 2;
  testAssert(PM_CARTESIAN(3, 1, 2) == v1);
  v2 = v2 * 2;
  testAssert(PM_CARTESIAN(-6, -2, -4) == v2);

  // test ==
  v1 = v2;
  testAssert(v1 == v2);
  v1 = v1 + PM_CARTESIAN(1, 1, 1) * V_FUZZ * 0.99;
  testAssert(v1 == v2);
  v1 = v2;
  v1 = v1 + PM_CARTESIAN(1, 1, 1) * V_FUZZ * 1.01;
  testAssert(v1 != v2);

  // test dot
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(4, 5, 6);
  testAssert(32 == dot(v1, v2));

  // test cross
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(4, 5, 6);
  testAssert(PM_CARTESIAN(-3, 6, -3) == cross(v1, v2));

  // test mag
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(4, 5, 6);
  testAssert(sqrt((double) 14.0) == mag(v1));

  // test norm
  v1 = unit(PM_CARTESIAN(1, 2, 3));
  testAssert(1.0 == mag(v1));

  // test disp
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(4, 5, 6);
  testAssert(sqrt((double)27.0) == disp(v1, v2));

  // test inv
  v1 = PM_CARTESIAN(1, 1, 1);
#if 0
  // g++/gcc versions 2.8.x and 2.9.x
  // will complain that the call to PM_CARTESIAN(PM_CARTESIAN) is
  // ambigous. (2.7.x and some others allow it)
  testAssert(1 == dot(v1, inv(v1)));
#endif

  // test delete
  delete pv;
}

static void testCyl()
{
  PM_CYLINDRICAL c1;
  PM_CYLINDRICAL c2(PM_PI_2, 2, 3);
  PM_CYLINDRICAL *pv;
  PM_CARTESIAN v1, v2;

  // test arg ctor
  testAssert(PM_CYLINDRICAL(PM_PI_2, 2, 3) == c2);

  // test new arg ctor
  pv = new PM_CYLINDRICAL(PM_PI_4, 5, 6);
  testAssert(PM_CYLINDRICAL(PM_PI_4, 5, 6) == *pv);

  // test assignment
  c1 = PM_CYLINDRICAL(PM_PI_2, 1, 2);
  c2 = PM_CYLINDRICAL(0, 0, 0);
  *pv = PM_CYLINDRICAL(0, 0, 0);
  *pv = c2 = c1;
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == c1);
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == c2);
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == *pv);

  // test unary operators
  *pv = -c1;
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == c1);
  testAssert(PM_CYLINDRICAL(-PM_PI_2, 1, -2) == *pv);
  *pv = +c1;
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == c1);
  testAssert(PM_CYLINDRICAL(PM_PI_2, 1, 2) == *pv);

  // test +/-
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(4, 5, 6);

  c1 = v1;
  c2 = v2;

  v1 = v1 + v2;
  c1 = c1 + c2;

  testAssert(PM_CARTESIAN(c1) == v1);
  testAssert(c1 == PM_CYLINDRICAL(v1));

  // test scalar *, /
  c1 = PM_CYLINDRICAL(-PM_PI_4, -2, 3);
  c2 = c1 / 2.0;
  testAssert(PM_CYLINDRICAL(-PM_PI_4, -1, 1.5) == c2);
  c2 = c1 * 2.0;
  testAssert(PM_CYLINDRICAL(-PM_PI_4, -4, 6) == c2);

  // test dot
#ifndef linux_2_2_5
  v1 = c1 = PM_CYLINDRICAL(1, 2, 3);
  v2 = c2 = PM_CYLINDRICAL(4, 5, 6);
  testAssert(dot(c1, c2) == dot(v1, v2));

  // test cross
  v1 = c1 = PM_CYLINDRICAL(1, 2, 3);
  v2 = c2 = PM_CYLINDRICAL(4, 5, 6);
  testAssert(cross(c1, c2) == cross(v1, v2));

  // test mag
  c1 = PM_CYLINDRICAL(1, 2, 3);
  testAssert(sqrt((double) 13.0) == mag(c1));

  // test norm
  //  c1 = norm(PM_CYLINDRICAL(1, 2, 3));
  // testAssert(1.0 == mag(c1));

  // test inv
  c1 = PM_CYLINDRICAL(1, 1, 1);
#if 0
  // g++/gcc versions 2.8.x and 2.9.x
  // will complain that the call to PM_CARTESIAN(PM_CARTESIAN) is
  // ambigous. (2.7.x and some others allow it)
  testAssert(1 == dot(c1, inv(c1)));
#endif
#endif

  // test delete
  delete pv;
}

static void testQuat()
{
  double d;
  PM_CARTESIAN v1;
  PM_CARTESIAN v2;
  PM_QUATERNION q1;
  PM_QUATERNION q2(1, 0, 0, 0);
  PM_QUATERNION *pq;

  // test arg ctor
  testAssert(PM_QUATERNION(1, 0, 0, 0) == q2);

  // test new arg ctor
  pq = new PM_QUATERNION(1, 0, 0, 0);
  testAssert(PM_QUATERNION(1, 0, 0, 0) == *pq);

  // test indexing
  d = q2[-1];                   // ERROR
  d = q2[4];                    // ERROR
  q1[0] = q2[0];
  q1[1] = q2[1];
  q1[2] = q2[2];
  q1[3] = q2[3];
  testAssert(q1 == q2);

  // test assignment
  q1 = PM_QUATERNION(1, 0, 0, 0);
  q2 = PM_QUATERNION(2, 3, 4, 5);
  *pq = PM_QUATERNION(6, 7, 8, 9);
  *pq = q2 = q1;
  testAssert(PM_QUATERNION(1, 0, 0, 0) == q1);
  testAssert(PM_QUATERNION(1, 0, 0, 0) == q2);
  testAssert(PM_QUATERNION(1, 0, 0, 0) == *pq);

  // test rotation vector assignment
  /*                                 .
   quaternion buffs will know that 90  rotations about X, Y, and Z
   axes are
   (sqrt(2)/2, sqrt(2)/2,         0,         0),
   (sqrt(2)/2,         0, sqrt(2)/2,         0), and
   (sqrt(2)/2,         0,         0, sqrt(2)/2), respectively.
   */
  d = sqrt(2.0) / 2.0;
  q1 = PM_ROTATION_VECTOR(PM_PI_2, 1, 0, 0);
  testAssert(PM_QUATERNION(d, d, 0, 0) == q1);
  q1 = PM_ROTATION_VECTOR(PM_PI_2, 0, 1, 0);
  testAssert(PM_QUATERNION(d, 0, d, 0) == q1);
  q1 = PM_ROTATION_VECTOR(PM_PI_2, 0, 0, 1);
  testAssert(PM_QUATERNION(d, 0, 0, d) == q1);

  // test unary operators
  q1 = PM_ROTATION_VECTOR(PM_PI_2, 1, 1, 1);
  q2 = +q1;
  testAssert(q1 == q2);
  q2 = -q1;
  testAssert(q1 != q2);
  q2 = -q2;
  testAssert(q1 == q2);

  // test Q * Q and Q * V
  v1 = PM_CARTESIAN(1, 2, 3);
  v2 = PM_CARTESIAN(1, 2, 3);
  q1 = PM_ROTATION_VECTOR(PM_PI_2, 0.5, 1, 1);
  q2 = PM_ROTATION_VECTOR(PM_PI_2, 1, 0.5, 1);
  *pq = PM_ROTATION_VECTOR(PM_PI_2, 1, 1, 0.5);
  v1 = q1 * v1;
  v1 = q2 * v1;
  v1 = *pq * v1;
  v2 = *pq * q2 * q1 * v2;
  testAssert(v1 == v2);

  // test scaling
  q1 = PM_ROTATION_VECTOR(PM_PI_2,       1, -1, 0.5);
  q2 = PM_ROTATION_VECTOR(PM_PI_2 / 2.0, 1, -1, 0.5);
  q1 = q1 / 2.0;
  testAssert(q1 == q2);
  q1 = PM_ROTATION_VECTOR(PM_PI_2,       1, -1, 0.5);
  q2 = PM_ROTATION_VECTOR(PM_PI_2 * 2.0, 1, -1, 0.5);
  q1 = q1 * 2.0;
  testAssert(q1 == q2);

  // test norm, isNorm
  q1.s = 1;
  q1.x = 2;
  q1.y = 3;
  q1.z = 4;
  testAssert(! isNorm(q1));
  q1 = norm(q1);
  testAssert(isNorm(q1));

  // test inv
  q1 = PM_QUATERNION(2, 3, 4, 5);
  q2 = inv(q1);
  testAssert(q1 * q2 == PM_QUATERNION(1, 0, 0, 0));

  // test delete
  delete pq;
}

void testMat()
{
  PM_ROTATION_MATRIX m1(PM_ROTATION_VECTOR(PM_PI_2, 1, 2, 3));
  PM_ROTATION_MATRIX m2 = PM_ROTATION_VECTOR(-PM_PI_4, 2, 0, -1);
  PM_ROTATION_MATRIX m3;
  PM_QUATERNION q1(PM_ROTATION_VECTOR(PM_PI_2, 1, 2, 3));
  PM_QUATERNION q2 = PM_ROTATION_VECTOR(-PM_PI_4, 2, 0, -1);
  PM_QUATERNION q3;


  // use implicit QQ mult
  m3 = m1 * m2;

  // use explicit QQ mult
  q3 = q1 * q2;

  // should be equal
  testAssert(q3 == m3);

  // set them equal via conversion
  q1 = m3;
  testAssert(q1 == q3);
}

void testHom()
{
  PM_HOMOGENEOUS h1, h2;
  PM_POSE p1;

  p1 = PM_POSE(PM_CARTESIAN(1, 2, 3), PM_ROTATION_MATRIX(PM_ROTATION_VECTOR(PM_PI_2, 1, 2, 3)));
  h1 = p1;
  testAssert(p1 == h1);

  h2 = -h1;
  testAssert(h1 * h2 == PM_POSE(0, 0, 0, 1, 0, 0, 0));
}

void testPose()
{
  PM_POSE p;
  PM_CARTESIAN v, v1, v2;

  p = PM_POSE(PM_CARTESIAN(1, 2, 3), PM_ROTATION_MATRIX(PM_ROTATION_VECTOR(PM_PI_2, 1, 2, 3)));
  v = PM_CARTESIAN(1, 1, 2);

  v1 = p * v;
  v2 = p.rot * v + p.tran;

  testAssert(v1 == v2);
}

void testOther()
{
  PM_RPY rpy1, rpy2;
  PM_EULER_ZYZ zyz1, zyz2;
  PM_EULER_ZYX zyx1, zyx2;
  PM_ROTATION_VECTOR v1;
  PM_QUATERNION q1, q2;

  v1 = PM_ROTATION_VECTOR(PM_PI_2, 1, 2, 3);

  rpy1 = PM_ROTATION_MATRIX(v1);
  zyz1 = PM_ROTATION_MATRIX(v1);
  zyx1 = PM_ROTATION_MATRIX(v1);
  rpy2 = PM_ROTATION_MATRIX(v1);
  zyz2 = PM_ROTATION_MATRIX(v1);
  zyx2 = PM_ROTATION_MATRIX(v1);

  testAssert(rpy1 == rpy2);
  testAssert(zyz1 == zyz2);
  testAssert(zyx1 == zyx2);

  rpy2 = - rpy1;
  zyz2 = - zyz1;
  zyx2 = - zyx1;

  testAssert(rpy1 != rpy2);
  testAssert(zyz1 != zyz2);
  testAssert(zyx1 != zyx2);

  q1 = rpy1;
  q2 = zyz2;
  testAssert(q1 * q2 == PM_QUATERNION(1, 0, 0, 0));

  q1 = rpy1;
  q2 = zyx2;
  testAssert(q1 * q2 == PM_QUATERNION(1, 0, 0, 0));

  // FIXME-- need assignment operators for all types
  // zyz1 = zyx1;

  // just check that linking is ok.
  PmQuaternion q;
  PmAxis a = PM_X;
  double angle = PM_PI/12;
  pmAxisAngleQuatConvert(a,angle,&q);

  PM_XYA xya1,xya2,xyap,xya2i,xya1r;
  PM_POSE p1,p2,pp,p2i,p1r;
  const double TEST_INC = 0.45;
  int tests_count = 0;
  for( xya1.x = -2.0; xya1.x < 2.0; xya1.x += TEST_INC ) {
    for( xya1.y = -2.0; xya1.y < 2.0; xya1.y += TEST_INC ) {
      for( xya1.a = -3*PM_PI; xya1.a < 3*PM_PI; xya1.a += TEST_INC*PM_PI ) {
	for( xya2.x = -2.0; xya2.x < 2.0; xya2.x += TEST_INC ) {
	  for( xya2.y = -2.0; xya2.y < 2.0; xya2.y += TEST_INC ) {
	    for( xya2.a = -3*PM_PI; xya2.a < 3*PM_PI; xya2.a += TEST_INC*PM_PI ) {

	      tests_count++;
	      if(tests_count % 10 == 0) {
		printf("%09d \r",tests_count);
	      }
	      //cout << "xya1=" << xya1 << endl;
	      //cout << "xya2=" << xya2 << endl;
	      
	      p1 = PM_POSE(xya1);
	      p2 = PM_POSE(xya2);
	      //cout << "p1=" << p1 << endl;
	      //cout << "p2=" << p2 << endl;
	      pp = p1*p2;
	      xyap = xya1*xya2;
	      //cout << "pp = " << pp << endl;
	      //cout << "xyap = " << xyap << endl; 
	      testAssert( pp == PM_POSE(xyap));
	      xya2i = inv(xya2);
	      //cout << "xya2i = " << xya2i << endl;
	      p2i = inv(p2);
	      //cout << "p2i = " << p2i << endl;
	      testAssert( p2i == PM_POSE(xya2i));
	      p1r = pp*p2i;
	      //cout << "p1r = " << p1r << endl;
	      testAssert(p1 == p1r);
	      xya1r = xyap*xya2i;
	      //cout << "xya1r = " << xya1r << endl;
	      testAssert( xya1 == xyap*xya2i);
	    }
	  }
	}
      }
    }
  }
}

// test loose namespace
typedef struct
{
  double x, y, z;
} VECTOR;


#ifdef VXWORKS
extern "C" int testpmcpp();


int testpmcpp()
#else

int main()
#endif
{
  testPrint();
  testCart();
  testQuat();
  testMat();
  testCyl();
  testHom();
  testPose();
  testOther();

  if (0 != numErrors)
  {
    fprintf(stderr, "%d errors\n", numErrors);
    exit(1);
  }
  else
  {
    exit(0);
  }
}
