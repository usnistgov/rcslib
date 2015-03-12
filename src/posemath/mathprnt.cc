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
  Modification History:

  14-Apr-1997  FMP created from C++ portion of mathprnt.c
*/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#if !defined(HAVE_STD) && !defined(HAVE_IOSTREAM_H)
#define NO_IOSTREAM
#endif
#endif

#ifndef NO_IOSTREAM

#if HAVE_CONFIG_H
#ifdef HAVE_NAMESPACES
using namespace std;
#endif // HAVE_NAMESPACES
#ifdef HAVE_STD
#include <iostream>
#include <cstdio>
#else //HAVE_CONFIG_H
#include <iostream.h>
#endif // HAVE_STD
#else
#include <iostream.h>
#endif // HAVE_CONFIG_H

#include "posemath.h"
#include "mathprnt.h"



ostream & operator << (ostream & stream, PM_CARTESIAN v)
{
  stream << v.x << "\t" << v.y << "\t" << v.z ;

  return stream;
}

ostream & operator << (ostream & stream, PM_SPHERICAL s)
{
  stream << s.theta << "\t" << s.phi << "\t" << s.r ;

  return stream;
}

ostream & operator << (ostream & stream, PM_CYLINDRICAL c)
{
  stream << c.theta << "\t" << c.r << "\t" << c.z ;

  return stream;
}

ostream & operator << (ostream & stream, PM_QUATERNION q)
{
  stream << q.s << "\t" << q.x << "\t" << q.y << "\t" << q.z ;

  return stream;
}

ostream & operator << (ostream & stream, PM_ROTATION_VECTOR r)
{
  stream << r.s << "\t" << r.x << "\t" << r.y << "\t" << r.z ;

  return stream;
}

ostream & operator << (ostream & stream, PM_ROTATION_MATRIX m)
{
  int row, col;

  for (col = 0; col < 3; col++)
  {
    for (row = 0; row < 3; row++)
    {
      stream << m[row][col] << "\t";
    }
    stream << endl;
  }

  return stream;
}

ostream & operator << (ostream & stream, PM_EULER_ZYZ zyz)
{
  stream << zyz.z << "\t" << zyz.y << "\t" << zyz.zp ;

  return stream;
}

ostream & operator << (ostream & stream, PM_EULER_ZYX zyx)
{
  stream << zyx.z << "\t" << zyx.y << "\t" << zyx.x ;

  return stream;
}

ostream & operator << (ostream & stream, PM_RPY rpy)
{
  stream << rpy.r << "\t" << rpy.p << "\t" << rpy.y ;

  return stream;
}

ostream & operator << (ostream & stream, PM_XYA xya)
{
  stream << xya.x << "\t" << xya.y << "\t" << xya.a ;

  return stream;
}

ostream & operator << (ostream & stream, PM_POSE pose)
{
  stream << pose.tran << "\t" << pose.rot ;

  return stream;
}

ostream & operator << (ostream & stream, PM_HOMOGENEOUS hom)
{
  int row, col;

  for (col = 0; col < 4; col++)
  {
    for (row = 0; row < 4; row++)
    {
      stream << hom[row][col] << "\t";
    }
    stream << endl;
  }

  return stream;
}

 // NO_IOSTREAM not defined
#endif
