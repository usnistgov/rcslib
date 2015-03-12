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
  posemath.cc

  C++ definitions for pose math library data types and
  manipulation functions.
*/

#include "posemath.h"
#include <math.h>

#ifdef PM_PRINT_ERROR
#define PM_DEBUG                // need debug with printing
#endif

// place to reference arrays when bounds are exceeded
static double noElement = 0.0;
static PM_CARTESIAN *noCart = 0;

//

// PM_CARTESIAN class

#if 0
PM_CARTESIAN::PM_CARTESIAN(double _x, double _y, double _z):
  x(_x),y(_y),z(_z)
{
  x = _x;
  y = _y;
  z = _z;
}
#endif

PM_CARTESIAN::PM_CARTESIAN(PM_CONST  PM_CYLINDRICAL PM_REF c):
  x(0.0),y(0.0),z(0.0)
{
  PmCylindrical cyl;
  PmCartesian cart;

  toCyl(c, &cyl);
  pmCylCartConvert(cyl, &cart);
  toCart(cart, this);
}

PM_CARTESIAN::PM_CARTESIAN(PM_CONST PM_SPHERICAL PM_REF s):
  x(0.0),y(0.0),z(0.0)
{
  PmSpherical sph;
  PmCartesian cart;

  toSph(s, &sph);
  pmSphCartConvert(sph, &cart);
  toCart(cart, this);
}

double & PM_CARTESIAN::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return x;
  case 1:
    return y;
  case 2:
    return z;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
#if 0
PM_CARTESIAN::PM_CARTESIAN(PM_CCONST PM_CARTESIAN &v):
  x(v.x),y(v.y),z(v.z)
{
  x = v.x;
  y = v.y;
  z = v.z;
}
#endif
#endif


#if 0
PM_CARTESIAN &
PM_CARTESIAN::operator = (PM_CONST PM_CARTESIAN &v)
{
  x = v.x;
  y = v.y;
  z = v.z;

  return (*this);
}
#endif

// PM_SPHERICAL

PM_SPHERICAL::PM_SPHERICAL(double _theta, double _phi, double _r):
  theta(_theta),phi(_phi),r(_r)
{
  theta = _theta;
  phi = _phi;
  r = _r;
}

PM_SPHERICAL::PM_SPHERICAL(PM_CONST PM_CARTESIAN PM_REF v):
  theta(0.0),phi(0.0),r(0.0)
{
  PmCartesian cart;
  PmSpherical sph;

  toCart(v, &cart);
  pmCartSphConvert(cart, &sph);
  toSph(sph, this);
}

PM_SPHERICAL::PM_SPHERICAL(PM_CONST PM_CYLINDRICAL PM_REF c):
  theta(0.0),phi(0.0),r(0.0)
{
  PmCylindrical cyl;
  PmSpherical sph;

  toCyl(c, &cyl);
  pmCylSphConvert(cyl, &sph);
  toSph(sph, this);
}

double & PM_SPHERICAL::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return theta;
  case 1:
    return phi;
  case 2:
    return r;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_SPHERICAL::PM_SPHERICAL(PM_CCONST PM_SPHERICAL &s):
  theta(s.theta),phi(s.phi),r(s.r)
{
  /*  theta = s.theta;
  phi = s.phi;
  r = s.r; */
}
#endif


PM_SPHERICAL PM_SPHERICAL::operator = (PM_SPHERICAL s)
{
  theta = s.theta;
  phi = s.phi;
  r = s.r;

  return (*this);
}

// PM_CYLINDRICAL

PM_CYLINDRICAL::PM_CYLINDRICAL(double _theta, double _r, double _z):
  theta(_theta),r(_r),z(_z)
{
  /*
  theta = _theta;
  r = _r;
  z = _z; 
  */
}

PM_CYLINDRICAL::PM_CYLINDRICAL(PM_CONST PM_CARTESIAN PM_REF v):
  theta(0.0),r(0.0),z(0.0)
{
  PmCartesian cart;
  PmCylindrical cyl;

  toCart(v, &cart);
  pmCartCylConvert(cart, &cyl);
  toCyl(cyl, this);
}



PM_CYLINDRICAL::PM_CYLINDRICAL(PM_CONST PM_SPHERICAL PM_REF s):
  theta(0.0),r(0.0),z(0.0)
{
  PmSpherical sph;
  PmCylindrical cyl;

  toSph(s, &sph);
  pmSphCylConvert(sph, &cyl);
  toCyl(cyl, this);
}

double & PM_CYLINDRICAL::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return theta;
  case 1:
    return r;
  case 2:
    return z;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_CYLINDRICAL::PM_CYLINDRICAL(PM_CCONST PM_CYLINDRICAL &c):
  theta(c.theta),r(c.r),z(c.z)
{
  /*
  theta = c.theta;
  r = c.r;
  z = c.z;
  */
}
#endif

PM_CYLINDRICAL PM_CYLINDRICAL::operator = (PM_CYLINDRICAL c)
{
  theta = c.theta;
  r = c.r;
  z = c.z;

  return (*this);
}


// PM_ROTATION_VECTOR

PM_ROTATION_VECTOR::PM_ROTATION_VECTOR(double _s, double _x,
                                       double _y, double _z):
  s(_s),x(_x),y(_y),z(_z)
{
  PmRotationVector rv;

  rv.s = _s;
  rv.x = _x;
  rv.y = _y;
  rv.z = _z;

  pmRotNorm(rv, &rv);
  toRot(rv, this);
}

PM_ROTATION_VECTOR::PM_ROTATION_VECTOR(PM_CONST PM_QUATERNION PM_REF q):
  s(0.0),x(0.0),y(0.0),z(0.0)
{
  PmQuaternion quat;
  PmRotationVector rv;

  toQuat(q, &quat);
  pmQuatRotConvert(quat, &rv);
  toRot(rv, this);
}

double & PM_ROTATION_VECTOR::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return s;
  case 1:
    return x;
  case 2:
    return y;
  case 3:
    return z;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_ROTATION_VECTOR::PM_ROTATION_VECTOR(PM_CCONST PM_ROTATION_VECTOR &r):
  s(r.s),x(r.x),y(r.y),z(r.z)
{
  /* 
     s = r.s;
     x = r.x;
     y = r.y;
     z = r.z;
  */
}
#endif

PM_ROTATION_VECTOR PM_ROTATION_VECTOR::operator = (PM_ROTATION_VECTOR r)
{
  s = r.s;
  x = r.x;
  y = r.y;
  z = r.z;

  return (*this);
}

// PM_ROTATION_MATRIX class

// ctors/dtors

/*
  A PM_ROTATION_MATRIX's elements are viewed in textbook matrix form as: 

  | x.x y.x z.x |
  | x.y y.y z.y |
  | x.z y.z z.z |

  In the 9-argument constructor, the first 3 are the x unit vector components,
  the second 3 are the y unit vector components, and the third 3 are the
  z unit vector components.
*/

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX
(double xx, double xy, double xz, // x unit vector
 double yx, double yy, double yz, // y unit vector
 double zx, double zy, double zz) : // z unit vector
  x(xx,xy,xz), y(yx,yy,yz), z(zx,zy,zz)
{
  x.x = xx;
  x.y = xy;
  x.z = xz;

  y.x = yx;
  y.y = yy;
  y.z = yz;

  z.x = zx;
  z.y = zy;
  z.z = zz;

  norm(*this);
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CARTESIAN _x, PM_CARTESIAN _y, PM_CARTESIAN _z):
  x(_x),y(_y),z(_z)
{
  /*
    x = _x;
    y = _y;
    z = _z;
  */
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CONST PM_ROTATION_VECTOR PM_REF v):
  x(),y(),z()
{
  PmRotationVector rv;
  PmRotationMatrix mat;

  toRot(v, &rv);
  pmRotMatConvert(rv, &mat);
  toMat(mat, this);
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CONST PM_QUATERNION PM_REF q):
  x(),y(),z()
{
  PmQuaternion quat;
  PmRotationMatrix mat;

  toQuat(q, &quat);
  pmQuatMatConvert(quat, &mat);
  toMat(mat, this);
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CONST PM_RPY PM_REF rpy):
    x(0,0,0),y(0,0,0),z(0,0,0)
{
  PmRpy _rpy;
  PmRotationMatrix mat;

  toRpy(rpy, &_rpy);
  pmRpyMatConvert(_rpy, &mat);
  toMat(mat, this);
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CONST PM_EULER_ZYZ PM_REF zyz):
  x(0,0,0),y(0,0,0),z(0,0,0)
{
  PmEulerZyz _zyz;
  PmRotationMatrix mat;

  toEulerZyz(zyz, &_zyz);
  pmZyzMatConvert(_zyz, &mat);
  toMat(mat, this);
}

PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CONST PM_EULER_ZYX PM_REF zyx):
  x(0,0,0),y(0,0,0),z(0,0,0)
{
  PmEulerZyx _zyx;
  PmRotationMatrix mat;

  toEulerZyx(zyx, &_zyx);
  pmZyxMatConvert(_zyx, &mat);
  toMat(mat, this);
}

// operators

PM_CARTESIAN & PM_ROTATION_MATRIX::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return x;
  case 1:
    return y;
  case 2:
    return z;
  default:
    if(0 == noCart)
      {
        noCart = new PM_CARTESIAN(0.0,0.0,0.0);
      }
    return (*noCart);           // need to return a PM_CARTESIAN &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_ROTATION_MATRIX::PM_ROTATION_MATRIX(PM_CCONST PM_ROTATION_MATRIX &m):
  x(m.x),y(m.y),z(m.z)
{
  /*
  x = m.x;
  y = m.y;
  z = m.z;
  */
}
#endif

PM_ROTATION_MATRIX PM_ROTATION_MATRIX::operator = (PM_ROTATION_MATRIX m)
{
  x = m.x;
  y = m.y;
  z = m.z;

  return (*this);
}

// PM_QUATERNION class

PM_QUATERNION::PM_QUATERNION(double _s, double _x, double _y, double _z):
  s(_s),x(_x),y(_y),z(_z)
{
  PmQuaternion quat;

  quat.s = _s;
  quat.x = _x;
  quat.y = _y;
  quat.z = _z;

  pmQuatNorm(quat, &quat);

  s = quat.s;
  x = quat.x;
  y = quat.y;
  z = quat.z;
}


PM_QUATERNION::PM_QUATERNION(PM_CONST PM_ROTATION_VECTOR PM_REF v):
  s(0),x(0),y(0),z(0)
{
  PmRotationVector rv;
  PmQuaternion quat;

  toRot(v, &rv);
  pmRotQuatConvert(rv, &quat);
  toQuat(quat, this);
}

PM_QUATERNION::PM_QUATERNION(PM_CONST PM_ROTATION_MATRIX PM_REF m):
  s(0),x(0),y(0),z(0)
{
  PmRotationMatrix mat;
  PmQuaternion quat;

  toMat(m, &mat);
  pmMatQuatConvert(mat, &quat);
  toQuat(quat, this);
}

PM_QUATERNION::PM_QUATERNION(PM_CONST PM_EULER_ZYZ PM_REF zyz):
  s(0),x(0),y(0),z(0)
{
  PmEulerZyz _zyz;
  PmQuaternion quat;

  toEulerZyz(zyz, &_zyz);
  pmZyzQuatConvert(_zyz, &quat);
  toQuat(quat, this);
}

PM_QUATERNION::PM_QUATERNION(PM_CONST PM_EULER_ZYX PM_REF zyx):
  s(0),x(0),y(0),z(0)
{
  PmEulerZyx _zyx;
  PmQuaternion quat;

  toEulerZyx(zyx, &_zyx);
  pmZyxQuatConvert(_zyx, &quat);
  toQuat(quat, this);
}

PM_QUATERNION::PM_QUATERNION(PM_CONST PM_RPY PM_REF rpy):
  s(0),x(0),y(0),z(0)
{
  PmRpy _rpy;
  PmQuaternion quat;

  toRpy(rpy, &_rpy);
  pmRpyQuatConvert(_rpy, &quat);
  toQuat(quat, this);
}


PM_QUATERNION::PM_QUATERNION( PM_AXIS _axis,  double _angle):
  s(0),x(0),y(0),z(0)
{
  PmQuaternion quat;

  pmAxisAngleQuatConvert((PmAxis) _axis,_angle, &quat);
  toQuat(quat, this);
}


void PM_QUATERNION::axisAngleMult( PM_AXIS _axis,  double _angle)
{
  PmQuaternion quat;

  toQuat((*this), &quat);
  pmQuatAxisAngleMult(quat, (PmAxis) _axis,_angle, &quat);
  toQuat(quat, this);
}


double & PM_QUATERNION::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return s;
  case 1:
    return x;
  case 2:
    return y;
  case 3:
    return z;
  default:
    return noElement;           // need to return a double &
  }
}

#if 0
PM_QUATERNION PM_QUATERNION::operator = (PM_QUATERNION q)
{
  s = q.s;
  x = q.x;
  y = q.y;
  z = q.z;

  return (*this);
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_QUATERNION::PM_QUATERNION(PM_CCONST PM_QUATERNION &q):
  s(0),x(0),y(0),z(0)
{
  s = q.s;
  x = q.x;
  y = q.y;
  z = q.z;
}
#endif
#endif

// PM_EULER_ZYZ class

PM_EULER_ZYZ::PM_EULER_ZYZ(double _z, double _y, double _zp):
  z(_z),y(_y),zp(_zp)
{
  z = _z;
  y = _y;
  zp = _zp;
}

PM_EULER_ZYZ::PM_EULER_ZYZ(PM_CONST PM_QUATERNION PM_REF q):
  z(0),y(0),zp(0)
{
  PmQuaternion quat;
  PmEulerZyz zyz;

  toQuat(q, &quat);
  pmQuatZyzConvert(quat, &zyz);
  toEulerZyz(zyz, this);
}

PM_EULER_ZYZ::PM_EULER_ZYZ(PM_CONST PM_ROTATION_MATRIX PM_REF m):
    z(0),y(0),zp(0)
{
  PmRotationMatrix mat;
  PmEulerZyz zyz;

  toMat(m, &mat);
  pmMatZyzConvert(mat, &zyz);
  toEulerZyz(zyz, this);
}

double & PM_EULER_ZYZ::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return z;
  case 1:
    return y;
  case 2:
    return zp;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_EULER_ZYZ::PM_EULER_ZYZ(PM_CCONST PM_EULER_ZYZ &zyz):
    z(0),y(0),zp(0)
{
  z = zyz.z;
  y = zyz.y;
  zp = zyz.zp;
}
#endif

PM_EULER_ZYZ PM_EULER_ZYZ::operator = (PM_EULER_ZYZ zyz)
{
  z = zyz.z;
  y = zyz.y;
  zp = zyz.zp;

  return (*this);
}

// PM_EULER_ZYX class

PM_EULER_ZYX::PM_EULER_ZYX(double _z, double _y, double _x):
  z(_z),y(_y),x(_x)
{
  /*
  z = _z;
  y = _y;
  x = _x;
  */
}

PM_EULER_ZYX::PM_EULER_ZYX(PM_CONST PM_QUATERNION PM_REF q):
  z(0),y(0),x(0)
{
  PmQuaternion quat;
  PmEulerZyx zyx;

  toQuat(q, &quat);
  pmQuatZyxConvert(quat, &zyx);
  toEulerZyx(zyx, this);
}

PM_EULER_ZYX::PM_EULER_ZYX(PM_CONST PM_ROTATION_MATRIX PM_REF m):
  z(0),y(0),x(0)
{
  PmRotationMatrix mat;
  PmEulerZyx zyx;

  toMat(m, &mat);
  pmMatZyxConvert(mat, &zyx);
  toEulerZyx(zyx, this);
}

double & PM_EULER_ZYX::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return z;
  case 1:
    return y;
  case 2:
    return x;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_EULER_ZYX::PM_EULER_ZYX(PM_CCONST PM_EULER_ZYX &zyx):
  z(zyx.z),y(zyx.y),x(zyx.x)
{
  /*
  z = zyx.z;
  y = zyx.y;
  x = zyx.x;
  */
}
#endif

PM_EULER_ZYX PM_EULER_ZYX::operator = (PM_EULER_ZYX zyx)
{
  z = zyx.z;
  y = zyx.y;
  x = zyx.x;

  return (*this);
}

// PM_RPY class

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_RPY::PM_RPY(PM_CCONST PM_RPY &rpy):
  r(rpy.r),p(rpy.p),y(rpy.y)
{
  /* 
     r = rpy.r;
     p = rpy.p;
     y = rpy.y;
  */
}
#endif

//Converts each member of rpy to be between -PI and PI.
void
PM_RPY::normalize()
{
  int r_wraps = (int) ((r+PM_PI)/(2*PM_PI));
  r -= (2*PM_PI)*r_wraps;
  int p_wraps = (int) ((p+PM_PI)/(2*PM_PI));
  p -= (2*PM_PI)*p_wraps;
  int y_wraps = (int) ((y+PM_PI)/(2*PM_PI));
  y -= (2*PM_PI)*y_wraps;
}
  
PM_RPY::PM_RPY(double _r, double _p, double _y):
  r(_r),p(_p),y(_y)
{
  /*
    r = _r;
    p = _p;
    y = _y;
  */
}

PM_RPY::PM_RPY(PM_CONST PM_QUATERNION PM_REF q):
  r(0),p(0),y(0)
{
  PmQuaternion quat;
  PmRpy rpy;

  toQuat(q, &quat);
  pmQuatRpyConvert(quat, &rpy);
  toRpy(rpy, this);
}

PM_RPY::PM_RPY(PM_CONST PM_ROTATION_MATRIX PM_REF m):
  r(0),p(0),y(0)
{
  PmRotationMatrix mat;
  PmRpy rpy;

  toMat(m, &mat);
  pmMatRpyConvert(mat, &rpy);
  toRpy(rpy, this);
}

double & PM_RPY::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return r;
    case 1:
    return p;
  case 2:
    return y;
  default:
    return noElement;           // need to return a double &
  }
}

/*
PM_RPY PM_RPY::operator = (PM_RPY rpy)
{
  r = rpy.r;
  p = rpy.p;
  y = rpy.y;

  return (*this);
  }
*/

// PM_POSE class

PM_POSE::PM_POSE(PM_CARTESIAN v, PM_QUATERNION q):
  tran(v),rot(q)
{
  /*
    tran.x = v.x;
    tran.y = v.y;
    tran.z = v.z;
    rot.s = q.s;
    rot.x = q.x;
    rot.y = q.y;
    rot.z = q.z;
  */
}

PM_POSE::PM_POSE(double x, double y, double z,
		 double s, double sx, double sy, double sz):
  tran(x,y,z),rot(s,sx,sy,sz)
{
  tran.x = x;
  tran.y = y;
  tran.z = z;
  rot.s = s;
  rot.x = sx;
  rot.y = sy;
  rot.z = sz;
}

PM_POSE::PM_POSE(PM_CONST PM_HOMOGENEOUS PM_REF h):
  tran(0,0,0),rot(0,0,0,0)
{
  PmHomogeneous hom;
  PmPose pose;

  toHom(h, &hom);
  pmHomPoseConvert(hom, &pose);
  toPose(pose, this);
}

PM_POSE::PM_POSE(PM_CONST PM_XYA PM_REF xya):
  tran(xya.x,xya.y,0),rot(0,0,0,0)
{
  PmRpy rpy;
  rpy.r = 0;
  rpy.p = 0;
  rpy.y = xya.a;
  PmQuaternion quat;
  pmRpyQuatConvert(rpy,&quat);
  toQuat(quat,&rot);
}

double & PM_POSE::operator [] (int n)
{
  switch (n)
  {
  case 0:
    return tran.x;
  case 1:
    return tran.y;
  case 2:
    return tran.z;
  case 3:
    return rot.s;
  case 4:
    return rot.x;
  case 5:
    return rot.y;
  case 6:
    return rot.z;
  default:
    return noElement;           // need to return a double &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_POSE::PM_POSE(PM_CCONST PM_POSE &p):
  tran(p.tran),rot(p.rot)
{
  tran = p.tran;
  rot = p.rot;
}
#endif


PM_POSE PM_POSE::operator = (PM_POSE p)
{
  tran = p.tran;
  rot = p.rot;

  return (*this);
}

// PM_HOMOGENEOUS class

PM_HOMOGENEOUS::PM_HOMOGENEOUS(PM_CARTESIAN v, PM_ROTATION_MATRIX m):
  tran(v),rot(m)
{
  tran = v;
  rot = m;
}

PM_HOMOGENEOUS::PM_HOMOGENEOUS(PM_CONST PM_POSE PM_REF p):
  tran(),rot()
{
  PmPose pose;
  PmHomogeneous hom;

  toPose(p, &pose);
  pmPoseHomConvert(pose, &hom);
  toHom(hom, this);
}

PM_CARTESIAN & PM_HOMOGENEOUS::operator [] (int n)
{
  // if it is a rotation vector, stuff 0 as default bottom
  // if it is a translation vector, stuff 1 as default bottom

  switch (n)
  {
  case 0:
    noElement = 0.0;
    return rot.x;
  case 1:
    noElement = 0.0;
    return rot.y;
  case 2:
    noElement = 0.0;
    return rot.z;
  case 3:
    noElement = 1.0;
    return tran;
  default:
    if(0 == noCart)
      {
        noCart = new PM_CARTESIAN(0.0,0.0,0.0);
      }
    return (*noCart);           // need to return a PM_CARTESIAN &
  }
}

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_HOMOGENEOUS::PM_HOMOGENEOUS(PM_CCONST PM_HOMOGENEOUS &h):
  tran(h.tran),rot(h.rot)
{
  tran = h.tran;
  rot = h.rot;
}
#endif


PM_HOMOGENEOUS PM_HOMOGENEOUS::operator = (PM_HOMOGENEOUS h)
{
  tran = h.tran;
  rot = h.rot;

  return (*this);
}

// PM_LINE class

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_LINE::PM_LINE(PM_CCONST PM_LINE &l):
  start(l.start),end(l.end),uVec(l.uVec)
{
  start = l.start;
  end = l.end;
  uVec = l.uVec;
}
#endif

int PM_LINE::init(PM_POSE start_arg, PM_POSE end_arg)
{
  PmLine _line;
  PmPose _start, _end;
  int retval;

  toPose(start_arg, &_start);
  toPose(end_arg, &_end);

  retval = pmLineInit(&_line, _start, _end);

  toLine(_line, this);

  return retval;
}

int PM_LINE::point(double len, PM_POSE * point_arg)
{
  PmLine _line;
  PmPose _point;
  int retval;

  toLine(*this, &_line);

  retval = pmLinePoint(&_line, len, &_point);

  toPose(_point, point_arg);

  return retval;
}

// PM_CIRCLE class

#ifdef INCLUDE_POSEMATH_COPY_CONSTRUCTORS
PM_CIRCLE::PM_CIRCLE(PM_CCONST PM_CIRCLE &c):
  center(c.center),normal(c.normal),rTan(c.rTan),
  rPerp(c.rPerp),rHelix(c.rHelix),radius(c.radius),
  angle(c.angle),spiral(c.spiral)
{
  center = c.center;
  normal = c.normal;
  rTan = c.rTan;
  rPerp = c.rPerp;
  rHelix = c.rHelix;
  radius = c.radius;
  angle = c.angle;
  spiral = c.spiral;
}
#endif

int PM_CIRCLE::init(PM_POSE start, PM_POSE end,
                    PM_CARTESIAN center_arg, PM_CARTESIAN normal_arg,
                    int turn)
{
  PmCircle _circle;
  PmPose _start, _end;
  PmCartesian _center, _normal;
  int retval;

  toPose(start, &_start);
  toPose(end, &_end);
  toCart(center_arg, &_center);
  toCart(normal_arg, &_normal);

  retval = pmCircleInit(&_circle, _start, _end, _center, _normal, turn);

  toCircle(_circle, this);

  return retval;
}

int PM_CIRCLE::point(double angle_arg, PM_POSE * point_arg)
{
  PmCircle _circle;
  PmPose _point;
  int retval;

  toCircle(*this, &_circle);

  retval = pmCirclePoint(&_circle, angle_arg, &_point);

  toPose(_point, point_arg);

  return retval;
}

// overloaded external functions

// dot

double dot(PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  double d;
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  pmCartCartDot(_v1, _v2, &d);

  return d;
}

// cross

PM_CARTESIAN cross(PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PM_CARTESIAN ret;
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  pmCartCartCross(_v1, _v2, &_v1);

  toCart(_v1, &ret);

  return ret;
}


// unit = norm

PM_CARTESIAN unit(PM_CARTESIAN v)
{
  return norm(v);
}

// norm

PM_CARTESIAN norm(PM_CARTESIAN v)
{
  PM_CARTESIAN vout;
  PmCartesian _v;

  toCart(v, &_v);

  pmCartNorm(_v, &_v);

  toCart(_v, &vout);

  return vout;
}

PM_QUATERNION norm(PM_QUATERNION q)
{
  PM_QUATERNION qout;
  PmQuaternion _q;

  toQuat(q, &_q);
  pmQuatNorm(_q, &_q);

  toQuat(_q, &qout);

  return qout;
}

PM_ROTATION_VECTOR norm(PM_ROTATION_VECTOR r)
{
  PM_ROTATION_VECTOR rout;
  PmRotationVector _r;

  toRot(r, &_r);

  pmRotNorm(_r, &_r);

  toRot(_r, &rout);

  return rout;
}

PM_ROTATION_MATRIX norm(PM_ROTATION_MATRIX m)
{
  PM_ROTATION_MATRIX mout;
  PmRotationMatrix _m;

  toMat(m, &_m);

  pmMatNorm(_m, &_m);

  toMat(_m, &mout);

  return mout;
}

PM_POSE norm(PM_POSE p)
{
  PM_POSE pout;
  PmPose _p;

  toPose(p, &_p);

  pmPoseNorm(_p, &_p);

  toPose(_p, &pout);

  return pout;
}

PM_XYA norm(PM_XYA p)
{
  PM_XYA pout = p;
  int pa_i=0;
  if(p.a > PM_PI*2) {
    pa_i = (int) (p.a/(PM_PI*2));
    pout.a -= PM_PI*2*pa_i;
  }
  else if(p.a <  -PM_PI*2) {
    pa_i = (int) (-p.a/(PM_PI*2));
    pout.a += PM_PI*2*pa_i;
  }

  return pout;
}

// isNorm

int isUnit(PM_CARTESIAN v)
{
  return isNorm(v);
}

int isNorm(PM_CARTESIAN v)
{
  PmCartesian _v;

  toCart(v, &_v);

  return pmCartIsNorm(_v);
}

int isNorm(PM_QUATERNION q)
{
  PmQuaternion _q;

  toQuat(q, &_q);

  return pmQuatIsNorm(_q);
}

int isNorm(PM_ROTATION_VECTOR r)
{
  PmRotationVector _r;

  toRot(r, &_r);

  return pmRotIsNorm(_r);
}

int isNorm(PM_ROTATION_MATRIX m)
{
  PmRotationMatrix _m;

  toMat(m, &_m);

  return pmMatIsNorm(_m);
}

int isNorm(PM_POSE p)
{
  return isNorm(p.rot);
}


int isNorm(PM_XYA p)
{
  return (p.a >= -PM_PI && p.a <= +PM_PI);
}

// mag

double mag(PM_CARTESIAN v)
{
  double ret;
  PmCartesian _v;

  toCart(v, &_v);

  pmCartMag(_v, &ret);

  return ret;
}

// disp

double disp(PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  double ret;
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  pmCartCartDisp(_v1, _v2, &ret);

  return ret;
}

// inv

PM_CARTESIAN inv(PM_CARTESIAN v)
{
  PM_CARTESIAN ret;
  PmCartesian _v;

  toCart(v, &_v);

  pmCartInv(_v, &_v);

  toCart(_v, &ret);

  return ret;
}

PM_ROTATION_MATRIX inv(PM_ROTATION_MATRIX m)
{
  PM_ROTATION_MATRIX ret;
  PmRotationMatrix _m;

  toMat(m, &_m);

  pmMatInv(_m, &_m);

  toMat(_m, &ret);

  return ret;
}

PM_QUATERNION inv(PM_QUATERNION q)
{
  PM_QUATERNION ret;
  PmQuaternion _q;

  toQuat(q, &_q);

  pmQuatInv(_q, &_q);

  toQuat(_q, &ret);

  return ret;
}

PM_POSE inv(PM_POSE p)
{
  PM_POSE ret;
  PmPose _p;

  toPose(p, &_p);

  pmPoseInv(_p, &_p);

  toPose(_p, &ret);

  return ret;
}

PM_XYA inv(PM_XYA p)
{
  PM_XYA ret;
  ret.x = -p.x*cos(p.a)-p.y*sin(p.a);
  ret.y = p.x*sin(p.a)-p.y*cos(p.a);
  ret.a = -p.a;
  return ret;
}

PM_HOMOGENEOUS inv(PM_HOMOGENEOUS h)
{
  PM_HOMOGENEOUS ret;
  PmHomogeneous _h;

  toHom(h, &_h);

  pmHomInv(_h, &_h);

  toHom(_h, &ret);

  return ret;
}

// project

PM_CARTESIAN proj(PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PM_CARTESIAN ret;
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  pmCartCartProj(_v1, _v2, &_v1);

  toCart(_v1, &ret);

  return ret;
}

// overloaded arithmetic operators

PM_CARTESIAN operator + (PM_CARTESIAN v)
{
  return v;
}

PM_CARTESIAN operator - (PM_CARTESIAN v)
{
  PM_CARTESIAN ret;

  ret.x = -v.x;
  ret.y = -v.y;
  ret.z = -v.z;

  return ret;
}

PM_QUATERNION operator + (PM_QUATERNION q)
{
  return q;
}

PM_QUATERNION operator - (PM_QUATERNION q)
{
  PM_QUATERNION ret;
  PmQuaternion _q;

  toQuat(q, &_q);

  pmQuatInv(_q, &_q);

  toQuat(_q, &ret);

  return ret;
}

PM_POSE operator + (PM_POSE p)
{
  return p;
}

PM_POSE operator - (PM_POSE p)
{
  PM_POSE ret;
  PmPose _p;

  toPose(p, &_p);

  pmPoseInv(_p, &_p);

  toPose(_p, &ret);

  return ret;
}

PM_XYA operator + (PM_XYA p)
{
  return p;
}

PM_XYA operator - (PM_XYA p)
{
  return inv(p);
}

int operator == (PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  return pmCartCartCompare(_v1, _v2);
}

int operator == (PM_QUATERNION q1, PM_QUATERNION q2)
{
  PmQuaternion _q1, _q2;

  toQuat(q1, &_q1);
  toQuat(q2, &_q2);

  return pmQuatQuatCompare(_q1, _q2);
}

int operator == (PM_POSE p1, PM_POSE p2)
{
  PmPose _p1, _p2;

  toPose(p1, &_p1);
  toPose(p2, &_p2);

  return pmPosePoseCompare(_p1, _p2);
}

int operator == (PM_XYA p1, PM_XYA p2)
{
  return (fabs(p1.x-p2.x) <= Q_FUZZ &&
	  fabs(p1.y-p2.y) <= Q_FUZZ &&
	  fmod(fabs(p1.a-p2.a),(2*PM_PI))*(2*PM_PI) <= Q_FUZZ);
}

int operator != (PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PmCartesian _v1, _v2;

  toCart(v1, &_v1);
  toCart(v2, &_v2);

  return ! pmCartCartCompare(_v1, _v2);
}

int operator != (PM_QUATERNION q1, PM_QUATERNION q2)
{
  PmQuaternion _q1, _q2;

  toQuat(q1, &_q1);
  toQuat(q2, &_q2);

  return ! pmQuatQuatCompare(_q1, _q2);
}

int operator != (PM_POSE p1, PM_POSE p2)
{
  PmPose _p1, _p2;

  toPose(p1, &_p1);
  toPose(p2, &_p2);

  return ! pmPosePoseCompare(_p1, _p2);
}

int operator != (PM_XYA p1, PM_XYA p2)
{
  return !(fabs(p1.x-p2.x) <= Q_FUZZ &&
	   fabs(p1.y-p2.y) <= Q_FUZZ &&
	   fmod(fabs(p1.a-p2.a),(2*PM_PI))*(2*PM_PI) <= Q_FUZZ);
}


PM_CARTESIAN operator + (PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PM_CARTESIAN ret;

  ret.x = v1.x + v2.x;
  ret.y = v1.y + v2.y;
  ret.z = v1.z + v2.z;

  return ret;
}

PM_CARTESIAN operator - (PM_CARTESIAN v1, PM_CARTESIAN v2)
{
  PM_CARTESIAN ret;

  ret.x = v1.x - v2.x;
  ret.y = v1.y - v2.y;
  ret.z = v1.z - v2.z;

  return ret;
}

PM_CARTESIAN operator * (PM_CARTESIAN v, double s)
{
  PM_CARTESIAN ret;

  ret.x = v.x * s;
  ret.y = v.y * s;
  ret.z = v.z * s;

  return ret;
}

PM_CARTESIAN operator * (double s, PM_CARTESIAN v)
{
  PM_CARTESIAN ret;

  ret.x = v.x * s;
  ret.y = v.y * s;
  ret.z = v.z * s;

  return ret;
}

PM_CARTESIAN operator / (PM_CARTESIAN v, double s)
{
  PM_CARTESIAN ret;

#ifdef PM_DEBUG
  if (s == 0.0)
  {
#ifdef PM_PRINT_ERROR
    pmPrintError("PM_CARTESIAN::operator / : divide by 0\n");
#endif
    pmErrno = PM_DIV_ERR;
    return ret;
  }
#endif

  ret.x = v.x / s;
  ret.y = v.y / s;
  ret.z = v.z / s;

  return ret;
}

PM_RPY operator + (PM_RPY r1, PM_RPY r2)
{
  PM_RPY ret;

  ret.r = r1.r + r2.r;
  ret.p = r1.p + r2.p;
  ret.y = r1.y + r2.y;

  return ret;
}

PM_RPY operator - (PM_RPY r1, PM_RPY r2)
{
  PM_RPY ret;

  ret.r = r1.r - r2.r;
  ret.p = r1.p - r2.p;
  ret.y = r1.y - r2.y;

  return ret;
}

PM_QUATERNION operator * (double s, PM_QUATERNION q)
{
  PM_QUATERNION qout;
  PmQuaternion _q;

  toQuat(q, &_q);

  pmQuatScalMult(_q, s, &_q);

  toQuat(_q, &qout);

  return qout;
}

PM_QUATERNION operator * ( PM_QUATERNION q, double s)
{
  PM_QUATERNION qout;
  PmQuaternion _q;

  toQuat(q, &_q);

  pmQuatScalMult(_q, s, &_q);

  toQuat(_q, &qout);

  return qout;
}

PM_QUATERNION operator / ( PM_QUATERNION q, double s)
{
  PM_QUATERNION qout;
  PmQuaternion _q;

  toQuat(q, &_q);

#ifdef PM_DEBUG
  if (s == 0.0)
  {
#ifdef PM_PRINT_ERROR
    pmPrintError("Divide by 0 in operator /\n");
#endif
    pmErrno = PM_NORM_ERR;

#if 0
 // g++/gcc versions 2.8.x and 2.9.x
  // will complain that the call to PM_QUATERNION(PM_QUATERNION) is
  // ambigous. (2.7.x and some others allow it)
  return qout = PM_QUATERNION((double) 0, (double)0, (double)0, (double)0);
#else

  PmQuaternion quat;

  quat.s = 0;
  quat.x = 0;
  quat.y = 0;
  quat.z = 0;

  pmQuatNorm(quat, &quat);

  qout.s = quat.s;
  qout.x = quat.x;
  qout.y = quat.y;
  qout.z = quat.z;
  return qout;
#endif

  }
#endif

  pmQuatScalMult(_q, 1.0 / s, &_q);
  toQuat(_q, &qout);

  pmErrno = 0;
  return qout;
}

PM_CARTESIAN operator * (PM_QUATERNION q, PM_CARTESIAN v)
{
  PM_CARTESIAN vout;
  PmQuaternion _q;
  PmCartesian _v;

  toQuat(q, &_q);
  toCart(v, &_v);

  pmQuatCartMult(_q, _v, &_v);

  toCart(_v, &vout);

  return vout;
}

PM_CARTESIAN operator * (PM_CARTESIAN v, PM_QUATERNION q)
{
  return q * v;
}

PM_QUATERNION operator * (PM_QUATERNION q1, PM_QUATERNION q2)
{
  PM_QUATERNION ret;
  PmQuaternion _q1, _q2;

  toQuat(q1, &_q1);
  toQuat(q2, &_q2);

  pmQuatQuatMult(_q1, _q2, &_q1);

  toQuat(_q1, &ret);

  return ret;
}

PM_ROTATION_MATRIX operator * (PM_ROTATION_MATRIX m1, PM_ROTATION_MATRIX m2)
{
  PM_ROTATION_MATRIX ret;
  PmRotationMatrix _m1, _m2;

  toMat(m1, &_m1);
  toMat(m2, &_m2);

  pmMatMatMult(_m1, _m2, &_m1);

  toMat(_m1, &ret);

  return ret;
}

PM_POSE operator * (PM_POSE p1, PM_POSE p2)
{
  PM_POSE ret;
  PmPose _p1, _p2;

  toPose(p1, &_p1);
  toPose(p2, &_p2);

  pmPosePoseMult(_p1, _p2, &_p1);

  toPose(_p1, &ret);

  return ret;
}

PM_XYA operator * (PM_XYA p1, PM_XYA p2)
{
  PM_XYA ret;
  ret.x = p1.x + p2.x*cos(p1.a) - p2.y*sin(p1.a);
  ret.y = p1.y + p2.x*sin(p1.a) + p2.y*cos(p1.a);
  ret.a = p1.a+p2.a;
  int pa_i=0;
  if(ret.a > PM_PI*2) {
    pa_i = (int) (ret.a/(PM_PI*2));
    ret.a -= PM_PI*2*pa_i;
  }
  else if(ret.a < -PM_PI*2) {
    pa_i = (int) (-ret.a/(PM_PI*2));
    ret.a += PM_PI*2*pa_i;
  }
  return ret;
}

PM_CARTESIAN operator * (PM_POSE p, PM_CARTESIAN v)
{
  PM_CARTESIAN ret;
  PmPose _p;
  PmCartesian _v;

  toPose(p, &_p);
  toCart(v, &_v);

  pmPoseCartMult(_p, _v, &_v);

  toCart(_v, &ret);

  return ret;
}


void
PM_POSE::load_homogeneous_transform_matrix(PM_CONST float hfm[4][4])
{
  tran.x = (double) hfm[0][3];
  tran.y = (double) hfm[1][3];
  tran.z = (double) hfm[2][3];
  PM_ROTATION_MATRIX rm(PM_CARTESIAN(hfm[0][0],hfm[1][0],hfm[2][0]),
			PM_CARTESIAN(hfm[0][1],hfm[1][1],hfm[2][1]),
			PM_CARTESIAN(hfm[0][2],hfm[1][2],hfm[2][2]));
  rot = rm;
}

void 
PM_POSE::get_homogeneous_transform_matrix(float hfm[4][4])
{
  hfm[0][3] = (float) tran.x;
  hfm[1][3] = (float) tran.y;
  hfm[2][3] = (float) tran.z;
  PM_ROTATION_MATRIX rm(rot);
  hfm[0][0] = (float) rm.x.x;
  hfm[0][1] = (float) rm.y.x;
  hfm[0][2] = (float) rm.z.x;
  hfm[1][0] = (float) rm.x.y;
  hfm[1][1] = (float) rm.y.y;
  hfm[1][2] = (float) rm.z.y;
  hfm[2][0] = (float) rm.x.z;
  hfm[2][1] = (float) rm.y.z;
  hfm[2][2] = (float) rm.z.z;
}

// Convert from a float 2D transform matrix to a POSE
PM_POSE::PM_POSE(PM_CONST float hfm[4][4])
{
  load_homogeneous_transform_matrix(hfm);
}

// Convert from a double 2D transform matrix to a POSE
void 
PM_POSE::load_homogeneous_transform_matrix(PM_CONST double hdm[4][4])
{
  tran.x =  hdm[0][3];
  tran.y =  hdm[1][3];
  tran.z =  hdm[2][3];
  PM_ROTATION_MATRIX rm(PM_CARTESIAN(hdm[0][0],hdm[1][0],hdm[2][0]),
			PM_CARTESIAN(hdm[0][1],hdm[1][1],hdm[2][1]),
			PM_CARTESIAN(hdm[0][2],hdm[1][2],hdm[2][2]));
  rot = rm;
}

void
PM_POSE::get_homogeneous_transform_matrix(double hdm[4][4])
{
  hdm[0][3] = tran.x;
  hdm[1][3] = tran.y;
  hdm[2][3] = tran.z;
  PM_ROTATION_MATRIX rm(rot);
  hdm[0][0] = rm.x.x;
  hdm[0][1] = rm.y.x;
  hdm[0][2] = rm.z.x;
  hdm[1][0] = rm.x.y;
  hdm[1][1] = rm.y.y;
  hdm[1][2] = rm.z.y;
  hdm[2][0] = rm.x.z;
  hdm[2][1] = rm.y.z;
  hdm[2][2] = rm.z.z;
}

PM_POSE::PM_POSE(PM_CONST double hdm[4][4])
{
  load_homogeneous_transform_matrix(hdm);
}

void
PM_POSE::load_xyzrpy_array(PM_CONST double  xyzrpy[6])
{ 
    tran.x = xyzrpy[0];
    tran.y = xyzrpy[1];
    tran.z = xyzrpy[2];
    PM_RPY rpy(xyzrpy[3],xyzrpy[4],xyzrpy[5]);
    rot = rpy;
}

void
PM_POSE::get_xyzrpy_array(double  xyzrpy[6])
{ 
  xyzrpy[0] = tran.x;
  xyzrpy[1] = tran.y;
  xyzrpy[2] = tran.z;
  PM_RPY rpy(rot);
  xyzrpy[3] = rpy.r;
  xyzrpy[4] = rpy.p;
  xyzrpy[5] = rpy.y;
}

// Convert from array of 6 with xyz and rpy
PM_POSE::PM_POSE(PM_CONST double  xyzrpy[6])
{
  load_xyzrpy_array(xyzrpy);
}

void 
PM_POSE::setRoll(double _roll) {
  PM_RPY rpy(rot);
  rpy.r = _roll;
  rot = rpy;
}

void
PM_POSE::setPitch(double _pitch)  {
  PM_RPY rpy(rot);
  rpy.p = _pitch;
  rot = rpy;
}

void 
PM_POSE::setYaw(double _yaw) {
  PM_RPY rpy(rot);
  rpy.y = _yaw;
  rot = rpy;
} 


/*
  Modification history:

  $Log$
  Revision 1.9  2005/08/23 13:12:21  proctor
  Added a comment for PM_ROTATION_MATRIX describing the textbook matrix form
  of the elements.

  Revision 1.8  2005/07/07 14:06:24  shackle
  Add constructor initializers so that we can compile with -Weffc++ as a way of checking that all variables are initialized.

  Revision 1.7  2005/06/20 17:23:57  proctor
  Added PM_RPY sum, difference operators

  Revision 1.6  2005/06/06 13:26:03  proctor
  Added cart * quat operator, since quat * cart was there and it's commutative

  Revision 1.5  2005/06/03 20:15:17  proctor
  Normalized the PM_ROTATON_MATRIX constructor that used item-by-item values.

  Revision 1.4  2005/06/03 19:58:10  proctor
  Added norm(), isNorm(), pmPoseNorm(), pmPoseIsNorm() to posemath.h, and
  posemath.cc (first two), _posemath.c (last two).

  Revision 1.3  2005/06/03 16:29:14  proctor
  Made sense of unit v. norm, in the following way: only Cartesian types have
  the 'unit' functions, pmCartUnit(), pmCartIsUnit(), unit(), isUnit(), and
  these are mapped to their 'norm' counterparts. The other types only have
  'norm'.


  15-Nov-1999 WPS added PM_QUATERNION::PM_QUATERNION(PM_AXIS,angle) and
  5-Nov-1998 WPS made modifications to PM_QUATERNION operator /
  so that it could be compiled with the newer GNU compiler.
  1-Sep-1998  FMP added PM_CARTESIAN operator * (PM_POSE, PM_CARTESIAN)
  15-Jan-1998  FMP added inv(PM_HOMOGENEOUS)
  18-Dec-1997  FMP changed line, circle to use poses
  11-Sep-1997  FMP added PM_PRINT_ERROR
  11-Jul-1997  FMP added conversion from C++ structs to C struct when
  calling C functions
  7-May-1997 WPS added copy constructors to avoid
  ambigous type error from VxWorks compiler.
  14-Apr-1997  FMP created from posemath.c C/C++ hybrid
*/
