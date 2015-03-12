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

/**************************************************************************
* File: cms_pm.cc                                                         *
* Provides CMS update functions for the POSEMATH classes.                 *
**************************************************************************/

#include "cms.hh"		// class CMS
#include "posemath.h"		// POSEMATH classes
#include "rcs_prnt.hh"


CMS_STATUS CMS::update (PM_XYA & Xya)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Xya.x);
  update (Xya.y);
  update (Xya.a);
  return (status);
}

CMS_STATUS CMS::update (PM_XYA * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}


// translation types
CMS_STATUS CMS::update (PM_CARTESIAN & Cart)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Cart.x);
  update (Cart.y);
  update (Cart.z);
  return (status);
}

CMS_STATUS CMS::update (PM_CARTESIAN * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

CMS_STATUS CMS::update (PM_SPHERICAL & Sph)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Sph.theta);
  update (Sph.phi);
  update (Sph.r);
  return (status);
}

CMS_STATUS CMS::update (PM_SPHERICAL * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

CMS_STATUS CMS::update (PM_CYLINDRICAL & Cyl)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Cyl.theta);
  update (Cyl.r);
  update (Cyl.z);
  return (status);
}

CMS_STATUS CMS::update (PM_CYLINDRICAL * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

  // rotation types
CMS_STATUS CMS::update (PM_ROTATION_VECTOR & Rot)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Rot.s);
  update (Rot.x);
  update (Rot.y);
  update (Rot.z);
  return (status);
}

CMS_STATUS CMS::update (PM_ROTATION_VECTOR * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}



CMS_STATUS CMS::update (PM_ROTATION_MATRIX & Mat)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Mat.x);
  update (Mat.y);
  update (Mat.z);
  return (status);
}

CMS_STATUS CMS::update (PM_ROTATION_MATRIX * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

CMS_STATUS CMS::update (PM_QUATERNION & Quat)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Quat.s);
  update (Quat.x);
  update (Quat.y);
  update (Quat.z);
  return (status);
}

CMS_STATUS CMS::update (PM_QUATERNION * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}


CMS_STATUS CMS::update (PM_EULER_ZYZ & Zyz)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Zyz.z);
  update (Zyz.y);
  update (Zyz.zp);
  return (status);
}

CMS_STATUS CMS::update (PM_EULER_ZYZ * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}


CMS_STATUS CMS::update (PM_EULER_ZYX & Zyx)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Zyx.z);
  update (Zyx.y);
  update (Zyx.x);
  return (status);
}

CMS_STATUS CMS::update (PM_EULER_ZYX * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

CMS_STATUS CMS::update (PM_RPY & Rpy)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Rpy.r);
  update (Rpy.p);
  update (Rpy.y);
  return (status);
}

CMS_STATUS CMS::update (PM_RPY * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}

  // pose types
CMS_STATUS CMS::update (PM_POSE & Pose)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Pose.tran);
  update (Pose.rot);
  return (status);
}

CMS_STATUS CMS::update (PM_POSE * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}


CMS_STATUS CMS::update (PM_HOMOGENEOUS & Hom)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  update (Hom.tran);
  update (Hom.rot);
  return (status);
}

CMS_STATUS CMS::update (PM_HOMOGENEOUS * x, int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int
    i;
  for (i = 0; i < n; i++)
    {
      update (x[i]);
    }
  return (status);
}



// translation types
CMS_STATUS CMS::update_with_name (const char *cname, PM_CARTESIAN & Cart)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  if(cname)
    {
      beginClassVar (cname);
    }
  beginClass ("PM_CARTESIAN", 0);
  update_with_name ("x", Cart.x);
  update_with_name ("y", Cart.y);
  update_with_name ("z", Cart.z);
  endClass ("PM_CARTESIAN", 0);
  
  if(cname)
    {
      endClassVar (cname);
    }
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_CARTESIAN * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS CMS::update_with_name (const char *cname, PM_SPHERICAL & Sph)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_SPHERICAL", 0);
  update_with_name ("theta", Sph.theta);
  update_with_name ("phi", Sph.phi);
  update_with_name ("r", Sph.r);
  endClass ("PM_SPHERICAL", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_SPHERICAL * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS CMS::update_with_name (const char *cname, PM_CYLINDRICAL & Cyl)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_CYLINDRICAL", 0);
  update_with_name ("theta", Cyl.theta);
  update_with_name ("r", Cyl.r);
  update_with_name ("z", Cyl.z);
  endClass ("PM_CYLINDRICAL", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_CYLINDRICAL * x,
			 unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

  // rotation types
CMS_STATUS CMS::update_with_name (const char *cname, PM_ROTATION_VECTOR & Rot)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_ROTATION_VECTOR", 0);
  update_with_name ("s", Rot.s);
  update_with_name ("x", Rot.x);
  update_with_name ("y", Rot.y);
  update_with_name ("z", Rot.z);
  endClass ("PM_ROTATION_VECTOR", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_ROTATION_VECTOR * x,
			 unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}



CMS_STATUS CMS::update_with_name (const char *cname, PM_ROTATION_MATRIX & Mat)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_ROTATION_MATRIX", 0);
  update_with_name ("x", Mat.x);
  update_with_name ("y", Mat.y);
  update_with_name ("z", Mat.z);
  endClass ("PM_ROTATION_MATRIX", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_ROTATION_MATRIX * x,
			 unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS CMS::update_with_name (const char *cname, PM_QUATERNION & Quat)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_QUATERNION", 0);
  update_with_name ("s", Quat.s);
  update_with_name ("x", Quat.x);
  update_with_name ("y", Quat.y);
  update_with_name ("z", Quat.z);
  endClass ("PM_QUATERNION", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_QUATERNION * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}


CMS_STATUS CMS::update_with_name (const char *cname, PM_EULER_ZYZ & Zyz)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_EULER_ZYZ", 0);
  update_with_name ("z", Zyz.z);
  update_with_name ("y", Zyz.y);
  update_with_name ("zp", Zyz.zp);
  endClass ("PM_EULER_ZYZ", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_EULER_ZYZ * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}


CMS_STATUS CMS::update_with_name (const char *cname, PM_EULER_ZYX & Zyx)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_EULER_ZYX", 0);
  update_with_name ("z", Zyx.z);
  update_with_name ("y", Zyx.y);
  update_with_name ("x", Zyx.x);
  endClass ("PM_EULER_ZYX", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_EULER_ZYX * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS CMS::update_with_name (const char *cname, PM_RPY & Rpy)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_RPY", 0);
  update_with_name ("r", Rpy.r);
  update_with_name ("p", Rpy.p);
  update_with_name ("y", Rpy.y);
  endClass ("PM_RPY", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_RPY * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

  // pose types
CMS_STATUS CMS::update_with_name (const char *cname, PM_POSE & Pose)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_POSE", 0);
  update_with_name ("tran", Pose.tran);
  update_with_name ("rot", Pose.rot);
  endClass ("PM_POSE", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_POSE * x, unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}


CMS_STATUS CMS::update_with_name (const char *cname, PM_HOMOGENEOUS & Hom)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (cname);
  beginClass ("PM_HOMOGENEOUS", 0);
  update_with_name ("tran", Hom.tran);
  update_with_name ("rot", Hom.rot);
  endClass ("PM_HOMOGENEOUS", 0);
  endClassVar (cname);
  return (status);
}

CMS_STATUS
  CMS::update_with_name (const char *cname, PM_HOMOGENEOUS * x,
			 unsigned int n)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  unsigned int i;
  for (i = 0; i < n; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}



// translation types

CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_CARTESIAN * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_SPHERICAL * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_CYLINDRICAL * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_ROTATION_VECTOR * x,
			     int &len, int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}

CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_ROTATION_MATRIX * x,
			     int &len, int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_QUATERNION * x, int &len,
			     int maxlen)
{
  int i;
  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_EULER_ZYZ * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_EULER_ZYX * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}

CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_RPY * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}


CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_POSE * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}

CMS_STATUS
  CMS::update_dla_with_name (const char *cname, PM_HOMOGENEOUS * x, int &len,
			     int maxlen)
{
  int i;

  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  if (len > maxlen)
    {
      rcs_print_error ("Bad array length for %s of %d (max=%d)\n",
		       cname, len, maxlen);
      return (status = CMS_UPDATE_ERROR);
    }

  beginStructDynamicArray (cname, len, maxlen);
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, x[i]);
      endStructArrayElem (cname, i);
    }
  endStructDynamicArray (cname, len, maxlen);
  return (status);
}
