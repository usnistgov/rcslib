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
*	New C++ Header  File starts here.
*	This file should be named posemath_c_n.h
*/

#ifndef posemath_c_n_h_included
#define posemath_c_n_h_included

#ifndef __cplusplus

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

#ifdef __cplusplus
extern "C" {
#endif


/* Create C versions of the Enumeration types. */

#ifndef POSEMATH_H
enum PM_AXIS {
	PM_Z=2,
	PM_Y=1,
	PM_X=0
};
#endif


/* Redefine the message classes as C typedef structs. */

typedef struct {
	double x;
	double y;
	double z;
} nml_PM_CARTESIAN_c_t;

typedef struct {
	double theta;
	double r;
	double z;
} nml_PM_CYLINDRICAL_c_t;

typedef struct {
	double z;
	double y;
	double x;
} nml_PM_EULER_ZYX_c_t;

typedef struct {
	double z;
	double y;
	double zp;
} nml_PM_EULER_ZYZ_c_t;

typedef struct {
	double s;
	double x;
	double y;
	double z;
} nml_PM_QUATERNION_c_t;

typedef struct {
	double s;
	double x;
	double y;
	double z;
} nml_PM_ROTATION_VECTOR_c_t;

typedef struct {
	double r;
	double p;
	double y;
} nml_PM_RPY_c_t;

typedef struct {
	double theta;
	double phi;
	double r;
} nml_PM_SPHERICAL_c_t;

typedef struct {
	double x;
	double y;
	double z;
} nml_PmCartesian_c_t;

typedef struct {
	double theta;
	double r;
	double z;
} nml_PmCylindrical_c_t;

typedef struct {
	double z;
	double y;
	double x;
} nml_PmEulerZyx_c_t;

typedef struct {
	double z;
	double y;
	double zp;
} nml_PmEulerZyz_c_t;

typedef struct {
	double s;
	double x;
	double y;
	double z;
} nml_PmQuaternion_c_t;

typedef struct {
	double s;
	double x;
	double y;
	double z;
} nml_PmRotationVector_c_t;

typedef struct {
	double r;
	double p;
	double y;
} nml_PmRpy_c_t;

typedef struct {
	double theta;
	double phi;
	double r;
} nml_PmSpherical_c_t;

typedef struct {
	nml_PM_CARTESIAN_c_t center;
	nml_PM_CARTESIAN_c_t normal;
	nml_PM_CARTESIAN_c_t rTan;
	nml_PM_CARTESIAN_c_t rPerp;
	nml_PM_CARTESIAN_c_t rHelix;
	double radius;
	double angle;
	double spiral;
} nml_PM_CIRCLE_c_t;

typedef struct {
	nml_PM_CARTESIAN_c_t tran;
	nml_PM_QUATERNION_c_t rot;
} nml_PM_POSE_c_t;

typedef struct {
	nml_PM_CARTESIAN_c_t x;
	nml_PM_CARTESIAN_c_t y;
	nml_PM_CARTESIAN_c_t z;
} nml_PM_ROTATION_MATRIX_c_t;

typedef struct {
	nml_PmCartesian_c_t center;
	nml_PmCartesian_c_t normal;
	nml_PmCartesian_c_t rTan;
	nml_PmCartesian_c_t rPerp;
	nml_PmCartesian_c_t rHelix;
	double radius;
	double angle;
	double spiral;
} nml_PmCircle_c_t;

typedef struct {
	nml_PmCartesian_c_t tran;
	nml_PmQuaternion_c_t rot;
} nml_PmPose_c_t;

typedef struct {
	nml_PmCartesian_c_t x;
	nml_PmCartesian_c_t y;
	nml_PmCartesian_c_t z;
} nml_PmRotationMatrix_c_t;

typedef struct {
	nml_PM_CARTESIAN_c_t tran;
	nml_PM_ROTATION_MATRIX_c_t rot;
} nml_PM_HOMOGENEOUS_c_t;

typedef struct {
	nml_PM_POSE_c_t start;
	nml_PM_POSE_c_t end;
	nml_PM_CARTESIAN_c_t uVec;
} nml_PM_LINE_c_t;

typedef struct {
	nml_PmCartesian_c_t tran;
	nml_PmRotationMatrix_c_t rot;
} nml_PmHomogeneous_c_t;

typedef struct {
	nml_PmPose_c_t start;
	nml_PmPose_c_t end;
	nml_PmCartesian_c_t uVec;
	nml_PmQuaternion_c_t qVec;
	double tmag;
	double rmag;
	int tmag_is_greater_than_rmag;
	int tmag_zero;
	int rmag_zero;
} nml_PmLine_c_t;

/* Update function prototypes. */
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_PM_CYLINDRICAL_update(struct cms_c_struct *cms, nml_PM_CYLINDRICAL_c_t *x);
void cms_PM_EULER_ZYX_update(struct cms_c_struct *cms, nml_PM_EULER_ZYX_c_t *x);
void cms_PM_EULER_ZYZ_update(struct cms_c_struct *cms, nml_PM_EULER_ZYZ_c_t *x);
void cms_PM_QUATERNION_update(struct cms_c_struct *cms, nml_PM_QUATERNION_c_t *x);
void cms_PM_ROTATION_VECTOR_update(struct cms_c_struct *cms, nml_PM_ROTATION_VECTOR_c_t *x);
void cms_PM_RPY_update(struct cms_c_struct *cms, nml_PM_RPY_c_t *x);
void cms_PM_SPHERICAL_update(struct cms_c_struct *cms, nml_PM_SPHERICAL_c_t *x);
void cms_PmCartesian_update(struct cms_c_struct *cms, nml_PmCartesian_c_t *x);
void cms_PmCylindrical_update(struct cms_c_struct *cms, nml_PmCylindrical_c_t *x);
void cms_PmEulerZyx_update(struct cms_c_struct *cms, nml_PmEulerZyx_c_t *x);
void cms_PmEulerZyz_update(struct cms_c_struct *cms, nml_PmEulerZyz_c_t *x);
void cms_PmQuaternion_update(struct cms_c_struct *cms, nml_PmQuaternion_c_t *x);
void cms_PmRotationVector_update(struct cms_c_struct *cms, nml_PmRotationVector_c_t *x);
void cms_PmRpy_update(struct cms_c_struct *cms, nml_PmRpy_c_t *x);
void cms_PmSpherical_update(struct cms_c_struct *cms, nml_PmSpherical_c_t *x);
void cms_PM_CIRCLE_update(struct cms_c_struct *cms, nml_PM_CIRCLE_c_t *x);
void cms_PM_POSE_update(struct cms_c_struct *cms, nml_PM_POSE_c_t *x);
void cms_PM_ROTATION_MATRIX_update(struct cms_c_struct *cms, nml_PM_ROTATION_MATRIX_c_t *x);
void cms_PmCircle_update(struct cms_c_struct *cms, nml_PmCircle_c_t *x);
void cms_PmPose_update(struct cms_c_struct *cms, nml_PmPose_c_t *x);
void cms_PmRotationMatrix_update(struct cms_c_struct *cms, nml_PmRotationMatrix_c_t *x);
void cms_PM_HOMOGENEOUS_update(struct cms_c_struct *cms, nml_PM_HOMOGENEOUS_c_t *x);
void cms_PM_LINE_update(struct cms_c_struct *cms, nml_PM_LINE_c_t *x);
void cms_PmHomogeneous_update(struct cms_c_struct *cms, nml_PmHomogeneous_c_t *x);
void cms_PmLine_update(struct cms_c_struct *cms, nml_PmLine_c_t *x);


#ifndef MAX_PM_C_NAME_LENGTH
#define MAX_PM_C_NAME_LENGTH 1
#endif
#ifndef PM_C_NAME_LIST_LENGTH
#define PM_C_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char PM_c_name_list[PM_C_NAME_LIST_LENGTH][MAX_PM_C_NAME_LENGTH];
extern const NMLTYPE PM_c_id_list[PM_C_NAME_LIST_LENGTH];
extern const size_t PM_c_size_list[PM_C_NAME_LIST_LENGTH];

/* Enumerated Type Constants */

/* PM_AXIS */
#ifndef MAX_ENUM_PM_AXIS_C_STRING_LENGTH
#define MAX_ENUM_PM_AXIS_C_STRING_LENGTH 5
#endif
	/* MAX_ENUM_PM_AXIS_C_STRING_LENGTH */
#ifndef ENUM_PM_AXIS_C_LENGTH
#define ENUM_PM_AXIS_C_LENGTH 4
#endif
	/* ENUM_PM_AXIS_C_LENGTH */

extern const char enum_PM_AXIS_c_string_list[ENUM_PM_AXIS_C_LENGTH][MAX_ENUM_PM_AXIS_C_STRING_LENGTH];

extern const int enum_PM_AXIS_c_int_list[ENUM_PM_AXIS_C_LENGTH];

extern const char *PM_c_enum_PM_AXIS_symbol_lookup(long v);

extern const struct cms_enum_info enum_PM_AXIS_c_info_struct;

#ifdef __cplusplus
}
#endif

/* __cplusplus */
#endif

#endif
	/* # endif posemath_c_n_h_included */ 

