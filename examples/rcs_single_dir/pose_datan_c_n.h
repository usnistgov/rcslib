/*
*	New C++ Header  File starts here.
*	This file should be named pose_datan_c_n.h
*/

#ifndef pose_datan_c_n_h_included
#define pose_datan_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module pose_data_nml
%{
#include "pose_datan_c_n.h"
#include "nmlcms_c.h"
%}
#endif
/* end of #ifdef SWIG */

#ifndef SWIG
#ifdef __cplusplus
extern "C" {
#endif
#endif
/* end of #ifndef SWIG */


/* Create C versions of the Enumeration types. */

/* Redefine the message classes as C typedef structs. */

#ifndef POSE_DATA_MSG_TYPE
#define POSE_DATA_MSG_TYPE	116000
#endif

typedef struct {
	nml_PM_CARTESIAN_c_t tran;
	nml_PM_RPY_c_t rpy;
} nml_POSE_DATA_MSG_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_PM_RPY_update(struct cms_c_struct *cms, nml_PM_RPY_c_t *x);
void cms_POSE_DATA_MSG_update(struct cms_c_struct *cms, nml_POSE_DATA_MSG_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_POSE_DATA_C_NAME_LENGTH
#define MAX_POSE_DATA_C_NAME_LENGTH 14
#endif
#ifndef POSE_DATA_C_NAME_LIST_LENGTH
#define POSE_DATA_C_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char pose_data_c_name_list[POSE_DATA_C_NAME_LIST_LENGTH][MAX_POSE_DATA_C_NAME_LENGTH];
extern const NMLTYPE pose_data_c_id_list[POSE_DATA_C_NAME_LIST_LENGTH];
extern const size_t pose_data_c_size_list[POSE_DATA_C_NAME_LIST_LENGTH];
extern const char *pose_data_c_symbol_lookup(long type);


/* Enumerated Type Constants */

extern int pose_data_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_pose_data_open(const char *buf, const char *proc, const char *cfg);
extern void nml_pose_data_close(long nml_id);
extern int  nml_pose_data_read(long nml_id);
extern int  nml_pose_data_valid(long nml_id);
extern int nml_pose_data_POSE_DATA_MSG_write(long nml_id, const nml_POSE_DATA_MSG_c_t *msg);extern nml_POSE_DATA_MSG_c_t * nml_pose_data_POSE_DATA_MSG_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif pose_datan_c_n_h_included */ 

