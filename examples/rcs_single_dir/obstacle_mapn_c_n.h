/*
*	New C++ Header  File starts here.
*	This file should be named obstacle_mapn_c_n.h
*/

#ifndef obstacle_mapn_c_n_h_included
#define obstacle_mapn_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module obstacle_map_nml
%{
#include "obstacle_mapn_c_n.h"
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

#ifndef OBSTACLE_MAP_MSG_TYPE
#define OBSTACLE_MAP_MSG_TYPE	114000
#endif

typedef struct {
} nml_OBSTACLE_MAP_MSG_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_OBSTACLE_MAP_MSG_update(struct cms_c_struct *cms, nml_OBSTACLE_MAP_MSG_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_OBSTACLE_MAP_C_NAME_LENGTH
#define MAX_OBSTACLE_MAP_C_NAME_LENGTH 17
#endif
#ifndef OBSTACLE_MAP_C_NAME_LIST_LENGTH
#define OBSTACLE_MAP_C_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char obstacle_map_c_name_list[OBSTACLE_MAP_C_NAME_LIST_LENGTH][MAX_OBSTACLE_MAP_C_NAME_LENGTH];
extern const NMLTYPE obstacle_map_c_id_list[OBSTACLE_MAP_C_NAME_LIST_LENGTH];
extern const size_t obstacle_map_c_size_list[OBSTACLE_MAP_C_NAME_LIST_LENGTH];
extern const char *obstacle_map_c_symbol_lookup(long type);


/* Enumerated Type Constants */

extern int obstacle_map_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_obstacle_map_open(const char *buf, const char *proc, const char *cfg);
extern void nml_obstacle_map_close(long nml_id);
extern int  nml_obstacle_map_read(long nml_id);
extern int  nml_obstacle_map_valid(long nml_id);
extern int nml_obstacle_map_OBSTACLE_MAP_MSG_write(long nml_id, const nml_OBSTACLE_MAP_MSG_c_t *msg);extern nml_OBSTACLE_MAP_MSG_c_t * nml_obstacle_map_OBSTACLE_MAP_MSG_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif obstacle_mapn_c_n_h_included */ 

