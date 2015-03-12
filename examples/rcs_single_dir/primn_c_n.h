/*
*	New C++ Header  File starts here.
*	This file should be named primn_c_n.h
*/

#ifndef primn_c_n_h_included
#define primn_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module prim_nml
%{
#include "primn_c_n.h"
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

enum RCS_STATUS {
	RCS_EXEC=2,
	RCS_DONE=1,
	RCS_ERROR=3,
	UNINITIALIZED_STATUS=-1
};

/* Redefine the message classes as C typedef structs. */

#ifndef PRIM_CONFIG_TYPE
#define PRIM_CONFIG_TYPE	5001
#endif

typedef struct {
	int serial_number;
} nml_PRIM_CONFIG_c_t;

#ifndef PRIM_HALT_TYPE
#define PRIM_HALT_TYPE	5003
#endif

typedef struct {
	int serial_number;
} nml_PRIM_HALT_c_t;

#ifndef PRIM_INIT_TYPE
#define PRIM_INIT_TYPE	5004
#endif

typedef struct {
	int serial_number;
} nml_PRIM_INIT_c_t;

#ifndef PRIM_STATUS_TYPE
#define PRIM_STATUS_TYPE	5000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_PRIM_STATUS_c_t;

#ifndef PRIM_FOLLOW_WAYPOINTS_TYPE
#define PRIM_FOLLOW_WAYPOINTS_TYPE	5002
#endif

typedef struct {
	int serial_number;
	int waypoints_length;
	nml_PM_CARTESIAN_c_t waypoints[1000];
} nml_PRIM_FOLLOW_WAYPOINTS_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_PRIM_CONFIG_update(struct cms_c_struct *cms, nml_PRIM_CONFIG_c_t *x);
void cms_PRIM_HALT_update(struct cms_c_struct *cms, nml_PRIM_HALT_c_t *x);
void cms_PRIM_INIT_update(struct cms_c_struct *cms, nml_PRIM_INIT_c_t *x);
void cms_PRIM_STATUS_update(struct cms_c_struct *cms, nml_PRIM_STATUS_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_PRIM_FOLLOW_WAYPOINTS_update(struct cms_c_struct *cms, nml_PRIM_FOLLOW_WAYPOINTS_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_PRIM_C_NAME_LENGTH
#define MAX_PRIM_C_NAME_LENGTH 22
#endif
#ifndef PRIM_C_NAME_LIST_LENGTH
#define PRIM_C_NAME_LIST_LENGTH 6
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char prim_c_name_list[PRIM_C_NAME_LIST_LENGTH][MAX_PRIM_C_NAME_LENGTH];
extern const NMLTYPE prim_c_id_list[PRIM_C_NAME_LIST_LENGTH];
extern const size_t prim_c_size_list[PRIM_C_NAME_LIST_LENGTH];
extern const char *prim_c_symbol_lookup(long type);


/* Enumerated Type Constants */

/* RCS_STATUS */
#ifndef MAX_ENUM_RCS_STATUS_C_STRING_LENGTH
#define MAX_ENUM_RCS_STATUS_C_STRING_LENGTH 21
#endif
	/* MAX_ENUM_RCS_STATUS_C_STRING_LENGTH */
#ifndef ENUM_RCS_STATUS_C_LENGTH
#define ENUM_RCS_STATUS_C_LENGTH 5
#endif
	/* ENUM_RCS_STATUS_C_LENGTH */

extern const char enum_RCS_STATUS_c_string_list[ENUM_RCS_STATUS_C_LENGTH][MAX_ENUM_RCS_STATUS_C_STRING_LENGTH];

extern const int enum_RCS_STATUS_c_int_list[ENUM_RCS_STATUS_C_LENGTH];

extern const char *prim_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int prim_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_prim_open(const char *buf, const char *proc, const char *cfg);
extern void nml_prim_close(long nml_id);
extern int  nml_prim_read(long nml_id);
extern int  nml_prim_valid(long nml_id);
extern int nml_prim_PRIM_CONFIG_write(long nml_id, const nml_PRIM_CONFIG_c_t *msg);extern nml_PRIM_CONFIG_c_t * nml_prim_PRIM_CONFIG_get_msg(long nml_id);extern int nml_prim_PRIM_HALT_write(long nml_id, const nml_PRIM_HALT_c_t *msg);extern nml_PRIM_HALT_c_t * nml_prim_PRIM_HALT_get_msg(long nml_id);extern int nml_prim_PRIM_INIT_write(long nml_id, const nml_PRIM_INIT_c_t *msg);extern nml_PRIM_INIT_c_t * nml_prim_PRIM_INIT_get_msg(long nml_id);extern int nml_prim_PRIM_STATUS_write(long nml_id, const nml_PRIM_STATUS_c_t *msg);extern nml_PRIM_STATUS_c_t * nml_prim_PRIM_STATUS_get_msg(long nml_id);extern int nml_prim_PRIM_FOLLOW_WAYPOINTS_write(long nml_id, const nml_PRIM_FOLLOW_WAYPOINTS_c_t *msg);extern nml_PRIM_FOLLOW_WAYPOINTS_c_t * nml_prim_PRIM_FOLLOW_WAYPOINTS_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif primn_c_n_h_included */ 

