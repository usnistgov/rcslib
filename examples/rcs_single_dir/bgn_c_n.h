/*
*	New C++ Header  File starts here.
*	This file should be named bgn_c_n.h
*/

#ifndef bgn_c_n_h_included
#define bgn_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module bg_nml
%{
#include "bgn_c_n.h"
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

#ifndef BG_CONFIG_TYPE
#define BG_CONFIG_TYPE	4001
#endif

typedef struct {
	int serial_number;
} nml_BG_CONFIG_c_t;

#ifndef BG_HALT_TYPE
#define BG_HALT_TYPE	4003
#endif

typedef struct {
	int serial_number;
} nml_BG_HALT_c_t;

#ifndef BG_INIT_TYPE
#define BG_INIT_TYPE	4004
#endif

typedef struct {
	int serial_number;
} nml_BG_INIT_c_t;

#ifndef BG_STATUS_TYPE
#define BG_STATUS_TYPE	4000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_BG_STATUS_c_t;

#ifndef BG_GOTO_GOAL_TYPE
#define BG_GOTO_GOAL_TYPE	4002
#endif

typedef struct {
	int serial_number;
	nml_PM_CARTESIAN_c_t goal;
} nml_BG_GOTO_GOAL_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_BG_CONFIG_update(struct cms_c_struct *cms, nml_BG_CONFIG_c_t *x);
void cms_BG_HALT_update(struct cms_c_struct *cms, nml_BG_HALT_c_t *x);
void cms_BG_INIT_update(struct cms_c_struct *cms, nml_BG_INIT_c_t *x);
void cms_BG_STATUS_update(struct cms_c_struct *cms, nml_BG_STATUS_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_BG_GOTO_GOAL_update(struct cms_c_struct *cms, nml_BG_GOTO_GOAL_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_BG_C_NAME_LENGTH
#define MAX_BG_C_NAME_LENGTH 13
#endif
#ifndef BG_C_NAME_LIST_LENGTH
#define BG_C_NAME_LIST_LENGTH 6
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char bg_c_name_list[BG_C_NAME_LIST_LENGTH][MAX_BG_C_NAME_LENGTH];
extern const NMLTYPE bg_c_id_list[BG_C_NAME_LIST_LENGTH];
extern const size_t bg_c_size_list[BG_C_NAME_LIST_LENGTH];
extern const char *bg_c_symbol_lookup(long type);


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

extern const char *bg_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int bg_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_bg_open(const char *buf, const char *proc, const char *cfg);
extern void nml_bg_close(long nml_id);
extern int  nml_bg_read(long nml_id);
extern int  nml_bg_valid(long nml_id);
extern int nml_bg_BG_CONFIG_write(long nml_id, const nml_BG_CONFIG_c_t *msg);extern nml_BG_CONFIG_c_t * nml_bg_BG_CONFIG_get_msg(long nml_id);extern int nml_bg_BG_HALT_write(long nml_id, const nml_BG_HALT_c_t *msg);extern nml_BG_HALT_c_t * nml_bg_BG_HALT_get_msg(long nml_id);extern int nml_bg_BG_INIT_write(long nml_id, const nml_BG_INIT_c_t *msg);extern nml_BG_INIT_c_t * nml_bg_BG_INIT_get_msg(long nml_id);extern int nml_bg_BG_STATUS_write(long nml_id, const nml_BG_STATUS_c_t *msg);extern nml_BG_STATUS_c_t * nml_bg_BG_STATUS_get_msg(long nml_id);extern int nml_bg_BG_GOTO_GOAL_write(long nml_id, const nml_BG_GOTO_GOAL_c_t *msg);extern nml_BG_GOTO_GOAL_c_t * nml_bg_BG_GOTO_GOAL_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif bgn_c_n_h_included */ 

