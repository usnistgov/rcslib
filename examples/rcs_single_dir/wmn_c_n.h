/*
*	New C++ Header  File starts here.
*	This file should be named wmn_c_n.h
*/

#ifndef wmn_c_n_h_included
#define wmn_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module wm_nml
%{
#include "wmn_c_n.h"
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

#ifndef WM_CONFIG_TYPE
#define WM_CONFIG_TYPE	3001
#endif

typedef struct {
	int serial_number;
	double min_obstacle_height;
	double max_obstacle_height;
} nml_WM_CONFIG_c_t;

#ifndef WM_HALT_TYPE
#define WM_HALT_TYPE	3002
#endif

typedef struct {
	int serial_number;
} nml_WM_HALT_c_t;

#ifndef WM_INIT_TYPE
#define WM_INIT_TYPE	3003
#endif

typedef struct {
	int serial_number;
} nml_WM_INIT_c_t;

#ifndef WM_STATUS_TYPE
#define WM_STATUS_TYPE	3000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_WM_STATUS_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_WM_CONFIG_update(struct cms_c_struct *cms, nml_WM_CONFIG_c_t *x);
void cms_WM_HALT_update(struct cms_c_struct *cms, nml_WM_HALT_c_t *x);
void cms_WM_INIT_update(struct cms_c_struct *cms, nml_WM_INIT_c_t *x);
void cms_WM_STATUS_update(struct cms_c_struct *cms, nml_WM_STATUS_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_WM_C_NAME_LENGTH
#define MAX_WM_C_NAME_LENGTH 10
#endif
#ifndef WM_C_NAME_LIST_LENGTH
#define WM_C_NAME_LIST_LENGTH 5
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char wm_c_name_list[WM_C_NAME_LIST_LENGTH][MAX_WM_C_NAME_LENGTH];
extern const NMLTYPE wm_c_id_list[WM_C_NAME_LIST_LENGTH];
extern const size_t wm_c_size_list[WM_C_NAME_LIST_LENGTH];
extern const char *wm_c_symbol_lookup(long type);


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

extern const char *wm_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int wm_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_wm_open(const char *buf, const char *proc, const char *cfg);
extern void nml_wm_close(long nml_id);
extern int  nml_wm_read(long nml_id);
extern int  nml_wm_valid(long nml_id);
extern int nml_wm_WM_CONFIG_write(long nml_id, const nml_WM_CONFIG_c_t *msg);extern nml_WM_CONFIG_c_t * nml_wm_WM_CONFIG_get_msg(long nml_id);extern int nml_wm_WM_HALT_write(long nml_id, const nml_WM_HALT_c_t *msg);extern nml_WM_HALT_c_t * nml_wm_WM_HALT_get_msg(long nml_id);extern int nml_wm_WM_INIT_write(long nml_id, const nml_WM_INIT_c_t *msg);extern nml_WM_INIT_c_t * nml_wm_WM_INIT_get_msg(long nml_id);extern int nml_wm_WM_STATUS_write(long nml_id, const nml_WM_STATUS_c_t *msg);extern nml_WM_STATUS_c_t * nml_wm_WM_STATUS_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif wmn_c_n_h_included */ 

