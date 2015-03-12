/*
*	New C++ Header  File starts here.
*	This file should be named robot_supern_c_n.h
*/

#ifndef robot_supern_c_n_h_included
#define robot_supern_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module robot_super_nml
%{
#include "robot_supern_c_n.h"
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

#ifndef ROBOT_SUPER_CONFIG_TYPE
#define ROBOT_SUPER_CONFIG_TYPE	1001
#endif

typedef struct {
	int serial_number;
} nml_ROBOT_SUPER_CONFIG_c_t;

#ifndef ROBOT_SUPER_HALT_TYPE
#define ROBOT_SUPER_HALT_TYPE	1002
#endif

typedef struct {
	int serial_number;
} nml_ROBOT_SUPER_HALT_c_t;

#ifndef ROBOT_SUPER_INIT_TYPE
#define ROBOT_SUPER_INIT_TYPE	1003
#endif

typedef struct {
	int serial_number;
} nml_ROBOT_SUPER_INIT_c_t;

#ifndef ROBOT_SUPER_RUN_TYPE
#define ROBOT_SUPER_RUN_TYPE	1004
#endif

typedef struct {
	int serial_number;
	char filename[256];
} nml_ROBOT_SUPER_RUN_c_t;

#ifndef ROBOT_SUPER_STATUS_TYPE
#define ROBOT_SUPER_STATUS_TYPE	1000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_ROBOT_SUPER_STATUS_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_ROBOT_SUPER_CONFIG_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_CONFIG_c_t *x);
void cms_ROBOT_SUPER_HALT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_HALT_c_t *x);
void cms_ROBOT_SUPER_INIT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_INIT_c_t *x);
void cms_ROBOT_SUPER_RUN_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_RUN_c_t *x);
void cms_ROBOT_SUPER_STATUS_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_STATUS_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_ROBOT_SUPER_C_NAME_LENGTH
#define MAX_ROBOT_SUPER_C_NAME_LENGTH 19
#endif
#ifndef ROBOT_SUPER_C_NAME_LIST_LENGTH
#define ROBOT_SUPER_C_NAME_LIST_LENGTH 6
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char robot_super_c_name_list[ROBOT_SUPER_C_NAME_LIST_LENGTH][MAX_ROBOT_SUPER_C_NAME_LENGTH];
extern const NMLTYPE robot_super_c_id_list[ROBOT_SUPER_C_NAME_LIST_LENGTH];
extern const size_t robot_super_c_size_list[ROBOT_SUPER_C_NAME_LIST_LENGTH];
extern const char *robot_super_c_symbol_lookup(long type);


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

extern const char *robot_super_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int robot_super_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_robot_super_open(const char *buf, const char *proc, const char *cfg);
extern void nml_robot_super_close(long nml_id);
extern int  nml_robot_super_read(long nml_id);
extern int  nml_robot_super_valid(long nml_id);
extern int nml_robot_super_ROBOT_SUPER_CONFIG_write(long nml_id, const nml_ROBOT_SUPER_CONFIG_c_t *msg);extern nml_ROBOT_SUPER_CONFIG_c_t * nml_robot_super_ROBOT_SUPER_CONFIG_get_msg(long nml_id);extern int nml_robot_super_ROBOT_SUPER_HALT_write(long nml_id, const nml_ROBOT_SUPER_HALT_c_t *msg);extern nml_ROBOT_SUPER_HALT_c_t * nml_robot_super_ROBOT_SUPER_HALT_get_msg(long nml_id);extern int nml_robot_super_ROBOT_SUPER_INIT_write(long nml_id, const nml_ROBOT_SUPER_INIT_c_t *msg);extern nml_ROBOT_SUPER_INIT_c_t * nml_robot_super_ROBOT_SUPER_INIT_get_msg(long nml_id);extern int nml_robot_super_ROBOT_SUPER_RUN_write(long nml_id, const nml_ROBOT_SUPER_RUN_c_t *msg);extern nml_ROBOT_SUPER_RUN_c_t * nml_robot_super_ROBOT_SUPER_RUN_get_msg(long nml_id);extern int nml_robot_super_ROBOT_SUPER_STATUS_write(long nml_id, const nml_ROBOT_SUPER_STATUS_c_t *msg);extern nml_ROBOT_SUPER_STATUS_c_t * nml_robot_super_ROBOT_SUPER_STATUS_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif robot_supern_c_n_h_included */ 

