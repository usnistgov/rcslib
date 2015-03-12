/*
*	New C++ Header  File starts here.
*	This file should be named servon_c_n.h
*/

#ifndef servon_c_n_h_included
#define servon_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module servo_nml
%{
#include "servon_c_n.h"
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

#ifndef SERVO_CONFIG_TYPE
#define SERVO_CONFIG_TYPE	6001
#endif

typedef struct {
	int serial_number;
} nml_SERVO_CONFIG_c_t;

#ifndef SERVO_HALT_TYPE
#define SERVO_HALT_TYPE	6003
#endif

typedef struct {
	int serial_number;
} nml_SERVO_HALT_c_t;

#ifndef SERVO_INIT_TYPE
#define SERVO_INIT_TYPE	6004
#endif

typedef struct {
	int serial_number;
} nml_SERVO_INIT_c_t;

#ifndef SERVO_STATUS_TYPE
#define SERVO_STATUS_TYPE	6000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_SERVO_STATUS_c_t;

#ifndef SERVO_STATUS2_TYPE
#define SERVO_STATUS2_TYPE	6005
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_SERVO_STATUS2_c_t;

#ifndef SERVO_GOTO_POINT_TYPE
#define SERVO_GOTO_POINT_TYPE	6002
#endif

typedef struct {
	int serial_number;
	nml_PM_CARTESIAN_c_t point;
} nml_SERVO_GOTO_POINT_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_SERVO_CONFIG_update(struct cms_c_struct *cms, nml_SERVO_CONFIG_c_t *x);
void cms_SERVO_HALT_update(struct cms_c_struct *cms, nml_SERVO_HALT_c_t *x);
void cms_SERVO_INIT_update(struct cms_c_struct *cms, nml_SERVO_INIT_c_t *x);
void cms_SERVO_STATUS_update(struct cms_c_struct *cms, nml_SERVO_STATUS_c_t *x);
void cms_SERVO_STATUS2_update(struct cms_c_struct *cms, nml_SERVO_STATUS2_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_SERVO_GOTO_POINT_update(struct cms_c_struct *cms, nml_SERVO_GOTO_POINT_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_SERVO_C_NAME_LENGTH
#define MAX_SERVO_C_NAME_LENGTH 17
#endif
#ifndef SERVO_C_NAME_LIST_LENGTH
#define SERVO_C_NAME_LIST_LENGTH 7
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char servo_c_name_list[SERVO_C_NAME_LIST_LENGTH][MAX_SERVO_C_NAME_LENGTH];
extern const NMLTYPE servo_c_id_list[SERVO_C_NAME_LIST_LENGTH];
extern const size_t servo_c_size_list[SERVO_C_NAME_LIST_LENGTH];
extern const char *servo_c_symbol_lookup(long type);


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

extern const char *servo_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int servo_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_servo_open(const char *buf, const char *proc, const char *cfg);
extern void nml_servo_close(long nml_id);
extern int  nml_servo_read(long nml_id);
extern int  nml_servo_valid(long nml_id);
extern int nml_servo_SERVO_CONFIG_write(long nml_id, const nml_SERVO_CONFIG_c_t *msg);extern nml_SERVO_CONFIG_c_t * nml_servo_SERVO_CONFIG_get_msg(long nml_id);extern int nml_servo_SERVO_HALT_write(long nml_id, const nml_SERVO_HALT_c_t *msg);extern nml_SERVO_HALT_c_t * nml_servo_SERVO_HALT_get_msg(long nml_id);extern int nml_servo_SERVO_INIT_write(long nml_id, const nml_SERVO_INIT_c_t *msg);extern nml_SERVO_INIT_c_t * nml_servo_SERVO_INIT_get_msg(long nml_id);extern int nml_servo_SERVO_STATUS_write(long nml_id, const nml_SERVO_STATUS_c_t *msg);extern nml_SERVO_STATUS_c_t * nml_servo_SERVO_STATUS_get_msg(long nml_id);extern int nml_servo_SERVO_STATUS2_write(long nml_id, const nml_SERVO_STATUS2_c_t *msg);extern nml_SERVO_STATUS2_c_t * nml_servo_SERVO_STATUS2_get_msg(long nml_id);extern int nml_servo_SERVO_GOTO_POINT_write(long nml_id, const nml_SERVO_GOTO_POINT_c_t *msg);extern nml_SERVO_GOTO_POINT_c_t * nml_servo_SERVO_GOTO_POINT_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif servon_c_n_h_included */ 

