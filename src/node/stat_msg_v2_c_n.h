/*
*	New C++ Header  File starts here.
*	This file should be named stat_msg_v2_c_n.h
*/

#ifndef stat_msg_v2_c_n_h_included
#define stat_msg_v2_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */
#include "stat_msg_c_n.h"
#include "timetracker_c_n.h"

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module RCS_STAT_MSG_nml
%{
#include "stat_msg_v2_c_n.h"
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

/* Redefine the message classes as C typedef structs. */

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
	enum RCS_ADMIN_STATE admin_state;
	nml_time_tracker_c_t tt;
	int message_length;
	char message[80];
} nml_RCS_STAT_MSG_V2_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_RCS_STAT_MSG_V2_update(struct cms_c_struct *cms, nml_RCS_STAT_MSG_V2_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_RCS_STAT_MSG_C_NAME_LENGTH
#define MAX_RCS_STAT_MSG_C_NAME_LENGTH 1
#endif
#ifndef RCS_STAT_MSG_C_NAME_LIST_LENGTH
#define RCS_STAT_MSG_C_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char RCS_STAT_MSG_c_name_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH][MAX_RCS_STAT_MSG_C_NAME_LENGTH];
extern const NMLTYPE RCS_STAT_MSG_c_id_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH];
extern const size_t RCS_STAT_MSG_c_size_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH];
extern const char *RCS_STAT_MSG_c_symbol_lookup(long type);


/* Enumerated Type Constants */

/* RCS_STATE */
#ifndef MAX_ENUM_RCS_STATE_C_STRING_LENGTH
#define MAX_ENUM_RCS_STATE_C_STRING_LENGTH 20
#endif
	/* MAX_ENUM_RCS_STATE_C_STRING_LENGTH */
#ifndef ENUM_RCS_STATE_C_LENGTH
#define ENUM_RCS_STATE_C_LENGTH 54
#endif
	/* ENUM_RCS_STATE_C_LENGTH */

extern const char enum_RCS_STATE_c_string_list[ENUM_RCS_STATE_C_LENGTH][MAX_ENUM_RCS_STATE_C_STRING_LENGTH];

extern const int enum_RCS_STATE_c_int_list[ENUM_RCS_STATE_C_LENGTH];

extern const char *RCS_STAT_MSG_c_enum_RCS_STATE_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATE_c_info_struct;

/* RCS_ADMIN_STATE */
#ifndef MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH
#define MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH 20
#endif
	/* MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH */
#ifndef ENUM_RCS_ADMIN_STATE_C_LENGTH
#define ENUM_RCS_ADMIN_STATE_C_LENGTH 4
#endif
	/* ENUM_RCS_ADMIN_STATE_C_LENGTH */

extern const char enum_RCS_ADMIN_STATE_c_string_list[ENUM_RCS_ADMIN_STATE_C_LENGTH][MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH];

extern const int enum_RCS_ADMIN_STATE_c_int_list[ENUM_RCS_ADMIN_STATE_C_LENGTH];

extern const char *RCS_STAT_MSG_c_enum_RCS_ADMIN_STATE_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_ADMIN_STATE_c_info_struct;

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

extern const char *RCS_STAT_MSG_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int RCS_STAT_MSG_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_RCS_STAT_MSG_open(const char *buf, const char *proc, const char *cfg);
extern void nml_RCS_STAT_MSG_close(long nml_id);
extern int  nml_RCS_STAT_MSG_read(long nml_id);
extern int  nml_RCS_STAT_MSG_valid(long nml_id);

#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif stat_msg_v2_c_n_h_included */ 

