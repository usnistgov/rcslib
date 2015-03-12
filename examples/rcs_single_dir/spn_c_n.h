/*
*	New C++ Header  File starts here.
*	This file should be named spn_c_n.h
*/

#ifndef spn_c_n_h_included
#define spn_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module sp_nml
%{
#include "spn_c_n.h"
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

#ifndef SP_CONFIG_TYPE
#define SP_CONFIG_TYPE	2001
#endif

typedef struct {
	int serial_number;
} nml_SP_CONFIG_c_t;

#ifndef SP_HALT_TYPE
#define SP_HALT_TYPE	2002
#endif

typedef struct {
	int serial_number;
} nml_SP_HALT_c_t;

#ifndef SP_INIT_TYPE
#define SP_INIT_TYPE	2003
#endif

typedef struct {
	int serial_number;
} nml_SP_INIT_c_t;

#ifndef SP_STATUS_TYPE
#define SP_STATUS_TYPE	2000
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
	double stime;
} nml_SP_STATUS_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_SP_CONFIG_update(struct cms_c_struct *cms, nml_SP_CONFIG_c_t *x);
void cms_SP_HALT_update(struct cms_c_struct *cms, nml_SP_HALT_c_t *x);
void cms_SP_INIT_update(struct cms_c_struct *cms, nml_SP_INIT_c_t *x);
void cms_SP_STATUS_update(struct cms_c_struct *cms, nml_SP_STATUS_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_SP_C_NAME_LENGTH
#define MAX_SP_C_NAME_LENGTH 10
#endif
#ifndef SP_C_NAME_LIST_LENGTH
#define SP_C_NAME_LIST_LENGTH 5
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char sp_c_name_list[SP_C_NAME_LIST_LENGTH][MAX_SP_C_NAME_LENGTH];
extern const NMLTYPE sp_c_id_list[SP_C_NAME_LIST_LENGTH];
extern const size_t sp_c_size_list[SP_C_NAME_LIST_LENGTH];
extern const char *sp_c_symbol_lookup(long type);


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

extern const char *sp_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

extern int sp_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_sp_open(const char *buf, const char *proc, const char *cfg);
extern void nml_sp_close(long nml_id);
extern int  nml_sp_read(long nml_id);
extern int  nml_sp_valid(long nml_id);
extern int nml_sp_SP_CONFIG_write(long nml_id, const nml_SP_CONFIG_c_t *msg);extern nml_SP_CONFIG_c_t * nml_sp_SP_CONFIG_get_msg(long nml_id);extern int nml_sp_SP_HALT_write(long nml_id, const nml_SP_HALT_c_t *msg);extern nml_SP_HALT_c_t * nml_sp_SP_HALT_get_msg(long nml_id);extern int nml_sp_SP_INIT_write(long nml_id, const nml_SP_INIT_c_t *msg);extern nml_SP_INIT_c_t * nml_sp_SP_INIT_get_msg(long nml_id);extern int nml_sp_SP_STATUS_write(long nml_id, const nml_SP_STATUS_c_t *msg);extern nml_SP_STATUS_c_t * nml_sp_SP_STATUS_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif spn_c_n_h_included */ 

