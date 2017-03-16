/*
*	New C++ Header  File starts here.
*	This file should be named nml_ex1_c_n.h
*/

#ifndef nml_ex1_c_n_h_included
#define nml_ex1_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module ex_nml
%{
#include "nml_ex1_c_n.h"
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

#ifndef EXAMPLE_MSG_TYPE
#define EXAMPLE_MSG_TYPE	101
#endif

typedef struct {
	double d;
	float f;
	char c;
	short s;
	int i;
	long l;
	unsigned char uc;
	unsigned short us;
	unsigned int ui;
	unsigned long ul;
	int da_length;
	double da[20];
} nml_EXAMPLE_MSG_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_EXAMPLE_MSG_update(struct cms_c_struct *cms, nml_EXAMPLE_MSG_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_EX_C_NAME_LENGTH
#define MAX_EX_C_NAME_LENGTH 12
#endif
#ifndef EX_C_NAME_LIST_LENGTH
#define EX_C_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char ex_c_name_list[EX_C_NAME_LIST_LENGTH][MAX_EX_C_NAME_LENGTH];
extern const NMLTYPE ex_c_id_list[EX_C_NAME_LIST_LENGTH];
extern const size_t ex_c_size_list[EX_C_NAME_LIST_LENGTH];
extern const char *ex_c_symbol_lookup(long type);


/* Enumerated Type Constants */

extern int ex_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_ex_open(const char *buf, const char *proc, const char *cfg);
extern void nml_ex_close(long nml_id);
extern int  nml_ex_read(long nml_id);
extern int  nml_ex_valid(long nml_id);

extern int nml_ex_EXAMPLE_MSG_write(long nml_id, const nml_EXAMPLE_MSG_c_t *msg);
extern nml_EXAMPLE_MSG_c_t * nml_ex_EXAMPLE_MSG_get_msg(long nml_id);

#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif nml_ex1_c_n_h_included */ 

