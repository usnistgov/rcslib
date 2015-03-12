/*
*	New C++ Header  File starts here.
*	This file should be named otherheader_c_n.h
*/

#ifndef otherheader_c_n_h_included
#define otherheader_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */


 // Format function name set from common root of selected classes. (struct_from_other_header
 // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.
/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module struct_from_other_header_nml
%{
#include "otherheader_c_n.h"
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

typedef struct {
	char x;
} nml_struct_from_other_header_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_struct_from_other_header_update(struct cms_c_struct *cms, nml_struct_from_other_header_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_STRUCT_FROM_OTHER_HEADER_C_NAME_LENGTH
#define MAX_STRUCT_FROM_OTHER_HEADER_C_NAME_LENGTH 1
#endif
#ifndef STRUCT_FROM_OTHER_HEADER_C_NAME_LIST_LENGTH
#define STRUCT_FROM_OTHER_HEADER_C_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char struct_from_other_header_c_name_list[STRUCT_FROM_OTHER_HEADER_C_NAME_LIST_LENGTH][MAX_STRUCT_FROM_OTHER_HEADER_C_NAME_LENGTH];
extern const NMLTYPE struct_from_other_header_c_id_list[STRUCT_FROM_OTHER_HEADER_C_NAME_LIST_LENGTH];
extern const size_t struct_from_other_header_c_size_list[STRUCT_FROM_OTHER_HEADER_C_NAME_LIST_LENGTH];
extern const char *struct_from_other_header_c_symbol_lookup(long type);


/* Enumerated Type Constants */

extern int struct_from_other_header_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_struct_from_other_header_open(const char *buf, const char *proc, const char *cfg);
extern void nml_struct_from_other_header_close(long nml_id);
extern int  nml_struct_from_other_header_read(long nml_id);
extern int  nml_struct_from_other_header_valid(long nml_id);

#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif otherheader_c_n_h_included */ 

