/*
*	New C++ Header  File starts here.
*	This file should be named cgtester_n_codegen_protos.hh
*/

#ifndef cgtester_n_codegen_protos_hh_included
#define cgtester_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "cgtester.hh"

// Forward Function Prototypes
extern void mystruct_update(CMS *cms, mystruct *x);
extern void initialize_mystruct(mystruct *x);
extern void NML_UNBOUNDED_LENGTH_ARRAY_INSIDE_COMMAND_update(CMS *cms, NML_UNBOUNDED_LENGTH_ARRAY INSIDE_COMMAND *x);
extern void void_update(CMS *cms, void *x);
extern void NML_UNBOUNDED_LENGTH_ARRAY_mystruct_update(CMS *cms, NML_UNBOUNDED_LENGTH_ARRAY mystruct *x);


#ifndef MAX_CGTESTER_NAME_LENGTH
#define MAX_CGTESTER_NAME_LENGTH 16
#endif
#ifndef CGTESTER_NAME_LIST_LENGTH
#define CGTESTER_NAME_LIST_LENGTH 4
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char cgtester_name_list[CGTESTER_NAME_LIST_LENGTH][MAX_CGTESTER_NAME_LENGTH];
extern const NMLTYPE cgtester_id_list[CGTESTER_NAME_LIST_LENGTH];
extern const size_t cgtester_size_list[CGTESTER_NAME_LIST_LENGTH];
extern const char *cgtester_symbol_lookup(long type);


// Enumerated Type Constants

// RCS_STATUS
#ifndef MAX_ENUM_RCS_STATUS_STRING_LENGTH
#define MAX_ENUM_RCS_STATUS_STRING_LENGTH 21
#endif
	/* MAX_ENUM_RCS_STATUS_STRING_LENGTH */
#ifndef ENUM_RCS_STATUS_LENGTH
#define ENUM_RCS_STATUS_LENGTH 5
#endif
	/* ENUM_RCS_STATUS_LENGTH */

extern const char enum_RCS_STATUS_string_list[ENUM_RCS_STATUS_LENGTH][MAX_ENUM_RCS_STATUS_STRING_LENGTH];

extern const int enum_RCS_STATUS_int_list[ENUM_RCS_STATUS_LENGTH];

extern const char *cgtester_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_info_struct;

// enumtest
#ifndef MAX_ENUM_ENUMTEST_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_STRING_LENGTH 2
#endif
	/* MAX_ENUM_ENUMTEST_STRING_LENGTH */
#ifndef ENUM_ENUMTEST_LENGTH
#define ENUM_ENUMTEST_LENGTH 6
#endif
	/* ENUM_ENUMTEST_LENGTH */

extern const char enum_enumtest_string_list[ENUM_ENUMTEST_LENGTH][MAX_ENUM_ENUMTEST_STRING_LENGTH];

extern const int enum_enumtest_int_list[ENUM_ENUMTEST_LENGTH];

extern const char *cgtester_enum_enumtest_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest_info_struct;

extern int cgtester_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif cgtester_n_codegen_protos_hh_included */ 

