/*
*	New C++ Header  File starts here.
*	This file should be named nml_test_format_n_codegen_protos.hh
*/

#ifndef nml_test_format_n_codegen_protos_hh_included
#define nml_test_format_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "nml_test_format.hh"

// Forward Function Prototypes
extern void teststruct_update(CMS *cms, teststruct *x);
extern void initialize_teststruct(teststruct *x);


#ifndef MAX_NML_TEST_NAME_LENGTH
#define MAX_NML_TEST_NAME_LENGTH 13
#endif
#ifndef NML_TEST_NAME_LIST_LENGTH
#define NML_TEST_NAME_LIST_LENGTH 5
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char nml_test_name_list[NML_TEST_NAME_LIST_LENGTH][MAX_NML_TEST_NAME_LENGTH];
extern const NMLTYPE nml_test_id_list[NML_TEST_NAME_LIST_LENGTH];
extern const size_t nml_test_size_list[NML_TEST_NAME_LIST_LENGTH];
extern const char *nml_test_symbol_lookup(long type);


// Enumerated Type Constants

// enumtest
#ifndef MAX_ENUM_ENUMTEST_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_STRING_LENGTH 2
#endif
	/* MAX_ENUM_ENUMTEST_STRING_LENGTH */
#ifndef ENUM_ENUMTEST_LENGTH
#define ENUM_ENUMTEST_LENGTH 3
#endif
	/* ENUM_ENUMTEST_LENGTH */

extern const char enum_enumtest_string_list[ENUM_ENUMTEST_LENGTH][MAX_ENUM_ENUMTEST_STRING_LENGTH];

extern const int enum_enumtest_int_list[ENUM_ENUMTEST_LENGTH];

extern const char *nml_test_enum_enumtest_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest_info_struct;

extern int nml_test_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif nml_test_format_n_codegen_protos_hh_included */ 

