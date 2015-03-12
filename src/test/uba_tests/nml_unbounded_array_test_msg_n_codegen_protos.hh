/*
*	New C++ Header  File starts here.
*	This file should be named nml_unbounded_array_test_msg_n_codegen_protos.hh
*/

#ifndef nml_unbounded_array_test_msg_n_codegen_protos_hh_included
#define nml_unbounded_array_test_msg_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "nml_unbounded_array_test_msg.hh"

// Forward Function Prototypes
extern void nmlupdate(CMS *cms, s2 *x);
extern void initialize_s2(s2 *x);
extern void nmlupdate(CMS *cms, NML_UNBOUNDED_LENGTH_ARRAY struct s1 *x);
extern void nmlupdate(CMS *cms, s1 *x);
extern void initialize_s1(s1 *x);
extern void nmlupdate(CMS *cms, struct s2 *x);
extern void nmlupdate(CMS *cms, NML_UNBOUNDED_LENGTH_ARRAY struct s2 *x);


#ifndef MAX_NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LENGTH
#define MAX_NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LENGTH 30
#endif
#ifndef NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LIST_LENGTH
#define NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LIST_LENGTH 3
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const NMLTYPE nml_unbounded_array_test_msg_id_list[NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LIST_LENGTH];
extern const size_t nml_unbounded_array_test_msg_size_list[NML_UNBOUNDED_ARRAY_TEST_MSG_NAME_LIST_LENGTH];

// Enumerated Type Constants

extern int nml_unbounded_array_test_msg_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif nml_unbounded_array_test_msg_n_codegen_protos_hh_included */ 

