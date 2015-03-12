/*
*	New C++ Header  File starts here.
*	This file should be named echo_types_n_codegen_protos.hh
*/

#ifndef echo_types_n_codegen_protos_hh_included
#define echo_types_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "echo_types.hh"

// Forward Function Prototypes


#ifndef MAX_ECHO_NAME_LENGTH
#define MAX_ECHO_NAME_LENGTH 11
#endif
#ifndef ECHO_NAME_LIST_LENGTH
#define ECHO_NAME_LIST_LENGTH 3
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char ECHO_name_list[ECHO_NAME_LIST_LENGTH][MAX_ECHO_NAME_LENGTH];
extern const NMLTYPE ECHO_id_list[ECHO_NAME_LIST_LENGTH];
extern const size_t ECHO_size_list[ECHO_NAME_LIST_LENGTH];
extern const char *ECHO_symbol_lookup(long type);


// Enumerated Type Constants

extern int ECHO_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif echo_types_n_codegen_protos_hh_included */ 

