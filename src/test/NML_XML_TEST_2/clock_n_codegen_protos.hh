/*
*	New C++ Header  File starts here.
*	This file should be named clock_n_codegen_protos.hh
*/

#ifndef clock_n_codegen_protos_hh_included
#define clock_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "clock.hh"

// Forward Function Prototypes


#ifndef MAX_MY_CLOCK_NAME_LENGTH
#define MAX_MY_CLOCK_NAME_LENGTH 9
#endif
#ifndef MY_CLOCK_NAME_LIST_LENGTH
#define MY_CLOCK_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const NMLTYPE MY_CLOCK_id_list[MY_CLOCK_NAME_LIST_LENGTH];
extern const size_t MY_CLOCK_size_list[MY_CLOCK_NAME_LIST_LENGTH];
extern const char MY_CLOCK_name_list[MY_CLOCK_NAME_LIST_LENGTH][MAX_MY_CLOCK_NAME_LENGTH];
extern const char *MY_CLOCK_symbol_lookup(long type);


// Enumerated Type Constants

extern int MY_CLOCK_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif clock_n_codegen_protos_hh_included */ 

