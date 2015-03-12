/*
*	New C++ Header  File starts here.
*	This file should be named myconf_n_codegen_protos.hh
*/

#ifndef myconf_n_codegen_protos_hh_included
#define myconf_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "myconf.h"

// Forward Function Prototypes


#ifndef MAX_MY_CONF_NAME_LENGTH
#define MAX_MY_CONF_NAME_LENGTH 8
#endif
#ifndef MY_CONF_NAME_LIST_LENGTH
#define MY_CONF_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const NMLTYPE MY_CONF_id_list[MY_CONF_NAME_LIST_LENGTH];
extern const size_t MY_CONF_size_list[MY_CONF_NAME_LIST_LENGTH];
extern const char MY_CONF_name_list[MY_CONF_NAME_LIST_LENGTH][MAX_MY_CONF_NAME_LENGTH];
extern const char *MY_CONF_symbol_lookup(long type);


// Enumerated Type Constants

extern int MY_CONF_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif myconf_n_codegen_protos_hh_included */ 

