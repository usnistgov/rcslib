/*
*	New C++ Header  File starts here.
*	This file should be named ../node/timetracker_n_codegen_protos.hh
*/

#ifndef ___node_timetracker_n_codegen_protos_hh_included
#define ___node_timetracker_n_codegen_protos_hh_included

// Include all NML, CMS, and RCS classes and functions
#include "rcs.hh"

// Include command and status message definitions
#include "timetracker.hh"

// Forward Function Prototypes
extern void nmlupdate(CMS *cms, time_tracker *x);
extern void initialize_time_tracker(time_tracker *x);


#ifndef MAX_TIME_TRACKER_NAME_LENGTH
#define MAX_TIME_TRACKER_NAME_LENGTH 1
#endif
#ifndef TIME_TRACKER_NAME_LIST_LENGTH
#define TIME_TRACKER_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char time_tracker_name_list[TIME_TRACKER_NAME_LIST_LENGTH][MAX_TIME_TRACKER_NAME_LENGTH];
extern const NMLTYPE time_tracker_id_list[TIME_TRACKER_NAME_LIST_LENGTH];
extern const size_t time_tracker_size_list[TIME_TRACKER_NAME_LIST_LENGTH];
extern const char *time_tracker_symbol_lookup(long type);


// Enumerated Type Constants

// RCS_ADMIN_STATE
#ifndef MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH
#define MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH 20
#endif
	/* MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH */
#ifndef ENUM_RCS_ADMIN_STATE_LENGTH
#define ENUM_RCS_ADMIN_STATE_LENGTH 4
#endif
	/* ENUM_RCS_ADMIN_STATE_LENGTH */

extern const char *time_tracker_enum_RCS_ADMIN_STATE_symbol_lookup(long v);

extern int time_tracker_format(NMLTYPE type, void *buffer, CMS *cms);

#endif
	/* # endif ___node_timetracker_n_codegen_protos_hh_included */ 

