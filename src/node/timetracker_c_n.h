/*
*	New C++ Header  File starts here.
*	This file should be named timetracker_c_n.h
*/

#ifndef timetracker_c_n_h_included
#define timetracker_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */

#ifndef SWIG
#ifdef __cplusplus
extern "C" {
#endif
#endif
/* end of #ifndef SWIG */


/* Redefine the message classes as C typedef structs. */

typedef struct {
	int count;
	double last;
	double now;
	double start;
	double elapsed;
	double min;
	double max;
	double avg;
} nml_time_tracker_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_time_tracker_update(struct cms_c_struct *cms, nml_time_tracker_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_TIME_TRACKER_C_NAME_LENGTH
#define MAX_TIME_TRACKER_C_NAME_LENGTH 1
#endif
#ifndef TIME_TRACKER_C_NAME_LIST_LENGTH
#define TIME_TRACKER_C_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char time_tracker_c_name_list[TIME_TRACKER_C_NAME_LIST_LENGTH][MAX_TIME_TRACKER_C_NAME_LENGTH];
extern const NMLTYPE time_tracker_c_id_list[TIME_TRACKER_C_NAME_LIST_LENGTH];
extern const size_t time_tracker_c_size_list[TIME_TRACKER_C_NAME_LIST_LENGTH];
extern const char *time_tracker_c_symbol_lookup(long type);


#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif timetracker_c_n_h_included */ 

