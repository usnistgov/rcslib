/*
*	New C++ Header  File starts here.
*	This file should be named sensor_datan_c_n.h
*/

#ifndef sensor_datan_c_n_h_included
#define sensor_datan_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */
#include "offsets_c_n.h"

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module sensor_data_nml
%{
#include "sensor_datan_c_n.h"
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
	float range;
	float intensity;
} nml_sense_data_c_t;

#ifndef SENSOR_DATA_MSG_TYPE
#define SENSOR_DATA_MSG_TYPE	115000
#endif

typedef struct {
	int sd_length;
	nml_sense_data_c_t sd[5000];
} nml_SENSOR_DATA_MSG_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_sense_data_update(struct cms_c_struct *cms, nml_sense_data_c_t *x);
void cms_SENSOR_DATA_MSG_update(struct cms_c_struct *cms, nml_SENSOR_DATA_MSG_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_SENSOR_DATA_C_NAME_LENGTH
#define MAX_SENSOR_DATA_C_NAME_LENGTH 16
#endif
#ifndef SENSOR_DATA_C_NAME_LIST_LENGTH
#define SENSOR_DATA_C_NAME_LIST_LENGTH 2
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char sensor_data_c_name_list[SENSOR_DATA_C_NAME_LIST_LENGTH][MAX_SENSOR_DATA_C_NAME_LENGTH];
extern const NMLTYPE sensor_data_c_id_list[SENSOR_DATA_C_NAME_LIST_LENGTH];
extern const size_t sensor_data_c_size_list[SENSOR_DATA_C_NAME_LIST_LENGTH];
extern const char *sensor_data_c_symbol_lookup(long type);


/* Enumerated Type Constants */

extern int sensor_data_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_sensor_data_open(const char *buf, const char *proc, const char *cfg);
extern void nml_sensor_data_close(long nml_id);
extern int  nml_sensor_data_read(long nml_id);
extern int  nml_sensor_data_valid(long nml_id);
extern int nml_sensor_data_SENSOR_DATA_MSG_write(long nml_id, const nml_SENSOR_DATA_MSG_c_t *msg);extern nml_SENSOR_DATA_MSG_c_t * nml_sensor_data_SENSOR_DATA_MSG_get_msg(long nml_id);
#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif sensor_datan_c_n_h_included */ 

