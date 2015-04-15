/*
*	New C++ Header  File starts here.
*	This file should be named nml_test_format_c_n.h
*/

#ifndef nml_test_format_c_n_h_included
#define nml_test_format_c_n_h_included

/* Include all NML and CMS functions */
#include "nmlcms_c.h"

/* Include the other header files that contain message definitions we might need. */
#include "otherheader_c_n.h"

/* SWIG (Simplified Wrapper and Interface Generator) support. */
/* see http://www.swig.org */
#ifdef SWIG
%module nml_test_nml
%{
#include "nml_test_format_c_n.h"
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
#ifndef ENUM_RCS_ADMIN_STATE_DEFINED
#define ENUM_RCS_ADMIN_STATE_DEFINED

enum RCS_ADMIN_STATE {
	ADMIN_INITIALIZED=2,
	ADMIN_UNINITIALIZED=1,
	ADMIN_SHUT_DOWN=3,
	RCS_ADMIN_ZERO=0
};
#endif
/* end of #ifdef ENUM_RCS_ADMIN_STATE_DEFINED */

#ifndef ENUM_enumtest_typedef_DEFINED
#define ENUM_enumtest_typedef_DEFINED

enum enumtest_typedef {
	xxx=2,
	zzz=0,
	yyy=1
};
#endif
/* end of #ifdef ENUM_enumtest_typedef_DEFINED */

#ifndef ENUM_RCS_STATUS_DEFINED
#define ENUM_RCS_STATUS_DEFINED

enum RCS_STATUS {
	RCS_EXEC=2,
	RCS_DONE=1,
	RCS_ERROR=3,
	UNINITIALIZED_STATUS=-1
};
#endif
/* end of #ifdef ENUM_RCS_STATUS_DEFINED */

#ifndef ENUM_enumtest3ftoh_DEFINED
#define ENUM_enumtest3ftoh_DEFINED

enum enumtest3ftoh {
	fff=0,
	hhh=2,
	ggg=1
};
#endif
/* end of #ifdef ENUM_enumtest3ftoh_DEFINED */

#ifndef ENUM_enumtest_typedef2_DEFINED
#define ENUM_enumtest_typedef2_DEFINED

enum enumtest_typedef2 {
	uuu=2,
	www=0,
	vvv=1
};
#endif
/* end of #ifdef ENUM_enumtest_typedef2_DEFINED */

#ifndef ENUM_enumtest_DEFINED
#define ENUM_enumtest_DEFINED

enum enumtest {
	dd=77,
	b=1,
	a=0,
	e=88,
	aa=2,
	bb=3,
	ccc=99
};
#endif
/* end of #ifdef ENUM_enumtest_DEFINED */

#ifndef ENUM_anonymous_enum_nml_test_format_hh_82_DEFINED
#define ENUM_anonymous_enum_nml_test_format_hh_82_DEFINED

enum anonymous_enum_nml_test_format_hh_82 {
	INIT=0,
	DRIVEPAVED=6,
	SIGN=16,
	LAKE=11,
	STEPS=9,
	SUBSTATION=10,
	BUILDINGCONNECTOR=7,
	LIGHTPOLE=17,
	PARKINGPAVED=3,
	CONCRETE=8,
	UTILITYBOX=12,
	SIDEWALK=1,
	BUILDING=4,
	LAMP=18,
	SHRUB=14,
	TREE=15,
	UTILITYPOLE=13,
	TREES=5,
	UNKNOWN=2
};
#endif
/* end of #ifdef ENUM_anonymous_enum_nml_test_format_hh_82_DEFINED */


/* Redefine the message classes as C typedef structs. */

#ifndef BOP_MSG_TYPE
#define BOP_MSG_TYPE	104
#endif

typedef struct {
	unsigned long ula[2];
} nml_BOP_MSG_c_t;

#ifndef MyStat_TYPE
#define MyStat_TYPE	1001
#endif
#ifndef MY_STAT_TYPE
#define MY_STAT_TYPE	1001
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
} nml_MyStat_c_t;

#ifndef MyStat2_TYPE
#define MyStat2_TYPE	2002
#endif
#ifndef MY_STAT_V2_TYPE
#define MY_STAT_V2_TYPE	2002
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
	enum RCS_ADMIN_STATE admin_state;
	nml_time_tracker_c_t tt;
	int message_length;
	char message[80];
} nml_MyStat2_c_t;

#ifndef QTEST_MSG_TYPE
#define QTEST_MSG_TYPE	103
#endif

typedef struct {
	int priority;
	int pchanges_count;
	int count;
	int pid;
	char line[105];
	double time;
} nml_QTEST_MSG_c_t;

#ifndef SIMPLER_MSG_TYPE
#define SIMPLER_MSG_TYPE	102
#endif

typedef struct {
	int i;
	char cbuf[80];
	long lastvar;
} nml_SIMPLER_MSG_c_t;

#ifndef TEST_MESSAGE_BASE_TYPE
#define TEST_MESSAGE_BASE_TYPE	100
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
	enum RCS_ADMIN_STATE admin_state;
	nml_time_tracker_c_t tt;
	int message_length;
	char message[80];
	int test_message_base_var;
} nml_TEST_MESSAGE_BASE_c_t;

typedef struct {
	char csc;
	int csi;
} nml_c_struct_c_t;

typedef struct {
	char csc2;
	int csi2;
} nml_c_struct2_c_t;

typedef struct {
	unsigned short rangep[12];
} nml_fwLaserStruct_c_t;

typedef struct {
	nml_c_struct_c_t cs;
	nml_c_bool_t b;
	int i;
	char c;
	float f;
	double d;
	nml_c_bool_t bool_array[4];
	int ia[2];
	char ca[2];
	float fa[2];
	double da[2];
	double two_d_array[2][2];
	char two_c_array[2][2];
	double three_d_array[2][2][2];
	char three_c_array[2][2][2];
	double d_pi;
	int cda_length;
	char cda[2];
	double seventysevenpointseven;
	int ida_length;
	int ida[2];
	double eightyeightpointeight;
	int fda_length;
	float fda[2];
	int dda_length;
	double dda[2];
	float f_pi;
	char endtsbuf[16];
} nml_teststruct_c_t;

typedef struct {
	nml_c_struct2_c_t cs2;
	nml_c_bool_t b;
	int i;
	char c;
	float f;
	double d;
	nml_c_bool_t bool_array[2];
	int ia[2];
	char ca[2];
	float fa[2];
	double da[2];
	double two_d_array[2][2];
	double three_d_array[2][2][2];
	float f_pi;
	double d_pi;
	int cda_length;
	char cda[2];
	double seventysevenpointseven;
	int ida_length;
	int ida[2];
	double eightyeightpointeight;
	int fda_length;
	float fda[2];
	int dda_length;
	double dda[2];
	char endtsbuf[16];
} nml_teststruct_td2_c_t;

#ifndef TEST_MESSAGE_TYPE
#define TEST_MESSAGE_TYPE	101
#endif

typedef struct {
	long command_type;
	int echo_serial_number;
	enum RCS_STATUS status;
	int state;
	int line;
	int source_line;
	char source_file[64];
	enum RCS_ADMIN_STATE admin_state;
	nml_time_tracker_c_t tt;
	int message_length;
	char message[80];
	int test_message_base_var;
	char byte_to_messup_msg;
	long first_count;
	nml_struct_from_other_header_c_t sfoh;
	nml_c_bool_t b;
	char c;
	double d;
	int i;
	float f;
	long l;
	unsigned long ul;
	nml_fwLaserStruct_c_t fw;
	enum enumtest_typedef etd;
	enum enumtest_typedef2 etd2;
	char big_array[1000];
	nml_c_bool_t bool_array[2];
	int ia[2];
	char ca[2];
	float fa[2];
	double da[2];
	double two_d_array[2][2];
	double three_d_array[2][2][2];
	float f_pi;
	int cda_length;
	char cda[8];
	double seventysevenpointseven;
	int ida_length;
	int ida[8];
	double eightyeightpointeight;
	int fda_length;
	float fda[8];
	int dda_length;
	double dda[8];
	nml_teststruct_c_t s;
	nml_teststruct_td2_c_t s_td2;
	nml_teststruct_c_t sa[2];
	double d_pi;
	int sda_length;
	nml_teststruct_c_t sda[2];
	enum enumtest enumtestvar;
	enum enumtest enum_array[5];
	int enumtest_dla_length;
	enum enumtest enumtest_dla[7];
	nml_PM_CARTESIAN_c_t cart;
	nml_PM_CARTESIAN_c_t cart_array[3];
	int cart_dla_length;
	nml_PM_CARTESIAN_c_t cart_dla[5];
	nml_c_bool_t do_int_size_test;
	short smin;
	short smax;
	int i_smin;
	int i_smax;
	int imin;
	int imax;
	long l_imin;
	long l_imax;
	long lmin;
	long lmax;
	unsigned short usmax;
	unsigned int ui_usmax;
	unsigned int uimax;
	unsigned long ul_uimax;
	unsigned long ulmax;
	double d_ulmax;
	double d_lmin;
	double d_lmax;
	short s_array[3];
	int i_array[3];
	long l_array[3];
	unsigned short us_array[2];
	unsigned int ui_array[2];
	unsigned long ul_array[2];
	nml_c_bool_t false_bool;
	nml_c_bool_t true_bool;
	short sminusone;
	int iminusone;
	long lminusone;
	float fminusone;
	double dminusone;
	long last_count;
	nml_teststruct_c_t teststruct_2d_array[2][2];
	long lastvar;
} nml_TEST_MESSAGE_c_t;

#ifndef SWIG

/* Update function prototypes. */
void cms_BOP_MSG_update(struct cms_c_struct *cms, nml_BOP_MSG_c_t *x);
void cms_MyStat_update(struct cms_c_struct *cms, nml_MyStat_c_t *x);
void cms_MyStat2_update(struct cms_c_struct *cms, nml_MyStat2_c_t *x);
void cms_QTEST_MSG_update(struct cms_c_struct *cms, nml_QTEST_MSG_c_t *x);
void cms_SIMPLER_MSG_update(struct cms_c_struct *cms, nml_SIMPLER_MSG_c_t *x);
void cms_TEST_MESSAGE_BASE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_BASE_c_t *x);
void cms_c_struct_update(struct cms_c_struct *cms, nml_c_struct_c_t *x);
void cms_c_struct2_update(struct cms_c_struct *cms, nml_c_struct2_c_t *x);
void cms_fwLaserStruct_update(struct cms_c_struct *cms, nml_fwLaserStruct_c_t *x);
void cms_teststruct_update(struct cms_c_struct *cms, nml_teststruct_c_t *x);
void cms_teststruct_td2_update(struct cms_c_struct *cms, nml_teststruct_td2_c_t *x);
void cms_struct_from_other_header_update(struct cms_c_struct *cms, nml_struct_from_other_header_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_TEST_MESSAGE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_c_t *x);

#endif
/* end of #ifndef SWIG */


#ifndef MAX_NML_TEST_C_NAME_LENGTH
#define MAX_NML_TEST_C_NAME_LENGTH 18
#endif
#ifndef NML_TEST_C_NAME_LIST_LENGTH
#define NML_TEST_C_NAME_LIST_LENGTH 8
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
extern const char nml_test_c_name_list[NML_TEST_C_NAME_LIST_LENGTH][MAX_NML_TEST_C_NAME_LENGTH];
extern const NMLTYPE nml_test_c_id_list[NML_TEST_C_NAME_LIST_LENGTH];
extern const size_t nml_test_c_size_list[NML_TEST_C_NAME_LIST_LENGTH];
extern const char *nml_test_c_symbol_lookup(long type);


/* Enumerated Type Constants */

/* RCS_ADMIN_STATE */
#ifndef MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH
#define MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH 20
#endif
	/* MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH */
#ifndef ENUM_RCS_ADMIN_STATE_C_LENGTH
#define ENUM_RCS_ADMIN_STATE_C_LENGTH 5
#endif
	/* ENUM_RCS_ADMIN_STATE_C_LENGTH */

extern const char enum_RCS_ADMIN_STATE_c_string_list[ENUM_RCS_ADMIN_STATE_C_LENGTH][MAX_ENUM_RCS_ADMIN_STATE_C_STRING_LENGTH];

extern const int enum_RCS_ADMIN_STATE_c_int_list[ENUM_RCS_ADMIN_STATE_C_LENGTH];

extern const char *nml_test_c_enum_RCS_ADMIN_STATE_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_ADMIN_STATE_c_info_struct;

/* enumtest_typedef */
#ifndef MAX_ENUM_ENUMTEST_TYPEDEF_C_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_TYPEDEF_C_STRING_LENGTH 4
#endif
	/* MAX_ENUM_ENUMTEST_TYPEDEF_C_STRING_LENGTH */
#ifndef ENUM_ENUMTEST_TYPEDEF_C_LENGTH
#define ENUM_ENUMTEST_TYPEDEF_C_LENGTH 4
#endif
	/* ENUM_ENUMTEST_TYPEDEF_C_LENGTH */

extern const char enum_enumtest_typedef_c_string_list[ENUM_ENUMTEST_TYPEDEF_C_LENGTH][MAX_ENUM_ENUMTEST_TYPEDEF_C_STRING_LENGTH];

extern const int enum_enumtest_typedef_c_int_list[ENUM_ENUMTEST_TYPEDEF_C_LENGTH];

extern const char *nml_test_c_enum_enumtest_typedef_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest_typedef_c_info_struct;

/* RCS_STATUS */
#ifndef MAX_ENUM_RCS_STATUS_C_STRING_LENGTH
#define MAX_ENUM_RCS_STATUS_C_STRING_LENGTH 21
#endif
	/* MAX_ENUM_RCS_STATUS_C_STRING_LENGTH */
#ifndef ENUM_RCS_STATUS_C_LENGTH
#define ENUM_RCS_STATUS_C_LENGTH 5
#endif
	/* ENUM_RCS_STATUS_C_LENGTH */

extern const char enum_RCS_STATUS_c_string_list[ENUM_RCS_STATUS_C_LENGTH][MAX_ENUM_RCS_STATUS_C_STRING_LENGTH];

extern const int enum_RCS_STATUS_c_int_list[ENUM_RCS_STATUS_C_LENGTH];

extern const char *nml_test_c_enum_RCS_STATUS_symbol_lookup(long v);

extern const struct cms_enum_info enum_RCS_STATUS_c_info_struct;

/* enumtest3ftoh */
#ifndef MAX_ENUM_ENUMTEST3FTOH_C_STRING_LENGTH
#define MAX_ENUM_ENUMTEST3FTOH_C_STRING_LENGTH 4
#endif
	/* MAX_ENUM_ENUMTEST3FTOH_C_STRING_LENGTH */
#ifndef ENUM_ENUMTEST3FTOH_C_LENGTH
#define ENUM_ENUMTEST3FTOH_C_LENGTH 4
#endif
	/* ENUM_ENUMTEST3FTOH_C_LENGTH */

extern const char enum_enumtest3ftoh_c_string_list[ENUM_ENUMTEST3FTOH_C_LENGTH][MAX_ENUM_ENUMTEST3FTOH_C_STRING_LENGTH];

extern const int enum_enumtest3ftoh_c_int_list[ENUM_ENUMTEST3FTOH_C_LENGTH];

extern const char *nml_test_c_enum_enumtest3ftoh_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest3ftoh_c_info_struct;

/* enumtest_typedef2 */
#ifndef MAX_ENUM_ENUMTEST_TYPEDEF2_C_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_TYPEDEF2_C_STRING_LENGTH 4
#endif
	/* MAX_ENUM_ENUMTEST_TYPEDEF2_C_STRING_LENGTH */
#ifndef ENUM_ENUMTEST_TYPEDEF2_C_LENGTH
#define ENUM_ENUMTEST_TYPEDEF2_C_LENGTH 4
#endif
	/* ENUM_ENUMTEST_TYPEDEF2_C_LENGTH */

extern const char enum_enumtest_typedef2_c_string_list[ENUM_ENUMTEST_TYPEDEF2_C_LENGTH][MAX_ENUM_ENUMTEST_TYPEDEF2_C_STRING_LENGTH];

extern const int enum_enumtest_typedef2_c_int_list[ENUM_ENUMTEST_TYPEDEF2_C_LENGTH];

extern const char *nml_test_c_enum_enumtest_typedef2_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest_typedef2_c_info_struct;

/* enumtest */
#ifndef MAX_ENUM_ENUMTEST_C_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_C_STRING_LENGTH 4
#endif
	/* MAX_ENUM_ENUMTEST_C_STRING_LENGTH */
#ifndef ENUM_ENUMTEST_C_LENGTH
#define ENUM_ENUMTEST_C_LENGTH 8
#endif
	/* ENUM_ENUMTEST_C_LENGTH */

extern const char enum_enumtest_c_string_list[ENUM_ENUMTEST_C_LENGTH][MAX_ENUM_ENUMTEST_C_STRING_LENGTH];

extern const int enum_enumtest_c_int_list[ENUM_ENUMTEST_C_LENGTH];

extern const char *nml_test_c_enum_enumtest_symbol_lookup(long v);

extern const struct cms_enum_info enum_enumtest_c_info_struct;

/* anonymous_enum_nml_test_format_hh_82 */
#ifndef MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_STRING_LENGTH
#define MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_STRING_LENGTH 18
#endif
	/* MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_STRING_LENGTH */
#ifndef ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_LENGTH
#define ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_LENGTH 20
#endif
	/* ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_LENGTH */

extern const char enum_anonymous_enum_nml_test_format_hh_82_c_string_list[ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_LENGTH][MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_STRING_LENGTH];

extern const int enum_anonymous_enum_nml_test_format_hh_82_c_int_list[ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_C_LENGTH];

extern const char *nml_test_c_enum_anonymous_enum_nml_test_format_hh_82_symbol_lookup(long v);

extern const struct cms_enum_info enum_anonymous_enum_nml_test_format_hh_82_c_info_struct;

extern int nml_test_c_format(long type, void *buffer, struct cms_c_struct *cms);
extern long nml_nml_test_open(const char *buf, const char *proc, const char *cfg);
extern void nml_nml_test_close(long nml_id);
extern int  nml_nml_test_read(long nml_id);
extern int  nml_nml_test_valid(long nml_id);

extern int nml_nml_test_BOP_MSG_write(long nml_id, const nml_BOP_MSG_c_t *msg);
extern nml_BOP_MSG_c_t * nml_nml_test_BOP_MSG_get_msg(long nml_id);

extern int nml_nml_test_MyStat_write(long nml_id, const nml_MyStat_c_t *msg);
extern nml_MyStat_c_t * nml_nml_test_MyStat_get_msg(long nml_id);

extern int nml_nml_test_MyStat2_write(long nml_id, const nml_MyStat2_c_t *msg);
extern nml_MyStat2_c_t * nml_nml_test_MyStat2_get_msg(long nml_id);

extern int nml_nml_test_QTEST_MSG_write(long nml_id, const nml_QTEST_MSG_c_t *msg);
extern nml_QTEST_MSG_c_t * nml_nml_test_QTEST_MSG_get_msg(long nml_id);

extern int nml_nml_test_SIMPLER_MSG_write(long nml_id, const nml_SIMPLER_MSG_c_t *msg);
extern nml_SIMPLER_MSG_c_t * nml_nml_test_SIMPLER_MSG_get_msg(long nml_id);

extern int nml_nml_test_TEST_MESSAGE_BASE_write(long nml_id, const nml_TEST_MESSAGE_BASE_c_t *msg);
extern nml_TEST_MESSAGE_BASE_c_t * nml_nml_test_TEST_MESSAGE_BASE_get_msg(long nml_id);

extern int nml_nml_test_TEST_MESSAGE_write(long nml_id, const nml_TEST_MESSAGE_c_t *msg);
extern nml_TEST_MESSAGE_c_t * nml_nml_test_TEST_MESSAGE_get_msg(long nml_id);

#ifndef SWIG
#ifdef __cplusplus
}
#endif
#endif

#endif
	/* # endif nml_test_format_c_n_h_included */ 

