/*
*	New C++ File starts here.
*	This file should be named nml_test_format_c_n.c
*/

/* Include all C language NML and CMS function prototypes. */
#include "nmlcms_c.h"

/* Include externally supplied prototypes. */
#include "nml_test_format_c_n.h"

/* Forward Function Prototypes */
#ifdef __cplusplus
extern "C" {
#endif

void cms_BOP_MSG_update(struct cms_c_struct *cms, nml_BOP_MSG_c_t *x);
void cms_MyStat_update(struct cms_c_struct *cms, nml_MyStat_c_t *x);
void cms_MyStat2_update(struct cms_c_struct *cms, nml_MyStat2_c_t *x);
void cms_QTEST_MSG_update(struct cms_c_struct *cms, nml_QTEST_MSG_c_t *x);
void cms_SIMPLER_MSG_update(struct cms_c_struct *cms, nml_SIMPLER_MSG_c_t *x);
void cms_TEST_MESSAGE_BASE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_BASE_c_t *x);
void cms_struct_from_other_header_update(struct cms_c_struct *cms, nml_struct_from_other_header_c_t *x);
void cms_fwLaserStruct_update(struct cms_c_struct *cms, nml_fwLaserStruct_c_t *x);
void cms_teststruct_update(struct cms_c_struct *cms, nml_teststruct_c_t *x);
void cms_teststruct_td2_update(struct cms_c_struct *cms, nml_teststruct_td2_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_TEST_MESSAGE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_c_t *x);
void cms_c_struct_update(struct cms_c_struct *cms, nml_c_struct_c_t *x);
void cms_c_struct2_update(struct cms_c_struct *cms, nml_c_struct2_c_t *x);

#ifdef __cplusplus
}
#endif
long nml_nml_test_open(const char *buf, const char *proc, const char *cfg)
{
	return (long) nml_new(nml_test_c_format, buf,proc,cfg);
}

int  nml_nml_test_valid(long nml_id)
{
	return (int) nml_valid( (nml_c_t) nml_id);
}

void nml_nml_test_close(long nml_id)
{
	nml_free( (nml_c_t) nml_id);
}

int nml_nml_test_read(long nml_id)
{
	return (long) nml_read( (nml_c_t) nml_id);
}

int nml_nml_test_BOP_MSG_write(long nml_id, const nml_BOP_MSG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 104,sizeof(nml_BOP_MSG_c_t));
}

nml_BOP_MSG_c_t * nml_nml_test_BOP_MSG_get_msg(long nml_id){
	return (nml_BOP_MSG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_MyStat_write(long nml_id, const nml_MyStat_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1001,sizeof(nml_MyStat_c_t));
}

nml_MyStat_c_t * nml_nml_test_MyStat_get_msg(long nml_id){
	return (nml_MyStat_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_MyStat2_write(long nml_id, const nml_MyStat2_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 2002,sizeof(nml_MyStat2_c_t));
}

nml_MyStat2_c_t * nml_nml_test_MyStat2_get_msg(long nml_id){
	return (nml_MyStat2_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_QTEST_MSG_write(long nml_id, const nml_QTEST_MSG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 103,sizeof(nml_QTEST_MSG_c_t));
}

nml_QTEST_MSG_c_t * nml_nml_test_QTEST_MSG_get_msg(long nml_id){
	return (nml_QTEST_MSG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_SIMPLER_MSG_write(long nml_id, const nml_SIMPLER_MSG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 102,sizeof(nml_SIMPLER_MSG_c_t));
}

nml_SIMPLER_MSG_c_t * nml_nml_test_SIMPLER_MSG_get_msg(long nml_id){
	return (nml_SIMPLER_MSG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_TEST_MESSAGE_write(long nml_id, const nml_TEST_MESSAGE_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 101,sizeof(nml_TEST_MESSAGE_c_t));
}

nml_TEST_MESSAGE_c_t * nml_nml_test_TEST_MESSAGE_get_msg(long nml_id){
	return (nml_TEST_MESSAGE_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_nml_test_TEST_MESSAGE_BASE_write(long nml_id, const nml_TEST_MESSAGE_BASE_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 100,sizeof(nml_TEST_MESSAGE_BASE_c_t));
}

nml_TEST_MESSAGE_BASE_c_t * nml_nml_test_TEST_MESSAGE_BASE_get_msg(long nml_id){
	return (nml_TEST_MESSAGE_BASE_c_t *) nml_get_address( (nml_c_t) nml_id);
}




#ifndef MAX_NML_TEST_C_NAME_LENGTH
#define MAX_NML_TEST_C_NAME_LENGTH 18
#endif
#ifndef NML_TEST_C_NAME_LIST_LENGTH
#define NML_TEST_C_NAME_LIST_LENGTH 8
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
const char nml_test_c_name_list[NML_TEST_C_NAME_LIST_LENGTH][MAX_NML_TEST_C_NAME_LENGTH]= {
	"BOP_MSG", /* 0,104 */
	"MyStat", /* 1,1001 */
	"MyStat2", /* 2,2002 */
	"QTEST_MSG", /* 3,103 */
	"SIMPLER_MSG", /* 4,102 */
	"TEST_MESSAGE", /* 5,101 */
	"TEST_MESSAGE_BASE", /* 6,100 */
	""};
const NMLTYPE nml_test_c_id_list[NML_TEST_C_NAME_LIST_LENGTH]= {
	BOP_MSG_TYPE, /* 0,104 */
	MY_STAT_TYPE, /* 1,1001 */
	MY_STAT_V2_TYPE, /* 2,2002 */
	QTEST_MSG_TYPE, /* 3,103 */
	SIMPLER_MSG_TYPE, /* 4,102 */
	TEST_MESSAGE_TYPE, /* 5,101 */
	TEST_MESSAGE_BASE_TYPE, /* 6,100 */
	-1};
const size_t nml_test_c_size_list[NML_TEST_C_NAME_LIST_LENGTH]= {
	sizeof(nml_BOP_MSG_c_t),
	sizeof(nml_MyStat_c_t),
	sizeof(nml_MyStat2_c_t),
	sizeof(nml_QTEST_MSG_c_t),
	sizeof(nml_SIMPLER_MSG_c_t),
	sizeof(nml_TEST_MESSAGE_c_t),
	sizeof(nml_TEST_MESSAGE_BASE_c_t),
	0};
const char *nml_test_c_symbol_lookup(long type);


/* Enumerated Type Constants */

/*  RCS_ADMIN_STATE */
#ifndef MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH
#define MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH 20
#endif
#ifndef ENUM_RCS_ADMIN_STATE_LENGTH
#define ENUM_RCS_ADMIN_STATE_LENGTH 5
#endif

const char enum_RCS_ADMIN_STATE_string_list[ENUM_RCS_ADMIN_STATE_LENGTH][MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH]= {
	"ADMIN_INITIALIZED", /* 0,2 */
	"ADMIN_SHUT_DOWN", /* 1,3 */
	"ADMIN_UNINITIALIZED", /* 2,1 */
	"RCS_ADMIN_ZERO", /* 3,0 */
	""};

const int enum_RCS_ADMIN_STATE_int_list[ENUM_RCS_ADMIN_STATE_LENGTH]= {
	ADMIN_INITIALIZED, /* 0,2 */
	ADMIN_SHUT_DOWN, /* 1,3 */
	ADMIN_UNINITIALIZED, /* 2,1 */
	RCS_ADMIN_ZERO, /* 3,0 */
	};

const char *nml_test_c_enum_RCS_ADMIN_STATE_symbol_lookup(long v)
{
	switch(v)
	{
		case ADMIN_INITIALIZED: return("ADMIN_INITIALIZED"); /* 2 */
		case ADMIN_SHUT_DOWN: return("ADMIN_SHUT_DOWN"); /* 3 */
		case ADMIN_UNINITIALIZED: return("ADMIN_UNINITIALIZED"); /* 1 */
		case RCS_ADMIN_ZERO: return("RCS_ADMIN_ZERO"); /* 0 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_RCS_ADMIN_STATE_info_struct={
	"RCS_ADMIN_STATE",
	(const char **)enum_RCS_ADMIN_STATE_string_list,
	enum_RCS_ADMIN_STATE_int_list,
	MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH,
	ENUM_RCS_ADMIN_STATE_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_RCS_ADMIN_STATE_symbol_lookup
	};

/*  enumtest_typedef */
#ifndef MAX_ENUM_ENUMTEST_TYPEDEF_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_TYPEDEF_STRING_LENGTH 4
#endif
#ifndef ENUM_ENUMTEST_TYPEDEF_LENGTH
#define ENUM_ENUMTEST_TYPEDEF_LENGTH 4
#endif

const char enum_enumtest_typedef_string_list[ENUM_ENUMTEST_TYPEDEF_LENGTH][MAX_ENUM_ENUMTEST_TYPEDEF_STRING_LENGTH]= {
	"xxx", /* 0,2 */
	"yyy", /* 1,1 */
	"zzz", /* 2,0 */
	""};

const int enum_enumtest_typedef_int_list[ENUM_ENUMTEST_TYPEDEF_LENGTH]= {
	xxx, /* 0,2 */
	yyy, /* 1,1 */
	zzz, /* 2,0 */
	};

const char *nml_test_c_enum_enumtest_typedef_symbol_lookup(long v)
{
	switch(v)
	{
		case xxx: return("xxx"); /* 2 */
		case yyy: return("yyy"); /* 1 */
		case zzz: return("zzz"); /* 0 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_enumtest_typedef_info_struct={
	"enumtest_typedef",
	(const char **)enum_enumtest_typedef_string_list,
	enum_enumtest_typedef_int_list,
	MAX_ENUM_ENUMTEST_TYPEDEF_STRING_LENGTH,
	ENUM_ENUMTEST_TYPEDEF_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_enumtest_typedef_symbol_lookup
	};

/*  RCS_STATUS */
#ifndef MAX_ENUM_RCS_STATUS_STRING_LENGTH
#define MAX_ENUM_RCS_STATUS_STRING_LENGTH 21
#endif
#ifndef ENUM_RCS_STATUS_LENGTH
#define ENUM_RCS_STATUS_LENGTH 5
#endif

const char enum_RCS_STATUS_string_list[ENUM_RCS_STATUS_LENGTH][MAX_ENUM_RCS_STATUS_STRING_LENGTH]= {
	"RCS_DONE", /* 0,1 */
	"RCS_ERROR", /* 1,3 */
	"RCS_EXEC", /* 2,2 */
	"UNINITIALIZED_STATUS", /* 3,-1 */
	""};

const int enum_RCS_STATUS_int_list[ENUM_RCS_STATUS_LENGTH]= {
	RCS_DONE, /* 0,1 */
	RCS_ERROR, /* 1,3 */
	RCS_EXEC, /* 2,2 */
	UNINITIALIZED_STATUS, /* 3,-1 */
	};

const char *nml_test_c_enum_RCS_STATUS_symbol_lookup(long v)
{
	switch(v)
	{
		case RCS_DONE: return("RCS_DONE"); /* 1 */
		case RCS_ERROR: return("RCS_ERROR"); /* 3 */
		case RCS_EXEC: return("RCS_EXEC"); /* 2 */
		case UNINITIALIZED_STATUS: return("UNINITIALIZED_STATUS"); /* -1 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_RCS_STATUS_info_struct={
	"RCS_STATUS",
	(const char **)enum_RCS_STATUS_string_list,
	enum_RCS_STATUS_int_list,
	MAX_ENUM_RCS_STATUS_STRING_LENGTH,
	ENUM_RCS_STATUS_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_RCS_STATUS_symbol_lookup
	};

/*  enumtest3ftoh */
#ifndef MAX_ENUM_ENUMTEST3FTOH_STRING_LENGTH
#define MAX_ENUM_ENUMTEST3FTOH_STRING_LENGTH 4
#endif
#ifndef ENUM_ENUMTEST3FTOH_LENGTH
#define ENUM_ENUMTEST3FTOH_LENGTH 4
#endif

const char enum_enumtest3ftoh_string_list[ENUM_ENUMTEST3FTOH_LENGTH][MAX_ENUM_ENUMTEST3FTOH_STRING_LENGTH]= {
	"fff", /* 0,0 */
	"ggg", /* 1,1 */
	"hhh", /* 2,2 */
	""};

const int enum_enumtest3ftoh_int_list[ENUM_ENUMTEST3FTOH_LENGTH]= {
	fff, /* 0,0 */
	ggg, /* 1,1 */
	hhh, /* 2,2 */
	};

const char *nml_test_c_enum_enumtest3ftoh_symbol_lookup(long v)
{
	switch(v)
	{
		case fff: return("fff"); /* 0 */
		case ggg: return("ggg"); /* 1 */
		case hhh: return("hhh"); /* 2 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_enumtest3ftoh_info_struct={
	"enumtest3ftoh",
	(const char **)enum_enumtest3ftoh_string_list,
	enum_enumtest3ftoh_int_list,
	MAX_ENUM_ENUMTEST3FTOH_STRING_LENGTH,
	ENUM_ENUMTEST3FTOH_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_enumtest3ftoh_symbol_lookup
	};

/*  enumtest_typedef2 */
#ifndef MAX_ENUM_ENUMTEST_TYPEDEF2_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_TYPEDEF2_STRING_LENGTH 4
#endif
#ifndef ENUM_ENUMTEST_TYPEDEF2_LENGTH
#define ENUM_ENUMTEST_TYPEDEF2_LENGTH 4
#endif

const char enum_enumtest_typedef2_string_list[ENUM_ENUMTEST_TYPEDEF2_LENGTH][MAX_ENUM_ENUMTEST_TYPEDEF2_STRING_LENGTH]= {
	"uuu", /* 0,2 */
	"vvv", /* 1,1 */
	"www", /* 2,0 */
	""};

const int enum_enumtest_typedef2_int_list[ENUM_ENUMTEST_TYPEDEF2_LENGTH]= {
	uuu, /* 0,2 */
	vvv, /* 1,1 */
	www, /* 2,0 */
	};

const char *nml_test_c_enum_enumtest_typedef2_symbol_lookup(long v)
{
	switch(v)
	{
		case uuu: return("uuu"); /* 2 */
		case vvv: return("vvv"); /* 1 */
		case www: return("www"); /* 0 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_enumtest_typedef2_info_struct={
	"enumtest_typedef2",
	(const char **)enum_enumtest_typedef2_string_list,
	enum_enumtest_typedef2_int_list,
	MAX_ENUM_ENUMTEST_TYPEDEF2_STRING_LENGTH,
	ENUM_ENUMTEST_TYPEDEF2_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_enumtest_typedef2_symbol_lookup
	};

/*  enumtest */
#ifndef MAX_ENUM_ENUMTEST_STRING_LENGTH
#define MAX_ENUM_ENUMTEST_STRING_LENGTH 4
#endif
#ifndef ENUM_ENUMTEST_LENGTH
#define ENUM_ENUMTEST_LENGTH 8
#endif

const char enum_enumtest_string_list[ENUM_ENUMTEST_LENGTH][MAX_ENUM_ENUMTEST_STRING_LENGTH]= {
	"a", /* 0,0 */
	"aa", /* 1,2 */
	"b", /* 2,1 */
	"bb", /* 3,3 */
	"ccc", /* 4,99 */
	"dd", /* 5,77 */
	"e", /* 6,88 */
	""};

const int enum_enumtest_int_list[ENUM_ENUMTEST_LENGTH]= {
	a, /* 0,0 */
	aa, /* 1,2 */
	b, /* 2,1 */
	bb, /* 3,3 */
	ccc, /* 4,99 */
	dd, /* 5,77 */
	e, /* 6,88 */
	};

const char *nml_test_c_enum_enumtest_symbol_lookup(long v)
{
	switch(v)
	{
		case a: return("a"); /* 0 */
		case aa: return("aa"); /* 2 */
		case b: return("b"); /* 1 */
		case bb: return("bb"); /* 3 */
		case ccc: return("ccc"); /* 99 */
		case dd: return("dd"); /* 77 */
		case e: return("e"); /* 88 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_enumtest_info_struct={
	"enumtest",
	(const char **)enum_enumtest_string_list,
	enum_enumtest_int_list,
	MAX_ENUM_ENUMTEST_STRING_LENGTH,
	ENUM_ENUMTEST_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_enumtest_symbol_lookup
	};

/*  anonymous_enum_nml_test_format_hh_82 */
#ifndef MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_STRING_LENGTH
#define MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_STRING_LENGTH 18
#endif
#ifndef ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_LENGTH
#define ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_LENGTH 20
#endif

const char enum_anonymous_enum_nml_test_format_hh_82_string_list[ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_LENGTH][MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_STRING_LENGTH]= {
	"BUILDING", /* 0,4 */
	"BUILDINGCONNECTOR", /* 1,7 */
	"CONCRETE", /* 2,8 */
	"DRIVEPAVED", /* 3,6 */
	"INIT", /* 4,0 */
	"LAKE", /* 5,11 */
	"LAMP", /* 6,18 */
	"LIGHTPOLE", /* 7,17 */
	"PARKINGPAVED", /* 8,3 */
	"SHRUB", /* 9,14 */
	"SIDEWALK", /* 10,1 */
	"SIGN", /* 11,16 */
	"STEPS", /* 12,9 */
	"SUBSTATION", /* 13,10 */
	"TREE", /* 14,15 */
	"TREES", /* 15,5 */
	"UNKNOWN", /* 16,2 */
	"UTILITYBOX", /* 17,12 */
	"UTILITYPOLE", /* 18,13 */
	""};

const int enum_anonymous_enum_nml_test_format_hh_82_int_list[ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_LENGTH]= {
	BUILDING, /* 0,4 */
	BUILDINGCONNECTOR, /* 1,7 */
	CONCRETE, /* 2,8 */
	DRIVEPAVED, /* 3,6 */
	INIT, /* 4,0 */
	LAKE, /* 5,11 */
	LAMP, /* 6,18 */
	LIGHTPOLE, /* 7,17 */
	PARKINGPAVED, /* 8,3 */
	SHRUB, /* 9,14 */
	SIDEWALK, /* 10,1 */
	SIGN, /* 11,16 */
	STEPS, /* 12,9 */
	SUBSTATION, /* 13,10 */
	TREE, /* 14,15 */
	TREES, /* 15,5 */
	UNKNOWN, /* 16,2 */
	UTILITYBOX, /* 17,12 */
	UTILITYPOLE, /* 18,13 */
	};

const char *nml_test_c_enum_anonymous_enum_nml_test_format_hh_82_symbol_lookup(long v)
{
	switch(v)
	{
		case BUILDING: return("BUILDING"); /* 4 */
		case BUILDINGCONNECTOR: return("BUILDINGCONNECTOR"); /* 7 */
		case CONCRETE: return("CONCRETE"); /* 8 */
		case DRIVEPAVED: return("DRIVEPAVED"); /* 6 */
		case INIT: return("INIT"); /* 0 */
		case LAKE: return("LAKE"); /* 11 */
		case LAMP: return("LAMP"); /* 18 */
		case LIGHTPOLE: return("LIGHTPOLE"); /* 17 */
		case PARKINGPAVED: return("PARKINGPAVED"); /* 3 */
		case SHRUB: return("SHRUB"); /* 14 */
		case SIDEWALK: return("SIDEWALK"); /* 1 */
		case SIGN: return("SIGN"); /* 16 */
		case STEPS: return("STEPS"); /* 9 */
		case SUBSTATION: return("SUBSTATION"); /* 10 */
		case TREE: return("TREE"); /* 15 */
		case TREES: return("TREES"); /* 5 */
		case UNKNOWN: return("UNKNOWN"); /* 2 */
		case UTILITYBOX: return("UTILITYBOX"); /* 12 */
		case UTILITYPOLE: return("UTILITYPOLE"); /* 13 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_anonymous_enum_nml_test_format_hh_82_info_struct={
	"anonymous_enum_nml_test_format_hh_82",
	(const char **)enum_anonymous_enum_nml_test_format_hh_82_string_list,
	enum_anonymous_enum_nml_test_format_hh_82_int_list,
	MAX_ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_STRING_LENGTH,
	ENUM_ANONYMOUS_ENUM_NML_TEST_FORMAT_HH_82_LENGTH,
	(cms_symbol_lookup_function_t)nml_test_c_enum_anonymous_enum_nml_test_format_hh_82_symbol_lookup
	};

/*
*	NML/CMS Format function : nml_test_c_format
*	Automatically generated by NML CodeGen Java Applet.
*/
int nml_test_c_format(long type, void *buffer, struct cms_c_struct *cms)
{

	type = cms_check_type_info(cms,type,buffer,"nml_test_c",
		(cms_symbol_lookup_function_t) nml_test_c_symbol_lookup,
		(const char **)nml_test_c_name_list,
		nml_test_c_id_list,nml_test_c_size_list,
		NML_TEST_C_NAME_LIST_LENGTH,
		MAX_NML_TEST_C_NAME_LENGTH);

	switch(type)
	{
	case BOP_MSG_TYPE:
		cms_BOP_MSG_update(cms,(nml_BOP_MSG_c_t *) buffer);
		break;
	case MY_STAT_TYPE:
		cms_MyStat_update(cms,(nml_MyStat_c_t *) buffer);
		break;
	case MY_STAT_V2_TYPE:
		cms_MyStat2_update(cms,(nml_MyStat2_c_t *) buffer);
		break;
	case QTEST_MSG_TYPE:
		cms_QTEST_MSG_update(cms,(nml_QTEST_MSG_c_t *) buffer);
		break;
	case SIMPLER_MSG_TYPE:
		cms_SIMPLER_MSG_update(cms,(nml_SIMPLER_MSG_c_t *) buffer);
		break;
	case TEST_MESSAGE_TYPE:
		cms_TEST_MESSAGE_update(cms,(nml_TEST_MESSAGE_c_t *) buffer);
		break;
	case TEST_MESSAGE_BASE_TYPE:
		cms_TEST_MESSAGE_BASE_update(cms,(nml_TEST_MESSAGE_BASE_c_t *) buffer);
		break;

	default:
		return(0);
	}
	return 1;
}


/* NML Symbol Lookup Function */
const char *nml_test_c_symbol_lookup(long type)
{
	switch(type)
	{
	case BOP_MSG_TYPE:
		return "BOP_MSG";
	case MY_STAT_TYPE:
		return "MyStat";
	case MY_STAT_V2_TYPE:
		return "MyStat2";
	case QTEST_MSG_TYPE:
		return "QTEST_MSG";
	case SIMPLER_MSG_TYPE:
		return "SIMPLER_MSG";
	case TEST_MESSAGE_TYPE:
		return "TEST_MESSAGE";
	case TEST_MESSAGE_BASE_TYPE:
		return "TEST_MESSAGE_BASE";
	default:
		return"UNKNOWN";
		break;
	}
	return(NULL);
}

/*
*	NML/CMS Update function for SIMPLER_MSG
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_SIMPLER_MSG_update(struct cms_c_struct *cms, nml_SIMPLER_MSG_c_t *x)
{

	cms_begin_class(cms,"SIMPLER_MSG","NMLmsg");
	cms_update_int(cms,"i",&(x->i));
	cms_update_char_array(cms,"cbuf",x->cbuf,80);
	cms_update_long(cms,"lastvar",&(x->lastvar));

	cms_end_class(cms,"SIMPLER_MSG","NMLmsg");

}


/*
*	NML/CMS Update function for teststruct_td2
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_teststruct_td2_update(struct cms_c_struct *cms, nml_teststruct_td2_c_t *x)
{

	cms_begin_class(cms,"teststruct_td2",0);
	cms_begin_class_var(cms,"cs2");
	cms_c_struct2_update(cms, ( nml_c_struct2_c_t *) &(x->cs2));
	cms_end_class_var(cms,"cs2");
	cms_update_bool(cms,"b",&(x->b));
	cms_update_int(cms,"i",&(x->i));
	cms_update_char(cms,"c",&(x->c));
	cms_update_float(cms,"f",&(x->f));
	cms_update_double(cms,"d",&(x->d));
	cms_update_bool_array(cms,"bool_array",x->bool_array,2);
	cms_update_int_array(cms,"ia",x->ia,2);
	cms_update_char_array(cms,"ca",x->ca,2);
	cms_update_float_array(cms,"fa",x->fa,2);
	cms_update_double_array(cms,"da",x->da,2);
	cms_update_double_array(cms,"two_d_array",(double *) x->two_d_array,4);
	cms_update_double_array(cms,"three_d_array",(double *) x->three_d_array,8);
	cms_next_update_default(cms,"3.14159265");
	cms_update_float(cms,"f_pi",&(x->f_pi));
	cms_next_update_default(cms,"3.14159265");
	cms_update_double(cms,"d_pi",&(x->d_pi));
	cms_update_int(cms,"cda_length",&(x->cda_length));
	cms_update_char_dla(cms,"cda",x->cda, &(x->cda_length),2);
	cms_next_update_default(cms,"77.7");
	cms_update_double(cms,"seventysevenpointseven",&(x->seventysevenpointseven));
	cms_update_int(cms,"ida_length",&(x->ida_length));
	cms_update_int_dla(cms,"ida",x->ida, &(x->ida_length),2);
	cms_next_update_default(cms,"88.8");
	cms_update_double(cms,"eightyeightpointeight",&(x->eightyeightpointeight));
	cms_update_int(cms,"fda_length",&(x->fda_length));
	cms_update_float_dla(cms,"fda",x->fda, &(x->fda_length),2);
	cms_update_int(cms,"dda_length",&(x->dda_length));
	cms_update_double_dla(cms,"dda",x->dda, &(x->dda_length),2);
	cms_next_update_default(cms,"endts");
	cms_update_char_array(cms,"endtsbuf",x->endtsbuf,16);

	cms_end_class(cms,"teststruct_td2",0);

}


/*
*	NML/CMS Update function for BOP_MSG
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_BOP_MSG_update(struct cms_c_struct *cms, nml_BOP_MSG_c_t *x)
{

	cms_begin_class(cms,"BOP_MSG","NMLmsg");
	cms_update_unsigned_long_array(cms,"ula",x->ula,2);

	cms_end_class(cms,"BOP_MSG","NMLmsg");

}


/*
*	NML/CMS Update function for c_struct
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_c_struct_update(struct cms_c_struct *cms, nml_c_struct_c_t *x)
{

	cms_begin_class(cms,"c_struct",0);
	cms_update_char(cms,"csc",&(x->csc));
	cms_update_int(cms,"csi",&(x->csi));

	cms_end_class(cms,"c_struct",0);

}


/*
*	NML/CMS Update function for QTEST_MSG
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_QTEST_MSG_update(struct cms_c_struct *cms, nml_QTEST_MSG_c_t *x)
{

	cms_begin_class(cms,"QTEST_MSG","NMLmsg");
	cms_update_int(cms,"priority",&(x->priority));
	cms_update_int(cms,"pchanges_count",&(x->pchanges_count));
	cms_update_int(cms,"count",&(x->count));
	cms_update_int(cms,"pid",&(x->pid));
	cms_update_char_array(cms,"line",x->line,105);
	cms_update_double(cms,"time",&(x->time));

	cms_end_class(cms,"QTEST_MSG","NMLmsg");

}


/*
*	NML/CMS Update function for MyStat
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_MyStat_update(struct cms_c_struct *cms, nml_MyStat_c_t *x)
{

	cms_begin_class(cms,"MyStat","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	x->status= (enum RCS_STATUS) cms_update_enumeration(cms,"status", (int)x->status,(void*)&(x->status),&enum_RCS_STATUS_info_struct);
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);

	cms_end_class(cms,"MyStat","RCS_STAT_MSG");

}


/*
*	NML/CMS Update function for TEST_MESSAGE
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_TEST_MESSAGE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_c_t *x)
{

	cms_begin_class(cms,"TEST_MESSAGE","TEST_MESSAGE_BASE");

{

	cms_begin_class(cms,"TEST_MESSAGE_BASE","RCS_STAT_MSG_V2");

{

	cms_begin_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	x->status= (enum RCS_STATUS) cms_update_enumeration(cms,"status", (int)x->status,(void*)&(x->status),&enum_RCS_STATUS_info_struct);
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);
	x->admin_state= (enum RCS_ADMIN_STATE) cms_update_enumeration(cms,"admin_state", (int)x->admin_state,(void*)&(x->admin_state),&enum_RCS_ADMIN_STATE_info_struct);
	cms_begin_class_var(cms,"tt");
	cms_time_tracker_update(cms, ( nml_time_tracker_c_t *) &(x->tt));
	cms_end_class_var(cms,"tt");
	cms_update_int(cms,"message_length",&(x->message_length));
	cms_update_char_dla(cms,"message",x->message, &(x->message_length),80);

	cms_end_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");

}
	cms_end_base_class(cms,"RCS_STAT_MSG_V2");

	cms_update_int(cms,"test_message_base_var",&(x->test_message_base_var));

	cms_end_class(cms,"TEST_MESSAGE_BASE","RCS_STAT_MSG_V2");

}
	cms_end_base_class(cms,"TEST_MESSAGE_BASE");

	cms_update_char(cms,"byte_to_messup_msg",&(x->byte_to_messup_msg));
	cms_update_long(cms,"first_count",&(x->first_count));
	cms_begin_class_var(cms,"sfoh");
	cms_struct_from_other_header_update(cms, ( nml_struct_from_other_header_c_t *) &(x->sfoh));
	cms_end_class_var(cms,"sfoh");
	cms_update_bool(cms,"b",&(x->b));
	cms_update_char(cms,"c",&(x->c));
	cms_update_double(cms,"d",&(x->d));
	cms_update_int(cms,"i",&(x->i));
	cms_update_float(cms,"f",&(x->f));
	cms_update_long(cms,"l",&(x->l));
	cms_update_unsigned_long(cms,"ul",&(x->ul));
	cms_begin_class_var(cms,"fw");
	cms_fwLaserStruct_update(cms, ( nml_fwLaserStruct_c_t *) &(x->fw));
	cms_end_class_var(cms,"fw");
	x->etd= (enum enumtest_typedef) cms_update_enumeration(cms,"etd", (int)x->etd,(void*)&(x->etd),&enum_enumtest_typedef_info_struct);
	x->etd2= (enum enumtest_typedef2) cms_update_enumeration(cms,"etd2", (int)x->etd2,(void*)&(x->etd2),&enum_enumtest_typedef2_info_struct);
	cms_update_char_array(cms,"big_array",x->big_array,1000);
	cms_update_bool_array(cms,"bool_array",x->bool_array,2);
	cms_update_int_array(cms,"ia",x->ia,2);
	cms_update_char_array(cms,"ca",x->ca,2);
	cms_update_float_array(cms,"fa",x->fa,2);
	cms_update_double_array(cms,"da",x->da,2);
	cms_update_double_array(cms,"two_d_array",(double *) x->two_d_array,4);
	cms_update_double_array(cms,"three_d_array",(double *) x->three_d_array,8);
	cms_next_update_default(cms,"3.14159265");
	cms_update_float(cms,"f_pi",&(x->f_pi));
	cms_update_int(cms,"cda_length",&(x->cda_length));
	cms_update_char_dla(cms,"cda",x->cda, &(x->cda_length),8);
	cms_next_update_default(cms,"77.7");
	cms_update_double(cms,"seventysevenpointseven",&(x->seventysevenpointseven));
	cms_update_int(cms,"ida_length",&(x->ida_length));
	cms_update_int_dla(cms,"ida",x->ida, &(x->ida_length),8);
	cms_next_update_default(cms,"88.8");
	cms_update_double(cms,"eightyeightpointeight",&(x->eightyeightpointeight));
	cms_update_int(cms,"fda_length",&(x->fda_length));
	cms_update_float_dla(cms,"fda",x->fda, &(x->fda_length),8);
	cms_update_int(cms,"dda_length",&(x->dda_length));
	cms_update_double_dla(cms,"dda",x->dda, &(x->dda_length),8);
	cms_begin_class_var(cms,"s");
	cms_teststruct_update(cms, ( nml_teststruct_c_t *) &(x->s));
	cms_end_class_var(cms,"s");
	cms_begin_class_var(cms,"s_td2");
	cms_teststruct_td2_update(cms, ( nml_teststruct_td2_c_t *) &(x->s_td2));
	cms_end_class_var(cms,"s_td2");

	{
		int i_sa=0;

		for(i_sa = 0;i_sa < 2 ;i_sa++)
		{
			cms_begin_struct_array_elem(cms,"sa",i_sa);
			cms_teststruct_update(cms,&(( x->sa)[i_sa]));
			cms_end_struct_array_elem(cms,"sa",i_sa);
		}
	}

	cms_next_update_default(cms,"3.14159265");
	cms_update_double(cms,"d_pi",&(x->d_pi));
	cms_update_int(cms,"sda_length",&(x->sda_length));
	cms_begin_struct_dynamic_array(cms,"sda",&(x->sda_length), 2);

	{
		int i_sda=0;

			for(i_sda = 0;i_sda < x->sda_length; i_sda++)
		{
			cms_begin_struct_array_elem(cms,"sda",i_sda);
			cms_teststruct_update(cms,&(( x->sda)[i_sda]));
			cms_end_struct_array_elem(cms,"sda",i_sda);
		}
	}

	cms_end_struct_dynamic_array(cms,"sda",&(x->sda_length), 2);
	x->enumtestvar= (enum enumtest) cms_update_enumeration(cms,"enumtestvar", (int)x->enumtestvar,(void*)&(x->enumtestvar),&enum_enumtest_info_struct);
	cms_begin_enumeration_array(cms,"enum_array",&enum_enumtest_info_struct,5);
	{
		int i_enum_array=0;
		for(i_enum_array=0; i_enum_array < 5; i_enum_array++ )
		{
			x->enum_array[i_enum_array] = (enum enumtest)
				cms_update_enumeration_array_elem(cms,x->enum_array[i_enum_array],(int *) &(x->enum_array[i_enum_array]),i_enum_array);
		}
	}
	cms_end_enumeration_array(cms,"enum_array",&enum_enumtest_info_struct,5);
	cms_update_int(cms,"enumtest_dla_length",&(x->enumtest_dla_length));
	cms_begin_enumeration_dla(cms,"enumtest_dla",&enum_enumtest_info_struct,&(x->enumtest_dla_length),7);
	{
		int i_enumtest_dla=0;
		for(i_enumtest_dla=0; i_enumtest_dla < x->enumtest_dla_length && x->enumtest_dla_length <= 7; i_enumtest_dla++ )
		{
			x->enumtest_dla[i_enumtest_dla] = (enum enumtest)
				cms_update_enumeration_array_elem(cms,x->enumtest_dla[i_enumtest_dla],(int *) &(x->enumtest_dla[i_enumtest_dla]),i_enumtest_dla);
		}
	}
	cms_end_enumeration_dla(cms,"enumtest_dla",&enum_enumtest_info_struct,&(x->enumtest_dla_length),7);
	cms_begin_class_var(cms,"cart");
	cms_PM_CARTESIAN_update(cms, ( nml_PM_CARTESIAN_c_t *) &(x->cart));
	cms_end_class_var(cms,"cart");

	{
		int i_cart_array=0;

		for(i_cart_array = 0;i_cart_array < 3 ;i_cart_array++)
		{
			cms_begin_struct_array_elem(cms,"cart_array",i_cart_array);
			cms_PM_CARTESIAN_update(cms,&(( x->cart_array)[i_cart_array]));
			cms_end_struct_array_elem(cms,"cart_array",i_cart_array);
		}
	}

	cms_update_int(cms,"cart_dla_length",&(x->cart_dla_length));
	cms_begin_struct_dynamic_array(cms,"cart_dla",&(x->cart_dla_length), 5);

	{
		int i_cart_dla=0;

			for(i_cart_dla = 0;i_cart_dla < x->cart_dla_length; i_cart_dla++)
		{
			cms_begin_struct_array_elem(cms,"cart_dla",i_cart_dla);
			cms_PM_CARTESIAN_update(cms,&(( x->cart_dla)[i_cart_dla]));
			cms_end_struct_array_elem(cms,"cart_dla",i_cart_dla);
		}
	}

	cms_end_struct_dynamic_array(cms,"cart_dla",&(x->cart_dla_length), 5);
	cms_update_bool(cms,"do_int_size_test",&(x->do_int_size_test));
	cms_update_short(cms,"smin",&(x->smin));
	cms_update_short(cms,"smax",&(x->smax));
	cms_update_int(cms,"i_smin",&(x->i_smin));
	cms_update_int(cms,"i_smax",&(x->i_smax));
	cms_update_int(cms,"imin",&(x->imin));
	cms_update_int(cms,"imax",&(x->imax));
	cms_update_long(cms,"l_imin",&(x->l_imin));
	cms_update_long(cms,"l_imax",&(x->l_imax));
	cms_update_long(cms,"lmin",&(x->lmin));
	cms_update_long(cms,"lmax",&(x->lmax));
	cms_update_unsigned_short(cms,"usmax",&(x->usmax));
	cms_update_unsigned_int(cms,"ui_usmax",&(x->ui_usmax));
	cms_update_unsigned_int(cms,"uimax",&(x->uimax));
	cms_update_unsigned_long(cms,"ul_uimax",&(x->ul_uimax));
	cms_update_unsigned_long(cms,"ulmax",&(x->ulmax));
	cms_update_double(cms,"d_ulmax",&(x->d_ulmax));
	cms_update_double(cms,"d_lmin",&(x->d_lmin));
	cms_update_double(cms,"d_lmax",&(x->d_lmax));
	cms_update_short_array(cms,"s_array",x->s_array,3);
	cms_update_int_array(cms,"i_array",x->i_array,3);
	cms_update_long_array(cms,"l_array",x->l_array,3);
	cms_update_unsigned_short_array(cms,"us_array",x->us_array,2);
	cms_update_unsigned_int_array(cms,"ui_array",x->ui_array,2);
	cms_update_unsigned_long_array(cms,"ul_array",x->ul_array,2);
	cms_update_bool(cms,"false_bool",&(x->false_bool));
	cms_update_bool(cms,"true_bool",&(x->true_bool));
	cms_update_short(cms,"sminusone",&(x->sminusone));
	cms_update_int(cms,"iminusone",&(x->iminusone));
	cms_update_long(cms,"lminusone",&(x->lminusone));
	cms_update_float(cms,"fminusone",&(x->fminusone));
	cms_update_double(cms,"dminusone",&(x->dminusone));
	cms_update_long(cms,"last_count",&(x->last_count));

	{
		int i_teststruct_2d_array=0;

		for(i_teststruct_2d_array = 0;i_teststruct_2d_array < 4 ;i_teststruct_2d_array++)
		{
			cms_begin_struct_array_elem(cms,"teststruct_2d_array",i_teststruct_2d_array);
			cms_teststruct_update(cms,( nml_teststruct_c_t *) &((( nml_teststruct_c_t *)  x->teststruct_2d_array)[i_teststruct_2d_array]));
			cms_end_struct_array_elem(cms,"teststruct_2d_array",i_teststruct_2d_array);
		}
	}

	cms_update_long(cms,"lastvar",&(x->lastvar));

	cms_end_class(cms,"TEST_MESSAGE","TEST_MESSAGE_BASE");

}


/*
*	NML/CMS Update function for teststruct
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_teststruct_update(struct cms_c_struct *cms, nml_teststruct_c_t *x)
{

	cms_begin_class(cms,"teststruct",0);
	cms_begin_class_var(cms,"cs");
	cms_c_struct_update(cms, ( nml_c_struct_c_t *) &(x->cs));
	cms_end_class_var(cms,"cs");
	cms_update_bool(cms,"b",&(x->b));
	cms_update_int(cms,"i",&(x->i));
	cms_update_char(cms,"c",&(x->c));
	cms_update_float(cms,"f",&(x->f));
	cms_update_double(cms,"d",&(x->d));
	cms_update_bool_array(cms,"bool_array",x->bool_array,4);
	cms_update_int_array(cms,"ia",x->ia,2);
	cms_update_char_array(cms,"ca",x->ca,2);
	cms_update_float_array(cms,"fa",x->fa,2);
	cms_update_double_array(cms,"da",x->da,2);
	cms_update_double_array(cms,"two_d_array",(double *) x->two_d_array,4);
	cms_update_char_array(cms,"two_c_array",(char *) x->two_c_array,4);
	cms_update_double_array(cms,"three_d_array",(double *) x->three_d_array,8);
	cms_update_char_array(cms,"three_c_array",(char *) x->three_c_array,8);
	cms_next_update_default(cms,"3.14159265");
	cms_update_double(cms,"d_pi",&(x->d_pi));
	cms_update_int(cms,"cda_length",&(x->cda_length));
	cms_update_char_dla(cms,"cda",x->cda, &(x->cda_length),2);
	cms_next_update_default(cms,"77.7");
	cms_update_double(cms,"seventysevenpointseven",&(x->seventysevenpointseven));
	cms_update_int(cms,"ida_length",&(x->ida_length));
	cms_update_int_dla(cms,"ida",x->ida, &(x->ida_length),2);
	cms_next_update_default(cms,"88.8");
	cms_update_double(cms,"eightyeightpointeight",&(x->eightyeightpointeight));
	cms_update_int(cms,"fda_length",&(x->fda_length));
	cms_update_float_dla(cms,"fda",x->fda, &(x->fda_length),2);
	cms_update_int(cms,"dda_length",&(x->dda_length));
	cms_update_double_dla(cms,"dda",x->dda, &(x->dda_length),2);
	cms_next_update_default(cms,"3.14159265");
	cms_update_float(cms,"f_pi",&(x->f_pi));
	cms_next_update_default(cms,"endts");
	cms_update_char_array(cms,"endtsbuf",x->endtsbuf,16);

	cms_end_class(cms,"teststruct",0);

}


/*
*	NML/CMS Update function for c_struct2
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_c_struct2_update(struct cms_c_struct *cms, nml_c_struct2_c_t *x)
{

	cms_begin_class(cms,"c_struct2",0);
	cms_update_char(cms,"csc2",&(x->csc2));
	cms_update_int(cms,"csi2",&(x->csi2));

	cms_end_class(cms,"c_struct2",0);

}


/*
*	NML/CMS Update function for MyStat2
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_MyStat2_update(struct cms_c_struct *cms, nml_MyStat2_c_t *x)
{

	cms_begin_class(cms,"MyStat2","RCS_STAT_MSG_V2");

{

	cms_begin_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	x->status= (enum RCS_STATUS) cms_update_enumeration(cms,"status", (int)x->status,(void*)&(x->status),&enum_RCS_STATUS_info_struct);
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);
	x->admin_state= (enum RCS_ADMIN_STATE) cms_update_enumeration(cms,"admin_state", (int)x->admin_state,(void*)&(x->admin_state),&enum_RCS_ADMIN_STATE_info_struct);
	cms_begin_class_var(cms,"tt");
	cms_time_tracker_update(cms, ( nml_time_tracker_c_t *) &(x->tt));
	cms_end_class_var(cms,"tt");
	cms_update_int(cms,"message_length",&(x->message_length));
	cms_update_char_dla(cms,"message",x->message, &(x->message_length),80);

	cms_end_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");

}
	cms_end_base_class(cms,"RCS_STAT_MSG_V2");


	cms_end_class(cms,"MyStat2","RCS_STAT_MSG_V2");

}


/*
*	NML/CMS Update function for fwLaserStruct
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_fwLaserStruct_update(struct cms_c_struct *cms, nml_fwLaserStruct_c_t *x)
{

	cms_begin_class(cms,"fwLaserStruct",0);
	cms_update_unsigned_short_array(cms,"rangep",x->rangep,12);

	cms_end_class(cms,"fwLaserStruct",0);

}


/*
*	NML/CMS Update function for TEST_MESSAGE_BASE
*	Automatically generated by NML CodeGen Java Applet.
*/
void cms_TEST_MESSAGE_BASE_update(struct cms_c_struct *cms, nml_TEST_MESSAGE_BASE_c_t *x)
{

	cms_begin_class(cms,"TEST_MESSAGE_BASE","RCS_STAT_MSG_V2");

{

	cms_begin_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	x->status= (enum RCS_STATUS) cms_update_enumeration(cms,"status", (int)x->status,(void*)&(x->status),&enum_RCS_STATUS_info_struct);
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);
	x->admin_state= (enum RCS_ADMIN_STATE) cms_update_enumeration(cms,"admin_state", (int)x->admin_state,(void*)&(x->admin_state),&enum_RCS_ADMIN_STATE_info_struct);
	cms_begin_class_var(cms,"tt");
	cms_time_tracker_update(cms, ( nml_time_tracker_c_t *) &(x->tt));
	cms_end_class_var(cms,"tt");
	cms_update_int(cms,"message_length",&(x->message_length));
	cms_update_char_dla(cms,"message",x->message, &(x->message_length),80);

	cms_end_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");

}
	cms_end_base_class(cms,"RCS_STAT_MSG_V2");

	cms_update_int(cms,"test_message_base_var",&(x->test_message_base_var));

	cms_end_class(cms,"TEST_MESSAGE_BASE","RCS_STAT_MSG_V2");

}

