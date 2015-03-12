/*
*	New C++ File starts here.
*	This file should be named stat_msg_v2_c_n.c
*/

/* Include all C language NML and CMS function prototypes. */
#include "nmlcms_c.h"

/* Include externally supplied prototypes. */
#include "stat_msg_v2_c_n.h"

/* Forward Function Prototypes */
#ifdef __cplusplus
extern "C" {
#endif

void cms_time_tracker_update(struct cms_c_struct *cms, nml_time_tracker_c_t *x);
void cms_RCS_STAT_MSG_V2_update(struct cms_c_struct *cms, nml_RCS_STAT_MSG_V2_c_t *x);

#ifdef __cplusplus
}
#endif
long nml_RCS_STAT_MSG_open(const char *buf, const char *proc, const char *cfg)
{
	return (long) nml_new(RCS_STAT_MSG_c_format, buf,proc,cfg);
}

int  nml_RCS_STAT_MSG_valid(long nml_id)
{
	return (int) nml_valid( (nml_c_t) nml_id);
}

void nml_RCS_STAT_MSG_close(long nml_id)
{
	nml_free( (nml_c_t) nml_id);
}

int nml_RCS_STAT_MSG_read(long nml_id)
{
	return (long) nml_read( (nml_c_t) nml_id);
}




#ifndef MAX_RCS_STAT_MSG_C_NAME_LENGTH
#define MAX_RCS_STAT_MSG_C_NAME_LENGTH 1
#endif
#ifndef RCS_STAT_MSG_C_NAME_LIST_LENGTH
#define RCS_STAT_MSG_C_NAME_LIST_LENGTH 1
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
const char RCS_STAT_MSG_c_name_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH][MAX_RCS_STAT_MSG_C_NAME_LENGTH]= {
	""};
const NMLTYPE RCS_STAT_MSG_c_id_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH]= {
	-1};
const size_t RCS_STAT_MSG_c_size_list[RCS_STAT_MSG_C_NAME_LIST_LENGTH]= {
	0};
const char *RCS_STAT_MSG_c_symbol_lookup(long type);


/* Enumerated Type Constants */

/*  RCS_STATE */
#ifndef MAX_ENUM_RCS_STATE_STRING_LENGTH
#define MAX_ENUM_RCS_STATE_STRING_LENGTH 20
#endif
#ifndef ENUM_RCS_STATE_LENGTH
#define ENUM_RCS_STATE_LENGTH 54
#endif

const char enum_RCS_STATE_string_list[ENUM_RCS_STATE_LENGTH][MAX_ENUM_RCS_STATE_STRING_LENGTH]= {
	"NEW_COMMAND", /* 0,-2 */
	"NOP_STATE", /* 1,-3 */
	"S0", /* 2,0 */
	"S1", /* 3,1 */
	"S10", /* 4,10 */
	"S11", /* 5,11 */
	"S12", /* 6,12 */
	"S13", /* 7,13 */
	"S14", /* 8,14 */
	"S15", /* 9,15 */
	"S16", /* 10,16 */
	"S17", /* 11,17 */
	"S18", /* 12,18 */
	"S19", /* 13,19 */
	"S2", /* 14,2 */
	"S20", /* 15,20 */
	"S21", /* 16,21 */
	"S22", /* 17,22 */
	"S23", /* 18,23 */
	"S24", /* 19,24 */
	"S25", /* 20,25 */
	"S26", /* 21,26 */
	"S27", /* 22,27 */
	"S28", /* 23,28 */
	"S29", /* 24,29 */
	"S3", /* 25,3 */
	"S30", /* 26,30 */
	"S31", /* 27,31 */
	"S32", /* 28,32 */
	"S33", /* 29,33 */
	"S34", /* 30,34 */
	"S35", /* 31,35 */
	"S36", /* 32,36 */
	"S37", /* 33,37 */
	"S38", /* 34,38 */
	"S39", /* 35,39 */
	"S4", /* 36,4 */
	"S5", /* 37,5 */
	"S6", /* 38,6 */
	"S7", /* 39,7 */
	"S8", /* 40,8 */
	"S9", /* 41,9 */
	"SE0", /* 42,-10 */
	"SE1", /* 43,-11 */
	"SE2", /* 44,-12 */
	"SE3", /* 45,-13 */
	"SE4", /* 46,-14 */
	"SE5", /* 47,-15 */
	"SE6", /* 48,-16 */
	"SE7", /* 49,-17 */
	"SE8", /* 50,-18 */
	"SE9", /* 51,-19 */
	"UNINITIALIZED_STATE", /* 52,-1 */
	""};

const int enum_RCS_STATE_int_list[ENUM_RCS_STATE_LENGTH]= {
	NEW_COMMAND, /* 0,-2 */
	NOP_STATE, /* 1,-3 */
	S0, /* 2,0 */
	S1, /* 3,1 */
	S10, /* 4,10 */
	S11, /* 5,11 */
	S12, /* 6,12 */
	S13, /* 7,13 */
	S14, /* 8,14 */
	S15, /* 9,15 */
	S16, /* 10,16 */
	S17, /* 11,17 */
	S18, /* 12,18 */
	S19, /* 13,19 */
	S2, /* 14,2 */
	S20, /* 15,20 */
	S21, /* 16,21 */
	S22, /* 17,22 */
	S23, /* 18,23 */
	S24, /* 19,24 */
	S25, /* 20,25 */
	S26, /* 21,26 */
	S27, /* 22,27 */
	S28, /* 23,28 */
	S29, /* 24,29 */
	S3, /* 25,3 */
	S30, /* 26,30 */
	S31, /* 27,31 */
	S32, /* 28,32 */
	S33, /* 29,33 */
	S34, /* 30,34 */
	S35, /* 31,35 */
	S36, /* 32,36 */
	S37, /* 33,37 */
	S38, /* 34,38 */
	S39, /* 35,39 */
	S4, /* 36,4 */
	S5, /* 37,5 */
	S6, /* 38,6 */
	S7, /* 39,7 */
	S8, /* 40,8 */
	S9, /* 41,9 */
	SE0, /* 42,-10 */
	SE1, /* 43,-11 */
	SE2, /* 44,-12 */
	SE3, /* 45,-13 */
	SE4, /* 46,-14 */
	SE5, /* 47,-15 */
	SE6, /* 48,-16 */
	SE7, /* 49,-17 */
	SE8, /* 50,-18 */
	SE9, /* 51,-19 */
	UNINITIALIZED_STATE, /* 52,-1 */
	};

const char *RCS_STAT_MSG_c_enum_RCS_STATE_symbol_lookup(long v)
{
	switch(v)
	{
		case NEW_COMMAND: return("NEW_COMMAND"); /* -2 */
		case NOP_STATE: return("NOP_STATE"); /* -3 */
		case S0: return("S0"); /* 0 */
		case S1: return("S1"); /* 1 */
		case S10: return("S10"); /* 10 */
		case S11: return("S11"); /* 11 */
		case S12: return("S12"); /* 12 */
		case S13: return("S13"); /* 13 */
		case S14: return("S14"); /* 14 */
		case S15: return("S15"); /* 15 */
		case S16: return("S16"); /* 16 */
		case S17: return("S17"); /* 17 */
		case S18: return("S18"); /* 18 */
		case S19: return("S19"); /* 19 */
		case S2: return("S2"); /* 2 */
		case S20: return("S20"); /* 20 */
		case S21: return("S21"); /* 21 */
		case S22: return("S22"); /* 22 */
		case S23: return("S23"); /* 23 */
		case S24: return("S24"); /* 24 */
		case S25: return("S25"); /* 25 */
		case S26: return("S26"); /* 26 */
		case S27: return("S27"); /* 27 */
		case S28: return("S28"); /* 28 */
		case S29: return("S29"); /* 29 */
		case S3: return("S3"); /* 3 */
		case S30: return("S30"); /* 30 */
		case S31: return("S31"); /* 31 */
		case S32: return("S32"); /* 32 */
		case S33: return("S33"); /* 33 */
		case S34: return("S34"); /* 34 */
		case S35: return("S35"); /* 35 */
		case S36: return("S36"); /* 36 */
		case S37: return("S37"); /* 37 */
		case S38: return("S38"); /* 38 */
		case S39: return("S39"); /* 39 */
		case S4: return("S4"); /* 4 */
		case S5: return("S5"); /* 5 */
		case S6: return("S6"); /* 6 */
		case S7: return("S7"); /* 7 */
		case S8: return("S8"); /* 8 */
		case S9: return("S9"); /* 9 */
		case SE0: return("SE0"); /* -10 */
		case SE1: return("SE1"); /* -11 */
		case SE2: return("SE2"); /* -12 */
		case SE3: return("SE3"); /* -13 */
		case SE4: return("SE4"); /* -14 */
		case SE5: return("SE5"); /* -15 */
		case SE6: return("SE6"); /* -16 */
		case SE7: return("SE7"); /* -17 */
		case SE8: return("SE8"); /* -18 */
		case SE9: return("SE9"); /* -19 */
		case UNINITIALIZED_STATE: return("UNINITIALIZED_STATE"); /* -1 */
		default:break;
	}
	return(NULL);
}

const struct cms_enum_info enum_RCS_STATE_info_struct={
	"RCS_STATE",
	(const char **)enum_RCS_STATE_string_list,
	enum_RCS_STATE_int_list,
	MAX_ENUM_RCS_STATE_STRING_LENGTH,
	ENUM_RCS_STATE_LENGTH,
	(cms_symbol_lookup_function_t)RCS_STAT_MSG_c_enum_RCS_STATE_symbol_lookup
	};

/*  RCS_ADMIN_STATE */
#ifndef MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH
#define MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH 20
#endif
#ifndef ENUM_RCS_ADMIN_STATE_LENGTH
#define ENUM_RCS_ADMIN_STATE_LENGTH 4
#endif

const char enum_RCS_ADMIN_STATE_string_list[ENUM_RCS_ADMIN_STATE_LENGTH][MAX_ENUM_RCS_ADMIN_STATE_STRING_LENGTH]= {
	"ADMIN_INITIALIZED", /* 0,2 */
	"ADMIN_SHUT_DOWN", /* 1,3 */
	"ADMIN_UNINITIALIZED", /* 2,1 */
	""};

const int enum_RCS_ADMIN_STATE_int_list[ENUM_RCS_ADMIN_STATE_LENGTH]= {
	ADMIN_INITIALIZED, /* 0,2 */
	ADMIN_SHUT_DOWN, /* 1,3 */
	ADMIN_UNINITIALIZED, /* 2,1 */
	};

const char *RCS_STAT_MSG_c_enum_RCS_ADMIN_STATE_symbol_lookup(long v)
{
	switch(v)
	{
		case ADMIN_INITIALIZED: return("ADMIN_INITIALIZED"); /* 2 */
		case ADMIN_SHUT_DOWN: return("ADMIN_SHUT_DOWN"); /* 3 */
		case ADMIN_UNINITIALIZED: return("ADMIN_UNINITIALIZED"); /* 1 */
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
	(cms_symbol_lookup_function_t)RCS_STAT_MSG_c_enum_RCS_ADMIN_STATE_symbol_lookup
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

const char *RCS_STAT_MSG_c_enum_RCS_STATUS_symbol_lookup(long v)
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
	(cms_symbol_lookup_function_t)RCS_STAT_MSG_c_enum_RCS_STATUS_symbol_lookup
	};

/*
*	NML/CMS Format function : RCS_STAT_MSG_c_format
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Mar 18 16:54:54 EST 2006
*/
int RCS_STAT_MSG_c_format(long type, void *buffer, struct cms_c_struct *cms)
{

	type = cms_check_type_info(cms,type,buffer,"RCS_STAT_MSG_c",
		(cms_symbol_lookup_function_t) RCS_STAT_MSG_c_symbol_lookup,
		(const char **)RCS_STAT_MSG_c_name_list,
		RCS_STAT_MSG_c_id_list,RCS_STAT_MSG_c_size_list,
		RCS_STAT_MSG_C_NAME_LIST_LENGTH,
		MAX_RCS_STAT_MSG_C_NAME_LENGTH);

	switch(type)
	{

	default:
		return(0);
	}
	return 1;
}


/* NML Symbol Lookup Function */
const char *RCS_STAT_MSG_c_symbol_lookup(long type)
{
	switch(type)
	{
	default:
		return"UNKNOWN";
		break;
	}
	return(NULL);
}

/*
*	NML/CMS Update function for RCS_STAT_MSG_V2
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Mar 18 16:54:54 EST 2006
*/
void cms_RCS_STAT_MSG_V2_update(struct cms_c_struct *cms, nml_RCS_STAT_MSG_V2_c_t *x)
{

	cms_begin_class(cms,"RCS_STAT_MSG_V2","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
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

