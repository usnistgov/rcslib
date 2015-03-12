/*
*	New C++ File starts here.
*	This file should be named servon_c_n.c
*/

/* Include all C language NML and CMS function prototypes. */
#include "nmlcms_c.h"

/* Include externally supplied prototypes. */
#include "servon_c_n.h"

/* Forward Function Prototypes */
#ifdef __cplusplus
extern "C" {
#endif

void cms_SERVO_CONFIG_update(struct cms_c_struct *cms, nml_SERVO_CONFIG_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_SERVO_GOTO_POINT_update(struct cms_c_struct *cms, nml_SERVO_GOTO_POINT_c_t *x);
void cms_SERVO_HALT_update(struct cms_c_struct *cms, nml_SERVO_HALT_c_t *x);
void cms_SERVO_INIT_update(struct cms_c_struct *cms, nml_SERVO_INIT_c_t *x);
void cms_SERVO_STATUS_update(struct cms_c_struct *cms, nml_SERVO_STATUS_c_t *x);
void cms_SERVO_STATUS2_update(struct cms_c_struct *cms, nml_SERVO_STATUS2_c_t *x);

#ifdef __cplusplus
}
#endif
long nml_servo_open(const char *buf, const char *proc, const char *cfg)
{
	return (long) nml_new(servo_c_format, buf,proc,cfg);
}

int  nml_servo_valid(long nml_id)
{
	return (int) nml_valid( (nml_c_t) nml_id);
}

void nml_servo_close(long nml_id)
{
	nml_free( (nml_c_t) nml_id);
}

int nml_servo_read(long nml_id)
{
	return (long) nml_read( (nml_c_t) nml_id);
}

int nml_servo_SERVO_CONFIG_write(long nml_id, const nml_SERVO_CONFIG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6001,sizeof(nml_SERVO_CONFIG_c_t));
}

nml_SERVO_CONFIG_c_t * nml_servo_SERVO_CONFIG_get_msg(long nml_id){
	return (nml_SERVO_CONFIG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_servo_SERVO_GOTO_POINT_write(long nml_id, const nml_SERVO_GOTO_POINT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6002,sizeof(nml_SERVO_GOTO_POINT_c_t));
}

nml_SERVO_GOTO_POINT_c_t * nml_servo_SERVO_GOTO_POINT_get_msg(long nml_id){
	return (nml_SERVO_GOTO_POINT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_servo_SERVO_HALT_write(long nml_id, const nml_SERVO_HALT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6003,sizeof(nml_SERVO_HALT_c_t));
}

nml_SERVO_HALT_c_t * nml_servo_SERVO_HALT_get_msg(long nml_id){
	return (nml_SERVO_HALT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_servo_SERVO_INIT_write(long nml_id, const nml_SERVO_INIT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6004,sizeof(nml_SERVO_INIT_c_t));
}

nml_SERVO_INIT_c_t * nml_servo_SERVO_INIT_get_msg(long nml_id){
	return (nml_SERVO_INIT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_servo_SERVO_STATUS_write(long nml_id, const nml_SERVO_STATUS_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6000,sizeof(nml_SERVO_STATUS_c_t));
}

nml_SERVO_STATUS_c_t * nml_servo_SERVO_STATUS_get_msg(long nml_id){
	return (nml_SERVO_STATUS_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_servo_SERVO_STATUS2_write(long nml_id, const nml_SERVO_STATUS2_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 6005,sizeof(nml_SERVO_STATUS2_c_t));
}

nml_SERVO_STATUS2_c_t * nml_servo_SERVO_STATUS2_get_msg(long nml_id){
	return (nml_SERVO_STATUS2_c_t *) nml_get_address( (nml_c_t) nml_id);
}




#ifndef MAX_SERVO_C_NAME_LENGTH
#define MAX_SERVO_C_NAME_LENGTH 17
#endif
#ifndef SERVO_C_NAME_LIST_LENGTH
#define SERVO_C_NAME_LIST_LENGTH 7
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
const char servo_c_name_list[SERVO_C_NAME_LIST_LENGTH][MAX_SERVO_C_NAME_LENGTH]= {
	"SERVO_CONFIG", /* 0,6001 */
	"SERVO_GOTO_POINT", /* 1,6002 */
	"SERVO_HALT", /* 2,6003 */
	"SERVO_INIT", /* 3,6004 */
	"SERVO_STATUS", /* 4,6000 */
	"SERVO_STATUS2", /* 5,6005 */
	""};
const NMLTYPE servo_c_id_list[SERVO_C_NAME_LIST_LENGTH]= {
	SERVO_CONFIG_TYPE, /* 0,6001 */
	SERVO_GOTO_POINT_TYPE, /* 1,6002 */
	SERVO_HALT_TYPE, /* 2,6003 */
	SERVO_INIT_TYPE, /* 3,6004 */
	SERVO_STATUS_TYPE, /* 4,6000 */
	SERVO_STATUS2_TYPE, /* 5,6005 */
	-1};
const size_t servo_c_size_list[SERVO_C_NAME_LIST_LENGTH]= {
	sizeof(nml_SERVO_CONFIG_c_t),
	sizeof(nml_SERVO_GOTO_POINT_c_t),
	sizeof(nml_SERVO_HALT_c_t),
	sizeof(nml_SERVO_INIT_c_t),
	sizeof(nml_SERVO_STATUS_c_t),
	sizeof(nml_SERVO_STATUS2_c_t),
	0};
const char *servo_c_symbol_lookup(long type);


/* Enumerated Type Constants */

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

const char *servo_c_enum_RCS_STATUS_symbol_lookup(long v)
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
	(cms_symbol_lookup_function_t)servo_c_enum_RCS_STATUS_symbol_lookup
	};

/*
*	NML/CMS Format function : servo_c_format
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
int servo_c_format(long type, void *buffer, struct cms_c_struct *cms)
{

	type = cms_check_type_info(cms,type,buffer,"servo_c",
		(cms_symbol_lookup_function_t) servo_c_symbol_lookup,
		(const char **)servo_c_name_list,
		servo_c_id_list,servo_c_size_list,
		SERVO_C_NAME_LIST_LENGTH,
		MAX_SERVO_C_NAME_LENGTH);

	switch(type)
	{
	case SERVO_CONFIG_TYPE:
		cms_SERVO_CONFIG_update(cms,(nml_SERVO_CONFIG_c_t *) buffer);
		break;
	case SERVO_GOTO_POINT_TYPE:
		cms_SERVO_GOTO_POINT_update(cms,(nml_SERVO_GOTO_POINT_c_t *) buffer);
		break;
	case SERVO_HALT_TYPE:
		cms_SERVO_HALT_update(cms,(nml_SERVO_HALT_c_t *) buffer);
		break;
	case SERVO_INIT_TYPE:
		cms_SERVO_INIT_update(cms,(nml_SERVO_INIT_c_t *) buffer);
		break;
	case SERVO_STATUS_TYPE:
		cms_SERVO_STATUS_update(cms,(nml_SERVO_STATUS_c_t *) buffer);
		break;
	case SERVO_STATUS2_TYPE:
		cms_SERVO_STATUS2_update(cms,(nml_SERVO_STATUS2_c_t *) buffer);
		break;

	default:
		return(0);
	}
	return 1;
}


/* NML Symbol Lookup Function */
const char *servo_c_symbol_lookup(long type)
{
	switch(type)
	{
	case SERVO_CONFIG_TYPE:
		return "SERVO_CONFIG";
	case SERVO_GOTO_POINT_TYPE:
		return "SERVO_GOTO_POINT";
	case SERVO_HALT_TYPE:
		return "SERVO_HALT";
	case SERVO_INIT_TYPE:
		return "SERVO_INIT";
	case SERVO_STATUS_TYPE:
		return "SERVO_STATUS";
	case SERVO_STATUS2_TYPE:
		return "SERVO_STATUS2";
	default:
		return"UNKNOWN";
		break;
	}
	return(NULL);
}

/*
*	NML/CMS Update function for SERVO_STATUS2
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_STATUS2_update(struct cms_c_struct *cms, nml_SERVO_STATUS2_c_t *x)
{

	cms_begin_class(cms,"SERVO_STATUS2","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);

	cms_end_class(cms,"SERVO_STATUS2","RCS_STAT_MSG");

}


/*
*	NML/CMS Update function for SERVO_CONFIG
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_CONFIG_update(struct cms_c_struct *cms, nml_SERVO_CONFIG_c_t *x)
{

	cms_begin_class(cms,"SERVO_CONFIG","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"SERVO_CONFIG","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for SERVO_INIT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_INIT_update(struct cms_c_struct *cms, nml_SERVO_INIT_c_t *x)
{

	cms_begin_class(cms,"SERVO_INIT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"SERVO_INIT","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for SERVO_GOTO_POINT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_GOTO_POINT_update(struct cms_c_struct *cms, nml_SERVO_GOTO_POINT_c_t *x)
{

	cms_begin_class(cms,"SERVO_GOTO_POINT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);
	cms_begin_class_var(cms,"point");
	cms_PM_CARTESIAN_update(cms, ( nml_PM_CARTESIAN_c_t *) &(x->point));
	cms_end_class_var(cms,"point");

	cms_end_class(cms,"SERVO_GOTO_POINT","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for SERVO_STATUS
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_STATUS_update(struct cms_c_struct *cms, nml_SERVO_STATUS_c_t *x)
{

	cms_begin_class(cms,"SERVO_STATUS","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);

	cms_end_class(cms,"SERVO_STATUS","RCS_STAT_MSG");

}


/*
*	NML/CMS Update function for SERVO_HALT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:19 EST 2006
*/
void cms_SERVO_HALT_update(struct cms_c_struct *cms, nml_SERVO_HALT_c_t *x)
{

	cms_begin_class(cms,"SERVO_HALT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"SERVO_HALT","RCS_CMD_MSG");

}

