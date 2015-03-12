/*
*	New C++ File starts here.
*	This file should be named robot_supern_c_n.c
*/

/* Include all C language NML and CMS function prototypes. */
#include "nmlcms_c.h"

/* Include externally supplied prototypes. */
#include "robot_supern_c_n.h"

/* Forward Function Prototypes */
#ifdef __cplusplus
extern "C" {
#endif

void cms_ROBOT_SUPER_CONFIG_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_CONFIG_c_t *x);
void cms_ROBOT_SUPER_HALT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_HALT_c_t *x);
void cms_ROBOT_SUPER_INIT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_INIT_c_t *x);
void cms_ROBOT_SUPER_RUN_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_RUN_c_t *x);
void cms_ROBOT_SUPER_STATUS_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_STATUS_c_t *x);

#ifdef __cplusplus
}
#endif
long nml_robot_super_open(const char *buf, const char *proc, const char *cfg)
{
	return (long) nml_new(robot_super_c_format, buf,proc,cfg);
}

int  nml_robot_super_valid(long nml_id)
{
	return (int) nml_valid( (nml_c_t) nml_id);
}

void nml_robot_super_close(long nml_id)
{
	nml_free( (nml_c_t) nml_id);
}

int nml_robot_super_read(long nml_id)
{
	return (long) nml_read( (nml_c_t) nml_id);
}

int nml_robot_super_ROBOT_SUPER_CONFIG_write(long nml_id, const nml_ROBOT_SUPER_CONFIG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1001,sizeof(nml_ROBOT_SUPER_CONFIG_c_t));
}

nml_ROBOT_SUPER_CONFIG_c_t * nml_robot_super_ROBOT_SUPER_CONFIG_get_msg(long nml_id){
	return (nml_ROBOT_SUPER_CONFIG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_robot_super_ROBOT_SUPER_HALT_write(long nml_id, const nml_ROBOT_SUPER_HALT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1002,sizeof(nml_ROBOT_SUPER_HALT_c_t));
}

nml_ROBOT_SUPER_HALT_c_t * nml_robot_super_ROBOT_SUPER_HALT_get_msg(long nml_id){
	return (nml_ROBOT_SUPER_HALT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_robot_super_ROBOT_SUPER_INIT_write(long nml_id, const nml_ROBOT_SUPER_INIT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1003,sizeof(nml_ROBOT_SUPER_INIT_c_t));
}

nml_ROBOT_SUPER_INIT_c_t * nml_robot_super_ROBOT_SUPER_INIT_get_msg(long nml_id){
	return (nml_ROBOT_SUPER_INIT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_robot_super_ROBOT_SUPER_RUN_write(long nml_id, const nml_ROBOT_SUPER_RUN_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1004,sizeof(nml_ROBOT_SUPER_RUN_c_t));
}

nml_ROBOT_SUPER_RUN_c_t * nml_robot_super_ROBOT_SUPER_RUN_get_msg(long nml_id){
	return (nml_ROBOT_SUPER_RUN_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_robot_super_ROBOT_SUPER_STATUS_write(long nml_id, const nml_ROBOT_SUPER_STATUS_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 1000,sizeof(nml_ROBOT_SUPER_STATUS_c_t));
}

nml_ROBOT_SUPER_STATUS_c_t * nml_robot_super_ROBOT_SUPER_STATUS_get_msg(long nml_id){
	return (nml_ROBOT_SUPER_STATUS_c_t *) nml_get_address( (nml_c_t) nml_id);
}




#ifndef MAX_ROBOT_SUPER_C_NAME_LENGTH
#define MAX_ROBOT_SUPER_C_NAME_LENGTH 19
#endif
#ifndef ROBOT_SUPER_C_NAME_LIST_LENGTH
#define ROBOT_SUPER_C_NAME_LIST_LENGTH 6
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
const char robot_super_c_name_list[ROBOT_SUPER_C_NAME_LIST_LENGTH][MAX_ROBOT_SUPER_C_NAME_LENGTH]= {
	"ROBOT_SUPER_CONFIG", /* 0,1001 */
	"ROBOT_SUPER_HALT", /* 1,1002 */
	"ROBOT_SUPER_INIT", /* 2,1003 */
	"ROBOT_SUPER_RUN", /* 3,1004 */
	"ROBOT_SUPER_STATUS", /* 4,1000 */
	""};
const NMLTYPE robot_super_c_id_list[ROBOT_SUPER_C_NAME_LIST_LENGTH]= {
	ROBOT_SUPER_CONFIG_TYPE, /* 0,1001 */
	ROBOT_SUPER_HALT_TYPE, /* 1,1002 */
	ROBOT_SUPER_INIT_TYPE, /* 2,1003 */
	ROBOT_SUPER_RUN_TYPE, /* 3,1004 */
	ROBOT_SUPER_STATUS_TYPE, /* 4,1000 */
	-1};
const size_t robot_super_c_size_list[ROBOT_SUPER_C_NAME_LIST_LENGTH]= {
	sizeof(nml_ROBOT_SUPER_CONFIG_c_t),
	sizeof(nml_ROBOT_SUPER_HALT_c_t),
	sizeof(nml_ROBOT_SUPER_INIT_c_t),
	sizeof(nml_ROBOT_SUPER_RUN_c_t),
	sizeof(nml_ROBOT_SUPER_STATUS_c_t),
	0};
const char *robot_super_c_symbol_lookup(long type);


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

const char *robot_super_c_enum_RCS_STATUS_symbol_lookup(long v)
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
	(cms_symbol_lookup_function_t)robot_super_c_enum_RCS_STATUS_symbol_lookup
	};

/*
*	NML/CMS Format function : robot_super_c_format
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
int robot_super_c_format(long type, void *buffer, struct cms_c_struct *cms)
{

	type = cms_check_type_info(cms,type,buffer,"robot_super_c",
		(cms_symbol_lookup_function_t) robot_super_c_symbol_lookup,
		(const char **)robot_super_c_name_list,
		robot_super_c_id_list,robot_super_c_size_list,
		ROBOT_SUPER_C_NAME_LIST_LENGTH,
		MAX_ROBOT_SUPER_C_NAME_LENGTH);

	switch(type)
	{
	case ROBOT_SUPER_CONFIG_TYPE:
		cms_ROBOT_SUPER_CONFIG_update(cms,(nml_ROBOT_SUPER_CONFIG_c_t *) buffer);
		break;
	case ROBOT_SUPER_HALT_TYPE:
		cms_ROBOT_SUPER_HALT_update(cms,(nml_ROBOT_SUPER_HALT_c_t *) buffer);
		break;
	case ROBOT_SUPER_INIT_TYPE:
		cms_ROBOT_SUPER_INIT_update(cms,(nml_ROBOT_SUPER_INIT_c_t *) buffer);
		break;
	case ROBOT_SUPER_RUN_TYPE:
		cms_ROBOT_SUPER_RUN_update(cms,(nml_ROBOT_SUPER_RUN_c_t *) buffer);
		break;
	case ROBOT_SUPER_STATUS_TYPE:
		cms_ROBOT_SUPER_STATUS_update(cms,(nml_ROBOT_SUPER_STATUS_c_t *) buffer);
		break;

	default:
		return(0);
	}
	return 1;
}


/* NML Symbol Lookup Function */
const char *robot_super_c_symbol_lookup(long type)
{
	switch(type)
	{
	case ROBOT_SUPER_CONFIG_TYPE:
		return "ROBOT_SUPER_CONFIG";
	case ROBOT_SUPER_HALT_TYPE:
		return "ROBOT_SUPER_HALT";
	case ROBOT_SUPER_INIT_TYPE:
		return "ROBOT_SUPER_INIT";
	case ROBOT_SUPER_RUN_TYPE:
		return "ROBOT_SUPER_RUN";
	case ROBOT_SUPER_STATUS_TYPE:
		return "ROBOT_SUPER_STATUS";
	default:
		return"UNKNOWN";
		break;
	}
	return(NULL);
}

/*
*	NML/CMS Update function for ROBOT_SUPER_CONFIG
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_ROBOT_SUPER_CONFIG_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_CONFIG_c_t *x)
{

	cms_begin_class(cms,"ROBOT_SUPER_CONFIG","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"ROBOT_SUPER_CONFIG","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for ROBOT_SUPER_HALT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_ROBOT_SUPER_HALT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_HALT_c_t *x)
{

	cms_begin_class(cms,"ROBOT_SUPER_HALT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"ROBOT_SUPER_HALT","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for ROBOT_SUPER_STATUS
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_ROBOT_SUPER_STATUS_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_STATUS_c_t *x)
{

	cms_begin_class(cms,"ROBOT_SUPER_STATUS","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);

	cms_end_class(cms,"ROBOT_SUPER_STATUS","RCS_STAT_MSG");

}


/*
*	NML/CMS Update function for ROBOT_SUPER_INIT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_ROBOT_SUPER_INIT_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_INIT_c_t *x)
{

	cms_begin_class(cms,"ROBOT_SUPER_INIT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"ROBOT_SUPER_INIT","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for ROBOT_SUPER_RUN
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_ROBOT_SUPER_RUN_update(struct cms_c_struct *cms, nml_ROBOT_SUPER_RUN_c_t *x)
{

	cms_begin_class(cms,"ROBOT_SUPER_RUN","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);
	cms_update_char_array(cms,"filename",x->filename,256);

	cms_end_class(cms,"ROBOT_SUPER_RUN","RCS_CMD_MSG");

}

