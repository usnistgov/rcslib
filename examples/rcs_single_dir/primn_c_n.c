/*
*	New C++ File starts here.
*	This file should be named primn_c_n.c
*/

/* Include all C language NML and CMS function prototypes. */
#include "nmlcms_c.h"

/* Include externally supplied prototypes. */
#include "primn_c_n.h"

/* Forward Function Prototypes */
#ifdef __cplusplus
extern "C" {
#endif

void cms_PRIM_CONFIG_update(struct cms_c_struct *cms, nml_PRIM_CONFIG_c_t *x);
void cms_PM_CARTESIAN_update(struct cms_c_struct *cms, nml_PM_CARTESIAN_c_t *x);
void cms_PRIM_FOLLOW_WAYPOINTS_update(struct cms_c_struct *cms, nml_PRIM_FOLLOW_WAYPOINTS_c_t *x);
void cms_PRIM_HALT_update(struct cms_c_struct *cms, nml_PRIM_HALT_c_t *x);
void cms_PRIM_INIT_update(struct cms_c_struct *cms, nml_PRIM_INIT_c_t *x);
void cms_PRIM_STATUS_update(struct cms_c_struct *cms, nml_PRIM_STATUS_c_t *x);

#ifdef __cplusplus
}
#endif
long nml_prim_open(const char *buf, const char *proc, const char *cfg)
{
	return (long) nml_new(prim_c_format, buf,proc,cfg);
}

int  nml_prim_valid(long nml_id)
{
	return (int) nml_valid( (nml_c_t) nml_id);
}

void nml_prim_close(long nml_id)
{
	nml_free( (nml_c_t) nml_id);
}

int nml_prim_read(long nml_id)
{
	return (long) nml_read( (nml_c_t) nml_id);
}

int nml_prim_PRIM_CONFIG_write(long nml_id, const nml_PRIM_CONFIG_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 5001,sizeof(nml_PRIM_CONFIG_c_t));
}

nml_PRIM_CONFIG_c_t * nml_prim_PRIM_CONFIG_get_msg(long nml_id){
	return (nml_PRIM_CONFIG_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_prim_PRIM_FOLLOW_WAYPOINTS_write(long nml_id, const nml_PRIM_FOLLOW_WAYPOINTS_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 5002,sizeof(nml_PRIM_FOLLOW_WAYPOINTS_c_t));
}

nml_PRIM_FOLLOW_WAYPOINTS_c_t * nml_prim_PRIM_FOLLOW_WAYPOINTS_get_msg(long nml_id){
	return (nml_PRIM_FOLLOW_WAYPOINTS_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_prim_PRIM_HALT_write(long nml_id, const nml_PRIM_HALT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 5003,sizeof(nml_PRIM_HALT_c_t));
}

nml_PRIM_HALT_c_t * nml_prim_PRIM_HALT_get_msg(long nml_id){
	return (nml_PRIM_HALT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_prim_PRIM_INIT_write(long nml_id, const nml_PRIM_INIT_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 5004,sizeof(nml_PRIM_INIT_c_t));
}

nml_PRIM_INIT_c_t * nml_prim_PRIM_INIT_get_msg(long nml_id){
	return (nml_PRIM_INIT_c_t *) nml_get_address( (nml_c_t) nml_id);
}

int nml_prim_PRIM_STATUS_write(long nml_id, const nml_PRIM_STATUS_c_t *msg){
	return (int) nml_write( (nml_c_t) nml_id,(void *) msg, (nmltype_c_t) 5000,sizeof(nml_PRIM_STATUS_c_t));
}

nml_PRIM_STATUS_c_t * nml_prim_PRIM_STATUS_get_msg(long nml_id){
	return (nml_PRIM_STATUS_c_t *) nml_get_address( (nml_c_t) nml_id);
}




#ifndef MAX_PRIM_C_NAME_LENGTH
#define MAX_PRIM_C_NAME_LENGTH 22
#endif
#ifndef PRIM_C_NAME_LIST_LENGTH
#define PRIM_C_NAME_LIST_LENGTH 6
#endif


/* This list must be in alphabetical order and the three lists must correspond. */
const char prim_c_name_list[PRIM_C_NAME_LIST_LENGTH][MAX_PRIM_C_NAME_LENGTH]= {
	"PRIM_CONFIG", /* 0,5001 */
	"PRIM_FOLLOW_WAYPOINTS", /* 1,5002 */
	"PRIM_HALT", /* 2,5003 */
	"PRIM_INIT", /* 3,5004 */
	"PRIM_STATUS", /* 4,5000 */
	""};
const NMLTYPE prim_c_id_list[PRIM_C_NAME_LIST_LENGTH]= {
	PRIM_CONFIG_TYPE, /* 0,5001 */
	PRIM_FOLLOW_WAYPOINTS_TYPE, /* 1,5002 */
	PRIM_HALT_TYPE, /* 2,5003 */
	PRIM_INIT_TYPE, /* 3,5004 */
	PRIM_STATUS_TYPE, /* 4,5000 */
	-1};
const size_t prim_c_size_list[PRIM_C_NAME_LIST_LENGTH]= {
	sizeof(nml_PRIM_CONFIG_c_t),
	sizeof(nml_PRIM_FOLLOW_WAYPOINTS_c_t),
	sizeof(nml_PRIM_HALT_c_t),
	sizeof(nml_PRIM_INIT_c_t),
	sizeof(nml_PRIM_STATUS_c_t),
	0};
const char *prim_c_symbol_lookup(long type);


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

const char *prim_c_enum_RCS_STATUS_symbol_lookup(long v)
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
	(cms_symbol_lookup_function_t)prim_c_enum_RCS_STATUS_symbol_lookup
	};

/*
*	NML/CMS Format function : prim_c_format
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
int prim_c_format(long type, void *buffer, struct cms_c_struct *cms)
{

	type = cms_check_type_info(cms,type,buffer,"prim_c",
		(cms_symbol_lookup_function_t) prim_c_symbol_lookup,
		(const char **)prim_c_name_list,
		prim_c_id_list,prim_c_size_list,
		PRIM_C_NAME_LIST_LENGTH,
		MAX_PRIM_C_NAME_LENGTH);

	switch(type)
	{
	case PRIM_CONFIG_TYPE:
		cms_PRIM_CONFIG_update(cms,(nml_PRIM_CONFIG_c_t *) buffer);
		break;
	case PRIM_FOLLOW_WAYPOINTS_TYPE:
		cms_PRIM_FOLLOW_WAYPOINTS_update(cms,(nml_PRIM_FOLLOW_WAYPOINTS_c_t *) buffer);
		break;
	case PRIM_HALT_TYPE:
		cms_PRIM_HALT_update(cms,(nml_PRIM_HALT_c_t *) buffer);
		break;
	case PRIM_INIT_TYPE:
		cms_PRIM_INIT_update(cms,(nml_PRIM_INIT_c_t *) buffer);
		break;
	case PRIM_STATUS_TYPE:
		cms_PRIM_STATUS_update(cms,(nml_PRIM_STATUS_c_t *) buffer);
		break;

	default:
		return(0);
	}
	return 1;
}


/* NML Symbol Lookup Function */
const char *prim_c_symbol_lookup(long type)
{
	switch(type)
	{
	case PRIM_CONFIG_TYPE:
		return "PRIM_CONFIG";
	case PRIM_FOLLOW_WAYPOINTS_TYPE:
		return "PRIM_FOLLOW_WAYPOINTS";
	case PRIM_HALT_TYPE:
		return "PRIM_HALT";
	case PRIM_INIT_TYPE:
		return "PRIM_INIT";
	case PRIM_STATUS_TYPE:
		return "PRIM_STATUS";
	default:
		return"UNKNOWN";
		break;
	}
	return(NULL);
}

/*
*	NML/CMS Update function for PRIM_FOLLOW_WAYPOINTS
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_PRIM_FOLLOW_WAYPOINTS_update(struct cms_c_struct *cms, nml_PRIM_FOLLOW_WAYPOINTS_c_t *x)
{

	cms_begin_class(cms,"PRIM_FOLLOW_WAYPOINTS","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"waypoints_length",&(x->waypoints_length));
	cms_begin_struct_dynamic_array(cms,"waypoints",&(x->waypoints_length), 1000);

	{
		int i_waypoints=0;

			for(i_waypoints = 0;i_waypoints < x->waypoints_length; i_waypoints++)
		{
			cms_begin_struct_array_elem(cms,"waypoints",i_waypoints);
			cms_PM_CARTESIAN_update(cms,&(( x->waypoints)[i_waypoints]));
			cms_end_struct_array_elem(cms,"waypoints",i_waypoints);
		}
	}

	cms_end_struct_dynamic_array(cms,"waypoints",&(x->waypoints_length), 1000);

	cms_end_class(cms,"PRIM_FOLLOW_WAYPOINTS","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for PRIM_INIT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_PRIM_INIT_update(struct cms_c_struct *cms, nml_PRIM_INIT_c_t *x)
{

	cms_begin_class(cms,"PRIM_INIT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"PRIM_INIT","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for PRIM_CONFIG
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_PRIM_CONFIG_update(struct cms_c_struct *cms, nml_PRIM_CONFIG_c_t *x)
{

	cms_begin_class(cms,"PRIM_CONFIG","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"PRIM_CONFIG","RCS_CMD_MSG");

}


/*
*	NML/CMS Update function for PRIM_STATUS
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_PRIM_STATUS_update(struct cms_c_struct *cms, nml_PRIM_STATUS_c_t *x)
{

	cms_begin_class(cms,"PRIM_STATUS","RCS_STAT_MSG");
	cms_begin_update_stat_msg_base(cms,(void*)x);
	cms_update_long(cms,"command_type",&(x->command_type));
	cms_update_int(cms,"echo_serial_number",&(x->echo_serial_number));
	cms_update_int(cms,"status",&(x->status));
	cms_update_int(cms,"state",&(x->state));
	cms_update_int(cms,"source_line",&(x->source_line));
	cms_update_char_array(cms,"source_file",(x->source_file),64);
	cms_end_update_stat_msg_base(cms,(void*)x);

	cms_end_class(cms,"PRIM_STATUS","RCS_STAT_MSG");

}


/*
*	NML/CMS Update function for PRIM_HALT
*	Automatically generated by NML CodeGen Java Applet.
*	on Sat Feb 25 10:10:18 EST 2006
*/
void cms_PRIM_HALT_update(struct cms_c_struct *cms, nml_PRIM_HALT_c_t *x)
{

	cms_begin_class(cms,"PRIM_HALT","RCS_CMD_MSG");
	cms_begin_update_cmd_msg_base(cms,(void*)x);
	cms_update_int(cms,"serial_number",&(x->serial_number));	cms_end_update_cmd_msg_base(cms,(void*)x);

	cms_end_class(cms,"PRIM_HALT","RCS_CMD_MSG");

}

