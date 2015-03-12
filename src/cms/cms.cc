/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.

 */

/*************************************************************************
 * File: cms.cc
 * Authors: Fred Proctor, Will Shackleford
 * Purpose: C++ file for the  Communication Management System (CMS).
 *          Includes:
 *                    1. Member functions for class CMS.
 * NOTES:
 * See cms_in.cc for the internal interface member functions and cms_up.cc
 * for the update functions.
 *************************************************************************/

#define NEED_VPRINT 1
#define CMS_DERIVED_CLASS 1

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "cms_no_config.h"
#endif


#ifdef VXWORKS
static const unsigned char memory_align_default = 4;
#else
static const unsigned char memory_align_default = 0;
#endif


// HAVE_CONFIG_H

#include "cms.hh"		/* class CMS */
#include "cms_up.hh"		/* class CMS_UPDATER */
#include "cms_enum_info.h" 	/* struct cms_enum_info; */
#include "cms_cfg.hh"

#include "physmem.hh"		// class PHYSMEM_HANDLE
#include "linklist.hh"		// class RCS_LINKED_LIST

#ifdef ENABLE_RCS_XDR
#include "cms_xup.hh"		/* class CMS_XDR_UPDATER */
#endif

#ifdef ENABLE_PACKED_UP
#include "cms_pup.hh"		/* class CMS_PACKED_UPDATER */
#endif

#ifdef ENABLE_RCS_XML
#include "cms_xml_up.hh"	// class CMS_XML_UPDATER
#endif

#if 0
#include "cms_aup.hh"		/* class CMS_ASCII_UPDATER  */
#endif

#ifdef ENABLE_RCS_DISP
#include "cms_dup.hh"		/* class CMS_DISPLAY_ASCII_UPDATER  */
#endif

#include "rcs_prnt.hh"
#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */

#if ENABLE_RCS_CMS_MRPQ
#include "cms_mrpq.hh" 		//  class CMS_MULTIREADER_PRIORITY_QUEUE
#endif

#ifdef ENABLE_RCS_DIAG
#include "cmsdiag.hh"
#endif

#ifdef ENABLE_RCS_SOKINTRF
#include "sokintrf.h"
#endif

int cms_force_http_port = 0;
int cms_http_show_files = 0;

int last_port_number_set = -1;
enum CMS_REMOTE_PORT_TYPE last_port_type_set = CMS_NO_REMOTE_PORT_TYPE;

static bool connection_over_warning_given = false;
static double global_min_compatible_version = -1.0;
static bool global_min_compatible_version_env_checked = false;

RCS_LINKED_LIST *cmsHostAliases = NULL;
CMS_CONNECTION_MODE cms_connection_mode = CMS_NORMAL_CONNECTION_MODE;

/* Static Class Data Members. */
int CMS::number_of_cms_objects = 0;
int cms_encoded_data_explosion_factor = 4;

static const char *cms_type_string[] = {
            "none",
            "bool",
            "uchar",
            "char",
            "ushort",
            "short",
            "uint",
            "int",
            "ulong",
            "long",
            "float",
            "double",
            "ldouble",
            "enumeration",
            "bool_array",
            "uchar_array",
            "char_array",
            "ushort_array",
            "short_array",
            "uint_array",
            "int_array",
            "ulong_array",
            "long_array",
            "float_array",
            "double_array",
            "ldouble_array",
            "enumeration_array",
            "last"
        };

#if 0

static int
convert2lower(char *dest, char *src, unsigned int len) {
    int
    i;
    for (i = 0; i < len; i++) {
        if (src[i] == 0) {
            dest[i] = 0;
            return i;
        }
        dest[i] = tolower(src[i]);
    }
    return i;
}
#endif

static int
convert2upper(char *dest, const char *src, unsigned int len) {
    unsigned int
    i;

    for (i = 0; i < len; i++) {
        if (src[i] == 0) {
            dest[i] = 0;
            return i;
        }
        dest[i] = (char) toupper(src[i]);
    }
    return i;
}


/* Class CMS Member Functions */
#if 0

void *
CMS::operator
new (size_t new_arg_size) {
    if (new_arg_size < sizeof (CMS)) {
        cms_print_error
                ("CMS::operator new -- The size requested %lu is less than the mininimum size of CMS %lu.\n",
                (unsigned long) new_arg_size, (unsigned long) sizeof (CMS));
        cms_print_error("This could indicate a version mismatch problem.\n");
        new_arg_size = sizeof (CMS);
    }
    void *space = (void *) DEBUG_MALLOC(new_arg_size);
    if (NULL != space) {
        memset(space, 0, new_arg_size);
    }
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "%p = CMS::new(%lu)\n", space,
            (unsigned long) new_arg_size);
    return space;
}

void
CMS::operator
delete (void *space) {
    rcs_print_debug(PRINT_CMS_DESTRUCTORS, " CMS::delete(%p)\n", space);
    DEBUG_FREE(space);
    rcs_print_debug(PRINT_CMS_DESTRUCTORS, " CMS::delete successful.\n");
}
#endif


/* Constructor used by cms_config. */
/* Parameters:  */
/*  bufline - The buffer line from a CMS configuration file. */
/* procline - The process line from a CMS configuration file. */
/* set_to_server - */
/* -1 force this CMS object NOT to be in server mode. */
/* 0 allow the parameter in the procline to set whether server mode is used */

/* 1 force this CMS object to be in server mode. */
CMS::CMS(const char *bufline, const char *procline, int set_to_server) :
consecutive_timeouts(0), header(), queuing_header(), mode(CMS_NOT_A_MODE),
size(0), free_space(0), max_message_size(0), max_encoded_message_size(0),
temp_max_encoded_message_size(0), restore_max_encoded_message_size(0),
guaranteed_message_space(0), status(CMS_STATUS_NOT_SET), encoded_data(0),
temp_encoded_data(0),
restore_encoded_data(0),
zero_encoded_data_when_set(0), data(0), subdiv_data(0), toggle_bit(0),
first_read_done(0), first_write_done(0), write_permission_flag(0),
read_permission_flag(0), rpc_program_number(0), http_port_number(0),
tcp_port_number(0), stcp_port_number(0),
udp_port_number(0), gdrs_im_port_number(0),
buffer_number(0),
total_messages_missed(0), messages_missed_on_last_read(0), format_low_ptr(0),
format_high_ptr(0), format_size(0), BufferType(CMS_SHMEM_TYPE),
ProcessType(CMS_AUTO_TYPE), remote_port_type(CMS_NO_REMOTE_PORT_TYPE),
pointer_check_disabled(0), in_buffer_id(0), encoded_header(0),
encoded_queuing_header(0), encoded_header_size(0),
encoded_queuing_header_size(0), neutral_encoding_method(CMS_NO_ENCODING),
temp_updater_encoding_method(CMS_NO_ENCODING),
internal_access_type(CMS_ZERO_ACCESS), handle_to_global_data(0),
dummy_handle(0), read_mode(CMS_NOT_A_MODE), write_mode(CMS_NOT_A_MODE),
read_updater_mode(0), write_updater_mode(0), last_im(CMS_NOT_A_MODE),
timeout(0), orig_timeout(0),
read_timeout(0), write_timeout(0),
connect_timeout(0),
connection_number(0), total_connections(0),
updater(0), normal_updater(0), temp_updater(0), encode_state(0),
decode_state(0), sizeof_message_header(0), blocking_timeout(0),
orig_blocking_timeout(0), max_repeat_blocking_reads(0),
blocking_read_start(0), min_compatible_version(0),
confirm_write(0), disable_final_write_raw_for_dma(0),
total_subdivisions(0), current_subdivision(0), subdiv_size(0),
encoded_data_size(0), no_unbounded(0), temp_encoded_data_size(0),
restore_encoded_data_size(0), enc_max_size(0),
enable_diagnostics(0), first_diag_store(0),
pre_op_total_bytes_moved(0), time_bias(0), skip_area(0),
half_offset(0), half_size(0), size_without_diagnostics(0),
disable_diag_store(0), diag_offset(0),
last_id_side0(0), last_id_side1(0), use_autokey_for_connection_number(0),
update_cmd_msg_base(0), update_cmd_msg_base_in_format(0),
update_stat_msg_base(0), update_stat_msg_base_in_format(0),
bitwise_op(CMS_BITWISE_NOP), transfer_alias_list(0),
current_alias(0), copymode(0), endofcopybuf(0),
maxendofcopybuf(0),
copybuff(0), copybuff_size(0), xbase(0),
copymode_unbounded_struct_array_xbases(0),
copymode_unbounded_struct_array_copybuffs(0),
copymode_unbounded_struct_array_copybuff_sizes(0),
copymode_unbounded_struct_array_count(0),
max_copymode_unbounded_struct_array_count(0),
nmltypename(0),
nmlcfgsvr(0), xml_style_properties(0),
global_xml_style_properties_count(0),
spawn_server(0), mrpq(0), priority(0), default_priority(0), num_readers(0),
queue_length_to_wait_for(0), read_count(0), is_clear(0),
starting_wait_for_write_id(0), starting_wait_for_was_read(0),
starting_wait_for_queue_length(0), starting_wait_for_queue_head(0),
max_queue_length(0), max_size_from_size_list(0), extra_data(0), min_message_size(0), message_size_add(0), message_size_roundup(0),
use_ipv6(0),
di(0), dpi(0),
enable_xml_logging(false),
enable_xml_differencing(false),
fatal_error_occurred(false), add_array_indexes_to_name(false),
unbounded_used(false), write_just_completed(false), isserver(false),
is_phantom(false), delete_totally(false), is_local_master(false),
force_raw(false), split_buffer(false),
using_external_encoded_data(false),
restore_using_external_encoded_data(false),
neutral(false), queuing_enabled(false), fast_mode(false),
interrupting_operation(false), leave_resource(false),
multireader_priority_queue_enabled(false),
priority_set(false), preserve_mrpq_reader_id(false),
waiting_for_queue_length_over(false), wait_for_initialized(false),
blocking_support_enabled(false),
keep_read_count(false), stop_when_connection_refused(false),
max_size_from_size_list_set(false),
searching_for_max_size_from_size_list(false),
wait_for_master(false),
private_server_object(false),
fail_on_overflow(false),
cleaning_flag(false),
bind_proc_host(false),
looking_for_dvar(false),
inside_wrong_struct(false),
dvar_found(false),
cloned_buffer(false),
ignore_cloned_buff(false),
do_cloning(false),
no_verify_buf(false),
ignore_connect_err(false),
var_to_look_for(0),
varname_only_to_look_for(0),
dvar(0.0),
var_struct_to_look_for_len(0),
get_msg_start_only(false),
temp_data_size(0),
memory_align(memory_align_default),
enable_message_memory_map(false),
message_memory_map_cP(0),
end_message_memory_map_cP(0),
message_memory_map_max_size(0),
var_type(cms_update_var_type_none),
memory_map_use_raw(false),
max_message_size_set_on_buffer_line(false),
skip_print_memory_message_map(false),
memory_map_pos_offset_from(0),
set_memory_map_pos_offset_from_to_next_var(false),
memory_map_offset(0),
var_arraylen(0),
checking_for_nsname_from_format_check_type_info(false),
nsname_from_format_check_type_info(0),
name_to_lookup_type_id(0),
type_id_looked_up(-1),
do_not_print_errors(false),
do_not_print_timeout_errors(false),
queuing_header_offset(0),
header_file_name(0),
uses_unbounded(false)
{

    bool encoding_explicitly_set;
    char *word[32]; /* Array of pointers to strings.  */
    char *buffer_type_name; /* pointer to buffer type name from bufline */
    char *proc_type_name; /* pointer to process type  from procline */
    int i;
    bool could_not_use_encoding_type;
    could_not_use_encoding_type = false;
    encoding_explicitly_set = false;

    memset(cur_var_struct, 0, sizeof (cur_var_struct));
    memset(tbuf, 0, sizeof (tbuf));

    if (getenv("DO_NOT_PRINT_NML_QUEUE_FULL")  ||
	getenv("DO_NOT_PRINT_CMS_QUEUE_FULL"))
      {
	cms_print_queue_full_messages=0;
      }

    if (getenv("IPV6") || getenv("NML_IPV6") || getenv("CMS_IPV6")) {
        use_ipv6 = 1;
    }

#if defined(ENABLE_RCS_PRINT) || defined(ENABLE_RCS_PRNT)
    init_rcs_print_mode_flags();
#ifndef DISABLE_RCS_DEBUG_PRINT
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "CMS::CMS (bufline=%s,procline=%s,set_to_server=%d)",
            bufline,
            procline,
            set_to_server);
#endif
#endif

    stop_when_connection_refused = false;
    blocking_support_enabled = false;
    preserve_mrpq_reader_id = false;
    multireader_priority_queue_enabled = false;
    mrpq = 0;
    bitwise_op = CMS_BITWISE_NOP;
    priority = 0;
    default_priority = 0;
    num_readers = 1;
    priority_set = false;
    interrupting_operation = false;
    leave_resource = false;
    current_alias = 0;
    transfer_alias_list = 0;
    add_array_indexes_to_name = true;
    copybuff = 0;
    copybuff_size = 0;
    xbase = 0;
    nmlcfgsvr = 0;
    nmltypename = 0;
    xml_style_properties = 0;
    global_xml_style_properties_count = 0;
    waiting_for_queue_length_over = false;
    queue_length_to_wait_for = 0;
    wait_for_initialized = false;
    max_queue_length = -1;
    searching_for_max_size_from_size_list = false;
    max_size_from_size_list_set = false;
    max_size_from_size_list = 0;
    extra_data = 0;
    wait_for_master = false;
    private_server_object = false;
    fail_on_overflow = false;
    cleaning_flag = false;
    bind_proc_host = false;

    copymode_unbounded_struct_array_xbases = 0;
    copymode_unbounded_struct_array_copybuffs = 0;
    copymode_unbounded_struct_array_copybuff_sizes = 0;
    copymode_unbounded_struct_array_count = 0;
    max_copymode_unbounded_struct_array_count = 0;

    sizeof_message_header = 0;
    min_compatible_version = 0;
    if (!global_min_compatible_version_env_checked) {
#if HAVE_GETENV
        char *rcs_version_env = getenv("RCS_VERSION");
        if (rcs_version_env && *rcs_version_env) {
            global_min_compatible_version = strtod(rcs_version_env, 0);
        }
#endif
        global_min_compatible_version_env_checked = true;
    }
    if (global_min_compatible_version > 0) {
        min_compatible_version = global_min_compatible_version;
    }

#if ENABLE_PACKED_UP
    if (min_compatible_version <= 0 || min_compatible_version > 2004.1) {
        neutral_encoding_method = CMS_PACKED_ENCODING;
    }
#else
    neutral_encoding_method = CMS_XDR_ENCODING;
#endif
    force_raw = false;
    confirm_write = 0;
    disable_final_write_raw_for_dma = 0;

    /* Init string buffers */
    memset(BufferName, 0, CMS_CONFIG_LINELEN);
    memset(BufferHost, 0, CMS_CONFIG_LINELEN);
    memset(ProcessName, 0, CMS_CONFIG_LINELEN);
    memset(BufferLine, 0, CMS_CONFIG_LINELEN);
    memset(ProcessLine, 0, CMS_CONFIG_LINELEN);
    memset(ProcessHost, 0, CMS_CONFIG_LINELEN);
    memset(buflineupper, 0, CMS_CONFIG_LINELEN);
    memset(proclineupper, 0, CMS_CONFIG_LINELEN);
    memset(PermissionString, 0, CMS_CONFIG_LINELEN);

    /* Initailize some variables. */
    read_permission_flag = 0; /* Allow both read and write by default.  */
    write_permission_flag = 0;
    queuing_enabled = false;
    fatal_error_occurred = false;
    write_just_completed = false;


    sizeof_message_header = 0;
    blocking_timeout = 0;
    orig_blocking_timeout = 0;
    blocking_read_start = 0;
    max_repeat_blocking_reads = -1;


    enc_max_size = -1;
    max_encoded_message_size = 0;
    enable_diagnostics = 0;
    dpi = NULL;
    di = NULL;
    disable_diag_store = 0;
    diag_offset = 0;
    use_autokey_for_connection_number = 0;
    enable_xml_logging = 0;
    enable_xml_differencing = false;
    temp_updater = (CMS_UPDATER *) NULL;
    updater = 0;
    zero_encoded_data_when_set = 1;
    temp_encoded_data = 0;
    temp_encoded_data_size = 0;
    temp_max_encoded_message_size = 0;
    restore_encoded_data_size = 0;
    restore_encoded_data = 0;
    restore_max_encoded_message_size = 0;
    restore_using_external_encoded_data = false;
    update_cmd_msg_base = 0;
    update_cmd_msg_base_in_format = 1;
    update_stat_msg_base = 0;
    update_stat_msg_base_in_format = 1;


    if ((NULL == bufline) || (NULL == procline)) {
        cms_print_error("CMS: Pointer to bufline or procline is NULL.\n");
        return;
    }

    convert2upper(buflineupper, bufline, CMS_CONFIG_LINELEN);
    convert2upper(proclineupper, procline, CMS_CONFIG_LINELEN);

    is_phantom = false;
    max_message_size = 0;
    using_external_encoded_data = false;
    in_buffer_id = 0;
    last_id_side0 = 0;
    last_id_side1 = 0;
    delete_totally = false;
    queuing_enabled = false;
    split_buffer = false;
    fatal_error_occurred = false;
    consecutive_timeouts = 0;
    write_just_completed = false;
    pointer_check_disabled = 0;
    blocking_timeout = 0;
    orig_blocking_timeout = 0;
    blocking_read_start = 0;
    max_repeat_blocking_reads = -1;

    last_im = CMS_NOT_A_MODE;
    total_subdivisions = 1;
    size = 0;
    subdiv_size = 0;
    current_subdivision = 0;
    max_encoded_message_size = 0;
    skip_area = 0;
    half_offset = 0;
    half_size = 0;
    fast_mode = false;
    last_id_side0 = 0;
    last_id_side1 = 0;
    free_space = 0;
    handle_to_global_data = NULL;


    dummy_handle = (PHYSMEM_HANDLE *) NULL;
    remote_port_type = CMS_NO_REMOTE_PORT_TYPE;
    for (i = 0; i < 32; i++) {
        word[i] = (char *) NULL;
    }

    /* Store the bufline and procline for debugging later. */
    strcpy(BufferLine, bufline);
    strcpy(ProcessLine, procline);

    int num_words=0;

    num_words = 
      separate_words_with_buffer(word, 9, bufline, WordBuffer, sizeof (WordBuffer));
    /* Get parameters from the buffer's line in the config file. */
    if (num_words != 9) {
      cms_print_error("CMS: Error in buffer line from config file. (num_words=%d != 9)\n",num_words);
      cms_print_error("%s\n", bufline);
      status = CMS_CONFIG_ERROR;
      return;
    }

    /* Use the words from the buffer line to initialize some class variables. */
    strcpy(BufferName, word[1]);
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "new CMS (%s)\n", BufferName);

    /* Clear errno so we can determine if all of the parameters in the */
    /* buffer line were in an acceptable form. */
    if (errno == ERANGE) {
        errno = 0;
    }
    char *realname = cms_check_for_host_alias(word[3]);
    if (realname == NULL) {
        strcpy(BufferHost, word[3]);
    } else {
        strcpy(BufferHost, realname);
    }

    buffer_type_name = word[2];


    /* strtol should allow us to use the C syntax for specifying the radix of */
    /* the numbers in the configuration file. */
    /* (i.e. 0x???? for hexidecimal, 0??? for octal and ???? for decimal.) */
    size = (long) strtol(word[4], (char **) NULL, 0);
    neutral = (bool) (strtol(word[5], (char **) NULL, 0) != 0);
    rpc_program_number = 0;
    buffer_number = strtol(word[7], (char **) NULL, 0);
    total_connections = 0;
    if (word[8][0] >= '0' && word[8][0] <= '9') {
        total_connections = strtol(word[8], (char **) NULL, 0);
    }
    free_space = size;

    /* Check errno to see if all of the strtol's were sucessful. */
    if (ERANGE == errno) {
        cms_print_error("CMS: Error in buffer line from config file.\n");
        cms_print_error("%s\n", bufline);
        status = CMS_CONFIG_ERROR;
        return;
    }

    /* Determine the BufferType. */
    if (!strcmp(buffer_type_name, "SHMEM")) {
        BufferType = CMS_SHMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "GLOBMEM")) {
        BufferType = CMS_GLOBMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "OEMEM")) {
        BufferType = CMS_OEMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "BBDMEM")) {
        BufferType = CMS_BBDMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "PHANTOM")) {
        BufferType = CMS_PHANTOM_BUFFER;
        is_phantom = true;
    } else if (!strcmp(buffer_type_name, "LOCMEM")) {
        BufferType = CMS_LOCMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "FILEMEM")) {
        BufferType = CMS_FILEMEM_TYPE;
    } else if (!strcmp(buffer_type_name, "RTLMEM")) {
        BufferType = CMS_RTLMEM_TYPE;
    } else {
        cms_print_error("CMS: invalid buffer type (%s)\n", buffer_type_name);
        status = CMS_CONFIG_ERROR;
        return;
    }

    for (i = 0; i < 32; i++) {
        word[i] = (char *) NULL;
    }

    num_words = separate_words_with_buffer(word, 32, buflineupper, WordBuffer, sizeof (WordBuffer));
    if (num_words < 8) {
      cms_print_error("CMS: Error in buffer line from config file. (num_words=%d < 8)\n",num_words);
        cms_print_error("%s\n", bufline);
        status = CMS_CONFIG_ERROR;
        return;
    }
    for (i = 9; i < num_words && i < 32; i++) {
        if (word[i] == NULL) {
            break;
        }

        if (!strncmp(word[i], "VME_ADDR=", 9)) {
            continue;
        }

        if (!strncmp(word[i], "NO_TASK_LOCK", 12)) {
            continue;
        }

        if (!strcmp(word[i], "QUEUE")) {
            queuing_enabled = true;
            continue;
        }

        if (!strncmp(word[i], "HEADER=", 7)) {
            continue;
        }

        if (!strcmp(word[i], "PRIORITY_QUEUE")) {
#if ENABLE_RCS_CMS_MRPQ
            queuing_enabled = true;
            multireader_priority_queue_enabled = true;
#else
            cms_print_error("Priority queue requested but support for this was not compiled into rcslib.\n");
            status = CMS_CONFIG_ERROR;
#endif
            continue;
        }

        if (!strncmp(word[i], "MAX_MESSAGE_SIZE=", 17)) {
            max_message_size = strtol(word[i] + 17, 0, 0);
	    if(max_message_size < 1 || max_message_size > size)
	      {
		cms_print_error("Invalid MAX_MESSAGE_SIZE=%ld (%s) \n", 
				max_message_size, word[i]);
		status = CMS_CONFIG_ERROR;
	      }
	    max_message_size_set_on_buffer_line=true;
            continue;
        }

        if (!strncmp(word[i], "MAX_QUEUE_LENGTH=", 17)) {
            max_queue_length = strtol(word[i] + 17, 0, 0);
            continue;
        }

        if (!strncmp(word[i], "DEFAULT_PRIORITY=", 17)) {
            default_priority = strtol(word[i] + 17, 0, 0);
            rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "default_priority=%d\n", default_priority);
            continue;
        }

        if (!strncmp(word[i], "MULTI_QUEUE_READERS=", 20)) {
#if ENABLE_RCS_CMS_MRPQ
            num_readers = strtol(word[i] + 20, 0, 0);
            rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "num_readers=%d\n", num_readers);
            queuing_enabled = true;
            multireader_priority_queue_enabled = true;
#else
            cms_print_error("Multi reader queue requested but support for this was not compiled into rcslib.\n");
            status = CMS_CONFIG_ERROR;
#endif	  
            continue;
        }

        if (!strcmp(word[i], "DIAG")) {
#ifdef ENABLE_RCS_DIAG
            enable_diagnostics = 1;
            continue;
#else
            cms_print_error("DIAG support was not selected when the RCS library was compiled.\n");
#endif
        }

        if (!strcmp(word[i], "IPV6")) {
            use_ipv6 = 1;
            continue;
        }

        if (!strcmp(word[i], "USE_IPV6")) {
            use_ipv6 = 1;
            continue;
        }

        if (!strcmp(word[i], "DO_NOT_PRINT_ERRORS")) {
            do_not_print_errors=true;
	    do_not_print_timeout_errors=true;
            continue;
        }

        if (!strcmp(word[i], "DO_NOT_PRINT_TIMEOUT_ERRORS")) {
	    do_not_print_timeout_errors=true;
            continue;
        }

        if (!strncmp(word[i], "MEMORY_ALIGN=", 13)) {
            memory_align = (unsigned char) atoi(word[i] + 13);
            continue;
        }
        if (!strcmp(word[i], "SPLIT")) {
            split_buffer = true;
            continue;
        }
        if (!strcmp(word[i], "DISP")) {
#ifdef ENABLE_RCS_DISP
            neutral_encoding_method = CMS_DISPLAY_ASCII_ENCODING;
            encoding_explicitly_set = true;
#else
            cms_print_error("DISP support was not selected when the RCS library was compiled.\n");
            status = CMS_CONFIG_ERROR;
#endif
            continue;
        }
        if (!strcmp(word[i], "ASCII")) {
            cms_print_error("CMS: Configuration option \"ASCII\" no longer supported.\n");
#if 0
            neutral_encoding_method = CMS_ASCII_ENCODING;
#endif
            continue;
        }
        if (!strcmp(word[i], "XDR")) {
#ifdef ENABLE_RCS_XDR
            neutral_encoding_method = CMS_XDR_ENCODING;
            encoding_explicitly_set = true;
#else
            rcs_print_warning("XDR support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
            continue;
        }

        if (!strcmp(word[i], "FAIL_ON_OVERFLOW")) {
            fail_on_overflow = true;
        }

        if (!strcmp(word[i], "CLEANING_FLAG")) {
            cleaning_flag = true;
        }

        if (!strcmp(word[i], "PACKED")) {
#ifdef ENABLE_PACKED_UP
            neutral_encoding_method = CMS_PACKED_ENCODING;
            encoding_explicitly_set = true;
#else
            rcs_print_warning("PACKED updater support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
            continue;
        }

        if (!strcmp(word[i], "PACKEDL64")) {
#if defined(ENABLE_PACKED_UP) && defined(ENABLE_RCS_PACKEDL64)
            neutral_encoding_method = CMS_PACKEDL64_ENCODING;
            encoding_explicitly_set = true;
#else
            rcs_print_warning("PACKEDL64 updater support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
            continue;
        }

        if (!strcmp(word[i], "XML")) {
#ifdef ENABLE_RCS_XML
            neutral_encoding_method = CMS_XML_ENCODING;
            encoding_explicitly_set = true;
            continue;
#else
            rcs_print_warning("XML support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
        }

        if (!strcmp(word[i], "LOGXML") || !strcmp(word[i], "XMLLOG")) {
#ifdef ENABLE_RCS_XML
            enable_xml_logging = 1;
            continue;
#else
            cms_print_error("XML support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
        }
        if (!strcmp(word[i], "DIFFXML") || !strcmp(word[i], "XMLDIFF")) {
#ifdef ENABLE_RCS_XML
            enable_xml_differencing = true;
            encoding_explicitly_set = true;
            neutral_encoding_method = CMS_XML_ENCODING;
            continue;
#else
            cms_print_error("XML support was not selected when the RCS library was compiled.\n");
            could_not_use_encoding_type = true;
#endif
        }

        if (!strncmp(word[i], "NMLCFGSVR=", 10)) {
            nmlcfgsvr = strdup(word[i] + 10);
        }

        if (!strncmp(word[i], "CLONED_ON=", 10)) {
#if defined(ENABLE_RCS_SOKINTRF) && !defined(WIN32)
            char *cloned_on_host = word[i] + 10;
            cloned_buffer = dl_address_is_local(cloned_on_host, 0) && !dl_address_is_local(BufferHost, 0);
#else
            cms_print_error("Buffer cloning not supported on the current platform/configuration.\n");
            cloned_buffer = false;
#endif
            continue;
        }


        char *port_string;
        if (NULL != (port_string = strstr(word[i], "STCP="))) {
            remote_port_type = CMS_STCP_REMOTE_PORT_TYPE;
            stcp_port_number =
                    (int) strtol(port_string + 5, (char **) NULL, 0);
            last_port_number_set = stcp_port_number;
            last_port_type_set = remote_port_type;
            continue;
        } else if (NULL != (port_string = strstr(word[i], "TCP="))) {
            remote_port_type = CMS_TCP_REMOTE_PORT_TYPE;
            tcp_port_number = (int) strtol(port_string + 4, (char **) NULL, 0);
            last_port_number_set = tcp_port_number;
            last_port_type_set = remote_port_type;
            continue;
        } else if (NULL != (port_string = strstr(word[i], "HTTP="))) {
            remote_port_type = CMS_HTTP_REMOTE_PORT_TYPE;
            http_port_number =
                    (int) strtol(port_string + 5, (char **) NULL, 0);
            last_port_number_set = http_port_number;
            last_port_type_set = remote_port_type;
            continue;
        } else if (NULL != (port_string = strstr(word[i], "UDP="))) {
            remote_port_type = CMS_UDP_REMOTE_PORT_TYPE;
            udp_port_number = (int) strtol(port_string + 4, (char **) NULL, 0);
            last_port_number_set = udp_port_number;
            last_port_type_set = remote_port_type;
            continue;
        }
        else if (NULL != (port_string = strstr(word[i], "GDRS_IM="))) {
            remote_port_type = CMS_GDRS_IM_REMOTE_PORT_TYPE;
            gdrs_im_port_number = (int) strtol(port_string + 8, (char **) NULL, 0);
            last_port_number_set = gdrs_im_port_number;
            last_port_type_set = remote_port_type;
            continue;
        }

        char *version_string;
        if (NULL != (version_string = strstr(word[i], "VERSION="))) {
            min_compatible_version =
                    strtod(version_string + 8, (char **) NULL);
            if (min_compatible_version > 0 && min_compatible_version < 2004.1) {
                if (!encoding_explicitly_set) {
                    neutral_encoding_method = CMS_XDR_ENCODING;
                }
            }
            continue;
        }

        char *subdiv_string;
        if (NULL != (subdiv_string = strstr(word[i], "SUBDIV="))) {
            total_subdivisions = strtol(subdiv_string + 7, (char **) NULL, 0);
            subdiv_size = size / total_subdivisions;
            subdiv_size -= subdiv_size % 4;
            continue;
        }

        char *enc_max_string;
        if (NULL != (enc_max_string = strstr(word[i], "ENC_MAX_SIZE="))) {
            enc_max_size = strtoul(enc_max_string + 13, (char **) NULL, 0);
            continue;
        }

        char *max_blocking_retries_string;
        if (NULL != (max_blocking_retries_string = strstr(word[i], "MAX_BLOCKING_RETRIES="))) {
            max_repeat_blocking_reads = strtoul(max_blocking_retries_string, 0, 0);
            continue;
        }

        if (NULL != strstr(word[i], "STOPONCONNREF")) {
            stop_when_connection_refused = true;
            continue;
        }

        if (!strcmp(word[i], "CONFIRM_WRITE=0")) {
            confirm_write = 0;
            continue;
        } else if (!strcmp(word[i], "CONFIRM_WRITE")) {
            confirm_write = 1;
            continue;
        }
        if (!strcmp(word[i], "FORCE_RAW")) {
            force_raw = true;
            continue;
        }
        if (!strncmp(word[i], "BSEM=", 4)) {
            continue;
        }
        if (!strcmp(word[i], "DO_NOT_TRACK_CLIENT_UDP_PORTS")) {
            // This is parsed in udp_srv.cc but here we just check for it to
            // skip the warning.
            continue;
        }
        if (!strcmp(word[i], "AUTOCNUM")) {
            use_autokey_for_connection_number = 1;
            continue;
        }
        if (!strcmp(word[i], "*")) {
            continue;
        }
        if (!strncmp(word[i], "NMLCFGSVR=", 10)) {
            continue;
        }
        if (!strncmp(word[i], "DOMAIN=", 7)) {
            continue;
        }
        if (!strncmp(word[i], "FORMAT_NAME=", 12)) {
            continue;
        }
        if (!strncmp(word[i], "FORMAT=", 7)) {
            continue;
        }
        if (!strncmp(word[i], "HEADER=", 7)) {
            continue;
        }
        if (!strncmp(word[i], "FORMAT_SOURCE=", 14)) {
            continue;
        }
        if (!strncmp(word[i], "MSG_TYPE=", 9)) {
            continue;
        }
        if (!strncmp(word[i], "MSG_TYPE=", 9)) {
            continue;
        }
        if (!isdigit(word[i][0])) {
            rcs_print_warning("CMS : Word[%d] \"%s\" is not recognized on line:\n%s\n", i, word[i], BufferLine);
        }
    }

    if (cms_force_http_port != 0) {
#if defined(ENABLE_RCS_XML) && defined(ENABLE_RCS_HTTP)
        remote_port_type = CMS_HTTP_REMOTE_PORT_TYPE;
        udp_port_number = 0;
        gdrs_im_port_number = 0;
        tcp_port_number = 0;
        http_port_number = cms_force_http_port;
        neutral_encoding_method = CMS_XML_ENCODING;
#else
        cms_print_error("cms_force_http_port=%d\n", cms_force_http_port);
        cms_print_error("Either XML or HTTP support was not selected when the RCS library was compiled.\n");
        cms_force_http_port = 0;
        status = CMS_CONFIG_ERROR;
#endif
    }

    for (i = 0; i < 32; i++) {
        word[i] = (char *) NULL;
    }

    /* Get parameters from the process's line in the config file. */
    int sepwords_ret;
    if (use_autokey_for_connection_number) {
        sepwords_ret =
                separate_words_with_buffer(word, 9, procline, WordBuffer, sizeof (WordBuffer));
        if (sepwords_ret != 9) {
            cms_print_error
                    ("CMS: Error parsing process line from config file.\n");
            cms_print_error("%s\n", procline);
            status = CMS_CONFIG_ERROR;
            for (int iji = 0; iji < sepwords_ret; iji++) {
                cms_print_error("word[%d]=%s\n", iji, word[iji]);
            }
            return;
        }
    } else {
        sepwords_ret = separate_words_with_buffer(word, 10, procline, WordBuffer, sizeof (WordBuffer));
        if (sepwords_ret != 10) {
            cms_print_error
                    ("CMS: Error parsing process line from config file. (sepwords_ret=%d)\n", sepwords_ret);
            cms_print_error("%s\n", procline);
            for (int ii = 0; ii < sepwords_ret; ii++) {
                cms_print_error("word[%d]=%s\n", ii, word[ii]);
            }
            status = CMS_CONFIG_ERROR;
            return;
        }
    }
    /* Clear errno so we can determine if all of the parameters in the */
    /* buffer line were in an acceptable form. */
    if (errno == ERANGE) {
        errno = 0;
    }


    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "ProcessLine: word[0]=%s,word[1]=%s,word[2]=%s,word[3]=%s,word[4]=%s,word[5]=%s,word[6]=%s,word[7]=%s,word[8]=%s\n",
            word[0], word[1], word[2], word[3], word[4], word[5], word[6], word[7], word[8]);

    strcpy(ProcessName, word[1]);
    strcpy(ProcessHost, word[4]);

    /* Clear errno so we can determine if all of the parameters in the */
    /* buffer line were in an acceptable form. */
    if (errno == ERANGE) {
        errno = 0;
    }
    proc_type_name = word[3];
    strcpy(PermissionString, word[5]);
    spawn_server = atoi(word[6]);
    if (set_to_server > 0) {
        spawn_server = set_to_server;
    }

    /* Compute timeout. */
    if (!strcmp(word[7], "INF")) { /* Never Time Out. */
        timeout = -1;
    } else {
        timeout = strtod(word[7], (char **) NULL);
    }
    orig_timeout = read_timeout = write_timeout = connect_timeout = timeout;

    is_local_master = (bool) (atol(word[8]) != 0);

    if (!use_autokey_for_connection_number && word[9]) {
        connection_number = atol(word[9]);
        if (total_connections > 0 && total_connections <= connection_number) {
            if (!connection_over_warning_given) {
                rcs_print_warning
                        ("CMS: connection number(%ld) larger than total connections (%ld). {ProcessLine=%s}\n",
                        connection_number, total_connections, procline);
                connection_over_warning_given = true;
            }
        }
    }


    /* Check errno to see if all of the strtol's were sucessful. */
    if (ERANGE == errno) {
        cms_print_error("CMS: Error in proc line from config file.\n");
        cms_print_error("%s\n", procline);
        status = CMS_CONFIG_ERROR;
        return;
    }

    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "isserver=%d,set_to_server=%d,spawn_server=%d\n",
            isserver, set_to_server, spawn_server);

    if (set_to_server < 0) {
      isserver = false;
    } else if (set_to_server == 1) {
      isserver = true;
    } else if(set_to_server == 0 && spawn_server == 1) {
      isserver = true;
    }
    
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "isserver=%d,set_to_server=%d,spawn_server=%d\n",
            isserver, set_to_server, spawn_server);

    if (BufferType == CMS_OEMEM_TYPE) {
        neutral = true;
        remote_port_type = CMS_OE_REMOTE_PORT_TYPE;
        if (!isserver && !set_to_server) {
            ProcessType = CMS_REMOTE_TYPE;
            cms_connection_mode = CMS_FORCE_REMOTE_CONNECTION_MODE;
        }
    }

    /* Determine the ProcessType. */
    switch (cms_connection_mode) {
        case CMS_NORMAL_CONNECTION_MODE:
            if (!strcmp(proc_type_name, "REMOTE")) {
                ProcessType = CMS_REMOTE_TYPE;
                spawn_server = 0;
            } else if (!strcmp(proc_type_name, "LOCAL")) {
                ProcessType = CMS_LOCAL_TYPE;
            } else if (!strcmp(proc_type_name, "AUTO")) {
                if (hostname_matches_bufferline_with_buffer(BufferLine, WordBuffer)) {
                    ProcessType = CMS_LOCAL_TYPE;
                } else {
                    ProcessType = CMS_REMOTE_TYPE;
                    spawn_server = 0;
                }
            } else if (!strcmp(proc_type_name, "PHANTOM")) {
                ProcessType = CMS_PHANTOM_USER;
                spawn_server = 0;
                is_phantom = true;
            } else {
                cms_print_error("CMS: invalid process type (%s)/n",
                        proc_type_name);
                status = CMS_CONFIG_ERROR;
                return;
            }
            break;

        case CMS_FORCE_LOCAL_CONNECTION_MODE:
            ProcessType = CMS_LOCAL_TYPE;
            break;

        case CMS_FORCE_REMOTE_CONNECTION_MODE:
            ProcessType = CMS_REMOTE_TYPE;
            break;
    }

    /* Set flags to make sure ops section of config file is correct. */
    if (NULL != strchr(PermissionString, 'R')) {
        read_permission_flag = 1;
    } else {
        read_permission_flag = 0;
    }

    if (NULL != strchr(PermissionString, 'W')) {
        write_permission_flag = 1;
    } else {
        write_permission_flag = 0;
    }
    if (isserver && BufferType != CMS_BBDMEM_TYPE) {
        read_permission_flag = 1;
        write_permission_flag = 1;
    }

    mode = CMS_NOT_A_MODE; /* Make sure user sets the mode before using. */

    // Search the end of the bufferline for key words.

    for (i = 0; i < 32; i++) {
        word[i] = (char *) NULL;
    }

    num_words = separate_words_with_buffer(word, 32, proclineupper, WordBuffer, sizeof (WordBuffer));
    if (num_words < 8) {
        cms_print_error("CMS: Error in process line from config file.\n");
        cms_print_error("%s\n", procline);
        status = CMS_CONFIG_ERROR;
        return;
    }
    for (i = 8; i < num_words && i < 32; i++) {
        if (word[i] == NULL) {
            break;
        }
        if (!strncmp(word[i], "DO_CLONING", 10)) {
#if defined(ENABLE_RCS_SOKINTRF) && !defined(WIN32)
            do_cloning = true;
#else
            cms_print_error("Buffer cloning not supported on the current platform/configuration.\n");
            do_cloning = false;
#endif
            continue;
        }
        if (!strncmp(word[i], "NO_VERIFY_BUF", 13)) {
            no_verify_buf = true;
            continue;
        }
        if (!strncmp(word[i], "IGNORE_CONNECT_ERR", 18)) {
            ignore_connect_err = true;
            continue;
        }
        if (!strncmp(word[i], "IGNORE_CLONED", 13)) {
            ignore_cloned_buff = true;
            continue;
        }
        if (NULL != strstr(word[i], "WAITFORMASTER")) {
            wait_for_master = true;
            continue;
        }
        if (!strncmp(word[i], "READ_TIMEOUT=", 13)) {
            if (!strncmp(word[i] + 13, "INF", 3)) {
                read_timeout = -1.0;
            } else {
                read_timeout = strtod(word[i] + 13, 0);
            }
            continue;
        }
        if (!strncmp(word[i], "CONNECT_TIMEOUT=", 16)) {
            if (!strncmp(word[i] + 16, "INF", 3)) {
                connect_timeout = -1.0;
            } else {
                connect_timeout = strtod(word[i] + 16, 0);
            }
            continue;
        }

        if (!strncmp(word[i], "WRITE_TIMEOUT=", 14)) {
            if (!strncmp(word[i] + 14, "INF", 3)) {
                write_timeout = -1.0;
            } else {
                write_timeout = strtod(word[i] + 14, 0);
            }
            continue;
        }

        if (!strncmp(word[i], "MAX_TIMEOUTS=", 13)) {
            // This is parsed in TCPMEM.cc but here we just check for it to
            // skip the warning.
            continue;
        }

        if (!strncmp(word[i], "SUB=", 4)) {
            // This is parsed in TCPMEM.cc but here we just check for it to
            // skip the warning.
            continue;
        }
        if (!strcmp(word[i], "DO_NOT_TRACK_CLIENT_UDP_PORTS")) {
            // This is parsed in udp_srv.cc but here we just check for it to
            // skip the warning.
            continue;
        }

        if (!strcmp(word[i], "DO_NOT_PRINT_ERRORS")) {
            do_not_print_errors=true;
	    do_not_print_timeout_errors=true;
            continue;
        }

        if (!strcmp(word[i], "DO_NOT_PRINT_TIMEOUT_ERRORS")) {
	    do_not_print_timeout_errors=true;
            continue;
        }

        if (!strncmp(word[i], "POLL", 4)) {
            // This is parsed in TCPMEM.cc or UDPMEM.cc  but here we just check for it to
            // skip the warning.
            continue;
        }

        if (!strncmp(word[i], "NO_TASK_LOCK", 12)) {
            // This is parsed in globmem.cc but here we just check for it to
            // skip the warning.
            continue;
        }

        if (!strncmp(word[i], "RETRY=", 6)) {
            // This is parsed in tcpmem.cc but here we just check for it to
            // skip the warning.
            continue;
        }

        if (!strncmp(word[i], "BROADCAST_TO_SVR=", 17)) {
            // This is parsed in udpmem.cc but here we just check for it to
            // skip the warning.
            continue;
        }

        if (NULL != strstr(word[i], "STOPONCONNREF")) {
            stop_when_connection_refused = true;
            continue;
        }

        if (NULL != strstr(word[i], "SERIALPORTDEVNAME=")) {
            remote_port_type = CMS_TTY_REMOTE_PORT_TYPE;
            continue;
        }


        if (!strncmp(word[i], "DEFAULT_PRIORITY=", 17)) {
            default_priority = strtol(word[i] + 17, 0, 0);
            continue;
        }


        if (!strncmp(word[i], "BIND_PROC_HOST", 17)) {
            bind_proc_host = true;
            continue;
        }

        char *max_blocking_retries_string2;
        if (NULL != (max_blocking_retries_string2 = strstr(word[i], "MAX_BLOCKING_RETRIES="))) {
            max_repeat_blocking_reads = strtoul(max_blocking_retries_string2, 0, 0);
            continue;
        }

        if (!strcmp(word[i], "LOGXML")) {
#ifdef ENABLE_RCS_XML
            enable_xml_logging = 1;
            continue;
#else
            cms_print_error("XML support was not selected when the RCS library was compiled.\n");
#endif
        }

        if (!strcmp(word[i], "DIFFXML") || !strcmp(word[i], "XMLDIFF")) {
#ifdef ENABLE_RCS_XML
            if (!neutral) {
                enable_xml_differencing = true;
            }
            continue;
#else
            cms_print_error("XML support was not selected when the RCS library was compiled.\n");
#endif
        }

        if (!strcmp(word[i], "CLEANING_FLAG")) {
            cleaning_flag = true;
            continue;
        }

        if (!isdigit(word[i][0])) {
            rcs_print_warning("CMS : Word[%d] \"%s\" is not recognized on line:\n%s\n", i, word[i], ProcessLine);
        }

    }

    if (min_compatible_version < 3.44 && min_compatible_version > 0) {
        total_subdivisions = 1;
    }
    if (queuing_enabled && split_buffer) {
        cms_print_error("CMS: Can not split buffer with queuing enabled.\n");
        status = CMS_CONFIG_ERROR;
        return;
    }
    if (min_compatible_version > 3.39 || min_compatible_version <= 0.0) {
        if (neutral_encoding_method == CMS_ASCII_ENCODING) {
            neutral_encoding_method = CMS_DISPLAY_ASCII_ENCODING;
        }
    }

    if (min_compatible_version <= 3.71 && min_compatible_version >= 1e-6) {
        enable_diagnostics = 0;
    }
    if (neutral) {
        enable_xml_differencing = false;
    }

    if (!force_raw) {
        if (neutral || isserver || CMS_REMOTE_TYPE == ProcessType) {
            if (could_not_use_encoding_type) {
                cms_print_error("A neutral encoding type is required but could not be used.\n");
                status = CMS_CONFIG_ERROR;
            } else if (!encoding_explicitly_set) {
                rcs_print_warning("The encoding type for %s was not explicitly set. PACKED encoding will be used. This may be incompatible with previous versions of NML. Add XDR or DISP or PACKED or XML to the Buffer Line in the NML config file to avoid seeing this message.\n", BufferName);
            }
        }
    }

#ifdef ENABLE_RCS_LOCMEM
    if (BufferType == CMS_LOCMEM_TYPE && isserver) {
        private_server_object = true;
    }
#endif

    open(); /* Allocate memory and intialize XDR streams */
#ifdef ENABLE_RCS_DIAG
    if (enable_diagnostics) {
        setup_diag_proc_info();
    }
#endif
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "finished constructing CMS\n");
}

/* Function for allocating memory and initializing XDR streams, which */
/*    is called from both CMS constructors.                           */
void
CMS::open(void) {
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "CMS::open() called.\n");
    int encode_header_ret;
    int encode_queuing_header_ret;

    /* Clear some status checking variables. */
    status = CMS_STATUS_NOT_SET;

    /* Set all the pointers to null before requesting memory so that we only */
    /* free successfully allocated memory. */
    data = NULL;
    subdiv_data = NULL;
    encoded_data = NULL;
    encoded_header = NULL;
    encoded_queuing_header = NULL;
    encoded_header_size = 0;
    updater = (CMS_UPDATER *) NULL;
    normal_updater = (CMS_UPDATER *) NULL;
    temp_updater = (CMS_UPDATER *) NULL;
    last_im = CMS_NOT_A_MODE;
    pointer_check_disabled = 0;

    dummy_handle = (PHYSMEM_HANDLE *) NULL;

    /* Initialize some debug variables. */
    first_read_done = 0;
    first_write_done = 0;
    total_messages_missed = 0;
    messages_missed_on_last_read = 0;
    format_low_ptr = (char *) NULL;
    format_high_ptr = (char *) NULL;
    header.was_read = 0;
    header.write_id = 0;
    header.in_buffer_size = 0;
    sizeof_message_header = 0;

    if (force_raw) {
        if (!isserver && !neutral && ProcessType == CMS_LOCAL_TYPE) {
            force_raw = false;
        }
    }

    number_of_cms_objects++; /* Increment the static variable.  */
    /* Save some memory and time if this is a PHANTOMMEM object. */
    if (!is_phantom) {
        /* Allocate memory for the local copy of global buffer. */
        if (size < 12) {
            cms_print_error("Bad size for buffer %ld\n", size);
            status = CMS_CONFIG_ERROR;
            return;
        }
	if(max_message_size_set_on_buffer_line &&
	   max_message_size > 0 && max_message_size < size && total_subdivisions < 2)
	  {
	    data = DEBUG_MALLOC(max_message_size);
	    if(data)
	      {
		memset(data, 0, max_message_size);
	      }
	  }
	else
	  {
	    data = DEBUG_MALLOC(size);
	    if(data)
	      {
		memset(data, 0, size);
	      }
	  }
        if (0 == data) {
            cms_print_error("malloc failed\n");
            status = CMS_CREATE_ERROR;
            return;
        }
        rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "%p = data = malloc(%ld);\n",
                data, size);
        subdiv_data = data;
        if (force_raw) {
            encoded_data = data;
        }
    }
    if ((isserver || neutral || ProcessType == CMS_REMOTE_TYPE) && !force_raw) {
        switch (neutral_encoding_method) {
            case CMS_XDR_ENCODING:
#ifdef ENABLE_RCS_XDR
                updater = new CMS_XDR_UPDATER(this);
#else
                cms_print_error("XDR support was not selected when the RCS library was compiled.\n");
                status = CMS_NO_IMPLEMENTATION_ERROR;
                return;
#endif
                break;

            case CMS_PACKED_ENCODING:
#ifdef ENABLE_PACKED_UP
                updater = new CMS_PACKED_UPDATER(this, false);
#else
                cms_print_error("packed updater support was not selected when the RCS library was compiled.\n");
                status = CMS_NO_IMPLEMENTATION_ERROR;
                return;
#endif
                break;

            case CMS_PACKEDL64_ENCODING:
#if defined(ENABLE_PACKED_UP) && defined(ENABLE_RCS_PACKEDL64)
                updater = new CMS_PACKED_UPDATER(this, true);
#else
                cms_print_error("packed or packedl64 updater support was not selected when the RCS library was compiled.\n");
                status = CMS_NO_IMPLEMENTATION_ERROR;
                return;
#endif
                break;

            case CMS_XML_ENCODING:
#ifdef ENABLE_RCS_XML
                update_cmd_msg_base = 1;
                update_cmd_msg_base_in_format = 0;
                update_stat_msg_base = 1;
                update_stat_msg_base_in_format = 0;
                updater = new CMS_XML_UPDATER(this);
#else
                cms_print_error("XML support was not selected when the RCS library was compiled.\n");
                status = CMS_NO_IMPLEMENTATION_ERROR;
                return;
#endif
                break;

#if 0
            case CMS_ASCII_ENCODING:
                updater = new CMS_ASCII_UPDATER(this);
                break;

#endif
            case CMS_ASCII_ENCODING:
            case CMS_DISPLAY_ASCII_ENCODING:
#ifdef ENABLE_RCS_DISP
                updater = new CMS_DISPLAY_ASCII_UPDATER(this);
#else
                cms_print_error("DISP support was not selected when the RCS library was compiled.\n");
                status = CMS_NO_IMPLEMENTATION_ERROR;
                return;
#endif
                break;

            default:
                updater = (CMS_UPDATER *) NULL;
                status = CMS_UPDATE_ERROR;
                cms_print_error("CMS: Invalid encoding method(%d)\n",
                        neutral_encoding_method);
                break;
        }
        normal_updater = updater;
        if (((int) status) < 0) {
            return;
        }
        header.was_read = 999999;
        header.write_id = 999999;
        header.in_buffer_size = 999999;

        /* Find out what size the header is after it has been encoded. */
        if ((encode_header_ret = encode_header()) == -1) {
            cms_print_error("CMS:Error encoding CMS header.\n");
            status = CMS_MISC_ERROR;
            return;
        }
        header.was_read = 0;
        header.write_id = 0;
        header.in_buffer_size = 0;

        encoded_header_size = (long) encode_header_ret;
        if (min_compatible_version <= 0.0 || min_compatible_version > 3.29) {
            if (neutral_encoding_method == CMS_DISPLAY_ASCII_ENCODING) {
                encoded_header_size = 16;
            }
        }

        if (queuing_enabled) {
            /* Initialize queuing header to avoid test center error message. */
            memset(&queuing_header, 0, sizeof (queuing_header));

            /* Find out what size the queuing_header is after being encoded. */
            if ((encode_queuing_header_ret = encode_queuing_header()) == -1) {
                cms_print_error("CMS:Error encoding CMS queuing_header.\n");
                status = CMS_MISC_ERROR;
                return;
            }
            encoded_queuing_header_size = (long) encode_queuing_header_ret;
        }
    }

    if (split_buffer && total_subdivisions > 1) {
        cms_print_error
                ("Can't split buffer and use subdivisions. (total_subsivisions=%d)",
                total_subdivisions);
        status = CMS_MISC_ERROR;
        return;
    }
    fast_mode = false;
    recompute_sizes();

    if ((neutral || ProcessType == CMS_REMOTE_TYPE) && !isserver && !force_raw) {
        /* Local processes that are use a neutral buffer and */
        /*  All remote processes. */
        read_mode = CMS_DECODE;
        read_updater_mode = CMS_DECODE_DATA;
        write_mode = CMS_ENCODE;
        write_updater_mode = CMS_ENCODE_DATA;
    } else if (!neutral && isserver && !force_raw) {
        /* Servers. */
        read_mode = CMS_ENCODE;
        read_updater_mode = CMS_ENCODE_DATA;
        write_mode = CMS_DECODE;
        write_updater_mode = CMS_DECODE_DATA;
    } else {
        /* Everybody else. */
        read_mode = CMS_RAW_OUT;
        read_updater_mode = CMS_NO_UPDATE;
        write_mode = CMS_RAW_IN;
        write_updater_mode = CMS_NO_UPDATE;
    }
}

void
CMS::recompute_sizes(void) {
    int nfactor = 4;
    if (NULL != updater) {
        nfactor = updater->neutral_size_factor;
    }

    /* Set some varaibles to let the user know how much space is left. */
    size_without_diagnostics = size;
    diag_offset = 0;
#ifdef ENABLE_RCS_DIAG
    if (total_connections <= 0) {
        if (enable_diagnostics) {
            rcs_print_warning("Can not enable diagnostics when total_connections or max_processes set in config file=%ld\n", total_connections);
        }
        enable_diagnostics = false;
    }
    if (enable_diagnostics) {
        diag_offset = (sizeof (CMS_DIAG_HEADER) +
                (total_connections > 0 ? (total_connections * sizeof (CMS_DIAG_PROC_INFO)) : 0));
        size_without_diagnostics -= diag_offset;
    }
#endif
    half_offset = (size_without_diagnostics / 2);
    half_size = (size_without_diagnostics / 2);
    if (total_connections < 0) {
        total_connections = 0;
    }
    if (split_buffer) {
        if (neutral) {
            subdiv_size = (size_without_diagnostics / 2) - total_connections;
            subdiv_size -= (subdiv_size % 4);
	    if(!max_message_size_set_on_buffer_line)
	      {
		max_message_size =
		  (size_without_diagnostics / 2) - total_connections -
		  encoded_header_size - 2;
	      }
            max_encoded_message_size =
                    size_without_diagnostics - total_connections -
                    encoded_header_size;
            guaranteed_message_space =
                    max_message_size / cms_encoded_data_explosion_factor;
        } else {
            if (ProcessType == CMS_REMOTE_TYPE) {
                subdiv_size =
                        (size_without_diagnostics / 2) - total_connections;
                subdiv_size -= (subdiv_size % 4);
		if(!max_message_size_set_on_buffer_line)
		  {
		    max_message_size =
		      (size_without_diagnostics / 2) - total_connections -
		      sizeof (CMS_HEADER) - 2;
		  }
                max_encoded_message_size = nfactor * max_message_size;
                guaranteed_message_space = max_message_size / nfactor;
            } else {
                subdiv_size =
                        (size_without_diagnostics / 2) - total_connections;
                subdiv_size -= (subdiv_size % 4);
		if(!max_message_size_set_on_buffer_line)
		  {
		    max_message_size =
		      (size_without_diagnostics / 2) - total_connections -
		      sizeof (CMS_HEADER) - 2;
		  }
                max_encoded_message_size = nfactor * max_message_size;
                guaranteed_message_space = max_message_size;
            }
        }
    } else {
        if (neutral) {
            subdiv_size =
                    (size_without_diagnostics -
                    total_connections) / total_subdivisions;
            subdiv_size -= (subdiv_size % 4);
	    if(!max_message_size_set_on_buffer_line)
	      {
		max_message_size = subdiv_size - encoded_header_size;
	      }
            max_encoded_message_size = subdiv_size - encoded_header_size;
            guaranteed_message_space = max_message_size / nfactor;
        } else {
            if (ProcessType == CMS_REMOTE_TYPE) {
                subdiv_size =
                        (size_without_diagnostics -
                        total_connections) / total_subdivisions;
                subdiv_size -= (subdiv_size % 4);
                if(!max_message_size_set_on_buffer_line)
		  {
		    max_message_size = subdiv_size - sizeof (CMS_HEADER);
		  }
                max_encoded_message_size = nfactor * max_message_size;
                guaranteed_message_space = max_message_size / nfactor;
            } else {
                subdiv_size =
                        (size_without_diagnostics -
                        total_connections) / total_subdivisions;
                subdiv_size -= (subdiv_size % 4);
                if(!max_message_size_set_on_buffer_line)
		  {
		    max_message_size = subdiv_size - sizeof (CMS_HEADER);
		  }
                max_encoded_message_size = nfactor * max_message_size;
                guaranteed_message_space = max_message_size;
            }
        }
    }
    if (enc_max_size > 0 && enc_max_size < max_encoded_message_size) {
        max_encoded_message_size = enc_max_size;
    }
    if (max_encoded_message_size > encoded_data_size &&
            encoded_data_size > 0 && encoded_data) {
        max_encoded_message_size = encoded_data_size;
    }
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "CMS::recompute_sizes() size=%ld,subdiv_size=%ld,encoded_data_size=%ld,max_message_size=%ld\n",
            size, subdiv_size, encoded_data_size, max_message_size);
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS,
            "CMS::recompute_sizes() enc_max_size=%ld,max_encoded_message_size=%ld\n",
            enc_max_size, max_encoded_message_size);

}

/* Set the area used for the encoded data buffer, and initialize the */
/* XDR streams to use this area. */
/* This function is called from open, which is called by the constructor */
/* and by one of the CMS_SERVER functions. */

/* _encoded_data should point to an area of memory at least cms_encoded_data_explosion_factor*size .*/
void
CMS::set_encoded_data(void *_encoded_data, long _encoded_data_size) {
    if (force_raw) {
        if (NULL != data && data != _encoded_data) {
            DEBUG_FREE(data);
        }
        data = encoded_data = _encoded_data;
        encoded_data_size = size;
        subdiv_data = data;
        using_external_encoded_data = true;
        recompute_sizes();
    } else {
        if (max_encoded_message_size > _encoded_data_size) {
            max_encoded_message_size = _encoded_data_size;
        }
        if (NULL != updater) {
            updater->set_encoded_data(_encoded_data, _encoded_data_size);
        }
        if (NULL != _encoded_data && zero_encoded_data_when_set) {
            if (_encoded_data_size < 0x40000) {
                memset(_encoded_data, 0, _encoded_data_size);
            } else {
                memset(_encoded_data, 0, 0x40000);
            }
        }
        using_external_encoded_data = true;
    }
}

/* Destructor */
CMS::~CMS() {
    rcs_print_debug(PRINT_CMS_DESTRUCTORS, "deleting CMS (%s)\n", BufferName);

    interrupt_operation();
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        if (!preserve_mrpq_reader_id) {
            remove_current_mrpq_reader_id();
        }
        delete mrpq;
        mrpq = 0;
    }
#else
    mrpq = 0;
#endif

    nmltypename = 0;
    if (NULL != updater) {
        if (updater == temp_updater) {
            temp_updater = NULL;
        }
        delete updater;
        updater = (CMS_UPDATER *) NULL;
    }

    if (encoded_data == temp_encoded_data) {
        temp_encoded_data = 0;
    }

    if (NULL != temp_updater) {
        delete temp_updater;
        temp_updater = (CMS_UPDATER *) NULL;
    }

    if (temp_encoded_data) {
      DEBUG_FREE(temp_encoded_data);
      temp_encoded_data = 0;
    }

    if (encoded_data == data) {
        encoded_data = 0;
    }
    /* Free the memory used for the local copy of the global buffer. */
    if (NULL != data && (!force_raw || !using_external_encoded_data)) {
        rcs_print_debug(PRINT_CMS_DESTRUCTORS, "free( data = %p);\n", data);
        DEBUG_FREE(data);
        data = NULL;
        if (!using_external_encoded_data && NULL != encoded_data) {
            DEBUG_FREE(encoded_data);
            encoded_data = NULL;
        }
    }
    number_of_cms_objects--;

    if (NULL != dummy_handle) {
        delete dummy_handle;
        dummy_handle = (PHYSMEM_HANDLE *) NULL;
    }
    if (NULL != transfer_alias_list) {
        delete transfer_alias_list;
        transfer_alias_list = 0;
    }
    copybuff = 0;
    copybuff_size = 0;
    xbase = 0;
    if (copymode_unbounded_struct_array_xbases != 0) {
        DEBUG_FREE(copymode_unbounded_struct_array_xbases);
        copymode_unbounded_struct_array_xbases = 0;
    }
    if (copymode_unbounded_struct_array_copybuffs != 0) {
        DEBUG_FREE(copymode_unbounded_struct_array_copybuffs);
        copymode_unbounded_struct_array_copybuffs = 0;
    }
    if (copymode_unbounded_struct_array_copybuff_sizes != 0) {
        DEBUG_FREE(copymode_unbounded_struct_array_copybuff_sizes);
        copymode_unbounded_struct_array_copybuff_sizes = 0;
    }
    if (nmlcfgsvr != 0) {
        free(nmlcfgsvr);
        nmlcfgsvr = 0;
    }
    if (message_memory_map_cP) {
        void *vp = (void *) message_memory_map_cP;
        message_memory_map_cP = 0;
        free(vp);
    }
    if(memory_map_pos_offset_from)
      {
	free(memory_map_pos_offset_from);
	memory_map_pos_offset_from=0;
      }

    max_copymode_unbounded_struct_array_count = 0;
    rcs_print_debug(PRINT_CMS_DESTRUCTORS, "Leaving ~CMS()\n");
}

static const char internal_access_type_lookup_array[22][36] ={
    "CMS_ZERO_ACCESS",
    "CMS_READ_ACCESS",
    "CMS_CHECK_IF_READ_ACCESS",
    "CMS_PEEK_ACCESS",
    "CMS_WRITE_ACCESS",
    "CMS_WRITE_IF_READ_ACCESS",
    "CMS_CLEAR_ACCESS",
    "CMS_GET_MSG_COUNT_ACCESS",
    "CMS_GET_DIAG_INFO_ACCESS",
    "CMS_GET_QUEUE_LENGTH_ACCESS",
    "CMS_GET_SPACE_AVAILABLE_ACCESS",
    "CMS_GET_NEW_READER_ID_ACCESS",
    "CMS_SET_READER_ID_ACCESS",
    "CMS_REMOVE_READER_ID_ACCESS",
    "CMS_WAIT_FOR_WRITE_ACCESS",
    "CMS_WAIT_FOR_READ_ACCESS",
    "CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS",
    "CMS_WAIT_FOR_CLEAR_ACCESS",
    "CMS_WAIT_FOR_ANYTHING_ACCESS",
    "CMS_GET_READ_COUNT_ACCESS",
    "CMS_GET_IS_CLEAR_ACCESS",
    ""};


/* This function should never be called. It exists so that classes  which */

/* overload read, write etc don't have to bother creating it. */
CMS_STATUS
CMS::main_access(void *_local) {
    cms_print_error("CMS::main_access called by %s for %s.\n",
            ProcessName, BufferName);
    cms_print_error("This should never happen.\n");
    cms_print_error
            ("Derived classes should either override main_access() or\n");
    cms_print_error("the functions that call it.(read(), write(), etc.)\n");
    cms_print_error("_local = %p\n", _local);
    cms_print_error("internal_access_type=%d:%s\n", internal_access_type,
            (internal_access_type < 20 && internal_access_type >= 0 ?
            internal_access_type_lookup_array[internal_access_type] :
            "bad type"));
    return (CMS_MISC_ERROR);
}

/* General Utility Functions. */

/* Check the buffer id against in_buffer_id to see if it is new. */
CMS_STATUS
CMS::check_id(CMSID id) {
    if (status < 0) {
        return (status);
    }


    if (0 == id) {
        messages_missed_on_last_read = 0;
        in_buffer_id = 0;
        return (status = CMS_READ_OLD);
    }

    if (get_msg_start_only) {
        return (status = CMS_READ_OK);
    }

    if (id == in_buffer_id) {
        status = CMS_READ_OLD;
        messages_missed_on_last_read = 0;
    } else {
        if (split_buffer) {
            if (id == last_id_side0 || id == last_id_side1) {
                status = CMS_READ_OLD;
                messages_missed_on_last_read = 0;
                return (status);
            }
            if (toggle_bit) {
                last_id_side0 = id;
            } else {
                last_id_side1 = id;
            }
        }
        status = CMS_READ_OK;
        messages_missed_on_last_read = id - in_buffer_id - 1;
        if (messages_missed_on_last_read < 0) {
            messages_missed_on_last_read = 0;
        }
        if (messages_missed_on_last_read > 0) {
            rcs_print_debug(PRINT_MISC, "CMS::check_id() BufferName=%s,current_subdivision=%d,id=%d,messages_missed_on_last_read=%ld, total_messages_missed=%ld,in_buffer_id=%d\n",
                    BufferName, current_subdivision, (int) id,
                    messages_missed_on_last_read, total_messages_missed,
                    (int) in_buffer_id);
        }
        total_messages_missed += messages_missed_on_last_read;
        in_buffer_id = id;
    }
    return (status);
}

void
CMS::clean_buffers() {
    in_buffer_id = 0;
    last_id_side0 = 0;
    last_id_side1 = 0;
    if (NULL != data) {
        memset(data, 0, size);
    }
    if (NULL != encoded_data) {
        memset(encoded_data, 0, max_encoded_message_size);
    }
}

/* Read and Write interface functions call appropriate virtual function. */
CMS_STATUS
CMS::clear() {
    in_buffer_id = 0;
    last_id_side0 = 0;
    last_id_side1 = 0;
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_CLEAR_ACCESS;
    main_access(data);
    return (status);
}

int
CMS::check_if_read() {
    internal_access_type = CMS_CHECK_IF_READ_ACCESS;
    status = CMS_STATUS_NOT_SET;
    main_access(data);
    return ((int) header.was_read);
}

int
CMS::get_queue_length() {
    internal_access_type = CMS_GET_QUEUE_LENGTH_ACCESS;
    status = CMS_STATUS_NOT_SET;
    if (!queuing_enabled) {
        return 0;
    }
    main_access(data);
    return ((int) queuing_header.queue_length);
}

int
CMS::get_space_available() {
    internal_access_type = CMS_GET_SPACE_AVAILABLE_ACCESS;
    status = CMS_STATUS_NOT_SET;
    if (!queuing_enabled) {
        return size;
    }
    main_access(data);
    return ((int) free_space);
}

int
CMS::check_if_transfers_complete() {
    return 1;
}

CMS_STATUS
CMS::read() {
    timeout = read_timeout;
    internal_access_type = CMS_READ_ACCESS;
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    main_access(data);
    timeout = orig_timeout;
    return (status);
}

CMS_STATUS
CMS::blocking_read(double _blocking_timeout) {
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_READ_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    return (status);
}

int
CMS::wait_for_anything(double _blocking_timeout) {
    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_WAIT_FOR_ANYTHING_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_READ_OK) ? 0 : -1);
    return retval;
}

int
CMS::wait_for_read(double _blocking_timeout) {
    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_WAIT_FOR_READ_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_READ_OK) ? 0 : -1);
    return retval;
}

int
CMS::wait_for_clear(double _blocking_timeout) {
    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_WAIT_FOR_CLEAR_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_CLEAR_OK) ? 0 : -1);
    return retval;
}

int
CMS::wait_for_write(double _blocking_timeout) {
    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    internal_access_type = CMS_WAIT_FOR_WRITE_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_WRITE_OK) ? 0 : -1);
    return retval;
}

int
CMS::wait_for_queue_length_over(int _ql, double _blocking_timeout) {
    if (!queuing_enabled) {
        status = CMS_MISC_ERROR;
        return (-1);
    }
    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    queue_length_to_wait_for = _ql;
    waiting_for_queue_length_over = true;
    internal_access_type = CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_QUEUE_LENGTH_OK) ? 0 : -1);
    return retval;
}

int
CMS::wait_for_queue_length_under(int _ql, double _blocking_timeout) {
    if (!queuing_enabled) {
        status = CMS_MISC_ERROR;
        return (-1);
    }

    wait_for_initialized = false;
    status = CMS_STATUS_NOT_SET;
    queue_length_to_wait_for = _ql;
    waiting_for_queue_length_over = false;
    internal_access_type = CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS;
    blocking_timeout = _blocking_timeout;
    main_access(data);
    int retval = ((status == CMS_WAIT_FOR_QUEUE_LENGTH_OK) ? 0 : -1);
    return retval;
}

void
CMS::disconnect() {
}

void
CMS::reconnect() {
}

CMS_STATUS
CMS::peek() {
    timeout = read_timeout;
    internal_access_type = CMS_PEEK_ACCESS;
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    main_access(data);
    timeout = orig_timeout;
    return (status);
}

CMS_STATUS
CMS::get_msg_start(void *tmp_data, size_t s) {
    void *orig_data = data;
    void *orig_subdiv_data = subdiv_data;
    data = tmp_data;
    subdiv_data = tmp_data;
    timeout = read_timeout;
    internal_access_type = CMS_PEEK_ACCESS;
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    get_msg_start_only = true;
    temp_data_size = s;
    long orig_max_message_size = max_message_size;
    if (max_message_size > ((long) s)) {
        max_message_size = (long) s;
    }
    main_access(data);
    temp_data_size = 0;
    get_msg_start_only = false;
    timeout = orig_timeout;
    data = orig_data;
    subdiv_data = orig_subdiv_data;
    max_message_size = orig_max_message_size;
    return (status);
}

long
CMS::get_msg_type() {
    status = CMS_NO_IMPLEMENTATION_ERROR;
    return -1;
}

CMS_STATUS
CMS::write(void *user_data) {
    timeout = write_timeout;
    internal_access_type = CMS_WRITE_ACCESS;
    status = CMS_STATUS_NOT_SET;
    main_access(user_data);
    timeout = orig_timeout;
    return (status);
}

CMS_STATUS
CMS::write_if_read(void *user_data) {
    timeout = write_timeout;
    internal_access_type = CMS_WRITE_IF_READ_ACCESS;
    status = CMS_STATUS_NOT_SET;
    main_access(user_data);
    timeout = orig_timeout;
    return (status);
}

CMS_STATUS
CMS::setup_subscription(double /* _subscription_period */ ) {
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}

CMS_STATUS
CMS::cancel_subscription () {
  return(status=CMS_NO_IMPLEMENTATION_ERROR);
}

// For protocols that provide No security, tell the
// application the login was successful.
// This method needs to be overloaded to have any security.

/*ARGSUSED*/
int
CMS::login(
    const char *,
    const char *) {
    return 1;
}

/* Function to set the mode to appropriate read or write mode. */
void
CMS::set_mode(CMSMODE im) {
    status = CMS_STATUS_NOT_SET;
    if (last_im == im && last_im != CMS_NOT_A_MODE) {
        return;
    }
    if (!force_raw) {
        if (CMS_WRITE == im) {
            mode = write_mode;
            if (NULL != updater) {
                updater->set_mode((CMS_UPDATER_MODE) write_updater_mode);
            }
            last_im = im;
            return;
        }
        if (CMS_READ == im) {
            mode = read_mode;
            if (NULL != updater) {
                updater->set_mode((CMS_UPDATER_MODE) read_updater_mode);
            }
            last_im = im;
            return;
        }
        if (CMS_DECODE == im) {
            mode = CMS_DECODE;
            if (NULL != updater) {
                updater->set_mode(CMS_DECODE_DATA);
            }
        }
        if (CMS_ENCODE == im) {
            mode = CMS_ENCODE;
            if (NULL != updater) {
                updater->set_mode(CMS_ENCODE_DATA);
            }
        }
    }
    last_im = im;
    mode = im;
}

/* Functions for changing/restoring the updator type. */
void
CMS::set_temp_updater(CMS_NEUTRAL_ENCODING_METHOD temp_encoding_method) {

    last_im = CMS_NOT_A_MODE;
    if (force_raw) {
        return;
    }
    if (temp_updater_encoding_method != temp_encoding_method &&
            NULL != temp_updater) {
        delete temp_updater;
        temp_updater = (CMS_UPDATER *) NULL;
        if (temp_encoded_data == encoded_data) {
            temp_encoded_data = 0;
        }
    }
    if (temp_encoding_method == CMS_XML_ENCODING) {
        update_cmd_msg_base = 1;
        update_cmd_msg_base_in_format = 0;
        update_stat_msg_base = 1;
        update_stat_msg_base_in_format = 0;
    } else {
        update_cmd_msg_base = 0;
        update_cmd_msg_base_in_format = 1;
        update_stat_msg_base = 0;
        update_stat_msg_base_in_format = 1;
    }
    if (0 == temp_encoded_data && encoded_data_size > 0) {
        temp_encoded_data = DEBUG_MALLOC(encoded_data_size);
        temp_encoded_data_size = encoded_data_size;
        temp_max_encoded_message_size = max_encoded_message_size;
    }
    if (encoded_data != temp_encoded_data) {
        restore_using_external_encoded_data = using_external_encoded_data;
        using_external_encoded_data = false;
        restore_encoded_data_size = encoded_data_size;
        restore_encoded_data = encoded_data;
        restore_max_encoded_message_size = max_encoded_message_size;
        encoded_data = temp_encoded_data;
        encoded_data_size = temp_encoded_data_size;
        max_encoded_message_size = temp_max_encoded_message_size;
    }
    if (NULL == temp_updater) {
        rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "Creating new temp_updater\n");
        switch (temp_encoding_method) {
            case CMS_XDR_ENCODING:
#ifdef ENABLE_RCS_XDR
                temp_updater = new CMS_XDR_UPDATER(this);
#else
                cms_print_error("XDR support was not selected when the RCS library was compiled.\n");
#endif
                break;

            case CMS_PACKED_ENCODING:
#ifdef ENABLE_PACKED_UP
                temp_updater = new CMS_PACKED_UPDATER(this, false);
#else
                cms_print_error("PACKED support was not selected when the RCS library was compiled.\n");
#endif
                break;

            case CMS_PACKEDL64_ENCODING:
#if defined(ENABLE_PACKED_UP) && defined(ENABLE_RCS_PACKEDL64)
                updater = new CMS_PACKED_UPDATER(this, true);
#else
                cms_print_error("packed or packedl64 updater support was not selected when the RCS library was compiled.\n");
#endif
                break;


#if 0
            case CMS_ASCII_ENCODING:
                temp_updater = new CMS_ASCII_UPDATER(this);
                break;

#endif

            case CMS_ASCII_ENCODING:
            case CMS_DISPLAY_ASCII_ENCODING:
#ifdef ENABLE_RCS_DISP
                temp_updater = new CMS_DISPLAY_ASCII_UPDATER(this);
#else
                cms_print_error("DISP support was not selected when the RCS library was compiled.\n");
#endif
                break;

            case CMS_XML_ENCODING:
#ifdef ENABLE_RCS_XML
                update_cmd_msg_base = 1;
                update_cmd_msg_base_in_format = 0;
                update_stat_msg_base = 1;
                update_stat_msg_base_in_format = 0;
                temp_updater = new CMS_XML_UPDATER(this);
#else
                cms_print_error("XML support was not selected when the RCS library was compiled.\n");
#endif
                break;


            default:
                temp_updater = (CMS_UPDATER *) NULL;
                status = CMS_UPDATE_ERROR;
                cms_print_error("CMS: Invalid encoding method(%d)\n",
                        neutral_encoding_method);
                break;
        }
    }
    if (NULL != temp_updater) {
        updater = temp_updater;
        temp_updater_encoding_method = temp_encoding_method;
    }
}

void
CMS::restore_normal_updater() {
    last_im = CMS_NOT_A_MODE;
    if (neutral_encoding_method == CMS_XML_ENCODING) {
        update_cmd_msg_base = 1;
        update_cmd_msg_base_in_format = 0;
        update_stat_msg_base = 1;
        update_stat_msg_base_in_format = 0;
    } else {
        update_cmd_msg_base = 0;
        update_cmd_msg_base_in_format = 1;
        update_stat_msg_base = 0;
        update_stat_msg_base_in_format = 1;
    }

    if (encoded_data != restore_encoded_data) {
        temp_encoded_data = encoded_data;
        temp_encoded_data_size = encoded_data_size;
        temp_max_encoded_message_size = max_encoded_message_size;
        encoded_data = restore_encoded_data;
        encoded_data_size = restore_encoded_data_size;
        max_encoded_message_size = restore_max_encoded_message_size;
    }
    using_external_encoded_data = restore_using_external_encoded_data;
    updater = normal_updater;

}

/* Updater Positioning Functions. */
void
CMS::rewind() {
    nmltypename = 0;
    if(memory_map_pos_offset_from)
      {
	free(memory_map_pos_offset_from);
	memory_map_pos_offset_from=0;
      }

    if(this->enable_message_memory_map)
    {
      this->memory_map_offset=0;
      this->end_message_memory_map_cP = this->message_memory_map_cP;
        if (this->message_memory_map_cP) {
            *this->message_memory_map_cP = 0;
        }
	if(!this->memory_map_use_raw) {
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(end_message_memory_map_cP, 
					this->message_memory_map_max_size),
			  "type,name,size,offset,elsize,arraylen,offset_from,dla_maxlen,comment\n");
	}
	else {
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(end_message_memory_map_cP, 
					this->message_memory_map_max_size),
			  "type,name,size,offset,elsize,arraylen,comment\n");
	}   
        end_message_memory_map_cP += strlen(end_message_memory_map_cP);
    }
    this->var_type = cms_update_var_type_none;
    this->var_arraylen = 0;
    rcs_print_debug(PRINT_MISC, "CMS::rewind()\n");
    if (force_raw) {
        return;
    }
    if (NULL != updater) {
        updater->rewind();
    }
}


/* XDR routines for accessing an encoded header. */

/*ARGSUSED*/
static const char *
header_symbol_lookup(long) {
    return "CMS_HEADER";
}

static const char header_string_list[2][11] = {
    "CMS_HEADER",
    ""
};

static const long header_id_list[2] = {
    1, -1
};
static const size_t header_size_list[2] = {
    sizeof (CMS_HEADER),
    0
};

/*ARGSUSED*/
static const char *
queuing_header_symbol_lookup(long) {
    return "CMS_QUEUING_HEADER";
}

static const char queuing_header_string_list[2][19] = {
    "CMS_QUEUING_HEADER",
    ""
};

static const long queuing_header_id_list[2] = {
    1, -1
};
static const size_t queuing_header_size_list[2] = {
    sizeof (CMS_QUEUING_HEADER),
    0
};

int
CMS::encode_header() {
    if (force_raw) {
        return 0;
    }
    if (NULL == updater) {
        return -1;
    }
    CMS_UPDATER_MODE original_mode;
    if (status == CMS_UPDATE_ERROR) {
        status = CMS_STATUS_NOT_SET;
    }
    original_mode = updater->get_mode();
    format_low_ptr = (char *) & header;
    format_high_ptr = ((char *) & header) + sizeof (CMS_HEADER);
    updater->set_mode(CMS_ENCODE_HEADER);
    updater->rewind();
    long last_type = updater->last_check_type_info_type;
    updater->check_type_info(1, &header, "header",
            (cms_symbol_lookup_function_t)
            header_symbol_lookup,
            (const char **) header_string_list,
            header_id_list, header_size_list, 2, 19);
    updater->beginClass("CMS_HEADER", 0);
    updater->update_with_name("was_read", header.was_read);
    updater->update_with_name("write_id", header.write_id);
    updater->update_with_name("in_buffer_size", header.in_buffer_size);
    updater->endClass("CMS_HEADER", 0);
    if (status == CMS_UPDATE_ERROR || status == CMS_MISC_ERROR) {
        return (-1);
    }
    int current_encoded_header_size = updater->get_encoded_msg_size();
    if (min_compatible_version <= 0.0 || min_compatible_version > 3.29) {
        if (neutral_encoding_method == CMS_DISPLAY_ASCII_ENCODING) {
            encoded_header_size = 16;
        }
    }
    updater->set_mode(original_mode);
    updater->last_check_type_info_type = last_type;
    return (current_encoded_header_size);
}

int
CMS::decode_header() {
    if (force_raw) {
        return 0;
    }
    if (NULL == updater) {
        return -1;
    }
    if (status == CMS_UPDATE_ERROR) {
        status = CMS_STATUS_NOT_SET;
    }
    CMS_UPDATER_MODE original_mode = updater->get_mode();
    format_low_ptr = (char *) & header;
    format_high_ptr = ((char *) & header) + sizeof (CMS_HEADER);
    updater->set_mode(CMS_DECODE_HEADER);
    updater->rewind();
    long last_type = updater->last_check_type_info_type;
    updater->check_type_info(1, &header, "header",
            (cms_symbol_lookup_function_t)
            header_symbol_lookup,
            (const char **) header_string_list,
            header_id_list, header_size_list, 2, 19);
    updater->beginClass("CMS_HEADER", 0);
    updater->update_with_name("was_read", header.was_read);
    updater->update_with_name("write_id", header.write_id);
    updater->update_with_name("in_buffer_size", header.in_buffer_size);
    updater->endClass("CMS_HEADER", 0);
    updater->set_mode(original_mode);
    updater->last_check_type_info_type = last_type;

    return ((int) (status != CMS_UPDATE_ERROR && status != CMS_MISC_ERROR) ? 0 :
            -1);
}

int
CMS::encode_queuing_header() {
    if (force_raw) {
        return 0;
    }
    if (NULL == updater) {
        return -1;
    }
    if (status == CMS_UPDATE_ERROR) {
        status = CMS_STATUS_NOT_SET;
    }
    CMS_UPDATER_MODE original_mode = updater->get_mode();
    format_low_ptr = (char *) & queuing_header;
    format_high_ptr =
            ((char *) & queuing_header) + sizeof (CMS_QUEUING_HEADER);
    updater->set_mode(CMS_ENCODE_QUEUING_HEADER);
    updater->rewind();
    updater->check_type_info(1, &queuing_header, "queuing_header",
            (cms_symbol_lookup_function_t)
            queuing_header_symbol_lookup,
            (const char **) queuing_header_string_list,
            queuing_header_id_list, queuing_header_size_list,
            2, 18);
    updater->beginClass("CMS_QUEUING_HEADER", 0);
    updater->update_with_name("head", queuing_header.head);
    updater->update_with_name("tail", queuing_header.tail);
    updater->update_with_name("queue_length", queuing_header.queue_length);
    updater->update_with_name("end_queue_space",
            queuing_header.end_queue_space);
    updater->update_with_name("write_id", queuing_header.write_id);
    updater->endClass("CMS_QUEUING_HEADER", 0);
    if (status == CMS_UPDATE_ERROR || status == CMS_MISC_ERROR) {
        return (-1);
    }
    encoded_queuing_header_size = updater->get_encoded_msg_size();
    if (neutral_encoding_method == CMS_XML_ENCODING) {
        encoded_queuing_header_size *= 2;
    }
    if (min_compatible_version <= 0.0 || min_compatible_version > 3.29) {
        if (neutral_encoding_method == CMS_DISPLAY_ASCII_ENCODING) {
            encoded_queuing_header_size = 24;
        }
    }
    updater->set_mode(original_mode);
    return (encoded_queuing_header_size);
}

int
CMS::decode_queuing_header() {
    if (force_raw) {
        return 0;
    }
    if (NULL == updater) {
        return -1;
    }
    if (status == CMS_UPDATE_ERROR) {
        status = CMS_STATUS_NOT_SET;
    }
    CMS_UPDATER_MODE original_mode = updater->get_mode();
    format_low_ptr = (char *) & queuing_header;
    format_high_ptr =
            ((char *) & queuing_header) + sizeof (CMS_QUEUING_HEADER);
    updater->set_mode(CMS_DECODE_QUEUING_HEADER);
    updater->rewind();
    updater->check_type_info(1, &queuing_header, "queuing_header",
            (cms_symbol_lookup_function_t)
            queuing_header_symbol_lookup,
            (const char **) queuing_header_string_list,
            queuing_header_id_list, queuing_header_size_list,
            2, 18);
    updater->beginClass("CMS_QUEUING_HEADER", 0);
    updater->update_with_name("head", queuing_header.head);
    updater->update_with_name("tail", queuing_header.tail);
    updater->update_with_name("queue_length", queuing_header.queue_length);
    updater->update_with_name("end_queue_space",
            queuing_header.end_queue_space);
    updater->update_with_name("write_id", queuing_header.write_id);
    updater->endClass("CMS_QUEUING_HEADER", 0);
    updater->set_mode(original_mode);
    return ((int) (status != CMS_UPDATE_ERROR && status != CMS_MISC_ERROR) ? 0 :
            -1);
}

int
CMS::get_encoded_msg_size() {
    if (force_raw) {
        return 0;
    }
    if (NULL == updater) {
        return (-1);
    }
    return (header.in_buffer_size = updater->get_encoded_msg_size());
}

int
CMS::check_pointer(char * ptr, long bytes) {
    /*  rcs_print_debug(PRINT_MISC,"CMS::check_pointer(ptr=%p,bytes=%ld) called.\n", ptr,bytes); */

    if (pointer_check_disabled > 0)
        return (0);
    if (force_raw) {
        return 0;
    }
    if (NULL == format_low_ptr || NULL == format_high_ptr
            || pointer_check_disabled) {
        return 0;
    }
    if (ptr < format_low_ptr || ptr > (format_high_ptr - bytes)) {
        cms_print_error
                ("CMS: pointer %p located %ld bytes from the base to %ld bytes out of range %p to %p (%ld) \n",
                ptr,
                (long) ((long) ptr - (long) format_low_ptr),
                bytes,
                format_low_ptr,
                format_high_ptr,
                (long) ((long) format_high_ptr - (long) format_low_ptr));
        cms_print_error("CMS: pointer %p to %ld bytes out of range %p to %p\n",
                ptr, bytes, format_low_ptr, format_high_ptr);
        cms_print_error("CMS: Check buffer and message sizes.\n");
        status = CMS_UPDATE_ERROR;
        return -1;
    }
    format_size = (long) (ptr - format_low_ptr) + bytes;
    return 0;
}

void
CMS::write_memory_map_string(long pos,
			     long size,
			     const char *name,
			     int maxlen,
			     const char *comment)
{

  int arraylen = var_arraylen;
  int elsize = (arraylen>1?(size/arraylen):size);

  if (!end_message_memory_map_cP) {
    end_message_memory_map_cP = message_memory_map_cP;
  }
  size_t bytes_used = (size_t)
    (end_message_memory_map_cP - message_memory_map_cP);
  size_t bytes_left = (size_t)
    (message_memory_map_max_size - bytes_used);
  if (bytes_left < 512) {
    message_memory_map_max_size += 8192;
    message_memory_map_cP =
      (char *) realloc(message_memory_map_cP,
		       message_memory_map_max_size);
    end_message_memory_map_cP = message_memory_map_cP + bytes_used;
  }
   if(this->set_memory_map_pos_offset_from_to_next_var)
    {
      this->memory_map_offset = pos;
      long sz = (cur_var_struct ? strlen(cur_var_struct) : 0)+strlen(name)+1;
      this->memory_map_pos_offset_from = (char *)
	malloc(sz);
      SNPRINTF_FUNC( SNPRINTF_ARGS(this->memory_map_pos_offset_from,sz),
		     "%s%s",(cur_var_struct ? cur_var_struct : ""),name);
      this->set_memory_map_pos_offset_from_to_next_var=false;
    }

   if(!this->memory_map_use_raw) {
     SNPRINTF_FUNC ( SNPRINTF_ARGS(end_message_memory_map_cP,bytes_left),
		     "%s,%s%s,%ld,%ld,%d,%d,%s,%d,%s\n",
		     ((this->var_type > 0 && 
		       this->var_type < cms_update_var_type_last) ? cms_type_string[this->var_type] : ""),
		     (cur_var_struct ? cur_var_struct : ""),
		     name, 
		     size, 
		     (pos-this->memory_map_offset),
		     elsize,
		     arraylen,
		     (memory_map_pos_offset_from?memory_map_pos_offset_from:""),
		     maxlen,
		     (comment?comment:""));  
   }
   else
     {
     SNPRINTF_FUNC ( SNPRINTF_ARGS(end_message_memory_map_cP,bytes_left),
		     "%s,%s%s,%ld,%ld,%d,%d,%s\n",
		     ((this->var_type > 0 && 
		       this->var_type < cms_update_var_type_last) ? cms_type_string[this->var_type] : ""),
		     (cur_var_struct ? cur_var_struct : ""),
		     name, 
		     size, 
		     (pos-this->memory_map_offset),
		     elsize,
		     arraylen,
		     (comment?comment:""));  
     }
  end_message_memory_map_cP += strlen(end_message_memory_map_cP);
}

void
CMS::write_memory_map_info(const char *name, 
			   char *ptr,
			   long  size)
{
  if(skip_print_memory_message_map)
    {
      return;
    }
  long pos = 0;
  if(this->memory_map_use_raw)
    {
      pos = (long) (ptr - format_low_ptr);
    }
  else 
    {
      pos = (long) get_encoded_msg_size();
    }
  write_memory_map_string(pos,size,name,0,0);
}


int
CMS::check_pointer_with_name(const char *name, 
			     char *ptr,
			     long bytes) {
    /*  rcs_print_debug(PRINT_MISC,"CMS::check_pointer_with_name(name=%s,ptr=%p,bytes=%ld) called.\n", (name?name:"(null)"),ptr,bytes); */
    if (pointer_check_disabled > 0)
        return (0);

    if (enable_message_memory_map) {
      write_memory_map_info(name,ptr,bytes);
    }
    if (force_raw) {
        return 0;
    }
    if (NULL == format_low_ptr || NULL == format_high_ptr
            || pointer_check_disabled) {
        return 0;
    }
    if (ptr < format_low_ptr || ptr > (format_high_ptr - bytes)) {
        cms_print_error
                ("CMS: pointer %p located %ld bytes from the base  to variable %s of %ld bytes out of range %p to %p (%ld) \n",
                ptr,
                (long) ((long) ptr - (long) format_low_ptr),
                name, bytes, format_low_ptr,
                format_high_ptr,
                (long) ((long) format_high_ptr - (long) format_low_ptr));
        cms_print_error("CMS: Check buffer and message sizes.\n");
        status = CMS_UPDATE_ERROR;
        return -1;
    }
    format_size = (long) (ptr - format_low_ptr) + bytes;
    return 0;
}

void
CMS::set_cms_status(CMS_STATUS new_status) {
    status = new_status;
}

/* Access functions for primitive C language data types */
CMS_STATUS
CMS::beginUnion(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginUnion(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endUnion(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endUnion(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginUnionVar(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginUnionVar(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endUnionVar(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endUnionVar(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginClass(const char *cname, const char *baseclass) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginClass(cname, baseclass));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endClass(const char *cname, const char *baseclass) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endClass(cname, baseclass));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginClassVar(const char *cname) {
    if (looking_for_dvar || this->enable_message_memory_map) {
        if (cname) {
            strcat(cur_var_struct, cname);
            strcat(cur_var_struct, ".");
        }
        if (strlen(cur_var_struct) != (unsigned int) var_struct_to_look_for_len ||
                strncmp(cur_var_struct, var_to_look_for, var_struct_to_look_for_len)) {
            inside_wrong_struct = true;
        } else {
            inside_wrong_struct = false;
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginClassVar(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endClassVar(const char *cname) {

    if (looking_for_dvar || this->enable_message_memory_map) {
        char *end_cur_var_struct = cur_var_struct + strlen(cur_var_struct) - 1;
        if (end_cur_var_struct >= cur_var_struct &&
                *end_cur_var_struct == '.') {
            *end_cur_var_struct = 0;
            end_cur_var_struct--;
        }
        while (end_cur_var_struct >= cur_var_struct &&
                *end_cur_var_struct && *end_cur_var_struct != '.') {
            *end_cur_var_struct = 0;
            end_cur_var_struct--;
        }
        if (strlen(cur_var_struct) != (unsigned int) var_struct_to_look_for_len ||
                strncmp(cur_var_struct, var_to_look_for, var_struct_to_look_for_len)) {
            inside_wrong_struct = true;
        } else {
            inside_wrong_struct = false;
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endClassVar(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginBaseClass(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginBaseClass(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endBaseClass(const char *cname) {
    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endBaseClass(cname));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginStructArrayElem(const char *cname, int elemnum) {
    if (looking_for_dvar || this->enable_message_memory_map) {
        SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf,
                sizeof (tbuf)),
                "%s[%d].", cname, elemnum);
        strcat(cur_var_struct, tbuf);
        if (strlen(cur_var_struct) != (unsigned int) var_struct_to_look_for_len ||
                strncmp(cur_var_struct, var_to_look_for, var_struct_to_look_for_len)) {
            inside_wrong_struct = true;
        } else {
            inside_wrong_struct = false;
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginStructArrayElem(cname, elemnum));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endStructArrayElem(const char *cname, int elemnum) {

    if (looking_for_dvar || this->enable_message_memory_map) {
        char *end_cur_var_struct = cur_var_struct + strlen(cur_var_struct) - 1;
        if (end_cur_var_struct >= cur_var_struct &&
                *end_cur_var_struct == '.') {
            *end_cur_var_struct = 0;
            end_cur_var_struct--;
        }
        while (end_cur_var_struct >= cur_var_struct &&
                *end_cur_var_struct && *end_cur_var_struct != '.') {
            *end_cur_var_struct = 0;
            end_cur_var_struct--;
        }
        if (strlen(cur_var_struct) != (unsigned int) var_struct_to_look_for_len ||
                strncmp(cur_var_struct, var_to_look_for, var_struct_to_look_for_len)) {
            inside_wrong_struct = true;
        } else {
            inside_wrong_struct = false;
        }
        return CMS_STATUS_NOT_SET;
    }


    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endStructArrayElem(cname, elemnum));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginStructDynamicArray(const char *cname,
        int &current_length, int max_length) {

    if(this->enable_message_memory_map && this->memory_map_use_raw) {
      current_length=max_length;
    }

    if(enable_message_memory_map && !memory_map_use_raw)
      {
	current_length=1;
	long start_pos = (long) get_encoded_msg_size();
	set_memory_map_pos_offset_from_to_next_var=true;
	this->var_type=cms_update_var_type_none;
	write_memory_map_string(start_pos,0,cname,max_length,"#begin_struct_dla");
        return CMS_STATUS_NOT_SET;
      }

    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (current_length > max_length) {
        cms_print_error
                ("Bad array length for %s current_length=%d, max_length=%d\n",
                cname, current_length, max_length);
        current_length = 0;
        return (status = CMS_UPDATE_ERROR);
    }
    if (NULL != updater) {
        return (updater->
                beginStructDynamicArray(cname, current_length, max_length));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endStructDynamicArray(const char *cname, int &current_length,
        int max_length) {

    if(this->enable_message_memory_map && this->memory_map_use_raw) {
      current_length=max_length;
    }

    if(enable_message_memory_map && !memory_map_use_raw)
      {
	current_length=1;
	long start_pos = (long) get_encoded_msg_size();
	this->var_type=cms_update_var_type_none;
	write_memory_map_string(start_pos,0,cname,max_length,"#end_struct_dla");
	set_memory_map_pos_offset_from_to_next_var=true;
        return CMS_STATUS_NOT_SET;
      }

    if (CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->
                endStructDynamicArray(cname, current_length, max_length));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::next_update_default(const char *next_default) {
    if (CMS_UPDATE_ERROR == status) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->
                next_update_default(next_default));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::check_for_dvar(const char *name, double d) {
    if (!inside_wrong_struct &&
            !dvar_found &&
            name && varname_only_to_look_for &&
            !strcmp(name, varname_only_to_look_for)) {
        dvar = d;
        dvar_found = true;
    }
    return CMS_STATUS_NOT_SET;
}

/* Access functions for primitive C language data types */
CMS_STATUS
CMS::update(bool &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(char &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned char &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(short int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned short int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(long int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned long int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#ifdef ENABLE_LONG_LONG_UP

CMS_STATUS
CMS::update(long long int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned long long int &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#else /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update(long long int &) {
  cms_print_error("cms::update(long long int &) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

CMS_STATUS
CMS::update(unsigned long long int &) {
  cms_print_error("cms::update(unsigned long long int &) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

#endif  /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update(float &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(double &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(long double &x) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(char *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned char *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(short *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned short *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(int *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned int *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}


CMS_STATUS
CMS::update(long *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned long *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#ifdef ENABLE_LONG_LONG_UP 

CMS_STATUS
CMS::update(long long *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(unsigned long long *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#else /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update(long long *, unsigned int) {
  cms_print_error("cms::update(long long *,unsigned int) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

CMS_STATUS
CMS::update(unsigned long long *, unsigned int) {
  cms_print_error("cms::update(unsigned long long *,unsigned int) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

#endif /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update(float *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}


CMS_STATUS
CMS::update(double *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update(long double *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update(x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

int
CMS::update_enumeration_with_name(const char *name,
        int enumin, void *enumaddr,
        const cms_enum_info * info) {
    this->var_type = cms_update_var_type_enumeration;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) enumin);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return -1;
    }

    if (NULL != updater) {
        return (updater->
                update_enumeration_with_name(name, enumin, enumaddr, info));
    } else {
        status = CMS_UPDATE_ERROR;
        return (-1);
    }
}

int
CMS::update_union_selector_with_name(const char *name,
        int enumin, void *enumaddr,
        const cms_enum_info * info) {
    if (CMS_UPDATE_ERROR == status || copymode) {
        return -1;
    }

    if (NULL != updater) {
        return (updater->
                update_union_selector_with_name(name, enumin, enumaddr, info));
    } else {
        status = CMS_UPDATE_ERROR;
        return (-1);
    }
}

/* Access functions for primitive C language data types */
CMS_STATUS
CMS::update_with_name(const char *name, bool &x) {
    this->var_type = cms_update_var_type_bool;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, char &x) {
    this->var_type = cms_update_var_type_char;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned char &x) {
    this->var_type = cms_update_var_type_uchar;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, short int &x) {
    this->var_type = cms_update_var_type_short;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned short int &x) {
    this->var_type = cms_update_var_type_ushort;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, int &x) {
    this->var_type = cms_update_var_type_int;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned int &x) {
    this->var_type = cms_update_var_type_uint;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}


CMS_STATUS
CMS::update_with_name(const char *name, long int &x) {
    this->var_type = cms_update_var_type_long;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned long int &x) {
    this->var_type = cms_update_var_type_ulong;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}


#ifdef ENABLE_LONG_LONG_UP

CMS_STATUS
CMS::update_with_name(const char *name, long long int &x) {
    this->var_type = cms_update_var_type_long_long;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned long long int &x) {
    this->var_type = cms_update_var_type_ulong_long;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#else /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update_with_name(const char *, long long int &) {
  cms_print_error("cms::update_with_name(const char *,long long int &) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

CMS_STATUS
CMS::update_with_name(const char *, unsigned long long int &) {
  cms_print_error("cms::update_with_name(const char *,unsigned long long int &) not implemented.\n");
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

#endif /* ENABLE_LONG_LONG_UP */


CMS_STATUS
CMS::update_with_name(const char *name, float &x) {
    this->var_type = cms_update_var_type_float;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, double &x) {
    this->var_type = cms_update_var_type_double;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, long double &x) {
    this->var_type = cms_update_var_type_ldouble;
    this->var_arraylen = 0;

    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, bool *x, unsigned int len) {
  this->var_type = cms_update_var_type_bool_array;
  this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, char *x, unsigned int len) {
    this->var_type = cms_update_var_type_char_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned char *x, unsigned int len) {
    this->var_type = cms_update_var_type_uchar_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode) {
        return CMS_STATUS_NOT_SET;
    }

    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, short *x, unsigned int len) {
    this->var_type = cms_update_var_type_short_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned short *x,
        unsigned int len) {
    this->var_type = cms_update_var_type_ushort_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, int *x, unsigned int len) {
    this->var_type = cms_update_var_type_int_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned int *x, unsigned int len) {
    this->var_type = cms_update_var_type_uint_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, long *x, unsigned int len) {
    this->var_type = cms_update_var_type_long_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned long *x, unsigned int len) {
    this->var_type = cms_update_var_type_ulong_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#ifdef ENABLE_LONG_LONG_UP

CMS_STATUS
CMS::update_with_name(const char *name, long long *x, unsigned int len) {
    this->var_type = cms_update_var_type_long_long_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, unsigned long long *x, 
		      unsigned int len) {
    this->var_type = cms_update_var_type_ulong_long_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

#else /* ENABLE_LONG_LONG_UP */

CMS_STATUS
CMS::update_with_name(const char *, long long *, unsigned int) {
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

CMS_STATUS
CMS::update_with_name(const char *, unsigned long long *, 
		      unsigned int) {
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

#endif /* ENABLE_LONG_LONG_UP */


CMS_STATUS
CMS::update_with_name(const char *name, float *x, unsigned int len) {
    this->var_type = cms_update_var_type_float_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, double *x, unsigned int len) {
    this->var_type = cms_update_var_type_double_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_with_name(const char *name, long double *x, unsigned int len) {
    this->var_type = cms_update_var_type_ldouble_array;
    this->var_arraylen = len;

    if (looking_for_dvar) {
        for (unsigned int i = 0; i < len; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, bool &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, char &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, unsigned char &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, short int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, unsigned short int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, unsigned int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, long int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, unsigned long int &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, float &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, double &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, long double &x) {
    if (looking_for_dvar) {
        return check_for_dvar(name, (double) x);
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, char *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name,
        unsigned char *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, short *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name,
        unsigned short *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, int *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, unsigned int *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, long *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name,
        unsigned long *x, unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, float *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, double *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, long double *x,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DURATION & x) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DURATION * x,
        int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DATE_TIME & x) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DATE_TIME * x,
        int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_TIME & x) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_TIME * x,
        int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DATE & x) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_attribute_with_name(const char *name, CMS_DATE * x,
        int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->update_attribute_with_name(name, x, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, char *x, int &len, int maxlen) {
    

  this->var_type = cms_update_var_type_char_array;
  this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }


    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, unsigned char *x,
        int &len, int maxlen) {
    
  this->var_type = cms_update_var_type_uchar_array;
  this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, short *x, int &len, int maxlen) {
    this->var_type = cms_update_var_type_short_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, unsigned short *x,
        int &len, int maxlen) {
  this->var_type = cms_update_var_type_ushort_array;
  this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, int *x, int &len, int maxlen) {
    this->var_type = cms_update_var_type_int_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, unsigned int *x,
        int &len, int maxlen) {
    this->var_type = cms_update_var_type_uint_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }
    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, long *x, int &len, int maxlen) {
    this->var_type = cms_update_var_type_long_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, unsigned long *x,
        int &len, int maxlen) {
    this->var_type = cms_update_var_type_ulong_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, float *x, int &len, int maxlen) {
    this->var_type = cms_update_var_type_float_array;
    this->var_arraylen = maxlen;

  if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
  else if(this->enable_message_memory_map && this->memory_map_use_raw) {
    len=maxlen;
  }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, double *x, int &len,
        int maxlen) {
    this->var_type = cms_update_var_type_double_array;
    this->var_arraylen = maxlen;

 
    if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	len=1;
	status = updater->update_dla_with_name(name, x, len, maxlen);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	long diff = end_pos - start_pos;
	write_memory_map_string(start_pos,diff,name,maxlen,"#dynamic_length_array");
	len=1;
	this->set_memory_map_pos_offset_from_to_next_var =true;
	return status;
      }
    else if(this->enable_message_memory_map && this->memory_map_use_raw) 
      {
	len=maxlen;
      }

    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::update_dla_with_name(const char *name, long double *x,
        int &len, int maxlen) {
    if (looking_for_dvar) {
        for (int i = 0; i < len && i < maxlen; i++) {
            SNPRINTF_FUNC(SNPRINTF_ARGS(tbuf, sizeof (tbuf)), "%s[%d]", name, i);
            check_for_dvar(tbuf, (double) (x[i]));
        }
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (len > maxlen) {
        cms_print_error("Array length for %s of %d is bad (max=%d).\n",
                name, len, maxlen);
        return (status = CMS_UPDATE_ERROR);
    }

    if (NULL != updater) {
        return (updater->update_dla_with_name(name, x, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

const char *
CMS::status_string(int status_type) {
  switch (status_type) {
    /* ERROR conditions */
  case CMS_MISC_ERROR:
    return ("CMS_MISC_ERROR:   A miscellaneous  error occured.");

  case CMS_UPDATE_ERROR:
    return ("CMS_UPDATE_ERROR: An error occured during an update. ");

  case CMS_INTERNAL_ACCESS_ERROR:
    return
      ("CMS_INTERNAL_ACCESS_ERROR: An error occured during an internal access function. ");

  case CMS_NO_MASTER_ERROR:
    return
      ("CMS_NO_MASTER_ERROR: An error occured because the master was not started.");

  case CMS_CONFIG_ERROR:
    return ("CMS_CONFIG_ERROR: There was an error in the configuration.");

  case CMS_TIMED_OUT:
    return ("CMS_TIMED_OUT: operation timed out.");

  case CMS_QUEUE_FULL:
    return
      ("CMS_QUEUE_FULL:=  A write failed because queuing was enabled but there was no room to add to the queue. ");

  case CMS_CREATE_ERROR:
    return
      ("CMS_CREATE_ERROR: Something could not be created because we were out of memory or another system resource.");

  case CMS_PERMISSIONS_ERROR:
    return ("CMS_PERMISSIONS_ERROR: Problem with permissions.");

    /* NON Error Conditions. */
  case CMS_STATUS_NOT_SET:
    return
      ("CMS_STATUS_NOT_SET: The status variable has not been set yet.");

  case CMS_READ_OLD:
    return ("CMS_READ_OLD:  Read successful, but data is old. \n");

  case CMS_READ_OK:
    return ("CMS_READ_OK: Read successful so far.");

  case CMS_WRITE_OK:
    return ("CMS_WRITE_OK:  Write successful so far. ");

  case CMS_WRITE_WAS_BLOCKED:
    return
      ("CMS_WRITE_WAS_BLOCKED: Write if read did not succeed, because the buffer had not been read yet.");

  case CMS_CLEAR_OK:
    return ("CMS_CLEAR_OK: A clear operation was successful.");

  case CMS_CLOSED:
    return ("CMS_CLOSED: The channel has been closed.");

  case CMS_NO_SERVER_ERROR:
    return
      (" CMS_NO_SERVER_ERROR: The server has not been started or could not be contacted.");

  case CMS_RESOURCE_CONFLICT_ERROR:
    return
      ("CMS_RESOURCE_CONFLICT_ERROR: Two or more CMS buffers are trying to use the same resource.");

  case CMS_NO_IMPLEMENTATION_ERROR:
    return
      ("CMS_NO_IMPLEMENTATION_ERROR: An operation was attempted which has not yet been implemented for the current platform or protocol.");

  case CMS_INSUFFICIENT_SPACE_ERROR:
    return
      ("CMS_INSUFFICIENT_SPACE_ERROR: The size of the buffer was insufficient for the requested operation.");

  case CMS_LIBRARY_UNAVAILABLE_ERROR:
    return
      ("CMS_LIBRARY_UNAVAILABLE_ERROR: A DLL or Shared Object library needed for the current protocol could not be found or initialized.");

  case CMS_SERVER_SIDE_ERROR:
    return ("CMS_SERVER_SIDE_ERROR: The server reported an error.");

  case CMS_NO_BLOCKING_SEM_ERROR:
    return
      ("CMS_NO_BLOCKING_SEM_ERROR: A blocking_read operartion was tried but no semaphore for the blocking was configured or available.");

  case CMS_INTERRUPTED_OPERATION:
    return
      ("CMS_INTERRUPTED_OPERATION: another thread or signal handler called interrupt_operation while (or before) this operation was in progress.");

  case CMS_SETUP_SUBSCRIPTION_OK:
    return("CMS_SETUP_SUBSCRIPTION_OK: a setup_subscription or cancel_subscription succeeded.");

  default:
    return ("UNKNOWN");
  }
}

const char *
CMS::short_status_string(int status_type) {
  switch (status_type) {
    /* ERROR conditions */
  case CMS_MISC_ERROR:
    return ("CMS_MISC_ERROR");

  case CMS_UPDATE_ERROR:
    return ("CMS_UPDATE_ERROR");

  case CMS_INTERNAL_ACCESS_ERROR:
    return ("CMS_INTERNAL_ACCESS_ERROR");

  case CMS_NO_MASTER_ERROR:
    return ("CMS_NO_MASTER_ERROR");

  case CMS_CONFIG_ERROR:
    return ("CMS_CONFIG_ERROR");

  case CMS_TIMED_OUT:
    return ("CMS_TIMED_OUT");

  case CMS_QUEUE_FULL:
    return ("CMS_QUEUE_FULL");

  case CMS_CREATE_ERROR:
    return ("CMS_CREATE_ERROR");

  case CMS_PERMISSIONS_ERROR:
    return ("CMS_PERMISSIONS_ERROR");

    /* NON Error Conditions. */
  case CMS_STATUS_NOT_SET:
    return ("CMS_STATUS_NOT_SET");

  case CMS_READ_OLD:
    return ("CMS_READ_OLD");

  case CMS_READ_OK:
    return ("CMS_READ_OK");

  case CMS_WRITE_OK:
    return ("CMS_WRITE_OK");

  case CMS_WRITE_WAS_BLOCKED:
    return ("CMS_WRITE_WAS_BLOCKED");

  case CMS_CLEAR_OK:
    return ("CMS_CLEAR_OK");

  case CMS_CLOSED:
    return ("CMS_CLOSED");

  case CMS_NO_SERVER_ERROR:
    return (" CMS_NO_SERVER_ERROR");

  case CMS_RESOURCE_CONFLICT_ERROR:
    return ("CMS_RESOURCE_CONFLICT_ERROR");

  case CMS_NO_IMPLEMENTATION_ERROR:
    return ("CMS_NO_IMPLEMENTATION_ERROR");

  case CMS_INSUFFICIENT_SPACE_ERROR:
    return ("CMS_INSUFFICIENT_SPACE_ERROR");

  case CMS_LIBRARY_UNAVAILABLE_ERROR:
    return ("CMS_LIBRARY_UNAVAILABLE_ERROR");

  case CMS_SERVER_SIDE_ERROR:
    return ("CMS_SERVER_SIDE_ERROR");

  case CMS_NO_BLOCKING_SEM_ERROR:
    return ("CMS_NO_BLOCKING_SEM_ERROR");

  case CMS_SETUP_SUBSCRIPTION_OK:
    return("CMS_SETUP_SUBSCRIPTION_OK");
  default:
    return ("UNKNOWN");
  }
}

int
CMS::set_subdivision(int _subdiv) {
    if (_subdiv < 0 || _subdiv > total_subdivisions) {
        return -1;
    }
    current_subdivision = _subdiv;
    subdiv_data = ((char *) data) + _subdiv * (subdiv_size);
    return (0);
}

/* Constructor used for hard coded tests and for non communications related
 CMS features such as format conversion */
/* Parameters: */

/* s - Size of the buffer. */
CMS::CMS(long s) :
consecutive_timeouts(0), header(), queuing_header(), mode(CMS_NOT_A_MODE),
size(s), free_space(s), max_message_size(0), max_encoded_message_size(0),
temp_max_encoded_message_size(0), restore_max_encoded_message_size(0),
guaranteed_message_space(0), status(CMS_STATUS_NOT_SET), encoded_data(0),
temp_encoded_data(0),
restore_encoded_data(0),
zero_encoded_data_when_set(0), data(0), subdiv_data(0), toggle_bit(0),
first_read_done(0), first_write_done(0), write_permission_flag(0),
read_permission_flag(0), rpc_program_number(0), http_port_number(0),
tcp_port_number(0), stcp_port_number(0),
udp_port_number(0), gdrs_im_port_number(0),
buffer_number(0),
total_messages_missed(0), messages_missed_on_last_read(0), format_low_ptr(0),
format_high_ptr(0), format_size(0), BufferType(CMS_SHMEM_TYPE),
ProcessType(CMS_AUTO_TYPE), remote_port_type(CMS_NO_REMOTE_PORT_TYPE),
pointer_check_disabled(0), in_buffer_id(0), encoded_header(0),
encoded_queuing_header(0), encoded_header_size(0),
encoded_queuing_header_size(0), neutral_encoding_method(CMS_NO_ENCODING),
temp_updater_encoding_method(CMS_NO_ENCODING),
internal_access_type(CMS_ZERO_ACCESS), handle_to_global_data(0),
dummy_handle(0), read_mode(CMS_NOT_A_MODE), write_mode(CMS_NOT_A_MODE),
read_updater_mode(0), write_updater_mode(0), last_im(CMS_NOT_A_MODE),
timeout(0), orig_timeout(0), read_timeout(0), write_timeout(0),
connection_number(0), total_connections(0),
updater(0), normal_updater(0), temp_updater(0), encode_state(0),
decode_state(0), sizeof_message_header(0), blocking_timeout(0),
orig_blocking_timeout(0), max_repeat_blocking_reads(0),
blocking_read_start(0), min_compatible_version(0),
confirm_write(0), disable_final_write_raw_for_dma(0),
total_subdivisions(0), current_subdivision(0), subdiv_size(0),
encoded_data_size(0), no_unbounded(0), temp_encoded_data_size(0),
restore_encoded_data_size(0), enc_max_size(0),
enable_diagnostics(0), first_diag_store(0),
pre_op_total_bytes_moved(0), time_bias(0), skip_area(0),
half_offset(0), half_size(0), size_without_diagnostics(0),
disable_diag_store(0), diag_offset(0),
last_id_side0(0), last_id_side1(0), use_autokey_for_connection_number(0),
update_cmd_msg_base(0), update_cmd_msg_base_in_format(0),
update_stat_msg_base(0), update_stat_msg_base_in_format(0),
bitwise_op(CMS_BITWISE_NOP), transfer_alias_list(0),
current_alias(0), copymode(0), endofcopybuf(0),
maxendofcopybuf(0),
copybuff(0), copybuff_size(0), xbase(0),
copymode_unbounded_struct_array_xbases(0),
copymode_unbounded_struct_array_copybuffs(0),
copymode_unbounded_struct_array_copybuff_sizes(0),
copymode_unbounded_struct_array_count(0),
max_copymode_unbounded_struct_array_count(0),
nmltypename(0),
nmlcfgsvr(0), xml_style_properties(0),
global_xml_style_properties_count(0),
spawn_server(0), mrpq(0), priority(0), default_priority(0), num_readers(0),
queue_length_to_wait_for(0), read_count(0), is_clear(0),
starting_wait_for_write_id(0), starting_wait_for_was_read(0),
starting_wait_for_queue_length(0), starting_wait_for_queue_head(0),
max_queue_length(0), max_size_from_size_list(0), extra_data(0), min_message_size(0), message_size_add(0), message_size_roundup(0),
use_ipv6(0),
di(0), dpi(0),
enable_xml_logging(false), enable_xml_differencing(false),
fatal_error_occurred(false), add_array_indexes_to_name(false),
unbounded_used(false), write_just_completed(false), isserver(false),
is_phantom(false), delete_totally(false), is_local_master(false),
force_raw(false), split_buffer(false),
using_external_encoded_data(false),
restore_using_external_encoded_data(false),
neutral(false), queuing_enabled(false), fast_mode(false),
interrupting_operation(false), leave_resource(false),
multireader_priority_queue_enabled(false),
priority_set(false), preserve_mrpq_reader_id(false),
waiting_for_queue_length_over(false), wait_for_initialized(false),
blocking_support_enabled(false),
keep_read_count(false), stop_when_connection_refused(false),
max_size_from_size_list_set(false),
searching_for_max_size_from_size_list(false), wait_for_master(false),
private_server_object(false),
fail_on_overflow(false),
cleaning_flag(false),
bind_proc_host(false),
looking_for_dvar(false),
inside_wrong_struct(false),
dvar_found(false),
var_to_look_for(0),
varname_only_to_look_for(0),
dvar(0.0),
var_struct_to_look_for_len(0),
get_msg_start_only(false),
temp_data_size(0),
memory_align(memory_align_default),
enable_message_memory_map(false),
message_memory_map_cP(0),
end_message_memory_map_cP(0),
message_memory_map_max_size(0),
var_type(cms_update_var_type_none),
memory_map_use_raw(false),
max_message_size_set_on_buffer_line(false),
skip_print_memory_message_map(false),
memory_map_pos_offset_from(0),
set_memory_map_pos_offset_from_to_next_var(false),
memory_map_offset(0),
var_arraylen(0),
checking_for_nsname_from_format_check_type_info(false),
nsname_from_format_check_type_info(0),
name_to_lookup_type_id(0),
type_id_looked_up(-1),
do_not_print_errors(false),
do_not_print_timeout_errors(false),
queuing_header_offset(0),
header_file_name(0),
uses_unbounded(false)
{
    /* Print a message if the PRINT_CMS_CONSTUCTORS */
    /* member of the print flags is set. */
    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "CMS::CMS(%ld)\n", s);

    stop_when_connection_refused = false;
    blocking_support_enabled = false;
    preserve_mrpq_reader_id = false;
    mrpq = 0;
    bitwise_op = CMS_BITWISE_NOP;
    priority = 0;
    default_priority = 0;
    num_readers = 1;
    priority_set = false;
    multireader_priority_queue_enabled = false;
    max_queue_length = -1;
    nmlcfgsvr = 0;
    searching_for_max_size_from_size_list = false;
    max_size_from_size_list_set = false;
    max_size_from_size_list = 0;
    extra_data = 0;
    wait_for_master = false;
    private_server_object = false;
    fail_on_overflow = false;
    cleaning_flag = false;

    interrupting_operation = false;
    leave_resource = false;
    current_alias = 0;
    transfer_alias_list = 0;
    add_array_indexes_to_name = true;

    copymode = 0;
    copybuff = 0;
    copybuff_size = 0;
    xbase = 0;
    copymode_unbounded_struct_array_xbases = 0;
    copymode_unbounded_struct_array_copybuffs = 0;
    copymode_unbounded_struct_array_copybuff_sizes = 0;
    copymode_unbounded_struct_array_count = 0;
    max_copymode_unbounded_struct_array_count = 0;
    nmltypename = 0;

    /* Init string buffers */
    memset(BufferName, 0, CMS_CONFIG_LINELEN);
    memset(BufferHost, 0, CMS_CONFIG_LINELEN);
    memset(ProcessName, 0, CMS_CONFIG_LINELEN);
    memset(BufferLine, 0, CMS_CONFIG_LINELEN);
    memset(ProcessLine, 0, CMS_CONFIG_LINELEN);
    memset(ProcessHost, 0, CMS_CONFIG_LINELEN);
    memset(buflineupper, 0, CMS_CONFIG_LINELEN);
    memset(proclineupper, 0, CMS_CONFIG_LINELEN);
    memset(PermissionString, 0, CMS_CONFIG_LINELEN);
    memset(cur_var_struct, 0, sizeof (cur_var_struct));
    memset(tbuf, 0, sizeof (tbuf));


    /* save constructor args */
    free_space = size = s;
    force_raw = 0;
    neutral = false;
    isserver = false;
    last_im = CMS_NOT_A_MODE;
    min_compatible_version = 0;
    confirm_write = 0;
    disable_final_write_raw_for_dma = 0;
    subdiv_data = 0;
    enable_diagnostics = 0;
    dpi = NULL;
    di = NULL;
    skip_area = 0;
    half_offset = s / 2;
    free_space = half_size = s / 2;
    fast_mode = false;
    disable_diag_store = 0;
    diag_offset = 0;
    enable_xml_differencing = false;
    enable_xml_logging = false;
    read_permission_flag = 0; /* Allow both read and write by default.  */
    write_permission_flag = 0;
    queuing_enabled = false;
    fatal_error_occurred = false;
    write_just_completed = false;
    neutral_encoding_method = CMS_XDR_ENCODING;
    sizeof_message_header = 0;
    blocking_timeout = 0;
    total_subdivisions = 1;
    subdiv_size = size;
    current_subdivision = 0;
    enc_max_size = s;
    max_encoded_message_size = s;
    last_id_side0 = 0;
    last_id_side1 = 0;
    handle_to_global_data = NULL;
    dummy_handle = (PHYSMEM_HANDLE *) NULL; /* Set pointers to NULL */
    temp_updater = (CMS_UPDATER *) NULL;
    zero_encoded_data_when_set = 1;
    updater = 0;
    update_cmd_msg_base = 0;
    update_cmd_msg_base_in_format = 1;
    update_stat_msg_base = 0;
    update_stat_msg_base_in_format = 1;


    temp_encoded_data = 0;
    temp_encoded_data_size = 0;
    temp_max_encoded_message_size = 0;
    restore_encoded_data_size = 0;
    restore_encoded_data = 0;
    restore_max_encoded_message_size = 0;
    restore_using_external_encoded_data = 0;

    /* so we'll know whether it really */
    /* points to something */

    delete_totally = false; /* If this object is deleted only do */
    /* normal delete instead of deleting totally. */

    mode = CMS_NOT_A_MODE; /* Force user to set the mode before using. */

    if (s > 0) {
        open(); /* Allocate memory and intialize XDR streams */
    }

    rcs_print_debug(PRINT_CMS_CONSTRUCTORS, "finished constructing CMS\n");

}

int
CMS::get_msg_count() {
    internal_access_type = CMS_GET_MSG_COUNT_ACCESS;
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    main_access(data);
    return (header.write_id);
}

int
CMS::get_read_count() {
    internal_access_type = CMS_GET_READ_COUNT_ACCESS;
    if (!keep_read_count && !queuing_enabled) {
        cms_print_error("get_read_count called buf keep_read_count not set.\n");
        status = CMS_MISC_ERROR;
        return -1;
    }
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    main_access(data);
    return (read_count);
}

int
CMS::get_is_clear() {
    internal_access_type = CMS_GET_IS_CLEAR_ACCESS;
    status = CMS_STATUS_NOT_SET;
    blocking_timeout = 0;
    main_access(data);
    return (is_clear);
}

char *
cms_check_for_host_alias(char *in) {
    if (NULL == in) {
        return NULL;
    }
    if (NULL == cmsHostAliases) {
        return NULL;
    }
    CMS_HOST_ALIAS_ENTRY *entry =
            (CMS_HOST_ALIAS_ENTRY *) cmsHostAliases->get_head();
    while (NULL != entry) {
        if (!strncmp(entry->alias, in, 64)) {
            return entry->host;
        }
        entry = (CMS_HOST_ALIAS_ENTRY *) cmsHostAliases->get_next();
    }
    return NULL;
}

CMS_STATUS
CMS::beginEnumerationArray(const char *name, const cms_enum_info * info,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;

    this->var_type = cms_update_var_type_enumeration_array;
    if(this->enable_message_memory_map)
      {
	write_memory_map_info(name,0,4*len);
      }

    if (NULL != updater) {
        return (updater->beginEnumerationArray(name, info, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::beginEnumerationDLA(const char *name, const cms_enum_info * info,
        int &len, int maxlen) {

    this->var_type = cms_update_var_type_enumeration_array;
    if(this->enable_message_memory_map) {
      if(! this->memory_map_use_raw)
	{
	  long start_pos = get_encoded_msg_size();
	  write_memory_map_string(start_pos,4,name,maxlen,"#dynamic_length_array");
	}
      else
	{
	  len=maxlen;
	}
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->beginEnumerationDLA(name, info, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endEnumerationArray(const char *name, const cms_enum_info * info,
        unsigned int len) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endEnumerationArray(name, info, len));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

CMS_STATUS
CMS::endEnumerationDLA(const char *name, const cms_enum_info * info,
        int &len, int maxlen) {


  if(this->enable_message_memory_map)
    {
      if(this->memory_map_use_raw) {
	len=maxlen;
      }
      else{
	set_memory_map_pos_offset_from_to_next_var=true;
      }
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->endEnumerationDLA(name, info, len, maxlen));
    } else {
        return (status = CMS_UPDATE_ERROR);
    }
}

int
CMS::update_enumeration_array_elem(int enumin, void *enumaddr, int elem) {

    this->var_type = cms_update_var_type_enumeration_array;
    if (looking_for_dvar) {
        return CMS_STATUS_NOT_SET;
    }

    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return (updater->
                update_enumeration_array_elem(enumin, enumaddr, elem));
    } else {
        status = CMS_UPDATE_ERROR;
        return -1;
    }
}

void
CMS::find_max_size(const size_t *sizelist, long list_length) {
    int i = 0;
    size_t max_size = 0;
    size_t s;
    if (!sizelist || list_length < 1) {
        return;
    }
    for (i = 0; i < list_length; i++) {
        if (sizelist[i] == 0) {
            break;
        }
        if (sizelist[i] < min_message_size) {
            cms_print_error("Bad sizelist value sizelist[%d] = %lu\n",
                    i, (unsigned long) sizelist[i]);
            return;
        }
        s = sizelist[i];
        s += message_size_add;
        if (message_size_roundup > 0 && (s % message_size_roundup) > 0) {
            s += message_size_roundup - (s % message_size_roundup);
        }
        if (max_size < s) {
            max_size = s;
        }
    }
    if (max_size_from_size_list < max_size) {
        max_size_from_size_list = max_size;
        max_size_from_size_list_set = true;
    }
}

void
CMS::find_type_id( const char **namelist,
		   const long *idlist,
		   const long list_length, 
		   const long max_name_length)
{
  const char * namePtr = 0;
  type_id_looked_up=-1;
  if(!namelist || !idlist || list_length < 1 || 
     !name_to_lookup_type_id)
    {
      return;
    }
  for(int i = 0; i < list_length; i++)
    {
      namePtr = ( ((const char *)namelist) + i*max_name_length);
      if(!namePtr) {
	return;
      }
      if(!strcmp(name_to_lookup_type_id,namePtr))
	{
	  type_id_looked_up = idlist[i];
	  return;
	}
    }
}

/*
 * This function was added to support XML encoding.
 * which needs more info about available class names before
 * determining the type.
 */
long
CMS::check_type_info(long type, void *buffer, 
		     const char *nsname,
		     cms_symbol_lookup_function_t symbol_lookup_function, 
		     const char **namelist,
		     const long *idlist, const size_t * sizelist,
		     long list_length, long max_name_length) 
{
  if (checking_for_nsname_from_format_check_type_info) {
    if(nsname)
      {
	nsname_from_format_check_type_info=nsname;
      }
    return 0;
  }
  
  if(name_to_lookup_type_id)
    {
      find_type_id(namelist,idlist,list_length,max_name_length);
      return 0;
    }


  if (searching_for_max_size_from_size_list) {
    find_max_size(sizelist, list_length);
    return 0;
  }
  xbase = buffer;

  if (0 != updater) {
    return updater->check_type_info(type, buffer, nsname,
				    symbol_lookup_function, namelist,
				    idlist, sizelist, list_length,
				    max_name_length);
  }
  return type;
}

/* This function is only useable with encoding methods such as 
   xml that allow partial messages to be sent. */
CMS_STATUS
CMS::setBufferForDiff(void *diffbuf, size_t diffbuffsize) {
    if (0 != updater) {
        return updater->setBufferForDiff(diffbuf, diffbuffsize);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update(CMS_DURATION & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_DURATION * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d, len);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DURATION & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update_with_name(name, d);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DURATION * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update_with_name(name, d, len);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_DATE_TIME & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_DATE_TIME * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d, len);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DATE_TIME & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update_with_name(name, d);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DATE_TIME * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;

    if (0 != updater) {
        return updater->update_with_name(name, d, len);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_TIME & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_TIME * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d, len);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_TIME & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update_with_name(name, d);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_TIME * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;

    if (0 != updater) {
        return updater->update_with_name(name, d, len);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_DATE & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d);
    }
    return status;
}

CMS_STATUS
CMS::update(CMS_DATE * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update(d, len);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DATE & d) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
    if (0 != updater) {
        return updater->update_with_name(name, d);
    }
    return status;
}

CMS_STATUS
CMS::update_with_name(const char *name, CMS_DATE * d, int len) {
    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;

    if (0 != updater) {
        return updater->update_with_name(name, d, len);
    }
    return status;
}

CMS_STATUS
CMS::update_dla_length_with_name(const char *name, int &len) {

    if (status == CMS_UPDATE_ERROR || copymode)
        return CMS_STATUS_NOT_SET;
  
    if(this->enable_message_memory_map && !this->memory_map_use_raw)
      {
	this->var_type = cms_update_var_type_int;
	long start_pos = (long) get_encoded_msg_size();
	bool orig_skip_print_memory_message_map=
	  this->skip_print_memory_message_map;
	this->skip_print_memory_message_map=true;
	status = updater->update_dla_length_with_name(name, len);
	this->skip_print_memory_message_map=
	  orig_skip_print_memory_message_map;
	long end_pos = (long) get_encoded_msg_size();
	write_memory_map_string(start_pos,(end_pos-start_pos),name,0,"#dynamic_length");
	len=1;
	return status;
      }


    if (looking_for_dvar) {
        return check_for_dvar(name, (double) len);
    }


    if (0 != updater) {
        return updater->update_dla_length_with_name(name, len);
    }
    return status;
}

int
CMS::xmlSetStyleProperty(const char *propstring) {
    int retval = 0;
    if (normal_updater && neutral_encoding_method == CMS_XML_ENCODING) {
        retval = normal_updater->xmlSetStyleProperty(propstring);
        if (retval < 0) {
            return retval;
        }
    }
    if (temp_updater && temp_updater_encoding_method == CMS_XML_ENCODING) {
        retval = temp_updater->xmlSetStyleProperty(propstring);
        if (retval < 0) {
            return retval;
        }
    } else if (updater == normal_updater) {
        set_temp_updater(CMS_XML_ENCODING);
        retval = temp_updater->xmlSetStyleProperty(propstring);
        restore_normal_updater();
    }
    return retval;
}

int
CMS::setTransferAlias(const char *name) {
    if (name == 0) {
        current_alias = 0;
        return 0;
    }
    if (name[0] == 0) {
        current_alias = 0;
        return 0;
    }
    if (transfer_alias_list) {
        CMS_TRANSFER_ALIAS *alias =
                (CMS_TRANSFER_ALIAS *) transfer_alias_list->get_head();
        while (alias) {
            if (!strcmp(alias->name, name)) {
                current_alias = alias;
                return 0;
            }
            alias = (CMS_TRANSFER_ALIAS *) transfer_alias_list->get_head();
        }
    }
    return -1;
}

int
CMS::addTransferAlias(const char *name,
        transfer_from_function_ptr fptr,
        transfer_to_function_ptr tptr, void *extra_info) {
    if (transfer_alias_list == 0) {
        transfer_alias_list = new RCS_LINKED_LIST();
    }
    CMS_TRANSFER_ALIAS tempalias;
    strncpy(tempalias.name, name, sizeof (tempalias.name));
    tempalias.fptr = fptr;
    tempalias.tptr = tptr;
    tempalias.extra_info = extra_info;
    tempalias.data = 0;
    transfer_alias_list->store_at_tail(&tempalias, sizeof (tempalias), 1);
    return 0;
}

CMS_STATUS
CMS::copy_to_copybuff(void **x, int len) {
    int offset = 0;
    void *newx = 0;
    void **locx = 0;

    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;

    offset = (int) (((char *) x) - ((char *) xbase));
    newx = endofcopybuf;

    endofcopybuf = ((char *) endofcopybuf) + len;
    if (endofcopybuf > maxendofcopybuf) {
        cms_print_error("CMS::copy_to_copybuff space on copybuf exceeded\n");
        status = CMS_UPDATE_ERROR;
        return status;
    }
    locx = (void **) (((char *) copybuff) + offset);
    if (locx < copybuff || locx >= newx) {
        cms_print_error
                ("CMS::copy_to_copybuff bad locx=%p,offset=%d(0x%X),x=%p,*x=%p,xbase=%p,copybuff_size=%lu(0x%lX),newx=%p,copybuff=%p,endofcopybuf=%p,len=%d(0x%X),maxendofcopybuf=%p\n",
                (void *) locx, offset, offset, (void *) x, (void *) (*x), (void *) xbase,
                (unsigned long) copybuff_size, (unsigned long) copybuff_size,
                newx, copybuff, endofcopybuf, len, len, maxendofcopybuf);
        status = CMS_UPDATE_ERROR;
        return status;
    }
    *locx = newx;
    memcpy(newx, *x, len);
    return status;
}

CMS_STATUS
CMS::update_unbounded_attribute_with_name(const char *name, char **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_attribute_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, char **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name,
        unsigned char **x, int &len,
        int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name
(const char *name, short **x, int &len, int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name
(const char *name, unsigned short **x, int &len, int &size_allocated) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, int **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name,
        unsigned int **x, int &len,
        int &size_allocated) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, long **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name,
        unsigned long **x, int &len,
        int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, float **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, double **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (copymode) {
        return copy_to_copybuff((void **) x, sizeof (**x) * len);
    }
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::update_unbounded_with_name(const char *name, long double **x,
        int &len, int &size_allocated) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->update_unbounded_with_name(name, x, len,
                size_allocated);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::beginStructUnboundedArray(const char *name, void **x,
        int &len, int &size_allocated,
        size_t elsize) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (copymode) {
        if (copymode_unbounded_struct_array_xbases == 0 ||
                copymode_unbounded_struct_array_copybuffs == 0 ||
                copymode_unbounded_struct_array_copybuff_sizes == 0) {
            max_copymode_unbounded_struct_array_count = 10;
            copymode_unbounded_struct_array_xbases = (void **)
                    DEBUG_MALLOC(max_copymode_unbounded_struct_array_count *
                    sizeof (void *));
            copymode_unbounded_struct_array_copybuffs =
                    (void **) DEBUG_MALLOC(max_copymode_unbounded_struct_array_count
                    * sizeof (void *));
            copymode_unbounded_struct_array_copybuff_sizes =
                    (size_t *) DEBUG_MALLOC(max_copymode_unbounded_struct_array_count
                    * sizeof (size_t));
        }
        if (copymode_unbounded_struct_array_count + 1 >=
                max_copymode_unbounded_struct_array_count) {
            max_copymode_unbounded_struct_array_count += 10;
            copymode_unbounded_struct_array_xbases = (void **)
                    DEBUG_REALLOC((void *) copymode_unbounded_struct_array_xbases,
                    max_copymode_unbounded_struct_array_count *
                    sizeof (void *));
            copymode_unbounded_struct_array_copybuffs =
                    (void **) DEBUG_REALLOC((void *)
                    copymode_unbounded_struct_array_copybuffs,
                    max_copymode_unbounded_struct_array_count
                    * sizeof (void *));
            copymode_unbounded_struct_array_copybuff_sizes =
                    (size_t *) DEBUG_REALLOC((void *)
                    copymode_unbounded_struct_array_copybuff_sizes,
                    max_copymode_unbounded_struct_array_count
                    * sizeof (size_t));
        }

        copymode_unbounded_struct_array_xbases
                [copymode_unbounded_struct_array_count] = xbase;
        copymode_unbounded_struct_array_copybuffs
                [copymode_unbounded_struct_array_count] = copybuff;
        copymode_unbounded_struct_array_copybuff_sizes
                [copymode_unbounded_struct_array_count] = copybuff_size;
        copymode_unbounded_struct_array_count++;
        copy_to_copybuff(x, (int) (elsize * len));
        xbase = *x;
        copybuff_size = elsize * len;
        copybuff = ((char *) endofcopybuf) - (copybuff_size);
        return status;
    }
    if (NULL != updater) {
        return updater->beginStructUnboundedArray(name, x, len, size_allocated,
                elsize);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::endStructUnboundedArray(const char *name, void **x, int &len,
        int &size_allocated, size_t elsize) {
    unbounded_used = true;

    if(enable_message_memory_map)
      {
	set_memory_map_pos_offset_from_to_next_var=true;
        return CMS_STATUS_NOT_SET;
      }

    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (copymode) {
        if (copymode_unbounded_struct_array_count > 0) {
            copymode_unbounded_struct_array_count--;
            copybuff =
                    copymode_unbounded_struct_array_copybuffs
                    [copymode_unbounded_struct_array_count];
            copybuff_size =
                    copymode_unbounded_struct_array_copybuff_sizes
                    [copymode_unbounded_struct_array_count];
            xbase =
                    copymode_unbounded_struct_array_xbases
                    [copymode_unbounded_struct_array_count];
        }
        return status;
    }
    if (NULL != updater) {
        return updater->endStructUnboundedArray(name, x, len, size_allocated,
                elsize);
    }

    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::beginEnumerationUnbounded(const char *name, int **x,
        const cms_enum_info * info,
        int &len, int &size_allocated,
        size_t elsize) {
    unbounded_used = true;
    if (no_unbounded || CMS_UPDATE_ERROR == status)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->beginEnumerationUnbounded(name, x, info, len,
                size_allocated, elsize);
    }
    return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS::endEnumerationUnbounded(const char *name, int **x,
        const cms_enum_info * info, int &len,
        int &size_allocated, size_t elsize) {
    if (CMS_UPDATE_ERROR == status || copymode)
        return CMS_STATUS_NOT_SET;
    if (NULL != updater) {
        return updater->endEnumerationUnbounded(name, x, info, len,
                size_allocated, elsize);
    }
    return CMS_STATUS_NOT_SET;
}

int
CMS::setCopyBuff(void *ptr, size_t start_size, size_t end_size) {
    if (ptr) {
        copymode = 1;
        copybuff = ptr;
        endofcopybuf = ((char *) ptr) + start_size;
        maxendofcopybuf = ((char *) ptr) + end_size;
        copybuff_size = start_size;
    } else {
        copybuff = 0;
        copymode = 0;
        endofcopybuf = 0;
        maxendofcopybuf = 0;
        copybuff_size = 0;
    }
    return 0;
}

#ifndef ENABLE_RCS_DIAG

CMS_DIAG_PROC_INFO *CMS::get_diag_proc_info() {
    cms_print_error("RCS library compiled without DIAG support.\n");
    return 0;
}

void CMS::set_diag_proc_info(CMS_DIAG_PROC_INFO *) {
    cms_print_error("RCS library compiled without DIAG support.\n");
}

void CMS::setup_diag_proc_info() {
    cms_print_error("RCS library compiled without DIAG support.\n");
}

void
CMS::calculate_and_store_diag_info(
        PHYSMEM_HANDLE *,
        void *) {
    cms_print_error("RCS library compiled without DIAG support.\n");
}

void
CMS::internal_retrieve_diag_info(
    PHYSMEM_HANDLE *,
    void *) {
    cms_print_error("RCS library compiled without DIAG support.\n");
}

CMS_DIAGNOSTICS_INFO *CMS::get_diagnostics_info() {
    cms_print_error("RCS library compiled without DIAG support.\n");
    return 0;
}

// ENABLE_RCS_DIAG
#endif

void CMS::interrupt_operation(void) {
    interrupting_operation = true;
}

void CMS::clear_interrupt_operation() {
    interrupting_operation = false;
}

int CMS::get_priority(void) {
    return priority;
}

int CMS::get_default_priority(void) {
    return default_priority;
}

void CMS::set_priority(int _priority) {
    priority = _priority;
    priority_set = true;
}

void CMS::set_default_priority(int _default_priority) {
    default_priority = _default_priority;
    priority_set = true;
}

void CMS::reset_priority(void) {
    priority = default_priority;
    priority_set = false;
}

int CMS::get_current_mrpq_reader_id() {
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        return mrpq->get_current_id();
    }
#endif
    return -1;
}

int CMS::get_new_mrpq_reader_id() {
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        internal_access_type = CMS_GET_NEW_READER_ID_ACCESS;
        status = CMS_STATUS_NOT_SET;
        main_access(0);
        return mrpq->get_current_id();
    }
#endif
    return -1;
}

void
CMS::set_current_mrpq_reader_id(
#if ENABLE_RCS_CMS_MRPQ
    int _id
#else
    int
#endif
    ) {
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        mrpq->set_pending_id(_id);
        status = CMS_STATUS_NOT_SET;
        internal_access_type = CMS_SET_READER_ID_ACCESS;
        main_access(0);
    }
#endif
}

void
CMS::remove_current_mrpq_reader_id() {
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        status = CMS_STATUS_NOT_SET;
        internal_access_type = CMS_REMOVE_READER_ID_ACCESS;
        main_access(0);
    }
#endif
}

void
CMS::set_preserve_mrpq_reader_id(bool _b) {
    preserve_mrpq_reader_id = _b;
#if ENABLE_RCS_CMS_MRPQ
    if (mrpq) {
        mrpq->set_preserve_mrpq_reader_id(_b);
    }
#endif
}

enum CMS_BITWISE_OP_TYPE
CMS::get_bitwise_op(void) {
    return bitwise_op;
}

void
CMS::set_bitwise_op(CMS_BITWISE_OP_TYPE t) {
    bitwise_op = t;
}

void
CMS::reset_bitwise_op(void) {
    bitwise_op = CMS_BITWISE_NOP;
}

void
CMS::set_leave_resource(bool b) {
    leave_resource = b;
}

void
CMS::set_message_memory_map_size(size_t _size_needed) {
    size_t new_size = _size_needed;
    if (message_memory_map_cP && new_size < message_memory_map_max_size) {
        return;
    }
    new_size += (8192 - _size_needed % 4096);

    message_memory_map_cP = (char *) realloc(message_memory_map_cP, new_size);
    message_memory_map_max_size = new_size;
}

#ifndef DISABLE_RCS_PRINT
int
CMS::print_error(const char * _fmt, ...)
{
 int retval = 0;
  va_list args;
  va_start (args, _fmt);
  if(!do_not_print_errors &&
     (status != CMS_TIMED_OUT || 
      (!do_not_print_timeout_errors && !getRcsDoNotPrintTimeoutErrors())))
    {
      retval = vprint_rcs_error_new(_fmt,args);
    }
  va_end(args);
  return retval;      
}

int
CMS::print_timeout_error(const char * _fmt, ...)
{
 int retval = 0;
  va_list args;
  va_start (args, _fmt);
  if(!do_not_print_errors && 
     !do_not_print_timeout_errors && 
     !getRcsDoNotPrintTimeoutErrors())
    {
      retval = vprint_rcs_error_new(_fmt,args);
    }
  va_end(args);
  if(status >= 0)
    {
      status = CMS_TIMED_OUT;
    }
  return retval;      
}

void CMS::set_header_file(const char *_header_file_name) {
  this->header_file_name = _header_file_name;
}

const char *CMS::get_header_file(void) {
  return this->header_file_name;
}

void CMS::set_uses_unbounded(bool _uses_unbounded) {
  this->uses_unbounded = _uses_unbounded;
}

bool CMS::get_uses_unbounded(void) {
  return this->uses_unbounded;
}

#endif
// end of #ifndef DISABLE_RCS_PRINT
