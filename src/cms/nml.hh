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
* File: nml.hh                                                           *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the Neutral Manufacturing Language (NML). *
*          Includes:                                                     *
*               1. class NML.                                            *
*               2. typedef for NMLTYPE.                                  *
*               3. typedef for NML_FORMAT_PTR                            *
*               4. enum NML_ERROR_TYPE                                   *
*************************************************************************/

#ifndef NML_HH
#define NML_HH

#include "cms_types.hh" // CMS_PACKED_ENCODING

/* Use only partial definition to avoid */
class NMLmsg;			/* depending on nmlmsg.hh. */
class CMS;			// ditto cms.hh
class RCS_LINKED_LIST;		// ditto linklist.hh
class NML_SERVER;		// ditto nml_srv.hh
class SchemaGenMsg;		// ditto cms_xml_up.hh

class NML_EXTRA_THREAD_INFO;	// defined in nml.cc but not in a very portable way.
class NML_INTERNALS;		// class to hide some protected NML variables.

#ifndef NMLTYPE_TYPEDEFED
#define NMLTYPE_TYPEDEFED
typedef long NMLTYPE;		/* Also defined in nmlmsg.hh */
#endif

#ifndef TRANSFER_FROM_FUNCTION_PTR_TYPEDEFED
#define TRANSFER_FROM_FUNCTION_PTR_TYPEDEFED
// Also defined in cms_user.hh
typedef void (*transfer_from_function_ptr) (CMS *, void *from,
					    unsigned long from_size, void *to,
					    unsigned long to_size);
#endif

#ifndef TRANSFER_TO_FUNCTION_PTR_TYPEDEFED
#define TRANSFER_TO_FUNCTION_PTR_TYPEDEFED
// Also defined in cms_user.hh
typedef void (*transfer_to_function_ptr) (CMS *, void *from, unsigned long from_size,
					  void *to, unsigned long to_size);
#endif

#ifndef NML_FORMAT_PTR_TYPEDEFED
#define NML_FORMAT_PTR_TYPEDEFED
/* Typedef for pointer to the function used to decode a message */
 /* by its id number. */
typedef int (*NML_FORMAT_PTR) (NMLTYPE, void *, CMS *);
#endif

#include "nmlcfgsvr_clntcalls.hh"


/* Values for NML::error_type. */
enum NML_ERROR_TYPE
{
  NML_NO_ERROR,
  NML_BUFFER_NOT_READ,
  NML_TIMED_OUT,
  NML_INVALID_CONFIGURATION,
  NML_FORMAT_ERROR,
  NML_INTERNAL_CMS_ERROR,
  NML_NO_MASTER_ERROR,
  NML_INVALID_MESSAGE_ERROR,
  NML_QUEUE_FULL_ERROR,
  NML_INVALID_CONSTRUCTOR_ARG,
  NML_INTERRUPTED_OPERATION,
  NML_NO_FORMAT_FUNCTION,
  NML_OUT_OF_MEMORY_ERROR,
  NML_IGNORED_REMOTE,
  NML_IGNORED_NO_BUFLINE,
  NML_FORMAT_NAME_DOES_NOT_MATCH_ERROR,
  NML_REQUIRED_MSG_TYPE_ERROR
};

enum NML_CHANNEL_TYPE
{
  INVALID_NML_CHANNEL_TYPE = 0,
  NML_GENERIC_CHANNEL_TYPE,
  RCS_CMD_CHANNEL_TYPE,
  RCS_STAT_CHANNEL_TYPE,
  NML_QUERY_CHANNEL_TYPE,
  NML_ID_CHANNEL_TYPE
};

enum NML_BITWISE_OP_TYPE {
  NML_BITWISE_NOP,
  NML_BITWISE_AND_OP,
  NML_BITWISE_OR_OP
};

class NML_DIAGNOSTICS_INFO;
class NML_EXTRA_STRING_BUFFERS;
class NMLCFG_SERVER_DATA;
class NML_C_DATA;

enum NML_CONNECTION_MODE
{
  NML_NORMAL_CONNECTION_MODE = 0,	// all config file parameters are honored.
  NML_FORCE_LOCAL_CONNECTION_MODE = 1,	// all connections are forced to be local
  NML_FORCE_REMOTE_CONNECTION_MODE = 2	// all connections are forced to be remote
};

extern "C" {
  extern void nmlGetBufferInfo(const char *bufferName,
			       char *, const int maxlen);

}


/* nml interface to CMS. */
class NML
{
protected:
  int run_format_chain (NMLTYPE, void *);
  int format_input (NMLmsg * nml_msg);	/* Format message if neccessary */
  int format_output ();		/* Decode message if neccessary. */
  int internal_xml_log(const char *log_type, NMLmsg *msg);
  class NML_INTERNALS *ni;

  friend void nmlGetBufferInfo(const char *bufferName,
			       char *buffer, 
			       const int maxlen);

public:
  CMS *cms;
  CMS *cms_for_read;
  RCS_LINKED_LIST *format_chain;
  long *cms_inbuffer_header_size;
  void *extra_data;
  void register_with_server ();	/* Add this channel to the server's list. */
  void unregister_with_server ();	/* Remove this channel from the server's list. */
  void spawn_and_register_server_if_needed( int set_to_server);
  void add_to_channel_list ();	/* Add this channel to the main list.  */
  enum NML_ERROR_TYPE error_type; /* check here if an NML function returns -1 */
  /* Get Address of message for user after read. */
  NMLTYPE (*phantom_read) ();
  NMLTYPE (*phantom_peek) ();
  int (*phantom_write) (NMLmsg * nml_msg);
  int (*phantom_write_if_read) (NMLmsg * nml_msg);
  int (*phantom_check_if_read) ();
  int (*phantom_clear) ();
  int ignore_format_chain;

  NMLmsg *get_address ();
  void delete_channel ();

  /* Read and Write Functions. */
  NMLTYPE read ();		/* Read the buffer. */
  NMLTYPE blocking_read (double timeout);	/* Read the buffer. (Wait for new data). */
  int wait_for_anything(double timeout);
  int wait_for_read(double timeout);
  int wait_for_clear(double timeout);
  int wait_for_write(double timeout);
  int wait_for_queue_length_over(int , double timeout);
  int wait_for_queue_length_under(int , double timeout);
  
  NMLTYPE peek ();		/* Read buffer without changing was_read */
  NMLTYPE read (void *, long);
  NMLTYPE peek (void *, long);
  int write (NMLmsg & nml_msg);	/* Write a message. (Use reference) */
  int write (NMLmsg * nml_msg);	/* Write a message. (Use pointer) */
  int write_with_priority (NMLmsg & nml_msg, int priority);	/* Write a message. (Use reference) */
  int write_with_priority (NMLmsg * nml_msg, int priority);	/* Write a message. (Use pointer) */
  int write_with_bitwise_op(NMLmsg & nml_msg, enum NML_BITWISE_OP_TYPE op);	/* Write a message. (Use reference) */
  int write_with_bitwise_op(NMLmsg * nml_msg, enum NML_BITWISE_OP_TYPE op);	/* Write a message. (Use pointer) */
  int write_if_read (NMLmsg & nml_msg);	/* Write only if buffer was_read */
  int write_if_read (NMLmsg * nml_msg);	/*  '' */
  NMLTYPE blocking_read_extended (double timeout, double poll_interval);

  NMLTYPE get_msg_type();

  int write_subdivision (int subdiv, NMLmsg & nml_msg);	/* Write a message. (Use reference) */
  int write_subdivision (int subdiv, NMLmsg * nml_msg);	/* Write a message. (Use pointer) */
  int write_if_read_subdivision (int subdiv, NMLmsg & nml_msg);	/* Write only if buffer was_read */
  int write_if_read_subdivision (int subdiv, NMLmsg * nml_msg);	/*  '' */
  NMLTYPE read_subdivision (int subdiv);	/* Read the buffer. */
  NMLTYPE blocking_read_subdivision (int subdiv, double timeout);	/* Read the buffer. (Wait for new data). */
  NMLTYPE peek_subdivision (int subdiv);	/* Read buffer without changing was_read */
  NMLmsg *get_address_subdivision (int subdiv);
  int get_total_subdivisions ();

  void clean_buffers ();
  const char *msg2str (NMLmsg & nml_msg);
  const char *msg2str (NMLmsg * nml_msg);
  const char *msg2xml (NMLmsg & nml_msg);
  const char *msg2xml (NMLmsg * nml_msg);
  const char *xmlSchema (void);
  int xmlSchemaSaveAs (const char *filename);
  int xmlMsgSaveAs (NMLmsg * nml_msg, const char *filename);
  int xmlMsgSaveAs (NMLmsg & nml_msg, const char *filename);
  NMLmsg *readMsgFromXmlFile (const char *filename);
  int xmlSetStyleProperty (const char *propstring);
  int addTransferAlias (const char *name,
			transfer_from_function_ptr fptr,
			transfer_to_function_ptr tptr,
			NML_FORMAT_PTR format_func);
  int setTransferAlias (const char *name);

  NMLTYPE str2msg (const char *);
  NMLTYPE xml2msg (const char *);
  int setMessageForDiff (NMLmsg * nml_msg);

  enum NMLCFGSVR_STATUS getCfgSvrStatus();

  int login (const char *name, const char *passwd);
  void reconnect ();
  void disconnect ();

  int setup_subscription(double _subscription_period);
  int cancel_subscription();


  /*
    Get a multi-line string that describes the offset to each variable 
    of the given message type and encoding method
    as well as other properties such as its type, whether it is an array or 
    dynamic length array. The first line is a header line and then one line
    for each variable with comma seperated fields.
    The buffer the string is written to may be overwritten by subsequent
    operations on this NML object and will be freed when this 
    NML object is deleted.
  */
  const char *getEncodedMessageMemoryMap(NMLmsg *nml_msgP,  
					 const int encoding_method);


  /*
    Writes the string that would have been obtained with 
    getEncodedMessageMemoryMap to the file filename.
  */
  void writeEncodedMessageMemoryMapToFile(NMLmsg *nml_msgP,  
					  const int encoding_method, 
					  const char *filename);


  /*
    Get a multi-line string that describes the offset to each variable 
    of the given message type as stored in memory on this platform
    as well as other properties such as its type, whether it is an array or 
    dynamic length array. The first line is a header line and then one line
    for each variable with comma seperated fields.
    The buffer the string is written to may be overwritten by subsequent
    operations on this NML object and will be freed when this 
    NML object is deleted.
  */
  const char *getRawMessageMemoryMap(NMLmsg *nml_msgP);


  /*
    Writes the string that would have been obtained with 
    getRawMessageMemoryMap to the file filename.
  */
  void writeRawMessageMemoryMapToFile(NMLmsg *nml_msgP,
				      const char *filename);


  /* 
     Run the given nml_msg through the NML data marshalling using the given
     encoding_method but do not send it anywhere. 
     The encoded version of the data will be pointed to by encoded_data pointer
     and the size stored in encoded_size. The buffer encoded_data points to may
     be overwritten by subsequent operations on this NML object. The buffer will
     be freed when this NML object is deleted.
  */
  void msg_to_encoded_data(NMLmsg *nml_msg, 
		   void *&encoded_data, 
                   long &encoded_size,
		   const int  _encoding_method);

  /* 
     Read a message file and pass the contents through 
     the NML data marshalling using the given
     encoding_method.
     Return a pointer to the decoded message that can then be cast to 
     a message pointer of the appropriate type.
     The buffer storing the message returned to may
     be overwritten by subsequent operations on this NML object. The buffer will
     be freed when this NML object is deleted.
  */
  NMLmsg *read_encoded_data_from_file (const char *filename, 
				       const int _encoding_method=CMS_PACKED_ENCODING);

  /* 
     Run the given nml_msg through the NML data marshalling using the given
     encoding_method and write the resulting encoded data to the file filaname. 
  */
  void write_encoded_data_to_file(NMLmsg *nml_msg, 
        const int _encoding_method, 
        const char *filename);


  /* 
     Pass the provided encoded_data through 
     the NML data marshalling using the given
     encoding_method.
     Return a pointer to the decoded message that can then be cast to 
     a message pointer of the appropriate type.
     The buffer storing the message returned to may
     be overwritten by subsequent operations on this NML object. The buffer will
     be freed when this NML object is deleted.
  */
  NMLmsg * encoded_data_to_msg(
		    void *encoded_data, 
		    long encoded_size,
		    const int _encoding_method);

  /* Function to check to see if this NML object is properly configured. */
  int valid ();

  /* Make just the check_if_read function from cms available to NML users. */
  int check_if_read ();

  /* For use with DMA */
  int check_if_transfers_complete ();

  /* Make just the clear  function from cms available to NML users. */
  int clear ();

  /* Get the number of messages written to this buffer so far. */
  int get_msg_count ();


  /* Get the number of times the buffer has been read. 
   (WARNING: not supported by default or on all platforms/protocols). */
  int get_read_count ();

  /* Does the buffer contain no data/ not even old data, ie clear was called. 
   (WARNING: not supported by default or on all platforms/protocols). */
  int get_is_clear ();

  /* Get an approximate estimate of the space available to store 
     messages in, for non queuing buffers this is just the fixed size of 
     the buffer, for queuing buffers it subtracts the space used by
     messages in the queue.
   */
  int get_space_available ();

  /* How many messages are currently stored in the queue. */
  int get_queue_length ();

  /* Get Diagnostics Information. */
  NML_DIAGNOSTICS_INFO *get_diagnostics_info ();


  int prefix_format_chain (NML_FORMAT_PTR);
  int copyMsg (NMLmsg *, void *addr, unsigned long max_size);

  int get_ni_pid();

  /* Constructors and destructors. */
#ifdef LINUXCNC_LIBNML_COMPAT
  NML (NML_FORMAT_PTR f_ptr,
       const char *, const char *, const char *,
       int set_to_server = 0, int set_to_master = 0);
  NML (NML_FORMAT_PTR f_ptr,
       const char *, const char *, const char *,
       int set_to_server, int set_to_master, enum NML_CONNECTION_MODE connect_mode);

#else
  NML (NML_FORMAT_PTR f_ptr,
       const char *, const char *, const char *,
       int set_to_server = 0, int set_to_master = 0, enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);
#endif
  NML (NML *, int set_to_server = 0, int set_to_master = 0,  enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);
  NML (const char *buffer_line, const char *proc_line, int set_to_server = 0);
  NML (class NML_C_DATA *);

  virtual ~NML();

  /* Attempt to clear past errors, similiar to deleting this NML object 
     and creating a new on with identical constructor parameters.*/
  int reset ();

  /* Print some QUEUE related diagnostics info to the RCS_PRINT_DESTINATION, ussually stdout */
  int print_queue_info ();

  /* Sets the NML error type from CMS status.*/
  int set_error ();
  
  /* Print some diagnostics info to the RCS_PRINT_DESTINATION, ussually stdout */
  void print_info (const char *_bufname = 0, 
		   const char *_procname =0, 
		   const char *_cfg_file = 0);

  /* If another thread is running a blocking function ie blocking_read,
     make it return immediately with an error. (this might also be called from  a signal handler.) */
  void interrupt_operation(void);
  
  /* After calling interupt_operation, call this to allow normal operations 
     to return. */
  void clear_interrupt_operation(void);

  /* Set to true to leave resources, shared memory segments semaphores etc
     around after this object is deleted. */
  void set_leave_resource(bool);

  /* Get a variable converted to double  from a message. */
  double get_double_var(NMLmsg *, const char *varname, bool &got_dvar);

#ifdef LINUXCNC_LIBNML_COMPAT
#define NML_NEW_DELETE 1
#endif

#ifdef NML_NEW_DELETE
  void *operator                          new(size_t);
  void operator                          delete(void *);
#endif

protected:
  int queue_length;
  bool fast_mode;
  NML (char *, char *, char *, 
       enum NML_CHANNEL_TYPE ct,
       int set_to_server = 0, 
       int set_to_master =0,
       enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);
  NML (RCS_LINKED_LIST *,
       const char *, 
       const char *, 
       const char *, 
       enum NML_CHANNEL_TYPE ct,
       int set_to_server = 0, 
       int set_to_master =0,
       enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);

  void base_setup(
		  const char *, const char *, const char *, enum NML_CHANNEL_TYPE ct, int set_to_server = 0, int set_to_master =0, enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);
  void reconstruct (NML_FORMAT_PTR, const char *, const char *, const char *,
		    int set_to_server = 0, int set_to_master = 0);

  /* 
   * Get the size of the largest message that this channel should need to handle. 
   * The data is retrieved from arguments passed to CMS::check_type_info() in
   * the automatically generated format function.
   * Used by the nmlcfgsvr support code and the file reading/writing functions.
   */
  unsigned long get_max_size_from_format(void);


  /* 
   * Get a name associated with this channels format function.
   * The data is retrieved from arguments passed to CMS::check_type_info() in
   * the automatically generated format function.
   * Used to check that the config file matches the format function passed to
   * the NML contstructor.
   * Returned string should NOT be freed or overwritten by the user.
   */
  const char * get_name_from_format(void);

  NMLTYPE get_type_id_for_name(const char *name);

  void check_format_name(void);

  int cfgsvr_query(const char *bufname,
		   const char *filename, 
		   const char *cfgsvr,
		   int set_to_server);
  void setup_cc_bufs(NML_FORMAT_PTR _f_ptr=0);
  void setup_cc_list_format_chain();
  void write_cc_list(NMLmsg *);
  
  int info_printed;

public:
  enum NML_CHANNEL_TYPE channel_type;
  long sizeof_message_header;
  int forced_type;
  bool interrupting_operation;
  bool leave_resource;

public:
  bool already_deleted;
  class NML_SERVER *immediate_spawned_server;
  class NML_EXTRA_THREAD_INFO *extra_thread_info;
  RCS_LINKED_LIST *cc_list;
  const char *header_file_name;
  bool uses_unbounded;

protected:
  // Don't copy me.
  NML (const NML & nml);		
  NML &operator=(const NML &nml);
  void format_chain_setup(void);
  
static int spawn_cloning_function(enum NML_CHANNEL_TYPE ct,
				  RCS_LINKED_LIST *format_chain, char *buf, char *proc, char *file);
public:
  class NML *loopback;
  static size_t forced_min_size;
  static bool forced_min_size_env_checked;
};


int create_NML (NML **, NML_FORMAT_PTR f_ptr,
			   char *buf, char *proc, char *file);

void free_NML (NML *);


extern RCS_LINKED_LIST *NML_Main_Channel_List;

extern "C"
{
  extern void nml_start ();
  extern void nml_cleanup ();
  extern void nml_set_kill_servers_on_cleanup (bool);
  extern void nml_wipeout_lists ();
  extern void set_default_nml_config_file (const char *);
  extern const char *get_default_nml_config_file ();
  extern NML *nmlWaitOpen (NML_FORMAT_PTR fPtr, char *buffer,
				      char *name, char *file,
				      double sleepTime);
  extern void mark_cancelled_thread_id(void *tidP);
  extern int  is_cancelled_thread_id(void *tidP);

  extern void nmlSetHostAlias (const char *hostName, const char *hostAlias);
  extern void nmlClearHostAliases ();
  extern void nmlAllowNormalConnection ();
  extern void nmlForceRemoteConnection ();
  extern void nmlForceLocalConnection ();
  extern void nmlSetToServer(void);
  extern void nmlSetIgnoreRemote(void);
  extern void nmlSetIgnoreNoBufline(void);
  extern void nmlSetIgnoreFormatName(void);
  extern void nmlGetBufferList(char *, const int maxlen, const char sepChar);
  extern const char * nmlGetStaticBufferList(const char sepChar);

  extern const char * nmlGetStaticBufferInfo(const char *bufferName);

  extern const char * nmlGetRemoteTcpServerBufferList(const char *hostName, 
							 const short port,
							 const int use_ipv6);

  extern const char * nmlGetRemoteTcpServerBufferInfo(const char *bufferName,
						      const char *hostName, 
						      const short port,
						      const int use_ipv6);
						      
}

extern NML *nmlGetBufferFromListByName(const char *bufferName);
extern void nml_remove_all_not_on_list(RCS_LINKED_LIST *preserve_list);
extern void set_default_nmlcfgsvr_options(const char *);
extern bool nml_allow_null_f_ptr;
extern bool verbose_nml_error_messages;
extern bool nml_print_hostname_on_error;
extern int nml_reset_errors_printed;
extern bool nml_cleanup_started;

#endif /* !NML_HH */
