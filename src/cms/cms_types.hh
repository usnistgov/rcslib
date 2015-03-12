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

#ifndef CMS_TYPES_HH
#define CMS_TYPES_HH

/* Return values for CMS::read, CMS::write, ... */
enum CMS_STATUS
{
/* ERROR conditions */
  CMS_MISC_ERROR = -1,		/* A miscellaneous  error occured. */
  CMS_UPDATE_ERROR = -2,	/* An error occured during an update. */
  CMS_INTERNAL_ACCESS_ERROR = -3,	/* An error occured during an internal access function. */
  CMS_NO_MASTER_ERROR = -4,	/* An error occured becouse */
  /* the master was not started */
  CMS_CONFIG_ERROR = -5,	/* There was an error in the configuration */
  CMS_TIMED_OUT = -6,		/* operation timed out. */
  CMS_QUEUE_FULL = -7,		/* A write failed because queuing was */
  /* enabled but there was no room to add to */
  /* the queue. */
  CMS_CREATE_ERROR = -8,	/* Something could not be created.  */
  CMS_PERMISSIONS_ERROR = -9,	// Problem with permissions
  CMS_NO_SERVER_ERROR = -10,	// The server has not been started or could not be contacted.
  CMS_RESOURCE_CONFLICT_ERROR = -11,	// Two or more CMS buffers are trying to use the same resource.
  CMS_NO_IMPLEMENTATION_ERROR = -12,	// An operation was attempted which has not yet been implemented for the current platform or protocol.
  CMS_INSUFFICIENT_SPACE_ERROR = -13,	// The size of the buffer was insufficient for the requested operation.
  CMS_LIBRARY_UNAVAILABLE_ERROR = -14,	// A DLL or Shared Object library needed for the current protocol could not be found or initialized.
  CMS_SERVER_SIDE_ERROR = -15,	// The server reported an error.
  CMS_NO_BLOCKING_SEM_ERROR = -16,	// A blocking_read operartion was tried but no semaphore for the blocking was configured or available.

  CMS_INTERRUPTED_OPERATION=-17, // another thread called interrupt_operation
  // while or before this operation was in progress.

/* NON Error Conditions.*/
  CMS_STATUS_NOT_SET = 0,	/* The status variable has not been set yet. */
  CMS_READ_OLD = 1,		/* Read successful, but data is old. */
  CMS_READ_OK = 2,		/* Read successful so far. */
  CMS_WRITE_OK = 3,		/* Write successful so far. */
  CMS_WRITE_WAS_BLOCKED = 4,	/* Write if read did not succeed, because */
  /* the buffer had not been read yet. */
  CMS_CLEAR_OK = 5,		/* A clear operation was successful.  */
  CMS_CLOSED = 6,		/* The channel has been closed.  */
  CMS_WAIT_FOR_READ_OK = 7,	/* wait for read waited until another read occured. */
  CMS_WAIT_FOR_WRITE_OK = 8,	/* wait for write waited until another write occured. */
  CMS_WAIT_FOR_QUEUE_LENGTH_OK = 9,	/* wait for queue_length waited until the required queue length occured. */
  CMS_WAIT_FOR_CLEAR_OK = 10,	/* wait for clear waited until the required queue length occured. */
  CMS_WAIT_FOR_READ_INCOMPLETE = 11,	/* wait for read has not yet waited  until another read occured. */
  CMS_WAIT_FOR_WRITE_INCOMPLETE = 12,	/* wait for write has not yet waited until another write occured. */
  CMS_WAIT_FOR_QUEUE_LENGTH_INCOMPLETE = 13,	/* wait for queue_length has not yet waited until the required queue length occured. */
  CMS_WAIT_FOR_CLEAR_INCOMPLETE = 14,	/* wait for clear waited until the required queue length occured. */
  CMS_SETUP_SUBSCRIPTION_OK=15 /* a setup_subscription or cancel_subscription succeeded */

};


/* Mode used within update functions. */
enum CMSMODE
{
  CMS_NOT_A_MODE = 0,
  CMS_ENCODE,
  CMS_DECODE,
  CMS_RAW_OUT,
  CMS_RAW_IN,
  CMS_READ,
  CMS_WRITE
};

typedef long int CMSID;

/* Mode stored for use by the internal access function. */
enum CMS_INTERNAL_ACCESS_TYPE
{
  CMS_ZERO_ACCESS = 0,
  CMS_READ_ACCESS,
  CMS_CHECK_IF_READ_ACCESS,
  CMS_PEEK_ACCESS,
  CMS_WRITE_ACCESS,
  CMS_WRITE_IF_READ_ACCESS,
  CMS_CLEAR_ACCESS,
  CMS_GET_MSG_COUNT_ACCESS,
  CMS_GET_DIAG_INFO_ACCESS,
  CMS_GET_QUEUE_LENGTH_ACCESS,
  CMS_GET_SPACE_AVAILABLE_ACCESS,
  CMS_GET_NEW_READER_ID_ACCESS,
  CMS_SET_READER_ID_ACCESS,
  CMS_REMOVE_READER_ID_ACCESS,
  CMS_WAIT_FOR_WRITE_ACCESS,
  CMS_WAIT_FOR_READ_ACCESS,
  CMS_WAIT_FOR_QUEUE_LENGTH_ACCESS,
  CMS_WAIT_FOR_CLEAR_ACCESS,
  CMS_WAIT_FOR_ANYTHING_ACCESS,
  CMS_GET_READ_COUNT_ACCESS,
  CMS_GET_IS_CLEAR_ACCESS
};

/* What type of global memory buffer. */
enum CMS_BUFFERTYPE
{
  CMS_SHMEM_TYPE,
  CMS_GLOBMEM_TYPE,
  CMS_PHANTOM_BUFFER,
  CMS_LOCMEM_TYPE,
  CMS_FILEMEM_TYPE,
  CMS_BBDMEM_TYPE,
  CMS_RTLMEM_TYPE,
  CMS_OEMEM_TYPE
};

/* How will this process access the buffer. */
enum CMS_PROCESSTYPE
{
  CMS_REMOTE_TYPE,
  CMS_LOCAL_TYPE,
  CMS_PHANTOM_USER,
  CMS_AUTO_TYPE
};

enum CMS_REMOTE_PORT_TYPE
{
  CMS_NO_REMOTE_PORT_TYPE = 0,
  CMS_RPC_REMOTE_PORT_TYPE,
  CMS_TTY_REMOTE_PORT_TYPE,
  CMS_TCP_REMOTE_PORT_TYPE,
  CMS_STCP_REMOTE_PORT_TYPE,
  CMS_UDP_REMOTE_PORT_TYPE,
  CMS_HTTP_REMOTE_PORT_TYPE,
  CMS_OE_REMOTE_PORT_TYPE,
  CMS_GDRS_IM_REMOTE_PORT_TYPE
};

enum CMS_NEUTRAL_ENCODING_METHOD
{
  CMS_NO_ENCODING,
  CMS_XDR_ENCODING,
  CMS_ASCII_ENCODING,
  CMS_DISPLAY_ASCII_ENCODING,
  CMS_XML_ENCODING,
  CMS_PACKED_ENCODING,
  CMS_PACKEDL64_ENCODING
};

#ifndef CMS_SYMBOL_LOOKUP_FUNCTION_TYPEDEFED
#define CMS_SYMBOL_LOOKUP_FUNCTION_TYPEDEFED
#ifdef __cplusplus
extern "C" {
#endif
typedef const char *(*cms_symbol_lookup_function_t) (long);
#ifdef __cplusplus
}
#endif
/*  CMS_SYMBOL_LOOKUP_FUNCTION_TYPEDEFED */
#endif

enum CMS_BITWISE_OP_TYPE {
  CMS_BITWISE_NOP,
  CMS_BITWISE_AND_OP,
  CMS_BITWISE_OR_OP
};

enum CMS_CONNECTION_MODE
{
  CMS_NORMAL_CONNECTION_MODE = 0,	// all config file parameters are honored.
  CMS_FORCE_LOCAL_CONNECTION_MODE = 1,	// all connections are forced to be local
  CMS_FORCE_REMOTE_CONNECTION_MODE = 2	// all connections are forced to be remote
};

extern enum CMS_CONNECTION_MODE cms_connection_mode;

#ifndef CMS_CONFIG_LINELEN
#define CMS_CONFIG_LINELEN 512
#endif

// CMS_TYPES_HH
#endif
