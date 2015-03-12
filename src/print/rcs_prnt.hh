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


#ifndef RCS_PRNT_HH
#define RCS_PRNT_HH

#include <stddef.h>		/* size_t */

#ifndef __GNUC__
#ifndef __attribute__
#define __attribute__(x)
#define I_DEFINED_ATTRIBUTE
#endif
#endif

#ifdef DISABLE_RCS_PRINT

#ifdef __GNUC__
static inline int  __attribute__ ((const)) __attribute__ ((format (printf, 1,2))) rcs_print_null_func(__attribute__((unused)) const char *_x,...) {return(0);} 
static inline int  __attribute__ ((const)) __attribute__ ((format (printf, 2,3))) rcs_print_debug_null_func(__attribute__((unused)) long _l, __attribute__((unused)) const char *_x,...) {return(0);} 
#define rcs_puts(x) 
#define rcs_print if(0) rcs_print_null_func
#define rcs_print_debug if(0) rcs_print_debug_null_func
#define rcs_print_error if(0) rcs_print_null_func
#define rcs_print_warning  if(0) rcs_print_null_func
#define rcs_print_sys_error if(0) rcs_print_null_func
#else
#define rcs_puts if(0)
#define rcs_print if(0)
#define rcs_print_debug if(0)
#define rcs_print_error if(0)
#define rcs_print_warning  if(0)
#define rcs_print_sys_error if(0)
#endif
#define set_rcs_print_flag(x)
#define set_rcs_print_tag(x)
#define set_rcs_print_destination(x)
#define set_abort_on_rcs_error(x)
#define set_pause_on_rcs_error(x)
#define set_rcs_print_file(x)
#ifndef WIN32
#define X_INLINE inline
#else
#define X_INLINE
#endif
static X_INLINE void null_init_rcs_print_mode_flags(void) {}
#define init_rcs_print_mode_flags null_init_rcs_print_mode_flags
#define set_rcs_print_do_flush(x) 

#ifdef __cplusplus
extern "C"
{
#endif

extern int separate_words_with_buffer(char ** _dest, int _max,
				      const char * _src, 
				      char *buf, size_t buflen);

#ifdef __cplusplus
}
#endif

#else
/* DISABLE_RCS_PRINT */

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
class RCS_LINKED_LIST;

extern class RCS_LINKED_LIST * get_rcs_print_list ();

#ifdef  ENUM_LIST_SIZING_MODE_DEFINED
extern void  set_rcs_print_list_sizing_mode (int, enum LIST_SIZING_MODE);
#endif

#endif

#ifdef __cplusplus
extern "C"
{
#endif

  extern void  clean_print_list (void);
  extern void  output_print_list (int output_func (char *));
  extern int  count_characters_in_print_list (void);
  extern int  count_lines_in_print_list (void);
  extern void  convert_print_list_to_lines (void);
  extern void  update_lines_table (void);
  extern int  rcs_print (const char * _fmt, ...) __attribute__ ((format (printf, 1, 2)));
#ifdef DO_NOT_USE_RCS_PRINT_ERROR_NEW
  extern int  rcs_print_error (char * _fmt, ...)__attribute__ ((format (printf, 1,2 ))) ;
#else
  extern int  set_print_rcs_error_info (const char *file, int line);

  extern int  print_rcs_error_new (const char * _fmt, ...)  __attribute__ ((format (printf, 1, 2)));
  extern int  print_rcs_warning_new (const char * _fmt, ...) __attribute__ ((format (printf, 1, 2))) ;
#define rcs_print_error set_print_rcs_error_info( __FILE__, __LINE__), print_rcs_error_new
#define rcs_print_warning set_print_rcs_error_info( __FILE__, __LINE__), print_rcs_warning_new
#endif

#ifdef DISABLE_RCS_DEBUG_PRINT
#ifdef __GNUC__
#if __GNUC__ >= 3
static inline int  __attribute__ ((format (printf, 2,3))) rcs_print_debug_null_func(__attribute__ ((unused)) long __lx, __attribute__ ((unused)) const char *__x,...) {return(0);} 
#else
static inline int __attribute__ ((format (printf, 2,3))) rcs_print_debug_null_func(long __lx, const char *__x, ...) {return(0);} 
#endif
#define rcs_print_debug if(0) rcs_print_debug_null_func
#else
#define rcs_print_debug if(0)
#endif
  /* __GNUC__ */

static inline void null2_init_rcs_print_mode_flags(void) {}
#define init_rcs_print_mode_flags null2_init_rcs_print_mode_flags


#else
  /* DISABLE_RCS_DEBUG_PRINT */

#ifdef DO_NOT_USE_RCS_PRINT_DEBUG_NEW
  extern int  rcs_print_debug (long flags_to_check, const char * _fmt, ...)  __attribute__ ((format (printf, 2,3)));
#else
  extern int  set_print_rcs_debug_info (const char *file, int line);
  extern int  print_rcs_debug_new (long flags_to_check,
					     const char * _fmt, ...)  __attribute__ ((format (printf, 2,3)));
  extern unsigned long rcs_print_mode_flags;
  extern char rcs_debugging_enabled;

#define rcs_print_debug if(rcs_debugging_enabled) set_print_rcs_debug_info( __FILE__, __LINE__), print_rcs_debug_new
#ifdef __cplusplus
  extern bool rcs_print_mode_flags_set;
#endif
#endif
  /* DO_NOT_USE_RCS_PRINT_DEBUG_NEW */

  extern int init_rcs_print_mode_flags(void);
#endif
  /* DISABLE_RCS_DEBUG_PRINT */
  
  extern void  set_rcs_print_flag (unsigned long flags_to_set);
  extern void  set_rcs_print_tag (const char *tag);
  extern void  clear_rcs_print_flag (unsigned long flags_to_set);
  extern void  set_rcs_print_do_flush(int);

  extern char * strip_control_characters (char *
							    _dest,
							    char *
							    _src);


  extern int separate_words_with_buffer(char ** _dest, int _max,
				      const char * _src, 
				      char *buf, size_t buflen);

  extern int  rcs_puts (const char *);
  extern int  rcs_fputs (const char *);

  extern char ** get_rcs_lines_table (void);
  extern int  get_rcs_print_list_size (void);
  extern int  set_rcs_print_file (const char * _file_name);
  extern void  close_rcs_printing (void);
  extern void setRcsDoNotPrintTimeoutErrors(int);
  extern int  getRcsDoNotPrintTimeoutErrors(void);

#ifdef NEED_VPRINT
#include <stdarg.h>
  extern int  vprint_rcs_error_new (const char *,va_list);
#endif


#ifdef __cplusplus
}
#endif

/* DISABLE_RCS_PRINT */
#endif


/* Print MODE flags. */
#define PRINT_RCS_ERRORS                0x00000001	/* 1 */
#define PRINT_NODE_CONSTRUCTORS         0x00000002	/* 2 */
#define PRINT_NODE_DESTRUCTORS          0x00000004	/* 4 */
#define PRINT_CMS_CONSTRUCTORS          0x00000008	/* 8 */
#define PRINT_CMS_DESTRUCTORS           0x00000010	/* 16 */
#define PRINT_NML_CONSTRUCTORS          0x00000020	/* 32 */
#define PRINT_NML_DESTRUCTORS           0x00000040	/* 64 */

#define PRINT_COMMANDS_RECIEVED         0x00000100	/* 256 */
#define PRINT_COMMANDS_SENT             0x00000200	/* 512 */
#define PRINT_STATUS_RECIEVED           0x00000400	/* 1024 */
#define PRINT_STATUS_SENT               0x00000800	/* 2048 */
#define PRINT_NODE_CYCLES               0x00001000	/* 4096 */
#define PRINT_NODE_MISSED_CYCLES        0x00002000	/* 8192 */
#define PRINT_NODE_CYCLE_TIMES          0x00004000	/* 16384 */
#define PRINT_NODE_PROCESS_TIMES        0x00008000	/* 32768 */
#define PRINT_NEW_WM                    0x00010000	/* 65536 */
#define PRINT_NODE_ABORT                0x00020000	/* 131072 */
#define PRINT_CMS_CONFIG_INFO           0x00040000	/* 262144 */
#define PRINT_SOCKET_READ_SIZE          0x00080000	/* 524288 */
#define PRINT_SOCKET_WRITE_SIZE         0x00100000	/* 1048576 */
#define PRINT_INTERFACE_LOADING         0x00200000	/* 2097152 */
#define PRINT_RPC_SERVER_CALL           0x00400000	/* 4194304 */
#define PRINT_SEMAPHORE_ACTIVITY        0x00800000	/* 8388608 */
#define PRINT_SOCKET_CONNECT            0x01000000	/* 16777216 */
#define PRINT_SERVER_THREAD_ACTIVITY    0x02000000	/* 33554432 */
#define PRINT_SERVER_SUBSCRIPTION_ACTIVITY    0x04000000	/* 67108864 */
#define PRINT_SHARED_MEMORY_ACTIVITY    0x08000000
#define PRINT_ALL_SOCKET_REQUESTS       0x10000000
#define PRINT_UPDATER_ACTIVITY          0x20000000
#define PRINT_MISC                      0x40000000
#define PRINT_EVERYTHING                0xFFFFFFFF	/* 4294967295 */

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
enum RCS_PRINT_DESTINATION_TYPE
{
#else
typedef enum
{
#endif
  RCS_PRINT_TO_STDOUT =0,
  RCS_PRINT_TO_STDERR =1,
  RCS_PRINT_TO_NULL =2,
  RCS_PRINT_TO_LIST =3,
  RCS_PRINT_TO_FILE =4,
  RCS_PRINT_TO_MESSAGE_BOX =5,	/* Only available for Windows */
  RCS_PRINT_TO_LOGGER=6,		/* Only available for VXWORKS */

// The following came from rpatel@gdrs.com Rcslib_Tweaks_for_GDRS.doc 
// Monday Nov. 27, 2006
  RCS_PRINT_TO_WIN32_DEBUGGER=7	/* Only available for Win32 DEBUG */

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
};
#else
}
RCS_PRINT_DESTINATION_TYPE;
#endif

#ifndef DISABLE_RCS_PRINT

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
extern "C"
{
#endif

  extern void 
    set_rcs_print_destination (RCS_PRINT_DESTINATION_TYPE);
  extern RCS_PRINT_DESTINATION_TYPE 
    get_rcs_print_destination (void);

  extern int  rcs_print_sys_error (int error_source,
					     const char * _fmt, ...);


  extern void set_abort_on_rcs_error(int);
  extern void set_pause_on_rcs_error(int);

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
}
#endif

extern int max_rcs_errors_to_print;
extern int rcs_errors_printed;

extern char last_error_bufs[4][100];
extern int last_error_buf_filled;

/* DISABLE_RCS_PRINT */
#endif

#if defined(__CPLUSPLUS__) || defined(__cplusplus)
  enum RCS_PRINT_ERROR_SOURCE_TYPE
  {
#else
  typedef enum
  {
#endif
    ERRNO_ERROR_SOURCE = 1,
    GETLASTERROR_ERROR_SOURCE,
    WSAGETLASTERROR_ERROR_SOURCE
#if defined(__CPLUSPLUS__) || defined(__cplusplus)
  };
#else
  }
  RCS_PRINT_ERROR_SOURCE_TYPE;
#endif

#ifdef I_DEFINED_ATTRIBUTE
#ifdef __attribute__
#undef __attribute__
#endif
#undef I_DEFINED_ATTRIBUTE
#endif

/* RCS_PRNT_HH */
#endif



