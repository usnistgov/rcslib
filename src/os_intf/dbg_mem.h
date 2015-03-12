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

#ifndef DBG_MEM_H
#define DBG_MEM_H

#include <stdlib.h>		/* size_t */

#ifdef __cplusplus


extern "C"
{
#endif

  void *debug_malloc (const char *file, int line, size_t);
  void *debug_calloc (const char *file, int line, size_t, size_t);
  void *debug_realloc (const char *file, int line, void *, size_t);
  void debug_free (const char *file, int line, void *);

  void disable_debug_memory (void);
  void enable_debug_memory (void);
  void clear_dbg_mem_list (void);
  void print_dbg_mem_list (void);

  extern int log_debug_mem_list;
  extern int print_debug_mem_calls;
  extern int next_log_debug_id;


#ifdef __cplusplus
}
#endif

#ifdef DEBUG_MEMORY

#define DEBUG_MALLOC(X) debug_malloc(__FILE__, __LINE__, X)
#define DEBUG_CALLOC(X,Y)  debug_calloc(__FILE__, __LINE__, X, Y)
#define DEBUG_REALLOC(X,Y) debug_realloc(__FILE__, __LINE__, X, Y)
#define DEBUG_FREE(X)  debug_free(__FILE__,__LINE__,X)

#ifdef __cplusplus

static inline void *operator
new (size_t size)
{
#ifdef __BASE_FILE__
  return debug_malloc (__BASE_FILE__, 0, size);
#else
  return debug_malloc ("new", 0, size);
#endif
};

static inline void operator
delete (void *ptr)
{
#ifdef __BASE_FILE__
  debug_free (__BASE_FILE__, 0, ptr);
#else
  debug_free ("delete", 0, ptr);
#endif
}

#endif /* __cplusplus */
#else

#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC malloc
#endif
#ifndef DEBUG_CALLOC
#define DEBUG_CALLOC calloc
#endif
#ifndef DEBUG_REALLOC
#define DEBUG_REALLOC realloc
#endif
#ifndef DEBUG_FREE
#define DEBUG_FREE free
#endif

#endif

#endif
