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

#ifndef NMLMACROS_H
#define NMLMACROS_H

#ifndef RCS_CONFIG_INCLUDE_H
#include <string.h>		/* strdup(),strlen() */
#include <stdlib.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef NMLTYPE_TYPEDEFED
#define NMLTYPE_TYPEDEFED
typedef long NMLTYPE;		/* Also defined in nml.hh */
#endif

  /* This is just a symbol passed to the RCS Java Tools (CodeGen, RCS-Design, RCS-Diagnostis) */
#ifndef NML_DYNAMIC_LENGTH_ARRAY
#define NML_DYNAMIC_LENGTH_ARRAY
#endif

#ifndef NML_UNBOUNDED_LENGTH_ARRAY
#define NML_UNBOUNDED_LENGTH_ARRAY
#endif

#ifndef DECLARE_NML_DYNAMIC_LENGTH_ARRAY
#define DECLARE_NML_DYNAMIC_LENGTH_ARRAY(type, name, size) int name##_length; type name[size];
#endif

#ifndef NO_NML_UNBOUNDED
#ifndef DECLARE_NML_UNBOUNDED_ARRAY
#define DECLARE_NML_UNBOUNDED_ARRAY(type, name) int name##_length; type *name; int name##_size_allocated;
#endif

#ifndef SET_NML_UNBOUNDED_STRING
#define SET_NML_UNBOUNDED_STRING(name, string)  name=strdup(string); name##_length = strlen(name)+1; name##_size_allocated=name##_length;
#endif

#ifndef INLINE_IT 
#ifdef __cplusplus 
#define INLINE_IT inline
#else 
/* ! __CPLUSPLUS__  */
#if (__STDC_VERSION__ >= 19990L) 
  /* check for C99 */
#define INLINE_IT inline
#else 
  /* ! C99  */
#ifdef __MSC_VER 
  /* Microsoft Visual C */
#define INLINE_IT __inline
#else 
  /* !Microsoft Visual C */
#ifdef __GNUC__
  /* GNU C */
#define INLINE_IT __inline__
#else
  /* ! GNU C */
#define INLINE_IT 
  /* #ifdef __GNUC__ */
#endif
  /* end of ifdef __MSC_VER */
#endif
  /* end of if (__STDC_VERSION__ >= 19990L) */
#endif
  /* #ifdef __cplusplus  */
#endif
  /* #ifndef INLINE_IT  */
#endif

#ifndef INLINE_IT
#error "Fix the macros above to make sure INLINE_IT is defined."
#endif

static INLINE_IT void
nml_unbounded_alloc(void **varP, int *lengthP, int *size_allocatedP, int new_len, size_t elsize)
{
  if(new_len < 1)
    {
      return;
    }
  if(!*varP)
    {                                                              
      *varP = malloc(new_len*elsize);
      memset(*varP,0,new_len*elsize);
      *lengthP = new_len;
      *size_allocatedP = new_len;
    }                                                              
  else
    {                                                              
      if(new_len > *size_allocatedP)
	{                                                          
	  *varP = realloc(*varP,new_len*elsize);
	  memset(((char*)(*varP))+*size_allocatedP*elsize,  
		 0,(new_len-*size_allocatedP)*elsize);
	  *size_allocatedP = new_len;
	}
      *lengthP = new_len;
    }
}

#ifndef NML_UNBOUNDED_ALLOC
#define NML_UNBOUNDED_ALLOC(var,len)	                           \
  nml_unbounded_alloc((void **)&var,&var##_length,&var##_size_allocated, len, sizeof(*var));


#endif

static INLINE_IT void
nml_unbounded_free(void **varP, int *lengthP, int *size_allocatedP)
{
  if(*varP)
    {
      free(*varP);
    }
  *varP= 0;
  *lengthP=0;
  *size_allocatedP=0;
}


#ifndef NML_UNBOUNDED_FREE
#define NML_UNBOUNDED_FREE(var)		     \
  nml_unbounded_free((void **)&var, &var##_length, &var##_size_allocated);

#endif


#else
#ifndef DECLARE_NML_UNBOUNDED_ARRAY
#define DECLARE_NML_UNBOUNDED_ARRAY(type, name) ;;;
#endif
#ifndef SET_NML_UNBOUNDED_STRING
#define SET_NML_UNBOUNDED_STRING(name, string) ;;;
#endif
#ifndef NML_UNBOUNDED_ALLOC
#define NML_UNBOUNDED_ALLOC(var,len) ;;;
#endif
#ifndef NML_UNBOUNDED_FREE
#define NML_UNBOUNDED_FREE(var,len) ;;;
#endif
#endif


#ifndef NML_ARRAY_LEN
#define NML_ARRAY_LEN(X) (sizeof(X)/sizeof(X[0]))
#endif

#ifndef NML_ARRAY_LENI
#define NML_ARRAY_LENI(X) ((int) (sizeof(X)/sizeof(X[0])))
#endif

#ifdef __cplusplus
}
#endif

/* NMLMACROS_H */
#endif
