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

/******************************************************************
 * File: rtlnml.h
 *
 * Introduction:
 * This file creates an interface to NML that can be used by both
 * real-time and non-realtime C  linux applications. It will be also used
 * by the C++ wrapper that makes these messages available to any NML
 * application.
 *
 * Modifications:
31-May-2001 WPS modified to work with rtai as well.
 16-Mar-2000 WPS created.
********************************************************************/

#ifndef RTLNML_H
#define RTLNML_H

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef RTLNML_TYPEDEFED
  typedef void *rtlnml_t;	/* This will be redefined in rtlnml.c but is opaque to
				   the rest of the world. */
  typedef rtlnml_t rtainml_t;
#endif

  /* All applications should call this before any other rtlnml functions. */
  /* In rtlinux, call this only from init_module, never from an RT task
     function. */
  extern void rtlnml_init (void);

  /* This fuction is used to create each channel. */
  /* In rtlinux, call this only from init_module, never from an RT task
     function. */
  extern rtlnml_t rtlnml_open (const char *name,	/* buffer name */
			       const char *processname,	/* process name (not used) */
			       const char *filename,	/* configuration file (not used) */
			       unsigned long size,	/* Maximum size of message for buffer, (actual buffer is slightly larger). */
			       int master, int key);

  /* This fuction is used to create each channel. */
  /* In rtai, call this only from init_module, never from an RT task
     function. 
     This function is identical to rtlnml_open() except that a
     integer key has been added.  The name is not used as a key.
   */
  extern rtainml_t rtainml_open (const char *name,	/* buffer name */
				 const char *processname,	/* process name (not used) */
				 const char *filename,	/* configuration file (not used) */
				 unsigned long size,	/* Maximum size of message for buffer, (actual buffer is slightly larger). */
				 int master, int key);


  /* These functions get and release
     a direct pointer to the shared memory area.
     If a read occurs while the
     writer has direct access to the shared memory a split-buffer error will
     occur then either  rtlnml_get_direct_pointer_for_read,
     rtlnml_release_direct_pointer_for_read or rtlnml_read will return
     RTLNML_SPLIT_READ
     So the pointer should be released as soon as possible.
     Returns:
     See  RTLNML_SPECIAL_READ_RETURN_VALUES.
   */
  extern int rtlnml_get_direct_pointer_for_read (rtlnml_t, void **);
  extern int rtlnml_release_direct_pointer_for_read (rtlnml_t, void **);

  extern int rtlnml_get_direct_pointer_for_write (rtlnml_t, void **);
  extern int rtlnml_release_direct_pointer_for_write (rtlnml_t, void **,
						      long msg_type,
						      unsigned long msg_size);

  /* Reads from the buffer, possibly copying the contents to the location
     that can be obtained with rtlnml_get_local_pointer. This is
     safer and more flexible than using direct pointers but possibly
     slower.
     Returns:
     >0  -- NML message type of new NML message succesfully retrieved.
     otherwise -- See  RTLNML_SPECIAL_READ_RETURN_VALUES.
   */
  extern long rtlnml_read (rtlnml_t);


  /* Writes to the buffer, copying the contents from  the location given.
     This is safer and more flexible than using direct pointers but possibly
     slower.
     Return:
     See RTLNML_SPECIAL_WRITE_RETURN_VALUES.
   */
  extern long rtlnml_write (rtlnml_t, void *data,	/* address of data to be copied into nml buffer. */
			    long msg_type,	/* NML message type. */
			    unsigned long);	/* size of message */

  /* This function should be called after a successful rtlnml_read to
     get a pointer to a local copy of the message. */
  extern void *rtlnml_get_local_pointer (rtlnml_t);

  /* Use only if you want rtlnml_read to copy data to a special location.
     In rtlinux, this should only be done from within init_module never,
     never from within a RT task function.
   */
  extern void rtlnml_set_local_pointer (rtlnml_t, void *);

  /* Close each channel. */
  extern void rtlnml_close (rtlnml_t, const char *bufname);

  /* This function should be called after all other rtlnml functions. */
  /* In rtlinux call this only from cleanup_module, never from an RT task function. */
  extern void rtlnml_exit (void);

  enum RTLNML_SPECIAL_READ_RETURN_VALUES
  {
    RTLNML_READ_OLD = 0,
    RTLNML_READ_ERROR = -1,
    RTLNML_SPLIT_READ_ERROR = -2
  };

  enum RTLNML_SPECIAL_WRITE_RETURN_VALUES
  {
    RTLNML_WRITE_ERROR = -1,
    RTLNML_WRITE_OK = 0
  };


#ifdef __cplusplus
}
#endif

#endif				/* RTLNML_H */
