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

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if HAVE_CONFIG_H || ( ( !defined(NO_RTL) || defined(rtai) || defined(linux_rtai) ) && defined(linux) )

struct RTLNML_HEADER
{
  long message_type;
  unsigned long message_size;
  long first_message_count;
  long second_message_count;
};

struct RTLNML
{
  struct RTLNML_HEADER *header;
  void *mbuff_address;
  void *direct_data_pointer;
  void *local_pointer;
  int rtlnml_allocated_local_pointer;
  unsigned long max_message_size;
  int last_id_read;
  int key;
  unsigned long size;
};

typedef struct RTLNML *rtlnml_t;
typedef rtlnml_t rtainml_t;
#define RTLNML_TYPEDEFED 1

#include "rtl_rtai_common_api.h"
#include "rtlnml.h"

#include <string.h>		/* memcpy() */


/* All applications should call this before any other rtlnml functions. */
/* In rtlinux, call this only from init_module, never from an RT task
   function. */
void
rtlnml_init (void)
{
}

/* This fuction is used to create each channel. */
/* In rtlinux, call this only from init_module, never from an RT task
   function. */


rtlnml_t
rtlnml_open (const char *name,	/* buffer name */
	     const char *processname,	/* process name (not used) */
	     const char *filename,	/* configuration file (not used) */
	     unsigned long size,	/* Maximum size of message for buffer, (actual buffer is slightly larger). */
	     int master, int key)
{
  return rtainml_open (name, processname, filename, size, master, key);
}

rtainml_t
rtainml_open (const char *name,	/* buffer name */
	      const char *processname,	/* process name (not used) */
	      const char *filename,	/* configuration file (not used) */
	      unsigned long size,	/* Maximum size of message for buffer, (actual buffer is slightly larger). */
	      int master, int key)
{
  int i;
  struct RTLNML *r = 0;

  r = (struct RTLNML *) rtlrtai_malloc (sizeof (struct RTLNML));
  if (0 == r)
    {
      return 0;
    }
  r->header = 0;
  r->mbuff_address = 0;
  r->direct_data_pointer = 0;
  r->local_pointer = 0;
  r->rtlnml_allocated_local_pointer = 0;
  r->max_message_size = 0;
  r->last_id_read = 0;
  r->size = size;

  r->mbuff_address = rtlrtai_shm_alloc (name, key, size + 16);
  r->key = key;

  if (0 == r->mbuff_address)
    {
      rtlrtai_free (r);
      return 0;
    }

  if (master)
    {
      for (i = 0; i < size + 16; i++)
	{
	  ((char *) (r->mbuff_address))[i] = 0;
	}
    }
  r->local_pointer = rtlrtai_malloc (size);
  r->rtlnml_allocated_local_pointer = 1;
  r->direct_data_pointer = (void *) (((char *) r->mbuff_address) + 16);
  r->header = (struct RTLNML_HEADER *) r->mbuff_address;
  r->max_message_size = size;
  return r;
}


/* These functions get and release
   a direct pointer to the shared memory area.
   If a read occurs while the
   writer has direct access to the shared memory a split-buffer error will
   occur when either  rtlnml_get_direct_pointer_for_read,
   rtlnml_release_direct_pointer_for_read or rtlnml_read will return
   RTLNML_SPLIT_READ
   So the pointer should be released as soon as possible.*/
int
rtlnml_get_direct_pointer_for_read (rtlnml_t r, void **p)
{
  if (r->last_id_read == r->header->first_message_count)
    {
      return RTLNML_READ_OLD;
    }
  if (r->header->first_message_count != r->header->second_message_count)
    {
      return RTLNML_SPLIT_READ_ERROR;
    }
  r->last_id_read = r->header->first_message_count;
  *p = r->direct_data_pointer;
  return r->header->message_type;
}

int
rtlnml_release_direct_pointer_for_read (rtlnml_t r, void **p)
{
  *p = 0;
  if (r->header->second_message_count != r->last_id_read)
    {
      return RTLNML_SPLIT_READ_ERROR;
    }
  return r->header->message_type;
}

int
rtlnml_get_direct_pointer_for_write (rtlnml_t r, void **p)
{
  r->header->first_message_count++;
  *p = r->direct_data_pointer;
  return (0);
}

int
rtlnml_release_direct_pointer_for_write (rtlnml_t r, void **p, long msg_type,
					 unsigned long msg_size)
{
  *p = 0;
  r->header->message_type = msg_type;
  r->header->message_size = msg_size;
  r->header->second_message_count = r->header->first_message_count;
  if (msg_size <= r->max_message_size && msg_size > 0)
    {
      return (-1);
    }
  return (0);
}

  /* Reads from the buffer, possibly copying the contents to the location
     that can be obtained with rtlnml_get_local_pointer. This is
     safer and more flexible than using direct pointers but possibly
     slower. */
long
rtlnml_read (rtlnml_t r)
{
  long orig_last_id_read = r->last_id_read;
  long msg_type;
  if (r->last_id_read == r->header->first_message_count)
    {
      return RTLNML_READ_OLD;
    }
  if (r->header->first_message_count != r->header->second_message_count)
    {
      return RTLNML_SPLIT_READ_ERROR;
    }
  if (r->header->message_size <= 0
      || r->header->message_size > r->max_message_size)
    {
      return RTLNML_READ_ERROR;
    }
  r->last_id_read = r->header->first_message_count;
  msg_type = r->header->message_type;
#ifdef USE_BCOPY
  bcopy (r->direct_data_pointer, r->local_pointer, r->header->message_size);
#else
  memcpy (r->local_pointer, r->direct_data_pointer, r->header->message_size);
#endif /* ] */
  if (r->last_id_read != r->header->second_message_count)
    {
      r->last_id_read = orig_last_id_read;
      return RTLNML_SPLIT_READ_ERROR;
    }
  return msg_type;
}

  /* Writes to the buffer. This may be
     safer and more flexible than using direct pointers but possibly
     slower. */
long
rtlnml_write (rtlnml_t r, void *data,	/* address of data to be copied into nml buffer. */
	      long msg_type,	/* NML message type. */
	      unsigned long msg_size)	/* size of message */
{
  if (msg_size <= 0 || msg_size > r->max_message_size)
    {
      return RTLNML_WRITE_ERROR;
    }
  r->header->first_message_count++;
#ifdef USE_BCOPY
  bcopy (data, r->direct_data_pointer, msg_size);
#else
  memcpy (r->direct_data_pointer, data, msg_size);
#endif
  r->header->message_size = msg_size;
  r->header->message_type = msg_type;
  r->header->second_message_count = r->header->first_message_count;
  return (RTLNML_WRITE_OK);
}

  /* This function should be called after a successful rtlnml_read to
     get a pointer to a local copy of the message. */
void *
rtlnml_get_local_pointer (rtlnml_t r)
{
  return r->local_pointer;
}

/* Use only if you want rtlnml_read to copy data to a special location.*/
extern void
rtlnml_set_local_pointer (rtlnml_t r, void *p)
{
  if (0 != r->local_pointer && r->rtlnml_allocated_local_pointer)
    {
      rtlrtai_free (r->local_pointer);
    }
  r->rtlnml_allocated_local_pointer = 0;
  r->local_pointer = p;
}

  /* Close each channel. */
void
rtlnml_close (rtlnml_t r, const char *bufname)
{
  if (0 != r)
    {
      if (0 != r->mbuff_address)
	{
	  rtlrtai_shm_free (bufname, r->key, r->size + 16, r->mbuff_address);
	  r->mbuff_address = 0;
	}
      if (0 != r->local_pointer && r->rtlnml_allocated_local_pointer)
	{
	  rtlrtai_free (r->local_pointer);
	  r->local_pointer = 0;
	}
      rtlrtai_free (r);
    }
}

/* This function should be called after all other rtlnml functions. */
/* In rtlinux call this only from cleanup_module, never from an RT task function. */
void
rtlnml_exit ()
{
}

#endif
