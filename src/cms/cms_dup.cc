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

/***************************************************************************
* File: cms_dup.cc
* Author(s): Will Shackleford, Fred Proctor
* Purpose: Provides the interface to CMS used by NML update functions including
* a CMS update function for all the basic C data types to convert NMLmsgs to XDR.
*
* NOTES:
* There are no update functions for enumerations, or pointers.
* The update function for long doubles is included, but it is recommended that
* doubles be used instead of long doubles since they will be faster and
* the increased range and precision of long doubles will be lost anyway.
****************************************************************************/

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_DISP)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "cms_dup_no_config.h"
#endif

#include "cms_dup.hh"		/* class CMS_DISPLAY_ASCII_UPDATER  */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "cms.hh"		// class CMS, CMS_HEADER
#include "dbg_mem.h"


#define DEFAULT_WARNING_COUNT_MAX 100

/* Member functions for CMS_DISPLAY_ASCII_UPDATER Class */
CMS_DISPLAY_ASCII_UPDATER::CMS_DISPLAY_ASCII_UPDATER (CMS * _cms_parent):
  CMS_UPDATER (_cms_parent),
  begin_current_string(0),
  end_current_string(0),
  max_length_current_string(0),
  length_current_string(0),
  encoding(0),
  warning_count(0),
  warning_count_max(DEFAULT_WARNING_COUNT_MAX),
  updating_string(0),
  comma_count(0)
{
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS,
		   "creating CMS_DISPLAY_ASCII_UPDATER\n");
  /* Set pointers to NULL. */
  begin_current_string = (char *) NULL;
  end_current_string = (char *) NULL;
  max_length_current_string = 0;
  updating_string = 0;

  /* Store and validate constructors arguments. */
  cms_parent = _cms_parent;
  if (NULL == cms_parent)
    {
      rcs_print_error ("CMS parent for updater is NULL.\n");
      return;
    }

  /* Allocate the encoded header too large, */
  /* and find out what size it really is. */
  encoded_header = DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_HEADER));
  if (encoded_header == NULL)
    {
      rcs_print_error ("CMS:can't malloc encoded_header");
      status = CMS_CREATE_ERROR;
      return;
    }

  /* If queuing is enabled, then initialize streams for */
  /* encoding and decoding the header with the queue information. */
  if (cms_parent->queuing_enabled)
    {
      /* Allocate the encoded header too large, */
      /* and find out what size it really is. */
      encoded_queuing_header =
	DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_QUEUING_HEADER));
    }
  using_external_encoded_data = false;
  warning_count = 0;
  warning_count_max = DEFAULT_WARNING_COUNT_MAX;
}

CMS_DISPLAY_ASCII_UPDATER::~CMS_DISPLAY_ASCII_UPDATER ()
{
  rcs_print_debug (PRINT_CMS_DESTRUCTORS,
		   "deleting CMS_DISPLAY_ASCII_UPDATER\n");

  if (NULL != encoded_data && !using_external_encoded_data)
    {
      DEBUG_FREE (encoded_data);
      encoded_data = NULL;
    }
  if (NULL != encoded_header)
    {
      DEBUG_FREE (encoded_header);
      encoded_header = NULL;
    }
  if (NULL != encoded_queuing_header)
    {
      DEBUG_FREE (encoded_queuing_header);
      encoded_queuing_header = NULL;
    }
}

void
CMS_DISPLAY_ASCII_UPDATER::find_next_comma ()
{
  while (*end_current_string != ',' && *end_current_string)
    {
      if (length_current_string >= max_length_current_string)
	{
	  rcs_print_error ("Maximum string length exceeded.{comma_count=%d}\n",
			   comma_count);
	  status = CMS_UPDATE_ERROR;
	  return;
	}
      length_current_string++;
      end_current_string++;
    }
  if(*end_current_string)
    {
      end_current_string++;
    }
  length_current_string++;
  comma_count++;
}

int
CMS_DISPLAY_ASCII_UPDATER::set_mode (CMS_UPDATER_MODE _mode)
{
  mode = _mode;
  CMS_UPDATER::set_mode (_mode);
  switch (mode)
    {
    case CMS_NO_UPDATE:
      begin_current_string = end_current_string = (char *) NULL;
      max_length_current_string = 0;
      length_current_string = 0;
      break;

    case CMS_ENCODE_DATA:
      begin_current_string = end_current_string = (char *) encoded_data;
      max_length_current_string = neutral_size_factor * size;
      if (max_length_current_string > cms_parent->max_encoded_message_size)
	{
	  max_length_current_string = cms_parent->max_encoded_message_size;
	}
      length_current_string = 0;
      encoding = 1;
      break;

    case CMS_DECODE_DATA:
      begin_current_string = end_current_string = (char *) encoded_data;
      max_length_current_string = neutral_size_factor * size;
      if (max_length_current_string > cms_parent->max_encoded_message_size)
	{
	  max_length_current_string = cms_parent->max_encoded_message_size;
	}
      length_current_string = 0;
      encoding = 0;
      break;

    case CMS_ENCODE_HEADER:
      begin_current_string = end_current_string = (char *) encoded_header;
      max_length_current_string = neutral_size_factor * sizeof (CMS_HEADER);
      length_current_string = 0;
      encoding = 1;
      break;

    case CMS_DECODE_HEADER:
      begin_current_string = end_current_string = (char *) encoded_header;
      max_length_current_string = neutral_size_factor * sizeof (CMS_HEADER);
      length_current_string = 0;
      encoding = 0;
      break;

    case CMS_ENCODE_QUEUING_HEADER:
      begin_current_string = end_current_string =
	(char *) encoded_queuing_header;
      max_length_current_string =
	neutral_size_factor * sizeof (CMS_QUEUING_HEADER);
      length_current_string = 0;
      encoding = 1;
      break;

    case CMS_DECODE_QUEUING_HEADER:
      begin_current_string = end_current_string =
	(char *) encoded_queuing_header;
      max_length_current_string =
	neutral_size_factor * sizeof (CMS_QUEUING_HEADER);
      length_current_string = 0;
      encoding = 0;
      break;

    default:
      rcs_print_error ("CMS updater in invalid mode.\n");
      return (-1);
    }
  return (0);
}

int
CMS_DISPLAY_ASCII_UPDATER::check_pointer (char * _pointer,
					  long _bytes)
{
  if (NULL == cms_parent || NULL == begin_current_string
      || NULL == end_current_string)
    {
      rcs_print_error
	("CMS_DISPLAY_ASCII_UPDATER: Required pointer is NULL.\n");
      return (-1);
    }
  if (length_current_string + _bytes * 4 > max_length_current_string)
    {
      rcs_print_error
	("CMS_DISPLAY_ASCII_UPDATER: length of current string(%ld) + bytes to add of(%ld) exceeds maximum of %ld.\n",
	 length_current_string, _bytes * 4, max_length_current_string);
      return (-1);
    }
  return (cms_parent->check_pointer (_pointer, _bytes));
}

/* Repositions the data buffer to the very beginning */
void
CMS_DISPLAY_ASCII_UPDATER::rewind ()
{
  CMS_UPDATER::rewind ();
  comma_count=0;
  end_current_string = begin_current_string;
#if 0 
  // This is nice for testing to make sure there are no left overs but
  // potentially very expensive for performance.
  if (encoding)
    {
      memset (begin_current_string, 0, max_length_current_string);
    }
#endif

  length_current_string = 0;
  if (NULL != cms_parent)
    {
      cms_parent->format_size = 0;
    }
}


int
CMS_DISPLAY_ASCII_UPDATER::get_encoded_msg_size ()
{
  rcs_print_debug(PRINT_MISC,"CMS_DISPLAY_ASCII_UPDATER::get_encoded_msg_size() comma_count=%d, length_current_string=%ld\n",comma_count,length_current_string);
  return (length_current_string);
}

/* Char functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update_char (char &x)
{

  if (encoding)
    {
      if (x == ',')
	{
	  strcat (end_current_string, "\\c");
	  end_current_string += 2;
	  length_current_string += 2;
	}
      else if (x == '\\')
	{
	  strcat (end_current_string, "\\\\");
	  end_current_string += 2;
	  length_current_string += 2;
	}
      else if (x == '\n')
	{
	  strcat (end_current_string, "\\n");
	  end_current_string += 2;
	  length_current_string += 2;
	}
      else if (x == 0 && updating_string)
	{
	}
      else if (!isgraph (x))
	{
	  SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "\\%3.3d", x);
	  end_current_string += 4;
	  length_current_string += 4;
	}
      else
	{
	  end_current_string[0] = x;
	  end_current_string++;
	  length_current_string++;
	}
    }
  else
    {
      if (end_current_string[0] == ',' || end_current_string[0] == 0)
	{
	  x = 0;
	}
      else if (end_current_string[0] == '\\')
	{
	  if (end_current_string[1] == 'c')
	    {
	      x = ',';
	      end_current_string += 2;
	      length_current_string += 2;
	    }
	  else if (end_current_string[1] == '\\')
	    {
	      x = end_current_string[1];
	      length_current_string += 2;
	      end_current_string += 2;
	    }
	  else if (end_current_string[1] == 'n')
	    {
	      x = '\n';
	      length_current_string += 2;
	      end_current_string += 2;
	    }
	  else
	    {
	      char
		temp[4];
	      memset(temp,0,4);
	      const char * const end = end_current_string+4;
	      char *tptr=temp;
	      end_current_string++;
	      length_current_string++;
	      while(end_current_string < end && 
		    *end_current_string && *end_current_string != ',')
		{
		  *tptr = *end_current_string;
		  tptr++;
		  end_current_string++;
		  length_current_string++;
		}
	      temp[3] = 0;
	      x = (char) strtol (temp, (char **) NULL, 10);
	    }
	}
      else
	{
	  x = end_current_string[0];
	  end_current_string += 1;
	  length_current_string++;
	}
    }
  return status;
}

CMS_STATUS 
CMS_DISPLAY_ASCII_UPDATER::update (bool &x)
{
  int orig_pointer_check_disabled = pointer_check_disabled;
  pointer_check_disabled = 1;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  char c = (x != 0);
  retval = update(c);
  x = (c != 0);
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS 
CMS_DISPLAY_ASCII_UPDATER::update (bool *x, unsigned int len)
{
  for(unsigned int i=0; i < len; i++)
    {
      update(x[i]);
    }
  return(status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (char)))
    {
      return (CMS_UPDATE_ERROR);
    }

  update_char (x);
  if(encoding)
    {
      end_current_string[0] = ',';
    }
  find_next_comma ();
  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (char) * len))
    {
      return (CMS_UPDATE_ERROR);
    }
  unsigned int
    i;
  updating_string = 1;

  for (i = 0; i < len; i++)
    {
      update_char (x[i]);
      if (x[i] == 0)
	{
	  break;
	}
    }
  if(encoding)
    {
      end_current_string[0] = ',';
    }
  find_next_comma ();
  updating_string = 0;
  return (status);
}

/* Char functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (unsigned char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned char)))
    {
      return (CMS_UPDATE_ERROR);
    }
  char
    cx;
  cx = (char) x;
  update_char (cx);
  x = (unsigned char) x;
  if(encoding)
    {
      end_current_string[0] = ',';
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS
  CMS_DISPLAY_ASCII_UPDATER::update (unsigned char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned char) * len))
    {
      return (CMS_UPDATE_ERROR);
    }
  char cx;
  unsigned int i;

  for (i = 0; i < len; i++)
    {
      cx = (char) x[i];
      update_char (cx);
      x[i] = (unsigned char) cx;
    }
  if(encoding)
    {
      end_current_string[0] = ',';
    }
  find_next_comma ();
  return (status);
}

/* Short functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (short &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%+d,", x);
    }
  else
    {
      errno = 0;
      long
	number =
	strtol (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtol of(%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      if ((number < SHRT_MIN || SHRT_MAX < number)
	  && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: Number %ld out of range for short(%d,%d)_comma_count=%d}\n",
	     number, SHRT_MIN, SHRT_MAX,comma_count);
	  rcs_print_error("%s\n",begin_current_string);
	  rcs_print_error("%s\n",end_current_string);
	  
	}
      x = (short) number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (short *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (short) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (unsigned short &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%u,", x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      unsigned long
	number =
	strtoul (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtoul of (%s).{comma_count=%d}\n",
	     errno, strerror (errno), end_current_string,comma_count);
	  return (status = CMS_UPDATE_ERROR);
	}
      if (number > USHRT_MAX && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: Number %ld out of range for unsigned short(0,%d) {comma_count=%d}\n",
	     number, USHRT_MAX,comma_count);
	  rcs_print_error("%s\n",begin_current_string);
	  rcs_print_error("end_current_string=%s\n",end_current_string);
	}
      x = (unsigned short) number;
    }
  find_next_comma ();


  return (status);
}


CMS_STATUS
  CMS_DISPLAY_ASCII_UPDATER::update (unsigned short *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 ==
      check_pointer ((char *) x, sizeof (unsigned short) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

/* Int  functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (int)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
#if 0
      if (x > 9999999 && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: int %d is too large. (Use type long.)\n",
	     x);
	}
#endif
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%+6d,", x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      long
	number =
	strtol (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d:%s occured during strtol of (%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      if (((number < ((long) INT_MIN)) || (((long) INT_MAX) < number))
	  && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: Number %ld out of range for int(%d,%d)\n",
	     number, INT_MIN, INT_MAX);
	}
      x = (int) number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (int *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (int) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (unsigned int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned int)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
#if 0
      if (x > 9999999 && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: unsigned int %u is too large. (Use type long.)\n",
	     x);
	}
#endif
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%6u,", x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      unsigned long
	number =
	strtoul (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d:%s {comma_count=%d} occured during strtoul of (%s).\n",
	     errno, strerror (errno), comma_count, end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      if (UINT_MAX < number && warning_count < warning_count_max)
	{
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: Number %lu out of range for unsigned int (0,%u){comma_count=%d}\n",
	     number, UINT_MAX,comma_count);
	}
      x = (unsigned int) number;
    }
  find_next_comma ();


  return (status);
}

CMS_STATUS
  CMS_DISPLAY_ASCII_UPDATER::update (unsigned int *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned int) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

/* Long functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (long &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (long)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      end_current_string[15] = 0;
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%+ld,", x);
      if (end_current_string[15] != 0 && warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER: long with value %ld caused an overflow. {comma_count=%d, end_current_string=%s}\n",
	     x,comma_count,end_current_string);
	  if(x > 0)
	    {
	      strcpy(end_current_string,"9999999999999,");
	    }
	  else
	    {
	      strcpy(end_current_string,"-999999999999,");
	    }
	  rcs_print_warning
	    ("CMS_DISPLAY_ASCII_UPDATER:end_current_string set to %s\n",end_current_string);
	}
      end_current_string[15] = 0;
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      long
	number =
	strtol (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtol of(%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      x = number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (long) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (unsigned long &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned long)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%lu,", x);

    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      unsigned long
	number =
	strtoul (end_current_string, (char **) NULL, 10);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d:%s occured during strtoul of(%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      x = number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS
  CMS_DISPLAY_ASCII_UPDATER::update (unsigned long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (unsigned long) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

/* Float functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (float &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (float)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%f,", x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      double
	number =
	strtod (end_current_string, (char **) NULL);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtol of (%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
#ifdef FLT_MAX
      if ((number < -FLT_MAX || FLT_MAX < number) &&
	  warning_count < warning_count_max)
	{
	  warning_count++;
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: (warning) Number %f out of range for float(%f,%f)\n",
	     number, -FLT_MAX, FLT_MAX);
	}
#endif
      x = (float) number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (float *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (float) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

/* Double functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (double &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%f,", x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      double
	number =
	strtod (end_current_string, (char **) NULL);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtol of (%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      x = number;
    }
  find_next_comma ();


  return (status);
}

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (double *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (double) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

/* long double functions */

CMS_STATUS CMS_DISPLAY_ASCII_UPDATER::update (long double &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (long double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (encoding)
    {
      end_current_string[15] = 0;
      SNPRINTF_FUNC ( SNPRINTF_ARGS(end_current_string,(max_length_current_string-length_current_string)), "%-14.8e,", (double) x);
    }
  else
    {
      if (0 == end_current_string[0])
	{
	  x = 0;
	  return status;
	}
      errno = 0;
      double
	number =
	strtod (end_current_string, (char **) NULL);
      if (errno != 0)
	{
	  rcs_print_error
	    ("CMS_DISPLAY_ASCII_UPDATER: Error %d: %s occured during strtod of (%s).\n",
	     errno, strerror (errno), end_current_string);
	  return (status = CMS_UPDATE_ERROR);
	}
      x = (long double) number;
    }
  find_next_comma ();

  return (status);
}

CMS_STATUS
  CMS_DISPLAY_ASCII_UPDATER::update (long double *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, sizeof (long double) * len))
    {
      return (CMS_UPDATE_ERROR);
    }

  for (unsigned int i = 0; i < len; i++)
    {
      if (CMS_UPDATE_ERROR == update (x[i]))
	{
	  return (CMS_UPDATE_ERROR);
	}
    }
  return (status);
}

//  defined(ENABLE_RCS_DISP)

#else
#include "rcs_empty_source"
#endif
