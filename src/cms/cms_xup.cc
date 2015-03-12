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
* File: cms_xup.cc
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

/* Includes */
#define XDR_SOURCE 1

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_XDR)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "cms_xup_no_config.h"
#endif
// HAVE_CONFIG_H

#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */
#include "cms_xup.hh"		/* class CMS_XDR_UPDATER  */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "extxdrintf.h"		// ext_xdr_destroy
#include "cms.hh"		// class CMS

/* Member functions for CMS_XDR_UPDATER Class */
CMS_XDR_UPDATER::CMS_XDR_UPDATER (CMS * _cms_parent):
  CMS_UPDATER (_cms_parent,0, 2),
  encode_data_stream(0),
  decode_data_stream(0),
  encode_header_stream(0),
  decode_header_stream(0),
  encode_queuing_header_stream(0),
  decode_queuing_header_stream(0),
  current_stream(0),
  xdrmem_size(0)
{
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS, "creating CMS_XDR_UPDATER: cms_parent=%p,isserver=%d,neutral=%d,size=%ld,neutral_size_factor=%ld,enc_max_size=%ld\n",
		   ((void *)cms_parent),
		   (cms_parent?cms_parent->isserver:-99),
		   (cms_parent?cms_parent->neutral:-99),
		   size,
		   neutral_size_factor,
		   (cms_parent?cms_parent->enc_max_size:-99)		   
		   );
  /* Set pointers to NULL. */
  encode_data_stream = (XDR *) NULL;
  decode_data_stream = (XDR *) NULL;
  encode_header_stream = (XDR *) NULL;
  decode_header_stream = (XDR *) NULL;
  encode_queuing_header_stream = (XDR *) NULL;
  decode_queuing_header_stream = (XDR *) NULL;
  encoded_header = NULL;
  encoded_queuing_header = NULL;

  if (!cms_parent->isserver)
    {
      encoded_data = NULL;
    }
  using_external_encoded_data = false;

#if 0
  if (load_rpc_interface () < 0)
    {
      rcs_print_error
	("CMS_XDR_UPDATER requires the rpc interface and it failed to load.\n");
      status = CMS_UPDATE_ERROR;
      return;
    }
#endif
  /* Store and validate constructors arguments. */
  cms_parent = _cms_parent;
  if (NULL == cms_parent)
    {
      rcs_print_error ("CMS parent for updater is NULL.\n");
      status = CMS_UPDATE_ERROR;
      return;
    }

  /* Allocate memory for: */
  /*  - encoded copies of the global buffer and the header */
  /*  - XDR streams. */
  /*  (Also initialize the XDR streams.) */

  /* Allocate the encoded header too large, */
  /* and find out what size it really is. */
  if(encoded_header)
    {
      DEBUG_FREE(encoded_header);
      encoded_header=0;
    }
  encoded_header = DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_HEADER));
  if (encoded_header == NULL)
    {
      rcs_print_error ("CMS:can't malloc encoded_header");
      status = CMS_CREATE_ERROR;
      return;
    }

  encode_header_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
  if (encode_header_stream == NULL)
    {
      cms_parent->status = CMS_CREATE_ERROR;
      rcs_print_error ("CMS:can't malloc encode_header_stream");
      return;
    }
  xdrmem_create ((XDR *) encode_header_stream, (char *) encoded_header,
		 (int) neutral_size_factor * sizeof (CMS_HEADER), XDR_ENCODE);

  decode_header_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
  if (decode_header_stream == NULL)
    {
      rcs_print_error ("CMS:can't malloc decode_header_stream");
      status = CMS_CREATE_ERROR;
      return;
    }
  xdrmem_create ((XDR *) decode_header_stream, (char *) encoded_header,
		 (int) neutral_size_factor * sizeof (CMS_HEADER), XDR_DECODE);

  /* If queuing is enabled, then initialize streams for */
  /* encoding and decoding the header with the queue information. */
  if (cms_parent->queuing_enabled)
    {
      /* Allocate the encoded header too large, */
      /* and find out what size it really is. */
      encoded_queuing_header =
	DEBUG_MALLOC (neutral_size_factor * sizeof (CMS_QUEUING_HEADER));
      if (encoded_queuing_header == NULL)
	{
	  rcs_print_error ("CMS:can't malloc encoded_queuing_header");
	  status = CMS_CREATE_ERROR;
	  return;
	}
      encode_queuing_header_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
      if (encode_queuing_header_stream == NULL)
	{
	  status = CMS_CREATE_ERROR;
	  rcs_print_error ("CMS:can't malloc encode_queuing_header_stream");
	  return;
	}
      xdrmem_create ((XDR *) encode_queuing_header_stream,
		     (char *) encoded_queuing_header,
		     (int) neutral_size_factor * sizeof (CMS_QUEUING_HEADER),
		     XDR_ENCODE);

      decode_queuing_header_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
      if (decode_queuing_header_stream == NULL)
	{
	  rcs_print_error ("CMS:can't malloc decode_queuing_header_stream");
	  status = CMS_CREATE_ERROR;
	  return;
	}
      xdrmem_create ((XDR *) decode_queuing_header_stream,
		     (char *) encoded_queuing_header,
		     (int) neutral_size_factor * sizeof (CMS_QUEUING_HEADER),
		     XDR_DECODE);
    }
  if (!cms_parent->isserver)
    {
      if (cms_parent->enc_max_size > 0
	  && cms_parent->enc_max_size < neutral_size_factor * size)
	{
	  set_encoded_data (DEBUG_MALLOC (cms_parent->enc_max_size),
			    cms_parent->enc_max_size);
	}
      else
	{
	  set_encoded_data (DEBUG_MALLOC (neutral_size_factor * size),
			    neutral_size_factor * size);
	}
    }
  using_external_encoded_data = false;
}

CMS_XDR_UPDATER::~CMS_XDR_UPDATER ()
{
  rcs_print_debug (PRINT_CMS_DESTRUCTORS, "deleting CMS_XDR_UPDATER\n");

  /* If encoded buffers were used destroy the xdr streams and free */
  /* memory used for encoded buffers. */
  if (NULL != encode_data_stream)
    {
      ext_xdr_destroy (((XDR *)encode_data_stream));
      DEBUG_FREE (((XDR *)encode_data_stream));
      encode_data_stream = (XDR *) NULL;
    }
  if (NULL != decode_data_stream)
    {
      ext_xdr_destroy (((XDR *)decode_data_stream));
      DEBUG_FREE (((XDR *)decode_data_stream));
      decode_data_stream = (XDR *) NULL;
    }
  if (NULL != encode_header_stream)
    {
      ext_xdr_destroy (((XDR *) encode_header_stream));
      DEBUG_FREE (encode_header_stream);
      encode_header_stream = (XDR *) NULL;
    }
  if (NULL != decode_header_stream)
    {
      ext_xdr_destroy (((XDR *) decode_header_stream));
      DEBUG_FREE (decode_header_stream);
      decode_header_stream = (XDR *) NULL;
    }
  if (NULL != encode_queuing_header_stream)
    {
      ext_xdr_destroy (((XDR *) encode_queuing_header_stream));
      DEBUG_FREE (encode_queuing_header_stream);
      encode_queuing_header_stream = (XDR *) NULL;
    }
  if (NULL != decode_queuing_header_stream)
    {
      ext_xdr_destroy (((XDR *) decode_queuing_header_stream));
      DEBUG_FREE (decode_queuing_header_stream);
      decode_queuing_header_stream = (XDR *) NULL;
    }
  //unload_rpc_interface ();

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
CMS_XDR_UPDATER::set_encoded_data (void *_encoded_data,
				   long _encoded_data_size)
{
  /* If the encoded data area has already been setup then release it. */
  if (NULL != encoded_data && !using_external_encoded_data)
    {
      DEBUG_FREE (encoded_data);
      encoded_data = NULL;
    }

  encoded_data_size = _encoded_data_size;
  encoded_data = _encoded_data;
  using_external_encoded_data = true;
  if (encoded_data == NULL)
    {
      rcs_print_error ("CMS: Attempt to set  encoded_data buffer to NULL.\n");
      status = CMS_MISC_ERROR;
      return;
    }
  /* Allocate memory for XDR structures. */
  if (NULL == encode_data_stream)
    {
      encode_data_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
    }
  else
    {
      ext_xdr_destroy (((XDR *)encode_data_stream));
    }
  if (NULL == encode_data_stream)
    {
      rcs_print_error ("CMS:can't malloc encode_data_stream");
      status = CMS_CREATE_ERROR;
      return;
    }
  if (NULL == decode_data_stream)
    {
      decode_data_stream = (XDR *) DEBUG_MALLOC (sizeof (XDR));
    }
  else
    {
      ext_xdr_destroy (((XDR *)decode_data_stream));
    }
  if (NULL == decode_data_stream)
    {
      rcs_print_error ("CMS:can't malloc decode_data_stream");
      status = CMS_CREATE_ERROR;
      return;
    }

  /* Initialize the XDR streams. */
  xdrmem_size = (int) (neutral_size_factor * size);
  if (xdrmem_size > cms_parent->max_encoded_message_size
      && cms_parent->max_encoded_message_size > 0)
    {
      xdrmem_size = cms_parent->max_encoded_message_size;
    }
  if (xdrmem_size > cms_parent->enc_max_size && cms_parent->enc_max_size > 0)
    {
      xdrmem_size = cms_parent->enc_max_size;
    }
  xdrmem_create (((XDR *)encode_data_stream), (char *) encoded_data, 
		 xdrmem_size,
		 XDR_ENCODE);
  xdrmem_create (((XDR *)decode_data_stream), (char *) encoded_data, 
		 xdrmem_size,
		 XDR_DECODE);

}

int
CMS_XDR_UPDATER::set_mode (CMS_UPDATER_MODE _mode)
{
  mode = _mode;
  CMS_UPDATER::set_mode (_mode);
  switch (mode)
    {
    case CMS_NO_UPDATE:
      current_stream = (XDR *) NULL;
      break;

    case CMS_ENCODE_DATA:
      current_stream = encode_data_stream;
      break;

    case CMS_DECODE_DATA:
      current_stream = decode_data_stream;
      break;

    case CMS_ENCODE_HEADER:
      current_stream = encode_header_stream;
      break;

    case CMS_DECODE_HEADER:
      current_stream = decode_header_stream;
      break;

    case CMS_ENCODE_QUEUING_HEADER:
      current_stream = encode_queuing_header_stream;
      break;

    case CMS_DECODE_QUEUING_HEADER:
      current_stream = decode_queuing_header_stream;
      break;

    default:
      rcs_print_error ("CMS updater in invalid mode.(%d)\n", mode);
      return (-1);
    }
  return (0);
}

int
CMS_XDR_UPDATER::check_pointer (char * _pointer, long _bytes)
{
  if (NULL == cms_parent || NULL == current_stream)
    {
      rcs_print_error ("CMS_XDR_UPDATER: Required pointer is NULL.\n");
      return (-1);
    }
  if ((current_stream == encode_data_stream) ||
      (current_stream == decode_data_stream) ||
      (mode == CMS_ENCODE_DATA) || (mode == CMS_DECODE_DATA))
    {
      int xdr_pos = ext_xdr_getpos (((XDR *) current_stream));
      if (xdr_pos + _bytes > encoded_data_size)
	{
	  rcs_print_error
	    ("Encoded message buffer full. (xdr_pos=%d,_bytes=%ld,(xdr_pos+_bytes)=%ld,encoded_data_size=%ld)\n",
	     xdr_pos, _bytes, (xdr_pos + _bytes), encoded_data_size);
	  return -1;
	}
    }
  return (cms_parent->check_pointer (_pointer, _bytes));
}

/* Repositions the data buffer to the very beginning */
void
CMS_XDR_UPDATER::rewind ()
{
  CMS_UPDATER::rewind ();
  if (NULL != current_stream)
    {
      ext_xdr_setpos (((XDR *) current_stream), 0);
    }
  else
    {
      rcs_print_error
	("CMS_XDR_UPDATER: Can't rewind because current_stream is NULL: mode=%d, isserver=%d,neutral=%d.\n",
	 mode,(cms_parent?cms_parent->isserver:-1), (cms_parent?cms_parent->neutral:-1));
    }
  if (NULL != cms_parent)
    {
      cms_parent->format_size = 0;
    }
}


int
CMS_XDR_UPDATER::get_encoded_msg_size ()
{
  if (NULL == current_stream)
    {
      rcs_print_error
	("CMS_XDR_UPDATER can not provide encoded_msg_size because the current stream is NULL.\n");
      return (-1);
    }
  return (ext_xdr_getpos (((XDR *) current_stream)));
}


/* Char functions */

CMS_STATUS CMS_XDR_UPDATER::update (char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (char)))
    {
      return (CMS_UPDATE_ERROR);
    }
  if (xdr_char (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_char failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, len * sizeof (char)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_bytes (((XDR *) current_stream), (char **) &x, &len, len) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_bytes failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (char)))
    {
      return (CMS_UPDATE_ERROR);
    }
  if (xdr_u_char (((XDR *) current_stream), (unsigned char *) &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_u_char failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS 
CMS_XDR_UPDATER::update(
#if  __GNUC__ >= 4
			unsigned char __attribute__((__may_alias__)) *x,
#else
			unsigned char  *x,
#endif
			unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, len * sizeof (unsigned char)))
    {
      return (CMS_UPDATE_ERROR);
    }
  if (xdr_bytes (((XDR *) current_stream), (char **) &x, &len, len) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_bytes failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

/* SHORT */

CMS_STATUS CMS_XDR_UPDATER::update (short &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_short (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_short failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (short *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) x, len * sizeof (short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_vector (((XDR *) current_stream), (char *) x, len, sizeof (short),
		  (xdrproc_t) xdr_short) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_short) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned short &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_u_short (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_u_short failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }

  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned short *x, unsigned int len)
{
  if (-1 ==
      check_pointer ((char *) x, len * sizeof (unsigned short)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_vector (((XDR *) current_stream),
		  (char *) x, len,
		  sizeof (unsigned short), (xdrproc_t) xdr_u_short) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_u_short) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

/* INT */

CMS_STATUS CMS_XDR_UPDATER::update (int &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (int)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_int (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_int failed. (get_pos()=%d,xdrmem_size=%d)\n",get_pos(),xdrmem_size);
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (int *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (int)))
    {
      return (CMS_UPDATE_ERROR);
    }
  if (xdr_vector (((XDR *) current_stream), (char *) x, len, sizeof (int),
		  (xdrproc_t) xdr_int) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_vector( ... xdr_int) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned int &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned int)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_u_int (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_u_int failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned int *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (unsigned int)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_vector (((XDR *) current_stream),
		  (char *) x, len,
		  sizeof (unsigned int), (xdrproc_t) xdr_u_int) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_u_int) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}


/* LOG */
#if (defined(LONG_MIN) && defined(LONG_MAX) && defined(INT_MIN) && defined(INT_MAX) && (LONG_MIN < INT_MIN || LONG_MAX > INT_MAX))   || (defined(ULONG_MAX) && defined(UINT_MAX) && (ULONG_MAX > UINT_MAX)  )
static int long_warning_count = 0;
#endif

CMS_STATUS CMS_XDR_UPDATER::update (long &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (long)))
    {
      return (CMS_UPDATE_ERROR);
    }
#if defined(LONG_MIN) && defined(LONG_MAX) && defined(INT_MIN) && defined(INT_MAX) && (LONG_MIN < INT_MIN || LONG_MAX > INT_MAX)  
  if(sizeof(long) > 4 && encoding && (x >  21474836487 ||  x < -2147483648) && long_warning_count < 10)
    {
      rcs_print_warning("Sending 64bit long %ld(0x%lX) over xdr which only uses 32bit integers.\n",x,x);
      long_warning_count++;
      if(long_warning_count >= 10)
	{
	  rcs_print_warning("Too many warnings about 64bit to 32bit conversion, additional incidents will not be reported.\n");
	}
    }
#endif

#if defined(__LP64__) && defined(__APPLE__)
  int tx = (int) x;  
  if (xdr_long (((XDR *) current_stream), &tx) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_long failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  x=(long) tx;
#else
  if (xdr_long (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_long failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
#endif

  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (long *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (long)))
    {
      return (CMS_UPDATE_ERROR);
    }

#if defined(LONG_MIN) && defined(LONG_MAX) && defined(INT_MIN) && defined(INT_MAX) && (LONG_MIN < INT_MIN || LONG_MAX > INT_MAX)  
  if(sizeof(long) > 4 && encoding  && long_warning_count < 10)
    {
      for(unsigned int i = 0; i < len; i++)
	{
	  if(x[i] >  21474836487 ||  x[i] < -2147483648)
	    {
	      rcs_print_warning("Sending 64bit long %ld(0x%lX) which only uses 32bit integers.\n",x[i],x[i]);
	      long_warning_count++;
	      if(long_warning_count >= 10)
		{
		  rcs_print_warning("Too many warnings about 64bit to 32bit conversion, additional incidents will not be reported.\n");
		}
	    }
	}
    }
#endif			

  if (xdr_vector (((XDR *) current_stream), (char *) x, len, sizeof (long),
		  (xdrproc_t) xdr_long) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_vector(... xdr_long) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned long &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned long)))
    {
      return (CMS_UPDATE_ERROR);
    }

#if defined(ULONG_MAX) && defined(UINT_MAX) && (ULONG_MAX > UINT_MAX)  
  if(sizeof(unsigned long) > 4 && encoding && (x >  4294967296) && long_warning_count < 10)
    {
      rcs_print_warning("Sending 64bit unsigned long %lu(0x%lX) over xdr which oonly uses 32bit unsigned integers.\n",x,x);
      long_warning_count++;
      if(long_warning_count >= 10)
	{
	  rcs_print_warning("Too many warnings about 64bit to 32bit conversion, additional incidents will not be reported.\n");
	}
    }
#endif

#if defined(__LP64__) && defined(__APPLE__)
  unsigned int tx = (unsigned int) x;  
  if (xdr_u_long (((XDR *) current_stream), &tx) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_long failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  x=(unsigned long) tx;
#else
  if (xdr_u_long (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_long failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
#endif
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (unsigned long *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (unsigned long)))
    {
      return (CMS_UPDATE_ERROR);
    }

#if defined(ULONG_MAX) && defined(UINT_MAX) && (ULONG_MAX > UINT_MAX)  
  if(sizeof(unsigned long) > 4 && encoding  && long_warning_count < 10)
    {
      for(unsigned int i = 0; i < len; i++)
	{
	  if(x[i] >  4294967296)
	    {
	      rcs_print_warning("Sending 64bit unsigned long %lu(0x%lX) over xdr which only uses 32bit unsigned integers.\n",x[i],x[i]);
	      long_warning_count++;
	      if(long_warning_count >= 10)
		{
		  rcs_print_warning("Too many warnings about 64bit to 32bit conversion, additional incidents will not be reported.\n");
		}
	    }
	}
    }
#endif

  if (xdr_vector (((XDR *) current_stream),
		  (char *) x, len, sizeof (unsigned long),
		  (xdrproc_t) xdr_u_long) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_u_long) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

/* FLOAT */

CMS_STATUS CMS_XDR_UPDATER::update (float &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (float)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_float (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_float failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (float *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (float)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_vector (((XDR *) current_stream), (char *) x, len, sizeof (float),
		  (xdrproc_t) xdr_float) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_float) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (double &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_double (((XDR *) current_stream), &x) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_double failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (double *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  if (xdr_vector (((XDR *) current_stream), (char *) x, len, sizeof (double),
		  (xdrproc_t) xdr_double) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_double) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }

  return (status);
}

/* NOTE: Because XDR does not include seperate facilities for long doubles. */
/* Some resolution will be lost if long doubles are passed through XDR. */
/* This routine is included only for the sake of completeness. */
/* Avoid using long doubles in NML messages. */
CMS_STATUS CMS_XDR_UPDATER::update (long double &x)
{
  if (-1 == check_pointer ((char *) &x, sizeof (long double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  double
    y;
#ifdef __BORLANDC__
  /* The Borland C++ compiler seems to need a little help in converting */
  /* long doubles to ordinary doubles. */
  if (x > 1E308)
    x = 1E308;
  if (x < -1E308)
    x = -1E308;
  if ((-2E-308 < x) && (x < 2E-308))
    x = 0.0;
#endif

  y = (double) x;


  if (xdr_double (((XDR *) current_stream), &y) != TRUE)
    {
      rcs_print_error ("CMS_XDR_UPDATER: xdr_double failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }
  x = (long double) y;
  return (status);
}

CMS_STATUS CMS_XDR_UPDATER::update (long double *x, unsigned int len)
{
  if (-1 == check_pointer ((char *) x, len * sizeof (long double)))
    {
      return (CMS_UPDATE_ERROR);
    }

  unsigned int
    i;
  double *
    y;
  y = (double *) DEBUG_MALLOC (sizeof (double) * len);
  for (i = 0; i < len; i++)
    {
#ifdef __BORLANDC__
      /* The Borland C++ compiler seems to need a little help in converting */
      /* long doubles to ordinary doubles. */
      if (x[i] > 1E308)
	x[i] = 1E308;
      if (x[i] < -1E308)
	x[i] = -1E308;
      if ((-2E-308 < x[i]) && (x[i] < 2E-308))
	x[i] = 0.0;
#endif
      y[i] = (double) x[i];
    }


  if (xdr_vector (((XDR *) current_stream), (char *) y, len, sizeof (double),
		  (xdrproc_t) xdr_double) != TRUE)
    {
      rcs_print_error
	("CMS_XDR_UPDATER: xdr_vector(... xdr_double) failed.\n");
      return (status = CMS_UPDATE_ERROR);
    }

  for (i = 0; i < len; i++)
    {
      x[i] = (long double) y[i];
    }
  DEBUG_FREE (y);
  return (status);
}


CMS_STATUS 
CMS_XDR_UPDATER::update (bool &x)
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
CMS_XDR_UPDATER::update (bool *x, unsigned int len)
{
  for(unsigned int i=0; i < len; i++)
    {
      update(x[i]);
    }
  return(status);
}


int
CMS_XDR_UPDATER::get_pos()
{
  if(!current_stream)
    {
      return -1;
    }
  return ((int) ext_xdr_getpos (((XDR *) current_stream)));
}  

//  defined(ENABLE_RCS_XDR)
#else
#include "rcs_empty_source"
#endif
