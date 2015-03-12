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
* File: cms_up.cc
* Author(s): Will Shackleford, Fred Proctor
* Purpose: Provides the interface to CMS used by NML update functions including
* a CMS update function for all the basic C data types.
*
* NOTES:
* There are no update functions for enumerations, or pointers.
****************************************************************************/

/* Includes */

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif
#include <stdlib.h>		// malloc(), free()
#include <string.h>		// memset()
#ifdef EXTERN_C_STD_HEADERS
}
#endif
#endif

#include "cms.hh"		/* class CMS */
#include "cms_up.hh"		/* class CMS_UPDATER */
#include "cms_enum_info.h" 	/* struct cms_enum_info; */

#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "dbg_mem.h"		// DEBUG_MALLOC,DEBUG_FREE
#include "nmlmsg.hh"



int
  CMS_UPDATER::updater_count =
  0;

static bool print_format=false;
static bool print_format_env_checked=false;

/* Member functions for CMS_UPDATER Class */
CMS_UPDATER::CMS_UPDATER (CMS * _cms_parent, int create_encoded_data,
			  long _neutral_size_factor):
  /**********************************************
  * Aliases to variables in the CMS parent
  * using aliases lets CMS and its CMS_UPDATER share this information
  * more conveniently and allowed the CMS_UPDATER functions to be pulled out
  * of CMS with fewer changes. (WPS - 6/12/95)
  *********************************************/
cms_parent(_cms_parent),
encoded_data (_cms_parent->encoded_data),
encoded_header (_cms_parent->encoded_header),
encoded_queuing_header (_cms_parent->encoded_queuing_header),
status (_cms_parent->status),
size (_cms_parent->size),
encoded_header_size (_cms_parent->encoded_header_size),
encoded_queuing_header_size (_cms_parent->encoded_queuing_header_size),
using_external_encoded_data (_cms_parent->using_external_encoded_data),
pointer_check_disabled (_cms_parent->pointer_check_disabled),
encoded_data_size (_cms_parent->encoded_data_size),
no_unbounded (_cms_parent->no_unbounded),
unbounded_used (_cms_parent->unbounded_used),
add_array_indexes_to_name(_cms_parent->add_array_indexes_to_name),
xml_style_properties(_cms_parent->xml_style_properties),
global_xml_style_properties_count(_cms_parent->global_xml_style_properties_count),
check_type_changed(true),
last_check_type_info_type(0),
last_check_type_info_buffer(0),
last_check_type_info_nsname(0),
last_check_type_info_symbol_lookup_function(0),
last_check_type_info_namelist(0),
last_check_type_info_idlist(0),
last_check_type_info_sizelist(0),
last_check_type_info_list_length(0),
last_check_type_info_max_name_length(0),
uba_list(0),
uba_list_length(0),
uba_list_size_allocated(0),
check_type_info_called(false),
fail_on_overflow(_cms_parent?_cms_parent->fail_on_overflow:false),
my_xml_style_properties_count(0),
neutral_size_factor(_neutral_size_factor),
encoding(0),
mode(CMS_NO_UPDATE),
next_default(0)
{
  if(0 == _cms_parent)
    {
      rcs_print_error("CMS_UPDATER:: _cms_parent is NULL\n");
      return;
    }
  if(!print_format_env_checked)
    {
      const char *print_format_env = getenv("CMS_PRINT_FORMAT");
      if(print_format_env)
	{
	  print_format=true;
	}
      print_format_env_checked=true;
    }

  rcs_print_debug (PRINT_CMS_CONSTRUCTORS, "constructing CMS_UPDATER\n");
  cms_parent = _cms_parent;
  mode = CMS_NO_UPDATE;
  neutral_size_factor = _neutral_size_factor;
  if ((encoded_data == NULL || !using_external_encoded_data)
      && create_encoded_data)
    {
      if (cms_parent->enc_max_size > 0
	  && cms_parent->enc_max_size < neutral_size_factor * size
	  && (encoded_data == NULL
	      || encoded_data_size < cms_parent->enc_max_size))
	{
	  set_encoded_data (DEBUG_MALLOC (cms_parent->enc_max_size),
			    cms_parent->enc_max_size);

	}
      else if (encoded_data == NULL ||
	       encoded_data_size < neutral_size_factor * size)
	{
	  set_encoded_data (DEBUG_MALLOC (neutral_size_factor * size),
			    neutral_size_factor * size);
	  if (!cms_parent->neutral)
	    {
	      cms_parent->max_encoded_message_size =
		cms_parent->max_message_size * neutral_size_factor;
	    }
	  if (cms_parent->max_encoded_message_size > encoded_data_size)
	    {
	      cms_parent->max_encoded_message_size = encoded_data_size;
	    }
	}
      using_external_encoded_data = false;
    }
  my_xml_style_properties_count = 0;
  last_check_type_info_type = 0;
  last_check_type_info_buffer = 0;
  last_check_type_info_nsname = 0;
  last_check_type_info_symbol_lookup_function = 0;
  last_check_type_info_namelist = 0;
  last_check_type_info_idlist = 0;
  last_check_type_info_sizelist = 0;
  last_check_type_info_list_length = 0;
  last_check_type_info_max_name_length = 0;
  next_default=0;
  check_type_info_called=false;
  updater_count++;
  rcs_print_debug (PRINT_CMS_CONSTRUCTORS, "CMS_UPDATER  constructor returning. (this=%p,cms_parent=%p,encoded_data=%p,encoded_data_size=%ld,using_external_encoded_data=%d,updater_count =%d)\n",
		   (void *) (this),(void *) (cms_parent),
		   encoded_data,encoded_data_size,using_external_encoded_data,updater_count);
}

void
CMS_UPDATER::remove_uba(void **vpp_to_remove)
{
  if(!uba_list || !vpp_to_remove)
    {
      return;
    }
  for(int i = 0; i < uba_list_length; i++)
    {
      void **vpp = uba_list[i];
      if(vpp && *vpp && (vpp_to_remove == vpp))
	{
	  *vpp=0;
	  uba_list[i]=0;
	}
    }
}

void
CMS_UPDATER::add_uba(void **vpp)
{
  if(!vpp || !*vpp)
    {
      return;
    }
  if(uba_list)
    {
      for(int i = 0; i < uba_list_length; i++)
	{
	  void **vpp_from_list = uba_list[i];
	  if(vpp_from_list == vpp)
	    {
	      return;
	    }
	  if(vpp_from_list == 0  || (*vpp_from_list) == 0)
	    {
	      uba_list[i]=vpp;
	      return;
	    }
	}
    }

  if(!uba_list || uba_list_size_allocated < 1 )
    {
      uba_list_size_allocated = 256;
      uba_list_length = 0;
      uba_list = (void ***) 
	malloc(sizeof(void **)*uba_list_size_allocated);
      memset(uba_list,0,sizeof(void **)*uba_list_size_allocated);
    }
  else if(uba_list_size_allocated <= uba_list_length )
    {
      uba_list_size_allocated += 256;
      uba_list = (void ***) 
	realloc((void *) uba_list,
		sizeof(void **)*uba_list_size_allocated);
      void *end_uba_list = (void *)
	(((char *) uba_list) +(sizeof(void **)*(uba_list_size_allocated-256)));
      memset(end_uba_list,
	     0,
	     sizeof(void **)*256);
    }
  uba_list[uba_list_length] = vpp;
  uba_list_length++;
}

void
CMS_UPDATER::free_uba_list()
{
  if(!uba_list)
    {
      return;
    }
  for(int i = 0; i < uba_list_length; i++)
    {
      void **vpp = uba_list[i];
      if(vpp && *vpp)
	{
	  free(*vpp);
	  *vpp=0;
	}
      uba_list[i]=0;
    }
  uba_list_length = 0;
}

	   
CMS_UPDATER::~CMS_UPDATER ()
{
  rcs_print_debug (PRINT_CMS_DESTRUCTORS, "deleting CMS_UPDATER\n");
  free_uba_list();
  if(uba_list)
    {
      void *vp = uba_list;
      uba_list=0;
      free(vp);
    }
  updater_count--;
}

void
CMS_UPDATER::rewind ()
{
  check_type_info_called=false;
  check_type_changed=true;
}

int
CMS_UPDATER::set_mode (CMS_UPDATER_MODE _mode)
{
  mode = _mode;
  switch (mode)
    {
    case CMS_NO_UPDATE:
      break;

    case CMS_ENCODE_DATA:
      encoding = 1;
      break;

    case CMS_DECODE_DATA:
      encoding = 0;
      break;

    case CMS_ENCODE_HEADER:
      encoding = 1;
      break;

    case CMS_DECODE_HEADER:
      encoding = 0;
      break;

    case CMS_ENCODE_QUEUING_HEADER:
      encoding = 1;
      break;

    case CMS_DECODE_QUEUING_HEADER:
      encoding = 0;
      break;

    default:
      rcs_print_error ("CMS updater in invalid mode.\n");
      return (-1);
    }
  return (0);
}

CMS_UPDATER_MODE CMS_UPDATER::get_mode ()
{
  return mode;
}


int
CMS_UPDATER::check_pointer (char * _pointer, long _bytes)
{
  if (pointer_check_disabled > 0)
    return (0);

  if (NULL == cms_parent)
    {
      return (-1);
    }
  return (cms_parent->check_pointer (_pointer, _bytes));
}


int
CMS_UPDATER::check_pointer_with_name (const char *name,
				      char * _pointer, long _bytes)
{
  if(print_format)
    {
      int pos=get_pos();
      const char *fullname =get_full_name(name);
      rcs_print("pos=\t%6.6d  \tname=\t%30.30s \tsz=\t%6.6ld\n",pos,fullname,_bytes);
    }		

  if (pointer_check_disabled > 0)
    return (0);
  if (NULL == cms_parent)
    {
      return (-1);
    }
  return (cms_parent->check_pointer_with_name (name, _pointer, _bytes));
}

void
CMS_UPDATER::set_encoded_data (void *_encoded_data, long _encoded_data_size)
{
  if (NULL != encoded_data &&
      !using_external_encoded_data && encoded_data != _encoded_data)
    {
      DEBUG_FREE (encoded_data);
      encoded_data = NULL;
    }
  encoded_data = _encoded_data;
  encoded_data_size = _encoded_data_size;
  using_external_encoded_data = 1;
}

CMS_STATUS 
CMS_UPDATER::update (bool &x)
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
CMS_UPDATER::update (bool *x, unsigned int len)
{
  for(unsigned int i=0; i < len; i++)
    {
      update(x[i]);
    }
  return(status);
}

 
CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, bool &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (bool)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

 
CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (char)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}


CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, unsigned char &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (unsigned char)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, short int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (short int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update (x);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned short int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (unsigned short int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, unsigned int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (unsigned int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (unsigned long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, float &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (float)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, double &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (double)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, long double &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (long double)))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x);
}


CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, bool *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (bool)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, char *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (char)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned char *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (unsigned char)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, short *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (short)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned short *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (unsigned short)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, int *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned int *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof (unsigned int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof(long)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, unsigned long *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof(unsigned long)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, float *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof(float)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, double *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof(double)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *vname, long double *x,
				 unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, sizeof(long double)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }

  return update (x, len);
}


CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, bool &x)
{
  return update_with_name (vname, x);
}


CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, char &x)
{
  return update_with_name (vname, x);
}


CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned char &x)
{
  return update_with_name (vname, x);
}


CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, short int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned short int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
CMS_UPDATER::update_attribute_with_name (const char *vname, int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, unsigned int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, long int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned long int &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, float &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, double &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, long double &x)
{
  return update_with_name (vname, x);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, char *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned char *x, unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, short *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned short *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, int *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, unsigned int *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, long *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname,
					   unsigned long *x, unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, float *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, double *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *vname, long double *x,
					   unsigned int len)
{
  return update_with_name (vname, x, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name, CMS_DURATION & d)
{
  return update_with_name (name, d);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name, CMS_DURATION * d,
					   int len)
{
  return update_with_name (name, d, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_DATE_TIME & d)
{
  return update_with_name (name, d);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_DATE_TIME * d, int len)
{
  return update_with_name (name, d, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_TIME & d)
{
  return update_with_name (name, d);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_TIME * d, int len)
{
  return update_with_name (name, d, len);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_DATE & d)
{
  return update_with_name (name, d);
}

CMS_STATUS
  CMS_UPDATER::update_attribute_with_name (const char *name,
					   CMS_DATE * d, int len)
{
  return update_with_name (name, d, len);
}


CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, char *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname,x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, unsigned char *x,
				     int &len, int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname,x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, short *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname,(void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, unsigned short *x,
				     int &len, int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void*)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, int *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname,(void*)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, unsigned int *x,
				     int &len, int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname,(void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, long *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, unsigned long *x,
				     int &len, int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, float *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, double *x, int &len,
				     int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

CMS_STATUS
  CMS_UPDATER::update_dla_with_name (const char *vname, long double *x,
				     int &len, int maxlen)
{
  if( len < 0 || len > maxlen || maxlen < 1)
    {
      rcs_print_error("CMS_UPDATER::update_dla_with_name(%s,%p,%d,%d) bad length\n",vname, (void *)x,len,maxlen);
      return(status =CMS_UPDATE_ERROR);
    }
  return update_with_name (vname, x,
			   (unsigned int) (len < maxlen ? (len>0?len:0) : maxlen));
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::beginClass (
			 __unused_parameter__ const char *, 
			 __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::endClass (
		       __unused_parameter__ const char *, 
		       __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::beginClassVar (
			    __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::endClassVar (
			  __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::beginBaseClass (
			     __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::endBaseClass (
			   __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::beginStructArrayElem (
				   __unused_parameter__ const char *, 
				   __unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}


/*ARGSUSED*/
CMS_STATUS
CMS_UPDATER::endStructArrayElem (__unused_parameter__ const char *, 
				 __unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}



/*ARGSUSED*/
CMS_STATUS
  CMS_UPDATER::beginStructDynamicArray (
					__unused_parameter__ const char *,
					__unused_parameter__ int &,
					__unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}


/*ARGSUSED*/
CMS_STATUS
  CMS_UPDATER::endStructDynamicArray (
				      __unused_parameter__ const char *, 
				      __unused_parameter__ int &, 
				      __unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}


/*
 * This function was added to support XML encoding.
 * which needs more info about available class names before
 * determining the type.
 */
long
CMS_UPDATER::check_type_info (long type, void *buffer, const char *nsname,
			      cms_symbol_lookup_function_t
			      symbol_lookup_function, const char **namelist,
			      const long *idlist, const size_t * sizelist,
			      long list_length, long max_name_length)
{
  check_type_info_called=true;
  check_type_changed = ( last_check_type_info_type != type) && (last_check_type_info_type > 0);
  last_check_type_info_type = type;
  last_check_type_info_buffer = buffer;
  last_check_type_info_nsname = nsname;
  last_check_type_info_symbol_lookup_function = symbol_lookup_function;
  last_check_type_info_namelist = namelist;
  last_check_type_info_idlist = idlist;
  last_check_type_info_sizelist = sizelist;
  last_check_type_info_list_length = list_length;
  last_check_type_info_max_name_length = max_name_length;

  if(!encoding)
    {
      for(int i = 0; i < list_length ; i++)
	{
	  if(type == idlist[i])
	    {
	      ((NMLmsg *)buffer)->size = sizelist[i];
	      break;
	    }
	}
      if(unbounded_used && check_type_changed && type != 1)
	{
	  free_uba_list();
	}
    }

  return type;
}

/* This function is only useable with encoding methods such as 
   xml that allow partial messages to be sent. */
CMS_STATUS
CMS_UPDATER::setBufferForDiff (
			       __unused_parameter__ void *, 
			       __unused_parameter__ size_t)
{
  return CMS_STATUS_NOT_SET;
}


int
CMS_UPDATER::update_enumeration_with_name (const char *name,
					   __unused_parameter__ int, 
					   void *enumaddr,
					   __unused_parameter__ const cms_enum_info *)
{
  update_with_name (name, ((int *) enumaddr), 1);
  return (*((int *) enumaddr));
}

int
CMS_UPDATER::update_union_selector_with_name (const char *name,
					      __unused_parameter__ int, 
					      void *enumaddr,
					      __unused_parameter__ const cms_enum_info *)
{
  update_with_name (name, ((int *) enumaddr), 1);
  return (*((int *) enumaddr));
}


CMS_STATUS
  CMS_UPDATER::beginEnumerationArray (
				      __unused_parameter__ const char *,
				      __unused_parameter__ const cms_enum_info *,
				      __unused_parameter__ unsigned int)
{
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
  CMS_UPDATER::beginEnumerationDLA (
				    __unused_parameter__ const char *,
				    __unused_parameter__ const cms_enum_info *, 
				    __unused_parameter__ int &,
				    __unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}

int
CMS_UPDATER::update_enumeration_array_elem (
					    __unused_parameter__ int, 
					    void *enumaddr,
					    __unused_parameter__ int)
{
  update ((int *) enumaddr, 1);
  return (*((int *) enumaddr));
}


CMS_STATUS
  CMS_UPDATER::endEnumerationArray (
				    __unused_parameter__ const char *,
				    __unused_parameter__ const cms_enum_info *,
				    __unused_parameter__ unsigned int)
{
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
  CMS_UPDATER::endEnumerationDLA (__unused_parameter__ const char *,
				  __unused_parameter__ const cms_enum_info *, 
				  __unused_parameter__ int &,
				  __unused_parameter__ int)
{
  return CMS_STATUS_NOT_SET;
}


CMS_STATUS
CMS_UPDATER::beginUnion (
			 __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_UPDATER::endUnion (
		       __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_UPDATER::beginUnionVar (
			    __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_UPDATER::endUnionVar (
			  __unused_parameter__ const char *)
{
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS CMS_UPDATER::update (CMS_DURATION & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  update (d.years);
  update (d.months);
  update (d.days);
  update (d.hours);
  update (d.minutes);
  update (d.seconds);
  return status;
}

CMS_STATUS CMS_UPDATER::update (CMS_DURATION * d, int len)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  for (int i = 0; i < len; i++)
    {
      update (d[i]);
    }
  return status;
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *name, CMS_DURATION & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  beginClassVar (name);
  beginClass ("CMS_DURATION", 0);
  update_with_name ("years", d.years);
  update_with_name ("months", d.months);
  update_with_name ("days", d.days);
  update_with_name ("hours", d.hours);
  update_with_name ("minutes", d.minutes);
  update_with_name ("seconds", d.seconds);
  endClass ("CMS_DURATION", 0);
  endClassVar (name);
  return status;
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *cname, CMS_DURATION * d, int len)
{

  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int i;
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, d[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}


CMS_STATUS
CMS_UPDATER::update (CMS_DATE_TIME & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  update (d.years);
  update (d.months);
  update (d.days);
  update (d.hours);
  update (d.minutes);
  update (d.seconds);
  update (d.timezoneoffsethours);
  return status;
}

CMS_STATUS
CMS_UPDATER::update (CMS_DATE_TIME * d, int len)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  for (int i = 0; i < len; i++)
    {
      update (d[i]);
    }
  return status;
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *name, CMS_DATE_TIME & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (name);
  beginClass ("CMS_DATE_TIME", 0);
  update_with_name ("years", d.years);
  update_with_name ("months", d.months);
  update_with_name ("days", d.days);
  update_with_name ("hours", d.hours);
  update_with_name ("minutes", d.minutes);
  update_with_name ("seconds", d.seconds);
  update_with_name ("timezoneoffsethours",d.timezoneoffsethours);
  endClass ("CMS_DATE_TIME", 0);
  endClassVar (name);
  return status;
}

CMS_STATUS
CMS_UPDATER::update_with_name (const char *cname, CMS_DATE_TIME * d,
			       int len)
{

  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int i;
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, d[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}


CMS_STATUS CMS_UPDATER::update (CMS_TIME & d)
{
  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  update (d.hours);
  update (d.minutes);
  update (d.seconds);
  update (d.timezoneoffsethours);
  return status;
}

CMS_STATUS CMS_UPDATER::update (CMS_TIME * d, int len)
{
  if (status == CMS_UPDATE_ERROR)
    {
      return CMS_STATUS_NOT_SET;
    }

  for (int i = 0; i < len; i++)
    {
      update (d[i]);
    }
  return status;
}

CMS_STATUS CMS_UPDATER::update_with_name (const char *name, CMS_TIME & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (name);
  beginClass ("CMS_TIME", 0);
  update_with_name ("hours", d.hours);
  update_with_name ("minutes", d.minutes);
  update_with_name ("seconds", d.seconds);
  update_with_name ("timezoneoffsethours",d.timezoneoffsethours);
  endClass ("CMS_TIME", 0);
  endClassVar (name);
  return status;
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *cname, CMS_TIME * d,
				 int len)
{

  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int i;
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, d[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS CMS_UPDATER::update (CMS_DATE & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  update (d.years);
  update (d.months);
  update (d.days);
  return status;
}

CMS_STATUS CMS_UPDATER::update (CMS_DATE * d, int len)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  for (int i = 0; i < len; i++)
    {
      update (d[i]);
    }
  return status;
}

CMS_STATUS CMS_UPDATER::update_with_name (const char *name, CMS_DATE & d)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  beginClassVar (name);
  beginClass ("CMS_DATE", 0);
  update_with_name ("years", d.years);
  update_with_name ("months", d.months);
  update_with_name ("days", d.days);
  endClass ("CMS_DATE", 0);
  endClassVar (name);
  return status;
}

CMS_STATUS
  CMS_UPDATER::update_with_name (const char *cname, CMS_DATE * d,
				 int len)
{
  if (status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;

  int i;
  for (i = 0; i < len; i++)
    {
      beginStructArrayElem (cname, i);
      update_with_name (0, d[i]);
      endStructArrayElem (cname, i);
    }
  return (status);
}

CMS_STATUS
  CMS_UPDATER::update_dla_length_with_name (const char *name, int &len)
{
  rcs_print_debug(PRINT_MISC,"CMS_UPDATER::update_dla_with_name(name=%s,len=%d(&len=%p)) called.\n",
		  name?name:"(null)",
		  len, (void *)(&len));
  return update_with_name (name, len);
}

int
CMS_UPDATER::xmlSetStyleProperty (
				  __unused_parameter__ const char *)
{
  return 0;
}

void
CMS_UPDATER::decode_len (
			 __unused_parameter__ const char *, 
			 __unused_parameter__ int &)
{
}


CMS_STATUS
CMS_UPDATER::update_unbounded_attribute_with_name (const char *name, char **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (char *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **)x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **)x);
	      *x = (char *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **)x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_attribute_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS
CMS_UPDATER::update_unbounded_with_name (const char *name, char **x,
					 int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (char *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (char *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name,
					   unsigned char **x, int &len,
					   int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (unsigned char *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (unsigned char *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, short **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (short *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **)x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (short *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name,
					   unsigned short **x, int &len,
					   int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (unsigned short *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (unsigned short *) realloc (*x, len * sizeof (**x));
	      memset(((char *)*x)+size_allocated*sizeof(**x),
		     0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, int **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (int *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (int *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, unsigned int **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (unsigned int *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (unsigned int *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, long **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (long *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (long *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name,
					   unsigned long **x, int &len,
					   int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (unsigned long *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (unsigned long *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, float **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (float *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (float *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, double **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      len = size_allocated;
	    }
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (double *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (double *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}

CMS_STATUS
  CMS_UPDATER::update_unbounded_with_name (const char *name, long double **x,
					   int &len, int &size_allocated)
{
  unbounded_used = true;
  if (no_unbounded || status == CMS_UPDATE_ERROR)
    return CMS_STATUS_NOT_SET;
  int orig_pointer_check_disabled = pointer_check_disabled;
  CMS_STATUS retval = CMS_STATUS_NOT_SET;
  update(len);
  pointer_check_disabled = 1;
  if (encoding)
    {
      if (*x==0)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (long double *) malloc (len * sizeof (**x));
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (long double *) realloc (*x, len * sizeof (**x));
	      memset(((char *)(*x))+size_allocated*sizeof(**x),0, (len-size_allocated)* sizeof (**x));
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  if (*x != 0 && len > 0)
    {
      retval = update_with_name (name, *x, len);
    }
  pointer_check_disabled = orig_pointer_check_disabled;
  return retval;
}


CMS_STATUS
  CMS_UPDATER::beginStructUnboundedArray (const char *name, void **x,
					  int &len, int &size_allocated,
					  size_t elsize)
{
  unbounded_used = true;
  if (pointer_check_disabled < 0)
    {
      pointer_check_disabled = 1;
    }
  else
    {
      pointer_check_disabled++;
    }
  if (no_unbounded || status == CMS_UPDATE_ERROR || elsize < 1)
    {
      return CMS_STATUS_NOT_SET;
    }

  update(len);
  if (encoding)
    {
      if(*x==0)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      decode_len (name, len);
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = malloc (elsize * len);
	  memset(*x,0,len * elsize);
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = realloc (*x, elsize * len);
	      memset(((char *)*x)+size_allocated*elsize,0,
		     (len-size_allocated)*elsize);
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_UPDATER::endStructUnboundedArray (
				      __unused_parameter__ const char *, 
				      __unused_parameter__ void **,
				      __unused_parameter__ int &,
				      __unused_parameter__ int &, 
				      __unused_parameter__ size_t)
{
  unbounded_used = true;
  if (pointer_check_disabled > 0)
    {
      pointer_check_disabled--;
    }
  return CMS_STATUS_NOT_SET;
}

CMS_STATUS
CMS_UPDATER::beginEnumerationUnbounded (
					const char *name, 
					int **x,
					const cms_enum_info * info,
					int &len,
					int &size_allocated,
					size_t elsize)
{
  unbounded_used = true;
  if (pointer_check_disabled < 0)
    {
      pointer_check_disabled = 1;
    }
  else
    {
      pointer_check_disabled++;
    }
  if (no_unbounded || status == CMS_UPDATE_ERROR || elsize < 1)
    return CMS_STATUS_NOT_SET;

  update(len);
  if (encoding)
    {
      if (*x==0)
	{
	  size_allocated = 0;
	}
      if (len > size_allocated)
	{
	  len = size_allocated;
	}
    }
  else
    {
      decode_len (name, len);
      if (elsize < sizeof (int))
	{
	  elsize = sizeof (int);
	}
      if ((*x==0 || check_type_changed) && len > 0)
	{
	  *x = (int *) malloc (elsize * len);
	  memset(*x,0,len * sizeof (**x));
	  size_allocated = len;
	  add_uba((void **) x);
	}
      if (*x != 0)
	{
	  if (len > size_allocated)
	    {
	      remove_uba((void **) x);
	      *x = (int *) realloc (*x, elsize * len);
	      size_allocated = len;
	      add_uba((void **) x);
	    }
	}
    }
  return beginEnumerationArray (name, info, len);
}


CMS_STATUS
CMS_UPDATER::endEnumerationUnbounded (
				      const char *name, 
				      __unused_parameter__ int **,
				      const cms_enum_info * info, 
				      int &len,
				      __unused_parameter__ int &, 
				      __unused_parameter__ size_t)
{
  unbounded_used = true;
  if (pointer_check_disabled > 0)
    {
      pointer_check_disabled--;
    }
  return endEnumerationArray (name, info, len);
}

CMS_STATUS 
CMS_UPDATER::next_update_default(const char *_next_default)
{
  next_default = _next_default;
  return status; 
}

void CMS_UPDATER::recheck_properties(void)
{
  my_xml_style_properties_count = global_xml_style_properties_count;
}

CMS_UPDATER::CMS_UPDATER(const CMS_UPDATER &_cms_up):
  /**********************************************
  * Aliases to variables in the CMS parent
  * using aliases lets CMS and its CMS_UPDATER share this information
  * more conveniently and allowed the CMS_UPDATER functions to be pulled out
  * of CMS with fewer changes. (WPS - 6/12/95)
  *********************************************/
cms_parent(_cms_up.cms_parent),
encoded_data (_cms_up.cms_parent->encoded_data),
encoded_header (_cms_up.cms_parent->encoded_header),
encoded_queuing_header (_cms_up.cms_parent->encoded_queuing_header),
status (_cms_up.cms_parent->status),
size (_cms_up.cms_parent->size),
encoded_header_size (_cms_up.cms_parent->encoded_header_size),
encoded_queuing_header_size (_cms_up.cms_parent->encoded_queuing_header_size),
using_external_encoded_data (_cms_up.cms_parent->using_external_encoded_data),
pointer_check_disabled (_cms_up.cms_parent->pointer_check_disabled),
encoded_data_size (_cms_up.cms_parent->encoded_data_size),
no_unbounded (_cms_up.cms_parent->no_unbounded),
unbounded_used (_cms_up.cms_parent->unbounded_used),
add_array_indexes_to_name(_cms_up.cms_parent->add_array_indexes_to_name),
xml_style_properties(_cms_up.cms_parent->xml_style_properties),
global_xml_style_properties_count(_cms_up.cms_parent->global_xml_style_properties_count),
last_check_type_info_type(0),
last_check_type_info_buffer(0),
last_check_type_info_nsname(0),
last_check_type_info_symbol_lookup_function(0),
last_check_type_info_namelist(0),
last_check_type_info_idlist(0),
last_check_type_info_sizelist(0),
last_check_type_info_list_length(0),
last_check_type_info_max_name_length(0),
check_type_info_called(false),
fail_on_overflow(false),
my_xml_style_properties_count(0),
neutral_size_factor(0),
encoding(0),
mode(CMS_NO_UPDATE),
next_default(0)
{
  rcs_print_error("CMS_UPDATER copy constructor should never be called.\n");
}

CMS_UPDATER &
CMS_UPDATER::operator=(const CMS_UPDATER &)
{
  rcs_print_error("CMS_UPDATER::operator= should never be called.\n");
  return(*this);
}

int
CMS_UPDATER::get_pos()
{
  return -1;
}

const char *
CMS_UPDATER::get_full_name(const char *name)
{
  return name;
}

#ifdef ENABLE_LONG_LONG_UP

enum CMS_STATUS
CMS_UPDATER::update (long long int &x)
{
  if(-1 == check_pointer((char *) &x, sizeof (long long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  int orig_pointer_check_disabled = pointer_check_disabled;
  pointer_check_disabled = 1;
  static const int int_bits = sizeof(int)*8;
  unsigned long long xu = (unsigned long long) x;
  unsigned int xh = 0;
  unsigned int xl = (unsigned int) x;
  if(sizeof(long long) > sizeof(int)) {
    xh = (unsigned int) (xu >>int_bits);
    xl = (unsigned int) (xu % (1ULL<<int_bits));
  }
  update(xh);
  update(xl);
  unsigned long long xullh = (unsigned long long) xh;
  unsigned long long xulll = (unsigned long long) xl;
  x = (long long int) xl;
  if(sizeof(long long) > sizeof(int)) {
    xu = (xullh << int_bits) + xulll;
    x = (long long int) xu;
  }
  pointer_check_disabled = orig_pointer_check_disabled;
  return status;
}

enum CMS_STATUS
CMS_UPDATER::update (unsigned long long int &x)
{
  if (-1 == check_pointer((char *) &x, sizeof (long long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  int orig_pointer_check_disabled = pointer_check_disabled;
  pointer_check_disabled = 1;
  static const unsigned int int_bits = sizeof(int)*8;
  unsigned int xh = 0;
  unsigned int xl = (unsigned int) x;
  if(sizeof(long long) > sizeof(int)) {
    xh = (unsigned int) (x >>int_bits);
    xl = (unsigned int) (x % (1ULL<<int_bits));
  }
  update(xh);
  update(xl);
  unsigned long long xullh = (unsigned long long) xh;
  unsigned long long xulll = (unsigned long long) xl;
  x = (long long int) xl;
  if(sizeof(long long) > sizeof(int)) {
    x = (xullh << int_bits) + xulll;
  }
  pointer_check_disabled = orig_pointer_check_disabled;
  return status;
}

enum CMS_STATUS
CMS_UPDATER::update (long long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (long long int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  for(unsigned int i = 0; i < len; i++) {
    if(update(x[i]) < 0) {
      return status;
    }
  }
  return status;
}

enum CMS_STATUS
CMS_UPDATER::update (unsigned long long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer ((char *) &x, sizeof (unsigned long long int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  for(unsigned int i = 0; i < len; i++) {
    if(update(x[i]) < 0) {
      return status;
    }
  }
  return status;
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, long long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (long long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update(x);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, unsigned long long int &x)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) &x, sizeof (unsigned long long int)))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  return update(x);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname, 
			       long long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, 
				     sizeof (long long int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  for(unsigned int i = 0; i < len; i++) {
    if(update(x[i]) < 0) {
      return status;
    }
  }
  return status;
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *vname,
			       unsigned long long *x, unsigned int len)
{
  /* Check to see if the pointers are in the proper range. */
  if (-1 == check_pointer_with_name (vname, (char *) x, 
				     sizeof (unsigned long long int)*len))
    {
      return (status=CMS_UPDATE_ERROR);
    }
  for(unsigned int i = 0; i < len; i++) {
    if(update(x[i]) < 0) {
      return status;
    }
  }
  return status;
}

#else  /* ENABLE_LONG_LONG */

enum CMS_STATUS
CMS_UPDATER::update (long long int &)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update (unsigned long long int &)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update (long long *, unsigned int)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update (unsigned long long *, unsigned int)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *, long long int &)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *, unsigned long long int &)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *, long long int *, unsigned int)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

enum CMS_STATUS
CMS_UPDATER::update_with_name (const char *, unsigned long long int *,unsigned int)
{
  return (status = CMS_NO_IMPLEMENTATION_ERROR);
}

#endif /* ENABLE_LONG_LONG */

