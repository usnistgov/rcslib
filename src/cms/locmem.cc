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

/************************************************************************
File: locmem.cc
Purpose: Implements LOCMEM which is a derived class of CMS that serves primarily
to provide addresses that match when matching buffer names are passed to
the constructor. It is useful in allowing control modules to use the
same inteface to communicate as would be required if they were not
running in the same process even though to use LOCMEM they must be.
**********************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_LOCMEM)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"		// EXTERN_C_STD_HEADERS
#endif

#include "locmem.hh"		// class LOCMEM
#include "cms.hh"		// class CMS
#include "linklist.hh"		// class RCS_LINKED_LIST
#include "rcs_prnt.hh"		// rcs_print_error()
#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>		// malloc()
#include <string.h>		// strcpy(), strcmp()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

RCS_LINKED_LIST *
  LOCMEM::buffers_list = (RCS_LINKED_LIST *)
  NULL;

class BUFFERS_LIST_NODE
{
public:
  BUFFERS_LIST_NODE():
    addr(0),size(0),refs(0)
  {
    memset(name,0,sizeof(name));
  };

  void *addr;
  long size;
  char name[64];
  int refs;
  
private:
  BUFFERS_LIST_NODE(const BUFFERS_LIST_NODE &_blnref):
    addr(0),size(0),refs(0)
  {
  };

  BUFFERS_LIST_NODE &operator=(const BUFFERS_LIST_NODE &_blnref)
  {
    return(*this);
  };
    
};


LOCMEM::LOCMEM (const char *bufline, 
		const char *procline,
		int set_to_server,
		int set_to_master):
  CMS (bufline, procline, set_to_server),
  lm_addr(0),
  buffer_id(-1),
  my_node(0)
{
  my_node = (BUFFERS_LIST_NODE *) NULL;
  lm_addr = NULL;

  if (set_to_master == 1)
    {
      is_local_master = true;
    }
  if (set_to_master == -1)
    {
      is_local_master = false;
    }

  my_node=0;
  if (is_local_master)
    {
      if(buffers_list)
	{
	  my_node = (BUFFERS_LIST_NODE *) buffers_list->get_head ();
	  while (my_node != NULL)
	    {
	      if (!strcmp (BufferName, my_node->name))
		{
		  /* Found it!!! */
		  if (my_node->size != size)
		    {
		      rcs_print_error ("LOCMEM - size mismatch for buffer %s.\n",
			       BufferName);
		      status = CMS_CONFIG_ERROR;
		      return;
		    }
		  buffer_id = buffers_list->get_current_id ();
		  lm_addr = my_node->addr;
		  return;
		}
	      my_node = (BUFFERS_LIST_NODE *) buffers_list->get_next ();
	    }
	}
      if(!my_node)
	{
	  if (buffers_list == NULL)
	    {
	      buffers_list = new RCS_LINKED_LIST;
	    }
	  if (buffers_list == NULL)
	    {
	      rcs_print_error ("LOCMEM: Can't create buffers_list.\n");
	      status = CMS_CREATE_ERROR;
	      return;
	    }
	  my_node = new BUFFERS_LIST_NODE;
	  lm_addr = my_node->addr = DEBUG_MALLOC (size);
	  if (my_node == NULL || lm_addr == NULL)
	    {
	      rcs_print_error ("Can't malloc needed space.\n");
	      status = CMS_CREATE_ERROR;
	      return;
	    }
	  my_node->size = size;
	  strcpy (my_node->name, BufferName);
	  memset (my_node->addr, 0, size);
	  buffer_id = buffers_list->store_at_tail (my_node, sizeof (my_node), 0);
	}
      return;
    }

  if (buffers_list == NULL)
    {
      rcs_print_error ("LOCMEM: buffers_list is NULL.\n");
      status = CMS_NO_MASTER_ERROR;
      return;
    }

  /* Search for a matching buffer name. */
  if(!my_node)
    {
      my_node = (BUFFERS_LIST_NODE *) buffers_list->get_head ();
      while (my_node != NULL)
	{
	  if (!strcmp (BufferName, my_node->name))
	    {
	      /* Found it!!! */
	      if (my_node->size != size)
		{
		  rcs_print_error ("LOCMEM - size mismatch for buffer %s.\n",
				   BufferName);
		  status = CMS_CONFIG_ERROR;
		  return;
		}
	      buffer_id = buffers_list->get_current_id ();
	      lm_addr = my_node->addr;
	      return;
	    }
	  my_node = (BUFFERS_LIST_NODE *) buffers_list->get_next ();
	}
    }
  if(my_node)
    {
      my_node->refs++;
    }
  rcs_print_error ("LOCMEM: buffer not found on buffers_list.\n");
  status = CMS_NO_MASTER_ERROR;
  return;

}


LOCMEM::~LOCMEM ()
{
  if (NULL != buffers_list)
    {
      buffers_list->delete_node (buffer_id);
      if (0 == buffers_list->list_size)
	{
	  delete buffers_list;
	  buffers_list = (RCS_LINKED_LIST *) NULL;
	}
    }
  if(my_node)
    {
      my_node->refs--;
      if(my_node->refs == 0 && my_node->addr != 0 && my_node->addr == lm_addr)
	{
	  DEBUG_FREE(my_node->addr);
	  my_node->addr=0;
	  delete my_node;
	}
    }
  my_node=0;
  lm_addr=0;
}

CMS_STATUS LOCMEM::main_access (void *local_address)
{
  internal_access (lm_addr, size, local_address);
  return status;
}

//Private copy constructor and = operator to prevent copying.
// only cms_cfg copy functions can be used to copy CMS objects.
LOCMEM::LOCMEM(
	       __unused_parameter__ const LOCMEM &_cms_ref):
  CMS (0),
  lm_addr(0),
  buffer_id(-1),
  my_node(0)
{
  rcs_print_error("LOCMEM copy constructor should never be called.\n");
}
  
LOCMEM &
LOCMEM::operator=(
		  __unused_parameter__ const LOCMEM &_cms_ref)
{
  rcs_print_error("LOCMEM::operator= should never be called.\n");
  return(*this);
}


//  defined(ENABLE_RCS_LOCMEM)

#else
#include "rcs_empty_source"
#endif
