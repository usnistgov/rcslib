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

/*************************************************************************
* File:nml_srv.cc                                                        *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++  file for RPC server that reads and writes                *
*          to a local NML buffer for remote processes.                   *
* Includes:                                                              *
*          1. class NML_SERVER member functions.                         *
*************************************************************************/

#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_SERVER)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "nml_srv_no_config.h"
#endif

#include "nml.hh"
#include "nmlmsg.hh"
#include "cms.hh"
#include "cms_srv.hh"		// class CMS_SERVER
#include "nml_srv.hh"
#include "linklist.hh"		// class RCS_LINKED_LIST
#include "rem_msg.hh"		/* struct REMOTE_READ_REQUEST, */
				/* struct REMOTE_WRITE_REQUEST, */
				/* struct REMOTE_READ_REPLY, */
				/* struct REMOTE_WRITE_REPLY, */
#include "rcs_prnt.hh"		/* rcs_print_error() */
#include "timer.hh"		// esleep()
#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE
#ifdef ENABLE_RCS_NMLCFGSVR
#include "nmlcfgsvr_clntcalls.hh" 
#endif

#include "cms_clnt_info.hh" // class CMS_CLIENT_INFO
#include "physmem.hh"		// class PHYSMEM_HANDLE

#ifdef ENABLE_RCS_DIAG
#include "cmsdiag.hh"
#endif

#ifdef __GLIBC__
#if __GLIBC_PREREQ(2,3)
// undef BAD_PTHREADS
#else
#define BAD_PTHREADS 1
#endif
#endif

#ifdef BAD_PTHREADS
// I have found pthreads on a RedHat 7.0 which uses libpthread 
// box to be buggy enough
// that although you could successfully compile with POSIX_THREADS and it
// will even work for simple applications, it is necessary to avoid.
// The problem was that pthread_create would not return until after a the 
// user pressed control C to send SIGINT to the process and begin shutdown. 
#define NO_THREADS
#undef POSIX_THREADS
#endif


NML_SERVER::NML_SERVER (NML * _nml, int _set_to_master):
  CMS_SERVER (),
  super_server_list_id(0),
  being_deleted(0)
{
  NML_SERVER_LOCAL_PORT *new_local_port = NULL;
  being_deleted = 0;
  if (NULL != _nml)
    {
      if (NULL != _nml->cms)
	{
	  if (CMS_REMOTE_TYPE != _nml->cms->ProcessType)
	    {
	      NML *new_nml;
	      if (_nml->cms->isserver &&
		  (0 == _set_to_master ||
		   (_nml->cms->is_local_master && _set_to_master == 1) ||
		   (!_nml->cms->is_local_master && _set_to_master == -1)))
		{
		  new_nml = _nml;
		  if (NULL != new_nml)
		    {
		      new_local_port = new NML_SERVER_LOCAL_PORT (new_nml);
		      add_local_port (new_local_port);
		    }
		  new_local_port->local_channel_reused = 1;
		}
	      else
		{
		  new_nml = new NML (_nml, 1, -1);
		  if (NULL != new_nml)
		    {
		      new_local_port = new NML_SERVER_LOCAL_PORT (new_nml);
		      add_local_port (new_local_port);
		    }
		  new_local_port->local_channel_reused = 0;
		}
	    }
	  else
	    {
	      rcs_print_error
		("NML_SERVER:(ERROR) ProcessType was REMOTE.\n");
	      _nml = (NML *) NULL;
	    }
	}
      else
	{
	  rcs_print_error ("NML_SERVER:(ERROR) cms was NULL.\n");
	}
    }
  else
    {
      rcs_print_error ("NML_SERVER:(ERROR) nml_ptr was NULL.\n");
    }
  add_to_nml_server_list ();
}

void
NML_SERVER::add_to_nml_server_list ()
{
  if (NULL == NML_Default_Super_Server)
    {
      NML_Default_Super_Server = new NML_SUPER_SERVER;
    }
  if (NULL != NML_Default_Super_Server)
    {
      NML_Default_Super_Server->add_to_list (this);
    }
}

NML_SERVER::~NML_SERVER ()
{
  being_deleted = 1;
  delete_from_list ();
}

void
NML_SERVER::delete_from_list ()
{
  CMS_SERVER::delete_from_list ();
  if (NULL != NML_Default_Super_Server)
    {
      if (NULL != NML_Default_Super_Server->servers)
	{
	  NML_Default_Super_Server->servers->
	    delete_node (super_server_list_id);
	}
    }
}

NML_SERVER_LOCAL_PORT::NML_SERVER_LOCAL_PORT (NML * _nml):
  CMS_SERVER_LOCAL_PORT ((CMS *) NULL),
  nml(_nml),nml_for_get_dvar(0)
{
  local_channel_reused = 1;
  nml = _nml;
  if (NULL != nml)
    {
      cms = nml->cms;
      if (NULL != cms)
	{
	  enable_xml_differencing = cms->enable_xml_differencing;
	  multireader_priority_queue_enabled = cms->multireader_priority_queue_enabled;
	  buffer_number = cms->buffer_number;
	}
    }
}

NML_SERVER_LOCAL_PORT::~NML_SERVER_LOCAL_PORT ()
{
  rcs_print_debug(PRINT_MISC,"~NML_SERVER_LOCAL_PORT: this=%p,nml=%p,local_channel_reused=%d",
		  (void*)this,(void*)nml,local_channel_reused);
  NML *nml_to_delete=nml_for_get_dvar;
  nml_for_get_dvar=0;
  if(nml_to_delete && 
     !nml_to_delete->already_deleted)
    {
      nml_to_delete->immediate_spawned_server=0;
      delete nml_to_delete;
      nml_to_delete=0;
    }

  nml_to_delete = nml;
  nml = (NML *) NULL;
  cms = (CMS *) NULL;
  if (NULL != nml_to_delete && 
      !local_channel_reused && 
      !nml_to_delete->already_deleted)
    {
#ifdef POSIX_THREADS
      rcs_print_debug(PRINT_MISC,
		      "~NML_SERVER_LOCAL_PORT: nml->immediate_spawned_server=%p\n",
		      (void*)nml_to_delete->immediate_spawned_server);
      if( 0 == nml_to_delete->immediate_spawned_server)
	{
	  delete nml_to_delete;
	}
#else
      nml_to_delete->immediate_spawned_server=0;
      delete nml_to_delete;
#endif
    }
}



double
NML_SERVER_LOCAL_PORT::get_dvar (const char *vname, int &_id, long _type, bool &got_dvar, bool read_new)
{
  got_dvar=false;
  if ((NULL == cms) || (NULL == nml))
    {
      rcs_print_error ("NMLserver:reader: CMS object is NULL.\n");
      return (-44444.4444);
    }

  if(!nml_for_get_dvar)
    {
      nml_for_get_dvar = new NML(nml,-1,-1);
    }
  

  NMLTYPE t = -1;
  if(read_new)
    {
      nml_for_get_dvar->cms->in_buffer_id = _id;
      nml_for_get_dvar->cms->last_id_side0 = _id;
      nml_for_get_dvar->cms->last_id_side1 = _id;
      t = nml_for_get_dvar->peek ();
    }
  else
    {
      t = (nml_for_get_dvar->get_address())->type;
    }
  _id = nml_for_get_dvar->cms->in_buffer_id;
  if(t== _type)
    {
      NMLmsg *nml_msg_ptr = nml_for_get_dvar->get_address();
      double d = nml->get_double_var(nml_msg_ptr,vname,got_dvar);
      return d;
    }

  return -66666.6666;
}
  



REMOTE_READ_REPLY *
NML_SERVER_LOCAL_PORT::reader (  CMS_CLIENT_INFO *_current_client_info,
				 REMOTE_READ_REQUEST * _req)
{
  if ((NULL == cms) || (NULL == nml))
    {
      rcs_print_error ("NMLserver:reader: CMS object is NULL.\n");
      return ((REMOTE_READ_REPLY *) NULL);
    }
  if(0 == read_reply_ptr)
    {
      read_reply_ptr = new REMOTE_READ_REPLY();
    }

  /* Setup CMS channel from request arguments. */
  cms->in_buffer_id = _req->last_id_read;
  cms->last_id_side0 = _req->last_id_read;
  cms->last_id_side1 = _req->last_id_read;

  if (_current_client_info)
    {
      if(cms->multireader_priority_queue_enabled && cms->mrpq)
	{
	  _current_client_info->cms = cms;
	  if(_current_client_info->mrpq_reader_id < 0)
	    {
	      _current_client_info->mrpq_reader_id = cms->get_new_mrpq_reader_id();
	    }
	  else
	    {
	      cms->set_current_mrpq_reader_id(_current_client_info->mrpq_reader_id);
	    }
	}	      
      if (!_current_client_info->last_message_info.addr)
	{
	  _current_client_info->last_message_info.addr =
	    DEBUG_MALLOC (cms->size);
	  if (0 == _current_client_info->last_message_info.addr)
	    {
	      rcs_print_error ("malloc failed.\n");
	      exit (-1);
	    }
	  _current_client_info->last_message_info.size = cms->size;
	  _current_client_info->last_message_info.deleteaddr = 1;
	  memset (_current_client_info->last_message_info.addr, 0, cms->size);
	  ((NMLmsg *) _current_client_info->last_message_info.addr)->size =
	    cms->size;
	  nml->setMessageForDiff (0);
	}
      else
	{
	  nml->setMessageForDiff ((NMLmsg *) _current_client_info->
				  last_message_info.addr);
	}
    }
  else
    {
      nml->setMessageForDiff (0);
    }

  /* Read and encode the buffer. */
  switch (_req->access_type)
    {
    case CMS_READ_ACCESS:
      nml->read ();
      break;
    case CMS_PEEK_ACCESS:
      nml->peek ();
      break;
    default:
      rcs_print_error ("NML_SERVER: Invalid access type.(%d)\n",
		       _req->access_type);
      break;
    }


  /* Setup reply structure to be returned to remote process. */
  read_reply_ptr->status = (int) cms->status;
  if (cms->status == CMS_READ_OLD)
    {
      read_reply_ptr->size = 0;
      read_reply_ptr->data = NULL;
      read_reply_ptr->write_id = _req->last_id_read;
      read_reply_ptr->was_read = 1;
    }
  else
    {
      read_reply_ptr->size = cms->header.in_buffer_size;
      read_reply_ptr->data = (unsigned char *) cms->encoded_data;
      read_reply_ptr->write_id = cms->in_buffer_id;
      read_reply_ptr->was_read = cms->header.was_read;
      if (_current_client_info)
	{
	  if (_current_client_info->last_message_info.addr)
	    {
	      long s = nml->get_address ()->size;
	      if (s > ((long) _current_client_info->last_message_info.size))
		{
		  s =  (long) _current_client_info->last_message_info.size;
		}
	      nml->copyMsg (nml->get_address (),
			    _current_client_info->last_message_info.addr,
			    cms->size);
	    }
	}
    }

  /* Reply structure contains the latest shared memory info-- now
     return it to cms_dispatch  for return to caller */
  return (read_reply_ptr);
}


REMOTE_READ_REPLY *
NML_SERVER_LOCAL_PORT::blocking_read (  CMS_CLIENT_INFO *_current_client_info,
					REMOTE_READ_REQUEST * _req)
{
  if ((NULL == cms) || (NULL == nml))
    {
      rcs_print_error ("NMLserver:blocking_read: CMS object is NULL.\n");
      return ((REMOTE_READ_REPLY *) NULL);
    }
  nml->cms->first_diag_store = 0;
  if (_req->type != REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE)
    {
      rcs_print_error ("NMLserver::blocking_read: Invalid request type(%d)\n",
		       _req->type);
      return NULL;
    }
  
  nml->setMessageForDiff (0);
  double orig_bytes_moved = 0.0;

  REMOTE_BLOCKING_READ_REQUEST *breq = (REMOTE_BLOCKING_READ_REQUEST *) _req;
  REMOTE_READ_REPLY *temp_read_reply = 0;
  NML *nmlcopy = 0;
  CMS *cmscopy = 0;
  bool delete_nmlcopy=false;

  size_t data_size = 0;
  rcs_print_debug(PRINT_MISC,
		  "NML_SERVER_LOCAL_PORT::blocking_read current_client_info=%p\n",
		  (void*)_current_client_info);
  if (_current_client_info)
    {
      if(_current_client_info->nmlcopies == 0)
	{
	  _current_client_info->nmlcopies = new RCS_LINKED_LIST();
	}
      nmlcopy = (NML *) _current_client_info->nmlcopies->get_head();
      while(nmlcopy)
	{
	  if(nmlcopy->cms && nmlcopy->cms->buffer_number == breq->buffer_number)
	    {
	      break;
	    }
	  nmlcopy = (NML *) _current_client_info->nmlcopies->get_next();
	}
      if(nmlcopy == 0)
	{
	  nmlcopy = new NML(nml,1,-1);
	  _current_client_info->nmlcopies->store_at_tail(nmlcopy,sizeof(NML),0);
	}
      cmscopy= nmlcopy->cms;
      data_size=(long) cmscopy->max_encoded_message_size;
      rcs_print_debug(PRINT_MISC,
		      "NML_SERVER_LOCAL_PORT::blocking_read cmscopy->multireader_priority_queue_enabled=%d, cmscopy->mrpq=%p,current_client_info->mrpq_reader_id=%d\n",
		     cmscopy->multireader_priority_queue_enabled,
		     (void*)cmscopy->mrpq,_current_client_info->mrpq_reader_id);
      if(cmscopy->multireader_priority_queue_enabled && cmscopy->mrpq)
	{
	  _current_client_info->cms = cms;
	  if(_current_client_info->mrpq_reader_id < 0)
	    {
	      _current_client_info->mrpq_reader_id = cmscopy->get_new_mrpq_reader_id();
	    }
	  else
	    {
	      cmscopy->set_current_mrpq_reader_id(_current_client_info->mrpq_reader_id);
	    }
	}
      if(_current_client_info->temp_read_reply ==0)
	{
	  _current_client_info->temp_read_reply = new REMOTE_READ_REPLY();
	  _current_client_info->temp_read_reply->data = 0;
	}
      if(0 == _current_client_info->temp_read_reply->data)
	{
	  _current_client_info->temp_read_reply->data = DEBUG_MALLOC (data_size);
	  _current_client_info->read_reply_data_size = data_size;
	}
      else if( _current_client_info->read_reply_data_size < data_size)
	{
	  _current_client_info->temp_read_reply->data = DEBUG_REALLOC (temp_read_reply->data, data_size);
	  _current_client_info->read_reply_data_size = data_size;
	}      
      temp_read_reply = _current_client_info->temp_read_reply;
      rcs_print_debug(PRINT_MISC,
		      "current_client_info=%p,current_client_info->temp_read_reply=%p, current_client_info->temp_read_reply->data=%p,current_client_info->read_reply_data_size=%lu,data_size=%lu,temp_read_reply=%p,nml=%p,nmlcopy=%p,cmscopy=%p\n",
		      (void*)_current_client_info,
		      (void*)_current_client_info->temp_read_reply,
		      (void*)_current_client_info->temp_read_reply->data,
		      (unsigned long)_current_client_info->read_reply_data_size,
		      (unsigned long) data_size,
		      (void*)temp_read_reply,
		      (void*)nml,(void*)nmlcopy,(void*)cmscopy);
    }

  if(0 == nmlcopy)
    {
      delete_nmlcopy=true;
      nmlcopy = new NML(nml,1,-1);
      cmscopy = nml->cms;
      data_size = (long) cmscopy->max_encoded_message_size;
    }
  if(temp_read_reply ==0)
    {
      temp_read_reply = new REMOTE_READ_REPLY();
      temp_read_reply->data = DEBUG_MALLOC (data_size);
      rcs_print_debug(PRINT_MISC,
		      "temp_read_reply=%p,temp_read_reply->data=%p,data_size=%lu\n",
		      (void*)temp_read_reply,
		      (void*)temp_read_reply->data,
		      (unsigned long) data_size);
    }

  breq->_nml = nmlcopy;
  cmscopy->set_preserve_mrpq_reader_id(true);
  nmlcopy->setMessageForDiff (0);


  double blocking_timeout = (double) (breq->timeout_millis / 1000.0);
  breq->_reply = temp_read_reply;
  breq->_data = temp_read_reply->data;

  if ((NULL == cmscopy) || (NULL == nmlcopy))
    {
      rcs_print_error ("NMLserver:blocking_read: CMS object is NULL.\n");
      return ((REMOTE_READ_REPLY *) NULL);
    }
  if (NULL != cmscopy->handle_to_global_data)
    {
      orig_bytes_moved = cmscopy->handle_to_global_data->get_total_bytes_moved();
    }

  if (NULL == temp_read_reply->data)
    {
      rcs_print_error
	("NMLserver:blocking_read: temp_read_reply->data object is NULL.\n");
      return ((REMOTE_READ_REPLY *) NULL);
    }

  nmlcopy->cms->set_encoded_data (temp_read_reply->data, (long) data_size);

  /* Setup CMS channel from request arguments. */
  cmscopy->in_buffer_id = _req->last_id_read;

  /* Read and encode the buffer. */
  nmlcopy->blocking_read (blocking_timeout);

  /* Setup reply structure to be returned to remote process. */
  temp_read_reply->status = (int) cmscopy->status;
  if (cmscopy->status == CMS_READ_OLD)
    {
      temp_read_reply->size = 0;
      if (NULL != temp_read_reply->data)
	{
	  breq->_data = NULL;
	  DEBUG_FREE (temp_read_reply->data);
	  temp_read_reply->data = NULL;
	}
      temp_read_reply->write_id = _req->last_id_read;
      temp_read_reply->was_read = 1;
    }
  else
    {
      temp_read_reply->size = cmscopy->header.in_buffer_size;
      temp_read_reply->write_id = cmscopy->in_buffer_id;
      temp_read_reply->was_read = cmscopy->header.was_read;
    }
  if (NULL != nml->cms->handle_to_global_data &&
      NULL != cmscopy->handle_to_global_data)
    {
      nml->cms->handle_to_global_data->increment_total_bytes_moved(
	(cmscopy->handle_to_global_data->get_total_bytes_moved() -
	 orig_bytes_moved));
      nml->cms->first_diag_store = cmscopy->first_diag_store;
    }
  if(delete_nmlcopy && nmlcopy)
    {
      delete nmlcopy;
      nmlcopy=0;
      breq->_nml = NULL;
    }
  /* Reply structure contains the latest shared memory info-- now
     return it to cms_dispatch  for return to caller */
  return (temp_read_reply);
}


/* REMOTE local_port function for writes */
REMOTE_WRITE_REPLY *
NML_SERVER_LOCAL_PORT::writer (  CMS_CLIENT_INFO *_current_client_info,
				 REMOTE_WRITE_REQUEST * _req)
{
  NMLmsg *temp;			/* Temporary Pointer */

  if ((NULL == cms) || (NULL == nml))
    {
      rcs_print_error ("NMLserver:writer: CMS object is NULL.\n");
      return ((REMOTE_WRITE_REPLY *) NULL);
    }
  
  if(0 == write_reply_ptr)
    {
      write_reply_ptr = new REMOTE_WRITE_REPLY();
    }
  
  if(cms->force_raw)
    {
      temp = (NMLmsg *) _req->data;
    }
  else
    {
      temp = (NMLmsg *) cms->data;
      /* Check to see if remote process writing too much into local buffer. */
      // if (_req->size > cms_encoded_data_explosion_factor * cms->size)
// 	{
// 	  rcs_print_error
// 	    ("CMSserver:cms_writer: CMS buffer size(%lu 0x%lX) is too small needs to be %lu 0x%lX or %d / %d\n",
// 	     cms->size,cms->size,
// 	     (unsigned long) (_req->size/cms_encoded_data_explosion_factor),
// 	     (unsigned long) (_req->size/cms_encoded_data_explosion_factor),
// 	     _req->size,
// 	     cms_encoded_data_explosion_factor);
// 	  rcs_print_error("BufferLine: %s\n",
// 			  cms->BufferLine);
// 	  return ((REMOTE_WRITE_REPLY *) NULL);
// 	}

      /* Copy the encoded data to the location set up in CMS. */
      // memcpy(cms->encoded_data, _req->data, _req->size);
      cms->header.in_buffer_size = _req->size;
      temp->size = _req->size;
    }

  if (_current_client_info)
    {
      if (!_current_client_info->last_message_info.addr)
	{
	  _current_client_info->last_message_info.addr =
	    DEBUG_MALLOC (cms->size);
	  if (0 == _current_client_info->last_message_info.addr)
	    {
	      rcs_print_error ("malloc failed.\n");
	      exit (-1);
	    }
	  _current_client_info->last_message_info.size = cms->size;
	  _current_client_info->last_message_info.deleteaddr = 1;
	  memset (_current_client_info->last_message_info.addr, 0, cms->size);
	  ((NMLmsg *) _current_client_info->last_message_info.addr)->size =
	    cms->size;
	}
      else
	{
	  nml->copyMsg ((NMLmsg *) _current_client_info->last_message_info.
			addr, (void *) temp, cms->size);
	}
    }

  switch (_req->access_type)
    {
    case CMS_WRITE_ACCESS:
      switch(_req->type)
	{
	case REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE:
	  nml->write_with_priority (*temp,_req->priority);
	  break;

	case REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE:
	  nml->write_with_bitwise_op(*temp, (enum NML_BITWISE_OP_TYPE) _req->bitwise_op);
	  break;

	case REMOTE_CMS_WRITE_REQUEST_TYPE:
	default:
	  nml->write (*temp);
	  break;
	}

      break;
    case CMS_WRITE_IF_READ_ACCESS:
      nml->write_if_read (*temp);
      break;
    default:
      rcs_print_error ("NML_SERVER: Invalid Access type. (%d)\n",
		       _req->access_type);
      break;
    }

  write_reply_ptr->status = (int) cms->status;
  write_reply_ptr->was_read = cms->header.was_read;
  write_reply_ptr->confirm_write = cms->confirm_write;

  if (_current_client_info)
    {
      if (_current_client_info->last_message_info.addr)
	{
	  long s = temp->size;
	  if (s > cms->size)
	    {
	      s = cms->size;
	    }
	  _current_client_info->last_message_info.size = s;
	  nml->copyMsg (nml->get_address (),
			_current_client_info->last_message_info.addr,
			cms->size);
	}
    }

  return (write_reply_ptr);
}


#ifndef __no_diag_unused_parameter__
#ifndef ENABLE_RCS_DIAG
#define __no_diag_unused_parameter__ __unused_parameter__
#else
#define __no_diag_unused_parameter__
#endif
#endif

REMOTE_SET_DIAG_INFO_REPLY *
NML_SERVER_LOCAL_PORT::set_diag_info (
				      __unused_parameter__ CMS_CLIENT_INFO *,
#ifdef ENABLE_RCS_DIAG
				      REMOTE_SET_DIAG_INFO_REQUEST * _req
#else
				      __unused_parameter__ REMOTE_SET_DIAG_INFO_REQUEST *
#endif
				      )
{
#ifdef ENABLE_RCS_DIAG
  if (NULL == _req)
    {
      return (NULL);
    }
  CMS_DIAG_PROC_INFO *dpi = cms->get_diag_proc_info ();
  if (NULL == dpi)
    {
      return (NULL);
    }
  if (orig_info == NULL)
    {
      orig_info = new CMS_DIAG_PROC_INFO ();
      *((CMS_DIAG_PROC_INFO*)orig_info) = *dpi;
    }
  strncpy (dpi->name, _req->process_name, 16);
  strncpy (dpi->host_sysinfo, _req->host_sysinfo, 32);
  if (cms->total_connections > _req->c_num && _req->c_num >= 0)
    {
      cms->connection_number = _req->c_num;
    }
  if (NULL != cms->handle_to_global_data)
    {
      cms->handle_to_global_data->set_total_bytes_moved(_req->bytes_moved);
    }
  dpi->pid = _req->pid;
  dpi->rcslib_ver = _req->rcslib_ver;
  cms->set_diag_proc_info (dpi);
  return (NULL);
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
  return 0;
#endif
}

REMOTE_GET_DIAG_INFO_REPLY *
NML_SERVER_LOCAL_PORT::get_diag_info (
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_DIAG_INFO_REQUEST *
				      )
{
#ifdef ENABLE_RCS_DIAG
  get_diag_info_reply_ptr->cdi = cms->get_diagnostics_info ();
  get_diag_info_reply_ptr->status = cms->status;
  return (get_diag_info_reply_ptr);
#else
  rcs_print_error("DIAG support was not selected when the RCS library was compiled.\n");
  return(0);
#endif
}

REMOTE_GET_MSG_COUNT_REPLY *
NML_SERVER_LOCAL_PORT::get_msg_count (
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_MSG_COUNT_REQUEST *
				      )
{
  return (NULL);
}


REMOTE_GET_MSG_TYPE_REPLY *
NML_SERVER_LOCAL_PORT::get_msg_type (
				      __unused_parameter__ CMS_CLIENT_INFO *,
				      __unused_parameter__ REMOTE_GET_MSG_TYPE_REQUEST *
				      )
{
  if ((NULL == cms) || (NULL == nml))
    {
      rcs_print_error ("NMLserver:reader: CMS object is NULL.\n");
      return ((REMOTE_GET_MSG_TYPE_REPLY *) NULL);
    }
  if(0 == get_msg_type_reply_ptr)
    {
      get_msg_type_reply_ptr = new REMOTE_GET_MSG_TYPE_REPLY();
    }

  NMLTYPE t = nml->get_msg_type();
  get_msg_type_reply_ptr->msg_type = t;


  /* Reply structure contains the latest shared memory info-- now
     return it to cms_dispatch  for return to caller */
  return (get_msg_type_reply_ptr);
}

void
NML_SERVER_LOCAL_PORT::reset_diag_info (
					__unused_parameter__ CMS_CLIENT_INFO *)
{
#ifdef ENABLE_RCS_DIAG
  if (NULL != orig_info)
    {
      CMS_DIAG_PROC_INFO *dpi = cms->get_diag_proc_info ();
      *dpi = *orig_info;
      cms->set_diag_proc_info (dpi);
    }
#endif
}

NML_SUPER_SERVER *NML_Default_Super_Server =
  (NML_SUPER_SERVER *) NULL;

NML_SUPER_SERVER::NML_SUPER_SERVER ():
  servers(0),unspawned_servers(0)
{
  servers = (RCS_LINKED_LIST *) NULL;
  unspawned_servers = 0;

  servers = new RCS_LINKED_LIST;
}

NML_SUPER_SERVER::~NML_SUPER_SERVER ()
{
  kill_all_servers ();
  delete_all_servers ();
  if (NULL != servers)
    {
      delete servers;
      servers = (RCS_LINKED_LIST *) NULL;
    }
  unspawned_servers=0;
}

void
NML_SUPER_SERVER::add_to_list (NML * _nml)
{
  NML_SERVER *server = (NML_SERVER *) NULL;
  NML_SERVER_LOCAL_PORT *local_port = (NML_SERVER_LOCAL_PORT *) NULL;
  NML *new_nml = (NML *) NULL;

  if (NULL != servers)
    {
      server = (NML_SERVER *) servers->get_head ();
      while (NULL != server)
	{
	  if (server->accept_local_port_cms (_nml->cms))
	    {
	      break;
	    }
	  server = (NML_SERVER *) servers->get_next ();
	}
      if (NULL == server)
	{
	  server = new NML_SERVER (_nml);
	  if (NULL == server)
	    {
	      rcs_print_error
		("NML_SERVER: Unable to create server object.\n");
	    }
	}
      else
	{
	  if (_nml->cms->isserver)
	    {
	      new_nml = _nml;
	      local_port = new NML_SERVER_LOCAL_PORT (new_nml);
	      local_port->local_channel_reused = 1;
	    }
	  else
	    {
	      new_nml = new NML (_nml, 1, -1);
	      local_port = new NML_SERVER_LOCAL_PORT (new_nml);
	      local_port->local_channel_reused = 0;
	    }
	  if (NULL == local_port)
	    {
	      rcs_print_error ("NML_SERVER: Unable to create local port.\n");
	      return;
	    }
	  server->add_local_port (local_port);
	}
    }
}


void
NML_SUPER_SERVER::remove_from_list (NML * _nml)
{
  NML_SERVER *server = (NML_SERVER *) NULL;
  NML_SERVER_LOCAL_PORT *local_port = (NML_SERVER_LOCAL_PORT *) NULL;
  NML *new_nml = (NML *) NULL;

  if (NULL != servers)
    {
      server = (NML_SERVER *) servers->get_head ();
      while (NULL != server)
	{
	  server->remove_local_port_cms(_nml->cms);
	  server = (NML_SERVER *) servers->get_next ();
	}
      if (NULL == server)
	{
	  server = new NML_SERVER (_nml);
	  if (NULL == server)
	    {
	      rcs_print_error
		("NML_SERVER: Unable to create server object.\n");
	    }
	}
      else
	{
	  if (_nml->cms->isserver)
	    {
	      new_nml = _nml;
	      local_port = new NML_SERVER_LOCAL_PORT (new_nml);
	      local_port->local_channel_reused = 1;
	    }
	  else
	    {
	      new_nml = new NML (_nml, 1, -1);
	      local_port = new NML_SERVER_LOCAL_PORT (new_nml);
	      local_port->local_channel_reused = 0;
	    }
	  if (NULL == local_port)
	    {
	      rcs_print_error ("NML_SERVER: Unable to create local port.\n");
	      return;
	    }
	  server->add_local_port (local_port);
	}
    }
}

void
NML_SUPER_SERVER::add_to_list (NML_SERVER * _server)
{
  if ((NULL != servers) && (NULL != _server))
    {
      _server->super_server_list_id
	= servers->store_at_tail (_server, sizeof (NML_SERVER), 0);
      unspawned_servers++;
    }
}

void
NML_SUPER_SERVER::spawn_all_servers ()
{
  NML_SERVER *server;

  if (NULL != servers)
    {
      server = (NML_SERVER *) servers->get_head ();
      while (NULL != server)
	{
	  if (server->spawn () > 0 && unspawned_servers > 0)
	    {
	      unspawned_servers--;
	    }
	  server = (NML_SERVER *) servers->get_next ();
	}
    }
}

void
NML_SUPER_SERVER::kill_all_servers ()
{
  NML_SERVER *server;

  if (NULL != servers)
    {
      server = (NML_SERVER *) servers->get_head ();
      while (NULL != server)
	{
	  if(!server->current_pid_equals_spawner_pid())
	    {
	      server = (NML_SERVER *) servers->get_next ();
	      continue;
	    }
	  if (server->server_spawned)
	    {
	      server->kill_server ();
	    }
	  server = (NML_SERVER *) servers->get_next ();
	}
    }
}

void
NML_SUPER_SERVER::delete_all_servers ()
{
  NML_SERVER *server;
  if (NULL != servers)
    {
      server = (NML_SERVER *) servers->get_head ();
      while (NULL != server)
	{
	  if((server->server_spawned &&
	      server->current_pid_equals_spawner_pid()) ||
	     !server->server_spawned)
	    {
	      server = (NML_SERVER *) servers->get_next ();
	      continue;
	    }
	  if (!server->server_spawned && unspawned_servers > 0)
	    {
	      unspawned_servers--;
	    }
	  delete server;
	  server = (NML_SERVER *) servers->get_next ();
	}
    }
}


int nml_control_C_caught = 0;
int nml_sigint_count = 0;
int dont_kill_servers = 0;
int dont_cleanup_servers = 0;
static int nmlsrv_last_sig = 0;

#if defined(WIN32) && !defined(gnuwin32)
static int __stdcall
catch_control_C1 (unsigned long sig)
#else
static void
catch_control_C1 (int sig)
#endif
{
  nmlsrv_last_sig = sig;
  nml_sigint_count++;
#ifndef WIN32
  signal (SIGINT, SIG_DFL);
#endif
  if (NULL != NML_Default_Super_Server)
    {
#ifdef VXWORKS
      NML_Default_Super_Server->delete_all_servers ();
      if (NULL != NML_Default_Super_Server->servers)
	{
	  if (0 == NML_Default_Super_Server->servers->list_size)
	    {
#endif
	      delete NML_Default_Super_Server;
	      NML_Default_Super_Server = (NML_SUPER_SERVER *) NULL;
#ifdef VXWORKS
	    }
	}
#endif
    }
#ifdef VXWORKS
  taskLock ();
#endif
  dont_kill_servers = 1;
  dont_cleanup_servers = 1;
  nml_cleanup ();
  dont_kill_servers = 0;
  dont_cleanup_servers = 0;
#ifdef VXWORKS
  taskUnlock ();
#endif
#ifdef HAVE_RAISE
  raise(9);
#endif
  exit (0);
#ifdef WIN32
  return (0);			// Just to please the compiler.
#endif
}				/*  */

#if defined(WIN32) && !defined(gnuwin32)
static int __stdcall
catch_control_C2 (unsigned long sig)
#else
static void
catch_control_C2 (int sig)
#endif
{
  nmlsrv_last_sig = sig;
#ifndef WIN32
  signal (SIGINT, SIG_DFL);
#endif
  nml_control_C_caught = 1;
#if defined(WIN32) && !defined(gnuwin32)
  return (0);
#endif
}

void
run_nml_server_exit (int i)
{
#if defined(_WINDOWS) && !defined(gnuwin32)
  MessageBox (NULL, "Exiting run_nml_servers().", "NML Message", MB_OK);
#endif
  exit (i);
}


void
run_nml_servers_with_func (svr_start_func _f)
{
  cms_svr_sfunc = _f;
  run_nml_servers();
}

void
run_nml_servers ()
{
  NML *nml=0;
  if (NULL != NML_Main_Channel_List)
    {
      nml = (NML *) NML_Main_Channel_List->get_head ();
      while(nml)
	{
	  if(!nml->already_deleted && 
	     !nml->immediate_spawned_server &&
	     nml->cms && !nml->cms->isserver &&
	     nml->cms->spawn_server < 1)
	    {
	      rcs_print_error("run_nml_servers() called but %s is not configured as a server for %s.\n",nml->cms->ProcessName, nml->cms->BufferName);
	    }
	  nml = (NML *) NML_Main_Channel_List->get_next ();
	}
    }
  if (NULL != NML_Default_Super_Server)
    {
      if (NML_Default_Super_Server->servers != NULL)
	{
	  if (NML_Default_Super_Server->servers->list_size <
	      NML_Default_Super_Server->unspawned_servers)
	    {
	      NML_Default_Super_Server->unspawned_servers =
		NML_Default_Super_Server->servers->list_size;
	    }
	  if (NML_Default_Super_Server->unspawned_servers <= 0)
	    {
	      rcs_print_error
		("run_nml_servers(): No buffers without servers already spawned for them.\n");
	      return;
	    }
	  if (NML_Default_Super_Server->unspawned_servers == 1)
	    {
	      NML_Default_Super_Server->unspawned_servers = 0;
	      NML_SERVER *sole_server;
	      sole_server =
		(NML_SERVER *) NML_Default_Super_Server->servers->get_head ();
	      while (sole_server != NULL)
		{
		  if (NULL != sole_server->remote_port)
		    {
		      if (!sole_server->remote_port->running &&
			  !sole_server->server_spawned)
			{
			  break;
			}
		    }
		  sole_server =
		    (NML_SERVER *) NML_Default_Super_Server->servers->
		    get_next ();
		}
	      if (NULL == sole_server)
		{
		  rcs_print_error
		    ("run_nml_servers() : sole_server is NULL.\n");
		  run_nml_server_exit (-1);
		}
	      else
		{
#if defined(WIN32) && !defined(gnuwin32)
		  SetConsoleCtrlHandler (catch_control_C1, TRUE);
#else
		  signal (SIGINT, catch_control_C1);
#endif
		  sole_server->run (0);
		  run_nml_server_exit (-1);
		}
	    }
	  else
	    {
	      nml_control_C_caught = 0;
	      NML_Default_Super_Server->spawn_all_servers ();

#if defined(WIN32) && !defined(gnuwin32)
	      SetConsoleCtrlHandler (catch_control_C2, TRUE);
#else
	      signal (SIGINT, catch_control_C2);
#endif
	      while (!nml_control_C_caught)
		esleep (2.0);
	      NML_Default_Super_Server->kill_all_servers ();
	      nml_cleanup ();
	      run_nml_server_exit (0);
	    }
	}
      else
	{
	  rcs_print_error
	    ("run_nml_servers(): No buffers without servers already spawned for them.\n");
	}
    }
  else
    {
      rcs_print_error
	("run_nml_servers(): No buffers without servers already spawned for them.\n");
    }
  run_nml_server_exit (-1);
}

void
spawn_nml_servers ()
{
  if (NULL != NML_Default_Super_Server)
    {
      NML_Default_Super_Server->spawn_all_servers ();
    }
}

void
kill_nml_servers ()
{
  if (!dont_kill_servers)
    {
      if (NULL != NML_Default_Super_Server)
	{
	  NML_Default_Super_Server->kill_all_servers ();
	}
    }
}

void
nml_server_cleanup ()
{
  cms_svr_sfunc=0;
  if (!dont_cleanup_servers)
    {
      if (NULL != NML_Default_Super_Server)
	{
	  NML_Default_Super_Server->kill_all_servers ();
	  NML_Default_Super_Server->delete_all_servers ();
#ifdef VXWORKS
	  if (NULL != NML_Default_Super_Server->servers)
	    {
	      if (0 == NML_Default_Super_Server->servers->list_size)
		{
#endif
		  delete NML_Default_Super_Server;
		  NML_Default_Super_Server = (NML_SUPER_SERVER *) NULL;
#ifdef VXWORKS
		}
	    }
#endif
	}
    }
  cms_svr_sfunc=0;
}

void
NML_SERVER::list_cleanup()
{
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() called.\n");
  NML_SERVER_LOCAL_PORT *local_port;
  RCS_LINKED_LIST *old_list_of_nmls;
  old_list_of_nmls= new RCS_LINKED_LIST();
  RCS_LINKED_LIST *clp = cms_local_ports;
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);
  local_port = (NML_SERVER_LOCAL_PORT *) clp->get_head ();
  while (NULL != local_port)
    {
      NML *nml_for_old_list = local_port->nml;
      old_list_of_nmls->store_at_tail(nml_for_old_list,sizeof(NML),0);
      local_port = (NML_SERVER_LOCAL_PORT *) clp->get_next ();
    }
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);
  nml_remove_all_not_on_list(old_list_of_nmls);
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);
  local_port = (NML_SERVER_LOCAL_PORT *) clp->get_head ();
  while (NULL != local_port)
    {
      rcs_print_debug(PRINT_MISC,
		      "NML_SERVER::list_cleanup() this=%p,ptinfo=%p\n",
		      (void*)this,(void*)ptinfo);
      rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() local_port=%p.\n",
		      (void*)local_port);
      NML *oldnml = local_port->nml;
      if(!oldnml)
	{
	  local_port = (NML_SERVER_LOCAL_PORT *) clp->get_next ();
	  continue;
	}
      local_port->nml = 0;
      local_port->cms = 0;
      NML *newnml = new NML(oldnml,1,-1);
      local_port->nml = newnml;
      local_port->cms = newnml->cms;
      if(local_port->cms)
	{
	  local_port->enable_xml_differencing = local_port->cms->enable_xml_differencing;
	  local_port->multireader_priority_queue_enabled = local_port->cms->multireader_priority_queue_enabled;
	  local_port->buffer_number = local_port->cms->buffer_number;
	}
      oldnml->set_leave_resource(true);
      delete oldnml;
      local_port = (NML_SERVER_LOCAL_PORT *) clp->get_next ();
    }
  delete old_list_of_nmls;
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() this=%p,ptinfo=%p\n",
		  (void*)this,(void*)ptinfo);
  rcs_print_debug(PRINT_MISC,"NML_SERVER::list_cleanup() returning.\n");
}

void
NML_SERVER::delete_all_local_ports_preserving_resources()
{
  NML_SERVER_LOCAL_PORT *local_port;
  RCS_LINKED_LIST *clp = cms_local_ports;
  cms_local_ports=0;
  local_port = (NML_SERVER_LOCAL_PORT *) clp->get_head ();
  while (NULL != local_port)
    {
      local_port->nml = 0;
      local_port->cms = 0;
      delete local_port;
      clp->delete_current_node();
      local_port = (NML_SERVER_LOCAL_PORT *) clp->get_next ();
    }
  delete clp;
}

class NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO
{
public:
  NML *nml;
  NML_SERVER_LOCAL_PORT *nslp;
  NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO();
  ~NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO();

private:
  NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO(const NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO &);
  NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO &operator=(const NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO &);
};

NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO::NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO():
  nml(0),nslp(0)
{
  nml=0;
  nslp=0;
}

NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO::~NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO()
{
  rcs_print_debug(PRINT_MISC,
		  "NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO::~NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO() called. (this=%p,nml=%p,nslp=%p)\n",
		  (void*)this,(void*)nml,(void*)nslp);

  if(nml)
    {
      delete nml;
      nml=0;
    }
  nslp=0;
}

NML_SERVER_LOCAL_WAITING_OBJECT::NML_SERVER_LOCAL_WAITING_OBJECT(NML_SERVER_LOCAL_PORT *_nslp):
  private_info(0)
{
  private_info = new NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO();
  rcs_print_debug(PRINT_MISC,
		  "NML_SERVER_LOCAL_WAITING_OBJECT::NML_SERVER_LOCAL_WAITING_OBJECT(%p) called. (this=%p,private_info=%p)\n",
		  (void*)_nslp,(void*)this,(void*)private_info);
  if(private_info)
    {
      private_info->nslp = _nslp;
      if(private_info->nslp)
      {
	private_info->nml = new NML(_nslp->nml,1,-1);
      }
    }
}

NML_SERVER_LOCAL_WAITING_OBJECT::~NML_SERVER_LOCAL_WAITING_OBJECT()
{
  rcs_print_debug(PRINT_MISC,
		  "NML_SERVER_LOCAL_WAITING_OBJECT::~NML_SERVER_LOCAL_WAITING_OBJECT() called. (this=%p,private_info=%p)\n",
		  (void*)this,(void*)private_info);
  if(private_info)
    {
      delete private_info;
      private_info=0;
    }
}

int
NML_SERVER_LOCAL_WAITING_OBJECT::wait_for_anything(void)
{
  if(private_info && private_info->nml)
    {
      return private_info->nml->wait_for_anything(-1.0);
    }
  return -1;
}

int
NML_SERVER_LOCAL_WAITING_OBJECT::valid(void)
{
  if(private_info && private_info->nml)
    {
      return private_info->nml->valid();
    }
  return 0;
}

CMS_SERVER_LOCAL_WAITING_OBJECT *
NML_SERVER_LOCAL_PORT::get_new_local_waiting_object(void)
{
  NML_SERVER_LOCAL_WAITING_OBJECT *nlwo =
    new NML_SERVER_LOCAL_WAITING_OBJECT(this);
  if(!nlwo->valid())
    {
      delete nlwo;
      return 0;
    }
  return (CMS_SERVER_LOCAL_WAITING_OBJECT *) nlwo;
}  

//  defined(ENABLE_RCS_SERVER)

#else
#include "rcs_empty_source"
#endif

 
