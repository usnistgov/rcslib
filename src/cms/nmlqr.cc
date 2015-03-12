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


#if HAVE_CONFIG_H
#include "rcs_config.h"
#endif

#if defined(ENABLE_RCS_NMLQR)

#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>

#ifdef EXTERN_C_STD_HEADERS
}
#endif
#endif

#include "cms.hh"		// class CMS
#include "timer.hh"		// esleep()
#include "nmlqr.hh"
#include "dbg_mem.h"		// DEBUG_MALLOC, DEBUG_FREE
#include "linklist.hh"		// class RCS_LINKED_LIST
#include "rcs_prnt.hh"		// rcs_print_debug

#define ID_REQUEST_TYPE 9991
#define ID_REPLY_TYPE 9992
#define ID_DELETE_TYPE 9993

class NML_QUERY_CHANNEL:public NML
{
public:
  NML_QUERY_CHANNEL (NML_FORMAT_PTR, char *, char *, char *,
		     int set_to_server = 0);
  ~NML_QUERY_CHANNEL ();

  NML_QUERY_MSG *get_address ()
  {
    return ((NML_QUERY_MSG *) NML::get_address ());
  };

};

class NML_ID_CHANNEL:public NML
{
public:
  NML_ID_CHANNEL (NML_FORMAT_PTR, char *, char *, char *,
		  int set_to_server = 0);
  ~NML_ID_CHANNEL ();
};



class ID_REQUEST:public NML_QUERY_MSG 
{
public:
  ID_REQUEST ():NML_QUERY_MSG (ID_REQUEST_TYPE, sizeof (ID_REQUEST))
  {
  };
  void update (CMS *);
};

void
ID_REQUEST::update (CMS * cms)
{
  cms->beginBaseClass("NML_QUERY_MSG");
  NML_QUERY_MSG::update(cms);
  cms->endBaseClass("NML_QUERY_MSG");
  
  cms->beginClass ("ID_REQUEST", 0);
  cms->endClass ("ID_REQUEST", 0);
}


class ID_REPLY:public NMLmsg
{
public:
  ID_REPLY ():
    NMLmsg(ID_REPLY_TYPE, sizeof (ID_REPLY)),
    subdiv(0)
  {
  };
  void update (CMS *);
  int subdiv;
};

void
ID_REPLY::update (CMS * cms)
{
  cms->beginClass ("ID_REPLY", 0);
  cms->update_with_name ("subdiv", subdiv);
  cms->endClass ("ID_REPLY", 0);
  rcs_print_debug(PRINT_MISC,"ID_REPLY::update() subdiv=%d\n",
		  subdiv);
}


class ID_DELETE:public NML_QUERY_MSG
{
public:
  ID_DELETE ():
    NML_QUERY_MSG (ID_DELETE_TYPE, sizeof (ID_DELETE)),
    subdiv(0)
  {
  };
  void update (CMS *);
  int subdiv;
};

void
ID_DELETE::update (CMS * cms)
{
  cms->beginBaseClass("NML_QUERY_MSG");
  NML_QUERY_MSG::update(cms);
  cms->endBaseClass("NML_QUERY_MSG");

  cms->beginClass ("ID_DELETE", 0);
  cms->update_with_name ("subdiv", subdiv);
  cms->endClass ("ID_DELETE", 0);
}

void
NML_QUERY_MSG::update (CMS * cms)
{
  cms->beginClass ("NML_QUERY_MESSAGE", 0);
  cms->update_with_name ("subdiv_for_reply", subdiv_for_reply);
  cms->endClass ("NML_QUERY_MESSAGE", 0);
  rcs_print_debug(PRINT_MISC,"NML_QUERY_MESSAGE::update() subdiv_for_reply=%d\n",
		  subdiv_for_reply);
}

int
queryFormat (NMLTYPE type, void *buf, CMS * cms)
{
  rcs_print_debug(PRINT_MISC,"idFormat(%ld,buf=%p,cms=%p)\n",
		  (long) type,buf,(void*)cms);
  ((NML_QUERY_MSG *) buf)->update (cms);
  return 0;
}


int
idFormat (NMLTYPE type, void *buf, CMS * cms)
{
  rcs_print_debug(PRINT_MISC,"idFormat(%ld,buf=%p,cms=%p)\n",
		  (long) type,buf,(void*)cms);
  //queryFormat (type, buf, cms);
  switch (type)
    {
    case ID_REQUEST_TYPE:
      ((ID_REQUEST *) buf)->ID_REQUEST::update (cms);
      break;

    case ID_REPLY_TYPE:
      ((ID_REPLY *) buf)->ID_REPLY::update (cms);
      break;

    case ID_DELETE_TYPE:
      ((ID_DELETE *) buf)->ID_DELETE::update (cms);
      break;

    default:
      return 0;
    }
  return 1;
}


NML_QR_SERVER::NML_QR_SERVER (NML_FORMAT_PTR f_ptr,
			      char *qr_name, char *process_name,
			      char *config_file):
  replyChannel(0),
  queryChannel(0),
  idChannel(0),
  reply_subdiv(0),
  subdiv_allocation_table(0)
{

  queryChannel = NULL;
  replyChannel = NULL;
  idChannel = NULL;
  reply_subdiv = -1;
  subdiv_allocation_table = NULL;

  char repbufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(repbufname,sizeof(repbufname)),
		  "%sReply", qr_name);
  char querybufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(querybufname,sizeof(querybufname)),
		  "%sQuery", qr_name);
  char idbufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(idbufname,sizeof(idbufname)),
		  "%sID", qr_name);
  idChannel =
    new NML_ID_CHANNEL (f_ptr, idbufname, process_name, config_file);
  queryChannel =
    new NML_QUERY_CHANNEL (f_ptr, querybufname, process_name, config_file);
  replyChannel = new NML (f_ptr, repbufname, process_name, config_file);

  if (replyChannel->get_total_subdivisions () > 1)
    {
      subdiv_allocation_table =
	(char *) DEBUG_MALLOC (replyChannel->get_total_subdivisions ());
    }
  memset (subdiv_allocation_table, 0,
	  replyChannel->get_total_subdivisions ());
}


NML_QR_SERVER::~NML_QR_SERVER ()
{
  if (NULL != queryChannel)
    {
      delete queryChannel;
      queryChannel = NULL;
    }
  if (NULL != idChannel)
    {
      delete idChannel;
      idChannel = NULL;
    }
  if (NULL != replyChannel)
    {
      delete replyChannel;
      replyChannel = NULL;
    }
  if (NULL != subdiv_allocation_table)
    {
      DEBUG_FREE (subdiv_allocation_table);
      subdiv_allocation_table = NULL;
    }
}

NMLTYPE NML_QR_SERVER::readQuery ()
{
  while (1)
    {
      NMLTYPE
	query_read_type;
      switch (query_read_type = queryChannel->read ())
	{
	case ID_REQUEST_TYPE:
	  {
	    int
	      subdiv_found =
	      0;
	    for (int i = 0; i < replyChannel->get_total_subdivisions (); i++)
	      {
		if (subdiv_allocation_table[i] == 0)
		  {
		    subdiv_allocation_table[i] = 1;
		    ID_REPLY
		      idMsg;
		    idMsg.subdiv = i;
		    idChannel->write (idMsg);
		    subdiv_found = 1;
		    break;
		  }
	      }
	    if (!subdiv_found)
	      {
		// Send the message we are full.
		ID_REPLY
		  idMsg;
		idMsg.subdiv = -1;
		idChannel->write (idMsg);
	      }
	  }
	  break;

	case ID_REPLY_TYPE:
	  return -1;		// Very weird a reply message in the query buffer.
	  break;

	case ID_DELETE_TYPE:
	  {
	    ID_DELETE *
	      idDeleteMsg = (ID_DELETE *)
	      queryChannel->
	      get_address ();
	    if (idDeleteMsg->subdiv >= 0
		&& idDeleteMsg->subdiv <
		replyChannel->get_total_subdivisions ())
	      {
		subdiv_allocation_table[idDeleteMsg->subdiv] = 0;
	      }
	  }
	  break;

	default:
	  NML_QUERY_MSG * qMsg =
	    (NML_QUERY_MSG *) queryChannel->get_address ();
	  reply_subdiv = qMsg->subdiv_for_reply;
	  return query_read_type;
	}
    }
}

NMLTYPE NML_QR_SERVER::waitForQuery (double timeout)
{
  while (1)
    {
      NMLTYPE
	query_read_type;
      switch (query_read_type = queryChannel->blocking_read (timeout))
	{
	case -1:
	  rcs_print_error("NML_QR_SERVER::waitForQuery() comm error from query channel");
	  return(-1);

	case 0:
	  if(timeout < -0.001)
	    {
	      rcs_print_error("NML_QR_SERVER::waitForQuery() blocking_read returned 0 with infinite timeout.\n");
	      return -1;
	    }
	  break;

	case ID_REQUEST_TYPE:
	  {
	    int
	      subdiv_found =
	      0;
	    for (int i = 0; i < replyChannel->get_total_subdivisions (); i++)
	      {
		if (subdiv_allocation_table[i] == 0)
		  {
		    subdiv_allocation_table[i] = 1;
		    ID_REPLY
		      idMsg;
		    idMsg.subdiv = i;
		    idChannel->write (idMsg);
		    subdiv_found = 1;
		    break;
		  }
	      }
	    if (!subdiv_found)
	      {
		// Send the message we are full.
		ID_REPLY
		  idMsg;
		idMsg.subdiv = -1;
		idChannel->write (idMsg);
	      }
	  }
	  break;

	case ID_REPLY_TYPE:
	  // Very weird a reply message in the query buffer.
	  rcs_print_error("ID_REPLY_TYPE written to query buffer.\n");
	  return -1;		

	case ID_DELETE_TYPE:
	  {
	    ID_DELETE *
	      idDeleteMsg = (ID_DELETE *)
	      queryChannel->
	      get_address ();
	    if (idDeleteMsg->subdiv >= 0
		&& idDeleteMsg->subdiv <
		replyChannel->get_total_subdivisions ())
	      {
		subdiv_allocation_table[idDeleteMsg->subdiv] = 0;
	      }
	  }
	  break;

	default:
	  NML_QUERY_MSG * qMsg =
	    (NML_QUERY_MSG *) queryChannel->get_address ();
	  reply_subdiv = qMsg->subdiv_for_reply;
	  return query_read_type;
	}
    }
}

NML_QUERY_MSG *
NML_QR_SERVER::getQueryAddress ()
{
  if (NULL == queryChannel)
    {
      return NULL;
    }
  return (NML_QUERY_MSG *) queryChannel->get_address ();
}


int
NML_QR_SERVER::replyToLastQuery (NMLmsg * message_to_send)
{
  if (NULL == replyChannel)
    {
      return -1;
    }
  if (reply_subdiv < 0
      || reply_subdiv > replyChannel->get_total_subdivisions ())
    {
      return -1;
    }
  return replyChannel->write_subdivision (reply_subdiv, message_to_send);
}


int
NML_QR_SERVER::valid ()
{
  if (!idChannel->valid ())
    {
      return 0;
    }
  if (!queryChannel->valid ())
    {
      return 0;
    }
  if (!replyChannel->valid ())
    {
      return 0;
    }
  return 1;
}

int
NML_QR_SERVER::reset ()
{
  if (!idChannel->valid ())
    {
      idChannel->reset ();
    }
  if (!queryChannel->valid ())
    {
      queryChannel->reset ();
    }
  if (!replyChannel->valid ())
    {
      replyChannel->reset ();
    }
  return valid ();
}



NML_QR_CLIENT::NML_QR_CLIENT (NML_FORMAT_PTR f_ptr, char *qr_name,
			      char *process_name, char *config_file):
  replyChannel(0),
  queryChannel(0),
  idChannel(0),
  reply_subdiv(0)
{
  queryChannel = NULL;
  replyChannel = NULL;
  idChannel = NULL;
  reply_subdiv = -1;

  char repbufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(repbufname,sizeof(repbufname)),
		  "%sReply", qr_name);
  char querybufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(querybufname,sizeof(querybufname)),
		  "%sQuery", qr_name);
  char idbufname[40];
  SNPRINTF_FUNC ( SNPRINTF_ARGS(idbufname,sizeof(idbufname)),
		  "%sID", qr_name);
  idChannel =
    new NML_ID_CHANNEL (f_ptr, idbufname, process_name, config_file);
  queryChannel =
    new NML_QUERY_CHANNEL (f_ptr, querybufname, process_name, config_file);
  replyChannel = new NML (f_ptr, repbufname, process_name, config_file);

  if (idChannel->valid () && queryChannel->valid ())
    {
      ID_REQUEST reqMsg;
      queryChannel->write (reqMsg);
      esleep (0.05);
      while (!idChannel->read ())
	{
	  esleep (0.05);
	}
      ID_REPLY *idReply = (ID_REPLY *) idChannel->get_address ();
      if (idReply->type == ID_REPLY_TYPE)
	{
	  reply_subdiv = idReply->subdiv;
	}
    }
}

NML_QR_CLIENT::~NML_QR_CLIENT ()
{
  if (NULL != queryChannel)
    {
      if (reply_subdiv >= 0)
	{
	  ID_DELETE idDeleteMsg;
	  idDeleteMsg.subdiv_for_reply = reply_subdiv;
	  idDeleteMsg.subdiv = reply_subdiv;
	  queryChannel->write (&idDeleteMsg);
	  esleep (0.05);
	}
      delete queryChannel;
      queryChannel = NULL;
    }
  if (NULL != idChannel)
    {
      delete idChannel;
      idChannel = NULL;
    }
  if (NULL != replyChannel)
    {
      delete replyChannel;
      replyChannel = NULL;
    }
}

int
NML_QR_CLIENT::sendQuery (NML_QUERY_MSG * qMsg)
{
  if (NULL == queryChannel)
    {
      return -1;
    }
  qMsg->subdiv_for_reply = reply_subdiv;
  return queryChannel->write (qMsg);
}


NMLTYPE NML_QR_CLIENT::readReply ()
{
  if (NULL == replyChannel || reply_subdiv < 0)
    {
      return -1;
    }
  return replyChannel->read_subdivision (reply_subdiv);
}


NMLTYPE NML_QR_CLIENT::waitForReply (double timeout)
{
  if (NULL == replyChannel || reply_subdiv < 0)
    {
      return -1;
    }
  return replyChannel->blocking_read_subdivision (reply_subdiv, timeout);
}

NMLmsg *
NML_QR_CLIENT::getReplyAddress ()
{
  if (NULL == replyChannel || reply_subdiv < 0)
    {
      return NULL;
    }
  return replyChannel->get_address_subdivision (reply_subdiv);
}


int
NML_QR_CLIENT::valid ()
{
  if (!idChannel->valid ())
    {
      return 0;
    }
  if (!queryChannel->valid ())
    {
      return 0;
    }
  if (!replyChannel->valid ())
    {
      return 0;
    }
  if (reply_subdiv < 0)
    {
      return 0;
    }
  return 1;
}

int
NML_QR_CLIENT::reset ()
{
  if (!idChannel->valid ())
    {
      idChannel->reset ();
    }
  if (!queryChannel->valid ())
    {
      queryChannel->reset ();
    }
  if (!replyChannel->valid ())
    {
      replyChannel->reset ();
    }
  if (idChannel->valid () && queryChannel->valid () && reply_subdiv < 0)
    {
      ID_REQUEST reqMsg;
      queryChannel->write (reqMsg);
      esleep (0.05);
      while (!idChannel->read ())
	{
	  esleep (0.05);
	}
      ID_REPLY *idReply = (ID_REPLY *) idChannel->get_address ();
      if (idReply->type == ID_REPLY_TYPE)
	{
	  reply_subdiv = idReply->subdiv;
	}
    }
  return valid ();
}


NML_QUERY_CHANNEL::NML_QUERY_CHANNEL (NML_FORMAT_PTR f_ptr, char *name, char *process, char *file, int set_to_server):
NML (f_ptr,name, process, file, set_to_server)
{
  //prefix_format_chain (idFormat);
  channel_type = NML_QUERY_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NML_QUERY_MSG);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
    }

  register_with_server ();

}

NML_QUERY_CHANNEL::~NML_QUERY_CHANNEL ()
{
  // Something funny happens to gdb without this being explicitly defined.
}


NML_ID_CHANNEL::NML_ID_CHANNEL (
				__unused_parameter__ NML_FORMAT_PTR f_ptr, 
				char *name,
				char *process, char *file, int set_to_server):
NML (idFormat,name, process, file, set_to_server)
{
  //format_chain = new RCS_LINKED_LIST;
  //prefix_format_chain (idFormat);
  channel_type = NML_ID_CHANNEL_TYPE;
  sizeof_message_header = sizeof (NML_QUERY_MSG);

  if (NULL != cms)
    {
      cms->sizeof_message_header = sizeof_message_header;
    }

  register_with_server ();

}

NML_ID_CHANNEL::~NML_ID_CHANNEL ()
{
  // Something funny happens to gdb without this being explicitly defined.
}

void NML_QR_SERVER::interrupt_operation()
{
  if(replyChannel)
    {
      replyChannel->interrupt_operation();
    }
  if(queryChannel)
    {
      queryChannel->interrupt_operation();
    }
  if(idChannel)
    {
      idChannel->interrupt_operation();
    }
}

void NML_QR_SERVER::clear_interrupt_operation()
{
  if(replyChannel)
    {
      replyChannel->clear_interrupt_operation();
    }
  if(queryChannel)
    {
      queryChannel->clear_interrupt_operation();
    }
  if(idChannel)
    {
      idChannel->clear_interrupt_operation();
    }
}  

void NML_QR_CLIENT::interrupt_operation()
{
  if(replyChannel)
    {
      replyChannel->interrupt_operation();
    }
  if(queryChannel)
    {
      queryChannel->interrupt_operation();
    }
  if(idChannel)
    {
      idChannel->interrupt_operation();
    }
}

void NML_QR_CLIENT::clear_interrupt_operation()
{
  if(replyChannel)
    {
      replyChannel->clear_interrupt_operation();
    }
  if(queryChannel)
    {
      queryChannel->clear_interrupt_operation();
    }
  if(idChannel)
    {
      idChannel->clear_interrupt_operation();
    }
}  


NML_QR_SERVER::NML_QR_SERVER(
			     __unused_parameter__ const NML_QR_SERVER &_nml_qr_server):
  replyChannel(0),
  queryChannel(0),
  idChannel(0),
  reply_subdiv(0),
  subdiv_allocation_table(0)
{
  rcs_print_error("NML_QR_SERVER copy constructor should never be called.\n");
}

NML_QR_SERVER &
NML_QR_SERVER::operator=(const NML_QR_SERVER &)
{
  rcs_print_error("NML_QR_SERVER::operator= should never be called.\n");
  return(*this);
}

NML_QR_CLIENT::NML_QR_CLIENT(
			     __unused_parameter__ const NML_QR_CLIENT &_nml_qr_CLIENT):
  replyChannel(0),
  queryChannel(0),
  idChannel(0),
  reply_subdiv(0)
{
  rcs_print_error("NML_QR_CLIENT copy constructor should never be called.\n");
}

NML_QR_CLIENT &
NML_QR_CLIENT::operator=(const NML_QR_CLIENT &)
{
  rcs_print_error("NML_QR_CLIENT::operator= should never be called.\n");
  return(*this);
}

//  defined(ENABLE_RCS_NMLQR)
#else
#include "rcs_empty_source"
#endif
