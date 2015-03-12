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

#if defined(ENABLE_RCS_SERVER)


#if HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#endif

#include "rcs_prnt.hh"
#include "linklist.hh"
#include "cms_srv.hh"

void
cms_print_servers ()
{
  CMS_SERVER *cms_server;

  rcs_print ("cms_server_count=%d\n", cms_server_count);
  if (NULL == cms_server_list)
    {
      rcs_print ("cms_server_list is NULL.\n");
      return;
    }

  cms_server = (CMS_SERVER *) cms_server_list->get_head ();
  rcs_print ("CMS Server Tasks:\n");
  rcs_print
    ("\t server_pid, \tnum_buffers, \tport,\t max_clients,\t cur_clients,\t requests_processed\n");
  while (NULL != cms_server)
    {
      int num_buffers = 0;
      if (cms_server->cms_local_ports != NULL)
	{
	  num_buffers = cms_server->cms_local_ports->list_size;
	}
      int port_num = 0;
      int max_clients = 0;
      int current_clients = 0;
      int requests_processed = cms_server->requests_processed;
      if (cms_server->remote_port != NULL)
	{
	  port_num = cms_server->remote_port->port_num;
	  max_clients = cms_server->remote_port->max_clients;
	  current_clients = cms_server->remote_port->current_clients;
	}
      rcs_print (" \t%ld (0x%lX),\t %d,\t %d,\t %d,\t %d,\t %d\n",
		 cms_server->get_server_pid_long_int(), 
		 cms_server->get_server_pid_long_int(), 
		 num_buffers, port_num,
		 max_clients, current_clients, requests_processed);
      cms_server = (CMS_SERVER *) cms_server_list->get_next ();
    }
}

//  defined(ENABLE_RCS_SERVER)

#else
#include "rcs_empty_source"
#endif

