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
* File:nml_srv.hh                                                        *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for RPC server that reads and writes          *
*          to a local NML buffer for remote processes.                   *
* Includes:                                                              *
*          1. class NML_SERVER                                           *
*************************************************************************/

#ifndef NML_SERVER_HH
#define NML_SERVER_HH

#include "cms_srv.hh"		/* class CMS_SERVER */

class NML;
class CMS_CLIENT_INFO;

struct REMOTE_READ_REQUEST;
struct REMOTE_READ_REPLY;
struct REMOTE_WRITE_REQUEST;
struct REMOTE_WRITE_REPLY;
struct REMOTE_SET_DIAG_INFO_REPLY;
struct REMOTE_SET_DIAG_INFO_REQUEST;
struct REMOTE_GET_DIAG_INFO_REPLY;
struct REMOTE_GET_DIAG_INFO_REQUEST;
struct REMOTE_GET_MSG_COUNT_REPLY;
struct REMOTE_GET_MSG_COUNT_REQUEST;

class NML_SERVER_LOCAL_WAITING_OBJECT;
class NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO;

/* Server class */
class NML_SERVER_LOCAL_PORT:public CMS_SERVER_LOCAL_PORT
{
protected:
  class NML * nml;
  class NML * nml_for_get_dvar;
  
  virtual double get_dvar(const char *dvar_name, int &id, long type, bool &got_dvar,bool read_new);

  REMOTE_READ_REPLY *reader (  CMS_CLIENT_INFO *_current_client_info,
			       REMOTE_READ_REQUEST * _req);
  REMOTE_READ_REPLY *blocking_read (  CMS_CLIENT_INFO *_current_client_info,
				      REMOTE_READ_REQUEST * _req);
  REMOTE_WRITE_REPLY *writer (  CMS_CLIENT_INFO *_current_client_info,
				REMOTE_WRITE_REQUEST * _req);
  REMOTE_SET_DIAG_INFO_REPLY *set_diag_info (  CMS_CLIENT_INFO *_current_client_info,
					       REMOTE_SET_DIAG_INFO_REQUEST *
					     buf);
  REMOTE_GET_DIAG_INFO_REPLY *get_diag_info (  CMS_CLIENT_INFO *_current_client_info,
					       REMOTE_GET_DIAG_INFO_REQUEST *
					     buf);
  REMOTE_GET_MSG_COUNT_REPLY *get_msg_count (  CMS_CLIENT_INFO *_current_client_info,
					       REMOTE_GET_MSG_COUNT_REQUEST *
					     buf);

  REMOTE_GET_MSG_TYPE_REPLY *get_msg_type (  CMS_CLIENT_INFO *_current_client_info,
					       REMOTE_GET_MSG_TYPE_REQUEST  *_req);

  virtual CMS_SERVER_LOCAL_WAITING_OBJECT *get_new_local_waiting_object(void);

  void reset_diag_info (  CMS_CLIENT_INFO *_current_client_info);

  friend class NML_SUPER_SERVER;
  friend class NML_SERVER;
  friend class NML_SERVER_LOCAL_WAITING_OBJECT;
  friend class NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO;

public:
    NML_SERVER_LOCAL_PORT (class NML * _nml);
    virtual ~ NML_SERVER_LOCAL_PORT ();

private:
  // Prevent copying.
  NML_SERVER_LOCAL_PORT(const NML_SERVER_LOCAL_PORT &);
  NML_SERVER_LOCAL_PORT &operator=(const NML_SERVER_LOCAL_PORT &);

};

class NML_SERVER:public CMS_SERVER
{
protected:
  int super_server_list_id;
  int being_deleted;

public:
  NML_SERVER (class NML * _nml, int set_to_master = 0);
  virtual ~ NML_SERVER ();
  virtual void list_cleanup(void);
  virtual void delete_all_local_ports_preserving_resources (void);
  void delete_from_list ();
  void add_to_nml_server_list ();
  friend class NML_SUPER_SERVER;

private:
  NML_SERVER(const NML_SERVER &);
  NML_SERVER &operator=(const NML_SERVER &);
};

class NML_SUPER_SERVER
{
public:
  RCS_LINKED_LIST * servers;
  NML_SUPER_SERVER ();
  ~NML_SUPER_SERVER ();
  void add_to_list (class NML *);
  void remove_from_list (class NML *);
  void add_to_list (NML_SERVER *);
  void spawn_all_servers ();
  void kill_all_servers ();
  void delete_all_servers ();
  int unspawned_servers;

private:
  //Prevent copying.
  NML_SUPER_SERVER(const NML_SUPER_SERVER &);
  NML_SUPER_SERVER &operator=(const NML_SUPER_SERVER &);
};

class NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO;

class NML_SERVER_LOCAL_WAITING_OBJECT: public CMS_SERVER_LOCAL_WAITING_OBJECT
{
protected:
  class NML_SERVER_LOCAL_WAITING_OBJECT_PRIVATE_INFO *private_info;
  
public:
  NML_SERVER_LOCAL_WAITING_OBJECT(NML_SERVER_LOCAL_PORT *_nslp);
  virtual ~NML_SERVER_LOCAL_WAITING_OBJECT();
  virtual int wait_for_anything(void);
  virtual int valid(void);

private:
  //Prevent copying
  NML_SERVER_LOCAL_WAITING_OBJECT(const NML_SERVER_LOCAL_WAITING_OBJECT &);
  NML_SERVER_LOCAL_WAITING_OBJECT &operator=(const NML_SERVER_LOCAL_WAITING_OBJECT &);

};
    
extern NML_SUPER_SERVER *NML_Default_Super_Server;
extern void run_nml_servers ();
extern void run_nml_servers_with_func (svr_start_func _f);

extern void spawn_nml_servers ();
extern void kill_nml_servers ();
extern "C"
{
  extern void nml_server_cleanup ();
}

#endif				/* !NML_SERVER_HH */



