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


#ifndef TTY_SRV_HH
#define TTY_SRV_HH

#include "cms_srv.hh"		/* class CMS_SERVER_REMOTE_PORT */
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */
#include "sokintrf.h"		// fd_set, dl_sa, timeval
#include "ttyintf.h"
#include "rem_msg.hh"

#ifdef EXTERN_C_STD_HEADERS
extern "C"
{
#endif

#include <string.h>		/* memset(), strerror() */

#include <errno.h>		/* errno */
#include <signal.h>		// SIGPIPE, signal()

#ifdef EXTERN_C_STD_HEADERS
}
#endif

#include "dbg_mem.h"		/* DEBUG_FREE,DEBUG_MALLOC,DEBUG_CALLOC */

#define MAX_TTY_BUFFER_SIZE 16
class CLIENT_TTY_PORT;
class RCS_LINKED_LIST;

class CMS_SERVER_REMOTE_TTY_PORT:public CMS_SERVER_REMOTE_PORT
{
public:
  CMS_SERVER_REMOTE_TTY_PORT (CMS_SERVER * _cms_server);
  virtual ~ CMS_SERVER_REMOTE_TTY_PORT ();
  int accept_local_port_cms (CMS *);
  void run ();
  void register_port ();
  void unregister_port ();
  double dtimeout;
protected:
    RCS_SERIAL_PORT_HANDLE handle;
  void handle_request ();
  REMOTE_CMS_REQUEST *request;
  char temp_buffer[0x2000];
  char devName[80];
  int serial_number;
  rcs_serial_port_setting settings;

private:
  CMS_SERVER_REMOTE_TTY_PORT(const CMS_SERVER_REMOTE_TTY_PORT &);
  CMS_SERVER_REMOTE_TTY_PORT &operator=(const CMS_SERVER_REMOTE_TTY_PORT &);
};


class CLIENT_TTY_PORT
{
public:
  CLIENT_TTY_PORT ();
  ~CLIENT_TTY_PORT ();
  long serial_number;
  int errors, max_errors;
};


#endif /* TTY_SRV_HH */
