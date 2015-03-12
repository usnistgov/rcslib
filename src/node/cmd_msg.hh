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


#ifndef RCS_CMD_HH
#define RCS_CMD_HH

#include "nml.hh"
#include "nmlmsg.hh"

class RCS_CMD_MSG:public NMLmsg
{
public:
  RCS_CMD_MSG (NMLTYPE t, long sz);
  int serial_number;
  void update_cmd_msg_base (CMS *);
};

extern int RCS_CMD_MSG_format (NMLTYPE, void *,CMS *);

class RCS_CMD_CHANNEL:public NML
{
public:

#ifdef LINUXCNC_LIBNML_COMPAT
  RCS_CMD_CHANNEL (NML_FORMAT_PTR, const char *, const char *, const char *,
		   int set_to_server, int set_to_master,
		   enum NML_CONNECTION_MODE connect_mode);

  RCS_CMD_CHANNEL (NML_FORMAT_PTR, const char *, const char *, const char *,
		   int set_to_server = 0);

#else  // LINUXCNC_LIBNML_COMPAT
  RCS_CMD_CHANNEL (NML_FORMAT_PTR, const char *, const char *, const char *,
		   int set_to_server = 0, int set_to_master=0,
		   enum NML_CONNECTION_MODE connect_mode=NML_NORMAL_CONNECTION_MODE);
#endif  // LINUXCNC_LIBNML_COMPAT

   ~RCS_CMD_CHANNEL ();
  RCS_CMD_MSG *get_address ()
  {
    return ((RCS_CMD_MSG*) NML::get_address ());
  };
protected:
  RCS_LINKED_LIST *cmd_format_chain_setup(NML_FORMAT_PTR);
};

enum RCS_GENERIC_CMD_ID
{
  GENERIC_INIT,
  GENERIC_HALT
};

#define RCS_GENERIC_CMD_TYPE    ((NMLTYPE) 1000000)

class RCS_GENERIC_CMD:public RCS_CMD_MSG
{
public:
  RCS_GENERIC_CMD ();
  void update (CMS *);
  int gen_id;
};

#endif
