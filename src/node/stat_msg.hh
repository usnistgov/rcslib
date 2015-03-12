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

#ifndef RCS_STAT_HH
#define RCS_STAT_HH

/*
  stat_msg.hh

  Declarations for RCS_STAT_MSG and related elements.
*/

#include "nml.hh"
#include "nmlmsg.hh"

enum RCS_STATE
{
  UNINITIALIZED_STATE = -1,
  NEW_COMMAND = -2,
  NOP_STATE = -3,
  SE0 = -10,
  SE1 = -11,
  SE2 = -12,
  SE3 = -13,
  SE4 = -14,
  SE5 = -15,
  SE6 = -16,
  SE7 = -17,
  SE8 = -18,
  SE9 = -19,
  S0 = 0,
  S1 = 1,
  S2 = 2,
  S3 = 3,
  S4 = 4,
  S5 = 5,
  S6 = 6,
  S7 = 7,
  S8 = 8,
  S9 = 9,
  S10 = 10,
  S11 = 11,
  S12 = 12,
  S13 = 13,
  S14 = 14,
  S15 = 15
  // States cut from 39 to 15 to avoid creating symbol S16 which conflicts with
  // sensoray board def. on Jul 2, 2009
};


enum RCS_STATUS
{
  UNINITIALIZED_STATUS = -1,
  RCS_DONE = 1,
  RCS_EXEC = 2,
  RCS_ERROR = 3
};

#define RCS_STAT_SOURCE_FILE_LEN 64

class  RCS_STAT_MSG:public NMLmsg
{
public:
  RCS_STAT_MSG (NMLTYPE t, size_t sz);
  NMLTYPE command_type;
  int echo_serial_number;
  RCS_STATUS status;
  int state;
  int line;
  int source_line;
  char source_file[RCS_STAT_SOURCE_FILE_LEN];

  void update_stat_msg_base (CMS * cms);
};

#define state_match(s,a) (s)->line = (s)->source_line = __LINE__, (s)->state == (a)
#define state_new(s) strncpy((s)->source_file, __FILE__, RCS_STAT_SOURCE_FILE_LEN), (s)->source_file[RCS_STAT_SOURCE_FILE_LEN-1] = 0
#define state_next(s,a) (s)->state = (a)
#define status_next(s,a) (s)->status = (a)
#define state_default(s) (s)->line = (s)->source_line = __LINE__

extern int  RCS_STAT_MSG_format (NMLTYPE, void *, CMS *);

class  RCS_STAT_CHANNEL:public NML
{
public:

#ifdef LINUXCNC_LIBNML_COMPAT

  RCS_STAT_CHANNEL (NML_FORMAT_PTR, 
		    const char *, 
		    const char *, 
		    const char *,
		    int set_to_server, int set_to_master,
		    enum NML_CONNECTION_MODE _connect_mode);

  RCS_STAT_CHANNEL (NML_FORMAT_PTR, 
		    const char *, 
		    const char *, 
		    const char *,
		    int set_to_server = 0);

#else // LINUXCNC_LIBNML_COMPAT
  RCS_STAT_CHANNEL (NML_FORMAT_PTR, 
		    const char *, 
		    const char *, 
		    const char *,
		    int set_to_server = 0, int set_to_master=0,
		    enum NML_CONNECTION_MODE _connect_mode=NML_NORMAL_CONNECTION_MODE);
#endif  // LINUXCNC_LIBNML_COMPAT

   ~RCS_STAT_CHANNEL ();

  RCS_STAT_MSG *get_address ()
  {
    return ((RCS_STAT_MSG *) NML::get_address ());
  };
protected:
  RCS_LINKED_LIST *stat_format_chain_setup(NML_FORMAT_PTR);
};

#define RCS_GENERIC_STATUS_TYPE         ((NMLTYPE) 2000000)

class  RCS_GENERIC_STATUS:public RCS_STAT_MSG
{
public:
  RCS_GENERIC_STATUS ();
  void update (CMS *);
};

extern const char *enum_RCS_STATUS_symbol_lookup (long v);

#endif
