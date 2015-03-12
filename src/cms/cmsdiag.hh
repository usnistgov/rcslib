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



#ifndef CMSDIAG_HH
#define CMSDIAG_HH

#include "cms_types.hh"		// enum CMS_INTERNAL_ACCESS_TYPE

class RCS_LINKED_LIST;

class CMS_DIAG_STATIC_PROC_INFO
{
public:
  char name[16];		// process name
  char host_sysinfo[32];
  long pid;			/* Process, Thread or Task Id. */
  double rcslib_ver;		// Version of the rcslib used by this component.
};

class CMS_DIAG_PROC_INFO:public CMS_DIAG_STATIC_PROC_INFO
{
public:
  CMS_INTERNAL_ACCESS_TYPE access_type;	//  access type of last operation
  long msg_id;			// id of the message written or at time of read.
  long msg_size;		// size of the message written or at time of read.
  long msg_type;		// id of the message written or at time of read.
  long number_of_accesses;
  long number_of_new_messages;
  double bytes_moved;
  double bytes_moved_across_socket;
  double last_access_time;
  double first_access_time;
  double max_difference;
  double min_difference;
};

class CMS_DIAG_HEADER
{
public:
  CMS_DIAG_HEADER(): last_writer(0),last_reader(0) {};
  long last_writer;
  long last_reader;
};


class CMS_DIAGNOSTICS_INFO:public CMS_DIAG_HEADER
{
public:
  CMS_DIAGNOSTICS_INFO ();
  virtual ~ CMS_DIAGNOSTICS_INFO ();
  CMS_DIAG_PROC_INFO *last_writer_dpi;
  CMS_DIAG_PROC_INFO *last_reader_dpi;
  RCS_LINKED_LIST *dpis;

private:
  //Prevent copying
  CMS_DIAGNOSTICS_INFO(const CMS_DIAGNOSTICS_INFO &);
  CMS_DIAGNOSTICS_INFO &operator=(const CMS_DIAGNOSTICS_INFO &);
};


extern double cmsdiag_timebias;
extern int cmsdiag_timebias_set;


#endif
