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

#if defined(ENABLE_RCS_DIAG)


#ifdef HAVE_CONFIG_H
#include "rcs_config_include.h"
#else
#include "cmsdiag_no_config.h"
#endif
// HAVE_CONFIG_H

#include "cms.hh"		// class CMS
#include "cmsdiag.hh"
#include "rcsvers.hh"		// rcs_minor_version_number, rcs_major_version_number
#include "physmem.hh"		// class PHYSMEM_HANDLE
#include "linklist.hh"		// class RCS_LINKED_LIST
#include "rcs_prnt.hh"

#include "sokintrf.h"

#if defined(MS_WINDOWS_API) && !defined(mingw32) && !defined(HAVE_CONFIG_H)
char *winver ();
#endif

#include "timer.hh"		// etime()


CMS_DIAGNOSTICS_INFO::CMS_DIAGNOSTICS_INFO ():
  CMS_DIAG_HEADER(),
  last_writer_dpi(0),last_reader_dpi(0),dpis(0)
{
  last_writer_dpi = NULL;
  last_reader_dpi = NULL;
  dpis = NULL;
}

CMS_DIAGNOSTICS_INFO::~CMS_DIAGNOSTICS_INFO ()
{
  last_writer_dpi = NULL;
  last_reader_dpi = NULL;
  if (NULL != dpis)
    {
      delete dpis;
      dpis = NULL;
    }
}

CMS_DIAG_PROC_INFO *
CMS::get_diag_proc_info ()
{
  return dpi;
}

void
CMS::set_diag_proc_info (CMS_DIAG_PROC_INFO * _dpi)
{
  dpi = _dpi;
}


double cmsdiag_timebias = 0.0;
int cmsdiag_timebias_set = 0;

void
CMS::setup_diag_proc_info ()
{
  first_diag_store = 1;
  if (NULL == dpi)
    {
      dpi = new CMS_DIAG_PROC_INFO ();
    }
  strncpy (dpi->name, ProcessName, 16);	// process name
  int sysinfo_len = 0;
  memset (dpi->host_sysinfo, 0, 32);


#ifdef HAVE_GETHOSTNAME
  load_socket_interface ();
  gethostname (dpi->host_sysinfo, 31);
  sysinfo_len += strlen (dpi->host_sysinfo);
  dpi->host_sysinfo[sysinfo_len++] = ',';
  dpi->host_sysinfo[sysinfo_len++] = ' ';
#endif

#ifdef HAVE_UNAME
  struct utsname utsnamevar;
  if(0 == uname(&utsnamevar))
    {
      if(sysinfo_len > 0 && sysinfo_len < 30 )
	{
	  dpi->host_sysinfo[sysinfo_len++] = ',';
	}
      if(0 != utsnamevar.sysname  && sysinfo_len > 0 && sysinfo_len < 30)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, utsnamevar.sysname, 31 - sysinfo_len);
	  sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
	}
     if(sysinfo_len > 0 && sysinfo_len < 30)
	{
	  dpi->host_sysinfo[sysinfo_len++] = ',';
	}
      if(0 != utsnamevar.version && sysinfo_len > 0 && sysinfo_len < 30)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, utsnamevar.version, 31 - sysinfo_len);
	  sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
	}
     if(sysinfo_len > 0 && sysinfo_len < 30)
	{
	  dpi->host_sysinfo[sysinfo_len++] = ',';
	}
      if(0 != utsnamevar.release && sysinfo_len >= 0 && sysinfo_len < 30)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, utsnamevar.release, 31 - sysinfo_len);
	  sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
	}
     if(sysinfo_len > 0 && sysinfo_len < 30)
	{
	  dpi->host_sysinfo[sysinfo_len++] = ',';
	}
     if(0 != utsnamevar.machine && sysinfo_len > 0 && sysinfo_len < 30)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, utsnamevar.machine, 31 - sysinfo_len);
	  sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
	}
    }
#else

#ifdef HAVE_SYSINFO
#ifdef SI_SYSNAM
  if (sysinfo_len < 31)
    {
      sysinfo (SI_SYSNAME, dpi->host_sysinfo + sysinfo_len, 31 - sysinfo_len);
      sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
    }
#endif
#ifdef SI_RELEASE
  if (sysinfo_len < 31)
    {
      dpi->host_sysinfo[sysinfo_len++] = ' ';
      sysinfo (SI_RELEASE, dpi->host_sysinfo + sysinfo_len, 31 - sysinfo_len);
      sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
    }
#endif
#ifdef SI_PLATFORM
  if (sysinfo_len < 31)
    {
      dpi->host_sysinfo[sysinfo_len++] = ',';
      sysinfo (SI_PLATFORM, dpi->host_sysinfo + sysinfo_len,
	       31 - sysinfo_len);
    }
#endif
#endif

#endif
  // #ifndef HAVE_UNAME

#if defined(MS_WINDOWS_API) && !defined(mingw32) && !defined(HAVE_CONFIG_H)
  char *wvptr = winver ();
  if (NULL != wvptr)
    {
      if(sysinfo_len > 0)
	{
	  dpi->host_sysinfo[sysinfo_len++] = ',';
	}
      strncpy (dpi->host_sysinfo + sysinfo_len, wvptr, 31 - sysinfo_len);
      sysinfo_len += strlen (dpi->host_sysinfo + sysinfo_len);
    }
#endif

#ifdef VXWORKS
  if (sysinfo_len < 31)
    {
      char *modelname = sysModel ();
      if (NULL != modelname)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, modelname,
		   31 - sysinfo_len);
	  sysinfo_len += strlen (modelname);
	  dpi->host_sysinfo[31] = 0;
	}
    }
  if (sysinfo_len < 31)
    {
      dpi->host_sysinfo[sysinfo_len++] = ',';
    }
  if (sysinfo_len < 31)
    {
      char *BspRevname = "VxWorks";
      if (NULL != BspRevname)
	{
	  strncpy (dpi->host_sysinfo + sysinfo_len, BspRevname,
		   31 - sysinfo_len);
	  sysinfo_len += strlen (BspRevname);
	  dpi->host_sysinfo[31] = 0;
	}
    }
#endif


  // Version of the rcslib used by this component.
  if (rcs_minor_version_number < 100)
    {
      dpi->rcslib_ver =
	(rcs_major_version_number + (rcs_minor_version_number * 1e-2));
    }
  else
    {
      dpi->rcslib_ver =
	(rcs_major_version_number + (rcs_minor_version_number * 1e-3));
    }

#if MS_WINDOWS_API && HAVE_GET_CURRENT_PROCESS_ID
  dpi->pid = GetCurrentProcessId ();	/* Process, Thread or Task Id. */
#else
#ifdef VXWORKS
  dpi->pid = taskIdSelf ();
#else
  dpi->pid = getpid ();
#endif
#endif

  dpi->access_type = CMS_ZERO_ACCESS;	//  access type of last operation
  dpi->msg_id = 0;		// id of the message written or at time of read.
  dpi->msg_size = 0;		// size of the message written or at time of read.
  dpi->msg_type = 0;		// id of the message written or at time of read.

  dpi->number_of_accesses = 0;
  dpi->number_of_new_messages = 0;
  dpi->bytes_moved = 0;
  dpi->last_access_time = 0;
  dpi->first_access_time = 0;
  dpi->max_difference = 0;
  dpi->min_difference = 0;
  first_diag_store = 1;
#ifndef VXWORKS
  if (!cmsdiag_timebias_set)
    {
      cmsdiag_timebias_set = 1;
      time_t ttime = time (NULL);
      cmsdiag_timebias = floor (etime () - ttime);
    }
#endif

#ifdef HAVE_GETHOSTNAME
  unload_socket_interface ();
#endif
}

void
CMS::calculate_and_store_diag_info (PHYSMEM_HANDLE * _handle,
				    void *_user_data)
{
  double cmsdiag_curtime = 0.0;
  if (NULL == dpi || _handle == NULL || !enable_diagnostics)
    {
      return;
    }
  long orig_offset = _handle->get_offset();
  CMS_DIAG_HEADER dh;

  _handle->set_enable_byte_counting(0);
  _handle->read (&dh, sizeof (CMS_DIAG_HEADER));
  if (connection_number == 0 && first_diag_store && dh.last_writer == 0)
    {
      dh.last_writer = -1;
    }
  if (connection_number == 0 && first_diag_store && dh.last_reader == 0)
    {
      dh.last_reader = -1;
    }
  if (internal_access_type == CMS_WRITE_ACCESS ||
      internal_access_type == CMS_WRITE_IF_READ_ACCESS)
    {
      dh.last_writer = connection_number;
    }
  else if (internal_access_type == CMS_READ_ACCESS)
    {
      dh.last_reader = connection_number;
    }
  _handle->write (&dh, sizeof (CMS_DIAG_HEADER));
  _handle->increment_offset( sizeof (CMS_DIAG_HEADER));
  _handle->increment_offset( (connection_number * sizeof (CMS_DIAG_PROC_INFO)));
  char c;
  _handle->read (&c, 1);
  first_diag_store |= ((c != ProcessName[0] && c != dpi->name[0]) || c == 0);
  if (!first_diag_store)
    {
      _handle->read (dpi, sizeof (CMS_DIAG_PROC_INFO));
    }
  dpi->access_type = internal_access_type;	//  access type of last operation
  dpi->msg_id = header.write_id;	// id of the message written or at time of read.
  dpi->msg_size = header.in_buffer_size;
  if (internal_access_type == CMS_WRITE_ACCESS ||
      internal_access_type == CMS_WRITE_IF_READ_ACCESS)
    {
      if (NULL != _user_data)
	{
	  dpi->msg_type = *((long *) _user_data);	// id of the message written or at time of read.
	}
    }
  else
    {
      if (NULL != subdiv_data)
	{
	  dpi->msg_type = *((long *) subdiv_data);	// id of the message written or at time of read.
	}
    }
  if (!disable_diag_store)
    {
      dpi->number_of_accesses++;
    }
  if (dpi->number_of_accesses < 1)
    {
      dpi->number_of_accesses = 1;
      dpi->number_of_new_messages = 1;
      _handle->set_total_bytes_moved(0);
      pre_op_total_bytes_moved = 0;
      first_diag_store = 1;
    }
  if (internal_access_type == CMS_WRITE_ACCESS ||
      internal_access_type == CMS_WRITE_IF_READ_ACCESS ||
      status == CMS_READ_OK)
    {
      dpi->number_of_new_messages++;
      if (dpi->number_of_new_messages < 1)
	{
	  dpi->number_of_accesses = 1;
	  dpi->number_of_new_messages = 1;
	  _handle->set_total_bytes_moved(0);
	  pre_op_total_bytes_moved = 0;
	  first_diag_store = 1;
	}
    }
  else if (disable_diag_store)
    {
      _handle->set_offset(orig_offset);
      first_diag_store = 0;
      _handle->set_enable_byte_counting(1);
      return;
    }

  dpi->bytes_moved += (_handle->get_total_bytes_moved() - pre_op_total_bytes_moved);
  cmsdiag_curtime = etime () - cmsdiag_timebias;
  if (!first_diag_store)
    {
      double diff = cmsdiag_curtime - dpi->last_access_time;
      if (diff < 0.0)
	{
	  _handle->set_total_bytes_moved(0.0);
	  dpi->bytes_moved = _handle->get_total_bytes_moved();
	  dpi->first_access_time = cmsdiag_curtime;
	  dpi->last_access_time = cmsdiag_curtime;
	  dpi->min_difference = 1e4;
	  dpi->max_difference = 0.0;
	  dpi->number_of_accesses = 0;
	  dpi->number_of_new_messages = 0;
	  _handle->set_total_bytes_moved(0);
	  pre_op_total_bytes_moved = 0;
	  first_diag_store = 1;
	}
      if (!disable_diag_store)
	{
	  if (diff < dpi->min_difference)
	    {
	      dpi->min_difference = diff;
	    }
	}
      if (diff > dpi->max_difference)
	{
	  dpi->max_difference = diff;
	}
      if (!disable_diag_store)
	{
	  dpi->last_access_time = cmsdiag_curtime;
	}
    }
  else
    {
      _handle->set_total_bytes_moved(
	(_handle->get_total_bytes_moved() - pre_op_total_bytes_moved));
      dpi->bytes_moved = _handle->get_total_bytes_moved();
      dpi->first_access_time = cmsdiag_curtime;
      dpi->last_access_time = cmsdiag_curtime;
      dpi->min_difference = 1e4;
      dpi->max_difference = 0.0;
      dpi->number_of_accesses = 1;
      dpi->number_of_new_messages = 1;
      _handle->set_total_bytes_moved(0.0);
      pre_op_total_bytes_moved = 0.0;
    }
  _handle->write (dpi, sizeof (CMS_DIAG_PROC_INFO));
  _handle->set_offset(orig_offset);
  first_diag_store = 0;
  _handle->set_enable_byte_counting(1);
}


void
CMS::internal_retrieve_diag_info (PHYSMEM_HANDLE * _handle, 
				  __unused_parameter__ void *_user_data)
{
  if (NULL == _handle || !enable_diagnostics)
    {
      return;
    }
  long orig_offset = _handle->get_offset();
  _handle->set_enable_byte_counting(0);
  if (NULL == di)
    {
      di = new CMS_DIAGNOSTICS_INFO ();
      di->dpis = new RCS_LINKED_LIST ();
    }
  else
    {
      di->dpis->delete_members ();
    }
  CMS_DIAG_HEADER dh;
  _handle->read (&dh, sizeof (CMS_DIAG_HEADER));
  di->last_writer = dh.last_writer;
  di->last_reader = dh.last_reader;
  _handle->increment_offset( sizeof (CMS_DIAG_HEADER));

  for (int i = 0; i < total_connections; i++)
    {
      CMS_DIAG_PROC_INFO cms_dpi;
      _handle->read (&cms_dpi, sizeof (CMS_DIAG_PROC_INFO));
      _handle->increment_offset( sizeof (CMS_DIAG_PROC_INFO));
      if (cms_dpi.name[0] != 0 || cms_dpi.number_of_accesses != 0)
	{
	  di->dpis->store_at_tail (&cms_dpi, sizeof (CMS_DIAG_PROC_INFO), 1);
	  if (i == di->last_writer)
	    {
	      di->last_writer_dpi =
		(CMS_DIAG_PROC_INFO *) di->dpis->get_tail ();
	    }
	  if (i == di->last_reader)
	    {
	      di->last_reader_dpi =
		(CMS_DIAG_PROC_INFO *) di->dpis->get_tail ();
	    }
	}
    }
  _handle->set_offset(orig_offset);
  _handle->set_enable_byte_counting(1);

}

CMS_DIAGNOSTICS_INFO *
CMS::get_diagnostics_info ()
{
  if (!enable_diagnostics)
    {
      return (NULL);
    }

  internal_access_type = CMS_GET_DIAG_INFO_ACCESS;
  status = CMS_STATUS_NOT_SET;
  blocking_timeout = 0;
  main_access (data);
  return (di);
}

CMS_DIAGNOSTICS_INFO::CMS_DIAGNOSTICS_INFO(
					   __unused_parameter__ const CMS_DIAGNOSTICS_INFO &_cdi):
  CMS_DIAG_HEADER(),
  last_writer_dpi(0),last_reader_dpi(0),dpis(0)
{
  rcs_print_error("CMS_DIAGNOSTICS_INFO copy constructor should never be called.\n");
}


CMS_DIAGNOSTICS_INFO &
CMS_DIAGNOSTICS_INFO::operator=(
				__unused_parameter__ const CMS_DIAGNOSTICS_INFO &_cdi)
{
  rcs_print_error("CMS_DIAGNOSTICS_INFO::operator= should never be called.\n");
  return(*this);
}


//  defined(ENABLE_RCS_DIAG)

#else
#include "rcs_empty_source"
#endif
