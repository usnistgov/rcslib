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

#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "rcs_defs.hh"
#include <stdlib.h>
#include <string.h>
#endif

#include "nmlcms_c.h"
#include "cms.hh"		// class CMS
#include "nml.hh"		// class NML
#include "nml_srv.hh"		// run_nml_servers
#include "nmlmsg.hh"		// class NMLmsg
#include "rcs_prnt.hh"		// rcs_print_error
#include "nml_c_data.hh"	// class NML_C_DATA

#ifndef __unused_parameter__
#ifdef __GNUC__
#if (__GNUC__ >= 3 ) && !defined(MS_WINDOWS_API)
#define __unused_parameter__ __attribute__ ((unused))
#else
#define __unused_parameter__
#endif
#else
#define __unused_parameter__
#endif
#endif

extern "C" {

  struct nml_c_struct
  {
    NML *nml;
    NML_C_DATA ncd;
  };


  struct cms_c_struct
  {
    CMS cmsobj;
  };
 
}
// end extern "C"

struct nmlcms_c_extra_data
{
  nml_c_format_func_type f_ptr;
  int cbuf_offset;
};

int nml_c_wrap_format(NMLTYPE type, void *buffer, CMS *cms)
{
  char *cbuf = (char*) buffer;
  if(!cms)
    {
      return -1;
    }
  struct nmlcms_c_extra_data *ed = (struct nmlcms_c_extra_data *) cms->extra_data;
  if(!ed)
    {
      return -1;
    }
  if(buffer)
    {
      cbuf += ed->cbuf_offset;
    }
  nml_c_format_func_type f_ptr= (nml_c_format_func_type) ed->f_ptr;
  if(f_ptr)
    {
      return (*f_ptr)(type,((void*)cbuf),(struct cms_c_struct *)cms);
    }
  return -1;
}

void
nml_set_to_server_c(void)
{
  nmlSetToServer();
}

void 
nml_set_msg_base_offset(nml_c_t ncs, int s)
{
  if(ncs && ncs->nml && ncs->nml->cms && ncs->nml->cms->extra_data)
    {
      struct nmlcms_c_extra_data *ed = (struct nmlcms_c_extra_data *) ncs->nml->cms->extra_data;
      if(((size_t)s) == sizeof(NMLmsg))
	{
	  ed->cbuf_offset = 0;
	}
    }
  if(ncs)
    {
      ncs->ncd.msg_base_offset = s;
    }
}

nml_c_t
nml_new(nml_c_format_func_type _f_ptr,
	const char *_buffer_name,
	const char *_process_name,
	const char *_configuration_file)
{
  nml_c_t ncs = (nml_c_t) malloc(sizeof(struct nml_c_struct));
  ncs->ncd.f_function = (void *) nml_c_wrap_format;
  ncs->ncd.extra_data = (void *) malloc(sizeof(struct nmlcms_c_extra_data));
  struct nmlcms_c_extra_data *ed = (struct nmlcms_c_extra_data *)ncs->ncd.extra_data;
  ed->f_ptr = _f_ptr;
  ed->cbuf_offset = sizeof(NMLmsg);
  ncs->ncd.bufname =  strdup(_buffer_name);
  ncs->ncd.procname =  strdup(_process_name);
  ncs->ncd.cfgname = strdup(_configuration_file);
  ncs->ncd.min_message_size = 0;
  ncs->ncd.message_size_add = sizeof(NMLmsg);
  ncs->ncd.message_size_roundup = sizeof(NMLmsg);
  ncs->ncd.set_to_server=0;
  ncs->ncd.set_to_master=0;

  ncs->nml = new NML(&(ncs->ncd));
  if(ncs->nml)
    {
      if(!ncs->nml->valid())
	{
	  delete ncs->nml;
	  ncs->nml=0;
	}
    }
  if(!ncs->nml)
    {
      free(ncs);
      ncs=0;
    }
  return ncs;
}

int
nml_valid(nml_c_t ncs)
{
  if(!ncs)
    {
      return 0;
    }
  else if(!ncs->nml)
    {
      return 0;
    }
  return ncs->nml->valid();
}

          
int
nml_write(nml_c_t _ncs, void *_addr, nmltype_c_t _t, size_t _s)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): Invalid NML connection.\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }
  else if(
	  ((unsigned long) (_s +sizeof(NMLmsg))) >= ((unsigned long)_ncs->nml->cms->size))
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): size too big. max = %lu\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s),
		      (unsigned long) (_s -sizeof(NMLmsg)));
      return -1;
    }
  else if(!_addr || _t < 1 || _s < 1)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu)\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }

  NMLmsg *msg = 0;
  if(_ncs->ncd.msg_base_offset == sizeof(NMLmsg))
    {
      msg = (NMLmsg *)_addr;
    }
  else
    {
      msg = _ncs->nml->get_address();
      if(!msg)
	{
	  rcs_print_error("_ncs->nml->get_address() returned NULL\n");
	  return -1;
	}
      msg->type = _t;
      msg->size =(long) ( _s + sizeof(NMLmsg));
      memcpy(((char *)msg) + sizeof(NMLmsg), _addr, _s);
    }
  return _ncs->nml->write(msg);
}


int
nml_write_if_read(nml_c_t _ncs, void *_addr, nmltype_c_t _t, size_t _s)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): Invalid NML connection.\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }
  else if(
	  ((unsigned long) (_s +sizeof(NMLmsg))) >= ((unsigned long)_ncs->nml->cms->size))
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): size too big. max = %lu\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s),
		      (unsigned long) (_s -sizeof(NMLmsg)));
      return -1;
    }
  else if(!_addr || _t < 1 || _s < 1)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu)\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }

  NMLmsg *msg = 0;
  if(_ncs->ncd.msg_base_offset == sizeof(NMLmsg))
    {
      msg = (NMLmsg *)_addr;
    }
  else
    {
      msg = _ncs->nml->get_address();
      if(!msg)
	{
	  rcs_print_error("_ncs->nml->get_address() returned NULL\n");
	  return -1;
	}
      msg->type = _t;
      msg->size = (long) ( _s + sizeof(NMLmsg));
      memcpy(((char *)msg) + sizeof(NMLmsg), _addr, _s);
    }
  return _ncs->nml->write_if_read(msg);
}

int
nml_check_if_read(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_check_if_read(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->check_if_read();
}

int
nml_get_msg_count(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_get_msg_count(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->get_msg_count();
}

int
nml_get_space_available(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_get_space_available(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->get_space_available();
}

int
nml_get_queue_length(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_get_queue_length(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->get_queue_length();
}


int
nml_reset(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_reset(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->reset();
}

int
nml_xml_schema_save_as(nml_c_t _ncs, const char *filename)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_reset(%p): Invalid NML connection.\n",
		      (void *)_ncs);
      return -1;
    }
  return _ncs->nml->xmlSchemaSaveAs(filename);
}

int
nml_xml_msg_save_as(nml_c_t _ncs, void  *_addr, nmltype_c_t _t, size_t _s, const char *filename)
{
  if(!_ncs || !_ncs->nml || !_ncs->nml->cms)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): Invalid NML connection.\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }
  else if(
	  ((unsigned long) (_s +sizeof(NMLmsg))) >= ((unsigned long)_ncs->nml->cms->size))
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu): size too big. max = %lu\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s),
		      (unsigned long) (_s -sizeof(NMLmsg)));
      return -1;
    }
  else if(!_addr || _t < 1 || _s < 1)
    {
      rcs_print_error("Bad argument to nml_write(%p,%p,%ld,%lu)\n",
		      (void *)_ncs,_addr,_t,((unsigned long) _s));
      return -1;
    }

  NMLmsg *msg = 0;
  if(_ncs->ncd.msg_base_offset == sizeof(NMLmsg))
    {
      msg = (NMLmsg *)_addr;
    }
  else
    {
      msg = _ncs->nml->get_address();
      if(!msg)
	{
	  rcs_print_error("_ncs->nml->get_address() returned NULL\n");
	  return -1;
	}
      msg->type = _t;
      msg->size = (long) (_s + sizeof(NMLmsg));
      memcpy(((char *)msg) + sizeof(NMLmsg), _addr, _s);
    }
  return _ncs->nml->xmlMsgSaveAs(msg,filename);
}

nmltype_c_t
nml_xml_msg_read_from_file(nml_c_t _ncs, const char *filename)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_read(%p)\n",
		      (void*) _ncs);
      return -1;
    }
  NMLmsg *msg = _ncs->nml->readMsgFromXmlFile(filename);
  if(!msg)
    {
      return -1;
    }
  return (nmltype_c_t) msg->type;
}

nmltype_c_t
nml_blocking_read(nml_c_t _ncs, double _timeout)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_read(%p)\n",
		      (void*) _ncs);
      return -1;
    }
  return (nmltype_c_t) _ncs->nml->blocking_read(_timeout);
}

nmltype_c_t
nml_read(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_read(%p)\n",
		      (void*) _ncs);
      return -1;
    }
  return (nmltype_c_t) _ncs->nml->read();
}

nmltype_c_t
nml_peek(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_read(%p)\n",
		      (void*) _ncs);
      return -1;
    }
  return (nmltype_c_t) _ncs->nml->read();
}  

void *
nml_get_address(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_read(%p)\n",
		      (void*) _ncs);
      return 0;
    }
  NMLmsg *msg = _ncs->nml->get_address();
  if(!msg)
    {
      rcs_print_error("_ncs->nml->get_address() returned NULL\n");
      return 0;
    }
  if(_ncs->ncd.msg_base_offset == sizeof(NMLmsg))
    {
      return (void *) msg;
    }
  return (void *) (((char*)msg)+sizeof(NMLmsg));
}  

int
nml_get_last_error_type(nml_c_t _ncs)
{
  if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_get_last_error_type(%p)\n",
		      (void*) _ncs);
      return 0;
    }
  return _ncs->nml->error_type;
}

void
nml_set_print_destination_c(int dest_int)
{
  set_rcs_print_destination((RCS_PRINT_DESTINATION_TYPE)dest_int);
} 
 
void
nml_set_print_file_c(const char *dest_string)
{
  set_rcs_print_file(dest_string);
}

void
nml_debug_on_c(void)
{
  set_rcs_print_flag(PRINT_EVERYTHING);
}

void
nml_debug_off_c(void)
{
  set_rcs_print_flag(PRINT_RCS_ERRORS);
}

void 
nml_free(nml_c_t _ncs)
{
    if(!_ncs || !_ncs->nml)
    {
      rcs_print_error("Bad argument to nml_free(%p)\n",
		      (void*) _ncs);
      return;
    }
    if(_ncs->ncd.bufname)
      {
	free((void *)_ncs->ncd.bufname);
	_ncs->ncd.bufname=0;
      }
    if(_ncs->ncd.procname)
      {
	free((void *)_ncs->ncd.procname);
	_ncs->ncd.procname=0;
      }
    if(_ncs->ncd.cfgname)
      {
	free((void *)_ncs->ncd.cfgname);
	_ncs->ncd.cfgname=0;
      }
    if(_ncs && _ncs->nml && _ncs->nml->cms && _ncs->nml->cms->extra_data)
    {
      struct nmlcms_c_extra_data *ed = (struct nmlcms_c_extra_data *)_ncs->nml->cms->extra_data;
      if(ed)
	{
	  free(ed);
	  _ncs->nml->cms->extra_data=0;
	}
    }
    NML *nmlptr = _ncs->nml;
    _ncs->nml = 0;
    delete nmlptr;
    free(_ncs);
}


void
cms_update_unsigned_char(struct cms_c_struct *_ccs,
			 const char *name,
			 unsigned char *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_char(struct cms_c_struct *_ccs,
		const char *name,
		char *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_unsigned_char_array(struct cms_c_struct *_ccs,
			       const char *name,
			       unsigned char *x,
			       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}

void
cms_update_char_array(struct cms_c_struct *_ccs,
		      const char *name,
		      char *x,
		      unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }  
}

void
cms_update_unsigned_char_dla(struct cms_c_struct *_ccs,
			     const char *name,
			     unsigned char *x,
			     int *lenptr,
			     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_char_dla(struct cms_c_struct *_ccs,
		    const char *name,
		    char *x,
		    int *lenptr,
		    int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}  


void
cms_update_unsigned_short(struct cms_c_struct *_ccs,
			 const char *name,
			 unsigned short *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_short(struct cms_c_struct *_ccs,
		const char *name,
		short *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_unsigned_short_array(struct cms_c_struct *_ccs,
			       const char *name,
			       unsigned short *x,
			       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}

void
cms_update_short_array(struct cms_c_struct *_ccs,
		      const char *name,
		      short *x,
		      unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }  
}

void
cms_update_unsigned_short_dla(struct cms_c_struct *_ccs,
			     const char *name,
			     unsigned short *x,
			     int *lenptr,
			     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_short_dla(struct cms_c_struct *_ccs,
		    const char *name,
		    short *x,
		    int *lenptr,
		    int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}  


void
cms_update_unsigned_int(struct cms_c_struct *_ccs,
			 const char *name,
			 unsigned int *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_int(struct cms_c_struct *_ccs,
		const char *name,
		int *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_unsigned_int_array(struct cms_c_struct *_ccs,
			       const char *name,
			       unsigned int *x,
			       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}

void
cms_update_int_array(struct cms_c_struct *_ccs,
		      const char *name,
		      int *x,
		      unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }  
}

void
cms_update_unsigned_int_dla(struct cms_c_struct *_ccs,
			     const char *name,
			     unsigned int *x,
			     int *lenptr,
			     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_int_dla(struct cms_c_struct *_ccs,
		    const char *name,
		    int *x,
		    int *lenptr,
		    int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}  

void
cms_update_unsigned_long(struct cms_c_struct *_ccs,
			 const char *name,
			 unsigned long *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_long(struct cms_c_struct *_ccs,
		const char *name,
		long *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_unsigned_long_array(struct cms_c_struct *_ccs,
			       const char *name,
			       unsigned long *x,
			       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}

void
cms_update_long_array(struct cms_c_struct *_ccs,
		      const char *name,
		      long *x,
		      unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }  
}


void
cms_update_unsigned_long_long(struct cms_c_struct *_ccs,
			 const char *name,
			 unsigned long long *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_long_long(struct cms_c_struct *_ccs,
		const char *name,
		long long *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }
}

void
cms_update_unsigned_long_long_array(struct cms_c_struct *_ccs,
				    const char *name,
				    unsigned long long *x,
				    unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}

void
cms_update_long_long_array(struct cms_c_struct *_ccs,
			   const char *name,
			   long long *x,
			   unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }  
}

void
cms_update_unsigned_long_dla(struct cms_c_struct *_ccs,
			     const char *name,
			     unsigned long *x,
			     int *lenptr,
			     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_long_dla(struct cms_c_struct *_ccs,
		    const char *name,
		    long *x,
		    int *lenptr,
		    int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}  

void
cms_update_bool(struct cms_c_struct *_ccs,
		const char *name,
		nml_c_bool_t *x)
{
  bool b=true;
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      b = ((*x) != 0);
      bool orig_ptr_check_disabled =  (0 != cmsptr->pointer_check_disabled);
      cmsptr->pointer_check_disabled= (int) true;
      cmsptr->update_with_name(name,b);
      if(b)
	{
	  *x = 1;
	}
      else
	{
	  *x = 0;
	}
      cmsptr->pointer_check_disabled= (int) orig_ptr_check_disabled;
    }
}

void
cms_update_bool_array(struct cms_c_struct *_ccs,
		      const char *name,
		      nml_c_bool_t *x,
		      unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,((bool *)x),len);
    }
}


#if 0
/*
void
cms_update_bool_dla(struct cms_c_struct *_ccs,
		    const char *name,
		    nml_c_bool_t *x,
		    int *lenptr,
		    int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,((bool *)x),(*lenptr),maxlen);
    }
} 
*/
#endif

void
cms_update_float(struct cms_c_struct *_ccs,
		 const char *name,
		 float *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }

}

void
cms_update_float_array(struct cms_c_struct *  _ccs,
		       const char *name,
		       float *x,
		       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}


void
cms_update_float_dla(struct cms_c_struct *_ccs,
		     const char *name,
		     float *x,
		     int *lenptr,
		     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_double(struct cms_c_struct *_ccs,
		 const char *name,
		 double *x)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,(*x));
    }

}

void
cms_update_double_array(struct cms_c_struct *  _ccs,
		       const char *name,
		       double *x,
		       unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_with_name(name,x,len);
    }
}


void
cms_update_double_dla(struct cms_c_struct *_ccs,
		     const char *name,
		     double *x,
		     int *lenptr,
		     int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_with_name(name,x,(*lenptr),maxlen);
    }
}

void
cms_update_dla_length(struct cms_c_struct *_ccs,
		      const char *name,
		      int *lenptr)
{
  
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->update_dla_length_with_name(name,(*lenptr));
    }
}


void
cms_begin_class(struct cms_c_struct *_ccs, 
		const char *name, 
		const char *parent_class)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginClass(name,parent_class);
    }
}

void
cms_end_class(struct cms_c_struct *_ccs, 
	      const char *name,
	      const char *parent_class)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endClass(name,parent_class);
    }
}

void
cms_begin_base_class(struct cms_c_struct *_ccs, 
		     const char *name)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginBaseClass(name);
    }
}

void
cms_end_base_class(struct cms_c_struct *_ccs, 
		   const char *name)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endBaseClass(name);
    }
}


void
cms_begin_class_var(struct cms_c_struct *_ccs, 
		     const char *name)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginClassVar(name);
    }
}

void
cms_end_class_var(struct cms_c_struct *_ccs, 
		   const char *name)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endClassVar(name);
    }
}

void
cms_begin_struct_array_elem (struct cms_c_struct *_ccs,
			     const char *name, int index)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginStructArrayElem(name,index);
    }
}

void
cms_end_struct_array_elem (struct cms_c_struct *_ccs,
			     const char *name, int index)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endStructArrayElem(name,index);
    }
}

void
cms_begin_struct_dynamic_array (struct cms_c_struct *_ccs,
				const char *_name, int *len, int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginStructDynamicArray(_name,(*len),maxlen);
    }
}  

void
cms_end_struct_dynamic_array (struct cms_c_struct *_ccs,
				const char *_name, int *len, int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endStructDynamicArray(_name,(*len),maxlen);
    }
}  

void
cms_begin_struct_unbounded_array (struct cms_c_struct *_ccs,
				  const char *name, void **x, int *len,
				  int *size_allocated, size_t elsize)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginStructUnboundedArray(name,x,(*len),(*size_allocated), elsize);
    }
}  


void
cms_end_struct_unbounded_array (struct cms_c_struct *_ccs,
				  const char *name, void **x, int *len,
				  int *size_allocated, size_t elsize)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endStructUnboundedArray(name,x,(*len),(*size_allocated), elsize);
    }
}  

void
cms_begin_update_cmd_msg_base(__unused_parameter__ struct cms_c_struct *, 
			      __unused_parameter__ void*)
{
}

void
cms_end_update_cmd_msg_base(__unused_parameter__ struct cms_c_struct *, 
			    __unused_parameter__ void *)
{
}

void
cms_begin_update_stat_msg_base(__unused_parameter__ struct cms_c_struct *, 
			       __unused_parameter__ void *)
{
}

void
cms_end_update_stat_msg_base(__unused_parameter__ struct cms_c_struct *, 
			     __unused_parameter__ void *)
{
}
 
void
cms_next_update_default(struct cms_c_struct *_ccs,const char *def)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->next_update_default(def);
    }
}
 
long
cms_check_type_info (struct cms_c_struct *_ccs,
		     long type, void *buffer, const char *nsname,
		     cms_symbol_lookup_function_t symbol_lookup_function,
		     const char **namelist,
		     const long *idlist,
		     const size_t * sizelist,
		     long list_length, long max_name_length)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  char *cbuf = (char *) buffer;
  if(cbuf)
    {
      cbuf -= sizeof(NMLmsg);
    }
  if(cmsptr)
    {
      return cmsptr->check_type_info(type,((void*)cbuf),nsname,
				     symbol_lookup_function,
				     namelist,idlist,sizelist,
				     list_length,
				     max_name_length);
    }
  return -1;
} 
  
void
cms_set_add_array_indexes_to_name(struct cms_c_struct *_ccs,
				  nml_c_bool_t _val)
{
    CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
    if(cmsptr)
      {
	cmsptr->add_array_indexes_to_name= (bool) (_val != 0);
      }
}


int
cms_update_enumeration(struct cms_c_struct *_ccs,
		       const char *name,
		       int enumin, 
		       void *enumaddr,
		       const struct cms_enum_info * info)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      return cmsptr->update_enumeration_with_name(name,enumin,enumaddr,info);
    }
  return -1;
}


void
cms_begin_enumeration_array (struct cms_c_struct *_ccs,
			     const char *name,
			     const struct cms_enum_info * info,
			     unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginEnumerationArray(name,info,len);
    }
}


void
cms_begin_enumeration_dla(struct cms_c_struct *_ccs,
					 const char *name,
					 const struct cms_enum_info * info, 
					 int *len,
					 int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginEnumerationDLA(name,info,(*len),maxlen);
    }
}


void
cms_begin_enumeration_unbounded (struct cms_c_struct *_ccs,
					       const char *name, int **x,
					       const cms_enum_info * info, 
					       int *len,
					       int *maxlen, 
					       size_t elsize)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->beginEnumerationUnbounded(name,x,info,(*len),(*maxlen),elsize);
    }
}


int
cms_update_enumeration_array_elem (struct cms_c_struct *_ccs,
						int enumin, 
						void *enumaddr, 
						int elem)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      return cmsptr->update_enumeration_array_elem(enumin,enumaddr,elem);
    }
  return -1;
}


void
cms_end_enumeration_array (struct cms_c_struct *_ccs,
					   const char *name,
					   const struct cms_enum_info * info,
					   unsigned int len)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endEnumerationArray(name,info,len);
    }
}


void
cms_end_enumeration_dla(struct cms_c_struct *_ccs,
					 const char *name,
					 const struct cms_enum_info * info, 
					 int *len,
					 int maxlen)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endEnumerationDLA(name,info,(*len),maxlen);
    }
}


void
cms_end_enumeration_unbounded (struct cms_c_struct *_ccs,
					     const char *name, int **x,
					     const cms_enum_info * info, 
					     int *len,
					     int *maxlen, 
					     size_t elsize)
{
  CMS *cmsptr = (CMS *) &(_ccs->cmsobj);
  if(cmsptr)
    {
      cmsptr->endEnumerationUnbounded(name,x,info,(*len),(*maxlen),elsize);
    }
}

#ifdef ENABLE_RCS_SERVER
void run_nml_servers_c(void)
{
  run_nml_servers();
}
#endif


extern void nml_cms_pointer_check_disable(nml_c_t ncs)
{
  if(ncs && ncs->nml && ncs->nml->cms)
    {
      ncs->nml->cms->pointer_check_disabled=true;
    }
}

const struct cms_enum_info *
cms_create_cms_enum_info(const char *nameptr,
			 const char *namelistptr,
			 const int *int_list_ptr,
			 int max_name_len,
			 int list_len,
			 cms_symbol_lookup_function_t sl)
{
  struct cms_enum_info *new_info = (struct cms_enum_info *)malloc(sizeof(struct cms_enum_info));
  new_info->name = nameptr;
  new_info->string_list = (const char **) namelistptr;
  new_info->int_list = int_list_ptr;
  new_info->max_string_len = max_name_len;
  new_info->list_len = list_len;
  new_info->lookupfunc = sl;
  return new_info;
}

void
cms_free_cms_enum_info(struct cms_enum_info *ptr)
{
  free(ptr);
}




