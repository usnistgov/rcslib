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


#ifndef NMLCFGSVR_CLNTCALLS_HH
#define NMLCFGSVR_CLNTCALLS_HH

#include <stddef.h>

enum NMLCFGSVR_STATUS 
  {
    NMLCFGSVR_STATUS_NOT_SET=0,
    NMLCFGSVR_CREATE_OK=1,
    NMLCFGSVR_BUFFER_NOT_YET_REGISTERED=2,
    NMLCFGSVR_CONNECT_FAILED=-1,
    NMLCFGSVR_REPORTED_ERROR=-2,
    NMLCFGSVR_HOSTNAME_LOOKUP_FAILED=-3,
    NMLCFGSVR_MISC_ERROR=-4
  };

enum NMLCFGSVR_STATUS
nmlcfgsvr_create(const char *_svrhostport,
		 const char *_bufname,
		 const char *_procname,
		 char *bufline,
		 char *procline,
		 char *recvstring, size_t recvstring_size,
		 char *sendstring, size_t sendstring_size,
		 const char *options=0,
		 double _timeout=-1.0,
		 const char *domainset=0,
		 bool print_errors=true);

bool
nmlcfgsvr_check(const char *_svrhostport,
		const char *_bufname,
		char *recvstring, size_t recvstring_size,
		char *sendstring, size_t sendstring_size,
		const char *_procname=0,
		enum NMLCFGSVR_STATUS *_sts=0,
		double _timeout=-1.0,
		const char *domainset=0);

void
nmlcfgsvr_set_default_domain(const char *_domain);

bool
nmlcfgsvr_setdomain(const char *_svrhostport,
		    const char *_domain,
		    char *recvstring, size_t recvstring_size,
		    char *sendstring, size_t sendstring_size,
		    enum NMLCFGSVR_STATUS *_sts=0,
		    double _timeout=-1.0);

void
nmlcfgsvr_delete(const char *_svrhostport,
		 const char *_bufname,
		 char *recvstring, size_t recvstring_size,
		 char *sendstring, size_t sendstring_size,
		 const char *_procname=0,
		 enum NMLCFGSVR_STATUS *_sts=0,
		 double _timeout=-1.0);

// NMLCFGSVR_CLNTCALLS_HH
#endif
