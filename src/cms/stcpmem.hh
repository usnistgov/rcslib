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



#ifndef STCPMEM_HH
#define STCPMEM_HH

#ifndef HAVE_CONFIG_H
#include "rcs_defs.hh"		/* EXTERN_C_STD_HEADERS */
#endif

#include "cms.hh"		/* class CMS */


// struct dl_sa -- Used by dl_socket functions as an address type; see sokintf.h,sockintf.c
struct dl_sa;

class STCPMEM:public CMS
{
public:
  STCPMEM (const char *bufline,
	   const char *procline);

  virtual ~ STCPMEM ();

  /* Overloaded CMS functions. */
  CMS_STATUS clear ();
  int check_if_read ();
  int get_queue_length ();
  CMS_STATUS read ();
  CMS_STATUS peek ();
  CMS_STATUS write (void *data);
  CMS_STATUS write_if_read (void *data);

  /* This is not implemented, it is only added here so we can have a more
     informative error message, use TCP rather than STCP if you need blocking. */
  virtual CMS_STATUS blocking_read (double _timeout);	/* Read from  buffer, wait for new data. */

  int login (const char *name, const char *passwd);

protected:
  CMS_STATUS get_readpeek_reply(void);

  long serial_number;
  long returned_serial_number;
  struct dl_sa *server_socket_address_ptr;
  int socket_fd;
  char *temp_buffer;
  long temp_buffer_size;
  int read_request_issued;
  double read_request_time;
  int polling;
  int lines_to_skip;
  CMS_STATUS skip_lines ();
  int bytes_at_beginning_of_line;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  bool last_request_was_write;

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  STCPMEM(const STCPMEM &);
  STCPMEM &operator=(const STCPMEM &);

};

#endif
