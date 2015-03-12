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

#ifndef HTTPNML_HH
#define HTTPNML_HH

#include "nml.hh"
#include "cms.hh"		/* class CMS */
#include "rem_msg.hh"		// REMOTE_CMS_REQUEST_TYPE

struct dl_sa;

class HttpXmlNml : protected NML
{
public:
  HttpXmlNml(NML_FORMAT_PTR f_ptr, const char *url);
  ~HttpXmlNml();

  NMLmsg *readMsg();
protected:
  char *hostname;
  int hostname_len;
  char *get_msg;
  int get_msg_len;
  struct dl_sa *server_socket_address_ptr;
  int tcp_port_number;
  int socket_fd;
  char *buffer;
  size_t buffer_size;
  int sockerrno;
  const char *sockerrstr;
  char sockerrbuf[256];
  int use_ipv6;

private:
  HttpXmlNml(const HttpXmlNml &);
  HttpXmlNml &operator=(const HttpXmlNml &);
};

#endif
//HTTPNML_HH
