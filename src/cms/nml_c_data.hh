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

#ifndef NML_C_DATA_HH
#define NML_C_DATA_HH

class NML_C_DATA
{
public:
  const char *bufname;
  const char *procname;
  const char *cfgname;
  void *extra_data;
  void *f_function;
  int set_to_server;
  int set_to_master;
  size_t min_message_size;
  size_t message_size_add;
  size_t message_size_roundup;
  int msg_base_offset;
  
  NML_C_DATA():
    bufname(0),procname(0),cfgname(0),extra_data(0),
    f_function(0),set_to_server(0),set_to_master(0),
    min_message_size(0),message_size_add(0),message_size_roundup(0),
    msg_base_offset(0)
  {
  };

private:
  NML_C_DATA(const NML_C_DATA &);
  NML_C_DATA &operator =(const NML_C_DATA &);
};


#endif
