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



#if !defined(TTYINTF_HH) && !defined(TTYINTF_H)
#define TTYINTF_HH
#define TTYINTF_H


struct rcs_serial_port_setting
{
  int baud_rate;
  int baud_rate_set;
  int data_bits;
  int data_bits_set;
  int stop_bits;
  int stop_bits_set;
  int use_parity;
  int use_parity_set;
  int even_parity;
  int even_parity_set;
};


#if defined(WIN32) || defined(MS_WINDOWS_API)
#include <windows.h>		/* HANDLE */
typedef HANDLE RCS_SERIAL_PORT_HANDLE;
#else
typedef int RCS_SERIAL_PORT_HANDLE;
#endif

#ifdef __cplusplus
extern "C"
{
#endif
  char *clean_string (char *string, int len);
  int print_serial_port_configuration (RCS_SERIAL_PORT_HANDLE _handle);
  int set_serial_port_configuration (RCS_SERIAL_PORT_HANDLE _handle,
				     struct rcs_serial_port_setting
				     *_settings);
  RCS_SERIAL_PORT_HANDLE open_serial_communications_port (const char *name);
  int read_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle,
				       char *buf, long maxlen);
  int readn_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle,
					char *buf, long maxlen);
  int write_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle,
					char *buf, long maxlen);
  int close_serial_communications_port (RCS_SERIAL_PORT_HANDLE _handle);

#ifdef __cplusplus
}
#endif


#endif
