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

/*************************************************************************
* File: cms_cfg.hh                                                    *
* Authors: Fred Proctor, Will Shackleford                                *
* Purpose: C++ Header file for the  Communication Management System (CMS)*
*          Includes:                                                     *
*                    1. cms_config -- function which reads               *
*                         configuration file.                            *
*************************************************************************/

#ifndef CMS_CFG_HH
#define CMS_CFG_HH

/* Include Files */
#include "cms_types.hh"

class CMS;
class CONFIG_FILE_INFO;

/* Config File Definitions. */
#ifndef CMS_CONFIG_COMMENTCHAR
#define CMS_CONFIG_COMMENTCHAR '#'
#endif


#ifdef __cplusplus
extern "C"
{
#endif

/* Function Prototypes. */
  extern int cms_config_with_buffers(CMS ** c, const char *b, const char *p, const char *f,
				     char *wordbuf,char *linebuf, char *buf,char *buf2,
				     int set_to_server = 0, 
				     int set_to_master = 0,
				     size_t max_size_from_format=0,
				     enum CMS_CONNECTION_MODE connection_mode=CMS_NORMAL_CONNECTION_MODE);

  extern int cms_copy_with_buffers(CMS ** dest, CMS * src,
				   char *wordbuf,
				   char *proctype,
				   char *buffertype,
				   int set_to_server = 0, 
				   int set_to_master =0,
				   enum CMS_CONNECTION_MODE connection_mode=CMS_NORMAL_CONNECTION_MODE);



  extern int cms_create_from_lines_with_buffers (CMS ** cms, 
						 const char *buffer_line,
						 const char *proc_line,
						 char *wordbuf,
						 char *proctype,
						 char *buffertype,
						 int set_to_server =0,
						 int set_to_master = 0,
						 enum CMS_CONNECTION_MODE connection_mode=CMS_NORMAL_CONNECTION_MODE);


  extern int cms_create_with_buffers (CMS ** cms, 
				      const char *buf_line, 
				      const char *proc_line,
				      char *buffer_type, char *proc_type,
				      char *wordbuf,
				      int set_to_server = 0, 
				      int set_to_master = 0,
				     enum CMS_CONNECTION_MODE connection_mode=CMS_NORMAL_CONNECTION_MODE);

  extern int load_nml_config_file (const char *file);
  extern int unload_nml_config_file (const char *file);
#ifdef __cplusplus
  extern "C" {
#endif
    extern int unload_all_nml_config_files ();
#ifdef __cplusplus
  }
#endif
    
  extern char *get_buffer_line_with_buffers (const char *buf, const char *file,
					     char *linebuf, char *wordbuf);
  extern int hostname_matches_bufferline_with_buffer (const char *bufline, char *wordbuf);

  extern class CONFIG_FILE_INFO *get_loaded_nml_config_file (const char *file);
  extern int print_loaded_nml_config_file (const char *file);
  extern int print_loaded_nml_config_file_list (void);

  extern void set_cmscfg_ignore_remote(int );
  extern int  get_cmscfg_ignore_remote(void);

  extern void set_cmscfg_ignore_no_bufline(int );
  extern int  get_cmscfg_ignore_no_bufline(void);

  extern int get_cmscfg_last_buffer_was_ignored_remote(void);
  extern void clear_cmscfg_last_buffer_was_ignored_remote(void);

  extern int get_cmscfg_last_buffer_was_ignored_no_bufline(void);
  extern void clear_cmscfg_last_buffer_was_ignored_no_bufline(void);

#ifdef __cplusplus
}
#endif



#endif				/* !defined(CMS_CFG_HH) */
