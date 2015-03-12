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

/************************************************************************
File: locmem.hh
Purpose: Defines LOCMEM which is a derived class of CMS that serves primarily
to provide addresses that match when matching buffer names are passed to
the constructor. It is useful in allowing control modules to use the
same inteface to communicate as would be required if they were not
running in the same process even though to use LOCMEM they must be.
**********************************************************************/

#ifndef LOCMEM_HH
#define LOCMEM_HH



#include "cms.hh"		// class CMS
class RCS_LINKED_LIST;
class BUFFERS_LIST_NODE;

class LOCMEM:public CMS
{
public:
  LOCMEM (const char *bufline,
	  const char *procline,
	  int set_to_server =0,
	  int set_to_master = 0);

  virtual ~ LOCMEM ();
  
  CMS_STATUS main_access (void *_local);

protected:
  void *lm_addr;
  int buffer_id;
  class BUFFERS_LIST_NODE *my_node;
  static class RCS_LINKED_LIST *buffers_list;

private:
  //Private copy constructor and = operator to prevent copying.
  // only cms_cfg*copy functions can be used to copy CMS objects.
  LOCMEM(const LOCMEM &);
  LOCMEM &operator=(const LOCMEM &);

};
















#endif
