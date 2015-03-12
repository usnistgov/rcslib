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



/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1


#include "rcs.hh"
#include "shmem.hh"
#include "tcpmem.hh"
#include "udpmem.hh"
#include "cms_up.hh"
#include "cms_xup.hh"
#include "cms_xml_up.hh"

#include <stdio.h>

int 
main(int argc, const char **argv)
{
  printf("sizeof(NMLmsg)          \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(NMLmsg),sizeof(NMLmsg),malloc(sizeof(NMLmsg)));
  printf("sizeof(RCS_CMD_MSG)     \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_CMD_MSG),sizeof(RCS_CMD_MSG),malloc(sizeof(RCS_CMD_MSG)));
  printf("sizeof(RCS_STAT_MSG)    \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_STAT_MSG),sizeof(RCS_STAT_MSG),malloc(sizeof(RCS_STAT_MSG)));
  printf("sizeof(NML)             \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(NML),sizeof(NML),malloc(sizeof(NML)));
  printf("sizeof(CMS)             \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(CMS),sizeof(CMS),malloc(sizeof(CMS)));
  printf("sizeof(SHMEM)           \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(SHMEM),sizeof(SHMEM),malloc(sizeof(SHMEM)));
  printf("sizeof(TCPMEM)          \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(TCPMEM),sizeof(TCPMEM),malloc(sizeof(TCPMEM)));
  printf("sizeof(UDPMEM)          \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(UDPMEM),sizeof(UDPMEM),malloc(sizeof(UDPMEM)));
  printf("sizeof(RCS_LINKED_LIST) \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_LINKED_LIST),sizeof(RCS_LINKED_LIST),malloc(sizeof(RCS_LINKED_LIST)));
  printf("sizeof(RCS_SHAREDMEM)   \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_SHAREDMEM),sizeof(RCS_SHAREDMEM),malloc(sizeof(RCS_SHAREDMEM)));
  printf("sizeof(RCS_SEMAPHORE)   \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_SEMAPHORE),sizeof(RCS_SEMAPHORE),malloc(sizeof(RCS_SEMAPHORE)));
  printf("sizeof(RCS_TIMER)       \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(RCS_TIMER),sizeof(RCS_TIMER),malloc(sizeof(RCS_TIMER)));
  printf("sizeof(CMS_UPDATER)     \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(CMS_UPDATER),sizeof(CMS_UPDATER),malloc(sizeof(CMS_UPDATER)));
  printf("sizeof(CMS_XML_UPDATER) \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(CMS_XML_UPDATER),sizeof(CMS_XML_UPDATER),malloc(sizeof(CMS_XML_UPDATER)));
  printf("sizeof(CMS_XDR_UPDATER) \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(CMS_XDR_UPDATER),sizeof(CMS_XDR_UPDATER),malloc(sizeof(CMS_XDR_UPDATER)));
  printf("sizeof(PM_CARTESIAN)    \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(PM_CARTESIAN),sizeof(PM_CARTESIAN),malloc(sizeof(PM_CARTESIAN)));
  printf("sizeof(PM_QUATERNION)   \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(PM_QUATERNION),sizeof(PM_QUATERNION),malloc(sizeof(PM_QUATERNION)));
  printf("sizeof(PM_HOMOGENEOUS)  \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(PM_HOMOGENEOUS),sizeof(PM_HOMOGENEOUS),malloc(sizeof(PM_HOMOGENEOUS)));
  printf("sizeof(NML_MODULE)      \t=  \t%d  \t(0x%X  %p)\n",
	 sizeof(NML_MODULE),sizeof(NML_MODULE),malloc(sizeof(NML_MODULE)));
}

