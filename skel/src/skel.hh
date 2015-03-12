/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#include <rcs.hh>		// NML

#ifndef SKEL_HH
#define SKEL_HH

#define SKEL_BASE 1000

#define SKEL_ERR_BASE (SKEL_BASE + 1)

#define SUP_BASE (SKEL_BASE + 1000)
#define SUB_BASE (SKEL_BASE + 2000)

#define SUP_CMD_BASE (SUP_BASE + 100)
#define SUP_STAT_BASE (SUP_BASE + 200)
#define SUP_CFG_BASE (SUP_BASE + 300)
#define SUP_SET_BASE (SUP_BASE + 400)

#define SUB_CMD_BASE (SUB_BASE + 100)
#define SUB_STAT_BASE (SUB_BASE + 200)
#define SUB_CFG_BASE (SUB_BASE + 300)
#define SUB_SET_BASE (SUB_BASE + 400)

#define SKEL_INI_FILE_NAME_LEN 80
#define SKEL_NML_FILE_NAME_LEN 80
#define SKEL_NML_BUFFER_NAME_LEN 80

// miscellaneous stuff

extern char * basename(char * path);
extern int reportError(NML * errBuf, const char *fmt, ...);
extern char * rcs_state_to_string(int s);
extern char * rcs_status_to_string(int s);

/*
  Modification history:

  $Log: skel.hh,v $
  Revision 1.1  2006/01/05 20:51:25  proctor
  Initial checkin

*/

#endif

