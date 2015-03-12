/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#include <stdio.h>		// vsprintf
#include <string.h>		// strlen
#include <stdarg.h>		// va_list, ...
#include <rcs.hh>		// NML, etime
#include "skel.hh"		// these decls
#include "skelIntf.hh"		// SkelErr

char * basename(char * path)
{
  char * ptr = &path[strlen(path)];

  while (1) {
    if (*ptr == '/') return ptr+1;
    if (ptr == path) return ptr;
    ptr--;
  }
}

int reportError(NML * errBuf, const char *fmt, ...)
{
  SkelErr skelErr;
  va_list ap;

  skelErr.timestamp = etime();

  va_start(ap, fmt);
  vsprintf(skelErr.text, fmt, ap);
  va_end(ap);

  skelErr.text_length = strlen(skelErr.text);

  return errBuf->write(&skelErr);
}

char * rcs_state_to_string(int s)
{
  enum {BUFLEN = 80};
  static char buf[BUFLEN];	// warning-- not reentrant

  if (s == UNINITIALIZED_STATE) sprintf(buf, "UNINITIALIZED_STATE");
  else if (s == NEW_COMMAND) sprintf(buf, "NEW_COMMAND");
  else if (s == NOP_STATE) sprintf(buf, "NOP_STATE");
  else if (s < 0) sprintf(buf, "SE%d", -(s+10));
  else sprintf(buf, "S%d", s);

  return buf;
}

char * rcs_status_to_string(int s)
{
  enum {BUFLEN = 80};
  static char buf[BUFLEN];	// warning-- not reentrant

  if (s == UNINITIALIZED_STATUS) sprintf(buf, "Uninitialized");
  else if (s == RCS_DONE) sprintf(buf, "Done");
  else if (s == RCS_EXEC) sprintf(buf, "Exec");
  else if (s == RCS_ERROR) sprintf(buf, "Error");
  else sprintf(buf, "%d", s);

  return buf;
}

/*
  Modification history:

  $Log: skel.cc,v $
  Revision 1.1  2006/01/05 20:51:24  proctor
  Initial checkin

*/
