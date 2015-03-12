/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#include <stdio.h>		// fprintf, stderr
#include <signal.h>		// signal, SIGINT
#include <rcs.hh>		// NEW_COMMAND, RCS_DONE, ...
#include "getopt.h"		// getopt, ...
#include "skel.hh"		// basename
#include "skelIntf.hh"		// skelIntf_format

#define DEFAULT_NML_FILE "../etc/skel.nml"

static int done = 0;
static void quit(int sig)
{
  done = 1;
}

int main(int argc, char *argv[])
{
#define BN basename(argv[0])
  char nmlFile[SKEL_NML_FILE_NAME_LEN] = DEFAULT_NML_FILE;
  NML * skelErrBuf;
  SkelErr * skelErrPtr;
  SkelMsg * skelMsgPtr;
  int retval;

  retval = 0;
  opterr = 0;
  while (1) {
    char option = getopt(argc, argv, ":n:");
    if (option == -1)
      break;
    switch (option) {
    case 'n':
      strncpy(nmlFile, optarg, sizeof(nmlFile));
      nmlFile[sizeof(nmlFile) - 1] = 0;
      break;
    case ':':
      fprintf(stderr, "%s: missing parameter to %c\n", BN, (char) optopt);
      retval = 1;
      break;
    default:			// '?'
      fprintf(stderr, "%s: unrecognized option %c\n", BN, (char) optopt);
      retval = 1;
      break;
    }
  }
  if (retval) return retval;

  skelErrBuf = new NML(skelIntf_format, "skelErr", BN, nmlFile);
  if (! skelErrBuf->valid()) {
    fprintf(stderr, "%s: can't open skelErr buffer\n", BN);
    return 1;
  }
  skelErrPtr = (SkelErr *) skelErrBuf->get_address();
  skelMsgPtr = (SkelMsg *) skelErrBuf->get_address();

  signal(SIGINT, quit);

  for (done = 0; done == 0; ) {
    NMLTYPE type = skelErrBuf->blocking_read(-1.0);

    switch (type) {
    case 0:
      // no new msg
      break;
    case 1:
      // comm error
      break;
    case SKEL_ERR_TYPE:
      printf("[%f] Error:   %s\n", skelErrPtr->timestamp, skelErrPtr->text);
      fflush(stdout);
      break;
      printf("[%f] Message: %s\n", skelMsgPtr->timestamp, skelMsgPtr->text);
      fflush(stdout);
    case SKEL_MSG_TYPE:
      break;
    }
  }

  delete skelErrBuf;

  return 0;
}

/*
  Modification history:

  $Log: skelLog.cc,v $
  Revision 1.2  2006/01/05 22:50:08  proctor
  Called fflush to get piped output to work

  Revision 1.1  2006/01/05 20:51:25  proctor
  Initial checkin

*/

