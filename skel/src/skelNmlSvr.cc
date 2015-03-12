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
#include <string.h>		// strlen
#include <rcs.hh>		// NEW_COMMAND, RCS_DONE, ...
#include "getopt.h"		// getopt, ...
#include "skel.hh"		// basename
#include "skelIntf.hh"		// skelIntf_format
#include "supIntf.hh"		// supIntf_format
#include "subIntf.hh"		// subIntf_format

#define DEFAULT_NML_FILE "../etc/skel.nml"

int main(int argc, char *argv[])
{
#define BN basename(argv[0])
#define SUB_NUM 2
  char nmlFile[SKEL_NML_FILE_NAME_LEN] = DEFAULT_NML_FILE;
  char chanName[SKEL_NML_BUFFER_NAME_LEN];
  NML * skelErrBuf;
  RCS_CMD_CHANNEL * supCmdBuf;
  RCS_STAT_CHANNEL * supStatBuf;
  RCS_CMD_CHANNEL * supCfgBuf;
  RCS_STAT_CHANNEL * supSetBuf;
  RCS_CMD_CHANNEL * subCmdBuf[SUB_NUM];
  RCS_STAT_CHANNEL * subStatBuf[SUB_NUM];
  RCS_CMD_CHANNEL * subCfgBuf[SUB_NUM];
  RCS_STAT_CHANNEL * subSetBuf[SUB_NUM];
  int subnum, bufnum;
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

  skelErrBuf = new NML(supIntf_format, "skelErr", BN, nmlFile);
  if (! skelErrBuf->valid()) {
    fprintf(stderr, "%s: can't open skelErr buffer\n", BN);
    return 1;
  }

  supCmdBuf = new RCS_CMD_CHANNEL(supIntf_format, "supCmd", BN, nmlFile);
  if (! supCmdBuf->valid()) {
    fprintf(stderr, "%s: can't open supCmd buffer\n", BN);
    return 1;
  }

  supStatBuf = new RCS_STAT_CHANNEL(supIntf_format, "supStat", BN, nmlFile);
  if (! supStatBuf->valid()) {
    fprintf(stderr, "%s: can't open supStat buffer\n", BN);
    return 1;
  }

  supCfgBuf = new RCS_CMD_CHANNEL(supIntf_format, "supCfg", BN, nmlFile);
  if (! supCfgBuf->valid()) {
    fprintf(stderr, "%s: can't open supCfg buffer\n", BN);
    return 1;
  }

  supSetBuf = new RCS_STAT_CHANNEL(supIntf_format, "supSet", BN, nmlFile);
  if (! supSetBuf->valid()) {
    fprintf(stderr, "%s: can't open supSet buffer\n", BN);
    return 1;
  }

  for (subnum = 0, bufnum = 1; subnum < SUB_NUM; subnum++, bufnum++) {
    sprintf(chanName, "subCmd%d", bufnum);
    subCmdBuf[subnum] = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! subCmdBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }

    sprintf(chanName, "subStat%d", bufnum);
    subStatBuf[subnum] = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! subStatBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }

    sprintf(chanName, "subCfg%d", bufnum);
    subCfgBuf[subnum] = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! subCfgBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }

    sprintf(chanName, "subSet%d", bufnum);
    subSetBuf[subnum] = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! subSetBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }
  }

  run_nml_servers();

  delete skelErrBuf;
  delete supCmdBuf;
  delete supStatBuf;
  delete supCfgBuf;
  delete supSetBuf;

  for (subnum = 0; subnum < SUB_NUM; subnum++) {
    delete subCmdBuf[subnum];
    delete subStatBuf[subnum];
    delete subCfgBuf[subnum];
    delete subSetBuf[subnum];
  }

  return 0;
}

/*
  Modification history:

  $Log: skelNmlSvr.cc,v $
  Revision 1.1  2006/01/05 20:51:25  proctor
  Initial checkin

*/

