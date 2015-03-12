/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/


#include <stdio.h>		// printf, sscanf, feof, stdin
#include <string.h>		// strlen, strcmp
#include <ctype.h>		// isspace
#include <rcs.hh>		// NEW_COMMAND, RCS_DONE, ...
#include "getopt.h"		// getopt, optarg,opt,err
#include "skel.hh"		// basename
#include "subIntf.hh"		// SubCmdXXX,Stat,CfgXXX,Set

#define DEFAULT_NML_FILE "../etc/skel.nml"
#define DEFAULT_INI_FILE "../etc/skel.ini"

static void printHelp(void)
{
  printf("quit --          quit\n");
  printf("?/help --        print help\n");
  printf("stat --          print status\n");
  printf("set --           print settings\n");
  printf("init --          initialize\n");
  printf("abort --         abort\n");
  printf("halt --          halt\n");
  printf("shutdown --      shut down\n");
  printf("task <number> -- send task command with <number> parameter\n");
  printf("ct <time> --     set the cycle time to <time> seconds\n");
  printf("debug <flag> --  set the debug flag\n");

  return;
}

static void printStat(SubStat * stat)
{
  printf("command_type:       %s\n",
	 subIntf_symbol_lookup(stat->command_type));
  printf("echo_serial_number: %d\n", (int) stat->echo_serial_number);
  printf("status:             %s\n", rcs_status_to_string(stat->status));
  printf("state:              %s\n", rcs_state_to_string(stat->state));
  printf("line:               %d\n", (int) stat->line);
  printf("source_line:        %d\n", (int) stat->source_line);
  printf("source_file:        %s\n", (char *) stat->source_file);
  printf("heartbeat:          %d\n", (int) stat->heartbeat);
  printf("what:               %d\n", (int) stat->what);
}

static void printSet(SubSet * set)
{
  printf("command_type:       %s\n",
	 subIntf_symbol_lookup(set->command_type));
  printf("echo_serial_number: %d\n", (int) set->echo_serial_number);
  printf("status:             %s\n", rcs_status_to_string(set->status));
  printf("state:              %s\n", rcs_state_to_string(set->state));
  printf("line:               %d\n", (int) set->line);
  printf("source_line:        %d\n", (int) set->source_line);
  printf("source_file:        %s\n", (char *) set->source_file);
  printf("cycleTime:          %f\n", (double) set->cycleTime);
  printf("debug:              0x%X\n", (int) set->debug);
}

int main(int argc, char *argv[])
{
#define BN basename(argv[0])
  enum {BUFFERLEN = 80};
  char buffer[BUFFERLEN];
  char * ptr;
  char iniFile[SKEL_INI_FILE_NAME_LEN] = DEFAULT_INI_FILE;
  char nmlFile[SKEL_NML_FILE_NAME_LEN] = DEFAULT_NML_FILE;
  char chanName[SKEL_NML_BUFFER_NAME_LEN];
  RCS_CMD_CHANNEL * subCmdBuf;
  RCS_STAT_CHANNEL * subStatBuf;
  RCS_CMD_CHANNEL * subCfgBuf;
  RCS_STAT_CHANNEL * subSetBuf;
  SubStat * subStatPtr;
  SubSet * subSetPtr;
  enum {WHICH_STAT = 1, WHICH_SET};
  int bufnum;
  int which = WHICH_STAT;
  int retval;

  bufnum = 1;
  retval = 0;
  opterr = 0;
  while (1) {
    int i1;
    char option = getopt(argc, argv, ":n:i:d:");
    if (option == -1)
      break;
    switch (option) {
    case 'n':
      strncpy(nmlFile, optarg, sizeof(nmlFile));
      nmlFile[sizeof(nmlFile) - 1] = 0;
      break;
    case 'i':
      strncpy(iniFile, optarg, sizeof(iniFile));
      iniFile[sizeof(iniFile) - 1] = 0;
      break;
    case 'd':
      if (1 == sscanf(optarg, "%i", &i1)) {
	if (i1 < 1) bufnum = 1;
	else if (i1 > 2) bufnum = 2;
	else bufnum = i1;
      } else {
	fprintf(stderr, "%s: bad buffer number\n", BN);
	retval = 1;
      }
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

  sprintf(chanName, "subCmd%d", bufnum);
  subCmdBuf = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subCmdBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }

  sprintf(chanName, "subStat%d", bufnum);
  subStatBuf = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subStatBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }
  subStatPtr = (SubStat *) (subStatBuf->get_address());

  sprintf(chanName, "subCfg%d", bufnum);
  subCfgBuf = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subCfgBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }

  sprintf(chanName, "subSet%d", bufnum);
  subSetBuf = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subSetBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }
  subSetPtr = (SubSet *) (subSetBuf->get_address());

  while (! feof(stdin)) {
    SubCmdInit subCmdInit;
    SubCmdAbort subCmdAbort;
    SubCmdHalt subCmdHalt;
    SubCmdShutdown subCmdShutdown;
    SubCmdTask subCmdTask;
    SubCfgCycleTime subCfgCycleTime;
    SubCfgDebug subCfgDebug;
    double d1;
    int i1;

    printf("> ");
    fflush(stdout);
    if (NULL == fgets(buffer, BUFFERLEN, stdin)) break;

    subStatBuf->read();
    subSetBuf->read();

    for (ptr = buffer; isspace(*ptr); ptr++);

#define TRY(s) if (! strncmp(ptr, s, strlen(s)))
    if (*ptr == 0) {
      if (which == WHICH_STAT) printStat(subStatPtr);
      else if (which == WHICH_SET) printSet(subSetPtr);
    } else TRY("quit") {
      break;
    } else TRY("?") {
      printHelp();
    } else TRY("help") {
      printHelp();
    } else TRY("stat") {
      which = WHICH_STAT;
      printStat(subStatPtr);
    } else TRY("set") {
      which = WHICH_SET;
      printSet(subSetPtr);
    } else TRY("init") {
      subCmdInit.serial_number = subStatPtr->echo_serial_number + 1;
      subCmdBuf->write(&subCmdInit);
      which = WHICH_STAT;
    } else TRY("abort") {
      subCmdAbort.serial_number = subStatPtr->echo_serial_number + 1;
      subCmdBuf->write(&subCmdAbort);
      which = WHICH_STAT;
    } else TRY("halt") {
      subCmdHalt.serial_number = subStatPtr->echo_serial_number + 1;
      subCmdBuf->write(&subCmdHalt);
      which = WHICH_STAT;
    } else TRY("shutdown") {
      subCmdShutdown.serial_number = subStatPtr->echo_serial_number + 1;
      subCmdBuf->write(&subCmdShutdown);
      which = WHICH_STAT;
    } else TRY("task") {
      if (1 == sscanf(ptr, "%*s %d", &i1)) {
	subCmdTask.serial_number = subStatPtr->echo_serial_number + 1;
	subCmdTask.what = i1;
	subCmdBuf->write(&subCmdTask);
	which = WHICH_STAT;
      } else {
	printf("need <int>\n");
      }
    } else TRY("ct") {
      if (1 == sscanf(ptr, "%*s %lf", &d1)) {
	subCfgCycleTime.serial_number = subSetPtr->echo_serial_number + 1;
	subCfgCycleTime.cycleTime = d1;
	subCfgBuf->write(&subCfgCycleTime);
	which = WHICH_SET;
      } else {
	printf("need <float>\n");
      }
    } else TRY("debug") {
      if (1 == sscanf(ptr, "%*s %i", &i1)) {
	subCfgDebug.serial_number = subSetPtr->echo_serial_number + 1;
	subCfgDebug.debug = i1;
	subCfgBuf->write(&subCfgDebug);
	which = WHICH_SET;
      } else {
	printf("need <int>\n");
      }
    } else {
      printf("?\n");
    }

  } // while (! feof(stdin))

  delete subCmdBuf;
  delete subStatBuf;
  delete subCfgBuf;
  delete subSetBuf;

  return 0;
}

/*
  Modification history:

  $Log: subShell.cc,v $
  Revision 1.2  2006/01/09 14:45:48  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:26  proctor
  Initial checkin

*/

