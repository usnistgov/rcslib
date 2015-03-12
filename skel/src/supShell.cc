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
#include "supIntf.hh"		// SupCmdXXX,Stat,CfgXXX,Set

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

static void printStat(SupStat * stat)
{
  printf("command_type:       %s\n",
	 supIntf_symbol_lookup(stat->command_type));
  printf("echo_serial_number: %d\n", (int) stat->echo_serial_number);
  printf("status:             %s\n", rcs_status_to_string(stat->status));
  printf("state:              %s\n", rcs_state_to_string(stat->state));
  printf("line:               %d\n", (int) stat->line);
  printf("source_line:        %d\n", (int) stat->source_line);
  printf("source_file:        %s\n", (char *) stat->source_file);
  printf("heartbeat:          %d\n", (int) stat->heartbeat);
  printf("what:               %d\n", (int) stat->what);
}

static void printSet(SupSet * set)
{
  printf("command_type:       %s\n",
	 supIntf_symbol_lookup(set->command_type));
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
  RCS_CMD_CHANNEL * supCmdBuf;
  RCS_STAT_CHANNEL * supStatBuf;
  RCS_CMD_CHANNEL * supCfgBuf;
  RCS_STAT_CHANNEL * supSetBuf;
  SupStat * supStatPtr;
  SupSet * supSetPtr;
  enum {WHICH_STAT = 1, WHICH_SET};
  int which = WHICH_STAT;
  int retval;

  retval = 0;
  opterr = 0;
  while (1) {
    char option = getopt(argc, argv, ":n:i:");
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
  supStatPtr = (SupStat *) (supStatBuf->get_address());

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
  supSetPtr = (SupSet *) (supSetBuf->get_address());

  while (! feof(stdin)) {
    SupCmdInit supCmdInit;
    SupCmdAbort supCmdAbort;
    SupCmdHalt supCmdHalt;
    SupCmdShutdown supCmdShutdown;
    SupCmdTask supCmdTask;
    SupCfgCycleTime supCfgCycleTime;
    SupCfgDebug supCfgDebug;
    double d1;
    int i1;

    printf("> ");
    fflush(stdout);
    if (NULL == fgets(buffer, BUFFERLEN, stdin)) break;

    supStatBuf->read();
    supSetBuf->read();

    for (ptr = buffer; isspace(*ptr); ptr++);

#define TRY(s) if (! strncmp(ptr, s, strlen(s)))
    if (*ptr == 0) {
      if (which == WHICH_STAT) printStat(supStatPtr);
      else if (which == WHICH_SET) printSet(supSetPtr);
    } else TRY("quit") {
      break;
    } else TRY("?") {
      printHelp();
    } else TRY("help") {
      printHelp();
    } else TRY("stat") {
      which = WHICH_STAT;
      printStat(supStatPtr);
    } else TRY("set") {
      which = WHICH_SET;
      printSet(supSetPtr);
    } else TRY("init") {
      supCmdInit.serial_number = supStatPtr->echo_serial_number + 1;
      supCmdBuf->write(&supCmdInit);
      which = WHICH_STAT;
    } else TRY("abort") {
      supCmdAbort.serial_number = supStatPtr->echo_serial_number + 1;
      supCmdBuf->write(&supCmdAbort);
      which = WHICH_STAT;
    } else TRY("halt") {
      supCmdHalt.serial_number = supStatPtr->echo_serial_number + 1;
      supCmdBuf->write(&supCmdHalt);
      which = WHICH_STAT;
    } else TRY("shutdown") {
      supCmdShutdown.serial_number = supStatPtr->echo_serial_number + 1;
      supCmdBuf->write(&supCmdShutdown);
      which = WHICH_STAT;
    } else TRY("task") {
      if (1 == sscanf(ptr, "%*s %d", &i1)) {
	supCmdTask.serial_number = supStatPtr->echo_serial_number + 1;
	supCmdTask.what = i1;
	supCmdBuf->write(&supCmdTask);
	which = WHICH_STAT;
      } else {
	printf("need <int>\n");
      }
    } else TRY("ct") {
      if (1 == sscanf(ptr, "%*s %lf", &d1)) {
	supCfgCycleTime.serial_number = supSetPtr->echo_serial_number + 1;
	supCfgCycleTime.cycleTime = d1;
	supCfgBuf->write(&supCfgCycleTime);
	which = WHICH_SET;
      } else {
	printf("need <float>\n");
      }
    } else TRY("debug") {
      if (1 == sscanf(ptr, "%*s %i", &i1)) {
	supCfgDebug.serial_number = supSetPtr->echo_serial_number + 1;
	supCfgDebug.debug = i1;
	supCfgBuf->write(&supCfgDebug);
	which = WHICH_SET;
      } else {
	printf("need <int>\n");
      }
    } else {
      printf("?\n");
    }

  } // while (! feof(stdin))

  delete supCmdBuf;
  delete supStatBuf;
  delete supCfgBuf;
  delete supSetBuf;

  return 0;
}

/*
  Modification history:

  $Log: supShell.cc,v $
  Revision 1.2  2006/01/09 14:45:49  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:27  proctor
  Initial checkin

*/

