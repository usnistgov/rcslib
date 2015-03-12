/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#include <stdio.h>		// printf, sscanf
#include <string.h>		// strlen
#include <signal.h>		// signal, SIGINT
#include <rcs.hh>		// NEW_COMMAND, RCS_DONE, ...
#include "getopt.h"		// getopt, ...
#include "skel.hh"		// basename
#include "subIntf.hh"		// SubCmdXXX,Stat,CfgXXX,Set

#define DEFAULT_NML_FILE "../etc/skel.nml"
#define DEFAULT_INI_FILE "../etc/skel.ini"
#define DEFAULT_CYCLE_TIME 0.1

static int done = 0;
static void quit(int sig)
{
  done = 1;
}

static void doCmdNop(SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd nop\n", set->id);
    }
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdInit(SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd init\n", set->id);
    }
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdAbort(SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd abort\n", set->id);
    }
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdHalt(SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd halt\n", set->id);
    }
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdShutdown(SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    done = 1;
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd shutdown\n", set->id);
    }
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdTask(SubCmdTask * cmd, SubStat * stat, SubSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUB_DEBUG_CMD) {
      printf("sub %d cmd task %d\n", set->id, cmd->what);
    }
    stat->what = 0;
    status_next(stat, RCS_EXEC);
    state_next(stat, S1);
  }

  if (state_match(stat, S1)) {
    if (cmd->what > stat->what) {
      stat->what++;
    } else if (cmd->what < stat->what) {
      stat->what--;
    } else {
      status_next(stat, RCS_DONE);
      state_next(stat, S0);
    }
  } else {			// S0
    state_default(stat);
  }
}

static void doCfgNop(SubSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    state_new(set);
    if (set->debug & SUB_DEBUG_CFG) {
      printf("sub %d cfg nop\n", set->id);
    }
    status_next(set, RCS_DONE);
    state_next(set, S0);
  } else {
    state_default(set);
  }
}

static void doCfgCycleTime(SubCfgCycleTime * cfg, SubSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    state_new(set);
    if (set->debug & SUB_DEBUG_CFG) {
      printf("sub %d cfg cycle time %f\n", set->id, cfg->cycleTime);
    }
    if (cfg->cycleTime <= 0.0) {
      status_next(set, RCS_ERROR);
    } else {
      set->cycleTime = cfg->cycleTime;
      status_next(set, RCS_DONE);
    }
    state_next(set, S0);
  } else {			// S0
    state_default(set);
  }
}

static void doCfgDebug(SubCfgDebug * cfg, SubSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    state_new(set);
    set->debug = cfg->debug;
    if (set->debug & SUB_DEBUG_CFG) {
      printf("sub %d cfg debug %d\n", set->id, cfg->debug);
    }
    status_next(set, RCS_DONE);
    state_next(set, S0);
  } else {
    state_default(set);
  }
}

int main(int argc, char *argv[])
{
#define BN basename(argv[0])
  char iniFile[SKEL_INI_FILE_NAME_LEN] = DEFAULT_INI_FILE;
  char nmlFile[SKEL_NML_FILE_NAME_LEN] = DEFAULT_NML_FILE;
  char chanName[SKEL_NML_BUFFER_NAME_LEN];
  int bufnum;
  int debug;
  int retval;
  RCS_CMD_CHANNEL * subCmdBuf;
  RCS_STAT_CHANNEL * subStatBuf;
  RCS_CMD_CHANNEL * subCfgBuf;
  RCS_STAT_CHANNEL * subSetBuf;
  SubCmdTask * subCmdTaskPtr;
  SubCfgCycleTime * subCfgCycleTimePtr;
  SubCfgDebug * subCfgDebugPtr;
  SubCmdNop subCmdNop;
  SubCfgNop subCfgNop;
  SubStat subStat;
  SubSet subSet;

  bufnum = 1;
  debug = 0;
  retval = 0;
  opterr = 0;
  while (1) {
    int i1;
    char option = getopt(argc, argv, ":n:i:b:d:");
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
    case 'b':
      if (1 == sscanf(optarg, "%i", &i1)) {
	if (i1 < 1) bufnum = 1;
	else if (i1 > 2) bufnum = 2;
	else bufnum = i1;
      } else {
	fprintf(stderr, "%s: bad buffer number: %s\n", BN, optarg);
	retval = 1;
      }
      break;
    case 'd':
      if (1 == sscanf(optarg, "%i", &i1)) {
	debug = i1;
      } else {
	fprintf(stderr, "%s: bad debug number: %s\n", BN, optarg);
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
  subCmdTaskPtr = (SubCmdTask *) (subCmdBuf->get_address());

  sprintf(chanName, "subStat%d", bufnum);
  subStatBuf = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subStatBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }

  sprintf(chanName, "subCfg%d", bufnum);
  subCfgBuf = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subCfgBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }
  subCfgCycleTimePtr = (SubCfgCycleTime *) (subCfgBuf->get_address());
  subCfgDebugPtr = (SubCfgDebug *) (subCfgBuf->get_address());

  sprintf(chanName, "subSet%d", bufnum);
  subSetBuf = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
  if (! subSetBuf->valid()) {
    fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
    return 1;
  }

  subCmdBuf->write(&subCmdNop);
  subCfgBuf->write(&subCfgNop);

  subStat.heartbeat = 0;
  subStat.what = 0;

  subSet.cycleTime = DEFAULT_CYCLE_TIME;
  subSet.id = bufnum;
  subSet.debug = debug;

  done = 0;
  signal(SIGINT, quit);

  while (! done) {
    NMLTYPE cmdType, cfgType;
    int cmdSerialNumber, cfgSerialNumber;

    // read command
    cmdType = subCmdBuf->read();
    cmdSerialNumber = subCmdBuf->get_address()->serial_number;

    switch (cmdType) {
    case 0:
      // no new command
      break;

    case -1:
      // comm error
      fprintf(stderr, "%s: NML read error\n", BN);
      break;

    case SUB_CMD_NOP_TYPE:
    case SUB_CMD_INIT_TYPE:
    case SUB_CMD_HALT_TYPE:
    case SUB_CMD_ABORT_TYPE:
    case SUB_CMD_TASK_TYPE:
    case SUB_CMD_SHUTDOWN_TYPE:
      subStat.command_type = cmdType;
      if (cmdSerialNumber != subStat.echo_serial_number) {
	subStat.echo_serial_number = cmdSerialNumber;
	subStat.state = NEW_COMMAND;
      }
      break;

    default:
      fprintf(stderr, "%s: unknown command %s\n", 
	      BN, subIntf_symbol_lookup(cmdType));
      break;
    } // switch (cmdType)

    // read config
    cfgType = subCfgBuf->read();
    cfgSerialNumber = subCfgBuf->get_address()->serial_number;

    switch (cfgType) {
    case 0:
      // no new command
      break;

    case -1:
      // comm error
      fprintf(stderr, "%s: NML read error\n", BN);
      break;

    case SUB_CFG_NOP_TYPE:
    case SUB_CFG_CYCLE_TIME_TYPE:
    case SUB_CFG_DEBUG_TYPE:
      subSet.command_type = cfgType;
      if (cfgSerialNumber != subSet.echo_serial_number) {
	subSet.echo_serial_number = cfgSerialNumber;
	subSet.state = NEW_COMMAND;
      }
      break;

    default:
      fprintf(stderr, "%s: unknown config %s\n", 
	      BN, subIntf_symbol_lookup(cfgType));
      break;
    } // switch (cfgType)

    // handle command state tables
    switch (subStat.command_type) {
    case SUB_CMD_NOP_TYPE:
      doCmdNop(&subStat, &subSet);
      break;

    case SUB_CMD_INIT_TYPE:
      doCmdInit(&subStat, &subSet);
      break;

    case SUB_CMD_ABORT_TYPE:
      doCmdAbort(&subStat, &subSet);
      break;

    case SUB_CMD_HALT_TYPE:
      doCmdHalt(&subStat, &subSet);
      break;

    case SUB_CMD_SHUTDOWN_TYPE:
      doCmdShutdown(&subStat, &subSet);
      break;

    case SUB_CMD_TASK_TYPE:
      doCmdTask(subCmdTaskPtr, &subStat, &subSet);
      break;

    default:
      // no command to handle
      break;
    } // switch (subStat.command_type)

    // handle config state tables
    switch (subSet.command_type) {
    case SUB_CFG_NOP_TYPE:
      doCfgNop(&subSet);
      break;

    case SUB_CFG_CYCLE_TIME_TYPE:
      doCfgCycleTime(subCfgCycleTimePtr, &subSet);
      break;

    case SUB_CFG_DEBUG_TYPE:
      doCfgDebug(subCfgDebugPtr, &subSet);
      break;

    default:
      // no config to handle
      break;
    } // switch (subSet.command_type)

    subStat.heartbeat++;

    // update status and settings
    subStatBuf->write(&subStat);
    subSetBuf->write(&subSet);

    esleep(subSet.cycleTime);
  } // while (! done)

  delete subCmdBuf;
  delete subStatBuf;
  delete subCfgBuf;
  delete subSetBuf;

  rcs_print("Sub%d done\n", bufnum);

  return 0;
}

/*
  Modification history:

  $Log: subMain.cc,v $
  Revision 1.2  2006/01/09 14:45:48  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:26  proctor
  Initial checkin

*/

