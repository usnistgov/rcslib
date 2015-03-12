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
#include "skel.hh"		// basename, reportError
#include "skelIntf.hh"		// SkelErr
#include "supIntf.hh"		// SupCmdXXX,Stat,CfgXXX,Set
#include "subIntf.hh"		// SubCmdXXX,Stat,CfgXXX,Set

#define DEFAULT_NML_FILE "../etc/skel.nml"
#define DEFAULT_INI_FILE "../etc/skel.ini"
#define DEFAULT_CYCLE_TIME 0.1
#define SUB_NUM 2

static int done = 0;
static void quit(int sig)
{
  done = 1;
}

static void doCmdNop(SupStat * stat, SupSet * set)
{
  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd nop\n");
    status_next(stat, RCS_DONE);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

static void doCmdInit(SupStat * stat, SupSet * set,
		      RCS_CMD_CHANNEL * subCmdBuf[],
		      SubStat * subStatPtr[])
{
  static int serialNumber[SUB_NUM];
  SubCmdInit subCmdInit;
  int subnum;
#define GOT_IT() (subStatPtr[subnum]->echo_serial_number == serialNumber[subnum])
#define IS_EXEC() (subStatPtr[subnum]->status == RCS_EXEC)
#define IS_ERROR() (subStatPtr[subnum]->status == RCS_ERROR)

  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd init\n");
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      serialNumber[subnum] = subStatPtr[subnum]->echo_serial_number + 1;
      subCmdInit.serial_number = serialNumber[subnum];
      subCmdBuf[subnum]->write(&subCmdInit);
    }
    status_next(stat, RCS_EXEC);
    state_next(stat, S1);
  }

  if (state_match(stat, S1)) {
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      if (! GOT_IT()) break;
      else if (IS_EXEC()) break;
      else if (IS_ERROR()) {
	status_next(stat, RCS_ERROR);
	state_next(stat, S0);
	break;
      }
      // else this one is done
    }
    if (subnum == SUB_NUM) {
      // got all the way through the loop, so all done
      status_next(stat, RCS_DONE);
      state_next(stat, S0);
    }
  } else {			// S0
    state_default(stat);
  }
#undef IS_ERROR
#undef IS_EXEC
#undef GOT_IT
}

static void doCmdAbort(SupStat * stat, SupSet * set,
		      RCS_CMD_CHANNEL * subCmdBuf[],
		      SubStat * subStatPtr[])
{
  static int serialNumber[SUB_NUM];
  SubCmdAbort subCmdAbort;
  int subnum;
#define GOT_IT() (subStatPtr[subnum]->echo_serial_number == serialNumber[subnum])
#define IS_EXEC() (subStatPtr[subnum]->status == RCS_EXEC)
#define IS_ERROR() (subStatPtr[subnum]->status == RCS_ERROR)

  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd abort\n");
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      serialNumber[subnum] = subStatPtr[subnum]->echo_serial_number + 1;
      subCmdAbort.serial_number = serialNumber[subnum];
      subCmdBuf[subnum]->write(&subCmdAbort);
    }
    status_next(stat, RCS_EXEC);
    state_next(stat, S1);
  }

  if (state_match(stat, S1)) {
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      if (! GOT_IT()) break;
      else if (IS_EXEC()) break;
      else if (IS_ERROR()) {
	status_next(stat, RCS_ERROR);
	state_next(stat, S0);
	break;
      }
      // else this one is done
    }
    if (subnum == SUB_NUM) {
      // got all the way through the loop, so all done
      status_next(stat, RCS_DONE);
      state_next(stat, S0);
    }
  } else {			// S0
    state_default(stat);
  }
#undef IS_ERROR
#undef IS_EXEC
#undef GOT_IT
}

static void doCmdHalt(SupStat * stat, SupSet * set,
		      RCS_CMD_CHANNEL * subCmdBuf[],
		      SubStat * subStatPtr[])
{
  static int serialNumber[SUB_NUM];
  SubCmdHalt subCmdHalt;
  int subnum;
#define GOT_IT() (subStatPtr[subnum]->echo_serial_number == serialNumber[subnum])
#define IS_EXEC() (subStatPtr[subnum]->status == RCS_EXEC)
#define IS_ERROR() (subStatPtr[subnum]->status == RCS_ERROR)

  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd halt\n");
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      serialNumber[subnum] = subStatPtr[subnum]->echo_serial_number + 1;
      subCmdHalt.serial_number = serialNumber[subnum];
      subCmdBuf[subnum]->write(&subCmdHalt);
    }
    status_next(stat, RCS_EXEC);
    state_next(stat, S1);
  }

  if (state_match(stat, S1)) {
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      if (! GOT_IT()) break;
      else if (IS_EXEC()) break;
      else if (IS_ERROR()) {
	status_next(stat, RCS_ERROR);
	state_next(stat, S0);
	break;
      }
      // else this one is uninitialized
    }
    if (subnum == SUB_NUM) {
      // got all the way through the loop, so all done
      status_next(stat, UNINITIALIZED_STATUS);
      state_next(stat, UNINITIALIZED_STATE);
    }
  } else {			// UNINITIALIZED_STATE
    state_default(stat);
  }
#undef IS_ERROR
#undef IS_EXEC
#undef GOT_IT
}

/*
  Here we just write shutdowns to all subordinates, and then shutdown
  ourselves without waiting for subordinates. We can't tell when they
  shut down anyway.
 */
static void doCmdShutdown(SupStat * stat, SupSet * set,
			  RCS_CMD_CHANNEL * subCmdBuf[],
			  SubStat * subStatPtr[])
{
  SubCmdShutdown subCmdShutdown;
  int subnum;

  if (state_match(stat, NEW_COMMAND)) {
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd shutdown\n");
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      subCmdShutdown.serial_number = subStatPtr[subnum]->echo_serial_number + 1;
      subCmdBuf[subnum]->write(&subCmdShutdown);
    }
    done = 1;
    state_new(stat);
    status_next(stat, RCS_EXEC);
    state_next(stat, S0);
  } else {			// S0
    state_default(stat);
  }
}

/*
  For a Task command, we do each subordinate in order, just to be
  different from Init, Abort and Halt.
 */
static void doCmdTask(SupCmdTask * cmd,
		      SupStat * stat,
		      SupSet * set,
		      RCS_CMD_CHANNEL * subCmdBuf[],
		      SubStat * subStatPtr[])
{
  static int serialNumber;
  static int subnum;
  SubCmdTask subCmdTask;

  if (state_match(stat, NEW_COMMAND)) {
    state_new(stat);
    if (set->debug & SUP_DEBUG_CMD) printf("sup cmd task %d\n", cmd->what);
    subnum = 0;
    status_next(stat, RCS_EXEC);
    state_next(stat, S1);
  }

  if (state_match(stat, S1)) {
    // here in S1 we send the command to the next subordinate
    serialNumber = subStatPtr[subnum]->echo_serial_number + 1;
    subCmdTask.serial_number = serialNumber;
    subCmdTask.what = cmd->what;
    subCmdBuf[subnum]->write(&subCmdTask);
    state_next(stat, S2);
  } else if (state_match(stat, S2)) {
    // here in S2 we wait for it to be done, increment the subordinate,
    // drop to S0 if there are no more subordinates, or go back to S1
    // if we have more to go
    if (subStatPtr[subnum]->echo_serial_number == serialNumber) {
      if (subStatPtr[subnum]->status == RCS_ERROR) {
	status_next(stat, RCS_ERROR);
	state_next(stat, S0);
      } else if (subStatPtr[subnum]->status == RCS_DONE) {
	subnum++;
	if (subnum == SUB_NUM) {
	  stat->what = cmd->what;
	  status_next(stat, RCS_DONE);
	  state_next(stat, S0);
	} else {
	  state_next(stat, S1);	// jump back up and do the next one
	}
      }
    }
  } else {			// S0
    state_default(stat);
  }
}

static void doCfgNop(SupSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    if (set->debug & SUP_DEBUG_CFG) printf("sup cfg nop\n");
    state_new(set);
    status_next(set, RCS_DONE);
    state_next(set, S0);
  } else {
    state_default(set);
  }
}

static void doCfgCycleTime(SupCfgCycleTime * cfg, SupSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    state_new(set);
    if (set->debug & SUP_DEBUG_CFG) printf("sup cfg cycle time %f\n", cfg->cycleTime);
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

static void doCfgDebug(SupCfgDebug * cfg, SupSet * set)
{
  if (state_match(set, NEW_COMMAND)) {
    state_new(set);
    if (set->debug & SUP_DEBUG_CFG) printf("sup cfg debug %d\n", cfg->debug);
    set->debug = cfg->debug;
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
  int subnum, bufnum;
  int debug;
  int retval;
  NML * skelErrBuf;
  RCS_CMD_CHANNEL * supCmdBuf;
  RCS_STAT_CHANNEL * supStatBuf;
  RCS_CMD_CHANNEL * supCfgBuf;
  RCS_STAT_CHANNEL * supSetBuf;
  RCS_CMD_CHANNEL * subCmdBuf[SUB_NUM];
  RCS_STAT_CHANNEL * subStatBuf[SUB_NUM];
  RCS_CMD_CHANNEL * subCfgBuf[SUB_NUM];
  RCS_STAT_CHANNEL * subSetBuf[SUB_NUM];
  SupCmdTask * supCmdTaskPtr;
  SupCfgCycleTime * supCfgCycleTimePtr;
  SupCfgDebug * supCfgDebugPtr;
  SupCmdNop supCmdNop;
  SupCfgNop supCfgNop;
  SupStat supStat;
  SupSet supSet;
  SubStat * subStatPtr[SUB_NUM];
#define IS_INIT() (supStat.state != UNINITIALIZED_STATE)

  debug = 0;
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
  supCmdTaskPtr = (SupCmdTask *) (supCmdBuf->get_address());

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
  supCfgCycleTimePtr = (SupCfgCycleTime *) (supCfgBuf->get_address());
  supCfgDebugPtr = (SupCfgDebug *) (supCfgBuf->get_address());

  supSetBuf = new RCS_STAT_CHANNEL(supIntf_format, "supSet", BN, nmlFile);
  if (! supSetBuf->valid()) {
    fprintf(stderr, "%s: can't open supSet buffer\n", BN);
    return 1;
  }

  for (subnum = 0, bufnum = 1; subnum < SUB_NUM; subnum++, bufnum++) {
    sprintf(chanName, "subCmd%d", bufnum);
    subCmdBuf[subnum] = new RCS_CMD_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! 
subCmdBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }

    sprintf(chanName, "subStat%d", bufnum);
    subStatBuf[subnum] = new RCS_STAT_CHANNEL(subIntf_format, chanName, BN, nmlFile);
    if (! subStatBuf[subnum]->valid()) {
      fprintf(stderr, "%s: can't open %s buffer\n", BN, chanName);
      return 1;
    }
    subStatPtr[subnum] = (SubStat *) (subStatBuf[subnum]->get_address());

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

  supCmdBuf->write(&supCmdNop);
  supCfgBuf->write(&supCfgNop);

  supStat.heartbeat = 0;
  supStat.what = 0;

  supSet.cycleTime = DEFAULT_CYCLE_TIME;
  supSet.debug = debug;

  done = 0;
  signal(SIGINT, quit);

  while (! done) {
    NMLTYPE cmdType, cfgType;
    int cmdSerialNumber, cfgSerialNumber;

    // read command
    cmdType = supCmdBuf->read();
    cmdSerialNumber = supCmdBuf->get_address()->serial_number;

    // read subordinate status
    for (subnum = 0; subnum < SUB_NUM; subnum++) {
      subStatBuf[subnum]->read();
    }

    switch (cmdType) {
    case 0:
      // no new command
      break;

    case -1:
      // comm error
      break;

    case SUP_CMD_NOP_TYPE:
    case SUP_CMD_INIT_TYPE:
    case SUP_CMD_HALT_TYPE:
    case SUP_CMD_ABORT_TYPE:
    case SUP_CMD_TASK_TYPE:
    case SUP_CMD_SHUTDOWN_TYPE:
      supStat.command_type = cmdType;
      if (cmdSerialNumber != supStat.echo_serial_number) {
	supStat.echo_serial_number = cmdSerialNumber;
	supStat.state = NEW_COMMAND;
      }
      break;

    default:
      reportError(skelErrBuf, "%s: unknown command %s", BN, supIntf_symbol_lookup(cmdType));
      break;
    } // switch (cmdType)

    // read config
    cfgType = supCfgBuf->read();
    cfgSerialNumber = supCfgBuf->get_address()->serial_number;

    switch (cfgType) {
    case 0:
      // no new command
      break;

    case -1:
      // comm error
      break;

    case SUP_CFG_NOP_TYPE:
    case SUP_CFG_CYCLE_TIME_TYPE:
    case SUP_CFG_DEBUG_TYPE:
      supSet.command_type = cfgType;
      if (cfgSerialNumber != supSet.echo_serial_number) {
	supSet.echo_serial_number = cfgSerialNumber;
	supSet.state = NEW_COMMAND;
      }
      break;

    default:
      reportError(skelErrBuf, "%s: unknown config %s", BN, supIntf_symbol_lookup(cmdType));
      break;
    } // switch (cfgType)

    // handle command state tables
    switch (supStat.command_type) {
    case SUP_CMD_NOP_TYPE:
      doCmdNop(&supStat, &supSet);
      break;

    case SUP_CMD_INIT_TYPE:
      doCmdInit(&supStat, &supSet, subCmdBuf, subStatPtr);
      break;

    case SUP_CMD_ABORT_TYPE:
      doCmdAbort(&supStat, &supSet, subCmdBuf, subStatPtr);
      break;

    case SUP_CMD_HALT_TYPE:
      doCmdHalt(&supStat, &supSet, subCmdBuf, subStatPtr);
      break;

    case SUP_CMD_SHUTDOWN_TYPE:
      doCmdShutdown(&supStat, &supSet, subCmdBuf, subStatPtr);
      break;

    case SUP_CMD_TASK_TYPE:
      doCmdTask(supCmdTaskPtr, &supStat, &supSet, subCmdBuf, subStatPtr);
      break;

    default:
      // no command to handle
      break;
    } // switch (supStat.command_type)

    // handle config state tables
    switch (supSet.command_type) {
    case SUP_CFG_NOP_TYPE:
      doCfgNop(&supSet);
      break;

    case SUP_CFG_CYCLE_TIME_TYPE:
      doCfgCycleTime(supCfgCycleTimePtr, &supSet);
      break;

    case SUP_CFG_DEBUG_TYPE:
      doCfgDebug(supCfgDebugPtr, &supSet);
      break;

    default:
      // no config to handle
      break;
    } // switch (supSet.command_type)

    supStat.heartbeat++;

    // update status and settings
    supStatBuf->write(&supStat);
    supSetBuf->write(&supSet);

    esleep(supSet.cycleTime);
  } // while (! done)

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

  rcs_print("Sup done\n");

  return 0;

#undef IS_INIT
}

/*
  Modification history:

  $Log: supMain.cc,v $
  Revision 1.2  2006/01/09 14:45:49  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:27  proctor
  Initial checkin

*/

