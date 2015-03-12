/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#ifndef SUP_INTF_HH
#define SUP_INTF_HH

#include <rcs.hh>		// RCS_STAT_MSG, NMLTYPE, CMS
#include "skel.hh"		// SKEL_CMD,STAT,CFG,SET_BASE
#include "skelIntf.hh"		// RCS_ADSTAT_MSG

#define SUP_CMD_NOP_TYPE      (SUP_CMD_BASE + 1)
#define SUP_CMD_INIT_TYPE     (SUP_CMD_BASE + 2)
#define SUP_CMD_ABORT_TYPE    (SUP_CMD_BASE + 3)
#define SUP_CMD_HALT_TYPE     (SUP_CMD_BASE + 4)
#define SUP_CMD_SHUTDOWN_TYPE (SUP_CMD_BASE + 5)
#define SUP_CMD_TASK_TYPE     (SUP_CMD_BASE + 6)

#define SUP_STAT_TYPE (SUP_STAT_BASE + 1)

#define SUP_CFG_NOP_TYPE        (SUP_CFG_BASE + 1)
#define SUP_CFG_CYCLE_TIME_TYPE (SUP_CFG_BASE + 2)
#define SUP_CFG_DEBUG_TYPE      (SUP_CFG_BASE + 3)

#define SUP_SET_TYPE (SUP_SET_BASE + 1)

class SupCmdNop : public RCS_CMD_MSG {
public:
  SupCmdNop() : RCS_CMD_MSG(SUP_CMD_NOP_TYPE, sizeof(SupCmdNop)) {};
  void update(CMS *);
};

class SupCmdInit : public RCS_CMD_MSG {
public:
  SupCmdInit() : RCS_CMD_MSG(SUP_CMD_INIT_TYPE, sizeof(SupCmdInit)) {};
  void update(CMS *);
};

class SupCmdAbort : public RCS_CMD_MSG {
public:
  SupCmdAbort() : RCS_CMD_MSG(SUP_CMD_ABORT_TYPE, sizeof(SupCmdAbort)) {};
  void update(CMS *);
};

class SupCmdHalt : public RCS_CMD_MSG {
public:
  SupCmdHalt() : RCS_CMD_MSG(SUP_CMD_HALT_TYPE, sizeof(SupCmdHalt)) {};
  void update(CMS *);
};

class SupCmdShutdown : public RCS_CMD_MSG {
public:
  SupCmdShutdown() : RCS_CMD_MSG(SUP_CMD_SHUTDOWN_TYPE, sizeof(SupCmdShutdown)) {};
  void update(CMS *);
};

class SupCmdTask : public RCS_CMD_MSG {
public:
  SupCmdTask() : RCS_CMD_MSG(SUP_CMD_TASK_TYPE, sizeof(SupCmdTask)) {};
  void update(CMS *);

  int what;
};

class SupStat : public RCS_STAT_MSG {
public:
  SupStat() : RCS_STAT_MSG(SUP_STAT_TYPE, sizeof(SupStat)) {};
  void update(CMS *);

  int heartbeat;
  int what;
};

class SupCfgNop : public RCS_CMD_MSG {
public:
  SupCfgNop() : RCS_CMD_MSG(SUP_CFG_NOP_TYPE, sizeof(SupCfgNop)) {};
  void update(CMS *);
};

class SupCfgCycleTime : public RCS_CMD_MSG {
public:
  SupCfgCycleTime() : RCS_CMD_MSG(SUP_CFG_CYCLE_TIME_TYPE, sizeof(SupCfgCycleTime)) {};
  void update(CMS *);

  double cycleTime;
};

// debug masks for supervisor's debug printing options
enum {
  SUP_DEBUG_CMD = 0x01,
  SUP_DEBUG_CFG = 0x02
};

class SupCfgDebug : public RCS_CMD_MSG {
public:
  SupCfgDebug() : RCS_CMD_MSG(SUP_CFG_DEBUG_TYPE, sizeof(SupCfgDebug)) {};
  void update(CMS *);

  int debug;
};

class SupSet : public RCS_STAT_MSG {
public:
  SupSet() : RCS_STAT_MSG(SUP_SET_TYPE, sizeof(SupSet)) {};
  void update(CMS *);

  double cycleTime;
  int debug;
};

// from NML code generator
extern int supIntf_format(NMLTYPE type, void *buffer, CMS *cms);
extern const char *supIntf_symbol_lookup(long type);

/*
  Modification history:

  $Log: supIntf.hh,v $
  Revision 1.2  2006/01/09 14:45:48  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:27  proctor
  Initial checkin

*/

#endif
