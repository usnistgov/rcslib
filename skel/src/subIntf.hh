/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#ifndef SUB_INTF_HH
#define SUB_INTF_HH

#include <rcs.hh>		// RCS_STAT_MSG, NMLTYPE, CMS
#include "skel.hh"		// SKEL_CMD,STAT,CFG,SET_BASE

#define SUB_CMD_NOP_TYPE      (SUB_CMD_BASE + 1)
#define SUB_CMD_INIT_TYPE     (SUB_CMD_BASE + 2)
#define SUB_CMD_ABORT_TYPE    (SUB_CMD_BASE + 3)
#define SUB_CMD_HALT_TYPE     (SUB_CMD_BASE + 4)
#define SUB_CMD_SHUTDOWN_TYPE (SUB_CMD_BASE + 5)
#define SUB_CMD_TASK_TYPE     (SUB_CMD_BASE + 6)

#define SUB_STAT_TYPE (SUB_STAT_BASE + 1)

#define SUB_CFG_NOP_TYPE        (SUB_CFG_BASE + 1)
#define SUB_CFG_CYCLE_TIME_TYPE (SUB_CFG_BASE + 2)
#define SUB_CFG_DEBUG_TYPE      (SUB_CFG_BASE + 3)

#define SUB_SET_TYPE (SUB_SET_BASE + 1)

class SubCmdNop : public RCS_CMD_MSG {
public:
  SubCmdNop() : RCS_CMD_MSG(SUB_CMD_NOP_TYPE, sizeof(SubCmdNop)) {};
  void update(CMS *);
};

class SubCmdInit : public RCS_CMD_MSG {
public:
  SubCmdInit() : RCS_CMD_MSG(SUB_CMD_INIT_TYPE, sizeof(SubCmdInit)) {};
  void update(CMS *);
};

class SubCmdAbort : public RCS_CMD_MSG {
public:
  SubCmdAbort() : RCS_CMD_MSG(SUB_CMD_ABORT_TYPE, sizeof(SubCmdAbort)) {};
  void update(CMS *);
};

class SubCmdHalt : public RCS_CMD_MSG {
public:
  SubCmdHalt() : RCS_CMD_MSG(SUB_CMD_HALT_TYPE, sizeof(SubCmdHalt)) {};
  void update(CMS *);
};

class SubCmdShutdown : public RCS_CMD_MSG {
public:
  SubCmdShutdown() : RCS_CMD_MSG(SUB_CMD_SHUTDOWN_TYPE, sizeof(SubCmdShutdown)) {};
  void update(CMS *);
};

class SubCmdTask : public RCS_CMD_MSG {
public:
  SubCmdTask() : RCS_CMD_MSG(SUB_CMD_TASK_TYPE, sizeof(SubCmdTask)) {};
  void update(CMS *);

  int what;
};

class SubStat : public RCS_STAT_MSG {
public:
  SubStat() : RCS_STAT_MSG(SUB_STAT_TYPE, sizeof(SubStat)) {};
  void update(CMS *);

  int heartbeat;
  int what;
};

class SubCfgNop : public RCS_CMD_MSG {
public:
  SubCfgNop() : RCS_CMD_MSG(SUB_CFG_NOP_TYPE, sizeof(SubCfgNop)) {};
  void update(CMS *);
};

class SubCfgCycleTime : public RCS_CMD_MSG {
public:
  SubCfgCycleTime() : RCS_CMD_MSG(SUB_CFG_CYCLE_TIME_TYPE, sizeof(SubCfgCycleTime)) {};
  void update(CMS *);

  double cycleTime;
};

// debug masks for subordinate's debug printing options
enum {
  SUB_DEBUG_CMD = 0x01,
  SUB_DEBUG_CFG = 0x02
};

class SubCfgDebug : public RCS_CMD_MSG {
public:
  SubCfgDebug() : RCS_CMD_MSG(SUB_CFG_DEBUG_TYPE, sizeof(SubCfgDebug)) {};
  void update(CMS *);

  int debug;
};

class SubSet : public RCS_STAT_MSG {
public:
  SubSet() : RCS_STAT_MSG(SUB_SET_TYPE, sizeof(SubSet)) {};
  void update(CMS *);

  double cycleTime;
  int id;
  int debug;
};

// from NML code generator
extern int subIntf_format(NMLTYPE type, void *buffer, CMS *cms);
extern const char *subIntf_symbol_lookup(long type);

/*
  Modification history:

  $Log: subIntf.hh,v $
  Revision 1.2  2006/01/09 14:45:48  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:26  proctor
  Initial checkin

*/

#endif
