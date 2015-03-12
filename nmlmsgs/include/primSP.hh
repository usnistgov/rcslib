/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

/*!
  \file primSP.hh

  \brief NML declaration for the prim sensor processing level.  Contains
         prim level sensor command, status, set, and data messages.  

  \code CVS Status:
  $Author: tomrkramer $
  $Revision: 1.6 $
  $Date: 2008/02/11 23:46:08 $
  \endcode

  \author Fred Proctor
  \date 8 June 2005
*/

#ifndef PRIM_SP_HH
#define PRIM_SP_HH

#include "moastNmlOffsets.hh"

#define PRIM_SP_CMD_NAME          "primSPCmd"
#define PRIM_SP_STAT_NAME         "primSPStat"
#define PRIM_SP_CFG_NAME          "primSPCfg"
#define PRIM_SP_SET_NAME          "primSPSet"
#define PRIM_SP_DATA_NAME         "primSPData"

#define PRIM_SP_CMD_BASE          (PRIM_SP_BASE)
#define PRIM_SP_STAT_BASE         (PRIM_SP_BASE + 100)
#define PRIM_SP_CFG_BASE          (PRIM_SP_BASE + 200)
#define PRIM_SP_SET_BASE          (PRIM_SP_BASE + 300)
#define PRIM_SP_DATA_BASE         (PRIM_SP_BASE + 400)

#define PRIM_SP_CMD_NOP_TYPE      (PRIM_SP_CMD_BASE + 1)
#define PRIM_SP_CMD_INIT_TYPE     (PRIM_SP_CMD_BASE + 2)
#define PRIM_SP_CMD_ABORT_TYPE    (PRIM_SP_CMD_BASE + 3)
#define PRIM_SP_CMD_HALT_TYPE     (PRIM_SP_CMD_BASE + 4)
#define PRIM_SP_CMD_SHUTDOWN_TYPE (PRIM_SP_CMD_BASE + 5)
#define PRIM_SP_CMD_GO_TYPE       (PRIM_SP_CMD_BASE + 6)

#define PRIM_SP_STAT_TYPE         (PRIM_SP_STAT_BASE + 1)

#define PRIM_SP_CFG_NOP_TYPE        (PRIM_SP_CFG_BASE + 1)
#define PRIM_SP_CFG_CYCLE_TIME_TYPE (PRIM_SP_CFG_BASE + 2)
#define PRIM_SP_CFG_DEBUG_TYPE      (PRIM_SP_CFG_BASE + 3)

#define PRIM_SP_SET_TYPE          (PRIM_SP_SET_BASE + 1)
         
#define PRIM_SP_DATA_1D_TYPE      (PRIM_SP_DATA_BASE + 1)
#define PRIM_SP_DATA_2D_TYPE      (PRIM_SP_DATA_BASE + 2)

#define PRIM_SP_DATA_1D_MAXSIZE   128
#define PRIM_SP_DATA_2D_MAXSIZE   256

 
// PRIM_SP_CMD_XXX
/////////////////////////////////////////////////
class PrimSPCmdNop:public RCS_CMD_MSG {
public:
  PrimSPCmdNop():RCS_CMD_MSG(PRIM_SP_CMD_NOP_TYPE,
			      sizeof(PrimSPCmdNop)) {};
  void update(CMS *);
};

class PrimSPCmdInit:public RCS_CMD_MSG {
public:
  PrimSPCmdInit():RCS_CMD_MSG(PRIM_SP_CMD_INIT_TYPE,
				   sizeof(PrimSPCmdInit)) {};
  void update(CMS *);
};

class PrimSPCmdAbort:public RCS_CMD_MSG {
public:
  PrimSPCmdAbort():RCS_CMD_MSG(PRIM_SP_CMD_ABORT_TYPE,
				    sizeof(PrimSPCmdAbort)) {};
  void update(CMS *);
};

class PrimSPCmdHalt:public RCS_CMD_MSG {
public:
  PrimSPCmdHalt():RCS_CMD_MSG(PRIM_SP_CMD_HALT_TYPE,
				   sizeof(PrimSPCmdHalt)) {};
  void update(CMS *);
};

class PrimSPCmdShutdown:public RCS_CMD_MSG {
public:
  PrimSPCmdShutdown():RCS_CMD_MSG(PRIM_SP_CMD_SHUTDOWN_TYPE,
				   sizeof(PrimSPCmdShutdown)) {};
  void update(CMS *);
};

class PrimSPCmdGo:public RCS_CMD_MSG {
public:
  PrimSPCmdGo():RCS_CMD_MSG(PRIM_SP_CMD_GO_TYPE,
				   sizeof(PrimSPCmdGo)) {};
  void update(CMS *);
  double outputCycleTime;
};

// PRIM_SP_STAT
//////////////////////////////////////////////

class PrimSPStat : public RCS_STAT_MSG_V2 {
public:
  PrimSPStat() : RCS_STAT_MSG_V2(PRIM_SP_STAT_TYPE, sizeof(PrimSPStat)) {};
  void update(CMS *);
};

// PRIM_SP_CFG_XXX
//////////////////////////////////////////////////////
class PrimSPCfgNop:public RCS_CMD_MSG {
public:
  PrimSPCfgNop():RCS_CMD_MSG(PRIM_SP_CFG_NOP_TYPE,
     sizeof(PrimSPCfgNop)) {};
  void update(CMS *);
};

class PrimSPCfgCycleTime:public RCS_CMD_MSG {
public:
  PrimSPCfgCycleTime():RCS_CMD_MSG(PRIM_SP_CFG_CYCLE_TIME_TYPE,
			       sizeof(PrimSPCfgCycleTime)) {};
  void update(CMS *);

  //! period in seconds
  double cycleTime;
};

class PrimSPCfgDebug:public RCS_CMD_MSG {
public:
  PrimSPCfgDebug():RCS_CMD_MSG(PRIM_SP_CFG_DEBUG_TYPE,
			       sizeof(PrimSPCfgDebug)) {};
  void update(CMS *);

  //! debug level
  int debug;
};

// PRIM_SP_SET
//////////////////////////////////////////////////////
class PrimSPSet : public RCS_STAT_MSG {
public:
  PrimSPSet():RCS_STAT_MSG(PRIM_SP_SET_TYPE, sizeof(PrimSPSet)) {};
  void update(CMS *);

  double cycleTime;
  int debug;
};

extern int primSP_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * primSP_symbol_lookup(long type);

#endif // PRIM_SP_HH

/*
  Modification history:

  $Log: primSP.hh,v $
  Revision 1.6  2008/02/11 23:46:08  tomrkramer
  Changed PrimSPCmdGo by removing (useless) attribute foo and replacing it
  with outputCycleTime, which tells primSP how long to wait between writing
  output data.

  Revision 1.5  2006/06/21 19:38:13  proctor
  Added support for Java, which involved including some left-out headers
  like moastNmlOffsets.hh and moastTypes.hh

  Revision 1.4  2006/04/19 22:35:06  tomrkramer
  Deleted heartbeat from PrimSPStat class (use tt.count instead).

  Revision 1.3  2006/04/05 13:48:24  tomrkramer
  Switched to using RCS_STAT_MSG_V2.

  Revision 1.1.1.1  2005/10/12 20:50:32  root
  initial import

  Revision 1.2  2005/06/15 13:30:48  beytin
  Updated so primSP now has a go command

  Revision 1.1  2005/06/08 19:50:28  proctor
  Initial checkin

*/
