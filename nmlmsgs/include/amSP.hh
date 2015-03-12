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
  \file amSP.hh

  \brief NML declaration for the am sensor processing level.  Contains
         am level sensor command, status, set, and data messages.  

  \code CVS Status:
  $Author: tomrkramer $
  $Revision: 1.9 $
  $Date: 2008/02/11 23:33:36 $
  \endcode

  \author Fred Proctor
  \date 8 June 2005
*/

#ifndef AM_SP_HH
#define AM_SP_HH

#include "rcs.hh"
#include "moastNmlOffsets.hh"

#define AM_SP_CMD_NAME          "amSPCmd"
#define AM_SP_STAT_NAME         "amSPStat"
#define AM_SP_CFG_NAME          "amSPCfg"
#define AM_SP_SET_NAME          "amSPSet"
#define AM_SP_DATA_NAME         "amSPData"

#define AM_SP_CMD_BASE          (AM_SP_BASE)
#define AM_SP_STAT_BASE         (AM_SP_BASE + 100)
#define AM_SP_CFG_BASE          (AM_SP_BASE + 200)
#define AM_SP_SET_BASE          (AM_SP_BASE + 300)
#define AM_SP_DATA_BASE         (AM_SP_BASE + 400)

#define AM_SP_CMD_INIT_TYPE     (AM_SP_CMD_BASE + 1)
#define AM_SP_CMD_ABORT_TYPE    (AM_SP_CMD_BASE + 2)
#define AM_SP_CMD_HALT_TYPE     (AM_SP_CMD_BASE + 3)
#define AM_SP_CMD_SHUTDOWN_TYPE (AM_SP_CMD_BASE + 4)
#define AM_SP_CMD_GO_TYPE	(AM_SP_CMD_BASE + 5)
#define AM_SP_CMD_NOP_TYPE	(AM_SP_CMD_BASE + 6)

#define AM_SP_STAT_TYPE         (AM_SP_STAT_BASE + 1)

#define AM_SP_CFG_CYCLE_TIME_TYPE (AM_SP_CFG_BASE + 1)
#define AM_SP_CFG_DEBUG_TYPE      (AM_SP_CFG_BASE + 2)
#define AM_SP_CFG_DUMP_WM_TYPE    (AM_SP_CFG_BASE + 3)
#define AM_SP_CFG_NOP_TYPE        (AM_SP_CFG_BASE + 4)

#define AM_SP_SET_TYPE          (AM_SP_SET_BASE + 1)
         
#define AM_SP_DATA_1D_TYPE      (AM_SP_DATA_BASE + 1)
#define AM_SP_DATA_2D_TYPE      (AM_SP_DATA_BASE + 2)

#define AM_SP_DATA_1D_MAXSIZE   128
#define AM_SP_DATA_2D_MAXSIZE   256

#define AMSP_NOT_OBSTACLE   0.0
#define AMSP_UNSEEN         0.2
#define AMSP_MAYBE_OBSTACLE 0.4
#define AMSP_OBSTACLE       0.6
#define AMSP_PROBINIT       0.2
#define AMSP_PROBINC        0.07
#define AMSP_PROBDEC       -0.02

// AM_SP_CMD_XXX
/////////////////////////////////////////////////
class AmSPCmdInit:public RCS_CMD_MSG {
public:
  AmSPCmdInit():RCS_CMD_MSG(AM_SP_CMD_INIT_TYPE,
				   sizeof(AmSPCmdInit)) {};
  void update(CMS *);
};

class AmSPCmdAbort:public RCS_CMD_MSG {
public:
  AmSPCmdAbort():RCS_CMD_MSG(AM_SP_CMD_ABORT_TYPE,
				    sizeof(AmSPCmdAbort)) {};
  void update(CMS *);
};

class AmSPCmdHalt:public RCS_CMD_MSG {
public:
  AmSPCmdHalt():RCS_CMD_MSG(AM_SP_CMD_HALT_TYPE,
				   sizeof(AmSPCmdHalt)) {};
  void update(CMS *);
};

class AmSPCmdShutdown:public RCS_CMD_MSG {
public:
  AmSPCmdShutdown():RCS_CMD_MSG(AM_SP_CMD_SHUTDOWN_TYPE,
				   sizeof(AmSPCmdShutdown)) {};
  void update(CMS *);
};

class AmSPCmdGo:public RCS_CMD_MSG {
public:
  AmSPCmdGo():RCS_CMD_MSG(AM_SP_CMD_GO_TYPE,
				   sizeof(AmSPCmdGo)) {};
  void update(CMS *);
};

class AmSPCmdNop:public RCS_CMD_MSG {
public:
  AmSPCmdNop():RCS_CMD_MSG(AM_SP_CMD_NOP_TYPE,
				   sizeof(AmSPCmdNop)) {};
  void update(CMS *);
};

// AM_SP_STAT
//////////////////////////////////////////////
class AmSPStat : public RCS_STAT_MSG_V2 {
public:
  AmSPStat() : RCS_STAT_MSG_V2(AM_SP_STAT_TYPE, sizeof(AmSPStat)) {};
  void update(CMS *);
};

// AM_SP_CFG_XXX
//////////////////////////////////////////////////////

/***********************************************************************/

/*!  AmSPCfgCycleTime is a configuration command class that
     adujsts the cycle time.

*/

class AmSPCfgCycleTime:public RCS_CMD_MSG {
public:
  AmSPCfgCycleTime():RCS_CMD_MSG(AM_SP_CFG_CYCLE_TIME_TYPE,
			       sizeof(AmSPCfgCycleTime)) {};
  void update(CMS *);

  //! period in seconds
  double cycleTime;
};

/***********************************************************************/

/*!  AmSPCfgDebug is a configuration command class that
     adujsts the AM SP debugging level.

*/

class AmSPCfgDebug:public RCS_CMD_MSG {
public:
  AmSPCfgDebug():RCS_CMD_MSG(AM_SP_CFG_DEBUG_TYPE,
			       sizeof(AmSPCfgDebug)) {};
  void update(CMS *);

  //! debug level
  int debug;
};

/***********************************************************************/

/*!  AmSPCfgDumpWM is a configuration command class that
     sets graphical output of the current world view to
     off or show height map or show obstacle map.

*/

class AmSPCfgDumpWM:public RCS_CMD_MSG {
public:
  AmSPCfgDumpWM():RCS_CMD_MSG(AM_SP_CFG_DUMP_WM_TYPE,
				 sizeof(AmSPCfgDumpWM)) {};
  void update(CMS *);

  //! turn on or off output (true on, false off)
  int level;
};

/***********************************************************************/

//!  AmSPCfgNop is a configuration command class that does nothing

class AmSPCfgNop:public RCS_CMD_MSG {
public:
  AmSPCfgNop():RCS_CMD_MSG(AM_SP_CFG_NOP_TYPE, sizeof(AmSPCfgNop)) {};
  void update(CMS *);

};

/***********************************************************************/

/*! AmSPSet is the configuration status class for AM SP.
    It holds settings that change infrequently and in response
    to configuration commands.
*/
 
class AmSPSet : public RCS_STAT_MSG {
public:
  AmSPSet():RCS_STAT_MSG(AM_SP_SET_TYPE, sizeof(AmSPSet)) {};
  void update(CMS *);

  double cycleTime;
  int debug;
  int dumpLevel;
};

/***********************************************************************/

extern int amSP_format(NMLTYPE type, void *buf, CMS * cms);
extern const char * amSP_symbol_lookup(long type);
extern const char * amSP_enum_RCS_ADMIN_STATE_symbol_lookup(long v);

/***********************************************************************/

#endif // AM_SP_HH

/*
  Modification history:

  $Log: amSP.hh,v $
  Revision 1.9  2008/02/11 23:33:36  tomrkramer
  Since there are now three choices of dump level, changed "bool turnOn"
  to "int level" and changed "bool dumpWM" to "int dumpLevel".

  Revision 1.8  2006/09/25 19:02:40  dr_steveb
  Moved location of am map probabilitys to avoid multiple declarations of "CELL_NAME"

  Revision 1.7  2006/06/21 19:38:13  proctor
  Added support for Java, which involved including some left-out headers
  like moastNmlOffsets.hh and moastTypes.hh

  Revision 1.6  2006/04/19 22:36:53  tomrkramer
  Removed heartbeat from AmSPStat class (use tt.count instead).

  Revision 1.5  2006/04/07 16:35:13  tomrkramer
  Added extern declaration of amSP_enum_RCS_ADMIN_STATE_symbol_lookup
  used in amShell. Improved documentation of AmSPSet.

  Revision 1.4  2006/04/05 13:50:42  tomrkramer
  Switched using RCS_STAT_MSG_V2 and NOP.

  Revision 1.3  2005/11/16 14:04:18  tomrkramer
  added NoOp and changed handling of hits and probabilities

  Revision 1.2  2005/10/23 15:41:07  dr_steveb
  Changes to add dump command to amSP and adjust for its new resolution.

  Revision 1.1.1.1  2005/10/12 20:50:32  root
  initial import

  Revision 1.2  2005/06/15 13:31:04  beytin
  Updated so amSP now has a go command.

  Revision 1.1  2005/06/08 19:52:12  proctor
  Initial checkin

*/
