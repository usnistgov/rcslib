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
  \file moastNmlOffsets.hh

  \brief Starting numbers for MOAST NMLtype declarations

  \code CVS Status:
  $Author: wshackle $
  $Revision: 1.14 $
  $Date: 2010/06/17 17:51:22 $
  \endcode

  \author Fred Proctor
*/

#ifndef MOAST_NML_OFFSETS_HH
#define MOAST_NML_OFFSETS_HH

//! used by moastError.hh
#define MOAST_ERROR_BASE 100

//! used by navDataExt.hh
#define NAV_DATA_EXT_BASE    1000

//! used by servoSP.hh
#define SERVO_SP_BASE        2000

//! used by servoMisJA.hh
#define SERVO_MIS_JA_BASE    3000

//! used by servoMobJA.hh
#define SERVO_MOB_JA_BASE    4000

//! used by servoEffJA.hh
#define SERVO_EFF_JA_BASE    5000

//! used by primSP.hh
#define PRIM_SP_BASE         6000

//! used by primMisJA.hh
#define PRIM_MIS_JA_BASE     7000

//! used by primMobJA.hh
#define PRIM_MOB_JA_BASE     8000       

//! used by amSP.hh
#define AM_SP_BASE           9000

//! used by amMisJA.hh
#define AM_MIS_JA_BASE       10000

//! used by amMobJA.hh
#define AM_MOB_JA_BASE      11000

//! used by vehSP.hh
#define VEH_SP_BASE         12000

//! used by vehMobPL.hh
#define VEH_MOB_PL_BASE     13000

#define SENSOR_DATA_BASE    14000

//! symbolicData.hh
#define SYMBOLIC_DATA_BASE  15000

//! used by sectMobPL.hh
#define SECT_MOB_PL_BASE    16000

//! used by trafficData.hh
#define TRAFFIC_DATA_BASE   17000

//! used by trafficCtrl.hh
#define TRAFFIC_CTRL_BASE   17500

//! used by joystickData.hh
#define JOYSTICK_DATA_BASE 18000

//! user by linkmonitor.hh
#define LINK_MON_BASE      19000

//! used by factory controller
#define SERVO_FACT_JA_BASE 20000

// FIXME-- obsolete
#define FACT_CTRL_BASE SERVO_FACT_JA_BASE

//! used by ATRV servo module for testing/tuning only
#define ATRV_TUNE_BASE 22000

//! used by nav200 module for testing/tuning only
#define NAV200DBG_BASE 23000

//! used by primAtrv module for testing/tuning only
#define PRIM_DEBUG_BASE 24000

//! used by moastLogRecorder module for testing/tuning only
#define MOAST_LOG_RECORDER_STS_BASE 25000

//! used by moastLogRecorder module for testing/tuning only
#define MOAST_LOG_RECORDER_CMD_BASE 26000

//! used by humanTrack software 
#define HUMAN_TRACKING_BASE 27000

//! used by humanTrack software large output primarily for debugging.
#define HUMAN_TRACKING_DEBUG_BASE 27100

#endif // MOAST_NML_OFFSETS_HH

/*
  Modification history:

  $Log: moastNmlOffsets.hh,v $
  Revision 1.14  2010/06/17 17:51:22  wshackle
  add humanTracking dir and source code for human tracking using sick linescan data, change default primAtrv algorithm to A4, add quick hack in hopes of making docking not turn into the last point

  Revision 1.13  2010/03/10 15:22:52  wshackle
  make LOG_RECORDER_STS_BASE and LOG_RECORDER_CMD_BASE MOAST specific to not conflict with c-crada_repos versions

  Revision 1.12  2010/03/07 22:54:13  wshackle
  add logRecorder stuff

  Revision 1.11  2010/01/18 18:10:38  wshackle
  add projected points to primDebug, enable message to atrvTune

  Revision 1.10  2009/12/10 19:01:41  wshackle
  add nav200Dbg

  Revision 1.9  2009/10/08 13:26:10  wshackle
  add ATRV sensoray PCI Card servo see doc/ATRV_HOWTO.txt, remove configure check for player

  Revision 1.8  2009/06/02 20:40:28  proctor
  Added servoFactJA to the menagerie of servo-level things. Previously it was
  its own echelon, but it belonged at the simware interface for servo. This
  meant touching all the NML files, Java files and the simware code.

  Revision 1.7  2009/02/17 21:15:43  dr_steveb
  Added factory controller

  Revision 1.6  2007/11/14 01:54:19  cj_scrapper
  Adding servo Effecter

  Revision 1.5  2007/09/11 20:27:43  dr_steveb
  Changes for link monitoring

  Revision 1.4  2007/07/24 18:38:22  proctor
  Added JoystickData

  Revision 1.3  2007/04/17 17:13:35  dr_steveb
  Added traffic simulation offsets

  Revision 1.2  2006/04/24 21:00:02  proctor
  Added MOAST error NML buffer

  Revision 1.1.1.1  2005/10/12 20:50:32  root
  initial import

  Revision 1.14  2005/10/09 14:36:39  stephen
  Added offsets for new section level.

  Revision 1.13  2005/07/18 21:16:28  proctor
  Replaced sensorData with servoSPLinescan, ...

  Revision 1.12  2005/06/20 19:38:36  stephen
  Added symbolicData header file for high-level sensor processing.

  Revision 1.11  2005/06/08 20:49:35  stephen
  Changed occupancyData to sensorData

  Revision 1.10  2005/06/08 14:29:54  proctor
  Added vehMobPLMain name change

  Revision 1.9  2005/06/07 21:45:11  proctor
  Changed servoPL to servoMobJA

  Revision 1.8  2005/06/07 15:54:01  proctor
  Added amMis buffers

  Revision 1.7  2005/06/07 15:45:22  proctor
  Added servo,primMis buffers

  Revision 1.6  2005/06/06 15:30:08  stephen
  Fixed mission channel defines.

  Revision 1.5  2005/05/17 17:04:55  stephen
  Added VEH_PL_BASE.

  Revision 1.4  2005/05/11 20:01:46  proctor
  Added PRIM_PL,EX_BASE

  Revision 1.3  2005/05/10 13:41:07  stephen
  Made offsets comply with our meeting notes. Added offset for occupancyData channel.

  Revision 1.2  2005/05/05 18:04:28  proctor
  Changed command to cmd, status to stat, etc.

*/
