/* 
This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST RCS intelligent mobility software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.
*/ 

// Author Tsai Hong

// Prevent Multiple Inclusion
#ifndef LOG_PLAYBACK_CMD_HH
#define LOG_PLAYBACK_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //LOG_PLAYBACK_BASE
#define Log_Playback_Cmd_name  "Log_Playback_Cmd"

// Define the integer type ids.
typedef enum {
  LOG_PLAYBACK_HALT_TYPE = LOG_PLAYBACK_CMD_BASE,
  LOG_PLAYBACK_INIT_TYPE,
  LOG_PLAYBACK_PAUSE_TYPE,
  LOG_PLAYBACK_RESUME_TYPE,
  LOG_PLAYBACK_STEP_TYPE,
  LOG_PLAYBACK_GOTO_TIME_TYPE,
  LOG_PLAYBACK_GOTO_PERCENT_TYPE,
} Log_Playback_CmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class LOG_PLAYBACK_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class LOG_PLAYBACK_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_INIT();
  // CMS Update Function
  void update(CMS *);  
};

// Command Class: Pause
class LOG_PLAYBACK_PAUSE : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_PAUSE();
  // CMS Update Function
  void update(CMS *);  
};

// Command Class: Resume
class LOG_PLAYBACK_RESUME : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_RESUME();
  // CMS Update Function
  void update(CMS *);  
};

// Command Class: Resume
class LOG_PLAYBACK_STEP: public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_STEP();
  // CMS Update Function
  void update(CMS *);

  int num_steps;
};

// Command Class: Resume
class LOG_PLAYBACK_GOTO_TIME: public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_GOTO_TIME();
  // CMS Update Function
  void update(CMS *);

  double time_since_start;
};

class LOG_PLAYBACK_GOTO_PERCENT: public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_PLAYBACK_GOTO_PERCENT();
  // CMS Update Function
  void update(CMS *);

  double percent;
};


//! Standard NML format routine
int logPlaybackCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *logPlaybackCmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
