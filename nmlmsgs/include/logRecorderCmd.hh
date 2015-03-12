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
#ifndef LOG_RECORDER_CMD_HH
#define LOG_RECORDER_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //LOG_RECORDER_BASE
#define logRecorderCmd_name  "logRecorderCmd"

// Define the integer type ids.
typedef enum {
  LOG_RECORDER_HALT_TYPE = LOG_RECORDER_CMD_BASE,
  LOG_RECORDER_INIT_TYPE,
  LOG_RECORDER_COLLECT_DATA_TYPE,
} logRecorderCmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class LOG_RECORDER_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_RECORDER_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class LOG_RECORDER_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_RECORDER_INIT();
  // CMS Update Function
  void update(CMS *);  
};

// Command Class: Collect_Data
class LOG_RECORDER_COLLECT_DATA : public RCS_CMD_MSG
{
public:
  //Constructor
  LOG_RECORDER_COLLECT_DATA();
  // CMS Update Function
  void update(CMS *);  

  double min_time;
  double max_time;
  char label[64];
};


//! Standard NML format routine
int logRecorderCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *logRecorderCmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
