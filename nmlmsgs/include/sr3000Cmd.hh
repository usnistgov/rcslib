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
#ifndef SR3000_CMD_HH
#define SR3000_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //SR3000_BASE
#include "definesSR.h"

#define DEFINES_SR_H

#define SR3000Cmd_name  "sr3000Cmd"

// Define the integer type ids.
typedef enum {
  SR3000_CMD_HALT_TYPE = SR3000_CMD_BASE,
  SR3000_CMD_INIT_TYPE,
  SR3000_CMD_SET_DISTANCE_OFFSET_TYPE,
  SR3000_CMD_SET_AMPLITUDE_THRESHOLD_TYPE,
  SR3000_CMD_SET_SATURATION_THRESHOLD_TYPE,
  SR3000_CMD_SET_INTEGRATION_TIME_TYPE,
  SR3000_CMD_SET_MODULATION_FREQUENCY_TYPE,
  SR3000_CMD_SET_ACQUIRE_MODE_TYPE,
} Sr3000CmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class SR3000_CMD_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class SR3000_CMD_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_INIT();
  // CMS Update Function
  void update(CMS *);
  
};

// Command Class: Config

class SR3000_CMD_SET_INTEGRATION_TIME : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_INTEGRATION_TIME();

  // CMS Update Function
  void update(CMS *);
  double sr3000_integrationTime;
};

class SR3000_CMD_SET_MODULATION_FREQUENCY : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_MODULATION_FREQUENCY();

  // CMS Update Function
  void update(CMS *);
  enum ModulationFrq sr3000_modulationFrequency;
};


class SR3000_CMD_SET_AMPLITUDE_THRESHOLD : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_AMPLITUDE_THRESHOLD();

  // CMS Update Function
  void update(CMS *);
  double sr3000_amplitudeThreshold;
};


class SR3000_CMD_SET_SATURATION_THRESHOLD : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_SATURATION_THRESHOLD();

  // CMS Update Function
  void update(CMS *);
  double sr3000_saturationThreshold;
};

class SR3000_CMD_SET_DISTANCE_OFFSET : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_DISTANCE_OFFSET();

  // CMS Update Function
  void update(CMS *);
  double sr3000_distanceOffset;
};


class SR3000_CMD_SET_ACQUIRE_MODE : public RCS_CMD_MSG
{
public:
  //Constructor
  SR3000_CMD_SET_ACQUIRE_MODE();

  // CMS Update Function
  void update(CMS *);

  // acquire_mode  should be one of 3 possible options bitwise OR'd together
  // 0x1 fix pattern correction, 0x2 led non lin, 0x4 3 by 3 median filter
  // See SR_Acquire() description. 
  int acquire_mode;
};


//! Standard NML format routine
int sr3000Cmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *sr3000Cmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
