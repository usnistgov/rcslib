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

// Prevent Multiple Inclusion
#ifndef PMD_CMD_HH
#define PMD_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //PMD_BASE
#define PMDCmd_name  "PMDCmd"

// Define the integer type ids.
typedef enum {
  PMD_CMD_HALT_TYPE = PMD_CMD_BASE,
  PMD_CMD_INIT_TYPE,
  PMD_CMD_SET_MODULATION_FREQUENCY_TYPE,
  PMD_CMD_SET_RANGE_OFFSET_TYPE,
  PMD_CMD_SET_INTEGRATION_TIME_TYPE,
  PMD_CMD_CONFIG_TYPE,
} PMDCmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class PMD_CMD_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class PMD_CMD_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_INIT();
  // CMS Update Function
  void update(CMS *);
  
};

// Command Class: Config

class PMD_CMD_SET_INTEGRATION_TIME : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_SET_INTEGRATION_TIME():
  RCS_CMD_MSG(  PMD_CMD_SET_INTEGRATION_TIME_TYPE, sizeof(PMD_CMD_SET_INTEGRATION_TIME)){};
  // CMS Update Function
  void update(CMS *);

  int ladar_id;
  unsigned short integration_time;
};


class PMD_CMD_SET_MODULATION_FREQUENCY : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_SET_MODULATION_FREQUENCY():
  RCS_CMD_MSG(  PMD_CMD_SET_MODULATION_FREQUENCY_TYPE, sizeof(PMD_CMD_SET_MODULATION_FREQUENCY)){};
  // CMS Update Function
  void update(CMS *);

  int ladar_id;
  double modulation_frequency;
};


class PMD_CMD_SET_RANGE_OFFSET : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_SET_RANGE_OFFSET():
  RCS_CMD_MSG(  PMD_CMD_SET_RANGE_OFFSET_TYPE, sizeof(PMD_CMD_SET_RANGE_OFFSET)){};
  // CMS Update Function
  void update(CMS *);

  float range_offset;
};


class PMD_CMD_CONFIG : public RCS_CMD_MSG
{
public:
  //Constructor
  PMD_CMD_CONFIG():
  RCS_CMD_MSG(  PMD_CMD_CONFIG_TYPE, sizeof(PMD_CMD_CONFIG)){};
  // CMS Update Function
  void update(CMS *);
  
  int ladar_id;
  unsigned short integration_time;
  double modulation_frequency;
  float range_offset;
};


//! Standard NML format routine
int PMDCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *PMDCmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
