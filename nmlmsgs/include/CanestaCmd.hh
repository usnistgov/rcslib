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
#ifndef CANESTA_CMD_HH
#define CANESTA_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //CANESTA_BASE
#define CanestaCmd_name  "CanestaCmd"

// Define the integer type ids.
typedef enum {
  CANESTA_CMD_HALT_TYPE = CANESTA_CMD_BASE,
  CANESTA_CMD_INIT_TYPE,
  CANESTA_CMD_SET_MODULATION_FREQUENCY_TYPE,
  CANESTA_CMD_SET_CMR_COUNT_TYPE,
  CANESTA_CMD_SET_SHUTTER_TIME_TYPE,
  CANESTA_CMD_CONFIG_TYPE,
} CANESTACmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class CANESTA_CMD_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_HALT();

  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class CANESTA_CMD_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_INIT();
  // CMS Update Function
  void update(CMS *);
  
};

class CANESTA_CMD_SET_MODULATION_FREQUENCY : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_SET_MODULATION_FREQUENCY();

  // CMS Update Function
  void update(CMS *);

  double modulation_frequency;
};


class CANESTA_CMD_SET_SHUTTER_TIME : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_SET_SHUTTER_TIME();

  // CMS Update Function
  void update(CMS *);

  double shutter_time;
};

class CANESTA_CMD_SET_CMR_COUNT : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_SET_CMR_COUNT();

  // CMS Update Function
  void update(CMS *);

  int cmr_count;
};


class CANESTA_CMD_CONFIG : public RCS_CMD_MSG
{
public:
  //Constructor
  CANESTA_CMD_CONFIG();

  // CMS Update Function
  void update(CMS *);
  

  double modulation_frequency;
  double shutter_time;
  int cmr_count;
};


//! Standard NML format routine
int CanestaCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *CanestaCmd_symbol_lookup(long type);

#endif

