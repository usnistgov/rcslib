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

#ifndef SICKSAFETY_CMD_HH
#define SICKSAFETY_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //SICKSAFETY_BASE
#define SICKSAFETYCmd_name  "sicksafetyCmd"

// Define the integer type ids.
typedef enum {
  SICKSAFETY_CMD_HALT_TYPE = SICKSAFETY_CMD_BASE,
  SICKSAFETY_CMD_INIT_TYPE,
  SICKSAFETY_CMD_SET_DISTANCEOFFSET_TYPE,
} SicksafetyCmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class SICKSAFETY_CMD_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  SICKSAFETY_CMD_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class SICKSAFETY_CMD_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  SICKSAFETY_CMD_INIT();
  // CMS Update Function
  void update(CMS *);
  
};


class SICKSAFETY_CMD_SET_DistanceOffset : public RCS_CMD_MSG
{
public:
  //Constructor
  SICKSAFETY_CMD_SET_DistanceOffset():
  RCS_CMD_MSG(SICKSAFETY_CMD_SET_DISTANCEOFFSET_TYPE, sizeof(SICKSAFETY_CMD_SET_DistanceOffset)){};
  // CMS Update Function
  void update(CMS *);
  double sicksafety_setDistanceOffset;
};


//! Standard NML format routine
int sicksafetyCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *sicksafetyCmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
