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
#ifndef SR3000_STS_HH
#define SR3000_STS_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //SR3000_BASE
#include "definesSR.h"
#define DEFINES_SR_H


#define SR3000Sts_name  "sr3000Sts"

// Define the integer type ids.
typedef enum {
  SR3000_STATUS_TYPE = SR3000_STS_BASE,
} Sr3000StsIdEnum;

// Define the NML Message Classes

// Status Class
class SR3000_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  SR3000_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  /* determine whether the sr3000 result is working or not.*/
  bool goodResult;
  // flash ladar parameters
  double sr3000_integrationTime;
  double sr3000_amplitudeThreshold;
  double sr3000_saturationThreshold;
  double sr3000_distanceOffset;
  enum ModulationFrq sr3000_modulationFrequency;

  // acquire_mode  should be one of 3 possible options bitwise OR'd together
  // 0x1 fix pattern correction, 0x2 led non lin, 0x4 3 by 3 median filter
  // See SR_Acquire() description. 
  int acquire_mode;

  time_tracker tt;
};

//! Standard NML format routine
int sr3000Sts_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *sr3000Sts_symbol_lookup(NMLTYPE type);

#endif 	// MYNAM_HH
