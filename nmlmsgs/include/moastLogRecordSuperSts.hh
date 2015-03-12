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

// generate_symbol_lookups=true
// generate_enum_symbol_lookups=true

// Prevent Multiple Inclusion
#ifndef MOAST_LOG_RECORD_SUPER_STS_HH
#define MOAST_LOG_RECORD_SUPER_STS_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //MOAST_LOG_RECORD_SUPER_BASE
#define moastLogRecordSuperSts_name  "moastLogRecordSuperSts"

// Define the integer type ids.
typedef enum {
  MOAST_LOG_RECORD_SUPER_STATUS_TYPE = MOAST_LOG_RECORD_SUPER_STS_BASE,
} moastLogRecordSuperStsIdEnum;

// Define the NML Message Classes

enum MoastMobLevelEnum {
  AM_LEVEL,
  PRIM_LEVEL
};

// Status Class
class MOAST_LOG_RECORD_SUPER_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  MOAST_LOG_RECORD_SUPER_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  time_tracker tt;
  enum MoastMobLevelEnum moastMobLevel;
};

//! Standard NML format routine
int moastLogRecordSuperSts_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *moastLogRecordSuperSts_symbol_lookup(long type);

#define MOAST_LOG_RECORD_SUPER_STS_ID "$Id: moastLogRecordSuperSts.hh 682 2009-03-09 20:13:33Z shackle $"

#endif 	// MYNAM_HH
