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
#ifndef PMD_STS_HH
#define PMD_STS_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //PMD_BASE
#include "LadarData.hh"

#define PMDSts_name  "PMDSts"

// Define the integer type ids.
typedef enum {
  PMD_STATUS_TYPE = PMD_STS_BASE,
} PMDStsIdEnum;

// Define the NML Message Classes

// Status Class
class PMD_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  PMD_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  /* determine whether the PMD result is working or not.*/
  time_tracker tt;
  float range_offset;

  struct LadarOptionalInfo opt;
};

//! Standard NML format routine
int PMDSts_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
const char *PMDSts_symbol_lookup(long type);

#endif 	// MYNAM_HH
