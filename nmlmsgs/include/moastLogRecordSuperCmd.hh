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
#ifndef MOAST_LOG_RECORD_SUPER_CMD_HH
#define MOAST_LOG_RECORD_SUPER_CMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh" //MOAST_LOG_RECORD_SUPER_BASE
#include "moastTypes.hh"

#define moastLogRecordSuperCmd_name  "moastLogRecordSuperCmd"

// Define the integer type ids.
typedef enum {
  MOAST_LOG_RECORD_SUPER_HALT_TYPE = MOAST_LOG_RECORD_SUPER_CMD_BASE,
  MOAST_LOG_RECORD_SUPER_INIT_TYPE,
  MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_WP_TYPE,
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_WP_TYPE,
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FORWARD_TYPE,
  MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_FILE_TYPE,
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FILE_TYPE,
} moastLogRecordSuperCmdIdEnum;

// Define the NML Message Classes

// Command Class: Halt
class MOAST_LOG_RECORD_SUPER_HALT : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_HALT();
  // CMS Update Function
  void update(CMS *);
  // Place custom variables here.
};


// Command Class: Init
class MOAST_LOG_RECORD_SUPER_INIT : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_INIT();
  // CMS Update Function
  void update(CMS *);  
};

struct WaypointList {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(Waypoint,waypoints,100);
};

// Command Class: Collect_Data
class MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_WP : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_WP();
  // CMS Update Function
  void update(CMS *);  
  
  struct WaypointList waypoint_list;
  char label[64];
};

class MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_WP : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_WP();
  // CMS Update Function
  void update(CMS *);  
  
  struct WaypointList waypoint_list;
  char label[64];
};

// Command Class: Collect_Data
class MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_FILE : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_RECORD_AM_MOVE_FILE();
  // CMS Update Function
  void update(CMS *);  
  
  char filename[80];
  char label[64];
};

class MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FILE : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FILE();
  // CMS Update Function
  void update(CMS *);  
  
  char filename[80];
  char label[64];
};

class MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FORWARD : public RCS_CMD_MSG
{
public:
  //Constructor
  MOAST_LOG_RECORD_SUPER_RECORD_PRIM_MOVE_FORWARD();
  // CMS Update Function
  void update(CMS *);  
  
  double distance;
  char label[64];
};

//! Standard NML format routine
extern int moastLogRecordSuperCmd_format(NMLTYPE type, void *buf, CMS *cms);
//! standard NML type lookup routine
extern const char *moastLogRecordSuperCmd_symbol_lookup(long type);

#endif 	// MYNAM_HH
