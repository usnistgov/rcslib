
// Prevent Multiple Inclusion
#ifndef EXCMD_HH
#define EXCMD_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

enum exCmdMsgType {
  EX_CONFIG_TYPE=6001,
  EX_GOTO_POINT_TYPE,
  EX_HALT_TYPE,
  EX_INIT_TYPE
};

// Define the NML Message Classes

// Command Classes
class EX_CONFIG : public RCS_CMD_MSG
{
public:

  //Constructor
  EX_CONFIG();

  // CMS Update Function
  void update(CMS *);

  // Place custom variables here.

};

class EX_GOTO_POINT : public RCS_CMD_MSG
{
public:

  //Constructor
  EX_GOTO_POINT();

  // CMS Update Function
  void update(CMS *);

  // Place custom variables here.
  PM_CARTESIAN point;

};

class EX_HALT : public RCS_CMD_MSG
{
public:

  //Constructor
  EX_HALT();

  // CMS Update Function
  void update(CMS *);

  // Place custom variables here.

};

class EX_INIT : public RCS_CMD_MSG
{
public:

  //Constructor
  EX_INIT();

  // CMS Update Function
  void update(CMS *);

  // Place custom variables here.

};

// Declare NML format function
extern int exCmd_format(NMLTYPE, void *, CMS *);

#endif 	// EXCMD_HH
