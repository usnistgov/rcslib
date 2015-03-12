/*
sickLoggern.hh

This C++ header file defines the NML Messages used for command and status by SICKLOGGER_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Nov 07 16:12:42 EST 2004	Modified by rcsdesign.
Sun Nov 07 16:11:48 EST 2004	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef SICKLOGGERN_HH
#define SICKLOGGERN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define SICKLOGGER_STATUS_TYPE 25000
#define SICKLOGGER_INIT_TYPE 25001
#define SICKLOGGER_HALT_TYPE 25002
#define SICKLOGGER_COLLECT_DATA_TYPE 25003
#define SICKLOGGER_SET_DIR_TYPE 25004
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the NML Message Classes

// Status Class
class SICKLOGGER_STATUS : public RCS_STAT_MSG
{
public:

	// Normal Constructor
	SICKLOGGER_STATUS();

	// Constructor used by derived classes
	SICKLOGGER_STATUS(NMLTYPE t, size_t s) :  RCS_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  char roof_filename[256];
  char bumper_filename[256];
};

// Command Classes

class SICKLOGGER_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	SICKLOGGER_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class SICKLOGGER_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	SICKLOGGER_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};


class SICKLOGGER_COLLECT_DATA : public RCS_CMD_MSG
{
public:

	//Constructor
	SICKLOGGER_COLLECT_DATA();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};


class SICKLOGGER_SET_DIR : public RCS_CMD_MSG
{
public:

	//Constructor
	SICKLOGGER_SET_DIR();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  char dir[80];
};

// Declare NML format function
extern int sickLoggerFormat(NMLTYPE, void *, CMS *);

#endif 	// SICKLOGGERN_HH
