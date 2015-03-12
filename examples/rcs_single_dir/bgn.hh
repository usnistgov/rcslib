/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

/*
bgn.hh

This C++ header file defines the NML Messages used for command and status by BG_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:50 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:19 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:07 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef BGN_HH
#define BGN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define BG_STATUS_TYPE 4000
#define BG_CONFIG_TYPE 4001
#define BG_GOTO_GOAL_TYPE 4002
#define BG_HALT_TYPE 4003
#define BG_INIT_TYPE 4004
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the NML Message Classes

#include "my_app_common_statusn.hh"

// Status Class
class BG_STATUS : public MY_APP_COMMON_STAT_MSG
{
public:

	// Normal Constructor
	BG_STATUS();

	// Constructor used by derived classes
	BG_STATUS(NMLTYPE t, size_t s) :  MY_APP_COMMON_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Command Classes

class BG_CONFIG : public RCS_CMD_MSG
{
public:

	//Constructor
	BG_CONFIG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class BG_GOTO_GOAL : public RCS_CMD_MSG
{
public:

	//Constructor
	BG_GOTO_GOAL();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  PM_CARTESIAN goal;

};

class BG_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	BG_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class BG_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	BG_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Declare NML format function
extern int bgFormat(NMLTYPE, void *, CMS *);

#endif 	// BGN_HH
