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
servon.hh

This C++ header file defines the NML Messages used for command and status by SERVO_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:21 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:07 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef SERVON_HH
#define SERVON_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define SERVO_STATUS_TYPE 6000
#define SERVO_CONFIG_TYPE 6001
#define SERVO_GOTO_POINT_TYPE 6002
#define SERVO_HALT_TYPE 6003
#define SERVO_INIT_TYPE 6004
#define SERVO_STATUS2_TYPE 6005
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

#include "my_app_common_statusn.hh"

// Define the NML Message Classes

// Status Class
class SERVO_STATUS : public MY_APP_COMMON_STAT_MSG
{
public:

	// Normal Constructor
	SERVO_STATUS();

	// Constructor used by derived classes
	SERVO_STATUS(NMLTYPE t, size_t s) :  MY_APP_COMMON_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  PM_CARTESIAN position;

};

class SERVO_STATUS2 : public RCS_STAT_MSG
{
public:

	// Normal Constructor
	SERVO_STATUS2();

	// Constructor used by derived classes
	SERVO_STATUS2(NMLTYPE t, size_t s) :  RCS_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Command Classes

class SERVO_CONFIG : public RCS_CMD_MSG
{
public:

	//Constructor
	SERVO_CONFIG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class SERVO_GOTO_POINT : public RCS_CMD_MSG
{
public:

	//Constructor
	SERVO_GOTO_POINT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  PM_CARTESIAN point;

};

class SERVO_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	SERVO_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class SERVO_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	SERVO_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Declare NML format function
extern int servoFormat(NMLTYPE, void *, CMS *);

#endif 	// SERVON_HH
