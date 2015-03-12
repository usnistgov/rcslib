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
robot_supern.hh

This C++ header file defines the NML Messages used for command and status by ROBOT_SUPER_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:20 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:07 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef ROBOT_SUPERN_HH
#define ROBOT_SUPERN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define ROBOT_SUPER_STATUS_TYPE 1000
#define ROBOT_SUPER_CONFIG_TYPE 1001
#define ROBOT_SUPER_HALT_TYPE 1002
#define ROBOT_SUPER_INIT_TYPE 1003
#define ROBOT_SUPER_RUN_TYPE 1004
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the NML Message Classes

// Status Class
class ROBOT_SUPER_STATUS : public RCS_STAT_MSG_V2
{
public:

	// Normal Constructor
	ROBOT_SUPER_STATUS();

	// Constructor used by derived classes
	ROBOT_SUPER_STATUS(NMLTYPE t, size_t s) :  RCS_STAT_MSG_V2(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  double stime;
};

// Command Classes

class ROBOT_SUPER_CONFIG : public RCS_CMD_MSG
{
public:

	//Constructor
	ROBOT_SUPER_CONFIG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class ROBOT_SUPER_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	ROBOT_SUPER_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class ROBOT_SUPER_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	ROBOT_SUPER_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class ROBOT_SUPER_RUN : public RCS_CMD_MSG
{
public:

	//Constructor
	ROBOT_SUPER_RUN();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  char filename[256];

};

// Declare NML format function
extern int robot_superFormat(NMLTYPE, void *, CMS *);

#endif 	// ROBOT_SUPERN_HH
