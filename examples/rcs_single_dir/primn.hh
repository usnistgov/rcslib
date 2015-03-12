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
primn.hh

This C++ header file defines the NML Messages used for command and status by PRIM_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:51 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:20 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:07 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef PRIMN_HH
#define PRIMN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define PRIM_STATUS_TYPE 5000
#define PRIM_CONFIG_TYPE 5001
#define PRIM_FOLLOW_WAYPOINTS_TYPE 5002
#define PRIM_HALT_TYPE 5003
#define PRIM_INIT_TYPE 5004
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the NML Message Classes

// Status Class
class PRIM_STATUS : public RCS_STAT_MSG
{
public:

	// Normal Constructor
	PRIM_STATUS();

	// Constructor used by derived classes
	PRIM_STATUS(NMLTYPE t, size_t s) :  RCS_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  int current_waypoint;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN, waypointsOut, 1000);

};

// Command Classes

class PRIM_CONFIG : public RCS_CMD_MSG
{
public:

	//Constructor
	PRIM_CONFIG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class PRIM_FOLLOW_WAYPOINTS : public RCS_CMD_MSG
{
public:

	//Constructor
	PRIM_FOLLOW_WAYPOINTS();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN, waypoints, 1000);

};

class PRIM_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	PRIM_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class PRIM_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	PRIM_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Declare NML format function
extern int primFormat(NMLTYPE, void *, CMS *);

#endif 	// PRIMN_HH
