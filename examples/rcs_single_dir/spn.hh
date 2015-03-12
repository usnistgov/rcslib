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
spn.hh

This C++ header file defines the NML Messages used for command and status by SP_MODULE
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:21 EST 2005	Modified by rcsdesign.
Sun Mar 06 10:07:08 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef SPN_HH
#define SPN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

// Predefined type files
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Define the integer type ids.
//  RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

#define SP_STATUS_TYPE 2000
#define SP_CONFIG_TYPE 2001
#define SP_HALT_TYPE 2002
#define SP_INIT_TYPE 2003
//  RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

#include "my_app_common_statusn.hh"

// Define the NML Message Classes

// Status Class
class SP_STATUS : public MY_APP_COMMON_STAT_MSG
{
public:

	// Normal Constructor
	SP_STATUS();

	// Constructor used by derived classes
	SP_STATUS(NMLTYPE t, size_t s) :  MY_APP_COMMON_STAT_MSG(t,s) {};

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  double stime;

};

// Command Classes

class SP_CONFIG : public RCS_CMD_MSG
{
public:

	//Constructor
	SP_CONFIG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class SP_HALT : public RCS_CMD_MSG
{
public:

	//Constructor
	SP_HALT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

class SP_INIT : public RCS_CMD_MSG
{
public:

	//Constructor
	SP_INIT();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.

};

// Declare NML format function
extern int spFormat(NMLTYPE, void *, CMS *);

#endif 	// SPN_HH
