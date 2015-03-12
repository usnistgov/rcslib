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
sensor_datan.hh

This C++ header file defines the NML Messages for SENSOR_DATA
Template Version 1.1

MODIFICATIONS:
Sat Mar 12 06:57:48 EST 2005	Created by rcsdesign.

*/

// Prevent Multiple Inclusion
#ifndef SENSOR_DATAN_HH
#define SENSOR_DATAN_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions

#include "offsets.hh"

// Trying to merge the type ids often results in redefinn the ID twice..
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


// Define the integer type ids.
#define SENSOR_DATA_MSG_TYPE (SENSOR_DATA_BASE+0)
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

struct sense_data
{
  float range;
  float intensity;
};


// Define the NML Message Classes

class SENSOR_DATA_MSG : public NMLmsg
{
public:

	//Constructor
	SENSOR_DATA_MSG();

	// CMS Update Function
	void update(CMS *);

	// Place custom variables here.
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(sense_data, sd, 5000);

};

// Declare NML format function
extern int sensor_dataFormat(NMLTYPE, void *, CMS *);

#endif 	// SENSOR_DATAN_HH
