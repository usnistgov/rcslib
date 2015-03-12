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
wm.hh

This C++ header file defines the class WM_MODULE
It was generated with the RCS-Design tool.
Template Version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:53 EST 2005	Modified by RCS-Design tool.
Sun Mar 06 10:07:21 EST 2005	Modified by RCS-Design tool.
Sun Mar 06 10:07:08 EST 2005	Created by RCS-Design tool.

*/

// Prevent Multiple Inclusion
#ifndef WM_HH
#define WM_HH

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "wmn.hh" 	// NML Commands and Status definitions for wm

// auxiliary Input NML Message Files
#include "sensor_datan.hh"	// NML Messages for sensor_data
#include "pose_datan.hh"	// NML Messages for pose_data

// auxiliary Output NML Message Files
#include "obstacle_mapn.hh"	// NML Messages for obstacle_map
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


class WM_MODULE: public NML_MODULE
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

public:

	WM_MODULE(int _is_base_class = 0); // Constructor

	// Overloaded Virtual Functions
	virtual void PRE_PROCESS();
	virtual void DECISION_PROCESS();
	virtual void POST_PROCESS();
	virtual void INITIALIZE_NML();

	// Command Functions
	virtual void CONFIG(WM_CONFIG *);
	virtual void HALT(WM_HALT *);
	virtual void INIT(WM_INIT *);

	// Convenience Variables
	WM_STATUS *wm_status;

	// auxiliary Input NML Channels
	NML *SENSOR_DATA_CHANNEL;	// NML Channel for sensor_data
	SENSOR_DATA_MSG *sensor_data_data;	// NML Data for sensor_data
	NML *POSE_DATA_CHANNEL;	// NML Channel for pose_data
	POSE_DATA_MSG *pose_data_data;	// NML Data for pose_data

	// auxiliary Output NML Channels
	NML *OBSTACLE_MAP_CHANNEL;	// NML Channel for obstacle_map
	OBSTACLE_MAP_MSG obstacle_map_data;	//NML Data for obstacle_map
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


private:
	// Add custom variables and functions here.

};

#endif 	// WM_HH



