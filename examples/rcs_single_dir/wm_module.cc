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
wm.cc

This C++ file defines member functions for the class WM_MODULE
It was generated with rcsdesign
with template version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:53 EST 2005	Deleted obstacle_map, obstacle_map, pose_data, 
Sun Mar 06 10:46:53 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:21 EST 2005	Deleted pose_data, 
Sun Mar 06 10:07:21 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:08 EST 2005	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "wm.hh" 	// WM_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "wmn.hh" 	// NML Commands and Status definitions for wm

// auxiliary Input NML Message Files
#include "sensor_datan.hh"	// NML Messages for sensor_data
#include "pose_datan.hh"	// NML Messages for pose_data

// auxiliary Output NML Message Files
#include "obstacle_mapn.hh"	// NML Messages for obstacle_map
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Constructor
WM_MODULE::WM_MODULE(int _is_base_class)
{

	// Initialize the NML channels if this module is not being used as the base class for another module.
	if(!_is_base_class)
	{
		INITIALIZE_NML();
	}
	// Add additional code to initialize the module here.

}

// Overloaded Virtual Functions

/*
INITIALIZE_NML

The INITIALIZE_NML function is ussually called only once from within the
constructor. It should not be called if a derived class will also call it.

*/
void WM_MODULE::INITIALIZE_NML()
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "wm", "rcs_single_dir.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(wmFormat, "wm_cmd", "wm", "rcs_single_dir.nml"));
	wm_status = new WM_STATUS();
	setStatChannel(new RCS_STAT_CHANNEL(wmFormat, "wm_sts", "wm", "rcs_single_dir.nml"), wm_status);

	// auxiliary Input NML Channels
	//sensor_data
	SENSOR_DATA_CHANNEL = new NML(sensor_dataFormat, "sensor_data", "wm", "rcs_single_dir.nml");
	sensor_data_data = (SENSOR_DATA_MSG *) SENSOR_DATA_CHANNEL->get_address();
	//pose_data
	POSE_DATA_CHANNEL = new NML(pose_dataFormat, "pose_data", "wm", "rcs_single_dir.nml");
	pose_data_data = (POSE_DATA_MSG *) POSE_DATA_CHANNEL->get_address();

	// auxiliary Output NML Channels
	//obstacle_map
	OBSTACLE_MAP_CHANNEL = new NML(obstacle_mapFormat, "obstacle_map", "wm", "rcs_single_dir.nml");
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

}

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void WM_MODULE::PRE_PROCESS()
{
	// auxiliary Input NML Channels
	// Read new data from sensor_data
	SENSOR_DATA_CHANNEL->read();
	// Read new data from pose_data
	POSE_DATA_CHANNEL->read();
	// Pre-Processing Code
}

/*
DECISION_PROCESS

The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.
It is expected to call a command function based on commandInData->type.

*/
void WM_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case WM_INIT_TYPE:
		INIT((WM_INIT *)commandInData);
		break;

	case WM_HALT_TYPE:
		HALT((WM_HALT *)commandInData);
		break;

	case WM_CONFIG_TYPE:
		CONFIG((WM_CONFIG *)commandInData);
		break;
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	default:
		logError("The command %d is not recognized.",commandInData->type);
		break;
	}
}

/*
POST_PROCESS

The POST_PROCESS function is called every cycle after DECISION_PROCESS is called
but before the status and the subordinates commands  have been written.
It is intended to be used for tasks such as output filters that should
be performed every cycle regardless of the current command or state.

*/
void WM_MODULE::POST_PROCESS()
{
	// Post-Processing Code

	// auxiliary Output NML Channels
	// Write data to obstacle_map
	OBSTACLE_MAP_CHANNEL->write(&obstacle_map_data);
}

// Command Functions

/*
INIT

Parameter(s):
WM_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void WM_MODULE::INIT(WM_INIT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		// Send an INIT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


		stateNext(S1);
		// Reinitialize variables here.

	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		1))
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

	{
		status = RCS_DONE;
		stateNext(S2);
	}
	else if(STATE_MATCH(S2))
	{
		// Idle State
	}
}

/*
HALT

Parameter(s):
WM_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void WM_MODULE::HALT(WM_HALT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		//Send a HALT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

		stateNext(S1);
	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		1))
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

	{
		status = RCS_DONE;
		stateNext(S2);
	}
	else if(STATE_MATCH(S2))
	{
		// Idle State
	}
}



/*
CONFIG

Parameter(s):
WM_CONFIG *cmd_in -- NML Message sent from superior.

*/
void WM_MODULE::CONFIG(WM_CONFIG *cmd_in)
{
	// Put state table for WM_CONFIG here.
}

