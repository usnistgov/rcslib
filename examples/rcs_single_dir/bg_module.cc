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
bg.cc

This C++ file defines member functions for the class BG_MODULE
It was generated with rcsdesign
with template version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:50 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:19 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:07 EST 2005	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "bg.hh" 	// BG_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "bgn.hh" 	// NML Commands and Status definitions for bg
#include "primn.hh" 	// NML Commands and Status definitions for prim

// auxiliary Input NML Message Files
#include "pose_datan.hh"	// NML Messages for pose_data
#include "obstacle_mapn.hh"	// NML Messages for obstacle_map
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Constructor
BG_MODULE::BG_MODULE(int _is_base_class)
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
void BG_MODULE::INITIALIZE_NML()
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "bg", "rcs_single_dir.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(bgFormat, "bg_cmd", "bg", "rcs_single_dir.nml"));
	bg_status = new BG_STATUS();
	setStatChannel(new RCS_STAT_CHANNEL(bgFormat, "bg_sts", "bg", "rcs_single_dir.nml"), bg_status);

	prim_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(primFormat, "prim_cmd", "bg", "rcs_single_dir.nml"),
		new  RCS_STAT_CHANNEL(primFormat, "prim_sts", "bg", "rcs_single_dir.nml"));
	prim_status = (PRIM_STATUS *)  statusInData[prim_sub_num];

	// auxiliary Input NML Channels
	//pose_data
	POSE_DATA_CHANNEL = new NML(pose_dataFormat, "pose_data", "bg", "rcs_single_dir.nml");
	pose_data_data = (POSE_DATA_MSG *) POSE_DATA_CHANNEL->get_address();
	//obstacle_map
	OBSTACLE_MAP_CHANNEL = new NML(obstacle_mapFormat, "obstacle_map", "bg", "rcs_single_dir.nml");
	obstacle_map_data = (OBSTACLE_MAP_MSG *) OBSTACLE_MAP_CHANNEL->get_address();
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

}

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void BG_MODULE::PRE_PROCESS()
{
	// auxiliary Input NML Channels
	// Read new data from pose_data
	POSE_DATA_CHANNEL->read();
	// Read new data from obstacle_map
	OBSTACLE_MAP_CHANNEL->read();
	// Pre-Processing Code
}

/*
DECISION_PROCESS

The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.
It is expected to call a command function based on commandInData->type.

*/
void BG_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case BG_INIT_TYPE:
		INIT((BG_INIT *)commandInData);
		break;

	case BG_HALT_TYPE:
		HALT((BG_HALT *)commandInData);
		break;

	case BG_CONFIG_TYPE:
		CONFIG((BG_CONFIG *)commandInData);
		break;

	case BG_GOTO_GOAL_TYPE:
		GOTO_GOAL((BG_GOTO_GOAL *)commandInData);
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
void BG_MODULE::POST_PROCESS()
{
	// Post-Processing Code
  bg_status->common_int_var++;
}

// Command Functions

/*
INIT

Parameter(s):
BG_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void BG_MODULE::INIT(BG_INIT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	PRIM_INIT primInitMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		// Send an INIT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&primInitMsg, prim_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


		stateNext(S1);
		// Reinitialize variables here.

	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		prim_status->status == RCS_DONE &&
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
BG_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void BG_MODULE::HALT(BG_HALT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	PRIM_HALT primHaltMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		//Send a HALT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&primHaltMsg, prim_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

		stateNext(S1);
	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		prim_status->status == RCS_DONE &&
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
BG_CONFIG *cmd_in -- NML Message sent from superior.

*/
void BG_MODULE::CONFIG(BG_CONFIG *cmd_in)
{
	// Put state table for BG_CONFIG here.
}

/*
GOTO_GOAL

Parameter(s):
BG_GOTO_GOAL *cmd_in -- NML Message sent from superior.

*/
void BG_MODULE::GOTO_GOAL(BG_GOTO_GOAL *cmd_in)
{
	// Put state table for BG_GOTO_GOAL here.
}

