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
robot_super.cc

This C++ file defines member functions for the class ROBOT_SUPER_MODULE
It was generated with rcsdesign
with template version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:20 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:07 EST 2005	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "robot_super.hh" 	// ROBOT_SUPER_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "robot_supern.hh" 	// NML Commands and Status definitions for robot_super
#include "bgn.hh" 	// NML Commands and Status definitions for bg
#include "spn.hh" 	// NML Commands and Status definitions for sp
#include "wmn.hh" 	// NML Commands and Status definitions for wm

// auxiliary Input NML Message Files
#include "pose_datan.hh"	// NML Messages for pose_data
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

#include <stdio.h>

// Constructor
ROBOT_SUPER_MODULE::ROBOT_SUPER_MODULE(int _is_base_class)
{

	// Initialize the NML channels if this module is not being used as the base class for another module.
	if(!_is_base_class)
	{
		INITIALIZE_NML();
	}
	// Add additional code to initialize the module here.
	reset_time_tracker(&(robot_super_status->tt));

}

// Overloaded Virtual Functions

/*
INITIALIZE_NML

The INITIALIZE_NML function is ussually called only once from within the
constructor. It should not be called if a derived class will also call it.

*/
void ROBOT_SUPER_MODULE::INITIALIZE_NML()
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "robot_super", "rcs_single_dir.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(robot_superFormat, "robot_super_cmd", "robot_super", "rcs_single_dir.nml"));
	robot_super_status = new ROBOT_SUPER_STATUS();
	setStatChannel(new RCS_STAT_CHANNEL(robot_superFormat, "robot_super_sts", "robot_super", "rcs_single_dir.nml"), robot_super_status);

	bg_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(bgFormat, "bg_cmd", "robot_super", "rcs_single_dir.nml"),
		new  RCS_STAT_CHANNEL(bgFormat, "bg_sts", "robot_super", "rcs_single_dir.nml"));
	bg_status = (BG_STATUS *)  statusInData[bg_sub_num];

	sp_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(spFormat, "sp_cmd", "robot_super", "rcs_single_dir.nml"),
		new  RCS_STAT_CHANNEL(spFormat, "sp_sts", "robot_super", "rcs_single_dir.nml"));
	sp_status = (SP_STATUS *)  statusInData[sp_sub_num];

	wm_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(wmFormat, "wm_cmd", "robot_super", "rcs_single_dir.nml"),
		new  RCS_STAT_CHANNEL(wmFormat, "wm_sts", "robot_super", "rcs_single_dir.nml"));
	wm_status = (WM_STATUS *)  statusInData[wm_sub_num];

	// auxiliary Input NML Channels
	//pose_data
	POSE_DATA_CHANNEL = new NML(pose_dataFormat, "pose_data", "robot_super", "rcs_single_dir.nml");
	pose_data_data = (POSE_DATA_MSG *) POSE_DATA_CHANNEL->get_address();
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

}

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void ROBOT_SUPER_MODULE::PRE_PROCESS()
{
	// auxiliary Input NML Channels
	// Read new data from pose_data
	POSE_DATA_CHANNEL->read();
	// Pre-Processing Code
}

/*
DECISION_PROCESS

The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.
It is expected to call a command function based on commandInData->type.

*/
void ROBOT_SUPER_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case ROBOT_SUPER_INIT_TYPE:
		INIT((ROBOT_SUPER_INIT *)commandInData);
		break;

	case ROBOT_SUPER_HALT_TYPE:
		HALT((ROBOT_SUPER_HALT *)commandInData);
		break;

	case ROBOT_SUPER_CONFIG_TYPE:
		CONFIG((ROBOT_SUPER_CONFIG *)commandInData);
		break;

	case ROBOT_SUPER_RUN_TYPE:
		RUN((ROBOT_SUPER_RUN *)commandInData);
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
void ROBOT_SUPER_MODULE::POST_PROCESS()
{
	// Post-Processing Code
  cycle_time_tracker(&(robot_super_status->tt));
  sprintf(robot_super_status->message,
	   "tt.last=%f\n",robot_super_status->tt.last);
  robot_super_status->message_length =
    strlen(robot_super_status->message)+1;
  robot_super_status->stime =sp_status->stime;
}

// Command Functions

/*
INIT

Parameter(s):
ROBOT_SUPER_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void ROBOT_SUPER_MODULE::INIT(ROBOT_SUPER_INIT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	BG_INIT bgInitMsg;
	SP_INIT spInitMsg;
	WM_INIT wmInitMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		// Send an INIT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&bgInitMsg, bg_sub_num);
		sendCommand(&spInitMsg, sp_sub_num);
		sendCommand(&wmInitMsg, wm_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


		stateNext(S1);
		// Reinitialize variables here.

	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		bg_status->status == RCS_DONE &&
		sp_status->status == RCS_DONE &&
		wm_status->status == RCS_DONE &&
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
ROBOT_SUPER_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void ROBOT_SUPER_MODULE::HALT(ROBOT_SUPER_HALT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	BG_HALT bgHaltMsg;
	SP_HALT spHaltMsg;
	WM_HALT wmHaltMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		//Send a HALT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&bgHaltMsg, bg_sub_num);
		sendCommand(&spHaltMsg, sp_sub_num);
		sendCommand(&wmHaltMsg, wm_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

		stateNext(S1);
	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		bg_status->status == RCS_DONE &&
		sp_status->status == RCS_DONE &&
		wm_status->status == RCS_DONE &&
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
ROBOT_SUPER_CONFIG *cmd_in -- NML Message sent from superior.

*/
void ROBOT_SUPER_MODULE::CONFIG(ROBOT_SUPER_CONFIG *cmd_in)
{
	// Put state table for ROBOT_SUPER_CONFIG here.
}

/*
RUN

Parameter(s):
ROBOT_SUPER_RUN *cmd_in -- NML Message sent from superior.

*/
void ROBOT_SUPER_MODULE::RUN(ROBOT_SUPER_RUN *cmd_in)
{
	// Put state table for ROBOT_SUPER_RUN here.
}

