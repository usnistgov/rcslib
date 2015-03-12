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
servo.cc

This C++ file defines member functions for the class SERVO_MODULE
It was generated with rcsdesign
with template version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Deleted pose_data, pose_data, 
Sun Mar 06 10:46:52 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:21 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:07 EST 2005	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "servo.hh" 	// SERVO_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "servon.hh" 	// NML Commands and Status definitions for servo

// auxiliary Output NML Message Files
#include "pose_datan.hh"	// NML Messages for pose_data
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


// Constructor
SERVO_MODULE::SERVO_MODULE(int _is_base_class)
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
void SERVO_MODULE::INITIALIZE_NML()
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "servo", "rcs_single_dir.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(servoFormat, "servo_cmd", "servo", "rcs_single_dir.nml"));
	servo_status = new SERVO_STATUS();
	setStatChannel(new RCS_STAT_CHANNEL(servoFormat, "servo_sts", "servo", "rcs_single_dir.nml"), servo_status);

	// auxiliary Input NML Channels

	// auxiliary Output NML Channels
	//pose_data
	POSE_DATA_CHANNEL = new NML(pose_dataFormat, "pose_data", "servo", "rcs_single_dir.nml");
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

}

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void SERVO_MODULE::PRE_PROCESS()
{
	// auxiliary Input NML Channels
	// Pre-Processing Code
}

/*
DECISION_PROCESS

The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.
It is expected to call a command function based on commandInData->type.

*/
void SERVO_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case SERVO_INIT_TYPE:
		INIT((SERVO_INIT *)commandInData);
		break;

	case SERVO_HALT_TYPE:
		HALT((SERVO_HALT *)commandInData);
		break;

	case SERVO_CONFIG_TYPE:
		CONFIG((SERVO_CONFIG *)commandInData);
		break;

	case SERVO_GOTO_POINT_TYPE:
		GOTO_POINT((SERVO_GOTO_POINT *)commandInData);
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
void SERVO_MODULE::POST_PROCESS()
{
	// Post-Processing Code

	// auxiliary Output NML Channels
	// Write data to pose_data
	POSE_DATA_CHANNEL->write(&pose_data_data);
}

// Command Functions

/*
INIT

Parameter(s):
SERVO_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void SERVO_MODULE::INIT(SERVO_INIT *cmd_in)
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
SERVO_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void SERVO_MODULE::HALT(SERVO_HALT *cmd_in)
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
SERVO_CONFIG *cmd_in -- NML Message sent from superior.

*/
void SERVO_MODULE::CONFIG(SERVO_CONFIG *cmd_in)
{
	// Put state table for SERVO_CONFIG here.
}

/*
GOTO_POINT

Parameter(s):
SERVO_GOTO_POINT *cmd_in -- NML Message sent from superior.

*/
void SERVO_MODULE::GOTO_POINT(SERVO_GOTO_POINT *cmd_in)
{
	// Put state table for SERVO_GOTO_POINT here.
  servo_status->position = cmd_in->point;
}

