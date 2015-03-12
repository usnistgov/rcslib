/*
mcps.cpp

This C++ file defines member functions for the class MCPS_MODULE
It was generated with rcsdesign
with template version 1.0

MODIFICATIONS:
1-Dec-97	Modified with RCS-Design tool.
17-Nov-97	Modified with RCS-Design tool.
13-Nov-97	Modified with RCS-Design tool.
5-Nov-97	Modified with RCS-Design tool.
3-Nov-97	Modified with RCS-Design tool.
30-Oct-97	Modified with RCS-Design tool.
23-Oct-97	Modified with RCS-Design tool.
22-Oct-97	Modified with RCS-Design tool.
21-Oct-97	Modified with RCS-Design tool.
21-Oct-97	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "mcps.hpp" 	// MCPS_MODULE definitions
#include "mcpsn.hpp" 	// NML Commands and Status definitions for mcps
#include "mcexmon.hpp" 	// NML Commands and Status definitions for mcexmo
#include "mcexton.hpp" 	// NML Commands and Status definitions for mcexto

// Auxilliary Input NML Message Files
#include "mcps_auxinn.hpp"	// NML Messages for mcps_auxin

// Auxilliary Output NML Message Files
#include "mcps_auxoutn.hpp"	// NML Messages for mcps_auxout
#include "mcexmon.hpp" 	// NML Commands and Status definitions for mcexmo

// Constructor
MCPS_MODULE::MCPS_MODULE()
{

	// Set up NML Channels
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "mcps", "isam.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(mcpsFormat, "mcps_cmd", "mcps", "isam.nml"));
	setStatChannel(new RCS_STAT_CHANNEL(mcpsFormat, "mcps_sts", "mcps", "isam.nml"), &mcps_status);

	mcexmo_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(mcexmoFormat, "mcexmo_cmd", "mcps", "isam.nml"),
		new  RCS_STAT_CHANNEL(mcexmoFormat, "mcexmo_sts", "mcps", "isam.nml"));
	mcexmo_status = (MCEXMO_STATUS *)  statusInData[mcexmo_sub_num];

	mcexto_sub_num = 
	addSubordinate(
		new RCS_CMD_CHANNEL(mcextoFormat, "mcexto_cmd", "mcps", "isam.nml"),
		new  RCS_STAT_CHANNEL(mcextoFormat, "mcexto_sts", "mcps", "isam.nml"));
	mcexto_status = (MCEXTO_STATUS *)  statusInData[mcexto_sub_num];

	// Auxilliary Input NML Channels
	//mcps_auxin
	MCPS_AUXIN_CHANNEL = new NML(mcps_auxinFormat, "mcps_auxin", "mcps", "isam.nml");
	mcps_auxin_data = (MCPS_AUXIN_MSG *) MCPS_AUXIN_CHANNEL->get_address();

	// Auxilliary Output NML Channels
	//mcps_auxout
	MCPS_AUXOUT_CHANNEL = new NML(mcps_auxoutFormat, "mcps_auxout", "mcps", "isam.nml");
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

	// Add additional code to initialize the module here.

}

// Overloaded Virtual Functions

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void MCPS_MODULE::PRE_PROCESS()
{

	// Auxilliary Input NML Channels
	// Pre-Processing Code
}

/*
DECISION_PROCESS

The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.
It is expected to call a command function based on commandInData->type.

*/
void MCPS_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case MCPS_INIT_TYPE:
		INIT((MCPS_INIT *)commandInData);
		break;

	case MCPS_HALT_TYPE:
		HALT((MCPS_HALT *)commandInData);
		break;

	case MCPS_HOME_TYPE:
		HOME((MCPS_HOME *)commandInData);
		break;

	case MCPS_MOVE_TYPE:
		MOVE((MCPS_MOVE *)commandInData);
		break;

	case MCPS_CUT_TYPE:
		CUT((MCPS_CUT *)commandInData);
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
void MCPS_MODULE::POST_PROCESS()
{
	// Post-Processing Code

	// Auxilliary Output NML Channels
}

// Command Functions

/*
INIT

Parameter(s):
MCPS_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void MCPS_MODULE::INIT(MCPS_INIT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	MCEXMO_INIT mcexmoInitMsg;
	MCEXTO_INIT mcextoInitMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		// Send an INIT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&mcexmoInitMsg, mcexmo_sub_num);
		sendCommand(&mcextoInitMsg, mcexto_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


		stateNext(S1);
		// Reinitialize variables here.

	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		mcexmo_status->status == RCS_DONE &&
		mcexto_status->status == RCS_DONE &&
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
MCPS_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void MCPS_MODULE::HALT(MCPS_HALT *cmd_in)
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	MCEXMO_HALT mcexmoHaltMsg;
	MCEXTO_HALT mcextoHaltMsg;
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.


	if(STATE_MATCH(NEW_COMMAND))
	{
		//Send a HALT command to all subordinates.
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

		sendCommand(&mcexmoHaltMsg, mcexmo_sub_num);
		sendCommand(&mcextoHaltMsg, mcexto_sub_num);
		// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

		stateNext(S1);
	}
	// Wait for all subordinates to report done.
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	else if(STATE_MATCH(S1,
		mcexmo_status->status == RCS_DONE &&
		mcexto_status->status == RCS_DONE &&
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
HOME

Parameter(s):
MCPS_HOME *cmd_in -- NML Message sent from superior.

*/
void MCPS_MODULE::HOME(MCPS_HOME *cmd_in)
{
	// Put state table for MCPS_HOME here.
}

/*
MOVE

Parameter(s):
MCPS_MOVE *cmd_in -- NML Message sent from superior.

*/
void MCPS_MODULE::MOVE(MCPS_MOVE *cmd_in)
{
	// Put state table for MCPS_MOVE here.
}

/*
CUT

Parameter(s):
MCPS_CUT *cmd_in -- NML Message sent from superior.

*/
void MCPS_MODULE::CUT(MCPS_CUT *cmd_in)
{
	// Put state table for MCPS_CUT here.
}



















