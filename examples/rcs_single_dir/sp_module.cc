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
sp.cc

This C++ file defines member functions for the class SP_MODULE
It was generated with rcsdesign
with template version 1.1

MODIFICATIONS:
Sun Mar 06 10:46:52 EST 2005	Deleted pose_data, 
Sun Mar 06 10:46:52 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:21 EST 2005	Deleted pose_data, 
Sun Mar 06 10:07:21 EST 2005	Modified with RCS-Design tool.
Sun Mar 06 10:07:07 EST 2005	Created with RCS-Design tool.

*/

// Include Files
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "sp.hh" 	// SP_MODULE definitions
// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.


#include "spn.hh" 	// NML Commands and Status definitions for sp

// auxiliary Input NML Message Files
#include "pose_datan.hh"	// NML Messages for pose_data

// auxiliary Output NML Message Files
#include "sensor_datan.hh"	// NML Messages for sensor_data
// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

#include <stdlib.h>
#include <math.h>

// Constructor
SP_MODULE::SP_MODULE(int _is_base_class)
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
void SP_MODULE::INITIALIZE_NML()
{
	// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	setErrorLogChannel(new NML(nmlErrorFormat, "errlog", "sp", "rcs_single_dir.nml"));
	setCmdChannel(new RCS_CMD_CHANNEL(spFormat, "sp_cmd", "sp", "rcs_single_dir.nml"));
	sp_status = new SP_STATUS();
	setStatChannel(new RCS_STAT_CHANNEL(spFormat, "sp_sts", "sp", "rcs_single_dir.nml"), sp_status);

	// auxiliary Input NML Channels
	//pose_data
	POSE_DATA_CHANNEL = new NML(pose_dataFormat, "pose_data", "sp", "rcs_single_dir.nml");
	pose_data_data = (POSE_DATA_MSG *) POSE_DATA_CHANNEL->get_address();

	// auxiliary Output NML Channels
	//sensor_data
	SENSOR_DATA_CHANNEL = new NML(sensor_dataFormat, "sensor_data", "sp", "rcs_single_dir.nml");
	// RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.

}

/*
PRE_PROCESS

The PRE_PROCESS function is called every cycle after the command and
subordinates status have been read but before DECISION_PROCESS is called.
It is intended to be used for tasks such as sensory processing that should
be performed every cycle regardless of the current command or state.

*/
void SP_MODULE::PRE_PROCESS()
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
void SP_MODULE::DECISION_PROCESS()
{
	switch(commandInData->type)
	{
		// RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.

	case SP_INIT_TYPE:
		INIT((SP_INIT *)commandInData);
		break;

	case SP_HALT_TYPE:
		HALT((SP_HALT *)commandInData);
		break;

	case SP_CONFIG_TYPE:
		CONFIG((SP_CONFIG *)commandInData);
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
void SP_MODULE::POST_PROCESS()
{
	// Post-Processing Code

	// auxiliary Output NML Channels
	// Write data to sensor_data
  sensor_data_data.sd_length= sizeof(sensor_data_data.sd)/sizeof(sensor_data_data.sd[0]);
  for(int i = 0 ; i < sensor_data_data.sd_length ; i++)
    {
      double dr = (((double)rand())-(RAND_MAX/2))/RAND_MAX;
      double dr2 = (((double)rand())-(RAND_MAX/2))/RAND_MAX;
      if(i > 0)
	{
	  sensor_data_data.sd[i].range = sensor_data_data.sd[i-1].range +dr;
	  sensor_data_data.sd[i].intensity = sensor_data_data.sd[i-1].intensity + dr*dr2;
	}
      else
	{
	  sensor_data_data.sd[i].intensity = sensor_data_data.sd[i].intensity +dr;
	  sensor_data_data.sd[i].range = sensor_data_data.sd[i].range +dr*dr2;
	}
    }
      
  SENSOR_DATA_CHANNEL->write(&sensor_data_data);

  SP_STATUS *sts = sp_status;
  sts->stime = sin(etime());
}

// Command Functions

/*
INIT

Parameter(s):
SP_INIT *cmd_in -- NML Message sent from superior.

Most Modules will have an INIT command.
The INIT function is expected to initialize any variables that may be
in an uninitialized or unknown state, send INIT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void SP_MODULE::INIT(SP_INIT *cmd_in)
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
SP_HALT *cmd_in -- NML Message sent from superior.

Most Modules will have an HALT command.
The HALT function is expected to stop any motion or command execution,
send HALT commands to its subordinates,
wait for the subordinates to be DONE and then inform its superior that it is done.
The state tables should use the STATE_MATCH macro so the diagnostics tool can 
highlight the current line in the state table.

*/
void SP_MODULE::HALT(SP_HALT *cmd_in)
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
SP_CONFIG *cmd_in -- NML Message sent from superior.

*/
void SP_MODULE::CONFIG(SP_CONFIG *cmd_in)
{
	// Put state table for SP_CONFIG here.
}

