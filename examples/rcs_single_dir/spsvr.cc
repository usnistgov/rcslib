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
	spsvr.cc

	This C++ file provides a main routin to start an NML server for this application
	It connects to all of the NML channels used by this application.
	If they are all valid it will call run_nml_servers(), otherwise it will exit immediately.

	MODIFICATIONS:
	Sun Mar 06 10:46:52 EST 2005	Created.

*/

// Include Files
#include <stdlib.h> // exit()
#include "rcs.hh" 	// Common RCS definitions
#include "spn.hh" 	// NML Commands and Status definitions for sp
#include "sensor_datan.hh" 	// sensor_dataFormat

// NML Channel Pointers
static RCS_CMD_CHANNEL *sp_cmd = NULL;
static RCS_STAT_CHANNEL *sp_stat = NULL;
static NML *sensor_data= NULL;

static int InitNML()
{


	// sp
	sp_cmd = new RCS_CMD_CHANNEL(spFormat, "sp_cmd", "spsvr", "rcs_single_dir.nml");
	if(NULL == sp_cmd)
		return -1;
	if(!sp_cmd->valid())
		return -1;

	sp_stat = new RCS_STAT_CHANNEL(spFormat, "sp_sts", "spsvr", "rcs_single_dir.nml");
	if(NULL == sp_stat)
		return -1;
	if(!sp_stat->valid())
		return -1;


	sensor_data = new NML(sensor_dataFormat, "sensor_data", "spsvr", "rcs_single_dir.nml");
	if(NULL == sensor_data)
		return -1;
	if(!sensor_data->valid())
		return -1;

	return 0;
}

static void DeleteNML()
{

	// sp
	if(NULL != sp_cmd)
	{
		delete sp_cmd;
		sp_cmd = NULL;
	}

	if(NULL != sp_stat)
	{
		delete sp_stat;
		sp_stat = NULL;
	}


	if(NULL != sensor_data)
	{
		delete sensor_data;
		sensor_data = NULL;
	}


}

// Main 
#ifdef VXWORKS
extern "C" int spsvr_run();

int spsvr_run()
#else
int main(int argc, char **argv)
#endif
{

	set_rcs_print_destination(RCS_PRINT_TO_STDOUT);

	if(InitNML() < 0)
	{
		DeleteNML();
		return(-1);
	}

	run_nml_servers();

}

