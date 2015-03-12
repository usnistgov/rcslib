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
	wmsvr.cc

	This C++ file provides a main routin to start an NML server for this application
	It connects to all of the NML channels used by this application.
	If they are all valid it will call run_nml_servers(), otherwise it will exit immediately.

	MODIFICATIONS:
	Sun Mar 06 10:46:53 EST 2005	Created.

*/

// Include Files
#include <stdlib.h> // exit()
#include "rcs.hh" 	// Common RCS definitions
#include "wmn.hh" 	// NML Commands and Status definitions for wm
#include "obstacle_mapn.hh" 	// obstacle_mapFormat

// NML Channel Pointers
static RCS_CMD_CHANNEL *wm_cmd = NULL;
static RCS_STAT_CHANNEL *wm_stat = NULL;
static NML *obstacle_map= NULL;

static int InitNML()
{


	// wm
	wm_cmd = new RCS_CMD_CHANNEL(wmFormat, "wm_cmd", "wmsvr", "rcs_single_dir.nml");
	if(NULL == wm_cmd)
		return -1;
	if(!wm_cmd->valid())
		return -1;

	wm_stat = new RCS_STAT_CHANNEL(wmFormat, "wm_sts", "wmsvr", "rcs_single_dir.nml");
	if(NULL == wm_stat)
		return -1;
	if(!wm_stat->valid())
		return -1;


	obstacle_map = new NML(obstacle_mapFormat, "obstacle_map", "wmsvr", "rcs_single_dir.nml");
	if(NULL == obstacle_map)
		return -1;
	if(!obstacle_map->valid())
		return -1;

	return 0;
}

static void DeleteNML()
{

	// wm
	if(NULL != wm_cmd)
	{
		delete wm_cmd;
		wm_cmd = NULL;
	}

	if(NULL != wm_stat)
	{
		delete wm_stat;
		wm_stat = NULL;
	}


	if(NULL != obstacle_map)
	{
		delete obstacle_map;
		obstacle_map = NULL;
	}


}

// Main 
#ifdef VXWORKS
extern "C" int wmsvr_run();

int wmsvr_run()
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

