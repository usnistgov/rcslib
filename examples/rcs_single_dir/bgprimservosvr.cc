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
	bgprimservosvr.cc

	This C++ file provides a main routin to start an NML server for this application
	It connects to all of the NML channels used by this application.
	If they are all valid it will call run_nml_servers(), otherwise it will exit immediately.

	MODIFICATIONS:
	Sun Mar 06 10:46:50 EST 2005	Created.

*/

// Include Files
#include <stdlib.h> // exit()
#include "rcs.hh" 	// Common RCS definitions
#include "bgn.hh" 	// NML Commands and Status definitions for bg
#include "primn.hh" 	// NML Commands and Status definitions for prim
#include "servon.hh" 	// NML Commands and Status definitions for servo
#include "pose_datan.hh" 	// pose_dataFormat

#define TEST_SET_CFG_STUFF

#ifdef TEST_SET_CFG_STUFF
#include "offsets.hh"
#include "servo_cmd_setn.hh"
#include "servo_stat_cfgn.hh"
#endif

// NML Channel Pointers
static RCS_CMD_CHANNEL *bg_cmd = NULL;
static RCS_STAT_CHANNEL *bg_stat = NULL;
static RCS_CMD_CHANNEL *prim_cmd = NULL;
static RCS_STAT_CHANNEL *prim_stat = NULL;
static RCS_CMD_CHANNEL *servo_cmd = NULL;
static RCS_STAT_CHANNEL *servo_stat = NULL;
static RCS_CMD_CHANNEL *servo_cmd_set = NULL;
static RCS_STAT_CHANNEL *servo_stat_cfg = NULL;
static NML *pose_data= NULL;
static NML *errlog = NULL;

static int InitNML()
{


	// bg
	bg_cmd = new RCS_CMD_CHANNEL(bgFormat, "bg_cmd", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == bg_cmd)
		return -1;
	if(!bg_cmd->valid())
		return -1;

	bg_stat = new RCS_STAT_CHANNEL(bgFormat, "bg_sts", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == bg_stat)
		return -1;
	if(!bg_stat->valid())
		return -1;


	// prim
	prim_cmd = new RCS_CMD_CHANNEL(primFormat, "prim_cmd", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == prim_cmd)
		return -1;
	if(!prim_cmd->valid())
		return -1;

	prim_stat = new RCS_STAT_CHANNEL(primFormat, "prim_sts", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == prim_stat)
		return -1;
	if(!prim_stat->valid())
		return -1;


	// servo
	servo_cmd = new RCS_CMD_CHANNEL(servoFormat, "servo_cmd", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == servo_cmd)
		return -1;
	if(!servo_cmd->valid())
		return -1;

	servo_stat = new RCS_STAT_CHANNEL(servoFormat, "servo_sts", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == servo_stat)
		return -1;
	if(!servo_stat->valid())
		return -1;


#ifdef TEST_SET_CFG_STUFF
	servo_cmd_set = new RCS_CMD_CHANNEL(servo_cmd_set_format, "servo_cmd_set", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == servo_cmd_set)
		return -1;
	if(!servo_cmd_set->valid())
		return -1;

	servo_stat_cfg = new RCS_STAT_CHANNEL(servo_stat_cfg_format, "servo_stat_cfg", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == servo_stat_cfg)
		return -1;
	if(!servo_stat_cfg->valid())
		return -1;

#endif


	pose_data = new NML(pose_dataFormat, "pose_data", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == pose_data)
		return -1;
	if(!pose_data->valid())
		return -1;

	errlog = new NML(nmlErrorFormat, "errlog", "bgprimservosvr", "rcs_single_dir.nml");
	if(NULL == errlog)
		return -1;
	if(!errlog->valid())
		return -1;

	return 0;
}

static void DeleteNML()
{

	// bg
	if(NULL != bg_cmd)
	{
		delete bg_cmd;
		bg_cmd = NULL;
	}

	if(NULL != bg_stat)
	{
		delete bg_stat;
		bg_stat = NULL;
	}


	// prim
	if(NULL != prim_cmd)
	{
		delete prim_cmd;
		prim_cmd = NULL;
	}

	if(NULL != prim_stat)
	{
		delete prim_stat;
		prim_stat = NULL;
	}


	// servo
	if(NULL != servo_cmd)
	{
		delete servo_cmd;
		servo_cmd = NULL;
	}

	if(NULL != servo_stat)
	{
		delete servo_stat;
		servo_stat = NULL;
	}


	if(NULL != pose_data)
	{
		delete pose_data;
		pose_data = NULL;
	}

	if(NULL != errlog)
	{
		delete errlog;
		errlog = NULL;
	}

}

// Main 
#ifdef VXWORKS
extern "C" int bgprimservosvr_run();

int bgprimservosvr_run()
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

