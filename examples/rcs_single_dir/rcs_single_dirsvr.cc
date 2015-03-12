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
	rcs_single_dirsvr.cc

	This C++ file provides a main routin to start an NML server for this application
	It connects to all of the NML channels used by this application.
	If they are all valid it will call run_nml_servers(), otherwise it will exit immediately.

	MODIFICATIONS:
	Sun Mar 06 10:46:52 EST 2005	Created.

*/

// Include Files
#include <stdlib.h> // exit()
#include "rcs.hh" 	// Common RCS definitions
#include "bgn.hh" 	// NML Commands and Status definitions for bg
#include "primn.hh" 	// NML Commands and Status definitions for prim
#include "robot_supern.hh" 	// NML Commands and Status definitions for robot_super
#include "servon.hh" 	// NML Commands and Status definitions for servo
#include "spn.hh" 	// NML Commands and Status definitions for sp
#include "wmn.hh" 	// NML Commands and Status definitions for wm
#include "pose_datan.hh" 	// pose_dataFormat
#include "obstacle_mapn.hh" 	// obstacle_mapFormat
#include "sensor_datan.hh" 	// sensor_dataFormat

// NML Channel Pointers
static RCS_CMD_CHANNEL *bg_cmd = NULL;
static RCS_STAT_CHANNEL *bg_stat = NULL;
static RCS_CMD_CHANNEL *prim_cmd = NULL;
static RCS_STAT_CHANNEL *prim_stat = NULL;
static RCS_CMD_CHANNEL *robot_super_cmd = NULL;
static RCS_STAT_CHANNEL *robot_super_stat = NULL;
static RCS_CMD_CHANNEL *servo_cmd = NULL;
static RCS_STAT_CHANNEL *servo_stat = NULL;
static RCS_CMD_CHANNEL *sp_cmd = NULL;
static RCS_STAT_CHANNEL *sp_stat = NULL;
static RCS_CMD_CHANNEL *wm_cmd = NULL;
static RCS_STAT_CHANNEL *wm_stat = NULL;
static NML *pose_data= NULL;
static NML *obstacle_map= NULL;
static NML *sensor_data= NULL;
static NML *errlog = NULL;

static int InitNML()
{


	// bg
	bg_cmd = new RCS_CMD_CHANNEL(bgFormat, "bg_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == bg_cmd)
		return -1;
	if(!bg_cmd->valid())
		return -1;

	bg_stat = new RCS_STAT_CHANNEL(bgFormat, "bg_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == bg_stat)
		return -1;
	if(!bg_stat->valid())
		return -1;


	// prim
	prim_cmd = new RCS_CMD_CHANNEL(primFormat, "prim_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == prim_cmd)
		return -1;
	if(!prim_cmd->valid())
		return -1;

	prim_stat = new RCS_STAT_CHANNEL(primFormat, "prim_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == prim_stat)
		return -1;
	if(!prim_stat->valid())
		return -1;


	// robot_super
	robot_super_cmd = new RCS_CMD_CHANNEL(robot_superFormat, "robot_super_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == robot_super_cmd)
		return -1;
	if(!robot_super_cmd->valid())
		return -1;

	robot_super_stat = new RCS_STAT_CHANNEL(robot_superFormat, "robot_super_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == robot_super_stat)
		return -1;
	if(!robot_super_stat->valid())
		return -1;


	// servo
	servo_cmd = new RCS_CMD_CHANNEL(servoFormat, "servo_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == servo_cmd)
		return -1;
	if(!servo_cmd->valid())
		return -1;

	servo_stat = new RCS_STAT_CHANNEL(servoFormat, "servo_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == servo_stat)
		return -1;
	if(!servo_stat->valid())
		return -1;


	// sp
	sp_cmd = new RCS_CMD_CHANNEL(spFormat, "sp_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == sp_cmd)
		return -1;
	if(!sp_cmd->valid())
		return -1;

	sp_stat = new RCS_STAT_CHANNEL(spFormat, "sp_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == sp_stat)
		return -1;
	if(!sp_stat->valid())
		return -1;


	// wm
	wm_cmd = new RCS_CMD_CHANNEL(wmFormat, "wm_cmd", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == wm_cmd)
		return -1;
	if(!wm_cmd->valid())
		return -1;

	wm_stat = new RCS_STAT_CHANNEL(wmFormat, "wm_sts", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == wm_stat)
		return -1;
	if(!wm_stat->valid())
		return -1;


	pose_data = new NML(pose_dataFormat, "pose_data", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == pose_data)
		return -1;
	if(!pose_data->valid())
		return -1;


	obstacle_map = new NML(obstacle_mapFormat, "obstacle_map", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == obstacle_map)
		return -1;
	if(!obstacle_map->valid())
		return -1;


	sensor_data = new NML(sensor_dataFormat, "sensor_data", "rcs_single_dirsvr", "rcs_single_dir.nml");
	if(NULL == sensor_data)
		return -1;
	if(!sensor_data->valid())
		return -1;

	errlog = new NML(nmlErrorFormat, "errlog", "rcs_single_dirsvr", "rcs_single_dir.nml");
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


	// robot_super
	if(NULL != robot_super_cmd)
	{
		delete robot_super_cmd;
		robot_super_cmd = NULL;
	}

	if(NULL != robot_super_stat)
	{
		delete robot_super_stat;
		robot_super_stat = NULL;
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


	if(NULL != pose_data)
	{
		delete pose_data;
		pose_data = NULL;
	}


	if(NULL != obstacle_map)
	{
		delete obstacle_map;
		obstacle_map = NULL;
	}


	if(NULL != sensor_data)
	{
		delete sensor_data;
		sensor_data = NULL;
	}

	if(NULL != errlog)
	{
		delete errlog;
		errlog = NULL;
	}

}

// Main 
#ifdef VXWORKS
extern "C" int rcs_single_dirsvr_run();

int rcs_single_dirsvr_run()
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

