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
   nml_mod.cc

   Definition of NML_MODULE class.
   */

/*
   Modification history:

    9-Sep-1997 WPS eliminated LOAL_LOCAL_VARIABLES, UNLOAD_LOCAL_VARIABLES,
    READ_CHILD_BUFFERS, WRITE_CHILD_BUFFERS, and DETERMINE_CHILD_STATUS
   29-May-1997 WPS replaced include nml_emc.hh with nml_oi.hh
    3-Apr-1997 WPS brought over the functions from RCS_MODULE_C
   11-Feb-1997 WPS modified  calc_avg_time, update_line_history, and
   stop_timing all use exec_history but fail to check to
   see if it is initialized.
   I override them here minimize changes to rcs_module.cc
   3-Dec-1996  FMP changed NML to RCS_CMD/STAT_CHANNEL for commandIn,
   statusOut, etc.
   112696-hui, changed the base class for commandInData, etc., from NMLmsg
   to RCS_CMD_MSG and RCS_STAT_MSG.
   16-Oct-1996  FMP inited done to 0 in ctor
   29-Jul-1996  FMP moved NML_ERROR, NML_TEXT, NML_DISPLAY, and NML_STATUS
   into nml_emc.cc
   29-Jul-1996  FMP added NML_TEXT and NML_DISPLAY classes
   10-Jun-1996  Fred Proctor added malloc/free of cmdOutstanding array
   5-Jun-1996  Fred Proctor added NML error stuff
   16-Apr-1996  Fred Proctor added NMLmsg * initing
   5-Apr-1996  Fred Proctor created
    */

#if HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#include "nml_mod_no_config.h"
#endif

#include "nml_mod.hh"
#include "stat_msg.hh"		// RCS_STATUS, RCS_STATE, RCS_STAT_MSG, RCS_STAT_CHANNL
#include "cmd_msg.hh"		// class RCS_CMD_CHANNEL, RCS_CMD_MSG
#include "nml_oi.hh"		// NML_ERROR, NML_TEXT, NML_DISPLAY
#include "timer.hh"		// class RCS_TIMER
#include "rcs_prnt.hh"
#include "cms.hh"		//  class CMS

#ifdef DEBUG_MEMORY
#include "dbg_mem.h"
#else
#ifndef DEBUG_MALLOC
#define DEBUG_MALLOC malloc
#endif
#ifndef DEBUG_CALLOC
#define DEBUG_CALLOC calloc
#endif
#ifndef DEBUG_REALLOC
#define DEBUG_REALLOC realloc
#endif
#ifndef DEBUG_FREE
#define DEBUG_FREE free
#endif
#endif
// DEBUG_MEMORY

NML_MODULE::NML_MODULE ():
  force_command(0),
  cycle_start(0),		// Data
  cycle_stop(0),
  command_time_averaged(0),
  new_command_sequence(0),
  new_line_num_sequence(0),
  new_sup_request(0),
  delta_clock(0),
  command_current_time(0),
  pause_status(UNINITIALIZED_STATUS),
  command(0),
  last_line(0),
  execute(0),
  command_time(0),
  state(UNINITIALIZED_STATE),
  status(UNINITIALIZED_STATUS),
  sup_req_num(0),
  sup_req_num_echo(0),
  command_num(0),
  command_num_echo(0),
  matched(0),
  stateBegin(0),
  source_file(0),
  source_line(0),
  commandIn(0),	// NML channel for command from supervisor
  statusOut(0),	// NML channel for status to supervisor
  errorLog(0),		// NML channel for logging errors
  commandInData(0),	// ptr to NML data in channel
  statusOutData(0),	// ptr to NML data to be put in channel
  commandLastNum(0),		// array of command nums saved before writes
  commandOutstanding(0),	// array of flags, 1 = command has been sent,

  subs(0),	// pointer to array of pointers to subordinates
  statusInData(0),	// ptrs to NML data in channels
  commandOutData(0),	// ptrs to NML data in channels
  timer(0),		// synch timer
  done(0),			// non-zero means stop calling controller()
  commands_received(0),
  commands_executed(0),
  cycles(0),
  cycles_executing(0),
  cycles_executing_completed_commands(0),
  cycles_executing_this_command(0),
  last_command_completed_serial_number(0),
  expected_cycle_time(0),
  start_run_time(0),
  last_start_run_time(0),
  stop_run_time(0),
  total_run_time(0),
  min_run_time(0),
  max_run_time(0),
  start_cycle_time(0),
  min_cycle_time(0),
  max_cycle_time(0),
  last_cycle_time(0),
  proc_name(0),
  temp_file(0),
  temp_line(0),
  numSubordinates(0),		// number of subordinates for this module
  Dclock_expiration(0),
  Dclock_start_time(0),
  log_line(0),
  log_src(0),
  subs_allocated(0),
  log_status_read_error_count(0),
  log_status_read_error_max(100),
  disable_log_status_read_error(false),
  ignore_invalid_cmd_channel(false),
  ignore_invalid_stat_channel(false),
  ignore_invalid_errlog_channel(false),
  ignore_status_write_err(false)
{
  zero_common_vars ();
}


void
NML_MODULE::zero_common_vars ()
{
  commandIn = 0;
  force_command = 0;
  commandInData = 0;
  statusOut = 0;
  statusOutData = 0;
  subs = 0;
  commandLastNum = 0;
  commandOutstanding = 0;
  timer = 0;
  numSubordinates = 0;
  errorLog = 0;
  done = 0;
  statusInData = 0;
  commandOutData = 0;


  cycle_start = 0;
  cycle_stop = 0;
  command_time_averaged = 0;
  new_command_sequence = 0;
  new_line_num_sequence = 0;
  new_sup_request = 0;

  delta_clock = 0;
  command_current_time = 0;

  pause_status = 0;
  command = 0;
  last_line = 0;

  execute = 0;
  command_time = 0;
  state = UNINITIALIZED_STATE;
  status = UNINITIALIZED_STATUS;

  sup_req_num = 0;
  sup_req_num_echo = 0;
  command_num = 0;
  command_num_echo = 0;

  // state table vars
  matched = 0;
  stateBegin = 0;

#if 0
//below 010997-hui
  exec_status.command_num = 0;
  exec_status.command = 0;
  exec_status.status = 0;
  exec_status.state = 0;
  exec_status.line_num = 0;
  exec_status.command_time = 0;

  run_command.run_state = 0;
  run_command.step_num = 0;

  run_status.step_num_echo = 0;
  run_status.dummy1 = 0;
#endif

  source_line = -1;
  source_file = NULL;

  commands_received = 0;
  commands_executed = 0;
  last_command_completed_serial_number = -1;
  total_run_time = 0;
  cycles = 0;
  max_cycle_time = 0;
  min_cycle_time = 1e6;
  last_cycle_time = 0;
  start_run_time = 0;
  stop_run_time = 0;
  last_start_run_time = 0;
  subs_allocated = 0;
  expected_cycle_time = 0;
  min_run_time = 1e6;
  max_run_time = 0;
  start_cycle_time = 0;
  temp_file = 0;
  temp_line = 0;
  Dclock_expiration = 0;
  Dclock_start_time = 0;
  log_line = 0;
  log_src = 0;
  log_status_read_error_count =0;
  log_status_read_error_max=100;
  disable_log_status_read_error=false;
}

NML_MODULE::~NML_MODULE ()
{
  int t;

  if (commandIn != 0)
    delete commandIn;
  commandIn = 0;

  if (statusOut != 0)
    delete statusOut;
  statusOut = 0;

  if (errorLog != 0)
    delete (errorLog);
  errorLog = 0;

  if (subs != 0)
    {
      for (t = 0; t < numSubordinates; t++)
	{
	  if (subs[t] == 0)
	    {
	      continue;
	    }
	  if (subs[t]->commandOut != 0)
	    {
	      delete subs[t]->commandOut;
	      subs[t]->commandOut = 0;
	    }
	  if (subs[t]->statusIn != 0)
	    {
	      delete subs[t]->statusIn;
	      subs[t]->statusIn = 0;
	    }
	  subs[t]->commandOutData = 0;
	  subs[t]->statusInData = 0;
	  delete subs[t];
	  subs[t] = 0;
	}

      DEBUG_FREE (subs);

#if 0
      if (NULL != ini)
	{
	  delete ini;
	  ini = NULL;
	}
#endif
      subs = 0;
    }

  if (commandOutstanding != 0)
    {
      DEBUG_FREE (commandOutstanding);
      commandOutstanding = 0;
    }

  if (commandLastNum != 0)
    {
      DEBUG_FREE (commandLastNum);
      commandLastNum = 0;
    }

  if (timer != 0)
    {
      delete timer;
      timer = 0;
    }

  if (proc_name != 0)
    {
      DEBUG_FREE (proc_name);
      proc_name = 0;
    }

}

void
NML_MODULE::setCmdChannel (RCS_CMD_CHANNEL * cmd_channel)
{
  if (NULL == cmd_channel)
    {
      if(ignore_invalid_cmd_channel)
	{
	  return;
	}
      rcs_print_error ("Command channel is NULL.\n");
      exit (-1);
    }

  commandIn = cmd_channel;
  if (!ignore_invalid_cmd_channel &&
      !commandIn->valid ())
    {
      rcs_print_error ("Command channel is invalid.\n");
      exit (-1);
    }
  else
    {
      commandInData = commandIn->get_address ();
    }
  if (NULL != commandIn->cms)
    {
      if (NULL != commandIn->cms->ProcessName)
	{
	  proc_name =
	    (char *) DEBUG_MALLOC (strlen (commandIn->cms->ProcessName) + 1);
	  strcpy (proc_name, commandIn->cms->ProcessName);
	}
    }
}

void
NML_MODULE::setStatChannel (RCS_STAT_CHANNEL * stat_channel,
			    RCS_STAT_MSG * stat_msg)
{
  if (NULL == stat_channel)
    {
      if(ignore_invalid_stat_channel)
	{
	  return;
	}
      rcs_print_error ("Status channel is NULL.\n");
      exit (-1);
    }
  statusOut = stat_channel;
  if (!ignore_invalid_stat_channel &&
      !statusOut->valid ())
    {
      rcs_print_error ("Status channel is invalid.\n");
      exit (-1);
    }
  if (NULL == stat_msg)
    {
      rcs_print_error ("Status out message is NULL.\n");
      exit (-1);
    }
  if (stat_msg->type <= 0)
    {
      rcs_print_error
	("Status out message must have positive type. (type = %ld)\n",
	 (long) stat_msg->type);
      exit (-1);
    }
  if (stat_msg->size < ((long) sizeof (RCS_STAT_MSG)))
    {
      rcs_print_error
	("Status out message must have a size of atleast sizeof(RCS_STAT_MSG) or %lu bytes,",
	 (unsigned long) sizeof (RCS_STAT_MSG));
      rcs_print_error ("but the status out message size was only %ld.\n",
		       stat_msg->size);
      exit (-1);
    }
  statusOutData = stat_msg;
}

void
NML_MODULE::setErrorLogChannel (NML * errorLog_channel)
{
  if (NULL == errorLog_channel)
    {
      if(ignore_invalid_errlog_channel)
	{
	  return;
	}
      rcs_print_error ("Error Log channel is NULL.\n");
      exit (-1);
    }
  errorLog = errorLog_channel;
  if (!ignore_invalid_errlog_channel &&
      !errorLog->valid ())
    {
      rcs_print_error ("Error Log channel is invalid.\n");
      exit (-1);
    }

}

int
NML_MODULE::addSubordinate (RCS_CMD_CHANNEL * cmd_channel,
			    RCS_STAT_CHANNEL * stat_channel)
{
  int sub_num = numSubordinates;
  setSubordinates (numSubordinates + 1);
  if (NULL == cmd_channel)
    {
      logError ("Command Channel for subordinate %d is NULL.\n", sub_num);
      exit (-1);
    }
  if (NULL == stat_channel)
    {
      logError ("Status Channel for subordinate %d is NULL.\n", sub_num);
      exit (-1);
    }
  subs[sub_num]->commandOut = cmd_channel;
  if (!subs[sub_num]->commandOut->valid ())
    {
      logError ("Command Channel for subordinate %d is invalid.\n", sub_num);
      exit (-1);
    }
  else
    {
      subs[sub_num]->commandOutData =
	subs[sub_num]->commandOut->get_address ();
    }
  commandOutData[sub_num] = subs[sub_num]->commandOutData;

  subs[sub_num]->statusIn = stat_channel;
  if (!subs[sub_num]->statusIn->valid ())
    {
      logError ("Command Channel for subordinate %d is invalid.\n", sub_num);
      exit (-1);
    }
  else
    {
      subs[sub_num]->statusInData = subs[sub_num]->statusIn->get_address ();
    }
  statusInData[sub_num] = subs[sub_num]->statusInData;
  return sub_num;
}

int
  NML_MODULE::use_realloc =
  1;

int
NML_MODULE::setSubordinates (int number)
{
  int t;
  int previousNumSubordinates = numSubordinates;
  if (NULL == subs)
    {
      subs_allocated = 0;
    }

  if (subs_allocated > numSubordinates)
    {
      numSubordinates = number;
      subs[number]->commandOut = 0;
      subs[number]->commandOutData = 0;
      subs[number]->statusIn = 0;
      subs[number]->statusInData = 0;
      statusInData[number] = 0;
      commandOutData[number] = 0;
      return 0;
    }

  if (number < 0)
    return -1;

  // record number of subordinates
  numSubordinates = number;

  // get out now if nothing else to do
  if (number == 0)
    return 0;

  // allocate subordinate NML channels
  // On NT we sometimes get a lame problem with realloc
  if (use_realloc)
    {
      subs = (NML_SUBORDINATE_STRUCT **) realloc (subs,
						  number *
						  sizeof
						  (NML_SUBORDINATE_STRUCT *));

      statusInData = (RCS_STAT_MSG **) realloc (statusInData,
						number *
						sizeof (RCS_STAT_MSG *));

      commandOutData = (RCS_CMD_MSG **) realloc (statusInData,
						 number *
						 sizeof (RCS_CMD_MSG *));
    }
  else
    {
      NML_SUBORDINATE_STRUCT **old_subs = subs;
      RCS_STAT_MSG **old_statusInData = statusInData;
      RCS_CMD_MSG **old_commandOutData = commandOutData;

      subs =
	(NML_SUBORDINATE_STRUCT **) DEBUG_MALLOC (number *
						  sizeof
						  (NML_SUBORDINATE_STRUCT *));
      if (NULL != old_subs && NULL != subs)
	{
	  memcpy (subs, old_subs,
		  previousNumSubordinates *
		  sizeof (NML_SUBORDINATE_STRUCT *));
	}


      statusInData =
	(RCS_STAT_MSG **) DEBUG_MALLOC (number * sizeof (RCS_STAT_MSG *));
      if (NULL != old_statusInData && NULL != statusInData)
	{
	  memcpy (statusInData, old_statusInData,
		  previousNumSubordinates * sizeof (RCS_STAT_MSG *));
	}

      commandOutData =
	(RCS_CMD_MSG **) DEBUG_MALLOC (number * sizeof (RCS_CMD_MSG *));
      if (NULL != old_commandOutData && NULL != commandOutData)
	{
	  memcpy (commandOutData, old_commandOutData,
		  previousNumSubordinates * sizeof (RCS_CMD_MSG *));
	}


    }
#if 0
  // allocate commandOutstanding array
  commandLastNum = (int *) realloc (commandLastNum, number * sizeof (int));
  commandOutstanding = (int *) realloc (commandOutstanding,
					number * sizeof (int));
#endif

  if (NULL == subs || NULL == statusInData)
    {
      rcs_print_error ("Out of memory.\n");
      exit (-1);
    }


  // initialize each NML channel in the new arrays to 0
  for (t = previousNumSubordinates; t < number; t++)
    {
      subs[t] = new NML_SUBORDINATE_STRUCT ();
      if (NULL == subs[t])
	{
	  rcs_print_error ("Out of memory.\n");
	  exit (-1);
	}
      subs[t]->commandOut = 0;
      subs[t]->commandOutData = 0;
      subs[t]->statusIn = 0;
      subs[t]->statusInData = 0;
      subs[t]->disable_log_status_read_error=false;
      statusInData[t] = 0;
      commandOutData[t] = 0;

      //      commandOutstanding[t] = 0;
    }

  subs_allocated = numSubordinates;
  return 0;
}

/*
   Error, text, and display functions. Note that all these go to
   the errorLog channel
   */

int
NML_MODULE::setLogInfo (const char *src, int l)
{
  log_line = l;
  log_src = src;
  return (0);
}

int
NML_MODULE::logError (const char *fmt, ...)
{
  NML_ERROR error_msg;
  va_list ap;

  status = RCS_ERROR;
  memset (error_msg.error, 0, NML_ERROR_LEN);
  if (log_line > 0 && log_src != NULL)
    {
      SNPRINTF_FUNC ( SNPRINTF_ARGS(error_msg.error,sizeof(error_msg.error)),
		      "%s:%d ", log_src, log_line);
    }

  // write args to NML message
  va_start (ap, fmt);
  vsprintf (error_msg.error, fmt, ap);

  va_end (ap);

  // force a NULL at the end for safety
  error_msg.error[NML_ERROR_LEN - 1] = 0;

#ifndef DISABLE_RCS_PRINT
  set_print_rcs_error_info (NULL, -1);
  print_rcs_error_new ("%s\n", error_msg.error);
#endif

  // check channel for validity
  if (errorLog == NULL)
    {
      return -1;
    }

  // write it
  return errorLog->write (error_msg);
}


int
NML_MODULE::logText (const char *fmt, ...)
{
  NML_TEXT text_msg;
  va_list ap;

  // write args to NML message
  va_start (ap, fmt);
  vsprintf (text_msg.text, fmt, ap);
  va_end (ap);

  // force a NULL at the end for safety
  text_msg.text[NML_TEXT_LEN - 1] = 0;

  rcs_print ("%s\n", text_msg.text);


  // check channel for validity
  if (errorLog == NULL)
    {
      return -1;
    }

  // write it
  return errorLog->write (text_msg);
}

int
logTextToNML (NML * errorLog, const char *fmt, ...)
{
  NML_TEXT text_msg;
  va_list ap;

  // check channel for validity
  if (errorLog == NULL)
    return -1;

  // write args to NML message
  va_start (ap, fmt);
  vsprintf (text_msg.text, fmt, ap);
  va_end (ap);

  // force a NULL at the end for safety
  text_msg.text[NML_TEXT_LEN - 1] = 0;

  //  rcs_print("%s\n", text_msg.text);


  // write it
  if (errorLog->write (text_msg) < 0)
    {
      return -1;
    }

  return 0;
}

int
NML_MODULE::requestDisplay (const char *display)
{
  NML_DISPLAY display_msg;

  // check channel for validity
  if (errorLog == NULL)
    return -1;
  if (!errorLog->valid ())
    return -1;

  // write args to NML message
  strcpy (display_msg.display, display);

  // force a NULL at the end for safety
  display_msg.display[NML_DISPLAY_LEN - 1] = 0;

  // write it
  errorLog->write (display_msg);

  return 0;
}




// The rest of the functions here were taken from RCS_MODULE_C
// Virtual Functions
//

void
NML_MODULE::DECISION_PROCESS (void)
{

}


void
NML_MODULE::setSelfCommand (RCS_CMD_MSG * cmd)
{
  if (NULL == cmd || NULL == statusOutData || NULL == commandInData
      || NULL == commandIn)
    {
      return;
    }
  if (NULL == commandIn->cms)
    {
      return;
    }
  if (cmd->size > commandIn->cms->size)
    {
      rcs_print_error
	("NML_MODULE::setSelfCommand, Command too big! %ld(0x%lX) > %ld(0x%lX)\n",
	 cmd->size, (unsigned long)cmd->size, 
	 commandIn->cms->size, (unsigned long)commandIn->cms->size);
    }
  statusOutData->echo_serial_number++;
  cmd->serial_number = statusOutData->echo_serial_number + 1;
  commandIn->write (cmd);
  memcpy (commandInData, cmd, cmd->size);
  statusOutData->command_type = cmd->type;
  force_command = 1;
}

void
NML_MODULE::read_command_in ()
{
  NMLTYPE type;

  if (force_command)
    {
      force_command = 0;
      return;
    }

  switch (type = commandIn->read ())
    {
    case -1:
      logError ("Can not read input command. (%d)", commandIn->error_type);
      if (NULL != statusOutData)
	{
	  statusOutData->command_type = type;
	}
      break;

    case 0:
      // no new command
      break;

    default:
      commandInData = commandIn->get_address ();
      if (NULL != statusOutData)
	{
	  statusOutData->command_type = type;
	}
      break;
    }
}

void
NML_MODULE::read_subordinates_status ()
{
  int t;
  NMLTYPE type;

  // read NML STATUS buffer from subordinates
  for (t = 0; t < numSubordinates; t++)
    {
      if (NULL == subs[t])
	{
	  continue;
	}
      if (NULL == subs[t]->statusIn)
	{
	  continue;
	}
      switch (type = subs[t]->statusIn->peek ())
	{
	case -1:
	  // error on NML channel
	  if(!subs[t]->disable_log_status_read_error 
	     && !disable_log_status_read_error)
	    {
	      if(log_status_read_error_count > log_status_read_error_max)
		{
		  logError ("Can not read status from subodinate %s (%d).\n",
			    subs[t]->statusIn->cms->BufferName,
			    subs[t]->statusIn->error_type);
		  log_status_read_error_count++;
		}
	      else if(log_status_read_error_count == log_status_read_error_max)
		{
		  logError ("Maximum errors reading status occurred,  no more errors will be loged.\n");
		}
	    }
	  break;

	case 0:
	  // no new data
	  break;

	default:
	  subs[t]->statusInData = subs[t]->statusIn->get_address ();
	  if (NULL != subs[t]->statusInData
	      && NULL != subs[t]->commandOutData)
	    {
	      if (subs[t]->statusInData->echo_serial_number !=
		  subs[t]->commandOutData->serial_number)
		{
		  subs[t]->statusInData->status = RCS_EXEC;
		}
	    }
	  // something new in STATUS
	  break;
	}
    }
}

void
NML_MODULE::READ_COMM_BUFFERS (void)
{
  read_command_in ();
  read_subordinates_status ();
  check_if_new_command ();
}

void
NML_MODULE::PRE_PROCESS (void)
{

}

void
NML_MODULE::POST_PROCESS (void)
{

}

void
NML_MODULE::write_status_out ()
{
  if (NULL == statusOutData)
    {
      return;
    }
  // update NML STATUS
  statusOutData->command_type = commandInData->type;
  statusOutData->state = state;
  statusOutData->status = (RCS_STATUS) status;
  if (status == RCS_DONE &&
      last_command_completed_serial_number != commandInData->serial_number)
    {
      last_command_completed_serial_number = commandInData->serial_number;
      commands_executed++;
    }

  if(source_line > 0 || statusOutData->source_line <= 0)
    {
      statusOutData->source_line = source_line;
    }
  if (NULL != source_file)
    {
      strncpy (statusOutData->source_file, source_file, 64);
    }

  // write STATUS
  if (-1 == statusOut->write (statusOutData))
    {
      if(ignore_status_write_err)
	{
	  logError ("bad write to status (%d)\n", statusOut->error_type);
	}
    }
}

void
NML_MODULE::write_commands_to_subordinates ()
{
  int t;
  // write subordinates
  for (t = 0; t < numSubordinates; t++)
    {
      if (subs[t]->commandOutData == NULL)
	{
	  continue;
	}
      if (subs[t]->commandOutData->type == 0)
	{
	  continue;
	}
      if (subs[t]->statusInData != NULL)
	{
	  if (subs[t]->statusInData->echo_serial_number ==
	      subs[t]->commandOutData->serial_number
	      && subs[t]->modification_number <= 0)
	    {
	      subs[t]->commandOutData->type = 0;
	      continue;
	    }
	}
      if (-1 == subs[t]->commandOut->write (subs[t]->commandOutData))
	{
	  logError ("Error writing to %s (%d).\n",
		    subs[t]->commandOut->cms->BufferName,
		    subs[t]->commandOut->error_type);
	}
      else
	{
	  //if the subordinates  command buffer is queued then
	  // mark the command so it isn't sent out again.
	  if (subs[t]->commandOut->cms->queuing_enabled)
	    {
	      subs[t]->commandOutData->type = 0;
	    }
	}
    }
}

void
NML_MODULE::WRITE_COMM_BUFFERS (void)
{
  write_status_out ();
  write_commands_to_subordinates ();
}


//***********************************************************************************
//* Overhead ** Overhead ** Overhead ** Overhead **Overhead ** Overhead **Overhead **
//***********************************************************************************

void
NML_MODULE::controller (void)
{

  check_cycle_time_start ();
  READ_COMM_BUFFERS ();
  PRE_PROCESS ();

  /* reset state table flags */
  stateBegin = 1;
  matched = 0;

  if (commandInData != NULL)
    {
      if (statusOutData != NULL)
	{
	  if (statusOutData->command_type > 0)
	    {
	      DECISION_PROCESS ();
	    }
	}
    }

  POST_PROCESS ();
  WRITE_COMM_BUFFERS ();
  check_cycle_time_end ();

}

void
NML_MODULE::check_cycle_time_start ()
{
  start_run_time = etime ();
  cycles++;
  if (cycles < 2)
    {
      start_cycle_time = start_run_time;
    }
  else
    {
      last_cycle_time = start_run_time - last_start_run_time;
      if (last_cycle_time > max_cycle_time)
	{
	  max_cycle_time = last_cycle_time;
	}
      if (last_cycle_time < min_cycle_time)
	{
	  min_cycle_time = last_cycle_time;
	}
    }
  last_start_run_time = start_run_time;
}


void
NML_MODULE::check_if_new_command (void)
{
  if (NULL == commandInData)
    {
      return;
    }
  if (NULL == statusOutData)
    {
      return;
    }
  if (statusOutData->echo_serial_number != commandInData->serial_number)
    {
      state = NEW_COMMAND;
      status = RCS_EXEC;
      commands_received++;
      statusOutData->echo_serial_number = commandInData->serial_number;
      statusOutData->command_type = commandInData->type;
      command_time_averaged = 0;
      new_command_sequence = 1;
      new_line_num_sequence = 1;
//    exec_status.line_num = 0;
    }
}

void
NML_MODULE::print_statistics ()
{
  double total_time = stop_run_time - start_cycle_time;
  rcs_print ("\n*************************************************\n");
  if (NULL != proc_name)
    {
      rcs_print ("Module Name: %s\n", proc_name);
    }
  rcs_print ("Total cycles: %d\n", cycles);
  rcs_print ("Total time: %f\n", total_time);
  if (cycles > 0)
    {
      rcs_print ("Average Cycle Time: %f\n", total_time / cycles);
    }
  else
    {
      rcs_print ("Average Cycle Time: CAN NOT BE DETERMINED\n");
    }

  rcs_print ("Minimum Cycle Time: %f\n", min_cycle_time);
  rcs_print ("Max Cycle Time: %f\n", max_cycle_time);
  rcs_print ("Commands Received: %d\n", commands_received);
  if (total_time > 0)
    {
      rcs_print ("Commands Received per second: %f\n",
		 commands_received / total_time);
    }
  else
    {
      rcs_print ("Commands Received per second: CAN NOT BE DETERMINED\n");
    }

  if (total_time > 0)
    {
      rcs_print ("Load: %f%%\n", total_run_time * 100 / total_time);
    }
  else
    {
      rcs_print ("Load: CAN NOT BE DETERMINED\n");
    }

  rcs_print ("*************************************************\n");
}

void
NML_MODULE::check_cycle_time_end ()
{
  stop_run_time = etime ();
  total_run_time += stop_run_time - start_run_time;
}




void
NML_MODULE::set_file_and_line (char *_source_file, int _source_line)
{
  temp_file = _source_file;
  temp_line = _source_line;
}


/*  'stateBegin' and 'matched' are set to 1 and 0, respectively, by
    controller(). Calls to stateMatch() in the controller logic
    will use these to determine if the line number needs to be set to
    1 or incremented, and if matches have occurred so that only one
    state table line is executed per pass. */
int
NML_MODULE::stateMatch (char *_source_file, int _source_line, int st,
			int conds)
{
  set_file_and_line (_source_file, _source_line);
  return stateMatch (st, conds);
}

int
NML_MODULE::stateMatch (int st, int conds)
{
  if (matched)
    {
      return 0;
    }

  // check if this is the first line in the state table
  if (stateBegin)
    {
      if (NULL != statusOutData)
	{
	  statusOutData->line = 0;
	}
      source_line = -1;
      source_file = NULL;
      stateBegin = 0;
      if (state != st || !conds)
	{
	  temp_line = -1;
	  temp_file = NULL;
	}
    }
  else
    {
      // lines have gone before-- just increment
      if (NULL != statusOutData)
	{
	  statusOutData->line++;
	}
    }

  // see if we match state and conditions
  if (state == st && conds)
    {
      matched = 1;
      source_file = temp_file;
      source_line = temp_line;
      return 1;
    }
  else
    {
      // no match
      return 0;
    }
}

void
NML_MODULE::stateNext (int st)
{
  state = (RCS_STATE) st;
}

void
NML_MODULE::stateNext (RCS_STATE st)
{
  state = (RCS_STATE) st;
}

int
NML_MODULE::sendCommand (RCS_CMD_MSG * cmd_msg, int sub_num)
{
  if (sub_num >= numSubordinates || sub_num < 0)
    {
      return -1;
    }
  if (cmd_msg == NULL)
    {
      return -1;
    }
  if (cmd_msg->size <= 0 || cmd_msg->type <= 0)
    {
      return -1;
    }
  if (NULL == subs[sub_num])
    {
      return -1;
    }
  if (NULL == subs[sub_num]->statusInData)
    {
      return -1;
    }
  if (NULL == subs[sub_num]->commandOutData)
    {
      return -1;
    }
  if (NULL == subs[sub_num]->commandOut)
    {
      return -1;
    }
  if (NULL == subs[sub_num]->commandOut->cms)
    {
      return -1;
    }

  if (cmd_msg->size >= subs[sub_num]->commandOut->cms->size)
    {
      return -1;
    }
  memcpy (subs[sub_num]->commandOutData, cmd_msg, cmd_msg->size);
  subs[sub_num]->modification_number = 0;
  if(subs[sub_num]->statusInData->echo_serial_number< 1)
    {
      subs[sub_num]->statusInData->echo_serial_number=1;
    }
  subs[sub_num]->commandOutData->serial_number =
    subs[sub_num]->statusInData->echo_serial_number + 1;
  return (0);
}

int
NML_MODULE::modifyCommand (RCS_CMD_MSG * cmd_msg, int sub_num)
{
  if (sub_num >= numSubordinates || sub_num < 0)
    {
      return -1;
    }
  if (cmd_msg == NULL)
    {
      return -1;
    }
  if (NULL == subs[sub_num])
    {
      return -1;
    }
  if (NULL == subs[sub_num]->commandOutData)
    {
      return -1;
    }
  cmd_msg->serial_number = subs[sub_num]->commandOutData->serial_number;
  memcpy (subs[sub_num]->commandOutData, cmd_msg, cmd_msg->size);
  subs[sub_num]->modification_number++;
  return (0);
}

void
NML_MODULE::loadDclock (double expiration)
{
  Dclock_expiration = expiration;
  Dclock_start_time = etime ();
}

int
NML_MODULE::checkDclock ()
{
  return (fabs (etime () - Dclock_start_time) < Dclock_expiration);
}



