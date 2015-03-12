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


#ifndef NML_MODULE_H
#define NML_MODULE_H

/*
   nml_mod.h

   Declarations for the NML_MODULE class  which  is the main class
   used as a template for RCS applications using NML channels and an RCS_TIMER,
   that are set up by the rcsDesign tool.
*/

#include "stat_msg.hh"		// RCS_STATUS, RCS_STATE, RCS_STAT_MSG, RCS_STAT_CHANNL

class RCS_CMD_CHANNEL;
class RCS_CMD_MSG;
class RCS_TIMER;
class NML;
class NMLmsg;

#define STATE_MATCH (set_file_and_line(__FILE__,__LINE__)),stateMatch

struct NML_SUBORDINATE_STRUCT
{
public:
  RCS_CMD_CHANNEL * commandOut;	// NML channels for commands to subordinates
  RCS_STAT_CHANNEL *statusIn;	// NML channels for status from subordinates
  RCS_CMD_MSG *commandOutData;	// ptrs to NML data to be put in channel
  RCS_STAT_MSG *statusInData;	// ptrs to NML data in channels
  int modification_number;
  char *name;
  bool disable_log_status_read_error;
};


class NML_MODULE
{
public:
  // This section taken from RCS_MODULE_C
  void controller (void);

  virtual void DECISION_PROCESS (void);

  virtual void READ_COMM_BUFFERS (void);
  virtual void PRE_PROCESS ();


  virtual void WRITE_COMM_BUFFERS (void);
  virtual void POST_PROCESS ();

  // State table functions
  int stateMatch (char *_src_file, int source_line, int state, int conds = 1);
  int stateMatch (int state, int conds = 1);
  void stateNext (int state);
  void stateNext (enum RCS_STATE state);

  void read_command_in ();
  void read_subordinates_status ();
  void write_status_out ();
  void write_commands_to_subordinates ();
  void setCmdChannel (RCS_CMD_CHANNEL *);

  void setStatChannel (RCS_STAT_CHANNEL *, RCS_STAT_MSG *);

  void setErrorLogChannel (NML *);
  int addSubordinate (RCS_CMD_CHANNEL *, RCS_STAT_CHANNEL *);
  int sendCommand (RCS_CMD_MSG *, int sub_num);
  int modifyCommand (RCS_CMD_MSG *, int sub_num);
  void setSelfCommand (RCS_CMD_MSG *);
  int force_command;

  void check_if_new_command (void);

#if 0
  RCS_EXEC_HISTORY_STRUCT exec_history;	// Exec History
  RCS_EXEC_STATUS_STRUCT exec_status;

  RCS_RUN_COMMAND_STRUCT run_command;	// Run Command
  RCS_RUN_STATUS_STRUCT run_status;
#endif

  long cycle_start;		// Data
  long cycle_stop;

  int command_time_averaged;
  int new_command_sequence;
  int new_line_num_sequence;
  int new_sup_request;

  long delta_clock;
  long command_current_time;

  int pause_status;
  int command;
  int last_line;

  int execute;
  int command_time;
  enum RCS_STATE state;
  enum RCS_STATUS status;

  int sup_req_num;
  int sup_req_num_echo;
  int command_num;
  int command_num_echo;

private:
  int matched;			/* flag set when a state is matched, to
				   prevent fall-through to another state */
  int stateBegin;		/* flag set by controller() signifying
				   that stateMatch should init line number */
  char *source_file;
  int source_line;

public:
    NML_MODULE ();
    virtual ~ NML_MODULE ();

  void zero_common_vars ();

#if 0
  NML_MODULE_INI_INFO *ini;	// pointer to an area of data from which ini file info is gathered.
#endif

  RCS_CMD_CHANNEL *commandIn;	// NML channel for command from supervisor
  RCS_STAT_CHANNEL *statusOut;	// NML channel for status to supervisor
  NML *errorLog;		// NML channel for logging errors

  RCS_CMD_MSG *commandInData;	// ptr to NML data in channel
  RCS_STAT_MSG *statusOutData;	// ptr to NML data to be put in channel

  int *commandLastNum;		// array of command nums saved before writes
  int *commandOutstanding;	// array of flags, 1 = command has been sent,
  // 0 = command has finished

  NML_SUBORDINATE_STRUCT **subs;	// pointer to array of pointers to subordinates
  RCS_STAT_MSG **statusInData;	// ptrs to NML data in channels
  RCS_CMD_MSG **commandOutData;	// ptrs to NML data in channels

  RCS_TIMER *timer;		// synch timer

  int done;			// non-zero means stop calling controller()

  int setSubordinates (int number);
  int setLogInfo (const char *src, int l);
  int logError (const char *fmt, ...);
  int logText (const char *fmt, ...);
  int requestDisplay (const char *display);
  void stop_timing (void);
  void set_file_and_line (char *file, int line);

  int commands_received;
  int commands_executed;
  int cycles;
  int cycles_executing;
  int cycles_executing_completed_commands;
  int cycles_executing_this_command;
  int last_command_completed_serial_number;
  double expected_cycle_time;
  double start_run_time;
  double last_start_run_time;
  double stop_run_time;
  double total_run_time;
  double min_run_time;
  double max_run_time;
  double start_cycle_time;
  double min_cycle_time;
  double max_cycle_time;
  double last_cycle_time;

  void check_cycle_time_start ();
  void check_cycle_time_end ();
  void print_statistics ();

  void loadDclock (double expiration);
  int checkDclock ();

protected:

  char *proc_name;
  char *temp_file;
  int temp_line;
  int numSubordinates;		// number of subordinates for this module
  double Dclock_expiration;
  double Dclock_start_time;
  int log_line;
  const char *log_src;

public:

  // 1 if realloc works and we should use, 0 if we need to avoid lame NT problem.
  int subs_allocated;
  static int use_realloc;
  int log_status_read_error_count;
  int log_status_read_error_max;
  bool disable_log_status_read_error;
  bool ignore_invalid_cmd_channel;
  bool ignore_invalid_stat_channel;
  bool ignore_invalid_errlog_channel;
  bool ignore_status_write_err;
  

private:
  NML_MODULE(const NML_MODULE &_nml_module);
  NML_MODULE &operator=(const NML_MODULE &_nml_module);
};

#define NML_MOD_LOG_ERROR setLogInfo(__FILE__,__LINE__); logError
extern int logTextToNML (NML *, const char *fmt, ...);


// NML_MODULE_H
#endif
