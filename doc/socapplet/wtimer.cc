/******************************************************************
* File: wtimer.cc 
*
* This file contains the code for a psuedo controller that accepts
* NML commands and posts the time to an NML world model buffer. 
* Unfortunately a fair amount of additional code was needed to 
* allow the controller to be started from a CGI script easily.
*
****************************************************************/

// Main RCS Library include file
#include "rcs.hh"		// class NML, etime(), esleep(), rcs_print()

// Message specific include file for this Application
#include "timetype.hh"		// timetype_format(), class NML_TIME

// Standard System include files
#include <signal.h>		// signal(), SIGINT
#include <stdlib.h>		// system(), exit()
#include <unistd.h>		// getpid()
#include <stdio.h>		// fopen(), fputs(), fclose()
#include <errno.h>		// errno
#include <string.h>		// strerror()


// Private global variables
static int quit_timer_var = 0;
static int timer_stopped = 0;
static  NML *time_channel;
static  NML *control_channel;

// Private functions
static void save_pid();
static void quit_timer_func(int);
static void exit_wtimer();
static float time_limit = 980;

/***************** MAIN *******************************************/
int main(int argc, char **argv)
{
  NML_TIME nmltime;
  double orig_start_time, start_time, stop_time;
  NMLTYPE control_type;

  if(argc > 1)
  {
    time_limit = strtod(argv[1],NULL);
  }

  // We want the master CGI script to be able to kill this process.
  // so we will save the pid in a file.
  rcs_print("Saving PID . . .\n");
  save_pid();

  // Record the start time so we can kill this process after about 20 minutes.
  orig_start_time = etime();

  // Print some debug messages.
  rcs_print("Initiallizing time_channel ...\n");

  // Connect the World Model Buffer.
  time_channel = new NML(timetype_format, "timebuf", "timer", "socapplet.nml");

  // Verify that the connection to the World Model Buffer was successful.
  if(NULL == time_channel)
  {
    // Cleanup and exit.
    exit_wtimer();
  }
  if(!time_channel->valid())
  {
    // Cleanup and exit.
    exit_wtimer();
  }

  
   // Print some debug messages.
  rcs_print("Initiallizing time_channel ...\n");

  // Connect to the command buffer.
  control_channel = new NML(timetype_format, "controlbuf", "timer", "socapplet.nml");

  // Verify that the connection to the command buffer was successful.
  if(NULL == control_channel)
  {
    // Cleanup and exit.
    exit_wtimer();
  }
  if(!control_channel->valid())
  {
    // Cleanup and exit.
    exit_wtimer();
  }

  // Install signal handler so master CGI script can kill this process cleanly.
  signal(SIGINT, quit_timer_func);

  // Reset/Initialize Variables
  quit_timer_var = 0;
  timer_stopped = 0;
  start_time = etime();

  // Print more debug messages.
  rcs_print("Timer starting (%f)... (time_limit = %f)\n",start_time,time_limit);

  // Main Loop
  while(!quit_timer_var)
  {
    // Read the command buffer.
    switch(control_type = control_channel->read())
    {
    case 0:
      break;
      
    case -1:
      quit_timer_var = 1;
      break;
      
    case NML_RESET_TIMER_TYPE:
      start_time = etime();
      stop_time = 0.0;
      nmltime.time = 0;
      if(time_channel->write(nmltime)<0)
      {
	quit_timer_var = 1;
	break;
      }
      break;

    case NML_STOP_TIMER_TYPE:
      if(!timer_stopped)
      {
	stop_time = etime() - start_time;
	timer_stopped = 1;
      }
      break;

    case NML_START_TIMER_TYPE:
      if(timer_stopped)
      {
	start_time = etime() - stop_time;
	timer_stopped = 0;
      }
      break;

    defualt:
      rcs_print_error("Invalid reset type %d\n", control_type);
      quit_timer_var = 1;
      break;
    } 
    if(!timer_stopped)
    {
      nmltime.time = etime() - start_time;
      if(time_channel->write(nmltime)<0)
      {
	break;
      }
    }
    
    // Let other processes run.
    esleep(0.5);

    // Check to see if it is time to die.
    if(etime() - orig_start_time > time_limit && time_limit > 0)
    {
      quit_timer_var = 1;
      break;
    }
  }
  
  // Cleanup and exit.
  exit_wtimer();

} // end main()


void exit_wtimer()
{
  // Print some debug message.
  rcs_print("Timer exiting.(%f)\n",etime());

  // Disconnect from the NML channels.
  if(NULL != control_channel)
  {
    delete control_channel;
    control_channel = NULL;
  }
  if(NULL != time_channel)
  {
    delete time_channel;
    time_channel = NULL;
  }
   // Delete this file to indicate to master CGI script that this process has finished.
    system("\\rm -f wtimer.run");
  exit(0);
}

// Control-C or SIGINT Signal Handler
void quit_timer_func(int i)
{
  quit_timer_var = 1;
}


// Fuction to save this process' PID in a file.
void save_pid()
{
  int mypid = getpid();
  char buf[80];
  FILE *pidfile = fopen("wtimer.pid","w");
  if(NULL == pidfile)
  {
    system("\\rm -f wtimer.run");
    rcs_print_error("Can't open wtimer.pid. errno = %d -- %s\n", errno, strerror(errno));
    exit(-1);
  }
  sprintf(buf,"%d",mypid);
  fputs(buf,pidfile);
  fclose(pidfile);
  pidfile = NULL;
  system("chmod -f a+rw wtimer.pid");
}
