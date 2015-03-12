/****************************************************************************
* File: timesvr.cc                                                             
* 
* This file provides the code for an NML server that can be launched and 
* killed by CGI script(s). It provides remote access to the time and 
* control buffers used
* by the Java/NML inteface applet example.
*****************************************************************************/

#include "rcs.hh"		// class NML, spawn_nml_servers(), kill_nml_servers(), esleep()

#include "timetype.hh"		// timetype_format()
#include <stdlib.h>		// system(), exit()
#include <signal.h>		// signal(), SIGINT
#include <unistd.h>		// getpid()
#include <stdio.h>		// fopen(), fputs(), fclose()
#include <errno.h>		// errno, 
#include <string.h>		// strerror()

// Private Global Variables
static NML *nmltime = NULL;
static NML *nmlcontrol = NULL;

// Private Functions
static void kill_timesvr(int);
static void save_pid();

static double time_limit;

/***************** MAIN *******************************************/
int main(int argc, char **argv)
{ 
  // Store the pid of this process in a file, to be used by master CGI script.
  save_pid();
  
  // Install signal handler so master CGI script can kill this process cleanly.
  signal(SIGINT, kill_timesvr);

  // Connect to NML channels to provide access to.
  nmltime = new NML(timetype_format,"timebuf","timesvr","socapplet.nml");
  nmlcontrol = new NML(timetype_format,"controlbuf","timesvr","socapplet.nml");

  // Verify local connections.
  if(NULL == nmltime || NULL == nmlcontrol)
  {
    kill_timesvr(2);
  }
  if(!nmltime->valid() || !nmltime->valid())
  {
     kill_timesvr(2);
  }
  
  if(argc > 1)
  {
    time_limit = strtod(argv[1],NULL);
  }
  else
  {
    time_limit = 1000;
  }
  
  if(time_limit > 0)
  {
    // Spawn the server(s) neccessary to provide access to the channels that were opened.
    rcs_print("Spawning NML servers . . . (time_limit = %f)\n",time_limit);
    spawn_nml_servers();

    // Wait until it is time to die.
    esleep(time_limit);

    // Kill the servers spawned and cleanup.
    kill_timesvr(2);
  }
  else
  {
    rcs_print("Running NML servers . . .(no time_limit) \n");
    run_nml_servers();
  }
}

// Control-C or SIGINT Signal Handler
void kill_timesvr(int id)
{
  rcs_print("Killing NML servers . . .\n");
  kill_nml_servers();
  system("\\rm -f timesvr.run");
  if(NULL != nmltime)
  {
    delete nmltime;
    nmltime = NULL;
  }
  if(NULL != nmlcontrol)
  {
    delete nmlcontrol;
    nmlcontrol = NULL;
  }
  exit(0);
}

// Function used to save the process id to a file, so the master CGI script can kill this
// process if necessary.
void save_pid()
{
  int mypid = getpid();
  FILE *pidfile = fopen("timesvr.pid","w");
  char buf[80];
  if(NULL == pidfile)
  {
    rcs_print_error("Can't open timesvr.pid. errno = %d -- %s.\n",
		    errno, strerror(errno));
    kill_timesvr(0);
  }
  sprintf(buf,"%d",mypid);
  fputs(buf,pidfile);
  fclose(pidfile);
  system("chmod -f a+rw timesvr.pid");
  pidfile = NULL;
}

