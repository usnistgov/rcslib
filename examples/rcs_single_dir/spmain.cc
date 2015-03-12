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
	spmain.cc

	This file provides the C++ main function which
	creates and runs the following control modules:

		SP_MODULE

	MODIFICATIONS:
	Sun Mar 06 10:46:52 EST 2005	Created.

*/

// Include Files
#include <stdlib.h>	// exit()
#include <signal.h>	// SIGINT, signal()
#include "rcs.hh" 	// Common RCS definitions
#include "nml_mod.hh" 	// NML_MODULE definitions
#include "sp.hh"	// definition of SP_MODULE

// flag signifying main loop is to terminate
int sp_done = 0;

//signal handler for ^C
extern "C" void sp_quit(int sig);
void sp_quit(int sig)
{
	sp_done = 1;
}

// main loop, running 6 controller(s)
#ifdef VXWORKS
extern "C" int sp_run();

int sp_run()
#else
int main(int argc, char **argv)
#endif
{

	set_rcs_print_destination(RCS_PRINT_TO_STDOUT);

	RCS_TIMER *timer = new RCS_TIMER(0.01);
	SP_MODULE *sp = new SP_MODULE();

	// set the SIGINT handler
	signal(SIGINT, sp_quit);

	// enter main loop
	while(!sp_done)
	{
		sp->controller();

		timer->wait();
	}

	// Delete Modules
	delete sp;

	// Delete Timer
	delete timer;
}

