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


#include "rcs.hh"

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "echo_types.hh"

bool echo_svr_quit = false;
NML_QR_SERVER *echoQrServer=0;

void sigint_handler(int)
{
  echo_svr_quit = true;
  if(echoQrServer)
    {
      echoQrServer->interrupt_operation();
    }
}

static void 
touch_running_file(const char *fname)
{
  FILE *f = fopen(fname,"w");
  if(f)
    {
      fprintf(f,"%s\n",fname);
      fclose(f);
    }
}

int main(int argc, char **argv)
{
  bool verbose=false;
  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile logfile\n");
      exit(127);
    }

  if(getenv("NML_TEST_QR_VERBOSE"))
    {
      verbose=true;
    }

  if(strcmp(argv[4],"--") && strcmp(argv[4],"stdout")) {
    set_rcs_print_destination(RCS_PRINT_TO_FILE);
    set_rcs_print_file(argv[4]);
  }
  print_rcs_version();
  set_abort_on_rcs_error(true);

  FILE *f_log = fopen(argv[4],"w");

  signal(SIGINT,sigint_handler);
  
  echoQrServer= new NML_QR_SERVER(ECHO_format,argv[1],argv[2],argv[3]);
  nml_start();
  fprintf(f_log,"echosvr started . . .\n");
  ECHO_QUERY *eqMsg;
  ECHO_REPLY erMsg;
  int global_reply_num = 0;
  double start_time = etime();
  int query_count=0;
  touch_running_file("nml_test_qr_server.running");

  while(!echo_svr_quit)
    {
      if(verbose)
	{
	  fflush(stdout);
	  fflush(stderr);
	}
      global_reply_num++;
      int wait_for_query_ret = echoQrServer->waitForQuery(NML_NO_TIMEOUT);
      query_count++;
      if(query_count > 1) {
	double time_diff = etime()-start_time;
	double freq= query_count/time_diff;
	fprintf(f_log,"query_count=%d, time=%f, freq=%f,ret=%d\n",
		query_count,
		time_diff,
		freq,
		wait_for_query_ret);
      }
      switch(wait_for_query_ret)
	{
	case ECHO_QUERY_TYPE:
	  eqMsg = (ECHO_QUERY *) echoQrServer->getQueryAddress();
	  if(verbose)
	    {
	      fprintf(f_log,"%s %s %s %s \t---Client %d sent %s (global_reply_num=%d)\n",
		     argv[0],argv[1],argv[2],argv[3],
		     eqMsg->subdiv_for_reply, eqMsg->line,global_reply_num);
	    }
	  if(!strcmp(eqMsg->line,"kill_server"))
	    {
	      echo_svr_quit=true;
	    }
	  erMsg.line_length = 80>eqMsg->line_length? eqMsg->line_length:80;
	  strncpy(erMsg.line, eqMsg->line, erMsg.line_length);
	  erMsg.client_num = eqMsg->subdiv_for_reply;
	  erMsg.reply_num = global_reply_num;
	  echoQrServer->replyToLastQuery(&erMsg);
	  if(eqMsg->line_length < 1 || eqMsg->line_length > 80)
	    {
	      fprintf(f_log,"%s %s %s %s \t--- eqMsg->line_length = %d\n",
		     argv[0],argv[1],argv[2],argv[3],eqMsg->line_length);
	      echo_svr_quit = 1;
	    }
	  break;
	  
	  // error 
	default:
	  fprintf(f_log,"%s %s %s %s \t---echoQrServer->waitForQuery(NML_NO_TIMEOUT) returned  %d\n",
		 argv[0],argv[1],argv[2],argv[3],wait_for_query_ret);

	  fprintf(stderr,"%s %s %s %s \t---echoQrServer->waitForQuery(NML_NO_TIMEOUT) returned  %d\n",
		 argv[0],argv[1],argv[2],argv[3],wait_for_query_ret);
	  // error 
	  echo_svr_quit = 1;
	  break;
	}
    }
  delete echoQrServer;
  echoQrServer=0;
  fclose(f_log);

  nml_cleanup();
}
