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

#include <stdio.h>
#include <signal.h>
#include <stdlib.h> 		// exit

#include "echo_types.hh"

int echo_clnt_quit = 0;
NML_QR_CLIENT *echoQrClient=0;

void sigint_handler(int)
{
  echo_clnt_quit = 1;
  if(echoQrClient)
    {
      echoQrClient->interrupt_operation();
    }
}
  

int main(int argc, char **argv)
{
  bool verbose=false;
  double t0,t1,t01diff, t01min, t01max,t01total;
  double t2,t12diff,t12min,t12max,t12total;
  double t02diff,t02min,t02max,t02total;
  int count;
  double dtimeout = 2.0;

  if(argc < 6)
    {
      fprintf(stderr,"usage : bufname procname configfile logfile timeout\n");
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
  dtimeout=  strtod(argv[5],0);

  print_rcs_version();
  set_abort_on_rcs_error(true);

  echoQrClient = new NML_QR_CLIENT(ECHO_format, argv[1],argv[2],argv[3]);
  if(0 == echoQrClient)
    {
      fprintf(stderr,"echoQrClient is NULL\n");
      exit(126);
    }
  if(!echoQrClient->valid())
    {
      fprintf(stderr,"echoQrClient is invalid\n");
      delete echoQrClient;
      echoQrClient=0;
      exit(125);
    }
  nml_start(); 

  t01max = -99e99;
  t01min = 99e99;
  t12max = -99e99;
  t12min = 99e99;
  t02max = -99e99;
  t02min = 99e99;
  t01total=0;
  t12total=0;
  t02total=0;
  count=0;
  signal(SIGINT,sigint_handler);
  char hbuf[80];
  int countdownmax =500;
  int countdown = countdownmax;

  printf("echoclnt started . . .\n");
  printf("Press control C or enter \"quit\" to quit.\n");
  printf("Enter \"kill_server\" to tell the server to quit.\n");
  while(!echo_clnt_quit)
    {
      if(verbose)
	{
	  fflush(stdout);
	  fflush(stderr);
	}
      ECHO_QUERY eqMsg;
      ECHO_REPLY *erMsg;
      if(feof(stdin))
	{
	  echo_clnt_quit=1;
	  break;
	}
      memset(eqMsg.line,0,sizeof(eqMsg.line));
      memset(hbuf,0,sizeof(hbuf));
      //fgets(eqMsg.line,79,stdin);
      int hmax = (countdown*39)/countdownmax;
      memset(hbuf,'#',hmax);
      sprintf(eqMsg.line,"%3.3d \t%s \t%3.3d",countdown,hbuf,countdown);
      countdown--;
      if(countdown <= 0)
	{
	  echo_clnt_quit=true;
	  break;
	}
      eqMsg.line_length = strlen(eqMsg.line)+1;
      if(!strcmp(eqMsg.line,"quit"))
	{
	  echo_clnt_quit=true;
	  break;
	}
      count++;
      t0=etime();
      if(verbose)
	{
	  printf("%s %s %s %s %s \t -- \tSending request to echo %s\n",
		 argv[0],argv[1],argv[2],argv[3],argv[4],eqMsg.line);
	}
      int query_ret = echoQrClient->sendQuery(&eqMsg);
      if(query_ret != 0)
	{
	  fprintf(stderr,"%s %s %s %s %s \t --sendQuery returned %d\n",
		  argv[0],argv[1],argv[2],argv[3],argv[4],query_ret);
	  delete echoQrClient;
	  echoQrClient=0;
	  exit(121);
	}
      t1=etime();
      t01diff = t1 - t0;
      t01total += t01diff;
      if(t01diff > t01max)
	{
	  t01max = t01diff;
	}
      if(t01diff < t01min)
	{
	  t01min = t01diff;
	}
      if(!strcmp(eqMsg.line,"kill_server"))
	{
	  echo_clnt_quit=true;
	}
      int t;
      t1=etime();
      switch((t = echoQrClient->waitForReply(dtimeout)))
	{
	case ECHO_REPLY_TYPE:
	  t2 =etime();
	  t12diff = t2 - t1;
	  t12total += t12diff;
	  if(t12diff > t12max)
	    {
	      t12max = t12diff;
	    }
	  if(t12diff < t12min)
	    {
	      t12min = t12diff;
	    }
	  t02diff = t01diff + t12diff;
	  t02total += t02diff;
	  if(t02diff > t02max)
	    {
	      t02max = t02diff;
	    }
	  if(t02diff < t02min)
	    {
	      t02min = t02diff;
	    }
	  erMsg = (ECHO_REPLY *) echoQrClient->getReplyAddress();
	  if(verbose)
	    {
	      printf("%s %s %s %s %s \t -- \tServer sent back %s\n",
		     argv[0],argv[1],argv[2],argv[3],argv[4],erMsg->line);
	    }
	  if(strcmp(erMsg->line,eqMsg.line))
	    {
	      fprintf(stderr,"%s %s %s %s %s \t --Reply (%s:erMsg->line_length=%d,erMsg->client_num=%d,erMsg->reply_num=%d) does not match query(%s:eqMsg->line_length=%d)\n",
		      argv[0],argv[1],argv[2],argv[3],argv[4],
		      erMsg->line,erMsg->line_length,erMsg->client_num,
		      erMsg->reply_num,
		      eqMsg.line,eqMsg.line_length);
	      delete echoQrClient;
	      echoQrClient=0;
	      exit(124);
	    }
	  break;
	  
	case 0:
	  // no reply/timeout.
	  printf("%s %s %s %s %s \t --waitForReply returned 0, timeout %f was specified\n",
		 argv[0],argv[1],argv[2],argv[3],argv[4],dtimeout);
	  delete echoQrClient;
	  echoQrClient=0;
	  exit(123);
	  echo_clnt_quit = 1;
	  break;

	case -1:
	default:
	  // error 
	  fprintf(stderr,"%s %s %s %s %s \t --waitForReply returned %d\n",
		 argv[0],argv[1],argv[2],argv[3],argv[4],
		 t);
	  delete echoQrClient;
	  echoQrClient=0;
	  exit(122);
	  echo_clnt_quit = 1;
	  break;
	  
	}
    }
  delete echoQrClient;
  echoQrClient=0;
  nml_cleanup();
  printf("%s %s %s %s %s \t --Performance Results:\n",
	 argv[0],argv[1],argv[2],argv[3],argv[4]);
  printf("count=%d, t01avg=%4.4f, t01min=%4.4f, t01max=%4.4f, t12avg=%4.4f, t12min=%4.4f, t12max=%4.4f, t02avg=%4.4f, t02min=%4.4f, t02max=%4.4f\n",
	count,t01total/count,t01min,t01max,t12total/count,t12min,t12max,t02total/count,t02min,t02max);
  printf("%s %s %s %s %s \t --OK\n",
	 argv[0],argv[1],argv[2],argv[3],argv[4]);
  exit(0);
}
