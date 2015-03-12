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


#include "nmlcms_c.h"
#include "nml_test_format_c_n.h"
#include "_timer.h"
#include "rcs_prnt.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <limits.h>


static char sigint_occured=0;
static void (*old_sigint_handler)(int)=0;

static void my_sigint_handler(int sig)
{
  sigint_occured=1;
  if(old_sigint_handler && old_sigint_handler != SIG_IGN &&
     old_sigint_handler != SIG_DFL)
    {
      (*old_sigint_handler)(sig);
    }
  sigint_occured=1;
}

int 
main(int argc, char **argv)
{

  nml_c_t nml = 0;
  nml_TEST_MESSAGE_c_t tst_msg;
  nml_SIMPLER_MSG_c_t smsg;
  char do_long_tests=1;
  int t=-1;
  double repeat_delay=-1.0;
  int repeat_count=0;
  char *simpler_msg_env = 0;
  int i=0;
  
  simpler_msg_env = getenv("SIMPLER_MSG");

  if(getenv("NO_LONG_TESTS"))
    {
      do_long_tests=0;
    }
  
  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile  lastvar\n");
      exit(127);
    }
  if(argc >= 6)
    {
      repeat_count=strtol(argv[5],0,0);
    }
  if(argc >= 7)
    {
      repeat_delay =strtod(argv[6],0);
    }
  nml = nml_new(nml_test_c_format,argv[1],argv[2],argv[3]);
  if(!nml_valid(nml))
    {
      fprintf(stderr,"nml->valid() check failed.\n");
      nml_free(nml);
      exit(125);
    }

  memset(&tst_msg,0,sizeof(tst_msg));

  /* these will be sent */
  tst_msg.enumtestvar = aa;
  tst_msg.enum_array[0] = aa;
  tst_msg.enum_array[4] = bb;
  tst_msg.enumtest_dla_length = 3;
  tst_msg.enumtest_dla[0] = aa;
  tst_msg.enumtest_dla[1] = bb;
  tst_msg.enumtest_dla[2] = aa;

  /* these will NOT be sent */
  tst_msg.enumtest_dla[3] = bb;
  tst_msg.enumtest_dla[4] = aa;
  tst_msg.enumtest_dla[5] = bb;
  tst_msg.enumtest_dla[6] = aa;

  /* this will be sent. */
  tst_msg.f  = -(1.0E-37);
  tst_msg.three_d_array[1][1][1] = 3.33;
  strcpy(tst_msg.cda,"01");
  tst_msg.cda_length = 3;

  /*  This string  will NOT be sent. */
  strcpy(tst_msg.cda+3,"2345");
  
  /*  this will be sent. */
  tst_msg.sda_length=1;
  tst_msg.sda[0].c= 'x';
  
  /*  this will NOT be sent; */
  tst_msg.sda[1].c = 'y';

  /*  Test some integer length features. */
  tst_msg.do_int_size_test=1;
  tst_msg.s_array[0] = tst_msg.smax = SHRT_MAX;
  tst_msg.s_array[1] = tst_msg.smin = SHRT_MIN;
  tst_msg.s_array[2] = 0;
  tst_msg.us_array[0] = tst_msg.usmax = USHRT_MAX;
  tst_msg.us_array[1] = 0;
  tst_msg.i_smin = SHRT_MIN;
  tst_msg.i_smax = SHRT_MAX;
  tst_msg.i_array[0] = tst_msg.imax = INT_MAX;
  tst_msg.i_array[1] = tst_msg.imin = INT_MIN;
  tst_msg.i_array[2] = 0;
  tst_msg.ui_usmax= USHRT_MAX;
  tst_msg.ui_array[0] = tst_msg.uimax = UINT_MAX;
  tst_msg.ui_array[1] = 0;
  tst_msg.l_imin = INT_MIN;
  tst_msg.l_imax = INT_MAX;
  tst_msg.ul_uimax = UINT_MAX;

  tst_msg.dda_length=0;

    /* 
   * FIXME: long long are more trouble to support than they are worth.
   tst_msg.ll = -1LL*0x123456789LL;
   tst_msg.ull = 0x123456789ULL;
   tst_msg.lla[0] = -1LL*0x123456789LL;
   tst_msg.ulla[0] = 0x123456789ULL;
    */

  rcs_print("do_long_tests=%d\n",(int)do_long_tests);
  if(do_long_tests)
    {
      tst_msg.l_array[0] = tst_msg.lmax = LONG_MAX;
      tst_msg.l_array[1] = tst_msg.lmin = LONG_MIN;
      tst_msg.l_array[2] = 0;
      tst_msg.ul_array[0] = tst_msg.ulmax = ULONG_MAX;
      tst_msg.ul_array[1] = 0;
      tst_msg.d_lmin = LONG_MIN;
      tst_msg.d_lmax = LONG_MAX;
      tst_msg.d_ulmax = ULONG_MAX;
    }
  tst_msg.false_bool =0;
  tst_msg.true_bool=1;

  tst_msg.sminusone = -1;
  tst_msg.iminusone = -1;
  tst_msg.lminusone = -1;
  tst_msg.fminusone = -1.0;
  tst_msg.dminusone = -1.0;
  
  /*  This will be sent. */
  tst_msg.lastvar=strtol(argv[4],0,0);
  if(simpler_msg_env)
    {
      if((t = nml_write(nml,&smsg,SIMPLER_MSG_TYPE,sizeof(smsg))))
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  nml_free(nml);
	  nml=0;
	  exit(124);
	}
    }
  else
    {
      if((t = nml_write(nml,&tst_msg,TEST_MESSAGE_TYPE,sizeof(tst_msg)))) 
	{
	  fprintf(stderr,"nml->write() returned %d\n", t);
	  nml_free(nml);
	  exit(124);
	}
      
      if(repeat_count != 0)
	{
	  old_sigint_handler = signal(SIGINT,my_sigint_handler);
	}
  
      for(i = 0 ;
	  ((i < repeat_count || repeat_count  < 0) && !sigint_occured) ; i++)
	{
	  tst_msg.i=i;
	  rcs_print("tst_msg.i=%d, \ttst_msg.lastvar=%ld\n",tst_msg.i,tst_msg.lastvar);
	  esleep(repeat_delay);
	  if((t = nml_write(nml,&tst_msg,TEST_MESSAGE_TYPE,sizeof(tst_msg))))
	    {
	      fprintf(stderr,"nml->write() returned %d\n", t);
	      signal(SIGINT,old_sigint_handler);
	      nml_free(nml);
	      exit(124);
	    }
	}
      if(repeat_count != 0)
	{
	  signal(SIGINT,old_sigint_handler);
	}

    }

  nml_free(nml);
  exit(0);
}

