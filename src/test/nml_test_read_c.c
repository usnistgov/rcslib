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

static char sigint_occured=0;

#ifndef __unused_param__
#ifdef __GNUC__
#define __unused_param__ __attribute__((unused))
#else
#define __unused_param__ 
#endif
#endif

static void my_sigint_handler( __unused_param__ int sig)
{
  sigint_occured=1;
}

static int
check_test_message(nml_TEST_MESSAGE_c_t *tst_msg, int expected_last_var, nml_c_t nml)
{
  int bads=0;
  if(tst_msg->lastvar != expected_last_var)
    {
      fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
	      expected_last_var, tst_msg->lastvar);
      nml_free(nml);
      exit(255);
    }
  /* this should have been sent and should be 3.33 */
  printf("tst_msg->three_d_array[1][1][1]=%f\n",tst_msg->three_d_array[1][1][1]);
  if((tst_msg->three_d_array[1][1][1] - 3.33) > -1e-15 &&
     (tst_msg->three_d_array[1][1][1] - 3.33) < 1e-15)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD should have been 3.33\n");
      bads++;
    }

  /* This should have been sent and should be 01 */
  printf("tst_msg.cda_length=%d\n", tst_msg->cda_length);
  if(tst_msg->cda_length == 3)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD should have been 3.\n");
      bads++;
    }

  /* This should have been sent and should be 01 */
  printf("tst_msg->cda=%s\n",tst_msg->cda);
  if(!strcmp(tst_msg->cda,"01"))
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD should have been 01.\n");
      bads++;
    }
      
  /*
   * This string should NOT have been sent.
   * avoid a possible pointer fault since really the value of cda[7] is undefined.
   */
  tst_msg->cda[7] = 0;
  printf("tst_msg->cda+3=%s\n",tst_msg->cda+3);
  if(strcmp(tst_msg->cda+3,"2345"))
    {
      printf("GOOD unnecessary data was NOT transmitted.\n");
    }
  else
    {
      printf("BAD unnecessary data was transmitted.\n");
      bads++;
    }
  
  /* this should have been sent */
  printf("tst_msg->sda_length=%d\n",
	 tst_msg->sda_length);
  if(tst_msg->sda_length==1)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD\n");
      bads++;
    }
  
  /* this should have been sent and be x */
  printf("tst_msg->sda[0].c=%c\n",tst_msg->sda[0].c);
  if(tst_msg->sda[0].c == 'x')
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD\n");
      bads++;
    }
  
  /* this should NOT have been sent. */
  printf("tst_msg->sda[1].c=%c\n", tst_msg->sda[1].c);
  if(tst_msg->sda[1].c != 'y')
    {
      printf("GOOD unneccessary data was NOT sent.\n");
    }
  else
    {
      printf("BAD unneccessary data was sent.\n");
      bads++;
    }

  if(tst_msg->do_int_size_test)
    {
      if(tst_msg->smax == tst_msg->s_array[0])
	{
	  printf("GOOD: tst_msg->smax == tst_msg->s_array[0] == %d\n",
		 tst_msg->smax);
	}
      else
	{
	  printf("BAD tst_msg->smax{%d} != tst_msg->s_array[0]{%d}\n",
		 tst_msg->smax, tst_msg->s_array[0]);
	  bads++;
	}

      if(tst_msg->smax == tst_msg->i_smax)
	{
	  printf("GOOD: tst_msg->smax == tst_msg->i_smax == %d\n",
		 tst_msg->i_smax);
	}
      else
	{
	  printf("BAD tst_msg->smax{%d} != tst_msg->i_smax{%d}\n",
		 tst_msg->smax, tst_msg->i_smax);
	  bads++;
	}

      if(tst_msg->smin == tst_msg->s_array[1])
	{
	  printf("GOOD: tst_msg->smin == tst_msg->s_array[1] == %d\n",
		 tst_msg->smax);
	}
      else
	{
	  printf("BAD tst_msg->smin{%d} != tst_msg->s_array[1]{%d}\n",
		 tst_msg->smin, tst_msg->s_array[1]);
	  bads++;
	}

      if(tst_msg->smin == tst_msg->i_smin)
	{
	  printf("GOOD: tst_msg->smin == tst_msg->i_smin == %d\n",
		 tst_msg->i_smin);
	}
      else
	{
	  printf("BAD tst_msg->smin{%d} != tst_msg->i_smin{%d}\n",
		 tst_msg->smin, tst_msg->i_smin);
	  bads++;
	}


      if(tst_msg->imax == tst_msg->i_array[0])
	{
	  printf("GOOD: tst_msg->imax == tst_msg->i_array[0] == %d\n",
		 tst_msg->imax);
	}
      else
	{
	  printf("BAD tst_msg->imax{%d} != tst_msg->i_array[0]{%d}\n",
		 tst_msg->imax, tst_msg->i_array[0]);
	  bads++;
	}

      if(tst_msg->imax == tst_msg->l_imax)
	{
	  printf("GOOD: tst_msg->imax == tst_msg->l_imax == %ld\n",
		 tst_msg->l_imax);
	}
      else
	{
	  printf("BAD tst_msg->imax{%d} != tst_msg->l_imax{%ld}\n",
		 tst_msg->imax, tst_msg->l_imax);
	  bads++;
	}

      if(tst_msg->imin == tst_msg->i_array[1])
	{
	  printf("GOOD: tst_msg->imin == tst_msg->i_array[1] == %d\n",
		 tst_msg->imax);
	}
      else
	{
	  printf("BAD tst_msg->imin{%d} != tst_msg->i_array[1]{%d}\n",
		 tst_msg->imin, tst_msg->i_array[1]);
	  bads++;
	}

      if(tst_msg->imin == tst_msg->l_imin)
	{
	  printf("GOOD: tst_msg->imin == tst_msg->l_imin == %ld\n",
		 tst_msg->l_imin);
	}
      else
	{
	  printf("BAD tst_msg->imin{%d} != tst_msg->l_imin{%ld}\n",
		 tst_msg->imin, tst_msg->l_imin);
	  bads++;
	}


      if(tst_msg->lmax == tst_msg->l_array[0])
	{
	  printf("GOOD: tst_msg->lmax == tst_msg->l_array[0] == %ld\n",
		 tst_msg->lmax);
	}
      else
	{
	  printf("BAD tst_msg->lmax{%ld} != tst_msg->l_array[0]{%ld}\n",
		 tst_msg->lmax, tst_msg->l_array[0]);
	  bads++;
	}

      if(tst_msg->lmax == tst_msg->d_lmax)
	{
	  printf("GOOD: tst_msg->lmax == tst_msg->d_lmax == %f\n",
		 tst_msg->d_lmax);
	}
      else
	{
	  printf("BAD tst_msg->lmax{%ld} != tst_msg->d_lmax{%f}\n",
		 tst_msg->lmax, tst_msg->d_lmax);
	  bads++;
	}

      if(tst_msg->lmin == tst_msg->l_array[1])
	{
	  printf("GOOD: tst_msg->lmin == tst_msg->l_array[1] == %ld\n",
		 tst_msg->lmax);
	}
      else
	{
	  printf("BAD tst_msg->lmin{%ld} != tst_msg->l_array[1]{%ld}\n",
		 tst_msg->lmin, tst_msg->l_array[1]);
	  bads++;
	}

      if(tst_msg->lmin == tst_msg->d_lmin)
	{
	  printf("GOOD: tst_msg->lmin == tst_msg->d_lmin == %f\n",
		 tst_msg->d_lmin);
	}
      else
	{
	  printf("BAD tst_msg->lmin{%ld} != tst_msg->d_lmin{%f}\n",
		 tst_msg->lmin, tst_msg->d_lmin);
	  bads++;
	}

      if(tst_msg->usmax == tst_msg->us_array[0])
	{
	  printf("GOOD: tst_msg->usmax == tst_msg->us_array[0] == %u\n",
		 tst_msg->usmax);
	}
      else
	{
	  printf("BAD tst_msg->usmax{%u} != tst_msg->us_array[0]{%u}\n",
		 tst_msg->usmax, tst_msg->us_array[0]);
	  bads++;
	}

      if(tst_msg->usmax == tst_msg->ui_usmax)
	{
	  printf("GOOD: tst_msg->usmax == tst_msg->ui_usmax == %u\n",
		 tst_msg->ui_usmax);
	}
      else
	{
	  printf("BAD tst_msg->usmax{%u} != tst_msg->ui_usmax{%u}\n",
		 tst_msg->usmax, tst_msg->ui_usmax);
	  bads++;
	}


      if(tst_msg->uimax == tst_msg->ui_array[0])
	{
	  printf("GOOD: tst_msg->uimax == tst_msg->ui_array[0] == %u\n",
		 tst_msg->uimax);
	}
      else
	{
	  printf("BAD tst_msg->uimax{%u} != tst_msg->ui_array[0]{%u}\n",
		 tst_msg->uimax, tst_msg->ui_array[0]);
	  bads++;
	}

      if(tst_msg->uimax == tst_msg->ul_uimax)
	{
	  printf("GOOD: tst_msg->uimax == tst_msg->ul_uimax == %lu\n",
		 tst_msg->ul_uimax);
	}
      else
	{
	  printf("BAD tst_msg->uimax{%u} != tst_msg->ul_uimax{%lu}\n",
		 tst_msg->uimax, tst_msg->ul_uimax);
	  bads++;
	}

      if(tst_msg->ulmax == tst_msg->ul_array[0])
	{
	  printf("GOOD: tst_msg->ulmax == tst_msg->ul_array[0] == %lu\n",
		 tst_msg->ulmax);
	}
      else
	{
	  printf("BAD tst_msg->ulmax{%lu} != tst_msg->ul_array[0]{%lu}\n",
		 tst_msg->ulmax, tst_msg->ul_array[0]);
	  bads++;
	}

      if(tst_msg->ulmax == tst_msg->d_ulmax)
	{
	  printf("GOOD: tst_msg->ulmax == tst_msg->d_ulmax == %f\n",
		 tst_msg->d_ulmax);
	}
      else
	{
	  printf("BAD tst_msg->ulmax{%lu} != tst_msg->d_ulmax{%f}\n",
		 tst_msg->ulmax, tst_msg->d_ulmax);
	  bads++;
	}
    }
  
  if(tst_msg->true_bool)
    {
      printf("GOOD tst_msg->true_bool is true\n");
    }
  else
    {
      printf("BAD tst_msg->true_bool is false\n");
      bads++;
    }

  if(!tst_msg->false_bool)
    {
      printf("GOOD tst_msg->false_bool is false\n");
    }
  else
    {
      printf("BAD tst_msg->false_bool is true\n");
      bads++;
    }
  
  if(tst_msg->sminusone == -1)
    {
      printf("GOOD  tst_msg->sminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msg->sminusone=%d\n",tst_msg->sminusone);
      bads++;
    }

  if(tst_msg->iminusone == -1)
    {
      printf("GOOD  tst_msg->iminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msg->iminusone=%d\n",tst_msg->iminusone);
      bads++;
    }

  if(tst_msg->lminusone == -1)
    {
      printf("GOOD  tst_msg->lminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msg->lminusone=%ld\n",tst_msg->lminusone);
      bads++;
    }

  if(tst_msg->fminusone == -1)
    {
      printf("GOOD  tst_msg->fminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msg->fminusone=%f\n",tst_msg->fminusone);
      bads++;
    }

  if(tst_msg->dminusone == -1)
    {
      printf("GOOD  tst_msg->dminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msg->dminusone=%f\n",tst_msg->dminusone);
      bads++;
    }

  printf("bads=%d\n",bads);
  return bads;
}

int
main(int argc, const char **argv)
{
  int repeat_count;
  double repeat_delay;
  nml_TEST_MESSAGE_c_t *tst_msg = 0;
  nml_SIMPLER_MSG_c_t *smsg = 0;
  nml_TEST_MESSAGE_c_t *tst_msg2 = 0;
  nml_c_t nml=0;
  int t;
  int expected_last_var=0;
  int i=0;
  void (*old_handler)(int)=0;
  int bads = 0;

  repeat_count=0;
  repeat_delay=1.0;

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
      exit(125);
    }  
  t = nml_read(nml);
  expected_last_var = strtol(argv[4],0,0);
  switch(t)
    {
    case TEST_MESSAGE_TYPE:
      {
	tst_msg = (nml_TEST_MESSAGE_c_t *) nml_get_address(nml);
	if(0 == tst_msg)
	  {
	    fprintf(stderr,"nml_get_address() returned NULL\n");
	    nml_free(nml);
	    exit(123);
	  }
	if(tst_msg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
		    expected_last_var, tst_msg->lastvar);
	    nml_free(nml);
	    exit(122);
	  }
	bads = check_test_message(tst_msg, expected_last_var,nml);
	if(bads)
	  {
	    nml_free(nml);
	    exit(bads);
	  }
      }
      break;

    case SIMPLER_MSG_TYPE:
      {
	smsg = (nml_SIMPLER_MSG_c_t *) nml_get_address(nml);
	if(0 == smsg)
	  {
	    fprintf(stderr,"nml->get_address() returned NULL\n");
	    nml_free(nml);
	    exit(123);
	  }
	if(smsg->lastvar != expected_last_var)
	  {
	    fprintf(stderr,"expected_last_var(%d) != smsg->last_var(%ld)\n",
		    expected_last_var, smsg->lastvar);
	    nml_free(nml);
	    exit(122);
	  }
      }
      break;

    default:
      fprintf(stderr,"nml->read() returned %d when %d was expected.\n", t,TEST_MESSAGE_TYPE);
      nml_free(nml);
      exit(124);
    }

  if(repeat_count != 0)
    {
      old_handler = signal(SIGINT,my_sigint_handler);
    }

  for(i=0;
      ((i < repeat_count || repeat_count < 0) && !sigint_occured) ; i++)
    {      
      esleep(repeat_delay);
      t = nml_read(nml);
      rcs_print("nml->read() returned %d\n",t);
      switch(t)
	{
	case TEST_MESSAGE_TYPE:
	  {
	    tst_msg2 = (nml_TEST_MESSAGE_c_t *) nml_get_address(nml);
	    rcs_print("tst_msg2->i=%d\n",tst_msg2->i);
	    if(tst_msg2->lastvar != expected_last_var)
	      {
		fprintf(stderr,"expected_last_var(%d) != tst_msg2->last_var(%ld)\n",
			expected_last_var, tst_msg2->lastvar);
		signal(SIGINT,old_handler);
		nml_free(nml);
		exit(122);
	      }
	  }
	  bads = check_test_message(tst_msg2,expected_last_var,nml);
	  if(bads)
	    {
	      nml_free(nml);
	      exit(bads);
	    }
	  break;
	  
	case 0:
	  break;
	  
	case -1:
	default:
	  fprintf(stderr,"Bad return value from read.\n");
	  signal(SIGINT,old_handler);
	  nml_free(nml);
	  exit(122);
	}
    }
  if(repeat_count != 0)
    {
      signal(SIGINT,old_handler);
    }
  nml_free(nml);
  exit(0);
}
