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


#include "nml_test_format.hh"
#include "msg_to_encoded_data.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

static int
check_test_message(TEST_MESSAGE *tst_msg, int expected_last_var)
{
  int bads=0;
  if(tst_msg->lastvar != expected_last_var)
    {
      fprintf(stderr,"expected_last_var(%d) != tst_msg->last_var(%ld)\n",
	      expected_last_var, tst_msg->lastvar);
      
      exit(255);
    }


  printf("tst_msg->first_count= %ld\t:\ttst_msg->last_count=%ld\n", 
	 tst_msg->first_count, tst_msg->last_count);
  if(tst_msg->first_count == tst_msg->last_count)
    {
      printf("GOOD first_count matches last_count\n");
    }
  else
    {
      printf("BAD: first_count does not match last_count\n");
      bads++;
    }

  // This should have been sent and be aa
  printf("tst_msg->enumtestvar=%d\n",((int) tst_msg->enumtestvar));
  if(aa == tst_msg->enumtestvar)
    {
      printf("GOOD (aa == tst_msg->enumtestvar)\n");
    }
  else
    {
      printf("BAD (aa != tst_msg->enumtestvar)\n");
      bads++;
    }


  // This should have been sent and be bb
  printf("tst_msg->enum_array[4]=%d\n",((int) tst_msg->enum_array[4]));
  if(bb == tst_msg->enum_array[4])
    {
      printf("GOOD (bb == tst_msg->enum_array[4])\n");
    }
  else
    {
      printf("BAD (bb != tst_msg->enum_array[4])\n");
      bads++;
    }


  // This should have been sent and be 3
  printf("tst_msg->enumtest_dla_length=%d\n",tst_msg->enumtest_dla_length);
  if(3 == tst_msg->enumtest_dla_length)
    {
      printf("GOOD (3 == tst_msg->enumtest_dla_length)\n");
    }
  else
    {
      printf("BAD (3 != tst_msg->enumtest_dla_length)\n");
      bads++;
    }

  // Elements 0,1,and 2 of the dynamic length array should have been sent, elements 3,4,5, and 6 should not.
  printf("tst_msg->enumtest_dla[2]=%d\n",tst_msg->enumtest_dla[2]);
  if(aa == tst_msg->enumtest_dla[2])
    {
      printf("GOOD (aa == tst_msg->enumtest_dla[2])\n");
    }
  else
    {
      printf("BAD (aa != tst_msg->enumtest_dla[2])\n");
      bads++;
    }
  
  // The write sets this to bb but it should not have been sent because of the low value of  enumtest_dla_length
  printf("tst_msg->enumtest_dla[3]=%d\n",tst_msg->enumtest_dla[3]);
  if(bb != tst_msg->enumtest_dla[3])
    {
      printf("GOOD (bb != tst_msg->enumtest_dla[3]) Unnecessary data was not sent.\n");
    }
  else
    {
      printf("BAD (bb == tst_msg->enumtest_dla[3]) Unnecessary data was sent.\n");
      bads++;
    }

 //this should have been sent and should be 3.33
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

  // This should have been sent and should be 01
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

  // This should have been sent and should be 01
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
      
  // This string should NOT have been sent.
  // avoid a possible pointer fault since really the value of cda[7] is undefined.
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
  
  // this should have been sent
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
  
  // this should have been sent and be x
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
  
  // this should NOT have been sent.
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
main(int argc, char **argv)
{
  char bads=0;

  if(argc != 5)
    {
      fprintf(stderr,"%s usage: buf proc cfg lastvar\n",
	      argv[0]);
      exit(254);
    }

  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  
  struct stat stat_struct;
  stat("tst_msg.packed",&stat_struct);
  FILE *f = fopen("tst_msg.packed","r");
  if(!f)
    {
      fprintf(stderr,"can't open tst_msg.packed -- %s\n",strerror(errno));
      exit(1);
    }
  void *data = (void *) malloc(stat_struct.st_size);
  fread(data,1,stat_struct.st_size,f);
  CMS *cms = new CMS(sizeof(TEST_MESSAGE));
  TEST_MESSAGE *tst_msg = 
    (TEST_MESSAGE *)
    encoded_data_to_msg(cms,
			data,(long) stat_struct.st_size,
			nml_test_format,
			CMS_PACKED_ENCODING);
  if(0 == tst_msg)
    {
      fprintf(stderr,"encoded_data_to_msg returned NULL\n");
      exit(255);
    }
  int expected_last_var=strtol(argv[4],0,0); 
 
  printf("tst_msg->command_type=%ld\n",tst_msg->command_type);
  printf("tst_msg->echo_serial_number=%d\n",tst_msg->echo_serial_number);
  printf("tst_msg->state=%d\n",tst_msg->state);
  printf("tst_msg->line=%d\n",tst_msg->line);
  printf("tst_msg->source_line=%d\n",tst_msg->source_line);
  printf("tst_msg->source_file=%s\n",tst_msg->source_file);
  printf("tst_msg->tt.count=%d\n",tst_msg->tt.count);
  printf("tst_msg->tt.elapsed=%f\n",tst_msg->tt.elapsed);
  printf("tst_msg->tt.min=%f\n",tst_msg->tt.min);
  printf("tst_msg->tt.max=%f\n",tst_msg->tt.max);
  printf("tst_msg->tt.avg=%f\n",tst_msg->tt.avg);
  printf("tst_msg->message_length=%d\n",tst_msg->message_length);
  printf("tst_msg->message=%s\n",tst_msg->message);
  
  bads = check_test_message(tst_msg,expected_last_var);

  // This should have been sent its value depends on a command line arg.
  printf("tst_msg.lastvar=%ld\n",tst_msg->lastvar);
  exit(bads);
}
