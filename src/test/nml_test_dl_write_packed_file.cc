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
//#include "msg_to_encoded_data.hh"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

static TEST_MESSAGE tst_msg;

static int
check_test_message(TEST_MESSAGE *tst_msgP, int expected_last_var)
{
  int bads=0;
  if(tst_msgP->lastvar != expected_last_var)
    {
      fprintf(stderr,"expected_last_var(%d) != tst_msgP->last_var(%ld)\n",
	      expected_last_var, tst_msgP->lastvar);
      exit(255);
    }


  printf("tst_msgP->first_count= %ld\t:\ttst_msgP->last_count=%ld\n", 
	 tst_msgP->first_count, tst_msgP->last_count);
  if(tst_msgP->first_count == tst_msgP->last_count)
    {
      printf("GOOD first_count matches last_count\n");
    }
  else
    {
      printf("BAD: first_count does not match last_count\n");
      bads++;
    }

  // This should have been sent and be aa
  printf("tst_msgP->enumtestvar=%d\n",((int) tst_msgP->enumtestvar));
  if(aa == tst_msgP->enumtestvar)
    {
      printf("GOOD (aa == tst_msgP->enumtestvar)\n");
    }
  else
    {
      printf("BAD (aa != tst_msgP->enumtestvar)\n");
      bads++;
    }


  // This should have been sent and be bb
  printf("tst_msgP->enum_array[4]=%d\n",((int) tst_msgP->enum_array[4]));
  if(bb == tst_msgP->enum_array[4])
    {
      printf("GOOD (bb == tst_msgP->enum_array[4])\n");
    }
  else
    {
      printf("BAD (bb != tst_msgP->enum_array[4])\n");
      bads++;
    }


  // This should have been sent and be 3
  printf("tst_msgP->enumtest_dla_length=%d\n",tst_msgP->enumtest_dla_length);
  if(tst_msg.enumtest_dla_length == tst_msgP->enumtest_dla_length)
    {
      printf("GOOD (%d == tst_msgP->enumtest_dla_length)\n",
	     tst_msg.enumtest_dla_length);
    }
  else
    {
      printf("BAD (%d != tst_msgP->enumtest_dla_length(%d))\n",
	     tst_msg.enumtest_dla_length,tst_msgP->enumtest_dla_length );
      bads++;
    }

  // Elements 0,1,and 2 of the dynamic length array should have been sent, elements 3,4,5, and 6 should not.
  printf("tst_msgP->enumtest_dla[2]=%d\n",tst_msgP->enumtest_dla[2]);
  if(aa == tst_msgP->enumtest_dla[2])
    {
      printf("GOOD (aa == tst_msgP->enumtest_dla[2])\n");
    }
  else
    {
      printf("BAD (aa != tst_msgP->enumtest_dla[2])\n");
      bads++;
    }
  
  // The write sets this to bb but it should not have been sent because of the low value of  enumtest_dla_length
  printf("tst_msgP->enumtest_dla[3]=%d\n",tst_msgP->enumtest_dla[3]);
  if(bb != tst_msgP->enumtest_dla[3])
    {
      printf("GOOD (bb != tst_msgP->enumtest_dla[3]) Unnecessary data was not sent.\n");
    }
  else
    {
      printf("BAD (bb == tst_msgP->enumtest_dla[3]) Unnecessary data was sent.\n");
      bads++;
    }

 //this should have been sent and should be 3.33
  printf("tst_msgP->three_d_array[1][1][1]=%f\n",tst_msgP->three_d_array[1][1][1]);
  if((tst_msgP->three_d_array[1][1][1] - 3.33) > -1e-15 &&
     (tst_msgP->three_d_array[1][1][1] - 3.33) < 1e-15)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD should have been 3.33\n");
      bads++;
    }

  // This should have been sent and should be 01
  printf("tst_msg.cda_length=%d\n", tst_msgP->cda_length);
  if(tst_msgP->cda_length == tst_msg.cda_length)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD should %d have been %d.\n",
	     tst_msgP->cda_length,tst_msg.cda_length);
      bads++;
    }

  // This should have been sent and should be 01
  printf("tst_msgP->cda=%s\n",tst_msgP->cda);
  if(!strcmp(tst_msgP->cda,"01"))
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
  tst_msgP->cda[7] = 0;
  printf("tst_msgP->cda+3=%s\n",tst_msgP->cda+3);
  if(strcmp(tst_msgP->cda+3,"2345"))
    {
      printf("GOOD unnecessary data was NOT transmitted.\n");
    }
  else
    {
      printf("BAD unnecessary data was transmitted.\n");
      bads++;
    }
  
  // this should have been sent
  printf("tst_msgP->sda_length=%d\n",
	 tst_msgP->sda_length);
  if(tst_msgP->sda_length==1)
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD\n");
      bads++;
    }
  
  // this should have been sent and be x
  printf("tst_msgP->sda[0].c=%c\n",tst_msgP->sda[0].c);
  if(tst_msgP->sda[0].c == 'x')
    {
      printf("GOOD\n");
    }
  else
    {
      printf("BAD\n");
      bads++;
    }
  
  // this should NOT have been sent.
  printf("tst_msgP->sda[3].c=%c\n", tst_msgP->sda[3].c);
  if(tst_msgP->sda[3].c != 'y')
    {
      printf("GOOD unneccessary data was NOT sent.\n");
    }
  else
    {
      printf("BAD unneccessary data was sent.\n");
      bads++;
    }

  if(tst_msgP->do_int_size_test)
    {
      if(tst_msgP->smax == tst_msgP->s_array[0])
	{
	  printf("GOOD: tst_msgP->smax == tst_msgP->s_array[0] == %d\n",
		 tst_msgP->smax);
	}
      else
	{
	  printf("BAD tst_msgP->smax{%d} != tst_msgP->s_array[0]{%d}\n",
		 tst_msgP->smax, tst_msgP->s_array[0]);
	  bads++;
	}

      if(tst_msgP->smax == tst_msgP->i_smax)
	{
	  printf("GOOD: tst_msgP->smax == tst_msgP->i_smax == %d\n",
		 tst_msgP->i_smax);
	}
      else
	{
	  printf("BAD tst_msgP->smax{%d} != tst_msgP->i_smax{%d}\n",
		 tst_msgP->smax, tst_msgP->i_smax);
	  bads++;
	}

      if(tst_msgP->smin == tst_msgP->s_array[1])
	{
	  printf("GOOD: tst_msgP->smin == tst_msgP->s_array[1] == %d\n",
		 tst_msgP->smax);
	}
      else
	{
	  printf("BAD tst_msgP->smin{%d} != tst_msgP->s_array[1]{%d}\n",
		 tst_msgP->smin, tst_msgP->s_array[1]);
	  bads++;
	}

      if(tst_msgP->smin == tst_msgP->i_smin)
	{
	  printf("GOOD: tst_msgP->smin == tst_msgP->i_smin == %d\n",
		 tst_msgP->i_smin);
	}
      else
	{
	  printf("BAD tst_msgP->smin{%d} != tst_msgP->i_smin{%d}\n",
		 tst_msgP->smin, tst_msgP->i_smin);
	  bads++;
	}


      if(tst_msgP->imax == tst_msgP->i_array[0])
	{
	  printf("GOOD: tst_msgP->imax == tst_msgP->i_array[0] == %d\n",
		 tst_msgP->imax);
	}
      else
	{
	  printf("BAD tst_msgP->imax{%d} != tst_msgP->i_array[0]{%d}\n",
		 tst_msgP->imax, tst_msgP->i_array[0]);
	  bads++;
	}

      if(tst_msgP->imax == tst_msgP->l_imax)
	{
	  printf("GOOD: tst_msgP->imax == tst_msgP->l_imax == %ld\n",
		 tst_msgP->l_imax);
	}
      else
	{
	  printf("BAD tst_msgP->imax{%d} != tst_msgP->l_imax{%ld}\n",
		 tst_msgP->imax, tst_msgP->l_imax);
	  bads++;
	}

      if(tst_msgP->imin == tst_msgP->i_array[1])
	{
	  printf("GOOD: tst_msgP->imin == tst_msgP->i_array[1] == %d\n",
		 tst_msgP->imax);
	}
      else
	{
	  printf("BAD tst_msgP->imin{%d} != tst_msgP->i_array[1]{%d}\n",
		 tst_msgP->imin, tst_msgP->i_array[1]);
	  bads++;
	}

      if(tst_msgP->imin == tst_msgP->l_imin)
	{
	  printf("GOOD: tst_msgP->imin == tst_msgP->l_imin == %ld\n",
		 tst_msgP->l_imin);
	}
      else
	{
	  printf("BAD tst_msgP->imin{%d} != tst_msgP->l_imin{%ld}\n",
		 tst_msgP->imin, tst_msgP->l_imin);
	  bads++;
	}


      if(tst_msgP->lmax == tst_msgP->l_array[0])
	{
	  printf("GOOD: tst_msgP->lmax == tst_msgP->l_array[0] == %ld\n",
		 tst_msgP->lmax);
	}
      else
	{
	  printf("BAD tst_msgP->lmax{%ld} != tst_msgP->l_array[0]{%ld}\n",
		 tst_msgP->lmax, tst_msgP->l_array[0]);
	  bads++;
	}

      if(tst_msgP->lmax == tst_msgP->d_lmax)
	{
	  printf("GOOD: tst_msgP->lmax == tst_msgP->d_lmax == %f\n",
		 tst_msgP->d_lmax);
	}
      else
	{
	  printf("BAD tst_msgP->lmax{%ld} != tst_msgP->d_lmax{%f}\n",
		 tst_msgP->lmax, tst_msgP->d_lmax);
	  bads++;
	}

      if(tst_msgP->lmin == tst_msgP->l_array[1])
	{
	  printf("GOOD: tst_msgP->lmin == tst_msgP->l_array[1] == %ld\n",
		 tst_msgP->lmax);
	}
      else
	{
	  printf("BAD tst_msgP->lmin{%ld} != tst_msgP->l_array[1]{%ld}\n",
		 tst_msgP->lmin, tst_msgP->l_array[1]);
	  bads++;
	}

      if(tst_msgP->lmin == tst_msgP->d_lmin)
	{
	  printf("GOOD: tst_msgP->lmin == tst_msgP->d_lmin == %f\n",
		 tst_msgP->d_lmin);
	}
      else
	{
	  printf("BAD tst_msgP->lmin{%ld} != tst_msgP->d_lmin{%f}\n",
		 tst_msgP->lmin, tst_msgP->d_lmin);
	  bads++;
	}

      if(tst_msgP->usmax == tst_msgP->us_array[0])
	{
	  printf("GOOD: tst_msgP->usmax == tst_msgP->us_array[0] == %u\n",
		 tst_msgP->usmax);
	}
      else
	{
	  printf("BAD tst_msgP->usmax{%u} != tst_msgP->us_array[0]{%u}\n",
		 tst_msgP->usmax, tst_msgP->us_array[0]);
	  bads++;
	}

      if(tst_msgP->usmax == tst_msgP->ui_usmax)
	{
	  printf("GOOD: tst_msgP->usmax == tst_msgP->ui_usmax == %u\n",
		 tst_msgP->ui_usmax);
	}
      else
	{
	  printf("BAD tst_msgP->usmax{%u} != tst_msgP->ui_usmax{%u}\n",
		 tst_msgP->usmax, tst_msgP->ui_usmax);
	  bads++;
	}


      if(tst_msgP->uimax == tst_msgP->ui_array[0])
	{
	  printf("GOOD: tst_msgP->uimax == tst_msgP->ui_array[0] == %u\n",
		 tst_msgP->uimax);
	}
      else
	{
	  printf("BAD tst_msgP->uimax{%u} != tst_msgP->ui_array[0]{%u}\n",
		 tst_msgP->uimax, tst_msgP->ui_array[0]);
	  bads++;
	}

      if(tst_msgP->uimax == tst_msgP->ul_uimax)
	{
	  printf("GOOD: tst_msgP->uimax == tst_msgP->ul_uimax == %lu\n",
		 tst_msgP->ul_uimax);
	}
      else
	{
	  printf("BAD tst_msgP->uimax{%u} != tst_msgP->ul_uimax{%lu}\n",
		 tst_msgP->uimax, tst_msgP->ul_uimax);
	  bads++;
	}

      if(tst_msgP->ulmax == tst_msgP->ul_array[0])
	{
	  printf("GOOD: tst_msgP->ulmax == tst_msgP->ul_array[0] == %lu\n",
		 tst_msgP->ulmax);
	}
      else
	{
	  printf("BAD tst_msgP->ulmax{%lu} != tst_msgP->ul_array[0]{%lu}\n",
		 tst_msgP->ulmax, tst_msgP->ul_array[0]);
	  bads++;
	}

      if(tst_msgP->ulmax == tst_msgP->d_ulmax)
	{
	  printf("GOOD: tst_msgP->ulmax == tst_msgP->d_ulmax == %f\n",
		 tst_msgP->d_ulmax);
	}
      else
	{
	  printf("BAD tst_msgP->ulmax{%lu} != tst_msgP->d_ulmax{%f}\n",
		 tst_msgP->ulmax, tst_msgP->d_ulmax);
	  bads++;
	}
    }
  
  if(tst_msgP->true_bool)
    {
      printf("GOOD tst_msgP->true_bool is true\n");
    }
  else
    {
      printf("BAD tst_msgP->true_bool is false\n");
      bads++;
    }

  if(!tst_msgP->false_bool)
    {
      printf("GOOD tst_msgP->false_bool is false\n");
    }
  else
    {
      printf("BAD tst_msgP->false_bool is true\n");
      bads++;
    }
  
  if(tst_msgP->sminusone == -1)
    {
      printf("GOOD  tst_msgP->sminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msgP->sminusone=%d\n",tst_msgP->sminusone);
      bads++;
    }

  if(tst_msgP->iminusone == -1)
    {
      printf("GOOD  tst_msgP->iminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msgP->iminusone=%d\n",tst_msgP->iminusone);
      bads++;
    }

  if(tst_msgP->lminusone == -1)
    {
      printf("GOOD  tst_msgP->lminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msgP->lminusone=%ld\n",tst_msgP->lminusone);
      bads++;
    }

  if(tst_msgP->fminusone == -1)
    {
      printf("GOOD  tst_msgP->fminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msgP->fminusone=%f\n",tst_msgP->fminusone);
      bads++;
    }

  if(tst_msgP->dminusone == -1)
    {
      printf("GOOD  tst_msgP->dminusone equals -1.\n");
    }
  else
    {
      printf("BAD tst_msgP->dminusone=%f\n",tst_msgP->dminusone);
      bads++;
    }

  printf("bads=%d\n",bads);
  return bads;
}


int 
main(int argc, char **argv)
{
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  bool do_long_tests=true;
  if(getenv("NO_LONG_TESTS"))
    {
      do_long_tests=false;
    }
  
  strncpy(tst_msg.source_file,__FILE__,sizeof(tst_msg.source_file));
  strncpy(tst_msg.message,"tst_msg.message",sizeof(tst_msg.message));
  tst_msg.message_length = strlen(tst_msg.message);
  reset_time_tracker(&tst_msg.tt);

  //this will be sent.
  tst_msg.first_count = 1;
  tst_msg.last_count = 1;
  tst_msg.f  = -(1.0E-37);
  tst_msg.three_d_array[3][3][3] = 3.33;
  strcpy(tst_msg.cda,"01");
  tst_msg.cda_length = 3;
  // This string  will NOT be sent.
  strcpy(tst_msg.cda+3,"2345");
  
  // this will be sent.
  tst_msg.sda_length=1;
  tst_msg.sda[0].c= 'x';
  tst_msg.sda[1].c= 'z';
  
  // this will NOT be sent;
  tst_msg.sda[3].c = 'y';

  // Test some integer length features.
  tst_msg.do_int_size_test=true;
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
  tst_msg.etd = xxx;
  tst_msg.etd2 = uuu;
  tst_msg.enumtestvar = aa;
  tst_msg.enum_array[0] = aa;
  tst_msg.enum_array[4] = bb;
  tst_msg.enumtest_dla_length = 3;
  tst_msg.enumtest_dla[0] = aa;
  tst_msg.enumtest_dla[1] = bb;
  tst_msg.enumtest_dla[2] = aa;
  tst_msg.enumtest_dla[3] = bb;
  tst_msg.enumtest_dla[4] = aa;
  tst_msg.enumtest_dla[5] = bb;
  tst_msg.enumtest_dla[6] = aa;
  

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

  tst_msg.false_bool =false;
  tst_msg.true_bool=true;

  tst_msg.sminusone = -1;
  tst_msg.iminusone = -1;
  tst_msg.lminusone = -1;
  tst_msg.fminusone = -1.0;
  tst_msg.dminusone = -1.0;

  tst_msg.cart.x = 1.1;


  // This will be sent.
  if(argc > 1)
    {
      tst_msg.lastvar=strtol(argv[1],0,0);
    }
  else
    {
      tst_msg.lastvar = 999;
    }

  cycle_time_tracker(&tst_msg.tt);
  tst_msg.command_type=99;
  tst_msg.echo_serial_number=1088;
  tst_msg.state=1;
  tst_msg.line=2;
  tst_msg.source_line=3;
  printf("tst_msg.command_type=%ld\n",tst_msg.command_type);
  printf("tst_msg.echo_serial_number=%d\n",tst_msg.echo_serial_number);
  printf("tst_msg.state=%d\n",tst_msg.state);
  printf("tst_msg.line=%d\n",tst_msg.line);
  printf("tst_msg.source_line=%d\n",tst_msg.source_line);
  printf("tst_msg.source_file=%s\n",tst_msg.source_file);
  printf("tst_msg.tt.count=%d\n",tst_msg.tt.count);
  printf("tst_msg.tt.elapsed=%f\n",tst_msg.tt.elapsed);
  printf("tst_msg.tt.min=%f\n",tst_msg.tt.min);
  printf("tst_msg.tt.max=%f\n",tst_msg.tt.max);
  printf("tst_msg.tt.avg=%f\n",tst_msg.tt.avg);
  printf("tst_msg.message_length=%d\n",tst_msg.message_length);
  printf("tst_msg.message=%s\n",tst_msg.message);
  printf("tst_msg.cart.x=%f\n",tst_msg.cart.x);
  printf("tst_msg.lastvar=%ld\n",tst_msg.lastvar);

//   FILE *f =fopen("tst_msg.packed","wb");
//   CMS *cms = new CMS(sizeof(TEST_MESSAGE));
//   void *data_ptr;
//   long data_size;
//   msg_to_encoded_data(cms, &tst_msg,
// 		      data_ptr, data_size,
// 		      nml_test_format,
// 		      CMS_PACKED_ENCODING);
//   fwrite(data_ptr,1,data_size,f);
//   fclose(f);
//   delete cms;

  NML n(nml_test_format, 0,0,0);
  
  n.write_encoded_data_to_file(&tst_msg,
			      CMS_PACKED_ENCODING,
			      "tst_msg.packed");

  TEST_MESSAGE *tst_msgP= (TEST_MESSAGE *) 
      n.read_encoded_data_from_file(
				    "tst_msg.packed",
				    CMS_PACKED_ENCODING);

  bool check_test_message_failed = false;
  if(check_test_message(tst_msgP,tst_msg.lastvar) != 0)
    {
      check_test_message_failed=true;
    }

  printf("tst_msgP->command_type=%ld\n",tst_msgP->command_type);
  printf("tst_msgP->echo_serial_number=%d\n",tst_msgP->echo_serial_number);
  printf("tst_msgP->state=%d\n",tst_msgP->state);
  printf("tst_msgP->line=%d\n",tst_msgP->line);
  printf("tst_msgP->source_line=%d\n",tst_msgP->source_line);
  printf("tst_msgP->source_file=%s\n",tst_msgP->source_file);
  printf("tst_msgP->tt.count=%d\n",tst_msgP->tt.count);
  printf("tst_msgP->tt.elapsed=%f\n",tst_msgP->tt.elapsed);
  printf("tst_msgP->tt.min=%f\n",tst_msgP->tt.min);
  printf("tst_msgP->tt.max=%f\n",tst_msgP->tt.max);
  printf("tst_msgP->tt.avg=%f\n",tst_msgP->tt.avg);
  printf("tst_msgP->message_length=%d\n",tst_msgP->message_length);
  printf("tst_msgP->message=%s\n",tst_msgP->message);
  printf("tst_msg.cart.x=%f\n",tst_msgP->cart.x);
  printf("tst_msgP->lastvar=%ld\n",tst_msgP->lastvar);


  TEST_MESSAGE orig_tst_msg;
  orig_tst_msg = tst_msg;

  n.writeEncodedMessageMemoryMapToFile(&tst_msg,
				       CMS_PACKED_ENCODING,
				       "tst_msg_encode_packed_format.csv");

  tst_msg = orig_tst_msg;
  n.writeRawMessageMemoryMapToFile(&tst_msg,"tst_msg_raw_format.csv");
  tst_msg = orig_tst_msg;

  if(check_test_message_failed)
    {
      fprintf(stderr,"check_tst_message_failed!\n");
      exit(1);
    }
		      
  exit(0);
}
