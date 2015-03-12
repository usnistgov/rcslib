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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

int 
main(int argc, char **argv)
{
  set_rcs_print_destination(RCS_PRINT_TO_STDERR);
  bool do_long_tests=true;
  if(getenv("NO_LONG_TESTS"))
    {
      do_long_tests=false;
    }
  
  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile  lastvar\n");
      exit(127);
    }
  
  NML *nml = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  if(nml == 0)
    {
      exit(126);
    }
  if(!nml->valid())
    {
      fprintf(stderr,"nml->valid() check failed.\n");
      delete nml;
      exit(125);
    }
  int t;
  TEST_MESSAGE tst_msg;
  
  strncpy(tst_msg.source_file,__FILE__,sizeof(tst_msg.source_file));
  strncpy(tst_msg.message,"tst_msg.message",sizeof(tst_msg.message));
  tst_msg.message_length = strlen(tst_msg.message);
  reset_time_tracker(&tst_msg.tt);

  //this will be sent.
  tst_msg.f  = -(1.0E-37);
  tst_msg.three_d_array[1][1][1] = 3.33;
  strcpy(tst_msg.cda,"01");
  tst_msg.cda_length = 3;
  // This string  will NOT be sent.
  strcpy(tst_msg.cda+3,"2345");
  
  // this will be sent.
  tst_msg.sda_length=1;
  tst_msg.sda[0].c= 'x';
  
  // this will NOT be sent;
  tst_msg.sda[1].c = 'y';

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

  tst_msg.false_bool =false;
  tst_msg.true_bool=true;

  tst_msg.sminusone = -1;
  tst_msg.iminusone = -1;
  tst_msg.lminusone = -1;
  tst_msg.fminusone = -1.0;
  tst_msg.dminusone = -1.0;
  

  // This will be sent.
  tst_msg.lastvar=strtol(argv[4],0,0);
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

  if((t = nml->write(tst_msg)))
    {
      fprintf(stderr,"nml->write() returned %d\n", t);
      delete nml;
      exit(124);
    }
  delete nml;
  exit(0);
}
