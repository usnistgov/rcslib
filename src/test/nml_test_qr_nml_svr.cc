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

NML_QR_SERVER *echoQrServer=0;

static void 
touch_running_file(void)
{
  static const char *fname = "nml_test_qr_nml_svr.running";
  FILE *f = fopen(fname,"w");
  if(f)
    {
      fprintf(f,"%s\n",fname);
      fclose(f);
    }
}


int main(int argc, char **argv)
{
  if(argc < 5)
    {
      fprintf(stderr,"usage : bufname procname configfile logfile\n");
      exit(127);
    }

  if(strcmp(argv[4],"--") && strcmp(argv[4],"stdout")) {
    set_rcs_print_destination(RCS_PRINT_TO_FILE);
    set_rcs_print_file(argv[4]);
  }
  print_rcs_version();

  
  echoQrServer= new NML_QR_SERVER(ECHO_format,argv[1],argv[2],argv[3]);
  //  touch_running_file("nml_test_qr_nml_svr.running");

  run_nml_servers_with_func(touch_running_file);
}
