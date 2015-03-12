
#include "nml_test_format.hh"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int 
main(int argc, char **argv)
{
  NML *nml_ptr;
  rcs_print("nml_test_server: %s compiled on %s at %s\n",__FILE__,__DATE__,__TIME__);
  print_rcs_version();
  
  if(argc < 3)
    {
      fprintf(stderr,"usage : bufname procname configfile\n");
      exit(-1);
    }
  
  nml_ptr = new NML(nml_test_format,argv[1],argv[2],argv[3]);
  rcs_print("nml_ptr=%p\n",nml_ptr);

  if(!nml_ptr)
    {
      fprintf(stderr,"OUT-OF-MEMORY\n");
      exit(127);
    }
  if(!nml_ptr->valid())
    {
      fprintf(stderr,"nml_ptr->valid() check failed.\n");
      exit(-1);
    }
  printf("Starting NML server(s) . . . \n");
  run_nml_servers();
  exit(0);
}
