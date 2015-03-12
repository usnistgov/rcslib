
#include "rcs.hh"

#include <stdio.h>

int null_format(NMLTYPE /*type*/, void */*buf*/, CMS */*cms*/) { return -1; };

int 
main(int argc, const char **argv)
{

  if(argc != 4)
    {
      fprintf(stderr,"%s usage: buf proc cfg\n",
	      argv[0]);
      exit(254);
    }

  NML *nml =new NML(null_format,argv[1],argv[2],argv[3]);

  int was_read = nml->check_if_read(); // Retrieves only the type from the message.

  printf("%d",was_read);
  
  delete nml;

  exit(was_read != 0);
  
};
