
#include "rcs.hh"

#include <stdio.h>
#include <stdlib.h>

int null_format(NMLTYPE /*type*/, void */*buf*/, CMS */*cms*/) { return -1; };

int 
main(int /*argc*/, const char **argv)
{

  NML *nml =new NML(null_format,argv[1],argv[2],argv[3]);

  NMLTYPE t = nml->get_msg_type(); // Retrieves only the type from the message.

  printf("%ld",t);
  
  delete nml;
  exit(0);
};
