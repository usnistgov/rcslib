
#include "rcs.hh"

#include <stdio.h>

int 
main(int argc, const char **argv)
{
  if(argc != 4)
    {
      fprintf(stderr,"%s usage: buf proc cfg\n",
	      argv[0]);
      exit(254);
    }

  NML *nml =new NML(nmlErrorFormat,argv[1],argv[2],argv[3]);

  NMLTYPE t = nml->read(); // Retrieves only the type from the message.

  printf("t=%ld\n",t);
  switch(t)
    {
    case NML_ERROR_TYPE:
      {
	NML_ERROR *errMsg = (NML_ERROR *) nml->get_address();
	printf("errMsg->error=%s\n",errMsg->error);
      }
      break;

    default:
      break;
    }

  delete nml;
  
};
