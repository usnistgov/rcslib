

#include "nml.hh"
#include "rcs_prnt.hh"

#include <stdlib.h>


int
main(int argc, const char **argv)
{
  if(argc < 3)
    {
      rcs_print_error("nmlGetRemoteTcpServerBufferList usage: HOST PORT <use_ipv6>\n");
      exit(1);
    }

  int port = atoi(argv[2]);
  rcs_print("Buffer list for %s %d:\n",
	    argv[1],port);
  int use_ipv6=0;
  if(argc == 4)
    {
      use_ipv6 = atoi(argv[3]);
    }
  rcs_print("%s\n",nmlGetRemoteTcpServerBufferList(argv[1],port,use_ipv6));
  return 0;
}
