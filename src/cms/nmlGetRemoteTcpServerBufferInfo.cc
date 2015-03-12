

#include "nml.hh"
#include "rcs_prnt.hh"

#include <stdlib.h>


int
main(int argc, const char **argv)
{
  if(argc < 4)
    {
      rcs_print_error("nmlGetRemoteTcpServerBufferInfo usage: BUFFERNAME HOST PORT <use_ipv6>\n");
      exit(1);
    }

  int port = atoi(argv[3]);
  rcs_print("Buffer info for %s from %s %d:\n",
	    argv[1],argv[2],port);
  int use_ipv6=0;
  if(argc == 5)
    {
      use_ipv6 = atoi(argv[4]);
    }
  rcs_print("%s\n",nmlGetRemoteTcpServerBufferInfo(argv[1],argv[2],port,use_ipv6));
  return 0;
}
