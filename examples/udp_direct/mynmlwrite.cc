

#include "rcs.hh"
#include "mymsg.hh"

#include <stdio.h>

NML *nmlP = 0;

int main(int, const char **)
{
  nmlP = new NML(MYMSG_format,"udp_buf","mynmlwrite","udp_direct.nml");

  if(!nmlP->valid())
    {
      delete nmlP;
      exit(1);
    }

  nml_start();
  MYMSG myMsg;
  myMsg.int_in_mymsg = 1033; // just a random number to confirm we are actually communicating.

  int nml_write_ret = nmlP->write(myMsg);
  printf("nml_write_ret=%d\n", nml_write_ret);

  delete nmlP;
  nml_cleanup();
  
}
