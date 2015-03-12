

#include "rcs.hh"
#include "mymsg.hh"

#include <stdio.h>

NML *nmlP = 0;

int main(int, const char **)
{
  nmlP = new NML(MYMSG_format,"udp_buf","mynmlread","udp_direct.nml");

  if(!nmlP->valid())
    {
      delete nmlP;
      exit(1);
    }

  nml_start();

  NMLTYPE nml_read_ret = nmlP->read();
  printf("nml_read_ret=%ld\n", nml_read_ret);
  if(nml_read_ret == MYMSG_TYPE)
    {
      MYMSG *mymsgP = (MYMSG *) nmlP->get_address();
      printf("mymsgP->int_in_mymsg=%d\n",mymsgP->int_in_mymsg);
    }

  delete nmlP;
  nml_cleanup();
  
}
