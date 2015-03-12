

#include "rcs.hh"
#include "mymsg.hh"

NML *nmlP = 0;

int main(int, const char **)
{
  nmlSetToServer();
  nmlP = new NML(MYMSG_format,"udp_buf","mynmlsvr","udp_direct.nml");
 if(!nmlP->valid())
    {
      delete nmlP;
      exit(1);
    }

  run_nml_servers();
}
