#include "myconf.h"
#include "myconf_n_codegen_protos.hh"

#include <stdio.h>

int 
main(int argc, const char **argv)
{
  NML nml(MY_CONF_format,0,0,0);
  
  
  MY_CONF *cf = (MY_CONF *) nml.readMsgFromXmlFile("myconf.xml");
  if(NULL != cf)
    {
      printf("p:%f\ni:%f\nd:%f\n",cf->p_gain,cf->i_gain,cf->d_gain);
    }

  exit(0);
}
