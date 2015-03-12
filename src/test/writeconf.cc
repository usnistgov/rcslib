#include "myconf.h"
#include "myconf_n_codegen_protos.hh"

int 
main(int argc, const char **argv)
{
  NML nml(MY_CONF_format,0,0,0);
  
  MY_CONF cf;
  
  cf.p_gain=0.99;
  cf.i_gain=0.01;
  cf.d_gain=0.02;
  nml.xmlMsgSaveAs(&cf,"myconf.xml");
  nml.xmlSchemaSaveAs("myconf.xsd");
  exit(0);
}

