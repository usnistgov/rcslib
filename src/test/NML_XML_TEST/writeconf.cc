#include <iostream>
#include "myconf.h"
#include "myconf_n_codegen_protos.hh"

using namespace std;

int 
main(int argc, const char **argv)
{
  cout << "Before NML creation" << endl;
  NML nml(MY_CONF_format,0,0,0);
  cout << "After NML creation" << endl;
  MY_CONF cf;
  cout << "Before variable declaration" << endl;
  cf.p_gain=0.99;
  cf.i_gain=0.01;
  cf.d_gain=0.02;
  cout << "After variable declaration" << endl;
  nml.xmlMsgSaveAs(&cf,"myconf.xml");
  cout << "Saved Configuration as XML file" << endl;
  exit(0);
}
