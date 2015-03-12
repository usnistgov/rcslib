
#include "rcs.hh"
#include "nml_ex1.hh"
#include <stdio.h>

int main(int ,const char **) {

  EXAMPLE_MSG example_msg;
  NML nml(ex_format,0,0,0);
  
  printf("%s\n",nml.msg2str(&example_msg));

  // this only works if generate_symbol_lookups=true passed to CodeGenerator.
  printf("%s\n",nml.msg2xml(&example_msg));

}

