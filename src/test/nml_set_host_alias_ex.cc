#include "rcs.hh"
#include "nml_ex1.hh"

int main(int argc, char **argv)
{

  if(argc > 1)
    {
      nmlSetHostAlias(argv[1],"localhost");
    }
  NML example_nml(ex_format, "ex_buf1", "bReader", "ex_cfg.nml");
  
  if(!example_nml.valid())
    {
      rcs_print_error("Bad.\n");
    }
  else
    {
      rcs_print("Good.\n");
    }

}
