
#include "nml_ex1.hh"

int main(int argc, const char **argv) 
{
  if(argc<2)
    {
      rcs_print_error("Need filename argument.\n");
      rcs_exit(1);
    }

  NML nml(ex_format, 0,0,0);
  EXAMPLE_MSG *example_msg_ptr = ( EXAMPLE_MSG *)
    nml.read_encoded_data_from_file(argv[1],
				    CMS_PACKED_ENCODING);
	
  rcs_print(" %s:. \n");
  rcs_print(" The value of its members are:\n ");
  rcs_print(" f=%f, c=%c, i=%d\n ",
	    example_msg_ptr->f,
	    example_msg_ptr->c,
	    example_msg_ptr->i);
}

x
