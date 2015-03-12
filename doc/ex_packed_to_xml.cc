#include "nml_ex1.hh"

int main(int argc, const char **argv)
{
  if(argc != 3)
    {
      rcs_print_error("run with packed_file xml_file\n");
      rcs_exit(1);
    }

  NML nml(ex_format,0,0,0);
  
  nml.write_encoded_data_to_file(
				 nml.read_encoded_data_from_file(argv[1],
								 CMS_PACKED_ENCODING),
				 CMS_XML_ENCODING,
				 argv[2]);
}
