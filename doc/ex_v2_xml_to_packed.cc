#include "nml_ex1_v2.hh"

int main(int argc, const char **argv)
{
  if(argc != 3)
    {
      rcs_print_error("run with xml_file packed_file\n");
      rcs_exit(1);
    }

  NML nml(ex_format,0,0,0);
  
  nml.write_encoded_data_to_file(
				 nml.read_encoded_data_from_file(argv[1],
								 CMS_XML_ENCODING),
				 CMS_PACKED_ENCODING,
				 argv[2]);
}
