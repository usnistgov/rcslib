

#include "nml_ex1.hh"

int main()
{
  NML nml(ex_format,"ex_buf1","simple_data_logger","ex_cfg.nml");

  nml.read();
  nml.write_encoded_data_to_file(nml.get_address(),
				 CMS_PACKED_ENCODING,
				 "ex_msg_file.EXAMPLE_MSG_TYPE");
}
