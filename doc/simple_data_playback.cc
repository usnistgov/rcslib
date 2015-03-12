

#include "nml_ex1.hh"

int main(int argc, const char **argv)
{
  NML nml(ex_format,"ex_buf1","simple_data_playback","ex_cfg.nml");

  nml.write(nml.read_encoded_data_from_file("ex_msg_file.EXAMPLE_MSG_TYPE",
					    CMS_PACKED_ENCODING));


}
