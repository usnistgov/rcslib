

#include "nml_ex1_v2.hh"

int main(int argc, const char **argv) 
{
 NML nml(ex_format, 0,0,0);
 EXAMPLE_MSG example_msg;
 
 nml.writeEncodedMessageMemoryMapToFile(&example_msg,
					CMS_PACKED_ENCODING,
					"EXAMPLE_MSG_TYPE_map_v2.csv");
}
