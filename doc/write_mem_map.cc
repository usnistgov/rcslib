

#include "nml_ex1.hh"

int main(int argc, const char **argv) 
{
 NML nml(ex_format, 0,0,0);
 EXAMPLE_MSG example_msg;
 
 nml.writeEncodedMessageMemoryMapToFile(&example_msg,
					CMS_PACKED_ENCODING,
					"EXAMPLE_MSG_TYPE_map.csv");
}
