


#include "nml_test_format.hh"

int 
main()
{

  SIMPLER_MSG sMsg;
  NML nml(nml_test_format,0,0,0);
  
  nml.writeEncodedMessageMemoryMapToFile(&sMsg, CMS_XDR_ENCODING, "simple_xdr_msg_format.txt");

  nml.writeEncodedMessageMemoryMapToFile(&sMsg, CMS_PACKED_ENCODING, "simple_packed_msg_format.txt");
  
  TEST_MESSAGE tMsg;
  nml.writeEncodedMessageMemoryMapToFile(&sMsg, CMS_PACKED_ENCODING, "test_msg_format.txt");

}
