
#ifndef MSG_TO_ENCODED_DATA_HH
#define MSG_TO_ENCODED_DATA_HH

#include "nml.hh"
#include "cms_types.hh"

class cms;
class NMLmsg;

extern void
msg_to_encoded_data(CMS *cms, NMLmsg *nml_msg, 
		    void *&encoded_data, long &encoded_size,
		    NML_FORMAT_PTR format_function,
		    enum CMS_NEUTRAL_ENCODING_METHOD enc_method);

extern NMLmsg *
encoded_data_to_msg(CMS *cms,
		    void *encoded_data, 
		    long encoded_size,
		    NML_FORMAT_PTR format_function,
		    enum CMS_NEUTRAL_ENCODING_METHOD enc_method);

#endif
