

#include "cms.hh"
#include "cms_up.hh"
#include "nml.hh"
#include "nmlmsg.hh"
#include "rcs_prnt.hh"
#include "msg_to_encoded_data.hh"

void
msg_to_encoded_data(CMS *cms, NMLmsg *nml_msg, 
		    void *&encoded_data, long &encoded_size,
		    NML_FORMAT_PTR format_function,
		    enum CMS_NEUTRAL_ENCODING_METHOD enc_method)
{
  cms->set_temp_updater(enc_method);
  cms->updater->set_mode(CMS_ENCODE_DATA);
  cms->format_low_ptr = (char *) nml_msg;
  cms->format_high_ptr = cms->format_low_ptr + nml_msg->size;
  cms->rewind();
  cms->update_with_name ("type", nml_msg->type);
  cms->update_with_name ("size", nml_msg->size);
  format_function(nml_msg->type,nml_msg,cms);
  encoded_data = cms->encoded_data;
  encoded_size = cms->get_encoded_msg_size();
  cms->restore_normal_updater();
}

 NMLmsg *
encoded_data_to_msg(CMS *cms,
		    void *encoded_data, 
		    long encoded_size,
		    NML_FORMAT_PTR format_function,
		    enum CMS_NEUTRAL_ENCODING_METHOD enc_method)
{
  cms->set_temp_updater(enc_method);
  bool orig_zero_encoded_data_when_set = (bool) (cms->zero_encoded_data_when_set != 0);
  void *orig_encoded_data = cms->encoded_data;
  long orig_encoded_data_size= cms->encoded_data_size;
  cms->encoded_data=0;
  cms->encoded_data_size=0;
  cms->zero_encoded_data_when_set=false;
  cms->set_encoded_data(encoded_data,encoded_size);
  cms->zero_encoded_data_when_set=orig_zero_encoded_data_when_set;
  cms->updater->set_mode(CMS_DECODE_DATA);
  cms->rewind();
  NMLmsg *nml_msg = (NMLmsg *) cms->subdiv_data;
  cms->update_with_name ("type",nml_msg->type );
  cms->update_with_name ("size", nml_msg->size);
  cms->format_low_ptr = (char *) cms->subdiv_data;
  cms->format_high_ptr = cms->format_low_ptr + cms->size;
  format_function(nml_msg->type,nml_msg,cms);
  cms->set_encoded_data(orig_encoded_data,orig_encoded_data_size);
  cms->restore_normal_updater();
  return (NMLmsg *) nml_msg;
}
