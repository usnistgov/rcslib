

#include "cms.hh"

int
msg2xdr(CMS *cms, NMLmsg *nml_msg, 
	void *xdr_data, size_t xdr_size,
	NML_FORMAT_PTR format_function)
{
  void *orig_encoded_data = cms->encoded_data;
  size_t orig_endoded_data_size= cms->encoded_data_size;
  cms->encoded_data=0;
  cms->encoded_data_size=0;
  cms->set_temp_updater(CMS_XDR_ENCODING);
  cms->set_encoded_data(xdr_data,xdr_size);
  cms->update->set_mode(CMS_ENCODE_DATA);
  cms->format_low_ptr = (char *) nml_msg;
  cms->format_high_ptr = cms->format_low_ptr + nml_msg->size;
  cms->rewind();
  cms->update_with_name ("type", nml_msg->type);
  cms->update_with_name ("size", nml_msg->size);
  format_function(nml_msg->type,nml_msg,cms);
  xdr_data = cms->encoded_data;
  xdr_size = cms->encoded_data_size;
  cms->restore_normal_updater();
}

int
xdr2msg(CMS *, NMLmsg *, 
	void *xdr_data, size_t xdr_size,
	NML_FORMAT_PTR format_function)
{
  void *orig_encoded_data = cms->encoded_data;
  size_t orig_endoded_data_size= cms->encoded_data_size;
  cms->encoded_data=0;
  cms->encoded_data_size=0;
  cms->set_temp_updater(CMS_XDR_ENCODING);
  cms->set_encoded_data(xdr_data,xdr_size);
  cms->update->set_mode(CMS_DECODE_DATA);
  cms->format_low_ptr = (char *) nml_msg;
  cms->format_high_ptr = cms->format_low_ptr + nml_msg->size;
  cms->rewind();
  NMLTYPE type_from_xdr_data = nml_msg->type;;
  cms->update_with_name ("type", nml_msg->type);
  if(type_from_xdr_data != nml_msg->type)
    {
      rcs_print_error("xdr2msg : types do not match %d != %d\n",
		      type_from_xdr_data, nml_msg->type);
      return -1;
    }
  size_t size_from_xdr_data = nml_msg->size;
  cms->update_with_name ("size", size_from_xdr_data);
  format_function(nml_msg->type,nml_msg,cms);
  cms->restore_normal_updater();
  cms->set_encoded_data(orig_encoded_data,orig_encoded_data_size);
}
