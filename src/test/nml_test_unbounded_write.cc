

#include "nml_test_unbounded_format.hh"
#include "check_test_unbounded_msg.hh"

#include <stdio.h>
#include <stdlib.h>


static void 
free_ntu_s1(ntu_s1 *ntu_s1P)
{
  if(ntu_s1P->ntu_s1_char_ua)
    {
      free(ntu_s1P->ntu_s1_char_ua);
      ntu_s1P->ntu_s1_char_ua=0;
    }
  if(ntu_s1P->ntu_s1_u_char_ua)
    {
      free(ntu_s1P->ntu_s1_u_char_ua);
      ntu_s1P->ntu_s1_u_char_ua=0;
    }
  if(ntu_s1P->ntu_s1_short_ua)
    {
      free(ntu_s1P->ntu_s1_short_ua);
      ntu_s1P->ntu_s1_short_ua=0;
    }
  if(ntu_s1P->ntu_s1_u_short_ua)
    {
      free(ntu_s1P->ntu_s1_u_short_ua);
      ntu_s1P->ntu_s1_u_short_ua=0;
    }
  if(ntu_s1P->ntu_s1_int_ua)
    {
      free(ntu_s1P->ntu_s1_int_ua);
      ntu_s1P->ntu_s1_int_ua=0;
    }
  if(ntu_s1P->ntu_s1_u_int_ua)
    {
      free(ntu_s1P->ntu_s1_u_int_ua);
      ntu_s1P->ntu_s1_u_int_ua=0;
    }
  if(ntu_s1P->ntu_s1_long_ua)
    {
      free(ntu_s1P->ntu_s1_long_ua);
      ntu_s1P->ntu_s1_long_ua=0;
    }
  if(ntu_s1P->ntu_s1_u_long_ua)
    {
      free(ntu_s1P->ntu_s1_u_long_ua);
      ntu_s1P->ntu_s1_u_long_ua=0;
    }
  if(ntu_s1P->ntu_s1_float_ua)
    {
      free(ntu_s1P->ntu_s1_float_ua);
      ntu_s1P->ntu_s1_float_ua=0;
    }
  if(ntu_s1P->ntu_s1_double_ua)
    {
      free(ntu_s1P->ntu_s1_double_ua);
      ntu_s1P->ntu_s1_double_ua=0;
    }
}

static void
free_ntu(NML_TEST_UNBOUNDED_MSG *ntu_test_msgP)
{
  if(ntu_test_msgP->char_ua)
    {
      free(ntu_test_msgP->char_ua);
      ntu_test_msgP->char_ua=0;
    }
  if(ntu_test_msgP->u_char_ua)
    {
      free(ntu_test_msgP->u_char_ua);
      ntu_test_msgP->u_char_ua=0;
    }
  if(ntu_test_msgP->short_ua)
    {
      free(ntu_test_msgP->short_ua);
      ntu_test_msgP->short_ua=0;
    }
  if(ntu_test_msgP->u_short_ua)
    {
      free(ntu_test_msgP->u_short_ua);
      ntu_test_msgP->u_short_ua=0;
    }
  if(ntu_test_msgP->int_ua)
    {
      free(ntu_test_msgP->int_ua);
      ntu_test_msgP->int_ua=0;
    }
  if(ntu_test_msgP->u_int_ua)
    {
      free(ntu_test_msgP->u_int_ua);
      ntu_test_msgP->u_int_ua=0;
    }
  if(ntu_test_msgP->long_ua)
    {
      free(ntu_test_msgP->long_ua);
      ntu_test_msgP->long_ua=0;
    }
  if(ntu_test_msgP->u_long_ua)
    {
      free(ntu_test_msgP->u_long_ua);
      ntu_test_msgP->u_long_ua=0;
    }
  if(ntu_test_msgP->float_ua)
    {
      free(ntu_test_msgP->float_ua);
      ntu_test_msgP->float_ua=0;
    }
  if(ntu_test_msgP->double_ua)
    {
      free(ntu_test_msgP->double_ua);
      ntu_test_msgP->double_ua=0;
    }
  free_ntu_s1(&(ntu_test_msgP->s1));
  
  for(int i=0; i < ARRAY_LENI(ntu_test_msgP->s1_a); i++)
    {
      free_ntu_s1(&(ntu_test_msgP->s1_a[i]));
    }
}

static void realloc_ntu_s1(ntu_s1 *ntu_s1P, int new_size)
{
  ntu_s1P->ntu_s1_char_ua = (char *)
    realloc(ntu_s1P->ntu_s1_char_ua, (sizeof(char)*new_size));
  ntu_s1P->ntu_s1_char_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_char_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_char_ua[i] = (char) ('0'+(i%10));
    }

  ntu_s1P->ntu_s1_u_char_ua = (unsigned char *)
    realloc(ntu_s1P->ntu_s1_u_char_ua, (sizeof(unsigned char)*new_size));
  ntu_s1P->ntu_s1_u_char_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_u_char_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_u_char_ua[i] = (unsigned char) ('0'+(i%10));
    }

  ntu_s1P->ntu_s1_short_ua = (short *)
    realloc(ntu_s1P->ntu_s1_short_ua, (sizeof(short)*new_size));
  ntu_s1P->ntu_s1_short_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_short_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_short_ua[i] = (short) i;
    }

  ntu_s1P->ntu_s1_u_short_ua = (unsigned short *)
    realloc(ntu_s1P->ntu_s1_u_short_ua, (sizeof(unsigned short)*new_size));
  ntu_s1P->ntu_s1_u_short_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_u_short_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_u_short_ua[i] = (unsigned short) i;
    }

  ntu_s1P->ntu_s1_int_ua = (int *)
    realloc(ntu_s1P->ntu_s1_int_ua, (sizeof(int)*new_size));
  ntu_s1P->ntu_s1_int_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_int_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_int_ua[i] = (int) i;
    }

  ntu_s1P->ntu_s1_u_int_ua = (unsigned int *)
    realloc(ntu_s1P->ntu_s1_u_int_ua, (sizeof(unsigned int)*new_size));
  ntu_s1P->ntu_s1_u_int_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_u_int_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_u_int_ua[i] = (unsigned int) i;
    }

  ntu_s1P->ntu_s1_long_ua = (long *)
    realloc(ntu_s1P->ntu_s1_long_ua, (sizeof(long)*new_size));
  ntu_s1P->ntu_s1_long_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_long_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_long_ua[i] = (long) i;
    }

  ntu_s1P->ntu_s1_u_long_ua = (unsigned long *)
    realloc(ntu_s1P->ntu_s1_u_long_ua, (sizeof(unsigned long)*new_size));
  ntu_s1P->ntu_s1_u_long_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_u_long_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_u_long_ua[i] = (unsigned long) i;
    }

  ntu_s1P->ntu_s1_float_ua = (float *)
    realloc(ntu_s1P->ntu_s1_float_ua, (sizeof(float)*new_size));
  ntu_s1P->ntu_s1_float_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_float_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_float_ua[i] = (float) i;
    }

  ntu_s1P->ntu_s1_double_ua = (double *)
    realloc(ntu_s1P->ntu_s1_double_ua, (sizeof(double)*new_size));
  ntu_s1P->ntu_s1_double_ua_size_allocated = new_size;
  ntu_s1P->ntu_s1_double_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_s1P->ntu_s1_double_ua[i] = (double) i;
    }
}


static void
realloc_ntu(NML_TEST_UNBOUNDED_MSG *ntu_test_msgP, int new_size, int ntu_s1_size)
{

  ntu_test_msgP->char_ua = (char *)
    realloc(ntu_test_msgP->char_ua, (sizeof(char)*new_size));
  ntu_test_msgP->char_ua_size_allocated = new_size;
  ntu_test_msgP->char_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->char_ua[i] = (char) ('0'+(i%10));
    }

  ntu_test_msgP->u_char_ua = (unsigned char *)
    realloc(ntu_test_msgP->u_char_ua, (sizeof(unsigned char)*new_size));
  ntu_test_msgP->u_char_ua_size_allocated = new_size;
  ntu_test_msgP->u_char_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->u_char_ua[i] = (unsigned char) ('0'+(i%10));
    }

  ntu_test_msgP->short_ua = (short *)
    realloc(ntu_test_msgP->short_ua, (sizeof(short)*new_size));
  ntu_test_msgP->short_ua_size_allocated = new_size;
  ntu_test_msgP->short_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->short_ua[i] = (short) i;
    }

  ntu_test_msgP->u_short_ua = (unsigned short *)
    realloc(ntu_test_msgP->u_short_ua, (sizeof(unsigned short)*new_size));
  ntu_test_msgP->u_short_ua_size_allocated = new_size;
  ntu_test_msgP->u_short_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->u_short_ua[i] = (unsigned short) i;
    }

  ntu_test_msgP->int_ua = (int *)
    realloc(ntu_test_msgP->int_ua, (sizeof(int)*new_size));
  ntu_test_msgP->int_ua_size_allocated = new_size;
  ntu_test_msgP->int_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->int_ua[i] = (int) i;
    }

  ntu_test_msgP->u_int_ua = (unsigned int *)
    realloc(ntu_test_msgP->u_int_ua, (sizeof(unsigned int)*new_size));
  ntu_test_msgP->u_int_ua_size_allocated = new_size;
  ntu_test_msgP->u_int_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->u_int_ua[i] = (unsigned int) i;
    }

  ntu_test_msgP->long_ua = (long *)
    realloc(ntu_test_msgP->long_ua, (sizeof(long)*new_size));
  ntu_test_msgP->long_ua_size_allocated = new_size;
  ntu_test_msgP->long_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->long_ua[i] = (long) i;
    }

  ntu_test_msgP->u_long_ua = (unsigned long *)
    realloc(ntu_test_msgP->u_long_ua, (sizeof(unsigned long)*new_size));
  ntu_test_msgP->u_long_ua_size_allocated = new_size;
  ntu_test_msgP->u_long_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->u_long_ua[i] = (unsigned long) i;
    }

  ntu_test_msgP->float_ua = (float *)
    realloc(ntu_test_msgP->float_ua, (sizeof(float)*new_size));
  ntu_test_msgP->float_ua_size_allocated = new_size;
  ntu_test_msgP->float_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->float_ua[i] = (float) i;
    }

  ntu_test_msgP->double_ua = (double *)
    realloc(ntu_test_msgP->double_ua, (sizeof(double)*new_size));
  ntu_test_msgP->double_ua_size_allocated = new_size;
  ntu_test_msgP->double_ua_length = new_size;
  for(int i =0 ; i < new_size; i++)
    {
      ntu_test_msgP->double_ua[i] = (double) i;
    }

  realloc_ntu_s1(&(ntu_test_msgP->s1),ntu_s1_size);
  
  for(int i=0; i < new_size && i < ARRAY_LENI(ntu_test_msgP->s1_a); i++)
    {
      realloc_ntu_s1(&(ntu_test_msgP->s1_a[i]),ntu_s1_size);
    }
}

NML *ntuw_nmlP=0;
NML *ntuw_read_xml_nmlP=0;
NML *ntuw_read_packed_nmlP=0;
NML *ntuw_read_xdr_nmlP=0;

static void close_nml()
{
  if(ntuw_nmlP)
    {
      delete ntuw_nmlP;
      ntuw_nmlP=0;
    }
  if(ntuw_read_xml_nmlP)
    {
      delete ntuw_read_xml_nmlP;
      ntuw_read_xml_nmlP=0;
    }
  if(ntuw_read_packed_nmlP)
    {
      delete ntuw_read_packed_nmlP;
      ntuw_read_packed_nmlP=0;
    }
  if(ntuw_read_xdr_nmlP)
    {
      delete ntuw_read_xdr_nmlP;
      ntuw_read_xdr_nmlP=0;
    }
  nml_cleanup();
}

int main(int argc, const char **argv)
{
  if(argc != 5)
    {
      fprintf(stderr,"nml_test_unbounded_usage: buf proc cfgfile lastvar\n");
      exit(1);
    }

  ntuw_nmlP = new NML(nml_test_unbounded_format,
		      argv[1],argv[2],argv[3]);
  if(!ntuw_nmlP->valid())
    {
      close_nml();
      exit(1);
    }

  ntuw_read_xml_nmlP = new NML(nml_test_unbounded_format,0,0,0);
  ntuw_read_packed_nmlP = new NML(nml_test_unbounded_format,0,0,0);
  ntuw_read_xdr_nmlP = new NML(nml_test_unbounded_format,0,0,0);
  nml_start();

  int lastvar = atoi(argv[4]);

  NML_TEST_UNBOUNDED_MSG ntu_test_msg;
  
  ntu_test_msg.lastvar = lastvar;
  
  if(ntuw_nmlP->write(ntu_test_msg) < 0)
    {
      close_nml();
      exit(1);
    }

  const char *XML_MSG_FILE1_NAME="ntu_test_msg1.xml";
  if(ntuw_nmlP->xmlMsgSaveAs(ntu_test_msg, XML_MSG_FILE1_NAME) < 0)
    {
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu_xml1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntuw_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE1_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml1_msgP,0,0))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml1_msgP) failed for %s\n",
	      XML_MSG_FILE1_NAME);
      close_nml();
      exit(1);
    }

      
  const char *PACKED_MSG_FILE1_NAME="ntu_test_msg1.packed";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_PACKED_ENCODING,
				 PACKED_MSG_FILE1_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_packed1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE1_NAME,
						       CMS_PACKED_ENCODING);

  if(!check_test_unbounded_msg(ntu_packed1_msgP,0,0))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml1_msgP) failed for %s\n",
	      XML_MSG_FILE1_NAME);
      close_nml();
      exit(1);
    }


  const char *XDR_MSG_FILE1_NAME="ntu_test_msg1.xdr";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_XDR_ENCODING,
				 XDR_MSG_FILE1_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_xdr1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE1_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr1_msgP,0,0))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml1_msgP) failed for %s\n",
	      XML_MSG_FILE1_NAME);
      close_nml();
      exit(1);
    }


  
  realloc_ntu(&ntu_test_msg,10,10);

  if(ntuw_nmlP->write(ntu_test_msg) < 0)
    {
      close_nml();
      exit(1);
    }

  const char *XML_MSG_FILE2_NAME="ntu_test_msg2.xml";
  if(ntuw_nmlP->xmlMsgSaveAs(ntu_test_msg, XML_MSG_FILE2_NAME) < 0)
    {
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu_xml2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntuw_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE2_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml2_msgP,10,10))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml2_msgP) failed for %s\n",
	      XML_MSG_FILE2_NAME);
      close_nml();
      exit(1);
    }

  const char *PACKED_MSG_FILE2_NAME="ntu_test_msg2.packed";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_PACKED_ENCODING,
				 PACKED_MSG_FILE2_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_packed2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE2_NAME,
						       CMS_PACKED_ENCODING);

  if(!check_test_unbounded_msg(ntu_packed2_msgP,10,10))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_packed2_msgP) failed for %s\n",
	      PACKED_MSG_FILE2_NAME);
      close_nml();
      exit(1);
    }


  const char *XDR_MSG_FILE2_NAME="ntu_test_msg2.xdr";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_XDR_ENCODING,
				 XDR_MSG_FILE2_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_xdr2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE2_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr2_msgP,10,10))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xdr2_msgP) failed for %s\n",
	      XDR_MSG_FILE2_NAME);
      close_nml();
      exit(1);
    }


  realloc_ntu(&ntu_test_msg,300,20);

  if(ntuw_nmlP->write(ntu_test_msg) < 0)
    {
      close_nml();
      exit(1);
    }

  const char *XML_MSG_FILE3_NAME="ntu_test_msg3.xml";
  if(ntuw_nmlP->xmlMsgSaveAs(ntu_test_msg, XML_MSG_FILE3_NAME) < 0)
    {
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu_xml3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntuw_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE3_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml3_msgP,300,20))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml3_msgP) failed for %s\n",
	      XML_MSG_FILE3_NAME);
      close_nml();
      exit(1);
    }

  const char *PACKED_MSG_FILE3_NAME="ntu_test_msg3.packed";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_PACKED_ENCODING,
				 PACKED_MSG_FILE3_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_packed3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE3_NAME,
						       CMS_PACKED_ENCODING);

  if(!check_test_unbounded_msg(ntu_packed3_msgP,300,20))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_packed3_msgP) failed for %s\n",
	      PACKED_MSG_FILE3_NAME);
      close_nml();
      exit(1);
    }


  const char *XDR_MSG_FILE3_NAME="ntu_test_msg3.xdr";
  ntuw_nmlP->write_encoded_data_to_file(&ntu_test_msg,
				 CMS_XDR_ENCODING,
				 XDR_MSG_FILE3_NAME);

  NML_TEST_UNBOUNDED_MSG *ntu_xdr3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntuw_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE3_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr3_msgP,300,20))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xdr3_msgP) failed for %s\n",
	      XDR_MSG_FILE3_NAME);
      close_nml();
      exit(1);
    }

  close_nml();
  free_ntu(&ntu_test_msg);

  printf("nml_test_unbounded_write %s %s %s %s finished OK.\n",
	 argv[1],argv[2],argv[3],argv[4]);
  exit(0);
}

