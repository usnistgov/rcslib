

#include "nml_test_unbounded_format.hh"
#include "check_test_unbounded_msg.hh"

#include <stdio.h>
#include <stdlib.h>

NML *ntur_nmlP=0;
NML *ntur_read_xml_nmlP=0;
NML *ntur_read_packed_nmlP=0;
NML *ntur_read_xdr_nmlP=0;

static void close_nml()
{
  if(ntur_nmlP)
    {
      delete ntur_nmlP;
      ntur_nmlP=0;
    }
  if(ntur_read_xml_nmlP)
    {
      delete ntur_read_xml_nmlP;
      ntur_read_xml_nmlP=0;
    }
  if(ntur_read_packed_nmlP)
    {
      delete ntur_read_packed_nmlP;
      ntur_read_packed_nmlP=0;
    }
  if(ntur_read_xdr_nmlP)
    {
      delete ntur_read_xdr_nmlP;
      ntur_read_xdr_nmlP=0;
    }
  nml_cleanup();
}

#ifdef CHECK_FILES
static void 
check_files()
{
  const char *XML_MSG_FILE1_NAME="ntu_test_msg1.xml";
  NML_TEST_UNBOUNDED_MSG *ntu_xml1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntur_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE1_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml1_msgP,0,0))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml1_msgP) failed for %s\n",
	      XML_MSG_FILE1_NAME);
      close_nml();
      exit(1);
    }

      
  const char *PACKED_MSG_FILE1_NAME="ntu_test_msg1.packed";
  NML_TEST_UNBOUNDED_MSG *ntu_packed1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE1_NAME,
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
  NML_TEST_UNBOUNDED_MSG *ntu_xdr1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE1_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr1_msgP,0,0))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml1_msgP) failed for %s\n",
	      XML_MSG_FILE1_NAME);
      close_nml();
      exit(1);
    }



  const char *XML_MSG_FILE2_NAME="ntu_test_msg2.xml";
  NML_TEST_UNBOUNDED_MSG *ntu_xml2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntur_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE2_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml2_msgP,10,10))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml2_msgP) failed for %s\n",
	      XML_MSG_FILE2_NAME);
      close_nml();
      exit(1);
    }

  const char *PACKED_MSG_FILE2_NAME="ntu_test_msg2.packed";
  NML_TEST_UNBOUNDED_MSG *ntu_packed2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE2_NAME,
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
  NML_TEST_UNBOUNDED_MSG *ntu_xdr2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE2_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr2_msgP,10,10))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xdr2_msgP) failed for %s\n",
	      XDR_MSG_FILE2_NAME);
      close_nml();
      exit(1);
    }

  const char *XML_MSG_FILE3_NAME="ntu_test_msg3.xml";
  NML_TEST_UNBOUNDED_MSG *ntu_xml3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) ntur_read_xml_nmlP->readMsgFromXmlFile(XML_MSG_FILE3_NAME);
  
  if(!check_test_unbounded_msg(ntu_xml3_msgP,300,20))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xml3_msgP) failed for %s\n",
	      XML_MSG_FILE3_NAME);
      close_nml();
      exit(1);
    }

  const char *PACKED_MSG_FILE3_NAME="ntu_test_msg3.packed";
  NML_TEST_UNBOUNDED_MSG *ntu_packed3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_packed_nmlP->read_encoded_data_from_file(PACKED_MSG_FILE3_NAME,
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
  NML_TEST_UNBOUNDED_MSG *ntu_xdr3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *) 
    ntur_read_xdr_nmlP->read_encoded_data_from_file(XDR_MSG_FILE3_NAME,
						       CMS_XDR_ENCODING);

  if(!check_test_unbounded_msg(ntu_xdr3_msgP,300,20))
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu_xdr3_msgP) failed for %s\n",
	      XDR_MSG_FILE3_NAME);
      close_nml();
      exit(1);
    }

}
#endif


int main(int argc, const char **argv)
{
  if(argc != 5)
    {
      fprintf(stderr,"nml_test_unbounded_usage: buf proc cfgfile lastvar\n");
      exit(1);
    }

  ntur_nmlP = new NML(nml_test_unbounded_format,
		      argv[1],argv[2],argv[3]);
  if(!ntur_nmlP->valid())
    {
      close_nml();
      exit(1);
    }

#ifdef CHECK_FILES
  ntur_read_xml_nmlP = new NML(nml_test_unbounded_format,0,0,0);
  ntur_read_packed_nmlP = new NML(nml_test_unbounded_format,0,0,0);
  ntur_read_xdr_nmlP = new NML(nml_test_unbounded_format,0,0,0);
#endif

  nml_start();

  int lastvar = atoi(argv[4]);
  NMLTYPE type_read=0;
  type_read = ntur_nmlP->read();
  if(type_read != NML_TEST_UNBOUNDED_MSG_TYPE)
    {
      fprintf(stderr,"read() returned %ld\n",
	      type_read);
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu1_msgP = 
    (NML_TEST_UNBOUNDED_MSG *)ntur_nmlP->get_address();
  if(!check_test_unbounded_msg(ntu1_msgP,0,0)) 
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu1_msgP) failed.\n");
      close_nml();
      exit(1);
    }

  if(ntu1_msgP->lastvar != lastvar)
    {
      fprintf(stderr,
	      "ntu1_msgP->lastvar(%d) != lastvar(%d)\n",
	      ntu1_msgP->lastvar,lastvar);
      close_nml();
      exit(1);
    }
      
  type_read = ntur_nmlP->read();
  if(type_read != NML_TEST_UNBOUNDED_MSG_TYPE)
    {
      fprintf(stderr,"read() returned %ld\n",
	      type_read);
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu2_msgP = 
    (NML_TEST_UNBOUNDED_MSG *)ntur_nmlP->get_address();
  if(!check_test_unbounded_msg(ntu2_msgP,10,10)) 
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu2_msgP) failed.\n");
      close_nml();
      exit(1);
    }

  if(ntu2_msgP->lastvar != lastvar)
    {
      fprintf(stderr,
	      "ntu2_msgP->lastvar(%d) != lastvar(%d)\n",
	      ntu2_msgP->lastvar,lastvar);
      close_nml();
      exit(1);
    }

  type_read = ntur_nmlP->read();
  if(type_read != NML_TEST_UNBOUNDED_MSG_TYPE)
    {
      fprintf(stderr,"read() returned %ld\n",
	      type_read);
      close_nml();
      exit(1);
    }

  NML_TEST_UNBOUNDED_MSG *ntu3_msgP = 
    (NML_TEST_UNBOUNDED_MSG *)ntur_nmlP->get_address();
  if(!check_test_unbounded_msg(ntu3_msgP,300,20)) 
    {
      fprintf(stderr,
	      "check_test_unbounded_msg(ntu3_msgP) failed.\n");
      close_nml();
      exit(1);
    }

  if(ntu3_msgP->lastvar != lastvar)
    {
      fprintf(stderr,
	      "ntu3_msgP->lastvar(%d) != lastvar(%d)\n",
	      ntu3_msgP->lastvar,lastvar);
      close_nml();
      exit(1);
    }

#ifdef CHECK_FILES
  check_files();
#endif

  close_nml();
  printf("nml_test_unbounded_read %s %s %s %s finished OK.\n",
	 argv[1],argv[2],argv[3],argv[4]);
  exit(0);
}

