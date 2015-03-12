/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1



#include "rcs.hh"		// NML

extern "C"
{

#include "bbd.h"

  int NMLInit ();
  int NMLReadInit ();
  int NMLWriteInit ();
  int NMLWrite ();
  int NMLRead ();
  int NMLExit ();
  int NMLReadExit ();
  int NMLWriteExit ();
  int BBDInit ();
  int BBDRead ();
  int BBDWrite ();
  int BBDExit ();
  int NBTest ();

}


struct bbd_struct
{
  int i;
  double d;
  char c;
};

#define NML_BBD_MSG_TYPE ((NMLTYPE) 101)

class NML_BBD_MSG:public NMLmsg
{
public:
  NML_BBD_MSG ():NMLmsg (NML_BBD_MSG_TYPE, sizeof (NML_BBD_MSG))
  {
  };

  void update (CMS *);

  bbd_struct internal_bbd;
};

int
nmlbbdFormat (NMLTYPE type, void *buf, CMS * cms)
{
  switch (type)
    {
    case NML_BBD_MSG_TYPE:
      ((NML_BBD_MSG *) buf)->update (cms);
      return 1;

    default:
      break;
    }
  return -1;
}

void
NML_BBD_MSG::update (CMS * cms)
{
  cms->update (internal_bbd.i);
  cms->update (internal_bbd.d);
  cms->update (internal_bbd.c);
};


NML *nml_read = NULL;
NML *nml_write = NULL;
BBD bbd_read = NULL;
BBD bbd_write = NULL;

int
NMLBBDNULL ()
{
  nml_read = NULL;
  nml_write = NULL;
  bbd_read = NULL;
  bbd_write = NULL;
}


int
NMLInit ()
{
  nml_read = NULL;
  nml_write = NULL;
  NMLWriteInit ();
  NMLReadInit ();
}

int
NMLReadInit ()
{
  nml_read = NULL;
  rcs_print ("NMLReadInit():\n");

  nml_read =
    new NML (nmlbbdFormat, "buf", "reader", "/users/shackle/rcslib/test.nml");

  if (NULL == nml_read)
    {
      return -1;
    }
  if (!nml_read->valid ())
    {
      return -1;
    }
  return 0;
}

int
NMLWriteInit ()
{
  nml_read = NULL;
  nml_write = NULL;

  rcs_print ("NMLWriteInit():\n");
  nml_write =
    new NML (nmlbbdFormat, "buf", "writer", "/users/shackle/rcslib/test.nml");

  if (NULL == nml_write)
    {
      return -1;
    }
  if (!nml_write->valid ())
    {
      return -1;
    }
  return 0;
}


int
NMLExit ()
{
  rcs_print ("NMLExit():\n");
  NMLReadExit ();
  NMLWriteExit ();
  return 0;
}

int
NMLReadExit ()
{
  rcs_print ("NMLReadExit():\n");
  if (NULL != nml_read)
    {
      delete nml_read;
      nml_read = NULL;
    }
  return 0;
}

int
NMLWriteExit ()
{
  rcs_print ("NMLWriteExit():\n");
  if (NULL != nml_write)
    {
      delete nml_write;
      nml_write = NULL;
    }
  return 0;
}

int
NMLRead ()
{
  rcs_print ("NMLRead():\n");
  if (NULL == nml_read)
    {
      rcs_print_error ("nml_read is NULL.\n");
      return -1;
    }

  NMLTYPE type = nml_read->read ();
  NML_BBD_MSG *bbd_msg = (NML_BBD_MSG *) nml_read->get_address ();

  rcs_print ("type = %d\n", type);
  if (NULL == bbd_msg)
    {
      rcs_print_error ("bbd_msg == NULL\n");
      return -1;
    }
  rcs_print ("i = %d\n", bbd_msg->internal_bbd.i);
  rcs_print ("d = %lf\n", bbd_msg->internal_bbd.d);
  return type;
}

int
NMLWrite ()
{
  rcs_print ("NMLWrite():\n");
  if (NULL == nml_write)
    {
      rcs_print_error ("nml_write is NULL.\n");
      return -1;
    }

  static int nml_write_count = 0;
  nml_write_count++;
  NML_BBD_MSG bbd_msg;
  bbd_msg.internal_bbd.i = nml_write_count;
  bbd_msg.internal_bbd.c = nml_write_count;
  bbd_msg.internal_bbd.d = nml_write_count;

  return nml_write->write (&bbd_msg);

}


int
BBDInit ()
{
  rcs_print ("BBDInit():\n");
  bbd_write = bbdConnect ("buf", BBD_MULT_WRITER, 0x100);

  bbd_read = bbdConnect ("buf", BBD_MULT_READER, 0x100);

  if (NULL == bbd_write)
    {
      return -1;
    }
  if (NULL == bbd_read)
    {
      return -1;
    }
  return 0;
}


int
BBDExit ()
{
  rcs_print ("BBDExit():\n");
  if (NULL != bbd_read)
    {
      bbdDelete (bbd_read);
      bbd_read = NULL;
    }
  if (NULL != bbd_write)
    {
      bbd_write = NULL;
    }
  return 0;
}

int
BBDRead ()
{
  rcs_print ("BBDRead():\n");
  if (NULL == bbd_read)
    {
      rcs_print_error ("bbd_read is NULL.\n");
      return -1;
    }

  bbd_struct bbd_msg;

  if (bbdRead (bbd_read, &bbd_msg, BBD_PLAIN_READ, NO_WAIT) != OK)
    {
      return -1;
    }

  rcs_print ("i = %d\n", bbd_msg.i);
  rcs_print ("d = %lf\n", bbd_msg.d);
  return 0;
}

int
BBDWrite ()
{
  rcs_print ("BBDWrite():\n");
  if (NULL == bbd_write)
    {
      rcs_print_error ("bbd_write is NULL.\n");
      return -1;
    }



  static int bbd_write_count = 0;
  bbd_write_count++;
  bbd_struct bbd_msg;
  bbd_msg.i = bbd_write_count;
  bbd_msg.c = bbd_write_count;
  bbd_msg.d = bbd_write_count;

  if (bbdWrite (bbd_write, &bbd_msg, BBD_PLAIN_WRITE, NO_WAIT) != OK)
    {
      return -1;
    }

  return 0;

}
