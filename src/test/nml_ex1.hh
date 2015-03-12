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

/* nml_ex1.hh */
#ifndef NML_EX1_HH
#define NML_EX1_HH
#include "rcs.hh"

/* Give the new structure a unique id number */
#define EXAMPLE_MSG_TYPE 101
/* The id number must be unique within a CMS buffer, i.e. the number
must be different than the id of any other type that might be written to
a particular buffer. For simplicity it is recommended that the id number
also be unique within an application. */

/* Define the new message structure */
struct EXAMPLE_MSG: public NMLmsg 
{
  /* The constructor needs to store the id number  */
  /* and the size of the new structure */
  /* by passing them as arguments to the base class constructor. */
  EXAMPLE_MSG(): 
    NMLmsg(EXAMPLE_MSG_TYPE, sizeof(EXAMPLE_MSG)){};

  /* Each new type needs to overload the update function. */
  void update(CMS *cms); 

  /* Data in this new message format. */
  double d;
  float f;
  char c;
  short s;
  int i;
  long l;
  unsigned char uc;
  unsigned short us;
  unsigned int ui;
  unsigned long ul;


  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, da, 20);
};

/* Declare the NML Format function. */
int ex_format(NMLTYPE type, void *buf, CMS *cms);

extern char *TEST_CFG;

#endif /* End of NML_EMC_HH */
