/*
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*/

#ifndef SKEL_INTF_HH
#define SKEL_INTF_HH

#include <rcs.hh>		// NMLmsg, NMLTYPE, CMS
#include "skel.hh"		// SKEL_ERR_BASE

#define SKEL_ERR_TYPE (SKEL_ERR_BASE + 1)
#define SKEL_MSG_TYPE (SKEL_ERR_BASE + 2)

#define SKEL_ERR_LEN 256
#define SKEL_MSG_LEN 256

class SkelErr : public NMLmsg {
public:
  SkelErr() : NMLmsg(SKEL_ERR_TYPE, sizeof(SkelErr)) {};
  void update(CMS *);

  double timestamp;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, text, SKEL_ERR_LEN);
};

class SkelMsg : public NMLmsg {
public:
  SkelMsg() : NMLmsg(SKEL_MSG_TYPE, sizeof(SkelMsg)) {};
  void update(CMS *);

  double timestamp;
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, text, SKEL_MSG_LEN);
};

// from NML code generator
extern int skelIntf_format(NMLTYPE type, void *buffer, CMS *cms);
extern const char *skelIntf_symbol_lookup(long type);

/*
  Modification history:

  $Log: skelIntf.hh,v $
  Revision 1.2  2006/01/09 14:45:48  proctor
  Reworked admin states to conform to Tom K's original proposal, in which
  commands out-of-protocol invoke their state tables with an RCS_ERROR result.

  Revision 1.1  2006/01/05 20:51:25  proctor
  Initial checkin

*/

#endif
