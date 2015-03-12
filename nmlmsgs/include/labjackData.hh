/* 
This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST RCS intelligent mobility software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.
*/ 

/************************************************************************\
 * DISCLAIMER:                                                          *
 * This software was produced by the National Institute of Standards    *
 * and Technology (NIST), an agency of the U.S. government, and by      *
 * statute is not subject to copyright in the United States.            *
 * Recipients of this software assume all responsibility associated     *
 * with its operation, modification, maintenance, and subsequent        *
 * redistribution.                                                      *
 *                                                                      *
 * See NIST Administration Manual 4.09.07 b and Appendix I.             *
 * Author : Peter Russo                                                 *
\************************************************************************/

#ifndef LABJACKDATA_HH
#define LABJACKDATA_HH

#include "rcs.hh"

#include "nmlOffsets.hh"

enum enumLabjackDataNmlType {
  LABJACK_DATA_TYPE=LABJACK_DATA_BASE,
  LABJACK_WRITE_DIGITAL_OUT_MSG_TYPE,
};

#define LABJACK_DATA_NAME "labjackData"



#define LABJACK_DATA_REV "$Rev: 783 $"
#define LABJACK_DATA_ID "$Id: labjackData.hh 783 2009-10-16 18:14:04Z shackle $"

#if !defined(__attribute__) && !defined(__GNUC__)
#define __attribute__(X) 
#endif

static const char __attribute__((unused)) *LABJACK_DATA_HEADER_FILE=__FILE__;

struct DigitalOutput 
{
  long channel;
  bool state;
};

struct DigitalOutputFrame {
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(struct DigitalOutput,do_dla,20);
};

#define MAX_LJ_CHANNELS 16

/*!
	\class LABJACK_DATA
	\brief Stores data from a muchameleons (A usb device with analog-to-diginal converters and other misc I/O 
*/
class LABJACK_DATA  : public NMLmsg
{
public:
  LABJACK_DATA();
  LABJACK_DATA(NMLTYPE t, size_t s): NMLmsg(t,s){};

  void update( CMS*);

  struct time_tracker tt;
  double ain_vals[MAX_LJ_CHANNELS];
  bool is_input[MAX_LJ_CHANNELS];
  int write_count[MAX_LJ_CHANNELS];
  struct DigitalOutputFrame writenOutputs;  
};


class LABJACK_WRITE_DIGITAL_OUT_MSG : public NMLmsg
{
public:
  LABJACK_WRITE_DIGITAL_OUT_MSG();
  LABJACK_WRITE_DIGITAL_OUT_MSG(NMLTYPE t, size_t s): NMLmsg(t,s){};

  void update(CMS *);
  struct DigitalOutputFrame outputs_to_write;  
};


int labjackData_format(NMLTYPE type, void *buf, CMS *cms);

#endif
