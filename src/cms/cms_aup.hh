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



#ifndef CMS_AUP_HH
#define CMS_AUP_HH

#include "cms_up.hh"		/* class CMS_UPDATER */

class CMS_ASCII_UPDATER:public CMS_UPDATER
{
public:
  CMS_STATUS update (char &x);
  CMS_STATUS update (unsigned char &x);
  CMS_STATUS update (short int &x);
  CMS_STATUS update (unsigned short int &x);
  CMS_STATUS update (int &x);
  CMS_STATUS update (unsigned int &x);
  CMS_STATUS update (long int &x);
  CMS_STATUS update (unsigned long int &x);
  CMS_STATUS update (float &x);
  CMS_STATUS update (double &x);
  CMS_STATUS update (long double &x);
  CMS_STATUS update (char *x, unsigned int len);
  CMS_STATUS update (unsigned char *x, unsigned int len);
  CMS_STATUS update (short *x, unsigned int len);
  CMS_STATUS update (unsigned short *x, unsigned int len);
  CMS_STATUS update (int *x, unsigned int len);
  CMS_STATUS update (unsigned int *x, unsigned int len);
  CMS_STATUS update (long *x, unsigned int len);
  CMS_STATUS update (unsigned long *x, unsigned int len);
  CMS_STATUS update (float *x, unsigned int len);
  CMS_STATUS update (double *x, unsigned int len);
  CMS_STATUS update (long double *x, unsigned int len);
  int set_mode (CMS_UPDATER_MODE);
  void rewind ();
  int get_encoded_msg_size ();
protected:
  int check_pointer (char RCS_HUGE *, long);
    CMS_ASCII_UPDATER (CMS *);
    virtual ~ CMS_ASCII_UPDATER ();
  friend class CMS;
  char *begin_current_string;
  char *end_current_string;
  long max_length_current_string;
  long length_current_string;
  int warning_count;
  int warning_count_max;
private:
  CMS_ASCII_UPDATER(const CMS_ASCII_UPDATER &);
  CMS_ASCII_UPDATER &operator=(onst CMS_ASCII_UPDATER &);
};



#endif
