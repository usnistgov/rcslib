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


#ifndef CMS_XUP_HH
#define CMS_XUP_HH

#include "cms_up.hh"		/* class CMS_UPDATER */

class CMS_XDR_UPDATER:public CMS_UPDATER
{
public:
  int get_pos();
  enum CMS_STATUS update (bool &x);
  enum CMS_STATUS update (char &x);
  enum CMS_STATUS update (unsigned char &x);
  enum CMS_STATUS update (short int &x);
  enum CMS_STATUS update (unsigned short int &x);
  enum CMS_STATUS update (int &x);
  enum CMS_STATUS update (unsigned int &x);
  enum CMS_STATUS update (long int &x);
  enum CMS_STATUS update (unsigned long int &x);
  enum CMS_STATUS update (float &x);
  enum CMS_STATUS update (double &x);
  enum CMS_STATUS update (long double &x);
  enum CMS_STATUS update (bool *x, unsigned int len);
  enum CMS_STATUS update (char *x, unsigned int len);
  enum CMS_STATUS update (unsigned char *x, unsigned int len);
  enum CMS_STATUS update (short *x, unsigned int len);
  enum CMS_STATUS update (unsigned short *x, unsigned int len);
  enum CMS_STATUS update (int *x, unsigned int len);
  enum CMS_STATUS update (unsigned int *x, unsigned int len);
  enum CMS_STATUS update (long *x, unsigned int len);
  enum CMS_STATUS update (unsigned long *x, unsigned int len);
  enum CMS_STATUS update (float *x, unsigned int len);
  enum CMS_STATUS update (double *x, unsigned int len);
  enum CMS_STATUS update (long double *x, unsigned int len);

  enum CMS_STATUS update (CMS_DURATION & d)
  {return CMS_UPDATER::update(d);};

  enum CMS_STATUS update (CMS_DURATION * d, int len)
  {return CMS_UPDATER::update(d,len);};
  
  
  enum CMS_STATUS update (CMS_DATE_TIME & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_DATE_TIME * d, int len)
  {return CMS_UPDATER::update(d,len);};


  enum CMS_STATUS update (CMS_TIME & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_TIME * d, int len)
  {return CMS_UPDATER::update(d,len);};
  

  enum CMS_STATUS update (CMS_DATE & d)
  {return CMS_UPDATER::update(d);};
  enum CMS_STATUS update (CMS_DATE * d, int len)
  {return CMS_UPDATER::update(d,len);};
  
  int set_mode (CMS_UPDATER_MODE);
  void rewind ();
  int get_encoded_msg_size ();
  void set_encoded_data (void *, long _encoded_data_size);
  CMS_XDR_UPDATER (CMS *);
  virtual ~ CMS_XDR_UPDATER ();
  friend class CMS;

protected:
  int check_pointer (char *, long);
  void *encode_data_stream;	/* XDR streams for data */
  void *decode_data_stream;
  void *encode_header_stream;	/* XDR streams for header */
  void *decode_header_stream;
  void *encode_queuing_header_stream;	/* XDR streams for header */
  void *decode_queuing_header_stream;
  void *current_stream;
  int xdrmem_size;

private:
  CMS_XDR_UPDATER(const CMS_XDR_UPDATER &);
  CMS_XDR_UPDATER &operator=(const CMS_XDR_UPDATER &);
};


// CMS_XUP_HH
#endif
