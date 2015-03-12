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

/*****************************************************************************
  DISCLAIMER:
  This software was produced by the National Institute of Standards
  and Technology (NIST), an agency of the U.S. government, and by statute is
  not subject to copyright in the United States.  Recipients of this software
  assume all responsibility associated with its operation, modification,
  maintenance, and subsequent redistribution.

  See NIST Administration Manual 4.09.07 b and Appendix I. 
*****************************************************************************/
/*!
  \file   sickLMS.hh
  \brief  Define interface to SICK LMS 

  <long description>

  \code CVS Status:
  $Author: tchang $
  $Revision: 758 $
  $Date: 2006-10-17 15:51:50 -0400 (Tue, 17 Oct 2006) $
  \endcode
  \par CVS Log:
  \ref sickLMS_hh "More..."

  \author Tommy Chang
  \date   2001-08-08
*/


#ifndef SICKLMS_HH
#define SICKLMS_HH

#include <pthread.h>
#include <semaphore.h>
#include <termios.h>

//! SICK LMS module.
class SICKLMS {
public:
  SICKLMS(bool noThread_flg=false, int useFIFO=0);
  ~SICKLMS();
  void stop();

  int start(char *, int=180, int=100, int=B500000, int=0);
  float *getScan (long *scanNum=NULL, unsigned char *intensities_uchrA=NULL);
  float *getNewestScan();
  int changeUnit(char, char=1);
  int isRunning() { return running; } //!< Returns running status.
  int isNewData() { return has_new_data; } //!< Test to see if data is new.

  void setFIFO () {useFIFO = 1;}
  void unsetFIFO () {useFIFO = 0;}
  int getFIFO () {return useFIFO;}
  
private:
  int useFIFO;
  int sickfd;      //! serial port file pointer
  unsigned char configBuffer[50];  //! for configuration purpose only
  unsigned char buffer[4500];  //! where data is stored, assume max 721
                               //points *quat reso at 180 deg, in reality
                               //quat reso only work at 100 deg.
  unsigned int dataLength;  // number of samples
  int targetBaud;
  int test_speed;               // test scan data rate
  bool noThread_flg;		// indicate no thread version
  
  unsigned int createCRC(unsigned char *CommData, unsigned int uLen);
  void sendFrame(unsigned char, int);
  int setSerial(int, int, unsigned char );

  void getSerialData(int=0);
  unsigned char* getDataBuffer(int);
  int getResponse();
  int init (unsigned short &, int, int, int);
  void configParam(unsigned char *);
  void configParam2(unsigned char *);
  void configParam3(unsigned char *);
  void configParam4(unsigned char *);

  float *scan_points;    
  pthread_t data_monitor_id;    


  char unit_chr; // 0: [cm] (80 m max), 1: [mm] (8 m max), 2: [dm] (150 m max) 
  void getUnitAndIntensity();
  bool hasIntensity_flg;

  // thread variables needs to be static 
  static void *monitor_data(void *);   
  static pthread_mutex_t data_ready_mutex;
  static sem_t semaphore;  
  static unsigned short telegram_len;    
  static int running;            
  static int has_new_data;        
  static int read_idx;            
  static int last_write_idx;
  static int cur_write_idx;
  static int pauseSICKdata;  
  
  static int request_newestdata;
  static int newestdata_idx;
  static long scanNum_long;     // sequential tag of each scan
};

#endif
