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



#ifndef CMS_DATETIME_HH
#define CMS_DATETIME_HH

struct CMS_DURATION
{
  long years;
  long months;
  long days;
  long hours;
  long minutes;
  double seconds;
};

struct CMS_DATE_TIME
{
  //  CMS_DATE_TIME() { years=2000; months=1;days=1;hours=1;minutes=0;seconds=0;};
  long years;
  long months;
  long days;
  long hours;
  long minutes;
  double seconds;
  int timezoneoffsethours;
};

void get_current_cms_date_time(struct CMS_DATE_TIME *dt);

struct CMS_TIME 
{
  int hours;
  int minutes;
  double seconds;
  int timezoneoffsethours;
};

void get_current_cms_time(struct CMS_TIME  *t);

struct CMS_DATE 
{
  long years;
  long months;
  long days;
};

void get_current_cms_date(struct CMS_DATE *d);

// CMS_DATETIME_HH
#endif
