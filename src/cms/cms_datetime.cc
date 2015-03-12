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


#ifdef HAVE_CONFIG_H
#include "rcs_config.h"
#include "rcs_config_include.h"
#else
#ifndef HAVE_TIME
#define HAVE_TIME 1
#endif
#ifndef HAVE_LOCALTIME
#define HAVE_LOCALTIME 1
#endif
#include <time.h>
#include <stdio.h>
#include <errno.h>
#endif

#include "cms_datetime.hh"
#include "rcs_prnt.hh"

void get_current_cms_date_time(struct CMS_DATE_TIME  *dt)
{
#if HAVE_TIME &&  HAVE_LOCALTIME
  struct tm *tm_ptr;
  time_t time_returned_from_time;
  time_t time_storage_var;

  if(!dt)
    {
      return;
    }


  tm_ptr=0;
  time_returned_from_time = time(&time_storage_var);
  if(((time_t)-1) == time_returned_from_time )
    {
      perror("time:");
      return;
    }
  tm_ptr = localtime(&time_returned_from_time);

  if(!tm_ptr)
    {
      return;
    }
  dt->seconds = tm_ptr->tm_sec;
  dt->minutes = tm_ptr->tm_min;
  dt->hours = tm_ptr->tm_hour;
  dt->days = tm_ptr->tm_mday;
  dt->months = tm_ptr->tm_mon+1;
  dt->years = tm_ptr->tm_year +1900;

  //HAVE_TIME && HAVE_LOCALTIME
#else
#warning "Without time or localtime functions get_current_cms_date_time will not be implemented"
  rcs_print_error("get_current_cms_date_time NOT IMPLEMENTED on this PLATFORM.\n");
#endif

}


void get_current_cms_date(struct CMS_DATE  *d)
{
#if HAVE_TIME && HAVE_LOCALTIME
  struct tm *tm_ptr;
  time_t time_returned_from_time;
  time_t time_storage_var;
  
  if(!d)
    {
      return;
    }


  tm_ptr=0;
  time_returned_from_time = time(&time_storage_var);
  if(((time_t)-1) == time_returned_from_time )
    {
      perror("time:");
      return;
    }
  tm_ptr = localtime(&time_returned_from_time);

  if(!tm_ptr)
    {
      return;
    }
  d->days = tm_ptr->tm_mday;
  d->months = tm_ptr->tm_mon+1;
  d->years = tm_ptr->tm_year +1900;

  //HAVE_TIME && HAVE_LOCALTIME
#else
#warning "Without time or localtime functions get_current_cms_date will not be implemented"
  rcs_print_error("get_current_cms_date NOT IMPLEMENTED on this PLATFORM.\n");
#endif

}


void get_current_cms_time(struct CMS_TIME  *t)
{
#if HAVE_TIME && HAVE_LOCALTIME

  struct tm *tm_ptr;
  time_t time_returned_from_time;
  time_t time_storage_var;

  if(!t)
    {
      return;
    }


  tm_ptr=0;
  time_returned_from_time = time(&time_storage_var);
  if(((time_t)-1) == time_returned_from_time )
    {
      perror("time:");
      return;
    }
  tm_ptr = localtime(&time_returned_from_time);

  if(!tm_ptr)
    {
      return;
    }
  t->seconds = tm_ptr->tm_sec;
  t->minutes = tm_ptr->tm_min;
  t->hours = tm_ptr->tm_hour;

  //HAVE_TIME && HAVE_LOCALTIME
#else
#warning "Without time or localtime functions get_current_cms_time will not be implemented"
  rcs_print_error("get_current_cms_time NOT IMPLEMENTED on this PLATFORM.\n");
#endif
}
