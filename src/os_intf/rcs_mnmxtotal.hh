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

#ifndef RCS_MNMMXTOTAL_H
#define RCS_MNMMXTOTAL_H

#ifdef __cplusplus

class RCS_MNMXTOTAL
{
public:

  // Don't initialize min max using parenthesis since it can conflict with
  // macros. 
  // -- modified based on Rcslib_Fixes_for_Errors.doc, Rashmi Patel,GDRS
  RCS_MNMXTOTAL()
  {
    min=1E15;
    max=-1E15;
    total=0;
    count=0;
  };

  double min;
  double max;
  double total;
  long count;

  inline void add_value(double value)
  {
    if(count < 1)
      {
	count = 1;
	total = value;
	min = value;
	max = value;
      }
    else
      {
	count++;
	total += value;
	if(min > value)
	  {
	    min = value;
	  }
	if(max < value)
	  {
	    max = value;
	  }
      }
  }
  
  inline double avg()
  {
    if(count > 0)
      {
	return total/count;
      }
    else
      {
	return -999.999;
      }
  }

  inline const RCS_MNMXTOTAL &operator+=(double value_to_add)
  {
    add_value(value_to_add);
    return(*this);
  }

};

// __cplusplus
#endif

// RCS_MNMXTOTAL
#endif

