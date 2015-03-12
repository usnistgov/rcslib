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
  \file   coordinate.hh
  \brief  Specify coordinate transformation routines.

  <long description>

  \code CVS Status:
  $Author: tchang $
  $Revision: 707 $
  $Date: 2006-10-16 14:15:15 -0400 (Mon, 16 Oct 2006) $
  \endcode
  \par CVS Log:
  \ref coordinate_hh "More..."

  \author Tommy Chang
  \date   2001-10-13
*/

#ifndef COORDINATE_HH
#define COORDINATE_HH

#include "simpleMatrix.hh"

void sickCoordInit (int nSamples);
void sickConvertFrame (float       *frame_xyz, 
                       const float *range_mPtr, 
                       const float *trans,
                       const float *rpy);

void sickConvertFrame (float       *frame_xyz, 
                       const float *range_mPtr, 
                       const float *trans,
                       const float *rpyA,
                       const float *rpyB); 

void convertFrameToFrame (float       *frame_xyz, 
                          const float *frameIn_xyz, 
                          const float *trans,
                          const float *rpy) ;

#endif


