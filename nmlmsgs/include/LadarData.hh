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

#ifndef __INCladarhh
#define __INCladarhh

#include "rcs.hh"

#define LADAR_DATA_BASE 50		// INCLUDE BASE_TYPES.H INSTEAD WHEN MOVED ONTO ANOTHER PLATFORM

enum enumLadarDataNmlType {
  LADAR_DATA_TYPE=LADAR_DATA_BASE,
  LADAR_DATA_SPLIT_XYZ_TYPE
};

#define LADAR_DATA_NAME "ladardata"

#define LADAR_SIZE_MAX 640*480		//!< LADAR_SIZE_MAX is the maximum number of pixels that a ladar may contain

enum LadarTypeEnum
  {
    LADAR_SR2=0,			//!< The type enumeration for the CSEM Swissranger 2
    LADAR_CANESTA=1,			//!< The type enumeration for any of the Canesta ladars
    LADAR_PMD=2,			//!< The type enumeration for any of the PMD ladars
    LADAR_SR3000=3,			//!< The type enumeration for the CSEM Swissranger 3000
    LADAR_LOG=4,                         //!< The type enumeration for logged data.  This is a type to allow commands for pausing/playing logged data
    LADAR_UNKNOWN
  };			

/**
 * Structure used to store as single return points 3D coordinates.
 */
struct xyz_float_struct
{
  //! distance along an axis across the image horizontally
  float x; 

  //! distance along an axis across the image vertically
  float y; 

  //! distance along the axis the FLASH Ladar Camera is pointing into the image.
  float z; 
};

struct xyz_frame
{
 //! Array of 3D data in camera coordinates.  Data is packed x1,y1,z1,x2,y2,z2,...xn,yn,zn
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(xyz_float_struct,xyz_data,LADAR_SIZE_MAX);
};

struct float_frame
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,fdata,LADAR_SIZE_MAX);
};

struct ushort_frame
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned short,usdata,LADAR_SIZE_MAX);
};

struct LadarOptionalInfo
{
  //! sensors internal frame count if available. 0 otherwise
  unsigned frame_count;

  //! Itegration time in microseconds if available.  0 otherwise
  unsigned  integration_time_microsec; 
  
  //! number of frames per second if available, 0 otherwise
  double frame_rate; 

  //! modulation frequency in MHz.
  unsigned modulation_frequency_MHz;
  
  double distanceOffset;
  double amplitudeThreshold;

  //! optional ladar specific meaning
  int mode; 

  //! shutter time in microseconds if available, 0 otherwise
  double shutterTime; 

  //! second shutter time in microseconds if available, 0 otherwise
  double shutterTime2; 

  //! ambient light cancellation CMR count, -- meanint is LADAR specific
  int cmr1; 

  //! ambient light cancellation CMR count, -- meanint is LADAR specific
  int cmr2; 

  //! unique string identifier/serial number for this device if available.
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, serial_number_string, 80);
};


#define LADAR_DATA_REV "$Rev: 783 $"
#define LADAR_DATA_ID "$Id: LadarData.hh 783 2009-10-16 18:14:04Z shackle $"

#if !defined(__attribute__) && !defined(__GNUC__)
#define __attribute__(X) 
#endif

static const char __attribute__((unused)) *LADAR_DATA_HEADER_FILE=__FILE__;

/*!
	\class LADAR_DATA
	\brief Stores data from [num_ladars] ladars to be distributed with NML
*/
class LADAR_DATA  : public NMLmsg
{
public:
  LADAR_DATA();
  LADAR_DATA(NMLTYPE t, size_t s): NMLmsg(t,s){};

  void update( CMS*);

  //! Message Info
  int Seqno;

  // Ladar Type
  enum LadarTypeEnum ladar_type;

  // TimeStamp returned with etime(). measured in seconds, generally precise to 0.000001 s.
  double timeStamp;
  
  // Ladar Dimensional Info
  int rows;			//!< Stores the number of rows in each ladar
  int cols;			//!< Stores the number of columns in each ladar
  float fov_r;			//!< Stores the field of view in the row direction for each ladar
  float fov_c;			//!< Stores the field of view in the column direction for each ladar
  
  struct LadarOptionalInfo opt; //!< Configuration options in effect when frame a was collected.

  struct xyz_frame xyz_frame;  //!<  3-Dimensional coords of each return point in frame.

  struct float_frame intensity_frame; //!< Intensity value for each point in frame.

  struct float_frame range_frame; //!< Detected range values 

  struct ushort_frame objects_frame; //! Object information for frame if provided.

};


/*!
	\class LADAR_DATA
	\brief Stores data from [num_ladars] ladars to be distributed with NML
*/
class LADAR_DATA_SPLIT_XYZ  : public LADAR_DATA
{
public:
  LADAR_DATA_SPLIT_XYZ();
  void update( CMS*);

  struct time_tracker tt;

  struct float_frame x_frame;  //!<  X axis val of 3-Dimensional coords of each return point in frame.

  struct float_frame y_frame;  //!<  X axis val of 3-Dimensional coords of each return point in frame.

  struct float_frame z_frame;  //!<  X axis val of 3-Dimensional coords of each return point in frame.

};

int LadarData_format(NMLTYPE type, void *buf, CMS *cms);

#endif
