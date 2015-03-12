/**
 * @file sicksafetyData.hh
 *
 *  @remark ~ NML data structure for SICK SAFETY 3000.
 *
 *  @author ~ Tommy Chang
 *
 *  @verbatim
 *  ,--------------------------------------------------------------------.
 *  . THIS software was developed at the National Institute of           .
 *  . Standards and Technology by employees of the Federal Government    .
 *  . in the course of their official duties. Pursuant to title 17       .
 *  . Section 105 of the United States Code this software is not         .
 *  . subject to copyright protection and is in the public               .
 *  . domain. NIST's [intelligent mobility] software is an experimental  .
 *  . system. NIST assumes no responsibility whatsoever for its use by   .
 *  . other parties, and makes no guarantees, expressed or implied,      .
 *  . about its quality, reliability, or any other characteristic. We    .
 *  . would appreciate acknowledgement if the software is used. This     .
 *  . software can be redistributed and/or modified freely provided      .
 *  . that any derivative works bear some notice that they are derived   .
 *  . from it, and any modified versions bear some notice that they      .
 *  . have been modified.                                                .
 *   `-------------------------------------------------------------------
 *  @endverbatim
 *
 *  @par Created by Tommy Chang, 2007-04-04 
 **/


#ifndef SICKSAFETYDATA_HH
#define SICKSAFETYDATA_HH
/*----------------------------------*
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include "rcs.hh"               // Common RCS definitions
#include "nmlOffsets.hh"        // SICKSAFETY_DATA_BASE


/*-----------------*
 * Macro Constants *
 \*---------------*/
#define SAFETY_SICK_S3000_RANGE_LEN 761 // 190 * 4 + 1
#define SAFETY_SICK_S3000_XYZ_LEN (761 * 3) 
#define SAFETY_SICK_S3000_DATA_TYPE SICKSAFETY_DATA_BASE

#define SAFETY_SICK_S3000_DATA_ID "$Id: sicksafetyData.hh 740 2009-08-03 17:17:16Z shackle $"
#define SAFETY_SICK_S3000_DATA_REV "$Rev: 740 $"

#ifdef LOG_DATA_FORMAT
static const char *SAFETY_SICK_S3000_DATA_HEADER_FILE=__FILE__;
#endif

/*-----------------*
 * Data Structures *
 \*---------------*/
class SAFETY_SICK_S3000_DATA : public NMLmsg
{
public:
  SAFETY_SICK_S3000_DATA();
  void update(CMS *);
  double time;                  // time-stamp 
  int nSamples;                 // number of pulses / samples (i.e. 761)

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY (float, 
                                    range_m, 
                                    SAFETY_SICK_S3000_RANGE_LEN); // in [m]
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY (float, 
                                    xyz_m, 
                                    SAFETY_SICK_S3000_XYZ_LEN); // in [m]
};



/*--------------------*
 * Function Prototype *
 \*------------------*/
int sicksafetyData_format (NMLTYPE type, void *buf, CMS *cms);


#endif // SICKSAFETYDATA_HH
