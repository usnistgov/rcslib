/**
 * @file fwCameraData_v2.hh
 *
 * <!--Description-->
 *  @remark: <What is the purpose of this file?>
 *
 *  @author: Tsai Hong
 *
 * <!--Disclaimer-->
 *  @verbatim
 *   ,---------------------------------------------------------------------
 *   | This software was developed at the National Institute of Standards  
 *   | and Technology by employees of the Federal Government in the course 
 *   | of their official duties. Pursuant to title 17 Section 105 of the   
 *   | United States Code this software is not subject to copyright        
 *   | protection and is in the public domain. NIST’s [intelligent         
 *   | mobility] software is an experimental system. NIST assumes no       
 *   | responsibility whatsoever for its use by other parties, and makes no
 *   | guarantees, expressed or implied, about its quality, reliability, or
 *   | any other characteristic. We would appreciate acknowledgement if the
 *   | software is used. This software can be redistributed and/or modified
 *   | freely provided that any derivative works bear some notice that they
 *   | are derived from it, and any modified versions bear some notice that
 *   | they have been modified.                                            
 *   `---------------------------------------------------------------------
 *  @endverbatim
 *
 * <!--Misc Info-->
 *  @par Created by Tsai Hong 2007-4-4
 **/

#ifndef FWCAMERADATA_HH
#define FWCAMERADATA_HH
/*----------------------------------*\
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include "rcs.hh"
#include "nmlOffsets.hh"


/*-----------------*\
 * Macro Constants *
 \*---------------*/
#define MAX_RGBATA_SIZE (1024*768 *3)


/*-----------------*\
 * Data Structures *
 \*---------------*/
typedef enum {
  FW_CAMERA_DATA_TYPE = FWCAMERADATA_BASE,
} fwCamerDataNmlIdType;

class FW_CAMERA_DATA : public NMLmsg
{
public:
  FW_CAMERA_DATA ();
  void update(CMS *);

  double time;
  char name[80];                // some custom name of the camera.

  int nRows;
  int nCols;

  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned char, rgbData, MAX_RGBATA_SIZE);
};


/*--------------------*\
 * Function Prototype *
 \*------------------*/
int fwCameraData_format(NMLTYPE type, void *buf, CMS *cms);

#define FWCAMERA_DATA_REV "$Rev: 728 $"

static const char *FWCAMERA_DATA_HEADER_FILE=__FILE__;

#endif

