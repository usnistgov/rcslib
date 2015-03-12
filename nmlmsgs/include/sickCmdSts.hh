/**
 * @file sickCmdSts.hh
 *
 * <!--Description-->
 *  @remark: <What is the purpose of this file?>
 *
 *  @author: Tommy Chang
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
 *  @par Created by Tommy Chang, 2006-06-12 
 **/

#ifndef SICKCMDSTS_HH
#define SICKCMDSTS_HH
/*----------------------------------*\
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include "rcs.hh" 	// Common RCS definitions
#include "nmlOffsets.hh"

/*-----------------*\
 * Macro Constants *
 \*---------------*/



/*-----------------*\
 * Data Structures *
 \*---------------*/
typedef enum {
  SICK_STATUS_TYPE = SICKCMDSTS_BASE,
  SICK_COMMAND_TYPE,
} SickCmdStsEnum;

class SICK_STATUS : public RCS_STAT_MSG
{
public:
  // Normal Constructor
  SICK_STATUS();
  
  // CMS Update Function
  void update(CMS *);
  
  // Place custom variables here.
  time_tracker tt;
  float rpy [3];                // in radians
  float xyz [3];                // in meters

  int scanNum;
};


class SICK_COMMAND : public RCS_CMD_MSG
{
public:
  // Normal Constructor
  SICK_COMMAND();
  
  // CMS Update Function
  void update(CMS *);
  
  // Place custom variables here.
  float rpy [3];                // in radians
  float xyz [3];                // in meters
};


/*--------------------*\
 * Function Prototype *
 \*------------------*/
// Declare NML format function
extern int sickCmdStsFormat(NMLTYPE, void *, CMS *);



#endif
