//=============================<Header Block>==============================
/*! @file sickData.hh
  - Description:
    - What does this program or file do?
  - Author(s):
    - Tommy Chang <email@address>
  $Revision: 623 $ $Author: tchang $ $Date: 2006-07-05 13:00:02 -0400 (Wed, 05 Jul 2006) $
  @verbatim
  ,----------------------------------------------------------------------+
  | * DISCLAIMER:                                                        |
  | This software was developed at the National Institute of Standards   |
  | and Technology by employees of the Federal Government in the course  |
  | of their official duties. Pursuant to title 17 Section 105 of the    |
  | United States Code this software is not subject to copyright         |
  | protection and is in the public domain. NIST’s [intelligent          |
  | mobility] software is an experimental system. NIST assumes no        |
  | responsibility whatsoever for its use by other parties, and makes no |
  | guarantees, expressed or implied, about its quality, reliability, or |
  | any other characteristic. We would appreciate acknowledgement if the |
  | software is used. This software can be redistributed and/or modified |
  | freely provided that any derivative works bear some notice that they |
  | are derived from it, and any modified versions bear some notice that |
  | they have been modified.                                             |
  `----------------------------------------------------------------------+
  @endverbatim
  @par CVS Log:
    @ref sickData_hh "More..."
  @par Created by Tommy Chang, 2006-03-20 */
//==========================>End of Header Block<==========================


#ifndef SICKDATA_V3_HH
#define SICKDATA_V3_HH

/*----------------------------------*\
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include <stdio.h>
#include "rcs.hh"
#include "navdata_v2.hh"
#include "nmlOffsets.hh"

/*-----------------*\
 * Macro Constants *
 \*---------------*/
#ifndef SICK_RP_MAX_V3
#define SICK_RP_MAX_V3 721         // 0.25 deg resolution, 180 deg
#endif

#define SICK_DATA_V3_BASE SICKDATA_V3_BASE

typedef enum {
  SICK_DATA_V3_TYPE = SICK_DATA_V3_BASE,
  SICK_DATA_WITH_NAV_V3_TYPE,
  SICK_XYZ_DATA_V3_TYPE,
} SickDataNmlIdType;



/*-----------------*\
 * Data Structures *
 \*---------------*/
class SICK_DATA_WITH_NAV_V3 : public NMLmsg
{
public:
  SICK_DATA_WITH_NAV_V3();
  void update(CMS *);
  
  NAV_DATA_V2 nav_data_v2;

  int numData;                  // same as length of dynamic array
  long int scanNum;             // scan number (id)
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(float,sick_rp,SICK_RP_MAX_V3); // in [m]
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned char, intensities,SICK_RP_MAX_V3); // intensity has [0..7]

  void Print ()
  {
    fprintf (stderr, "SICK in RANGE (with NAV)\n");
    fprintf (stderr, "tranRel (%f %f %f), relRpy (%f %f %f)\n",
             nav_data_v2.tranRel.x, 
             nav_data_v2.tranRel.y, 
             nav_data_v2.tranRel.z,
             nav_data_v2.relRpy.r, 
             nav_data_v2.relRpy.p,
             nav_data_v2.relRpy.y);
             
    for (int i = 0; i < sick_rp_length; i++)
      fprintf (stderr, "%d: (%f) [m]\n", i, sick_rp [i]);
    fprintf (stderr, "---------------------------\n");
  }
};



struct 
SICK_XYZ_V3_LIST
{
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(PM_CARTESIAN,xyz,SICK_RP_MAX_V3);
};


class SICK_XYZ_DATA_V3 : public NMLmsg
{
public:
  SICK_XYZ_DATA_V3();
  void update(CMS *);

  /* PM_CARTESIAN xyz version : */
//   SICK_XYZ_V3_LIST veh;         // in [m]
  SICK_XYZ_V3_LIST local;       // in [m]
  SICK_XYZ_V3_LIST vehWithRP; // in [m], take account of roll, pitch but no yaw
  SICK_XYZ_V3_LIST sensor;       // Z is 0.  in [m]

  /* nav data */
  NAV_DATA_V2 nav_data_v2;

  int numData;                  // number of data in scan
  long int scanNum;             // scan number (id)

  // intensity has [0..7] 
  DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned char,intensities,SICK_RP_MAX_V3); 

  void Print ()
  {
    fprintf (stderr, "SICK in VEHICLE\n");
    for (int i = 0; i < vehWithRP.xyz_length; i++)
      fprintf (stderr, "%d: (%f %f %f)\n", i, 
               vehWithRP.xyz[i].x, vehWithRP.xyz[i].y, vehWithRP.xyz[i].z);
    fprintf (stderr, "---------------------------\n");

    fprintf (stderr, "SICK in LOCAL\n");
    for (int i = 0; i < local.xyz_length; i++)
      fprintf (stderr, "%d: (%f %f %f)\n", i, 
               local.xyz[i].x, local.xyz[i].y, local.xyz[i].z);
    fprintf (stderr, "---------------------------\n");

  };
};


/*--------------------*\
 * Function Prototype *
 \*------------------*/
int sickData_v3_format(NMLTYPE type, void *buf, CMS *cms);

#define SICK_DATA_V3_ID "$Id: sickData_v3.hh 623 2008-11-10 16:01:29Z shackle $"

#endif

