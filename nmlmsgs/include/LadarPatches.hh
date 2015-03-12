/*
#   ,---------------------------------------------------------------------
#   | DISCLAIMER
#   |---------------------------------------------------------------------
#   | This software was developed at the National Institute of Standards  
#   | and Technology by employees of the Federal Government in the course
#   | of their official duties. Pursuant to title 17 Section 105 of the   
#   | United States Code this software is not subject to copyright        
#   | protection and is in the public domain. NIST's [intelligent         
#   | mobility] software is anexperimental system. NIST assumes no       
#   | responsibility whatsoever for its use by other parties, and makes no
#   | guarantees, expressed or implied, about its quality, reliability, or
#   | any other characteristic. We would appreciate acknowledgement if the
#   | software is used. This software can be redistributed and/or modified
#   | freely provided that any derivative works bear some notice that they
#   | are derived from it, and any modified versions bear some notice that
#   | they have been modified.                                            
#   `---------------------------------------------------------------------
*/

#ifndef __INCladarpatcheshh
#define __INCladarpatcheshh

#include "rcs.hh"

#define LADAR_PATCHES_BASE 		70		

#define LADAR_PATCHES_TYPE 		LADAR_PATCHES_BASE
#define LADAR_PATCHES_NAME 		"ladarpatches"
#define LADAR_PATCHES_SIZE 		300000

#define MAX_PATCHES			256
#define MAX_BOUNDARY_POINTS		32
#define PATCH_MAX_POINTS		128

//	MAX_ORDER		|	PATCH_EQ_LENGTH
//	------------------------|----------------------
//	0			|	1
//	1			|	3
//	2			|	6
//	3			|	10
//	4			| 	15

#define MAX_ORDER			1
#define PATCH_EQ_LENGTH			3

#define PATCH_COORD_LENGTH		3
#define PATCH_BASIS_LENGTH		9
#define PATCH_DATA_LENGTH		(PATCH_EQ_LENGTH + 2 * (PATCH_COORD_LENGTH + PATCH_BASIS_LENGTH))

//	This class stores a collection of surface patches.
//	The patches can be generated through analysis of a single LADAR frame or
//	by higher level filtering, but I recommend keeping filtered and 
//	unfiltered messages in different channels.  Also let's try having
//	one channel for all of our LADARs.

//	These messages can be generated with the SurfaceFit class.
class LADAR_PATCHES : public NMLmsg
{
public:
	LADAR_PATCHES();
  	void update( CMS*);

	int seqno;
	long time_s;
	int time_us;
	int num_patches;

	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(int, ladar_type, MAX_PATCHES);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned short, ids, MAX_PATCHES);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(unsigned short, bound_points, MAX_PATCHES);

	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, origin, MAX_PATCHES * PATCH_COORD_LENGTH);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, basis, MAX_PATCHES * PATCH_BASIS_LENGTH);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, equation, MAX_PATCHES * PATCH_EQ_LENGTH);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, bounds, MAX_PATCHES * PATCH_COORD_LENGTH * MAX_BOUNDARY_POINTS);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, derivatives, MAX_PATCHES * (PATCH_COORD_LENGTH + PATCH_BASIS_LENGTH));
};

int ladar_patches_format(NMLTYPE type, void *buf, CMS *cms);

#endif
