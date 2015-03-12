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

#ifndef __INCladarcmdhh
#define __INCladarcmdhh

#include "rcs.hh"

#define MAX_NUM_SETTINGS	7

#define LADAR_CMD_BASE		60
#define LADAR_CMD_TYPE 		LADAR_CMD_BASE
#define LADAR_CMD_NAME 		"ladarcmd"
#define LADAR_CMD_SIZE 		600

#define LADAR_STOP_CONTINUOUS	0
#define LADAR_GET_FRAME		1
#define LADAR_GET_CONTINUOUS	2
#define LADAR_SETTINGS		3
#define LADAR_GET_PREV_FRAME	4

class LADAR_CMD : public NMLmsg
{
public:
	int id;
	unsigned char command;
	unsigned char ladar_type;
	unsigned short ladar_id;
	
	int num_settings;
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(double, values, MAX_NUM_SETTINGS);
	DECLARE_NML_DYNAMIC_LENGTH_ARRAY(char, titles, MAX_NUM_SETTINGS*50);
	
	long time_s;
	int time_us;

	LADAR_CMD();
	void update(CMS* cms);
};

int LadarCmd_format(NMLTYPE type, void *buf, CMS *cms);

#endif
