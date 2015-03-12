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


/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

/************************************************************************************/
/*                                                                                  */
/* IDENTIFICATION                                                                   */
/*                                                                                  */
/*    Unit name:  Global_timer.c                                                    */
/*    Author(s):  Tom Wheatley                                                      */
/*    Version:    A.01                                                              */
/*    Date:        3/17/ 1993                                                       */
/*    Language:   C                                                                 */
/*    System:     Gnu Sun4 -> 68k cross-compiler                                    */
/*                                                                                  */
/*                                                                                  */
/* PURPOSE OF UNIT                                                                  */
/*                                                                                  */
/*    This program is a collection of timing utilities for the Global_timer, a      */
/*    custom 32-bit timer with usec resolution available on most RCS systems.       */
/*    This code can only be run on a VxWorks board.                                 */
/*                                                                                  */

// Converted to C++ MN 11-22-95
// Convert to milliseconds by dividing by 1000
// e.g. total_time = (get_Global_elapsed_time( prev_time))/1000  --total_time in msec
//  1-Dec-1995 Will Shackleford add global_timer_config() function

/************************************************************************************/

#include "vxWorks.h"
#include "stdioLib.h"
#include "gtimer.hh"

#define MAX_GLOBAL_TIME 0x7FFFFFFF

typedef unsigned short *SCOPE_PTR;
typedef int *TIMER_BD_PTR;

extern "C"
{
  int rd_Global_Clock (TIMER_BD_PTR);
}

#if 0				// Don't store addresses here.
static SCOPE_PTR p0 = (SCOPE_PTR) 0x00C00000;
static SCOPE_PTR p1 = (SCOPE_PTR) 0x00C00002;
static SCOPE_PTR p2 = (SCOPE_PTR) 0x00C00004;
static SCOPE_PTR p3 = (SCOPE_PTR) 0x00C00006;
static SCOPE_PTR p4 = (SCOPE_PTR) 0x00C00008;
static SCOPE_PTR p5 = (SCOPE_PTR) 0x00C0000A;
static SCOPE_PTR p6 = (SCOPE_PTR) 0x00C0000C;
static SCOPE_PTR p7 = (SCOPE_PTR) 0x00C0000E;
static TIMER_BD_PTR hkv3d_loc = (TIMER_BD_PTR) 0x00C00010;
#else
static SCOPE_PTR p0 = NULL;
static SCOPE_PTR p1 = NULL;
static SCOPE_PTR p2 = NULL;
static SCOPE_PTR p3 = NULL;
static SCOPE_PTR p4 = NULL;
static SCOPE_PTR p5 = NULL;
static SCOPE_PTR p6 = NULL;
static SCOPE_PTR p7 = NULL;
static TIMER_BD_PTR hkv3d_loc = NULL;
#endif

short scope_prev_value[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };	/* use for toggling scope values */

// This is the global variable that etime checks to see whether it should call
// get_Global_time();
int global_timer_available = 0;

int
global_timer_config (unsigned long timer_address)
{
  unsigned long base_address;
  global_timer_available = 0;
  if (timer_address <= 0x10)
    {
      return (0);
    }
  base_address = timer_address - 0x10;
  p0 = (SCOPE_PTR) (base_address + 0x0);
  p1 = (SCOPE_PTR) (base_address + 0x2);
  p2 = (SCOPE_PTR) (base_address + 0x4);
  p3 = (SCOPE_PTR) (base_address + 0x6);
  p4 = (SCOPE_PTR) (base_address + 0x8);
  p5 = (SCOPE_PTR) (base_address + 0xA);
  p6 = (SCOPE_PTR) (base_address + 0xC);
  p7 = (SCOPE_PTR) (base_address + 0xE);
  hkv3d_loc = (TIMER_BD_PTR) (timer_address);
  global_timer_available = 1;
  return (1);
}

int
get_Global_time ()
{

  int current_glob_time;

  current_glob_time = rd_Global_Clock (hkv3d_loc);
  return (current_glob_time);
}

#if 0

void
ScopePort (int port_num, short value)
{
  switch (port_num)
    {

    case 0:
      wr_Scope_Port (p0, value);
      break;
    case 1:
      wr_Scope_Port (p1, value);
      break;
    case 2:
      wr_Scope_Port (p2, value);
      break;
    case 3:
      wr_Scope_Port (p3, value);
      break;
    case 4:
      wr_Scope_Port (p4, value);
      break;
    case 5:
      wr_Scope_Port (p5, value);
      break;
    case 6:
      wr_Scope_Port (p6, value);
      break;
    case 7:
      wr_Scope_Port (p7, value);
      break;
    default:
      break;
    }
  scope_prev_value[port_num] = value;

}

void
ToggleScopePort (int port_num)
{

  short new_val;

  new_val = !scope_prev_value[port_num];
  switch (port_num)
    {

    case 0:
      wr_Scope_Port (p0, new_val);
      break;
    case 1:
      wr_Scope_Port (p1, new_val);
      break;
    case 2:
      wr_Scope_Port (p2, new_val);
      break;
    case 3:
      wr_Scope_Port (p3, new_val);
      break;
    case 4:
      wr_Scope_Port (p4, new_val);
      break;
    case 5:
      wr_Scope_Port (p5, new_val);
      break;
    case 6:
      wr_Scope_Port (p6, new_val);
      break;
    case 7:
      wr_Scope_Port (p7, new_val);
      break;
    default:
      break;
    }
  scope_prev_value[port_num] = new_val;
}

#endif
