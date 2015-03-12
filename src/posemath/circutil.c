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

/*
  circutil.c

  Generalized circle utilities

  Modification history:

  18-Jun-1997  FMP created
  */

#include <math.h>               /* acos() */
#include "posemath.h"           /* these decls, PM_CARTESIAN */

/*
  pmCircleInit() takes the defining parameters of a generalized circle
  and sticks them in the structure. It also computes the radius and vectors
  in the plane that are useful for other functions and that don't need
  to be recomputed every time.
  */
int pmCircleInit(PM_CIRCLE * circle,
                 PM_CARTESIAN start, PM_CARTESIAN end,
                 PM_CARTESIAN center, PM_CARTESIAN normal,
                 int turn)
{
  double dot;
  PM_CARTESIAN rEnd;

  if (0 == circle)
    {
      return -1;
    }

  /* load and normalize required descriptions */
  circle->start = start;
  circle->end = end;
  circle->center = center;
  pmCartNorm(normal, &circle->normal);
  circle->turn = turn;          /* 0 = 0..360, 1 = 360..720,
                                   -1 = 0..-360, -2 = -360..-720, etc. */

  /* radius */
  pmCartCartDisp(start, center, &circle->radius);

  /* vector in plane of circle from center to start, magnitude radius */
  pmCartCartSub(start, center, &circle->rTan);

  /* vector in plane of circle perpendicular to uTan, magnitude radius */
  pmCartCartCross(circle->normal, circle->rTan, &circle->rPerp);

  /* do rHelix, rEnd */
  pmCartCartSub(end, center, &circle->rHelix);
  pmCartPlaneProj(circle->rHelix, circle->normal, &rEnd);
  pmCartMag(rEnd, &circle->spiral);
  circle->spiral -= circle->radius;
  pmCartCartSub(circle->rHelix, rEnd, &circle->rHelix);
  pmCartNorm(rEnd, &rEnd);
  pmCartScalMult(rEnd, circle->radius, &rEnd);

  /* angle */
  pmCartCartDot(start, rEnd, &dot);
  dot = dot / (circle->radius * circle->radius);
  circle->angle = acos(dot);
  if (turn > 0)
    {
      circle->angle += turn * 2.0 * PM_PI;
    }
  else if (turn < 0)
    {
      circle->angle = - circle->angle - (turn + 1) * 2.0 * PM_PI;
    }

  return pmErrno = 0;
}

/*
  pmCirclePoint() returns the vector to the point at the given angle along
  the circle. If the circle is a helix or spiral or combination, the
  point will include interpolation off the actual circle.
  */
int pmCirclePoint(PM_CIRCLE * circle, double angle, PM_CARTESIAN * point)
{
  PM_CARTESIAN par, perp;
  double scale;

  /* compute components rel to center */
  pmCartScalMult(circle->rTan, cos(angle), &par);
  pmCartScalMult(circle->rPerp, sin(angle), &perp);

  /* add to get radius vector rel to center */
  pmCartCartAdd(par, perp, point);

  /* get scale for spiral, helix interpolation */
  scale = angle / circle->angle;

  /* add scaled vector in radial dir for spiral */
  pmCartNorm(*point, &par);
  pmCartScalMult(par, scale * circle->spiral, &par);
  pmCartCartAdd(*point, par, point);

  /* add scaled vector in helix dir */
  pmCartScalMult(circle->rHelix, scale, &perp);
  pmCartCartAdd(*point, perp, point);

  return pmErrno = 0;
}
