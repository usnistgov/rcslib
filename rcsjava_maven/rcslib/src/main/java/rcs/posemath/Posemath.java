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

 /*
_posemath.c

C definitions for pose math library data types and
manipulation functions.

Modification history:

15-Jan-1998  FMP added pmHomInv()
5-Jan-1998  FMP added pmAxisAngleQuatConvert(), pmQuatAxisAngleMult()
10-Oct-1997  FMP chickened out and checked for divide-by-0 always,
not just if PM_DEBUG is defined. Set results to DBL_MAX where appropriate.
17-Aug-1997  FMP removed include mathprnt.h-- not needed here
11-Jul-1997  FMP changed names from PM_CARTESIAN, for example, to
PmCartesian, to get rid of name conflicts with mixed C/C++ apps
19-Jun-1997  FMP added pmLine, pmCircle stuff
18-Jun-1997  FMP added pmCartPlaneProj()
14-Apr-1997  FMP created from C parts of posemath.c
 */
package rcs.posemath;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

/**
 * Class created only for its static functions to convert and manipulate the
 * pose representations in the rest of the package. Adapted from _posemath.c and
 * posemath.cc.
 *
 * @author shackle
 */
public class Posemath {

    /* global error number */
    static public int pmErrno = 0;
    static public boolean PM_PRINT_ERROR = false;
    static public boolean debug_on = false;
    // Miscellaneous Pre-Defined Values
    public static final double CART_FUZZ = 0.0001;
    public static final double DOUBLECP_FUZZ = 1.0842021724855044e-19;
    public static final double DOUBLE_FUZZ = 2.2204460492503131e-16;
    public static final double E_EPSILON = .005;
    public static final double PM_2_PI = 6.28318530717958647692;
    public static final int PM_DIV_ERR = -4;
    public static final int PM_ERR = -1;
    public static final int PM_IMPL_ERR = -2;
    public static final int PM_NORM_ERR = -3;
    public static final double PM_PI = 3.14159265358979323846;
    public static final double PM_PI_2 = 1.57079632679489661923;
    public static final double PM_PI_4 = 0.78539816339744830962;
    public static final double QSIN_FUZZ = .0003;
    public static final double QS_FUZZ = .0003;
    public static final double Q_FUZZ = .0003;
    public static final double RPY_P_FUZZ = 0.001;
    public static final double RS_FUZZ = .0001;
    public static final double SINGULAR_EPSILON = .0001;
    public static final double SQRT_FUZZ = -.0002;
    public static final double UNIT_QUAT_FUZZ = .01;
    public static final double UNIT_SC_FUZZ = .01;
    public static final double UNIT_VEC_FUZZ = .01;
    public static final double V_FUZZ = .0005;
    public static final double ZYX_Y_FUZZ = 0.001;
    public static final double ZYZ_Y_FUZZ = 0.001;
    public static final double DBL_MAX = 2 ^ 969;
    public static final double PI = PM_PI;
    public static final double PI_2 = PM_PI_2;
    public static final double PI_4 = PM_PI_4;
    // Enumerated Type Constants
    // PmAxis
    public static final int PM_X = 0;
    public static final int PM_Y = 1;
    public static final int PM_Z = 2;

//    static public void main(String args[]) {
//
//        LinkedList<PM_CARTESIAN> l = new LinkedList<PM_CARTESIAN>();
//
//        PM_CARTESIAN center = new PM_CARTESIAN(2, 3, 0);
//        double radius = 1.5;
//        double offset = 0.1;
//        for (double angle = 0; angle < Math.PI; angle += Math.PI / 25.0) {
//            radius += offset;
//            offset *= -1.0;
//            PM_CARTESIAN inc = new PM_CARTESIAN(Math.cos(angle) * radius,
//                    Math.sin(angle) * radius, 0);
//            PM_CARTESIAN point = add(inc, center);
//            l.add(point);
//        }
//        PM_CIRCLE circle = Posemath.fitCircle2D_XY(l);
//        System.out.println("circle = " + circle);
//        System.out.println("circle.center = " + circle.center);
//        System.out.println("circle.radius = " + circle.radius);
//    }
//    static public void main(String args[]) {
//        LinkedList<PM_CARTESIAN> l = new LinkedList<PM_CARTESIAN>();
//
//        l.add(new PM_CARTESIAN(0,0,0));
//        l.add(new PM_CARTESIAN(1,10,0));
////        l.add(new PM_CARTESIAN(2,21,0));
//
//
//        PM_LINE line = Posemath.fitLine(l);
//        System.out.println("line = " + line);
//    }
    public static void writeTimeAndPoseListS(List<Double> tlist,
            List<PM_POSE> posList,
            String fname) {
        try {
            writeTimeAndPoseList(tlist, posList,
                    new PrintStream(new FileOutputStream(new File(fname))));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void writeTimeAndPoseList(List<Double> tlist,
            List<PM_POSE> posList,
            PrintStream psOut) throws Exception {
        Iterator<Double> t_it = tlist.iterator();
        Iterator<PM_POSE> p_it = posList.iterator();
        psOut.println("time,x,y,z,roll,pitch,yaw");
        while (t_it.hasNext() && p_it.hasNext()) {
            double time = t_it.next();
            PM_POSE pos = p_it.next();
            PM_RPY rpy = new PM_RPY(pos.rot);
            psOut.printf("%+14.3f,%+9.5f,%+9.5f,%+9.5f,%+9.5f,%+9.5f,%+9.5f\n",
                    time,
                    pos.tran.x,
                    pos.tran.y,
                    pos.tran.z,
                    Math.toDegrees(rpy.r),
                    Math.toDegrees(rpy.p),
                    Math.toDegrees(rpy.y));
        }
    }

    public static void csvSync(File f1, File f2, PrintStream psOut)
            throws Exception {
        List<Double> tlist = Posemath.csvToListF(f1, 0);
        List<PM_POSE> posList = Posemath.csvWithTimeToPoseListF(f2,
                tlist,
                0, 1, 2, 3, 4, 5, 6);
        writeTimeAndPoseList(tlist, posList, psOut);
    }

    static public double shiftedSumOfProducts(List<Double> l1, List<Double> l2, double dshift) {
        int ishift = (int) Math.floor(dshift);
        double dfrac = dshift - ishift;
        Iterator<Double> l1_it = l1.iterator();
        Iterator<Double> l2_it = l2.iterator();
        double last_l1_val = 0;
        double last_l2_val = 0;
        if (ishift > 0) {
            while (ishift > 0) {
                if (!l1_it.hasNext()) {
                    return 0;
                }
                last_l1_val = l1_it.next();
                ishift--;
            }
        } else if (ishift < 0) {
            while (ishift < 0) {
                if (!l2_it.hasNext()) {
                    return 0;
                }
                last_l2_val = l2_it.next();
                ishift++;
            }
        }
        double total = 0;
        for (int i = 0; i < l1.size(); i++) {
            if (!l1_it.hasNext()) {
                l1_it = l1.iterator();
            }
            if (!l2_it.hasNext()) {
                l2_it = l2.iterator();
            }
            double l1_val = l1_it.next();
            double l2_val = l2_it.next();
            total += (l1_val * (l2_val * (1 - dfrac) + last_l2_val * dfrac));
            last_l2_val = l2_val;
            last_l1_val = l1_val;
        }
        return total;
    }

    static void printCorrelationShifts(List<Double> l1, List<Double> l2,
            double start_shift, double end_shift, double inc,
            PrintStream ps) {
        double mean_l1 = Posemath.mean(l1);
        double mean_l2 = Posemath.mean(l2);
        double dev_l1 = Posemath.stddev(l1);
        double dev_l2 = Posemath.stddev(l2);
        double mean_prod = mean_l1 * mean_l2;
        double dev_prod = dev_l1 * dev_l2;
        ps.println("shift,correlation");
        for (double dshift = start_shift; dshift <= end_shift; dshift += inc) {
            double sop = Posemath.shiftedSumOfProducts(l1, l2, dshift);
            double corr = (sop / l1.size() - mean_prod) / (dev_prod);
            ps.println(dshift + "," + corr);
        }
    }

    static public void printCorrelationShifts(String fname1, int f1,
            String fname2, int f2,
            double start_shift, double end_shift, double inc,
            PrintStream ps) throws Exception {
        List<Double> l1 = Posemath.csvToList(fname1, f1);
        List<Double> l2 = Posemath.csvToList(fname2, f2);
        Posemath.printCorrelationShifts(l1, l2,
                start_shift, end_shift, inc, ps);
    }

    static public void main(String args[]) throws Exception {
        
        
       PmRotationVector rv = new PmRotationVector(PM_PI, 1, 0, 1);
       PmRotationMatrix mat = toMat(rv);
        System.out.println("mat = " + mat);
        
        
        PmEulerZyz zyz = new PmEulerZyz(PM_PI, 0, PM_PI);
        System.out.println("zyz = " + zyz);
        mat = toMat(zyz);
        System.out.println("mat = " + mat);
        
        
        PmEulerZyx zyx = new PmEulerZyx(PM_PI, 0, PM_PI);
        System.out.println("zyx = " + zyx);
        mat = toMat(zyx);
        System.out.println("mat = " + mat);
        
        zyx = new PmEulerZyx(PM_PI/2, 0, PM_PI);
        System.out.println("zyx = " + zyx);
        mat = toMat(zyx);
        System.out.println("mat = " + mat);
        
        
//        if (args[0].compareTo("--csvShiftCorrel") == 0) {
//            printCorrelationShifts(args[1],
//                    Integer.valueOf(args[2]),
//                    args[3],
//                    Integer.valueOf(args[4]),
//                    Double.valueOf(args[5]),
//                    Double.valueOf(args[6]),
//                    Double.valueOf(args[7]),
//                    System.out);
//        } else if (args[0].compareTo("--findStillTimes") == 0) {
//            findStillTimes(Arrays.copyOfRange(args, 1, args.length));
//        } else if (args[0].compareTo("--csvSync") == 0) {
//            csvSync(new File(args[1]),
//                    new File(args[2]),
//                    System.out);
//        } else {
//            System.err.println("Unrecognized args " + args[0]);
//        }
    }

    static public void findStillTimes(String args[]) {
        try {
            List<Double> tlist = Posemath.csvToListF(new File(args[0]), 0);
            List<PM_POSE> posList = Posemath.csvWithTimeToPoseListF(new File(args[0]),
                    tlist,
                    0, 1, 2, 3, 4, 5, 6);
            List<Double> stillTimesList = new LinkedList<Double>();
            List<PM_POSE> stillList
                    = Posemath.findStillPoints(tlist, posList,
                            Double.valueOf(args[1]),
                            Integer.valueOf(args[2]),
                            stillTimesList);
            Posemath.writeTimeAndPoseList(stillTimesList, stillList,
                    System.out);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Ignores the z value in each element of the list and returns a line fit
     * with least-squares error to x,y points
     *
     * @param l list of PmCartesian points to fit
     * @return PM_LINE with start,end and unit vector of the line.
     */
    static public PM_LINE fitLine(List<? extends PmCartesian> l) {
        PM_LINE line = new PM_LINE();
        try {
            PmCartesian sum = new PM_CARTESIAN();
            for (PmCartesian c : l) {
                sum = add(sum, c);
            }
            PmCartesian mean = multiply(sum, 1.0 / l.size());
            double sxx = 0;
            double syy = 0;
            double sxy = 0;
            double szz = 0;
            double sxz = 0;
            double syz = 0;
            for (PmCartesian c : l) {
                double dx = c.x - mean.x;
                double dy = c.y - mean.y;
                double dz = c.z - mean.z;
                sxx += dx * dx;
                syy += dy * dy;
                sxy += dx * dy;
                szz += dz * dz;
                sxz += dx * dz;
                syz += dy * dz;
            }
            double slope_xy = 0;
            double slope_xz = 0;
            if (sxx > Double.MIN_VALUE) {
                slope_xz = sxz / sxx;
                slope_xy = sxy / sxx;
                line.uVec = Posemath.norm(new PM_CARTESIAN(1, slope_xy, slope_xz));
            } else {
                double slope_yz = 0;
                if (syy > Double.MIN_VALUE) {
                    slope_yz = syz / syy;
                }
                line.uVec = Posemath.norm(new PM_CARTESIAN(0, 1, slope_yz));
            }
            double min_dot = Double.POSITIVE_INFINITY;
            double max_dot = Double.NEGATIVE_INFINITY;
            line.start = line.end = new PM_CARTESIAN(mean);
            PM_CARTESIAN fstart = (PM_CARTESIAN) line.start.clone();
            PM_CARTESIAN fend = (PM_CARTESIAN) line.end.clone();
            for (PmCartesian c : l) {
                PmCartesian dmean = subtract(c, mean);
                double d = dot(dmean, line.uVec);
                if (d < min_dot) {
                    min_dot = d;
                    fstart = new PM_CARTESIAN(Posemath.point_on_line(line, c));
                }
                if (d > max_dot) {
                    max_dot = d;
                    fend = new PM_CARTESIAN(Posemath.point_on_line(line, c));
                }
            }
            line.start = fstart;
            line.end = fend;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return line;
    }

    /**
     * Adds two PM_CARTESIANS and returns a weightedAvg ie. c1*s1+c2*s2;
     *
     * @param c1 first PM_CARTESIAN to add
     * @param s1 weighting scale factor for c1
     * @param c2 second PM_CARTESIAN to add
     * @param s2 weighting scale factor for c2
     * @return c1*s1+c2*s2
     */
    public static PM_CARTESIAN weightedAvg(PM_CARTESIAN c1, double s1,
            PM_CARTESIAN c2, double s2) {
        return add(multiply(c1, s1), multiply(c2, s2));
    }

    /**
     * Adds two PM_POSES and returns a weightedAvg ie. c1*s1+c2*s2;
     *
     * @param p1 first PM_POSE to add
     * @param s1 weighting scale factor for c1
     * @param p2 second PM_POSE to add
     * @param s2 weighting scale factor for c2
     * @return c1*s1+c2*s2
     * @throws rcs.posemath.PmException when multiplication fails
     */
    public static PM_POSE weightedAvg(PM_POSE p1, double s1,
            PM_POSE p2, double s2) throws PmException {
        PM_POSE outPose = new PM_POSE();
        outPose.tran.x = p1.tran.x * s1 + p2.tran.x * s2;
        outPose.tran.y = p1.tran.y * s1 + p2.tran.y * s2;
        outPose.tran.z = p1.tran.z * s1 + p2.tran.z * s2;
        PmQuaternion q1s = p1.rot.clone();
        Posemath.pmQuatScalMult(p1.rot, s1, q1s);
        PmQuaternion q2s = p2.rot.clone();
        Posemath.pmQuatScalMult(p2.rot, s2, q2s);
        Posemath.pmQuatQuatMult(q1s, q2s, outPose.rot);
        return outPose;
    }

    /**
     * Compute an array of errors or the distance between each point in list l
     * to line line.
     *
     * @param line line points should be near
     * @param l list of points near line
     * @return array of errors
     */
    public static double[] computeLineFitErrors(PM_LINE line,
            List<? extends PmCartesian> l) {
        double errors[] = new double[l.size()];
        for (int i = 0; i < l.size(); i++) {
            PmCartesian c = l.get(i);
            PmCartesian p = Posemath.point_on_line(line, c);
            errors[i] = p.distFrom(c);
        }
        return errors;
    }

    /**
     * Ignores the z value in each element of the list and returns a circle fit
     * with least-squares error to x,y points
     *
     * @param l list of PmCartesian points to fit
     * @return PM_CIRCLE with center and radius of the fitted circle.
     */
    static public PM_CIRCLE fitCircle2D_XY(List<? extends PmCartesian> l) {
        //
        // Taken from www.dtcenter.org/met/users/docs/write_ups/circle_fit.pdf
        //  --
        // October 24, 2006 10:22
        // Least Squares Circle Fit
        // by R. Bullock
        //
        if (l == null || l.size() < 3) {
            return null;
        }
        PM_CIRCLE circle = new PM_CIRCLE();
        PM_CARTESIAN sum = new PM_CARTESIAN();
        for (PmCartesian c : l) {
            sum.x += c.x;
            sum.y += c.y;
        }
        PM_CARTESIAN mean = multiply(sum, 1.0 / l.size());
//        System.out.println("mean = " + mean);
        double suu = 0;
        double suv = 0;
        double svv = 0;
        double suuu = 0;
        double svvv = 0;
        double suvv = 0;
        double svuu = 0;
        for (PmCartesian c : l) {
            double u = c.x - mean.x;
            double v = c.y - mean.y;
            suu += u * u;
            svv += v * v;
            suv += u * v;
            suuu += u * u * u;
            svvv += v * v * v;
            suvv += u * v * v;
            svuu += v * u * u;
        }
        circle.setAngle(2 * Math.PI);
        circle.setNormal(new PM_CARTESIAN(0, 0, 1));
        double a0 = suu;
        double b0 = suv;
        double c0 = (0.5 * (suuu + suvv));
        double a1 = suv;
        double b1 = svv;
        double c1 = (0.5 * (svvv + svuu));
        double det = a0 * b1 - a1 * b0;
        if (Math.abs(det) <= Double.MIN_NORMAL) {
            return null;
        }
        double uc = (b1 * c0 - b0 * c1) / det;
        double vc = (a0 * c1 - a1 * c0) / det;
        circle.setCenter(new PM_CARTESIAN(uc + mean.x, vc + mean.y, 0));
        double alpha = uc * uc
                + vc * vc
                + (suu + svv) / l.size();
        circle.setRadius(Math.sqrt(alpha));
//        double err = 0;
//        double sum_g = 0;
//        for (PmCartesian c : l) {
//            double u = c.x - mean.x;
//            double v = c.y - mean.y;
//            double du = (u - uc);
//            double dv = (v - vc);
//            double g = du * du + dv * dv - alpha;
//            sum_g += g;
//            double dx = c.x - circle.center.x;
//            double dy = c.y - circle.center.y;
//            err += dx*dx + dy*dy - alpha;
//        }
//        System.out.println("uc = " + uc);
//        System.out.println("vc = " + vc);
//        System.out.println("uc*a0+vc*b0 - c0= "+(uc*a0+vc*b0 - c0));
//        System.out.println("uc*a1+vc*b1 - c1= "+(uc*a1+vc*b1 - c1));
//        System.out.println("sum_g = " + sum_g);
//        System.out.println("err = " + err);
        return circle;
    }

    /**
     * Print an error if PM_PRINT_ERROR is set and throw an exception.
     *
     * @param str string to print
     * @param new_errno integer error code to put in pmErrno
     * @throws PmException
     */
    static private void pmPrintError(String str, int new_errno) throws PmException {
        pmErrno = new_errno;
        if (PM_PRINT_ERROR) {
            try {
                System.err.println(str);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        throw new PmException(pmErrno, str);
    }

    /* fuzz checker */
    static public boolean IS_FUZZ(double a, double fuzz) throws PmException {
        return Math.abs(a) < fuzz;
    }


    /* Pose Math Basis Functions */

 /* Scalar functions */
    /**
     * Square root function that adds some extra checks similiar to the C++
     * pmSqrt() primarily only exists for converting C++ apps. Otherwise use
     * Math.sqrt().
     *
     * @param x number to take square root of
     * @return square root of x
     * @throws PmException when x less than zero
     */
    static public double pmSqrt(double x) throws PmException {
        if (x > 0.0) {
            return Math.sqrt(x);
        }

        if (x > SQRT_FUZZ) {
            return 0.0;
        }

        pmPrintError("sqrt of large negative number\n", PM_ERR);
        return 0.0;
    }

    /**
     * Square of x or x*x.
     *
     * @param x number to square
     * @return square of x
     */
    static public double pmSq(double x) {
        return x * x;
    }

    /**
     * Determine if a within eps of b.
     *
     * @param a value to check if close to b
     * @param b value to check if close to b
     * @param eps allowed difference between two values
     * @return {@code (Math.abs(a - b) < eps) }
     */
    public static boolean pmClose(double a, double b, double eps) {
        return (Math.abs(a - b) < eps);
    }


    /* Translation rep conversion functions */

 /*
     * Convert PM_CARTESIAN to a PM_CYLINDRICAL
     * @param v cartesian to convert    
     * @return v in cylindrical coords
    /
    static public PM_CYLINDRICAL toCyl(PM_CARTESIAN v) {
    PM_CYLINDRICAL c = new PM_CYLINDRICAL();
    pmCartCylConvert(v, c);
    return c;
    }

    /**
     * Convert a PmCartesian to a PmCylindrical
     * @param v cartesian to convert
     * @param c stores converted cylindrical
     * @return 0 
     */
    static public int pmCartCylConvert(PmCartesian v, PmCylindrical c) {
        c.theta = Math.atan2(v.y, v.x);
        try {
            c.r = pmSqrt(pmSq(v.x) + pmSq(v.y));
        } catch (Exception e) {
        }
        c.z = v.z;
        return pmErrno = 0;
    }

    /**
     * Convert a PM_CARTESIAN to PM_SPHERICAL
     *
     * @param v cartesian to convert
     * @return v in spherical coords
     */
    static public PM_SPHERICAL toSph(PM_CARTESIAN v) {
        PM_SPHERICAL s = new PM_SPHERICAL();
        pmCartSphConvert(v, s);
        return s;
    }

    /**
     * Convert a PmCartesian to PmSpherical
     *
     * @param v cartesian to convert
     * @param s spherical to store converted results in
     * @return 0
     */
    static public int pmCartSphConvert(PmCartesian v, PmSpherical s) {
        double _r;

        s.theta = Math.atan2(v.y, v.x);
        try {
            s.r = pmSqrt(pmSq(v.x) + pmSq(v.y) + pmSq(v.z));
            _r = pmSqrt(pmSq(v.x) + pmSq(v.y));
            s.phi = Math.atan2(_r, v.z);
        } catch (Exception e) {
        }
        return pmErrno = 0;
    }

    /**
     * Convert PM_SPHERICAL to PM_CARTESIAN
     *
     * @param v cartesian to convert
     * @return v in spherical coords
     */
    static public PM_CARTESIAN toCart(PM_SPHERICAL v) {
        PM_CARTESIAN c = new PM_CARTESIAN();
        pmSphCartConvert(v, c);
        return c;
    }

    /**
     * Convert PmSherical to PmCartesian
     *
     * @param s spherical to convert
     * @param v cartesian to store converted result
     * @return 0
     */
    static public int pmSphCartConvert(PmSpherical s, PmCartesian v) {
        double _r;

        _r = s.r * Math.sin(s.phi);
        v.z = s.r * Math.cos(s.phi);
        v.x = _r * Math.cos(s.theta);
        v.y = _r * Math.sin(s.theta);

        return pmErrno = 0;
    }

    /**
     * Convert a spherical to cylindrical
     *
     * @param s spherical to convert
     * @return s in cylindrical coordinates.
     */
    static public PM_CYLINDRICAL toCyl(PM_SPHERICAL s) {
        PM_CYLINDRICAL c = new PM_CYLINDRICAL();
        pmSphCylConvert(s, c);
        return c;
    }

    /**
     * Convert a spherical to cylindrical
     *
     * @param s spherical to convert
     * @param c cylindrical to store converted value
     * @return 0
     */
    static public int pmSphCylConvert(PmSpherical s, PmCylindrical c) {
        PmCartesian v = new PmCartesian();
        pmSphCartConvert(s, v);
        pmCartCylConvert(v, c);
        return pmErrno;
    }

    /**
     * Convert a cylindrical to a cartesian
     *
     * @param v cylindrical to convert
     * @return v in cartesian coords
     */
    static public PM_CARTESIAN toCart(PM_CYLINDRICAL v) {
        PM_CARTESIAN c = new PM_CARTESIAN();
        pmCylCartConvert(v, c);
        return c;
    }

    /**
     * Convert cylindrical to cartesian.
     *
     * @param c cylindrical to convert
     * @param v cartesian to store result
     * @return 0
     */
    static public int pmCylCartConvert(PmCylindrical c, PmCartesian v) {
        v.x = c.r * Math.cos(c.theta);
        v.y = c.r * Math.sin(c.theta);
        v.z = c.z;
        return pmErrno = 0;
    }

    /**
     * Convert a cylindrical to a spherical
     *
     * @param c cylindrical to convert
     * @return c in spherical coordinates
     */
    static public PM_SPHERICAL toSph(PM_CYLINDRICAL c) {
        PM_SPHERICAL s = new PM_SPHERICAL();
        pmCylSphConvert(c, s);
        return s;
    }

    /**
     * Convert a cylindrical to a spherical
     *
     * @param c cylindrical to convert
     * @param s spherical to store result
     * @return 0
     */
    static public int pmCylSphConvert(PmCylindrical c, PmSpherical s) {
        pmErrno = 0;
        PmCartesian v = new PmCartesian();
        pmCylCartConvert(c, v);
        pmCartSphConvert(v, s);
        return pmErrno;
    }


    /* Rotation rep conversion functions */
    /**
     * Store a quaternion based on a single rotation about an axis
     *
     * @param axis either PM_X,PM_Y, or PM_Z
     * @param a angle in radians for rotation
     * @param q quaternion to store result
     * @return 0
     * @throws PmException when conversion is not possible
     */
    static public int pmAxisAngleQuatConvert(int axis, double a, PmQuaternion q) throws PmException {
        double sh;

        a *= 0.5;
        sh = Math.sin(a);
        q.s = Math.cos(a);

        switch (axis) {
            case PM_X:
                q.x = sh;
                q.y = 0.0;
                q.z = 0.0;
                break;

            case PM_Y:
                q.x = 0.0;
                q.y = sh;
                q.z = 0.0;
                break;

            case PM_Z:
                q.x = 0.0;
                q.y = 0.0;
                q.z = sh;
                break;

            default:
                pmPrintError("error: bad axis in pmAxisAngleQuatConvert\n", PM_ERR);
                return -1;
        }

        if (q.s < 0.0) {
            q.s *= -1.0;
            q.x *= -1.0;
            q.y *= -1.0;
            q.z *= -1.0;
        }

        return 0;
    }

    /**
     * Convert a rotation vector to a quaternion
     *
     * @param v rotation vector to convert
     * @return v as a quaternion
     * @throws PmException when conversion is not possible
     */
    static public PM_QUATERNION toQuat(PM_ROTATION_VECTOR v) throws PmException {
        PM_QUATERNION q = new PM_QUATERNION();
        pmRotQuatConvert(v, q);
        return q;
    }

    /**
     * Convert a rotation vector to a quaternion
     *
     * @param v rotation vector to convert
     * @return v as a quaternion
     * @throws PmException when conversion is not possible
     */
    static public PmQuaternion toQuat(PmRotationVector v) throws PmException {
        PmQuaternion q = new PmQuaternion();
        pmRotQuatConvert(v, q);
        return q;
    }

    /**
     * Convert a rotation vector to a quaternion
     *
     * @param r rotation vector to convert
     * @param q quaternion to store result
     * @return 0
     * @throws PmException when conversion is not possible
     */
    static public int pmRotQuatConvert(PmRotationVector r, PmQuaternion q) throws PmException {
        double sh;

        /* make sure r is normalized */
        if (0 != pmRotNorm(r, r)) {

            pmPrintError("error: pmRotQuatConvert rotation vector not normalized\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        if (pmClose(r.s, 0.0, 1e-3)) {
            q.s = 1.0;
            q.x = q.y = q.z = 0.0;

            return pmErrno = 0;
        }

        sh = Math.sin(r.s / 2.0);
        q.s = Math.cos(r.s / 2.0);

        if (q.s >= 0.0) {
            q.x = r.x * sh;
            q.y = r.y * sh;
            q.z = r.z * sh;
        } else {
            q.s *= -1;
            q.x = -r.x * sh;
            q.y = -r.y * sh;
            q.z = -r.z * sh;
        }

        return pmErrno = 0;
    }

//    /**
//     * Convert rotation vector to rotation matrix
//     *
//     * @param v rotation vector to convert (must be normalized)
//     * @return v as a rotation matrix
//     * @throws PmException when conversion is not possible
//     */
//    static public PM_ROTATION_MATRIX toMat(PM_ROTATION_VECTOR v) throws PmException {
//        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
//        pmRotMatConvert(v, m);
//        return m;
//    }

    /**
     * Convert rotation vector to rotation matrix
     *
     * @param v rotation vector to convert (must be normalized)
     * @return v as a rotation matrix
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_MATRIX toMat(PmRotationVector v) throws PmException {
        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
        pmRotMatConvert(v, m);
        return m;
    }
    
    /**
     * Convert Euler zyx to rotation matrix
     *
     * @param zyx Euler zyx to convert (must be normalized)
     * @return v as a rotation matrix
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_MATRIX toMat(PmEulerZyx zyx) throws PmException {
        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
        pmZyxMatConvert(zyx, m);
        return m;
    }

    
    
    
    /**
     * Convert Euler zyz to rotation matrix
     *
     * @param zyz rEuler zyz to convert (must be normalized)
     * @return v as a rotation matrix
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_MATRIX toMat(PmEulerZyz zyz) throws PmException {
        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
        pmZyzMatConvert(zyz, m);
        return m;
    }
    
    /**
     * Convert roll-pitch-yaw to rotation matrix
     *
     * @param rpy roll-pitch-yaw to convert (must be normalized)
     * @return v as a rotation matrix
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_MATRIX toMat(PmRpy rpy) throws PmException {
        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
        pmRpyMatConvert(rpy, m);
        return m;
    }

//    /**
//     * Convert roll-pitch-yaw to rotation matrix
//     *
//     * @param rpy roll-pitch-yaw to convert (must be normalized)
//     * @return v as a rotation matrix
//     * @throws PmException when conversion is not possible
//     */
//    static public PmRotationMatrix toMat(PmRpy rpy) throws PmException {
//        PmRotationMatrix m = new PmRotationMatrix();
//        pmRpyMatConvert(rpy, m);
//        return m;
//    }

    /**
     * Convert a rotation vector to a rotation matrix.
     *
     * @param r rotation vector to convert
     * @param m rotation matrix to store result
     * @return 0
     * @throws PmException when conversion is not possible
     */
    static public int pmRotMatConvert(PmRotationVector r, PmRotationMatrix m) throws PmException {
        double s, c, omc;

        if (!pmRotIsNorm(r)) {

            pmPrintError("Bad vector in pmRotMatConvert\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        s = Math.sin(r.s);
        c = Math.cos(r.s);

        /* omc = One Minus Cos */
        omc = 1-c;
        /* from space book */
        m.x.x = c + pmSq(r.x) * omc;
        
        m.y.x = -r.z * s + r.x * r.y * omc;
        m.z.x = r.y * s + r.x * r.z * omc;

        m.x.y = r.z * s + r.y * r.x * omc;
        m.y.y = c + pmSq(r.y) * omc;
        m.z.y = -r.x * s + r.y * r.z * omc;

        m.x.z = -r.y * s + r.z * r.x * omc;
        m.y.z = r.x * s + r.z * r.y * omc;
        m.z.z = c + pmSq(r.z) * omc;

        return pmErrno = 0;
    }

//    static public int pmRotZyzConvert(PmRotationVector r, PmEulerZyz zyz) throws PmException {
//
//        pmPrintError("error: pmRotZyzConvert not implemented\n", PM_IMPL_ERR);
//
//        return pmErrno = PM_IMPL_ERR;
//    }
//
//    static public int pmRotZyxConvert(PmRotationVector r, PmEulerZyx zyx) throws PmException {
//
//        pmPrintError("error: pmRotZyxConvert not implemented\n", PM_IMPL_ERR);
//
//        return pmErrno = PM_IMPL_ERR;
//    }
    static public int pmRotRpyConvert(PmRotationVector r, PmRpy rpy) throws PmException {

        PmQuaternion q = new PmQuaternion();
        ;
        int r1, r2;
        r1 = 0;
        r2 = 0;
        q.s = q.x = q.y = q.z = 0.0;

        r1 = pmRotQuatConvert(r, q);
        r2 = pmQuatRpyConvert(q, rpy);

        return (r1 != 0) || (r2 != 0) ? pmErrno : 0;
    }

    /**
     * Convert quaternion to a rotation vector
     *
     * @param rpy roll-pitch-yaw to convert (must be normalized)
     * @return v as a rotation vector
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_VECTOR toRot(PM_RPY rpy) throws PmException {
        PM_ROTATION_VECTOR r = new PM_ROTATION_VECTOR();
        pmRpyRotConvert(rpy, r);
        return r;
    }

    /**
     * Convert roll-pitch-yaw to a rotation vector
     *
     * @param rpy roll-pitch-yaw to convert (must be normalized)
     * @return v as a rotation vector
     * @throws PmException when conversion is not possible
     */
    static public PmRotationVector toRot(PmRpy rpy) throws PmException {
        PmRotationVector r = new PmRotationVector();
        pmRpyRotConvert(rpy, r);
        return r;
    }

    /**
     * Convert quaternion to a rotation vector
     *
     * @param v quaternion to convert (must be normalized)
     * @return v as a rotation vector
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_VECTOR toRot(PM_QUATERNION v) throws PmException {
        PM_ROTATION_VECTOR r = new PM_ROTATION_VECTOR();
        pmQuatRotConvert(v, r);
        return r;
    }

    /**
     * Convert quaternion to a rotation vector
     *
     * @param v quaternion to convert (must be normalized)
     * @return v as a rotation vector
     * @throws PmException when conversion is not possible
     */
    static public PmRotationVector toRot(PmQuaternion v) throws PmException {
        PmRotationVector r = new PmRotationVector();
        pmQuatRotConvert(v, r);
        return r;
    }

    /**
     * Convert quaternion to a rotation vector
     *
     * @param m matrix to convert (must be normalized)
     * @return v as a rotation vector
     * @throws PmException when conversion is not possible
     */
    static public PmRotationVector toRot(PmRotationMatrix m) throws PmException {
        return toRot(toQuat(m));
    }

    /**
     * Convert a quaternion to a rotation vector
     *
     * @param q quaternion to convert
     * @param r rotation vector to store result
     * @return 0
     * @throws PmException when conversion is not possible
     */
    static public int pmQuatRotConvert(PmQuaternion q, PmRotationVector r) throws PmException {
        double sh;

        if (!pmQuatIsNorm(q)) {

            pmPrintError("Bad quaternion in pmQuatRotConvert\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        sh = pmSqrt(pmSq(q.x) + pmSq(q.y) + pmSq(q.z));

        if (sh > QSIN_FUZZ) {
            r.s = 2.0 * Math.atan2(sh, q.s);
            r.x = q.x / sh;
            r.y = q.y / sh;
            r.z = q.z / sh;
        } else {
            r.s = 0.0;
            r.x = 0.0;
            r.y = 0.0;
            r.z = 0.0;
        }

        return pmErrno = 0;
    }

    /**
     * Convert a quaternion to a rotation matrix.
     *
     * @param v quaternion to convert (must be normalized)
     * @return v as a rotation matrix
     * @throws PmException when conversion is not possible
     */
    static public PM_ROTATION_MATRIX toMat(PmQuaternion v) throws PmException {
        PM_ROTATION_MATRIX m = new PM_ROTATION_MATRIX();
        pmQuatMatConvert(v, m);
        return m;
    }

    /**
     * Return the minimum distance of the point p from the path specified by the
     * array of waypoints. The distance is the distance to the closest line
     * segment specified by each pair of consecutive points.
     *
     * @param p point we want to know how close it is to a path
     * @param waypoints array of waypoints to specify a path.
     * @return distance to nearest line segment in whatever units (p and
     * waypoints are in)
     */
    public static double distFromPath(PM_CARTESIAN p, PM_CARTESIAN[] waypoints) {
        double min_diff = Double.POSITIVE_INFINITY;
        for (int i = 1; i < waypoints.length; i++) {
            PM_CARTESIAN p_on_line = point_on_line_segment(waypoints[i - 1], waypoints[i], p);
            double diff = p_on_line.distFrom(p);
            if (diff < min_diff) {
                min_diff = diff;
            }
        }
        return min_diff;
    }

    public static double maxA(double[] da) {
        double max = Double.NEGATIVE_INFINITY;
        for (int i = 0; i < da.length; i++) {
            if (max < da[i]) {
                max = da[i];
            }
        }
        return max;
    }

    public static List<PM_LINE> splitToLines(List<? extends PM_CARTESIAN> l,
            int num_lines, double max_error[], int best_indexes[]) {
        PM_LINE lines[] = new PM_LINE[num_lines];
        int indexes[] = new int[num_lines];
        double min_error = Double.POSITIVE_INFINITY;
        indexes[0] = 0;
        if (num_lines < 2) {
            lines[0] = Posemath.fitLine(l);
            max_error[0] = maxA(Posemath.computeLineFitErrors(lines[0], l));
            best_indexes[0] = 0;
            return Arrays.asList(lines);
        }
        indexes[0] = 0;
        final int max_i = (int) Math.pow(l.size(), num_lines - 1);
        for (int i = 0; i < max_i; i++) {
            for (int j = 1; j < num_lines; j++) {
                int div = (int) Math.pow(l.size(), num_lines - j - 1);
                if (div < 1) {
                    div = 1;
                }
                int index = (i / div) % l.size();
                if (index < indexes[j - 1] + 3) {
                    i += (indexes[j - 1] + 3 - index) * div;
                    if (i >= max_i) {
                        break;
                    }
                    index = (i / div) % l.size();
                }
                indexes[j] = index;
            }
            int j = 0;
            double current_max_error = 0;
            boolean in_order = true;
            for (int k = 1; k < num_lines; k++) {
                if (indexes[k] < indexes[k - 1] + 3) {
                    in_order = false;
                    break;
                }
            }
            if (in_order) {
                for (int k = 0; k < num_lines; k++) {

                    List<? extends PM_CARTESIAN> slist = l.subList(indexes[k],
                            k < num_lines - 1 ? indexes[k + 1] : l.size() - 1);
                    lines[k] = Posemath.fitLine(slist);
                    double error = maxA(Posemath.computeLineFitErrors(lines[k],
                            slist));
                    if (error > current_max_error) {
                        current_max_error = error;
                    }
                    if (error > min_error) {
                        break;
                    }
                }
                if (current_max_error < min_error) {
                    min_error = current_max_error;
                    System.arraycopy(indexes, 0, best_indexes, 0,
                            best_indexes.length);
                }
            }
        }
        indexes = best_indexes;
        for (int k = 0; k < num_lines; k++) {
            List<? extends PM_CARTESIAN> slist = l.subList(indexes[k],
                    k < num_lines - 1 ? indexes[k + 1] : l.size());
            lines[k] = Posemath.fitLine(slist);
        }
        max_error[0] = min_error;
        return Arrays.asList(lines);
    }

    static public List<PM_LINE> fitLines(List<? extends PM_CARTESIAN> l,
            double tolerance,
            int max_lines) {

        // Using array so the max_error can be passed back.
        // We only care about max_error[0]
        double max_error[] = new double[1];
        max_error[0] = Double.POSITIVE_INFINITY;
        int num_lines = 0;
        List<PM_LINE> lines = null;
        int best_indexes[] = null;
        while (max_error[0] > tolerance
                && num_lines < l.size() / 3
                && num_lines < max_lines) {
            num_lines++;
            best_indexes = new int[num_lines];
            lines = splitToLines(l, num_lines, max_error, best_indexes);
        }
//        for (int k = 0; k < num_lines; k++) {
//            List<? extends PM_CARTESIAN> slist = l.subList(best_indexes[k],
//                    k < num_lines - 1 ? best_indexes[k + 1] : l.size());
////            this.line_set_hm.put(lines.get(k), slist);
//        }
        return lines;
    }

    static public ArrayList<PM_CARTESIAN> multiply(PM_XYA xya,
            List<? extends PM_CARTESIAN> l) {
        ArrayList<PM_CARTESIAN> out = new ArrayList<PM_CARTESIAN>(l.size());
        for (int i = 0; i < l.size(); i++) {
            out.add(Posemath.multiply(xya, l.get(i)));
        }
        return out;
    }

    static public PM_CARTESIAN[] multiply(PM_XYA xya, PM_CARTESIAN l[]) {
        PM_CARTESIAN out[] = new PM_CARTESIAN[l.length];
        for (int i = 0; i < l.length; i++) {
            out[i] = Posemath.multiply(xya, l[i]);
        }
        return out;
    }

    static public PM_CARTESIAN[] multiply(PM_POSE pose, PM_CARTESIAN l[]) throws PmException {
        PM_CARTESIAN out[] = new PM_CARTESIAN[l.length];
        for (int i = 0; i < l.length; i++) {
            out[i] = Posemath.multiply(pose, l[i]);
        }
        return out;
    }

    static public PmCartesian pmCartCentroid(List<? extends PmCartesian> l) {
        PmCartesian result = new PmCartesian();
        int count = 0;
        for (PmCartesian c : l) {
            result.add(c);
            count++;
        }
        if (count > 1) {
            result.multiply((double) (1.0 / count));
        }
        return result;
    }

    static public PmCartesian pmCartCentroid(PmCartesian l[]) {
        PmCartesian result = new PmCartesian();
        int count = 0;
        for (PmCartesian c : l) {
            result.add(c);
            count++;
        }
        if (count > 1) {
            result.multiply((double) (1.0 / count));
        }
        return result;
    }

    static public PM_CARTESIAN centroid(PM_CARTESIAN l[]) {
        PM_CARTESIAN result = new PM_CARTESIAN();
        int count = 0;
        for (PM_CARTESIAN c : l) {
            result.add(c);
            count++;
        }
        if (count > 1) {
            result.multiply((double) (1.0 / count));
        }
        return result;
    }

    static public PM_CARTESIAN centroid(List<? extends PM_CARTESIAN> l) {
        PM_CARTESIAN result = new PM_CARTESIAN();
        int count = 0;
        for (PM_CARTESIAN c : l) {
            result.add(c);
            count++;
        }
        if (count > 1) {
            result.multiply((double) (1.0 / count));
        }
        return result;
    }

    static public int pmQuatMatConvert(PmQuaternion q, PmRotationMatrix m) throws PmException {

        if (!pmQuatIsNorm(q)) {

            pmPrintError("Bad quaternion in pmQuatMatConvert\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }


        /* from space book where e1=q.x e2=q.y e3=q.z e4=q.s */
        m.x.x = 1 - 2 * (pmSq(q.y) + pmSq(q.z));
        m.y.x = 2 * (q.x * q.y - q.z * q.s);
        m.z.x = 2 * (q.z * q.x + q.y * q.s);

        m.x.y = 2 * (q.x * q.y + q.z * q.s);
        m.y.y = 1 - 2 * (pmSq(q.z) + pmSq(q.x));
        m.z.y = 2 * (q.y * q.z - q.x * q.s);

        m.x.z = 2 * (q.z * q.x - q.y * q.s);
        m.y.z = 2 * (q.y * q.z + q.x * q.s);
        m.z.z = 1 - 2 * (pmSq(q.x) + pmSq(q.y));

        return pmErrno = 0;
    }

    static public int pmQuatZyzConvert(PmQuaternion q, PmEulerZyz zyz) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        /*  need direct equations */
        r1 = pmQuatMatConvert(q, m);
        r2 = pmMatZyzConvert(m, zyz);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public int pmQuatZyxConvert(PmQuaternion q, PmEulerZyx zyx) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        /* need direct equations */
        r1 = pmQuatMatConvert(q, m);
        r2 = pmMatZyxConvert(m, zyx);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;

    }

    static public PM_RPY toRpy(PM_QUATERNION v) throws PmException {
        PM_RPY r = new PM_RPY();
        pmQuatRpyConvert(v, r);
        return r;
    }

    static public PmRpy toRpy(PmQuaternion v) throws PmException {
        PmRpy r = new PmRpy();
        pmQuatRpyConvert(v, r);
        return r;
    }

    static public int pmQuatRpyConvert(PmQuaternion q, PmRpy rpy) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        /* need direct equations */
        r1 = pmQuatMatConvert(q, m);
        r2 = pmMatRpyConvert(m, rpy);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public int pmMatRotConvert(PmRotationMatrix m, PmRotationVector r) throws PmException {

        PmQuaternion q = new PmQuaternion();
        pmMatQuatConvert(m, q);
        PmRotationVector v = new PmRotationVector();
        pmQuatRotConvert(q, r);

        return pmErrno;
    }

    static public PM_QUATERNION toQuat(PM_ROTATION_MATRIX m) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmMatQuatConvert(m, qout);
        return qout;
    }

    static public PmQuaternion toQuat(PmRotationMatrix m) throws PmException {
        PmQuaternion qout = new PmQuaternion();
        pmMatQuatConvert(m, qout);
        return qout;
    }

    static public int pmMatQuatConvert(PmRotationMatrix m, PmQuaternion q) throws PmException {
        /*
        from Stephe's "space" book
        e1 = (c32 - c23) / 4*e4
        e2 = (c13 - c31) / 4*e4
        e3 = (c21 - c12) / 4*e4
        e4 = sqrt(1 + c11 + c22 + c33) / 2

        if e4 == 0
        e1 = sqrt(1 + c11 - c33 - c22) / 2
        e2 = sqrt(1 + c22 - c33 - c11) / 2
        e3 = sqrt(1 + c33 - c11 - c22) / 2
        to determine whether to take the positive or negative sqrt value
        since e4 == 0 indicates a 180* rotation then (0 x y z) = (0 -x -y -z).
        Thus some generallities can be used:
        1) find which of e1, e2, or e3 has the largest magnitude and leave it pos.
        2) if e1 is largest then
        if c21 < 0 then take the negative for e2
        if c31 < 0 then take the negative for e3
        3) else if e2 is largest then
        if c21 < 0 then take the negative for e1
        if c32 < 0 then take the negative for e3
        4) else if e3 is larget then
        if c31 < 0 then take the negative for e1
        if c32 < 0 then take the negative for e2

        Note: c21 in the space book is m.x.y in this C code
         */

        double a;

        if (!pmMatIsNorm(m)) {
            Posemath.last_bad_rotmat = m;
            pmPrintError("Bad matrix in pmMatQuatConvert:\n" + m.toString(), PM_NORM_ERR);
            return pmErrno = PM_NORM_ERR;
        }

        q.s = 0.5 * pmSqrt(1.0 + m.x.x + m.y.y + m.z.z);

        if (Math.abs(q.s) > QS_FUZZ) {
            q.x = (m.y.z - m.z.y) / (a = 4 * q.s);
            q.y = (m.z.x - m.x.z) / a;
            q.z = (m.x.y - m.y.x) / a;
        } else {
            q.s = 0;
            q.x = pmSqrt(1.0 + m.x.x - m.y.y - m.z.z) / 2.0;
            q.y = pmSqrt(1.0 + m.y.y - m.x.x - m.z.z) / 2.0;
            q.z = pmSqrt(1.0 + m.z.z - m.y.y - m.x.x) / 2.0;

            if (q.x > q.y && q.x > q.z) {
                if (m.x.y < 0.0) {
                    q.y *= -1;
                }
                if (m.x.z < 0.0) {
                    q.z *= -1;
                }
            } else if (q.y > q.z) {
                if (m.x.y < 0.0) {
                    q.x *= -1;
                }
                if (m.y.z < 0.0) {
                    q.z *= -1;
                }
            } else {
                if (m.x.z < 0.0) {
                    q.x *= -1;
                }
                if (m.y.z < 0.0) {
                    q.y *= -1;
                }
            }
        }

        return pmQuatNorm(q, q);
    }

    static public int pmMatZyzConvert(PmRotationMatrix m, PmEulerZyz zyz) throws PmException {
        zyz.y = Math.atan2(pmSqrt(pmSq(m.x.z) + pmSq(m.y.z)), m.z.z);

        if (Math.abs(zyz.y) < ZYZ_Y_FUZZ) {
            zyz.z = 0.0;
            zyz.y = 0.0;
            /* force Y to 0 */
            zyz.zp = Math.atan2(-m.y.x, m.x.x);
        } else if (Math.abs(zyz.y - PM_PI) < ZYZ_Y_FUZZ) {
            zyz.z = 0.0;
            zyz.y = PM_PI;
            /* force Y to 180 */
            zyz.zp = Math.atan2(m.y.x, -m.x.x);
        } else {
            zyz.z = Math.atan2(m.z.y, m.z.x);
            zyz.zp = Math.atan2(m.y.z, -m.x.z);
        }

        return pmErrno = 0;
    }

    static public int pmMatZyxConvert(PmRotationMatrix m, PmEulerZyx zyx) throws PmException {
        zyx.y = Math.atan2(-m.x.z, pmSqrt(pmSq(m.x.x) + pmSq(m.x.y)));

        if (Math.abs(zyx.y - PM_PI_2) < ZYX_Y_FUZZ) {
            zyx.z = 0.0;
            zyx.y = PM_PI_2;
            /* force it */
            zyx.x = Math.atan2(m.y.x, m.y.y);
        } else if (Math.abs(zyx.y + PM_PI_2) < ZYX_Y_FUZZ) {
            zyx.z = 0.0;
            zyx.y = -PM_PI_2;
            /* force it */
            zyx.x = -Math.atan2(m.y.z, m.y.y);
        } else {
            zyx.z = Math.atan2(m.x.y, m.x.x);
            zyx.x = Math.atan2(m.y.z, m.z.z);
        }

        return pmErrno = 0;
    }

    static public int pmMatRpyConvert(PmRotationMatrix m, PmRpy rpy) throws PmException {
        rpy.p = Math.atan2(-m.x.z, pmSqrt(pmSq(m.x.x) + pmSq(m.x.y)));

        if (Math.abs(rpy.p - PM_PI_2) < RPY_P_FUZZ) {
            rpy.r = Math.atan2(m.y.x, m.y.y);
            rpy.p = PM_PI_2;
            /* force it */
            rpy.y = 0.0;
        } else if (Math.abs(rpy.p + PM_PI_2) < RPY_P_FUZZ) {
            rpy.r = -Math.atan2(m.y.z, m.y.y);
            rpy.p = -PM_PI_2;
            /* force it */
            rpy.y = 0.0;
        } else {
            rpy.r = Math.atan2(m.y.z, m.z.z);
            rpy.y = Math.atan2(m.x.y, m.x.x);
        }

        return pmErrno = 0;
    }

    static public PM_RPY toRpy(PmRotationMatrix m) throws PmException {
        PM_RPY rpy = new PM_RPY();
        pmMatRpyConvert(m, rpy);
        return rpy;
    }

    static public PM_RPY toRpy(PmRotationVector rv) throws PmException {
        PM_RPY rpy = new PM_RPY();
        pmRotRpyConvert(rv, rpy);
        return rpy;
    }
    static public int pmZyzRotConvert(PmEulerZyz zyz, PmRotationVector r) throws PmException {
        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmZyzMatConvert(zyz, mat);
        if (c1 < 0) {
            pmPrintError("error: pmZyzRotConvert failed calling pmZyzMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatRotConvert(mat, r);
        if (c2 < 0) {
            pmPrintError("error: pmZyzRotConvert failed calling pmMatRotConvert\n", c2);
            return c2;
        }
        return pmErrno;
    }

    static public PM_QUATERNION toQuat(PM_EULER_ZYZ v) throws PmException {
        PM_QUATERNION q = new PM_QUATERNION();
        pmZyzQuatConvert(v, q);
        return q;
    }

    static public int pmZyzQuatConvert(PmEulerZyz zyz, PmQuaternion q) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        /*  need direct equations */
        r1 = pmZyzMatConvert(zyz, m);
        r2 = pmMatQuatConvert(m, q);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public int pmZyzMatConvert(PmEulerZyz zyz, PmRotationMatrix m) throws PmException {
        double sa, sb, sg;
        double ca, cb, cg;

        sa = Math.sin(zyz.z);
        sb = Math.sin(zyz.y);
        sg = Math.sin(zyz.zp);

        ca = Math.cos(zyz.z);
        cb = Math.cos(zyz.y);
        cg = Math.cos(zyz.zp);

        m.x.x = ca * cb * cg - sa * sg;
        m.y.x = -ca * cb * sg - sa * cg;
        m.z.x = ca * sb;

        m.x.y = sa * cb * cg + ca * sg;
        m.y.y = -sa * cb * sg + ca * cg;
        m.z.y = sa * sb;

        m.x.z = -sb * cg;
        m.y.z = sb * sg;
        m.z.z = cb;

        return pmErrno = 0;
    }

    static public int pmZyzRpyConvert(PmEulerZyz zyz, PmRpy rpy) throws PmException {
        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmZyzMatConvert(zyz, mat);
        if (c1 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmZyzMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatRpyConvert(mat, rpy);
        if (c2 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmMatZyzConvert\n", c2);
            return c2;
        }
        return pmErrno;
    }

    static public int pmZyxRotConvert(PmEulerZyx zyx, PmRotationVector r) throws PmException {
        
        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmZyxMatConvert(zyx, mat);
        if (c1 < 0) {
            pmPrintError("error: pmZyxRotConvert failed calling pmZyxMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatRotConvert(mat, r);
        if (c2 < 0) {
            pmPrintError("error: pmZyxRotConvert failed calling pmMatRotConvert\n", c2);
            return c2;
        }
        return pmErrno;
    }

    static public PM_QUATERNION toQuat(PM_EULER_ZYX v) throws PmException {
        PM_QUATERNION q = new PM_QUATERNION();
        pmZyxQuatConvert(v, q);
        return q;
    }

    static public int pmZyxQuatConvert(PmEulerZyx zyx, PmQuaternion q) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        r1 = pmZyxMatConvert(zyx, m);
        r2 = pmMatQuatConvert(m, q);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public int pmZyxMatConvert(PmEulerZyx zyx, PmRotationMatrix m) throws PmException {
        double sa, sb, sg;
        double ca, cb, cg;

        sa = Math.sin(zyx.z);
        sb = Math.sin(zyx.y);
        sg = Math.sin(zyx.x);

        ca = Math.cos(zyx.z);
        cb = Math.cos(zyx.y);
        cg = Math.cos(zyx.x);

        m.x.x = ca * cb;
        m.y.x = ca * sb * sg - sa * cg;
        m.z.x = ca * sb * cg + sa * sg;

        m.x.y = sa * cb;
        m.y.y = sa * sb * sg + ca * cg;
        m.z.y = sa * sb * cg - ca * sg;

        m.x.z = -sb;
        m.y.z = cb * sg;
        m.z.z = cb * cg;

        return pmErrno = 0;
    }

    static public int pmZyxZyzConvert(PmEulerZyx zyx, PmEulerZyz zyz) throws PmException {

        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmZyzMatConvert(zyz, mat);
        if (c1 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmZyzMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatZyzConvert(mat, zyz);
        if (c2 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmMatZyzConvert\n", c2);
            return c2;
        }
        return pmErrno;
    }

    static public int pmZyxRpyConvert(PmEulerZyx zyx, PmRpy rpy) throws PmException {

        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmZyxMatConvert(zyx, mat);
        if (c1 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmZyxMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatRpyConvert(mat, rpy);
        if (c2 < 0) {
            pmPrintError("error: pmZyxZyzConvert failed calling pmMatRpyConvert\n", c2);
            return c2;
        }
        return pmErrno;
    }

    static public int pmRpyRotConvert(PmRpy rpy, PmRotationVector r) throws PmException {

        //pmPrintError("error: pmRpyRotConvert not implemented\n", PM_IMPL_ERR);
        PmQuaternion q = new PmQuaternion();
        Posemath.pmRpyQuatConvert(rpy, q);
        Posemath.pmQuatRotConvert(q, r);
        return pmErrno = PM_IMPL_ERR;
    }

    static public PM_QUATERNION toQuat(PM_RPY v) throws PmException {
        PM_QUATERNION q = new PM_QUATERNION();
        pmRpyQuatConvert(v, q);
        return q;
    }

    static public PmQuaternion toQuat(PmRpy v) throws PmException {
        PmQuaternion q = new PmQuaternion();
        pmRpyQuatConvert(v, q);
        return q;
    }

    static public int pmRpyQuatConvert(PmRpy rpy, PmQuaternion q) throws PmException {
        PmRotationMatrix m = new PmRotationMatrix();
        int r1, r2;

        r1 = pmRpyMatConvert(rpy, m);
        if (r1 != 0) {
            throw new PmException(Posemath.PM_NORM_ERR, "Can not convert rpy:" + rpy + " to mat.");
        }
        r2 = pmMatQuatConvert(m, q);
        if (r2 != 0) {
            throw new PmException(Posemath.PM_NORM_ERR, "Can not convert mat:" + m + " to quat.");
        }
        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }
        Posemath.pmQuatNorm(q, q);

        if (!Posemath.pmQuatIsNorm(q)) {
            throw new PmException(Posemath.PM_NORM_ERR, q + " is not normalized after rpy convert.");
        }
        return pmErrno;
    }

    static public int pmRpyMatConvert(PmRpy rpy, PmRotationMatrix m) throws PmException {
        double sa, sb, sg;
        double ca, cb, cg;

        sa = Math.sin(rpy.y);
        sb = Math.sin(rpy.p);
        sg = Math.sin(rpy.r);

        ca = Math.cos(rpy.y);
        cb = Math.cos(rpy.p);
        cg = Math.cos(rpy.r);

        m.x.x = ca * cb;
        m.y.x = ca * sb * sg - sa * cg;
        m.z.x = ca * sb * cg + sa * sg;

        m.x.y = sa * cb;
        m.y.y = sa * sb * sg + ca * cg;
        m.z.y = sa * sb * cg - ca * sg;

        m.x.z = -sb;
        m.y.z = cb * sg;
        m.z.z = cb * cg;

        return pmErrno = 0;
    }

    static public int pmRpyZyzConvert(PmRpy rpy, PmEulerZyz zyz) throws PmException {
        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmRpyMatConvert(rpy, mat);
        if (c1 < 0) {
            pmPrintError("error: pmRpyZyzConvert failed calling pmRpyMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatZyzConvert(mat, zyz);
        if (c1 < 0) {
            pmPrintError("error: pmRpyZyzConvert failed calling pmMatZyzConvert\n", c1);
            return c1;
        }
        return pmErrno;
    }

    static public int pmRpyZyxConvert(PmRpy rpy, PmEulerZyx zyx) throws PmException {
        PmRotationMatrix mat = new PmRotationMatrix();
        int c1 = pmRpyMatConvert(rpy, mat);
        if (c1 < 0) {
            pmPrintError("error: pmRpyZyxConvert failed calling pmRpyMatConvert\n", c1);
            return c1;
        }
        int c2 = pmMatZyxConvert(mat, zyx);
        if (c1 < 0) {
            pmPrintError("error: pmRpyZyxConvert failed calling pmMatZyxConvert\n", c1);
            return c1;
        }
        return pmErrno;
    }

    static public PM_POSE toPose(PM_HOMOGENEOUS h) throws PmException {
        PM_POSE pout = new PM_POSE();
        pmHomPoseConvert(h, pout);
        return pout;
    }

    static public PM_HOMOGENEOUS toHom(PM_POSE p) throws PmException {
        PM_HOMOGENEOUS hout = new PM_HOMOGENEOUS();
        pmPoseHomConvert(p, hout);
        return hout;
    }

    static public int pmPoseHomConvert(PmPose p, PmHomogeneous h) throws PmException {
        int r1;

        h.tran = p.tran;
        r1 = pmQuatMatConvert(p.rot, h.rot);

        return pmErrno = r1;
    }

    static public int pmHomPoseConvert(PmHomogeneous h, PmPose p) throws PmException {
        int r1;

        p.tran = h.tran;
        r1 = pmMatQuatConvert(h.rot, p.rot);

        return pmErrno = r1;
    }

    /* PmCartesian functions */
    static public boolean pmCartCartCompare(PmCartesian v1, PmCartesian v2) throws PmException {
        if (debug_on) {
            System.out.println("pmCartCartCompare(" + v1 + ", " + v2 + ")");
        }
        if (Math.abs(v1.x - v2.x) >= V_FUZZ
                || Math.abs(v1.y - v2.y) >= V_FUZZ
                || Math.abs(v1.z - v2.z) >= V_FUZZ) {
            return false;
        }

        return true;
    }

    static public double dot(PmCartesian v1, PmCartesian v2) {
        return pmCartCartDot(v1, v2);
    }

    static public double dot(PM_CARTESIAN v1, PM_CARTESIAN v2) {
        return pmCartCartDot(v1, v2);
    }

    static public double pmCartCartDot(PmCartesian v1, PmCartesian v2) {
        double d = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;

        return d;
    }

    static public PM_CARTESIAN cross(PM_CARTESIAN v1, PM_CARTESIAN v2) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartCartCross(v1, v2, vout);
        return vout;
    }

    static public PmCartesian cross(PmCartesian v1, PmCartesian v2) throws PmException {
        PmCartesian vout = new PmCartesian();
        pmCartCartCross(v1, v2, vout);
        return vout;
    }

    static public int pmCartCartCross(PmCartesian v1, PmCartesian v2, PmCartesian vout) throws PmException {
        vout.x = v1.y * v2.z - v1.z * v2.y;
        vout.y = v1.z * v2.x - v1.x * v2.z;
        vout.z = v1.x * v2.y - v1.y * v2.x;

        pmErrno = 0;
        return 0;
    }

    static public double mag(PM_CARTESIAN v) {
        return pmCartMag(v);
    }

    static public double mag(PmCartesian v) {
        return pmCartMag(v);
    }

    static public double pmCartMag(PmCartesian v) {
        double d = Math.sqrt(pmSq(v.x) + pmSq(v.y) + pmSq(v.z));
        return d;
    }

    static public double disp(PM_CARTESIAN v1, PM_CARTESIAN v2) throws PmException {
        return pmCartCartDisp(v1, v2);
    }

    static public double pmCartCartDisp(PmCartesian v1, PmCartesian v2) throws PmException {
        double d = pmSqrt(pmSq(v2.x - v1.x) + pmSq(v2.y - v1.y) + pmSq(v2.z - v1.z));
        return d;
    }

    static public PM_CARTESIAN add(PM_CARTESIAN v1, PM_CARTESIAN v2) {
        return new PM_CARTESIAN(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z);
//    pmCartCartAdd(v1,v2, vout);
//    return vout;
    }

    static public PmCartesian add(PmCartesian v1, PmCartesian v2) {
        return new PM_CARTESIAN(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z);
//    pmCartCartAdd(v1,v2, vout);
//    return vout;
    }

    static public int pmCartCartAdd(PmCartesian v1, PmCartesian v2, PmCartesian vout) {
        vout.x = v1.x + v2.x;
        vout.y = v1.y + v2.y;
        vout.z = v1.z + v2.z;

        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN subtract(PM_CARTESIAN v1, PM_CARTESIAN v2) {
        return new PM_CARTESIAN(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z);
//    pmCartCartSub(v1,v2, vout);
//    return vout;
    }

    static public PmCartesian subtract(PmCartesian v1, PmCartesian v2) {
        return new PM_CARTESIAN(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z);
//    pmCartCartSub(v1,v2, vout);
//    return vout;
    }

    static public int pmCartCartSub(PmCartesian v1, PmCartesian v2, PmCartesian vout) {
        vout.x = v1.x - v2.x;
        vout.y = v1.y - v2.y;
        vout.z = v1.z - v2.z;
        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN multiply(double d, PM_CARTESIAN v) {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartScalMult(v, d, vout);
        return vout;
    }

    static public PM_CARTESIAN multiply(PM_CARTESIAN v, double d) {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartScalMult(v, d, vout);
        return vout;
    }

    static public PmCartesian multiply(double d, PmCartesian v) {
        PmCartesian vout = new PM_CARTESIAN();
        pmCartScalMult(v, d, vout);
        return vout;
    }

    static public PmCartesian multiply(PmCartesian v, double d) {
        PmCartesian vout = new PmCartesian();
        pmCartScalMult(v, d, vout);
        return vout;
    }

    static public int pmCartScalMult(PmCartesian v1, double d, PmCartesian vout) {
        vout.x = v1.x * d;
        vout.y = v1.y * d;
        vout.z = v1.z * d;
        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN divide(PM_CARTESIAN v, double d) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartScalDiv(v, d, vout);
        return vout;
    }

    static public int pmCartScalDiv(PmCartesian v1, double d, PmCartesian vout) throws PmException {
        if (d == 0.0) {

            pmPrintError("Divide by 0 in pmCartScalDiv\n", PM_DIV_ERR);

            vout.x = DBL_MAX;
            vout.y = DBL_MAX;
            vout.z = DBL_MAX;

            pmErrno = PM_DIV_ERR;
            return pmErrno;
        }

        vout.x = v1.x / d;
        vout.y = v1.y / d;
        vout.z = v1.z / d;

        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN neg(PM_CARTESIAN v) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartNeg(v, vout);
        return vout;
    }

    static public int pmCartNeg(PmCartesian v1, PmCartesian vout) throws PmException {
        vout.x = -v1.x;
        vout.y = -v1.y;
        vout.z = -v1.z;

        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN inv(PM_CARTESIAN v) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartInv(v, vout);
        return vout;
    }

    static public int pmCartInv(PmCartesian v1, PmCartesian vout) throws PmException {
        double size_sq = pmSq(v1.x) + pmSq(v1.y) + pmSq(v1.z);

        if (size_sq == 0.0) {

            pmPrintError("Zero vector in pmCartInv\n", PM_NORM_ERR);

            vout.x = DBL_MAX;
            vout.y = DBL_MAX;
            vout.z = DBL_MAX;

            pmErrno = PM_NORM_ERR;
            return pmErrno;
        }

        vout.x = v1.x / size_sq;
        vout.y = v1.y / size_sq;
        vout.z = v1.z / size_sq;

        pmErrno = 0;
        return 0;
    }

    static public PM_CARTESIAN norm(PM_CARTESIAN v) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartNorm(v, vout);
        return vout;
    }

    static public int pmCartNorm(PmCartesian v, PmCartesian vout) throws PmException {
        double size = pmSqrt(pmSq(v.x) + pmSq(v.y) + pmSq(v.z));

        if (size == 0.0) {

            pmPrintError("Zero vector in pmCartNorm\n", PM_NORM_ERR);

            vout.x = DBL_MAX;
            vout.y = DBL_MAX;
            vout.z = DBL_MAX;

            pmErrno = PM_NORM_ERR;
            return pmErrno;
        }

        vout.x = v.x / size;
        vout.y = v.y / size;
        vout.z = v.z / size;

        pmErrno = 0;
        return 0;
    }

    static public boolean isNorm(PM_CARTESIAN v) throws PmException {
        return pmCartIsNorm(v);
    }

    static public boolean pmCartIsNorm(PmCartesian v) throws PmException {
        return pmSqrt(pmSq(v.x) + pmSq(v.y) + pmSq(v.z)) - 1.0
                < UNIT_VEC_FUZZ;
    }

    static public PM_CARTESIAN proj(PM_CARTESIAN v1, PM_CARTESIAN v2) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmCartCartProj(v1, v2, vout);
        return vout;
    }

    static public int pmCartCartProj(PmCartesian v1, PmCartesian v2, PmCartesian vout) throws PmException {
        int r1, r2, r3;
        double d;

        r1 = pmCartNorm(v2, v2);
        d = pmCartCartDot(v1, v2);
        r2 = pmCartScalMult(v2, d, vout);
        r3 = pmErrno;

        if (r1 != 0 || r2 != 0 || r3 != 0) {
            pmErrno = PM_NORM_ERR;
            return pmErrno;
        }

        pmErrno = 0;
        return 0;
    }

    static public int pmCartPlaneProj(PmCartesian v, PmCartesian normal, PmCartesian vout) throws PmException {
        int r1, r2;
        PmCartesian par = new PmCartesian();

        r1 = pmCartCartProj(v, normal, par);
        r2 = pmCartCartSub(v, par, vout);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
            return pmErrno;
        }

        pmErrno = 0;
        return 0;
    }

    /* PmCylindrical Functions */
//  static public PM_CYLINDRICAL add(PM_CYLINDRICAL c1, PM_CYLINDRICAL c2) throws PmException
//  {
//    PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
//    pmCylCylAdd(c1,c2, cout);
//    return cout;
//  }
    static public int pmCylCylAdd(PmCylindrical c1, PmCylindrical c2, PmCylindrical cout) throws PmException {
        PmCartesian v1 = new PmCartesian();
        PmCartesian v2 = new PmCartesian();
        PmCartesian vout = new PmCartesian();

        pmCylCartConvert(c1, v1);
        pmCylCartConvert(c2, v2);
        pmCartCartAdd(v1, v2, vout);
        pmCartCylConvert(vout, cout);

        pmErrno = 0;
        return 0;
    }

//  static public PM_CYLINDRICAL subtract(PM_CYLINDRICAL c1, PM_CYLINDRICAL c2) throws PmException
//  {
//    PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
//    pmCylCylSub(c1,c2,cout);
//    return cout;
//  }
    static public int pmCylCylSub(PmCylindrical c1, PmCylindrical c2, PmCylindrical cout) throws PmException {
        PmCartesian v1 = new PmCartesian();
        PmCartesian v2 = new PmCartesian();
        PmCartesian vout = new PmCartesian();

        pmCylCartConvert(c1, v1);
        pmCylCartConvert(c2, v2);
        pmCartCartSub(v1, v2, vout);
        pmCartCylConvert(vout, cout);

        pmErrno = 0;
        return 0;
    }

    static public PM_CYLINDRICAL multiply(double d, PM_CYLINDRICAL c) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylScalMult(c, d, cout);
        return cout;
    }

    static public PM_CYLINDRICAL multiply(PM_CYLINDRICAL c, double d) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylScalMult(c, d, cout);
        return cout;
    }

    static public int pmCylScalMult(PmCylindrical v1, double d, PmCylindrical vout) throws PmException {
        vout.theta = v1.theta;
        vout.r = v1.r * d;
        vout.z = v1.z * d;

        if (vout.r < 0) {
            vout.r *= -1;
            if (vout.theta > 0) {
                vout.theta -= Math.PI;
            } else {
                vout.theta += Math.PI;
            }
        }

        pmErrno = 0;
        return 0;
    }

    static public PM_CYLINDRICAL divide(PM_CYLINDRICAL c, double d) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylScalDiv(c, d, cout);
        return cout;
    }

    static public int pmCylScalDiv(PmCylindrical v1, double d, PmCylindrical vout) throws PmException {
        if (d == 0.0) {

            pmPrintError("Divide by 0 in pmCylScalDiv\n", PM_DIV_ERR);

            vout.r = DBL_MAX;
            vout.z = DBL_MAX;

            pmErrno = PM_DIV_ERR;
            return pmErrno;
        }

        vout.theta = v1.theta;
        vout.r = v1.r / d;
        vout.z = v1.z / d;

        if (vout.r < 0) {
            vout.r *= -1;
            if (vout.theta > 0) {
                vout.theta -= Math.PI;
            } else {
                vout.theta += Math.PI;
            }
        }
        pmErrno = 0;
        return 0;
    }

    static public boolean pmCylCylCompare(PmCylindrical v1, PmCylindrical v2) throws PmException {
        if (debug_on) {
            System.out.println("pmCylCylCompare(" + v1 + ", " + v2 + ")");
        }
        double angle_diff = Math.abs(v1.theta - v2.theta);
        if (angle_diff >= V_FUZZ
                && Math.abs(angle_diff - Math.PI * 2) >= V_FUZZ) {
            System.out.println("angle_diff = " + angle_diff);
            return false;
        }
        if (Math.abs(v1.r - v2.r) >= V_FUZZ
                || Math.abs(v1.z - v2.z) >= V_FUZZ) {
            return false;
        }

        return true;
    }

    static public double dot(PM_CYLINDRICAL v1, PM_CYLINDRICAL v2) throws PmException {
        return pmCylCylDot(v1, v2);
    }

    static public double pmCylCylDot(PmCylindrical c1, PmCylindrical c2) throws PmException {
        PmCartesian v1 = new PmCartesian();
        PmCartesian v2 = new PmCartesian();

        pmCylCartConvert(c1, v1);
        pmCylCartConvert(c2, v2);
        return pmCartCartDot(v1, v2);
    }

    static public double mag(PM_CYLINDRICAL v1) throws PmException {
        return pmCylMag(v1);
    }

    static public double pmCylMag(PmCylindrical c) throws PmException {
        return pmSqrt(pmSq(c.r) + pmSq(c.z));
    }

    static public PM_CYLINDRICAL cross(PM_CYLINDRICAL v1, PM_CYLINDRICAL v2) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylCylCross(v1, v2, cout);
        return cout;
    }

    static public int pmCylCylCross(PmCylindrical c1, PmCylindrical c2, PmCylindrical cout) throws PmException {
        PmCartesian v1 = new PmCartesian();
        PmCartesian v2 = new PmCartesian();
        PmCartesian vout = new PmCartesian();

        pmCylCartConvert(c1, v1);
        pmCylCartConvert(c2, v2);
        pmCartCartCross(v1, v2, vout);
        pmCartCylConvert(vout, cout);

        pmErrno = 0;
        return 0;
    }

    static public PM_CYLINDRICAL neg(PM_CYLINDRICAL v) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylNeg(v, cout);
        return cout;
    }

    static public int pmCylNeg(PmCylindrical v1, PmCylindrical vout) throws PmException {
        vout.theta = v1.theta + Math.PI;
        if (vout.theta > Math.PI) {
            vout.theta -= 2 * Math.PI;
        }
        vout.r = v1.r;
        vout.z = -v1.z;

        pmErrno = 0;
        return 0;
    }

    static public PM_CYLINDRICAL norm(PM_CYLINDRICAL v) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylNorm(v, cout);
        return cout;
    }

    static public int pmCylNorm(PmCylindrical v, PmCylindrical vout) throws PmException {
        double size = pmSqrt(pmSq(v.r) + pmSq(v.z));

        if (size == 0.0) {

            pmPrintError("Zero vector in pmCylNorm\n", PM_NORM_ERR);

            vout.r = 0;
            vout.z = 0;

            pmErrno = PM_NORM_ERR;
            return pmErrno;
        }

        vout.r = v.r / size;
        vout.z = v.z / size;

        pmErrno = 0;
        return 0;
    }

    static public PM_CYLINDRICAL inv(PM_CYLINDRICAL v) throws PmException {
        PM_CYLINDRICAL cout = new PM_CYLINDRICAL();
        pmCylInv(v, cout);
        return cout;
    }

    static public int pmCylInv(PmCylindrical c, PmCylindrical cout) throws PmException {


        /*
        double size_sq = pmSq(v.r) + pmSq(v.z);

        vout.theta = v.theta;
        if (size_sq == 0.0)
        {

        pmPrintError("Zero vector in pmCylInv\n", PM_NORM_ERR);


        vout.r = DBL_MAX;
        vout.z = DBL_MAX;

        pmErrno = PM_NORM_ERR;
        return pmErrno;
        }

        vout.r = v.r / size_sq;
        vout.z = v.z / size_sq;

        pmErrno = 0;
         */
        PmCartesian v = new PmCartesian();
        pmCylCartConvert(c, v);
        PmCartesian vout = new PmCartesian();
        pmCartInv(v, vout);
        pmCartCylConvert(vout, cout);
        return 0;
    }

    static public boolean isNorm(PM_CYLINDRICAL v) throws PmException {
        return pmCylIsNorm(v);
    }

    static public boolean pmCylIsNorm(PmCylindrical v) throws PmException {
        return pmSqrt(pmSq(v.r) + pmSq(v.z)) - 1.0
                < UNIT_VEC_FUZZ;
    }


    /* angle-axis functions */
    static public PmQuaternion pmQuatAxisAngleMult(PmQuaternion q, int axis, double angle) throws PmException {
        PmQuaternion pq = new PmQuaternion();
        if (pmQuatAxisAngleMult(q, axis, angle, pq) != 0) {
            return null;
        }
        return pq;
    }

    static public int pmQuatAxisAngleMult(PmQuaternion q, int axis, double angle, PmQuaternion pq) throws PmException {
        double sh, ch;

        if (!pmQuatIsNorm(q)) {

            pmPrintError("error: non-unit quaternion in pmQuatAxisAngleMult\n", PM_NORM_ERR);

            return -1;
        }

        angle *= 0.5;
        sh = Math.sin(angle);
        ch = Math.cos(angle);

        switch (axis) {
            case PM_X:
                pq.s = ch * q.s - sh * q.x;
                pq.x = ch * q.x + sh * q.s;
                pq.y = ch * q.y + sh * q.z;
                pq.z = ch * q.z - sh * q.y;
                break;

            case PM_Y:
                pq.s = ch * q.s - sh * q.y;
                pq.x = ch * q.x - sh * q.z;
                pq.y = ch * q.y + sh * q.s;
                pq.z = ch * q.z + sh * q.x;
                break;

            case PM_Z:
                pq.s = ch * q.s - sh * q.z;
                pq.x = ch * q.x + sh * q.y;
                pq.y = ch * q.y - sh * q.x;
                pq.z = ch * q.z + sh * q.s;
                break;

            default:

                pmPrintError("error: bad axis in pmQuatAxisAngleMult\n", PM_NORM_ERR);

                return -1;
        }

        if (pq.s < 0.0) {
            pq.s *= -1.0;
            pq.x *= -1.0;
            pq.y *= -1.0;
            pq.z *= -1.0;
        }

        return 0;
    }

    /* PmRotationVector functions */
    static public PM_ROTATION_VECTOR multiply(PM_ROTATION_VECTOR r, double s) throws PmException {
        PM_ROTATION_VECTOR rout = new PM_ROTATION_VECTOR();
        pmRotScalMult(r, s, rout);
        return rout;
    }

    static public PmRotationVector multiply(double s, PM_ROTATION_VECTOR r) throws PmException {
        PM_ROTATION_VECTOR rout = new PM_ROTATION_VECTOR();
        pmRotScalMult(r, s, rout);
        return rout;
    }

    static public int pmRotScalMult(PmRotationVector r, double s, PmRotationVector rout) throws PmException {
        rout.s = r.s * s;
        rout.x = r.x;
        rout.y = r.y;
        rout.z = r.z;

        return pmErrno = 0;
    }

    static public PM_ROTATION_VECTOR divide(PM_ROTATION_VECTOR r, double s) throws PmException {
        PM_ROTATION_VECTOR rout = new PM_ROTATION_VECTOR();
        pmRotScalDiv(r, s, rout);
        return rout;
    }

    static public int pmRotScalDiv(PmRotationVector r, double s, PmRotationVector rout) throws PmException {
        if (s == 0.0) {

            pmPrintError("Divide by zero in pmRotScalDiv\n", PM_NORM_ERR);

            rout.s = DBL_MAX;
            rout.x = r.x;
            rout.y = r.y;
            rout.z = r.z;

            return pmErrno = PM_NORM_ERR;
        }

        rout.s = r.s / s;
        rout.x = r.x;
        rout.y = r.y;
        rout.z = r.z;

        return pmErrno = 0;
    }

    static public boolean isNorm(PM_ROTATION_VECTOR r) throws PmException {
        return pmRotIsNorm(r);
    }

    static public boolean pmRotIsNorm(PmRotationVector r) throws PmException {
        if (Math.abs(r.s) < RS_FUZZ
                || Math.abs(pmSqrt(pmSq(r.x) + pmSq(r.y) + pmSq(r.z))) - 1.0 < UNIT_VEC_FUZZ) {
            return true;
        }

        return false;
    }

    static public PM_ROTATION_VECTOR norm(PM_ROTATION_VECTOR r) throws PmException {
        PM_ROTATION_VECTOR rout = new PM_ROTATION_VECTOR();
        pmRotNorm(r, rout);
        return rout;
    }

    static public int pmRotNorm(PmRotationVector r, PmRotationVector rout) throws PmException {
        double size;

        size = pmSqrt(pmSq(r.x) + pmSq(r.y) + pmSq(r.z));

        if (Math.abs(r.s) < RS_FUZZ) {
            rout.s = 0.0;
            rout.x = 0.0;
            rout.y = 0.0;
            rout.z = 0.0;

            return pmErrno = 0;
        }

        if (size == 0.0) {

            pmPrintError("error: pmRotNorm size is zero\n", PM_NORM_ERR);

            rout.s = 0.0;
            rout.x = 0.0;
            rout.y = 0.0;
            rout.z = 0.0;

            return pmErrno = PM_NORM_ERR;
        }

        rout.s = r.s;
        rout.x = r.x / size;
        rout.y = r.y / size;
        rout.z = r.z / size;

        return pmErrno = 0;
    }

//    /* PmRotationMatrix functions */
//    static public int pmMatNorm(PmRotationMatrix m, PmRotationMatrix mout) throws PmException {
//        PmCartesian vout = new PmCartesian();
//        PmCartesian xunit = new PmCartesian();
//        PmCartesian yunit = new PmCartesian();
//        PmCartesian zunit = new PmCartesian();
//        int r1;
//
//        /* make x a unit vector*/
//        r1 = pmCartNorm(m.x,  mout.x);
//        if (r1 != 0) {
//            return r1;
//        }
//
//        /* make y orthonormal to x */
//        r1 = pmCartCartProj(m.y, mout.x, vout);
//        if (r1 != 0) {
//            return r1;
//        }
//        pmCartCartSub(m.y, vout, vout);
//        r1 = pmCartNorm(vout,  mout.y);
//        if (r1 != 0) {
//            return r1;
//        }
//        /* z = x cross y */
//        pmCartCartCross(mout.x, mout.y,mout.z);
//
//        return pmErrno = 0;
//    }
    static public boolean isNorm(PM_ROTATION_MATRIX m) throws PmException {
        return pmMatIsNorm(m);
    }
    static public PmRotationMatrix last_bad_rotmat = null;

    static public boolean pmMatIsNorm(PmRotationMatrix m) throws PmException {
        PmCartesian u = new PmCartesian();

        pmCartCartCross(m.x, m.y, u);

        return (pmCartIsNorm(m.x)
                && pmCartIsNorm(m.y)
                && pmCartIsNorm(m.z)
                && pmCartCartCompare(u, m.z));
    }

    static public int pmMatInv(PmRotationMatrix m, PmRotationMatrix mout) throws PmException {
        /* inverse of a rotation matrix is the transpose */

        mout.x.x = m.x.x;
        mout.x.y = m.y.x;
        mout.x.z = m.z.x;

        mout.y.x = m.x.y;
        mout.y.y = m.y.y;
        mout.y.z = m.z.y;

        mout.z.x = m.x.z;
        mout.z.y = m.y.z;
        mout.z.z = m.z.z;

        return pmErrno = 0;
    }

    static public PM_CARTESIAN multiply(PM_ROTATION_MATRIX m, PM_CARTESIAN v) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmMatCartMult(m, v, vout);
        return vout;
    }

    static public int pmMatCartMult(PmRotationMatrix m, PmCartesian v, PmCartesian vout) throws PmException {
        vout.x = m.x.x * v.x + m.y.x * v.y + m.z.x * v.z;
        vout.y = m.x.y * v.x + m.y.y * v.y + m.z.y * v.z;
        vout.z = m.x.z * v.x + m.y.z * v.y + m.z.z * v.z;

        return pmErrno = 0;
    }

    static public PM_ROTATION_MATRIX multiply(PM_ROTATION_MATRIX m1, PM_ROTATION_MATRIX m2) throws PmException {
        PM_ROTATION_MATRIX mout = new PM_ROTATION_MATRIX();
        pmMatMatMult(m1, m2, mout);
        return mout;
    }

    static public int pmMatMatMult(PmRotationMatrix m1, PmRotationMatrix m2,
            PmRotationMatrix mout) throws PmException {
        mout.x.x = m1.x.x * m2.x.x + m1.y.x * m2.x.y + m1.z.x * m2.x.z;
        mout.x.y = m1.x.y * m2.x.x + m1.y.y * m2.x.y + m1.z.y * m2.x.z;
        mout.x.z = m1.x.z * m2.x.x + m1.y.z * m2.x.y + m1.z.z * m2.x.z;

        mout.y.x = m1.x.x * m2.y.x + m1.y.x * m2.y.y + m1.z.x * m2.y.z;
        mout.y.y = m1.x.y * m2.y.x + m1.y.y * m2.y.y + m1.z.y * m2.y.z;
        mout.y.z = m1.x.z * m2.y.x + m1.y.z * m2.y.y + m1.z.z * m2.y.z;

        mout.z.x = m1.x.x * m2.z.x + m1.y.x * m2.z.y + m1.z.x * m2.z.z;
        mout.z.y = m1.x.y * m2.z.x + m1.y.y * m2.z.y + m1.z.y * m2.z.z;
        mout.z.z = m1.x.z * m2.z.x + m1.y.z * m2.z.y + m1.z.z * m2.z.z;

        return pmErrno = 0;
    }

    /* PmQuaternion functions */
    static public boolean pmQuatQuatCompare(PmQuaternion q1, PmQuaternion q2) throws PmException {

        if (!pmQuatIsNorm(q1)
                || !pmQuatIsNorm(q2)) {

            pmPrintError("Bad quaternion in pmQuatQuatCompare\n", PM_ERR);

        }

        if (Math.abs(q1.s - q2.s) < Q_FUZZ
                && Math.abs(q1.x - q2.x) < Q_FUZZ
                && Math.abs(q1.y - q2.y) < Q_FUZZ
                && Math.abs(q1.z - q2.z) < Q_FUZZ) {
            return true;
        }

        /* note (0, x, y, z) = (0, -x, -y, -z) */
        if (Math.abs(q1.s) >= QS_FUZZ
                || Math.abs(q1.x + q2.x) >= Q_FUZZ
                || Math.abs(q1.y + q2.y) >= Q_FUZZ
                || Math.abs(q1.z + q2.z) >= Q_FUZZ) {
            return false;
        }

        return true;
    }

    static public double mag(PM_QUATERNION q) throws PmException {
        return pmQuatMag(q);
    }

    static public double pmQuatMag(PmQuaternion q) throws PmException {
        double d;
        PmRotationVector r = new PmRotationVector();

        pmQuatRotConvert(q, r);
        d = r.s;
        return d;

    }

    static public PM_QUATERNION norm(PM_QUATERNION q1) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatNorm(q1, qout);
        return qout;
    }

    static public int pmQuatNorm(PmQuaternion q1, PmQuaternion qout) throws PmException {
        double size = pmSqrt(pmSq(q1.s) + pmSq(q1.x) + pmSq(q1.y) + pmSq(q1.z));

        if (size == 0.0) {

            pmPrintError("Bad quaternion in pmQuatNorm\n", PM_NORM_ERR);

            qout.s = 1;
            qout.x = 0;
            qout.y = 0;
            qout.z = 0;

            return pmErrno = PM_NORM_ERR;
        }

        if (q1.s >= 0.0) {
            qout.s = q1.s / size;
            qout.x = q1.x / size;
            qout.y = q1.y / size;
            qout.z = q1.z / size;

            return pmErrno = 0;
        } else {
            qout.s = -q1.s / size;
            qout.x = -q1.x / size;
            qout.y = -q1.y / size;
            qout.z = -q1.z / size;

            return pmErrno = 0;
        }
    }

    static public PM_QUATERNION inv(PM_QUATERNION q1) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatInv(q1, qout);
        return qout;
    }

    static public int pmQuatInv(PmQuaternion q1, PmQuaternion qout) throws PmException {
        qout.s = q1.s;
        qout.x = -q1.x;
        qout.y = -q1.y;
        qout.z = -q1.z;

        if (!pmQuatIsNorm(q1)) {

            pmPrintError("Bad quaternion in pmQuatInv\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        return pmErrno = 0;
    }

    static public boolean isNorm(PM_QUATERNION q1) throws PmException {
        return pmQuatIsNorm(q1);
    }

    static public boolean pmQuatIsNorm(PmQuaternion q1) throws PmException {
        return (Math.abs(pmSq(q1.s) + pmSq(q1.x) + pmSq(q1.y) + pmSq(q1.z) - 1.0)
                < UNIT_QUAT_FUZZ);
    }

    static public PM_QUATERNION multiply(PM_QUATERNION q, double s) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatScalMult(q, s, qout);
        return qout;
    }

    static public PM_QUATERNION multiply(double s, PM_QUATERNION q) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatScalMult(q, s, qout);
        return qout;
    }

    static public int pmQuatScalMult(PmQuaternion q, double s, PmQuaternion qout) throws PmException {
        /* need a native version; this goes through a rotation vector */
        PmRotationVector r = new PmRotationVector();
        int r1, r2, r3;

        r1 = pmQuatRotConvert(q, r);
        r2 = pmRotScalMult(r, s, r);
        r3 = pmRotQuatConvert(r, qout);

        if (r1 != 0 || r2 != 0 || r3 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public PM_QUATERNION divide(PM_QUATERNION q, double s) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatScalDiv(q, s, qout);
        return qout;
    }

    static public int pmQuatScalDiv(PmQuaternion q, double s, PmQuaternion qout) throws PmException {
        /*  need a native version; this goes through a rotation vector */
        PmRotationVector r = new PmRotationVector();
        int r1, r2, r3;

        r1 = pmQuatRotConvert(q, r);
        r2 = pmRotScalDiv(r, s, r);
        r3 = pmRotQuatConvert(r, qout);

        if (r1 != 0 || r2 != 0 || r3 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public PM_QUATERNION multiply(PM_QUATERNION q1, PM_QUATERNION q2) throws PmException {
        PM_QUATERNION qout = new PM_QUATERNION();
        pmQuatQuatMult(q1, q2, qout);
        return qout;
    }

    static public int pmQuatQuatMult(PmQuaternion q1, PmQuaternion q2, PmQuaternion qout) throws PmException {

        if (!pmQuatIsNorm(q1)) {
            pmPrintError("Bad quaternion q1=" + q1 + " in pmQuatQuatMult\n", PM_NORM_ERR);
            return pmErrno = PM_NORM_ERR;
        }
        if (!pmQuatIsNorm(q2)) {
            pmPrintError("Bad quaternion q2=" + q2 + " in pmQuatQuatMult\n", PM_NORM_ERR);
            return pmErrno = PM_NORM_ERR;
        }
        if (q1 == qout) {
            q1 = q1.clone();
        }
        if (q2 == qout) {
            q2 = q2.clone();
        }
        qout.s = q1.s * q2.s - q1.x * q2.x - q1.y * q2.y - q1.z * q2.z;

        if (qout.s >= 0.0) {
            qout.x = q1.s * q2.x + q1.x * q2.s + q1.y * q2.z - q1.z * q2.y;
            qout.y = q1.s * q2.y - q1.x * q2.z + q1.y * q2.s + q1.z * q2.x;
            qout.z = q1.s * q2.z + q1.x * q2.y - q1.y * q2.x + q1.z * q2.s;
        } else {
            qout.s *= -1;
            qout.x = -q1.s * q2.x - q1.x * q2.s - q1.y * q2.z + q1.z * q2.y;
            qout.y = -q1.s * q2.y + q1.x * q2.z - q1.y * q2.s - q1.z * q2.x;
            qout.z = -q1.s * q2.z - q1.x * q2.y + q1.y * q2.x - q1.z * q2.s;
        }
        return pmErrno = 0;
    }

    static public PM_CARTESIAN multiply(PmQuaternion q1, PM_CARTESIAN v2) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmQuatCartMult(q1, v2, vout);
        return vout;
    }

    static public int pmQuatCartMult(PmQuaternion q1, PmCartesian v2, PmCartesian vout) throws PmException {
        PmCartesian c = new PmCartesian();

        c.x = q1.y * v2.z - q1.z * v2.y;
        c.y = q1.z * v2.x - q1.x * v2.z;
        c.z = q1.x * v2.y - q1.y * v2.x;

        vout.x = v2.x + 2.0 * (q1.s * c.x + q1.y * c.z - q1.z * c.y);
        vout.y = v2.y + 2.0 * (q1.s * c.y + q1.z * c.x - q1.x * c.z);
        vout.z = v2.z + 2.0 * (q1.s * c.z + q1.x * c.y - q1.y * c.x);

        if (!pmQuatIsNorm(q1)) {

            pmPrintError("Bad quaternion in pmQuatCartMult\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        return pmErrno = 0;
    }

    /* PmPose functions*/
    static public boolean pmPosePoseCompare(PmPose p1, PmPose p2) throws PmException {

        if (!pmQuatIsNorm(p1.rot)
                || !pmQuatIsNorm(p2.rot)) {

            pmPrintError("Bad quaternion in pmPosePoseCompare\n", PM_ERR);

        }

        return (pmQuatQuatCompare(p1.rot, p2.rot)
                && pmCartCartCompare(p1.tran, p2.tran));
    }

    static public PM_POSE inv(PM_POSE p) throws PmException {
        PM_POSE pout = new PM_POSE();
        pmPoseInv(p, pout);
        return pout;
    }

    static public PM_XYA inv(PM_XYA p) throws PmException {
        return p.inv();
    }

    static public PM_XYA multiply(PM_XYA p1, PM_XYA p2) {
        PM_XYA ret = new PM_XYA();
        ret.x = p1.x + p2.x * Math.cos(p1.a) - p2.y * Math.sin(p1.a);
        ret.y = p1.y + p2.x * Math.sin(p1.a) + p2.y * Math.cos(p1.a);
        ret.a = p1.a + p2.a;
        int pa_i = 0;
        if (ret.a > PM_PI * 2) {
            pa_i = (int) (ret.a / (PM_PI * 2));
            ret.a -= PM_PI * 2 * pa_i;
        } else if (ret.a < -PM_PI * 2) {
            pa_i = (int) (-ret.a / (PM_PI * 2));
            ret.a += PM_PI * 2 * pa_i;
        }
        return ret;
    }

    static public PM_CARTESIAN multiply(PM_XYA p1, PM_CARTESIAN p2) {
        PM_CARTESIAN ret = new PM_CARTESIAN();
        ret.x = p1.x + p2.x * Math.cos(p1.a) - p2.y * Math.sin(p1.a);
        ret.y = p1.y + p2.x * Math.sin(p1.a) + p2.y * Math.cos(p1.a);
        ret.z = 0;
        return ret;
    }

    static public int pmPoseInv(PmPose p1, PmPose p2) throws PmException {
        int r1, r2;

        if (!pmQuatIsNorm(p1.rot)) {
            pmPrintError("Bad quaternion in pmPoseInv\n", PM_ERR);
        }

        r1 = pmQuatInv(p1.rot, p2.rot);
        r2 = pmQuatCartMult(p2.rot, p1.tran, p2.tran);

        p2.tran.x *= -1.0;
        p2.tran.y *= -1.0;
        p2.tran.z *= -1.0;

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }
        return pmErrno;
    }

    static public PM_CARTESIAN multiply(PM_POSE p1, PM_CARTESIAN v2) throws PmException {
        PM_CARTESIAN vout = new PM_CARTESIAN();
        pmPoseCartMult(p1, v2, vout);
        return vout;
    }

    static public int pmPoseCartMult(PmPose p1, PmCartesian v2, PmCartesian vout) throws PmException {
        int r1, r2;

        if (!pmQuatIsNorm(p1.rot)) {

            pmPrintError("Bad quaternion in pmPoseCartMult\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        r1 = pmQuatCartMult(p1.rot, v2, vout);
        r2 = pmCartCartAdd(p1.tran, vout, vout);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public PM_POSE multiply(PM_POSE p1, PM_POSE p2) throws PmException {
        PM_POSE pout = new PM_POSE();
        pmPosePoseMult(p1, p2, pout);
        return pout;
    }

    static public int pmPosePoseMult(PmPose p1, PmPose p2, PmPose pout) throws PmException {
        int r1, r2, r3;

        if (!pmQuatIsNorm(p1.rot)
                || !pmQuatIsNorm(p2.rot)) {

            pmPrintError("Bad quaternion in pmPosePoseMult\n", PM_NORM_ERR);

            return pmErrno = PM_NORM_ERR;
        }

        r1 = pmQuatCartMult(p1.rot, p2.tran, pout.tran);
        r2 = pmCartCartAdd(p1.tran, pout.tran, pout.tran);
        r3 = pmQuatQuatMult(p1.rot, p2.rot, pout.rot);

        if (r1 != 0 || r2 != 0 || r3 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    /* homogeneous transform functions */
    static public PM_HOMOGENEOUS inv(PM_HOMOGENEOUS h) throws PmException {
        PM_HOMOGENEOUS hout = new PM_HOMOGENEOUS();
        pmHomInv(h, hout);
        return hout;
    }

    static public int pmHomInv(PmHomogeneous h1, PmHomogeneous h2) throws PmException {
        int r1, r2;

        if (!pmMatIsNorm(h1.rot)) {

            pmPrintError("Bad rotation matrix in pmHomInv\n", PM_ERR);

        }

        r1 = pmMatInv(h1.rot, h2.rot);
        r2 = pmMatCartMult(h2.rot, h1.tran, h2.tran);

        h2.tran.x *= -1.0;
        h2.tran.y *= -1.0;
        h2.tran.z *= -1.0;

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;

    }

    /* line functions */
    static public int pmLineInit(PmLine line, PmPose start, PmPose end) throws PmException {
        int r1, r2 = 0;
        double mag;

        line.start = start;
        line.end = end;
        r1 = pmCartCartSub(end.tran, start.tran, line.uVec);
        mag = pmCartMag(line.uVec);
        if (IS_FUZZ(mag, CART_FUZZ)) {
            line.uVec.x = 1.0;
            line.uVec.y = 0.0;
            line.uVec.z = 0.0;
        } else {
            r2 = pmCartNorm(line.uVec, line.uVec);
        }

        /* return PM_NORM_ERR if uVec has been set to 1, 0, 0 */
        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    static public int pmLinePoint(PmLine line, double len, PmPose point) throws PmException {
        int r1, r2;

        /* return start + len * uVec */
        r1 = pmCartScalMult(line.uVec, len, point.tran);
        r2 = pmCartCartAdd(line.start.tran, point.tran, point.tran);

        if (r1 != 0 || r2 != 0) {
            pmErrno = PM_NORM_ERR;
        } else {
            pmErrno = 0;
        }

        return pmErrno;
    }

    /* circle functions */

 /*
    pmCircleInit() takes the defining parameters of a generalized circle
    and sticks them in the structure. It also computes the radius and vectors
    in the plane that are useful for other functions and that don't need
    to be recomputed every time.

    Note that the end can be placed arbitrarily, resulting in a combination of
    spiral and helical motion. There is an overconstraint between the start,
    center, and normal vector: the center vector and start vector are assumed
    to be in the plane defined by the normal vector. If this is not true, then
    it will be made true by moving the center vector onto the plane.
     */
    static public int pmCircleInit(PmCircle circle,
            PmPose start, PmPose end,
            PmCartesian center, PmCartesian normal,
            int turn) throws PmException {
        double dot;
        PmCartesian rEnd = new PmCartesian();
        PmCartesian v = new PmCartesian();
        double d;
        int r1;

        if (null == circle) {

            pmPrintError("error: pmCircleInit cirle pointer is null\n", PM_ERR);

            return pmErrno = PM_ERR;
        }


        /* adjust center */
        pmCartCartSub(start.tran, center, v);
        r1 = pmCartCartProj(v, normal, v);
        if (PM_NORM_ERR == r1) {
            /* bad normal vector-- abort */

            pmPrintError("error: pmCircleInit normal vector is 0\n", PM_ERR);

            return -1;
        }
        pmCartCartAdd(v, center, circle.center);

        /* normalize and redirect normal vector based on turns. If turn is
        less than 0, point normal vector in other direction and make
        turn positive, -1 . 0, -2 . 1, etc. */
        pmCartNorm(normal, circle.normal);
        if (turn < 0) {
            turn = -1 - turn;
            pmCartScalMult(circle.normal, -1.0, circle.normal);
        }

        /* radius */
        circle.radius = pmCartCartDisp(start.tran, circle.center);

        /* vector in plane of circle from center to start, magnitude radius */
        pmCartCartSub(start.tran, circle.center, circle.rTan);
        /* vector in plane of circle perpendicular to rTan, magnitude radius */
        pmCartCartCross(circle.normal, circle.rTan, circle.rPerp);

        /* do rHelix, rEnd */
        pmCartCartSub(end.tran, circle.center, circle.rHelix);
        pmCartPlaneProj(circle.rHelix, circle.normal, rEnd);
        circle.spiral = pmCartMag(rEnd);
        circle.spiral -= circle.radius;
        pmCartCartSub(circle.rHelix, rEnd, circle.rHelix);
        pmCartNorm(rEnd, rEnd);
        pmCartScalMult(rEnd, circle.radius, rEnd);

        /* angle */
        dot = pmCartCartDot(circle.rTan, rEnd);
        dot = dot / (circle.radius * circle.radius);
        circle.angle = Math.acos(dot);
        /* now angle is in range 0..PI . Check if cross is antiparallel to
        normal. If so, true angle is between PI..2PI. Need to subtract
        from 2PI. */
        pmCartCartCross(circle.rTan, rEnd, v);
        d = pmCartCartDot(v, circle.normal);
        if (d < 0.0) {
            circle.angle = PM_2_PI - circle.angle;
        }

        /* now add more angle for multi turns */
        if (turn > 0) {
            circle.angle += turn * 2.0 * PM_PI;
        }
        return pmErrno = 0;
    }

    public static List<PM_POSE> findStillPoints(List<Double> tlist,
            List<PM_POSE> posList, double velThresh, int avg_thresh,
            List<Double> stillTimes) throws PmException {

        Iterator<Double> t_it = tlist.iterator();
        Iterator<PM_POSE> p_it = posList.iterator();
        PM_POSE last_pose = p_it.next();
        double last_time = t_it.next();
        PM_POSE pAvg = new PM_POSE();
        LinkedList<PM_POSE> stillList = new LinkedList<PM_POSE>();
        int under_count = 0;
        int over_count = 0;
        double tAvg = 0;
        System.err.println("t,vel,underThresh,under_count,over_count");
        while (p_it.hasNext() && t_it.hasNext()) {
            PM_POSE p = p_it.next();
            double t = t_it.next();
            if (t <= last_time) {
                continue;
            }
            double vel = mag(subtract(p.tran, last_pose.tran)) / (t - last_time);
            System.err.println(t + "," + vel + "," + ((vel < velThresh) ? 0 : 1) + "," + under_count + "," + over_count);
            if (vel < velThresh) {
                pAvg.tran = add(pAvg.tran, p.tran);
                Posemath.pmQuatQuatMult(pAvg.rot, p.rot, pAvg.rot);
                Posemath.pmQuatNorm(pAvg.rot, pAvg.rot);
                tAvg += t;
                under_count++;
                if (under_count > avg_thresh || under_count > over_count) {
                    over_count = 0;
                }
            } else {
                if (over_count > avg_thresh || over_count > under_count) {
                    if (under_count > avg_thresh) {
                        pAvg.tran.x /= under_count;
                        pAvg.tran.y /= under_count;
                        pAvg.tran.z /= under_count;
                        tAvg /= under_count;
                        Posemath.pmQuatScalMult(pAvg.rot, 1.0 / under_count, pAvg.rot);
                        stillList.add(pAvg);
                        if (null != stillTimes) {
                            stillTimes.add(tAvg);
                        }
                    }
                    pAvg = new PM_POSE();
                    tAvg = 0;
                    under_count = 0;
                }
                over_count++;
            }
            last_time = t;
            last_pose = p.clone();
        }
        return stillList;
    }

    public static List<PM_POSE> prePostMultiplyList(List<? extends PM_POSE> l,
            PM_POSE pre, PM_POSE post) {
        LinkedList<PM_POSE> lout = new LinkedList<PM_POSE>();
        try {
            for (PM_POSE p : l) {
                PM_POSE pout = Posemath.multiply(pre, Posemath.multiply(p, post));
                lout.add(pout);
            }
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return lout;
    }

    static public double[][] poseListToHomMats(List<? extends PM_POSE> l) {
        try {
            double ddout[][] = new double[4][4 * l.size()];
            int i = 0;
            for (PM_POSE pose : l) {
                double dd4[][] = pose.toMatdd();
                System.arraycopy(dd4[0], 0, ddout[0], 4 * i, 4);
                System.arraycopy(dd4[1], 0, ddout[1], 4 * i, 4);
                System.arraycopy(dd4[2], 0, ddout[2], 4 * i, 4);
                System.arraycopy(dd4[3], 0, ddout[3], 4 * i, 4);
                i++;
            }
            return ddout;
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return null;
    }

    static public List<PM_POSE> poseListChk(PM_POSE X,
            List<? extends PM_POSE> A,
            PM_POSE Y,
            List<? extends PM_POSE> B) throws PmException {
        return poseListPoseListMultiply(
                prePostMultiplyList(A,
                        inv(Y),
                        X),
                poseListInv(B));
    }

    static public List<PM_POSE> poseListRandom(int size,
            double angle_noise,
            double cart_noise) throws PmException {
        PM_ROTATION_VECTOR rv = new PM_ROTATION_VECTOR();
        PM_ROTATION_VECTOR rvn = new PM_ROTATION_VECTOR();
        PM_CARTESIAN c = new PM_CARTESIAN();
        PM_CARTESIAN cn = new PM_CARTESIAN();
        LinkedList<PM_POSE> plout = new LinkedList<PM_POSE>();
        Random r = new Random();
        for (int i = 0; i < size; i++) {

            rv.x = r.nextDouble() / Math.sqrt(3);
            rv.y = r.nextDouble() / Math.sqrt(3);
            rv.z = r.nextDouble() / Math.sqrt(3);
            rv.s = r.nextGaussian() * angle_noise;
            pmRotNorm(rv, rvn);
            c.x = r.nextDouble() / Math.sqrt(3);
            c.y = r.nextDouble() / Math.sqrt(3);
            c.z = r.nextDouble() / Math.sqrt(3);
            pmCartNorm(c, cn);
            double cmag = r.nextGaussian() * cart_noise;
            cn.x *= cmag;
            cn.y *= cmag;
            cn.z *= cmag;
            PM_POSE pr = new PM_POSE(cn.clone(), new PM_QUATERNION(rvn));
            plout.add(pr);
        }
        return plout;
    }

    static public PM_POSE toPose(double dd[][]) throws PmException {
        return new PM_POSE(dd);
    }

    static public List<PM_POSE> homMatsPrePostMult(double X[][],
            double A[][],
            double Y[][]) throws PmException {
        return prePostMultiplyList(homMatsToPoseList(A),
                inv(toPose(Y)),
                toPose(X));
    }

    static public List<PM_POSE> homMatsChk(double X[][],
            double A[][],
            double Y[][],
            double B[][]) throws PmException {
        return poseListPoseListMultiply(
                homMatsPrePostMult(X, A, Y),
                poseListInv(homMatsToPoseList(B)));
    }

    static public List<PM_CARTESIAN> poseListToCartList(List<? extends PM_POSE> plin) {
        LinkedList<PM_CARTESIAN> clout = new LinkedList<PM_CARTESIAN>();
        for (PM_POSE p : plin) {
            clout.add(new PM_CARTESIAN(p.tran));
        }
        return clout;
    }

    static public List<PM_RPY> poseListToRpyList(List<? extends PM_POSE> plin) {
        LinkedList<PM_RPY> clout = new LinkedList<PM_RPY>();
        try {
            for (PM_POSE p : plin) {
                clout.add(new PM_RPY(p.rot));
            }
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return clout;
    }

    static public double[] cartListToMagArray(List<? extends PM_CARTESIAN> clin) {
        double ma[] = new double[clin.size()];
        for (int i = 0; i < clin.size(); i++) {
            ma[i] = mag(clin.get(i));
        }
        return ma;
    }

    static public double[] rpyListToMagArray(List<? extends PM_RPY> rlin) {
        double ma[] = new double[rlin.size()];
        try {
            for (int i = 0; i < rlin.size(); i++) {
                PM_ROTATION_VECTOR rv = new PM_ROTATION_VECTOR();
                Posemath.pmRpyRotConvert(rlin.get(i), rv);
                ma[i] = rv.s;
            }
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return ma;
    }

    static public double mean(List<Double> l) {
        double total = 0;
        for (double d : l) {
            total += d;
        }
        return total / l.size();
    }

    static public double mean(Double da[]) {
        double total = 0;
        for (double d : da) {
            total += d;
        }
        return total / da.length;
    }

    static public double mean(double da[]) {
        double total = 0;
        for (double d : da) {
            total += d;
        }
        return total / da.length;
    }

    static public double stddev(double da[]) {
        Math.abs(PI);
        double total = 0;
        double total2 = 0;
        for (double d : da) {
            total += d;
            total2 += d * d;
        }
        return Math.sqrt(total2 * da.length - total * total) / da.length;
    }

    static public double stddev(Double da[]) {
        Math.abs(PI);
        double total = 0;
        double total2 = 0;
        for (double d : da) {
            total += d;
            total2 += d * d;
        }
        return Math.sqrt(total2 * da.length - total * total) / da.length;
    }

    static public double stddev(List<Double> da) {
        Math.abs(PI);
        double total = 0;
        double total2 = 0;
        for (double d : da) {
            total += d;
            total2 += d * d;
        }
        return Math.sqrt(total2 * da.size() - total * total) / da.size();
    }

    public static List<PM_POSE> poseListPoseListMultiply(List<? extends PM_POSE> pl1,
            List<? extends PM_POSE> pl2) {
        Iterator<? extends PM_POSE> it1 = pl1.iterator();
        Iterator<? extends PM_POSE> it2 = pl2.iterator();
        LinkedList<PM_POSE> pl = new LinkedList<PM_POSE>();
        try {
            while (it1.hasNext() && it2.hasNext()) {
                pl.add(multiply(it1.next(), it2.next()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return pl;
    }

    public static List<PM_POSE> poseListInv(List<? extends PM_POSE> plin) {
        Iterator<? extends PM_POSE> it1 = plin.iterator();
        LinkedList<PM_POSE> plout = new LinkedList<PM_POSE>();
        try {
            while (it1.hasNext()) {
                plout.add(inv(it1.next()));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return plout;
    }

    public static List<PM_POSE> homMatsToPoseList(double dd[][]) {
        int i = 0;
        try {
            if (null == dd || dd.length != 4
                    || dd[0].length < 1
                    || dd[0].length % 4 != 0) {
                throw new IllegalArgumentException("homMatsToPoseList() argument must be a 4x4n matrix.");
            }
            LinkedList<PM_POSE> pl = new LinkedList<PM_POSE>();
            for (i = 0; i < dd[0].length; i += 4) {
                double ddout[][] = new double[4][4];
                System.arraycopy(dd[0], i, ddout[0], 0, 4);
                System.arraycopy(dd[1], i, ddout[1], 0, 4);
                System.arraycopy(dd[2], i, ddout[2], 0, 4);
                System.arraycopy(dd[3], i, ddout[3], 0, 4);
                pl.add(new PM_POSE(ddout));
            }
            return pl;
        } catch (PmException pmException) {
            pmException.printStackTrace();
            System.err.println("i = " + i);
        }
        return null;
    }

    static public List<PM_POSE> csvToPoseListF(File f,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) throws Exception {
        BufferedReader br = new BufferedReader(new FileReader(f));
        String line = br.readLine();
        LinkedList<PM_POSE> pose_list = new LinkedList<PM_POSE>();
        int line_num = 0;
        while ((line = br.readLine()) != null) {
            line_num++;
            String toks[] = line.split(",");
            double x = 0;
            double y = 0;
            double z = 0;
            double roll = 0;
            double pitch = 0;
            double yaw = 0;
            try {
                x = Double.valueOf(toks[x_pos]);
                y = Double.valueOf(toks[y_pos]);
                z = Double.valueOf(toks[z_pos]);
                roll = Math.toRadians(Double.valueOf(toks[roll_pos]));
                pitch = Math.toRadians(Double.valueOf(toks[pitch_pos]));
                yaw = Math.toRadians(Double.valueOf(toks[yaw_pos]));
            } catch (Exception e) {
                System.err.println(f.getName() + ":" + line_num + " bad line: " + line);
                System.err.println("toks.length=" + toks.length);
                System.err.println("");
                throw e;
            }
            PM_POSE p = new PM_POSE(new PmCartesian(x, y, z),
                    new PmRpy(roll, pitch, yaw));
            if (!Posemath.pmQuatIsNorm(p.rot)) {
                throw new RuntimeException("Bad pose on line :" + line);
            }
            pose_list.add(p);
        }
        br.close();
        return pose_list;
    }

    static public List<PM_POSE> csvToPoseList(String filename,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) throws Exception {
        return csvToPoseListF(new File(filename),
                x_pos, y_pos, z_pos,
                roll_pos, pitch_pos, yaw_pos);
    }

    static public List<PM_POSE> csvWithTimeToPoseListF(File f,
            List<Double> timeList,
            int time_pos,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) throws Exception {
        BufferedReader br = new BufferedReader(new FileReader(f));
        String line = br.readLine();
        LinkedList<PM_POSE> pose_list = new LinkedList<PM_POSE>();
        PM_POSE last_pose = null;
        Iterator<Double> time_it = timeList.iterator();
        double time_from_list = time_it.next();
        double last_time = 0;
        while ((line = br.readLine()) != null && time_it.hasNext()) {
            String toks[] = line.split(",");
            double time = Double.valueOf(toks[time_pos]);
            double x = Double.valueOf(toks[x_pos]);
            double y = Double.valueOf(toks[y_pos]);
            double z = Double.valueOf(toks[z_pos]);
            double roll = Math.toRadians(Double.valueOf(toks[roll_pos]));
            double pitch = Math.toRadians(Double.valueOf(toks[pitch_pos]));
            double yaw = Math.toRadians(Double.valueOf(toks[yaw_pos]));

            PM_POSE p = new PM_POSE(new PmCartesian(x, y, z),
                    new PmRpy(roll, pitch, yaw));
            if (!Posemath.pmQuatIsNorm(p.rot)) {
                throw new RuntimeException("Bad pose on line :" + line);
            }
            while (time_from_list < time) {
                if (last_pose != null) {
                    PM_POSE avgPose
                            = Posemath.weightedAvg(last_pose,
                                    Math.abs(last_time - time_from_list) / (time - last_time),
                                    p,
                                    Math.abs(time - time_from_list) / (time - last_time));
                    pose_list.add(avgPose);
                } else {
                    pose_list.add(p);
                }
                time_from_list = time_it.next();
            }
            last_pose = p.clone();
            last_time = time;
        }
        if (null != last_pose) {
            while (time_it.hasNext()) {
                pose_list.add(last_pose.clone());
                time_from_list = time_it.next();
            }
            pose_list.add(last_pose.clone());
        }
        br.close();
        return pose_list;
    }

    static public List<PM_POSE> csvWithTimeToPoseList(String filename,
            List<Double> timeList,
            int time_pos,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) throws Exception {
        return csvToPoseListF(new File(filename),
                x_pos, y_pos, z_pos,
                roll_pos, pitch_pos, yaw_pos);
    }

    static public List<Double> csvToListF(File f,
            int pos) throws Exception {
        BufferedReader br = new BufferedReader(new FileReader(f));
        String line = br.readLine();
        LinkedList<Double> time_list = new LinkedList<Double>();
        while ((line = br.readLine()) != null) {
            String toks[] = line.split(",");
            double time = Double.valueOf(toks[pos]);
            time_list.add(time);
        }
        br.close();
        return time_list;
    }

    static public List<Double> csvToList(String filename,
            int pos) throws Exception {
        return csvToListF(new File(filename),
                pos);
    }
    static private PM_POSE ck1 = null;

    static public PM_POSE getCk1Pose() {
        try {
            if (null == ck1) {
                ck1 = new PM_POSE(new PmCartesian(1.0, 2.0, 3.0),
                        new PmRpy(0, 0, Math.PI / 6));
            }
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return ck1;
    }
    static private PM_POSE ck2 = null;

    static public PM_POSE getCk2Pose() {
        try {
            if (null == ck2) {
                ck2 = new PM_POSE(new PmCartesian(-3, -2, -1),
                        new PmRpy(0, 0, -Math.PI / 4));
            }
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return ck2;
    }

    static public double[][] csvToHomMatsChk(String filename,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) {
        try {
            List<PM_POSE> pose_list
                    = csvToPoseList(filename,
                            x_pos,
                            y_pos,
                            z_pos,
                            roll_pos,
                            pitch_pos,
                            yaw_pos);
            pose_list = prePostMultiplyList(pose_list,
                    getCk1Pose(),
                    getCk2Pose());
            return poseListToHomMats(pose_list);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    static public double[][] csvToHomMats(String filename,
            int x_pos,
            int y_pos,
            int z_pos,
            int roll_pos,
            int pitch_pos,
            int yaw_pos) {
        try {
            List<PM_POSE> pose_list
                    = csvToPoseList(filename,
                            x_pos,
                            y_pos,
                            z_pos,
                            roll_pos,
                            pitch_pos,
                            yaw_pos);
            return poseListToHomMats(pose_list);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    
    

    /*
    pmCirclePoint() returns the vector to the point at the given angle along
    the circle. If the circle is a helix or spiral or combination, the
    point will include interpolation off the actual circle.
     */
    static public int pmCirclePoint(PmCircle circle, double angle, PmPose point) throws PmException {
        PmCartesian par = new PmCartesian();
        PmCartesian perp = new PmCartesian();
        double scale;

        if (null == circle
                || null == point) {

            pmPrintError("error: pmCirclePoint circle or point pointer is null\n", PM_ERR);

            return pmErrno = PM_ERR;
        }


        /* compute components rel to center */
        pmCartScalMult(circle.rTan, Math.cos(angle), par);
        pmCartScalMult(circle.rPerp, Math.sin(angle), perp);

        /* add to get radius vector rel to center */
        pmCartCartAdd(par, perp, point.tran);

        /* get scale for spiral, helix interpolation */
        if (circle.angle == 0.0) {
            pmPrintError("error: pmCirclePoint angle is zero\n", PM_ERR);

            return pmErrno = PM_DIV_ERR;
        }

        scale = angle / circle.angle;

        /* add scaled vector in radial dir for spiral */
        pmCartNorm(point.tran, par);
        pmCartScalMult(par, scale * circle.spiral, par);
        pmCartCartAdd(point.tran, par, point.tran);

        /* add scaled vector in helix dir */
        pmCartScalMult(circle.rHelix, scale, perp);
        pmCartCartAdd(point.tran, perp, point.tran);

        /* add to center vector for final result */
        pmCartCartAdd(circle.center, point.tran, point.tran);

        return pmErrno = 0;
    }

    public static PmCartesian point_on_line(PM_LINE line, PmCartesian p) {
        PmCartesian s = line.start;
        PmCartesian u = line.uVec;
        PmCartesian sp = subtract(p, s);
        double d = dot(sp, u);
        PmCartesian inc = multiply(u, d);
        PmCartesian result = add(s, inc);
        return result;
    }

    public static PM_CARTESIAN point_on_line(PM_LINE line, PM_CARTESIAN p) {
        PM_CARTESIAN s = line.start;
        PM_CARTESIAN u = line.uVec;
        PM_CARTESIAN sp = subtract(p, s);
        double d = dot(sp, u);
        PM_CARTESIAN inc = multiply(u, d);
        PM_CARTESIAN result = add(s, inc);
        return result;
    }

    public static PM_CARTESIAN point_on_line(PM_CARTESIAN s, PM_CARTESIAN u, PM_CARTESIAN p) {
        PM_CARTESIAN sp = subtract(p, s);
        double d = dot(sp, u);
        PM_CARTESIAN inc = multiply(u, d);
        PM_CARTESIAN result = add(s, inc);
        return result;
    }

    public static double dist_from_line(PM_LINE line, PM_CARTESIAN c) {
        PM_CARTESIAN p = point_on_line(line.start, line.uVec, c);
        return Posemath.mag(Posemath.subtract(p, c));
    }

    public static PM_CARTESIAN point_on_line_segment(PM_CARTESIAN s, PM_CARTESIAN e, PM_CARTESIAN p) {
        PM_CARTESIAN sp = subtract(p, s);
        PM_CARTESIAN se = subtract(e, s);
        double se_mag = mag(se);
        if (se_mag <= Math.sqrt(Double.MIN_NORMAL)) {
            return s;
        }
        double u = dot(sp, se) / (se_mag * se_mag);
        if (u < 0) {
            return s;
        }
        if (u > 1) {
            return e;
        }
        PM_CARTESIAN inc = multiply(se, u);
        PM_CARTESIAN result = add(s, inc);
        return result;
    }

    public static double[][] tstArray() {
        LinkedList<PM_POSE> pose_list = new LinkedList<PM_POSE>();
        try {
            pose_list.add(
                    new PM_POSE(new PmCartesian(1.0, 2.0, 3.0),
                            new PmRpy(0, 0, Math.PI / 6)));
            pose_list.add(
                    new PM_POSE(new PmCartesian(1.0, 2.0, 3.0),
                            new PmRpy(0, 0, Math.PI / 6)));
        } catch (PmException pmException) {
            pmException.printStackTrace();
        }
        return Posemath.poseListToHomMats(pose_list);
    }

    static public PM_CARTESIAN intersection(PM_LINE l1, PM_LINE l2) throws PmException {
        double udot = dot(l1.uVec, l2.uVec);
        if (Math.abs(udot - 1) < Double.MIN_NORMAL) {
            throw new PmException(PmException.PM_DIV_ERR, "Can't find intersection of parallel lines.");
        }
        PM_CARTESIAN c1 = point_on_line(l1, l2.start);
        PM_CARTESIAN diff2 = subtract(c1, l2.start);
        double diff2mag = mag(diff2);
        PM_CARTESIAN intersect1 = add(c1, multiply(l1.uVec, diff2mag * udot * 1.0 / Math.sqrt(1 - udot * udot)));
        PM_CARTESIAN cc1 = point_on_line(l2, intersect1);
        PM_CARTESIAN intersect2 = add(c1, multiply(l1.uVec, diff2mag * udot * -1.0 / Math.sqrt(1 - udot * udot)));
        PM_CARTESIAN cc2 = point_on_line(l2, intersect2);
        PM_CARTESIAN intersect = null;
        if (mag(subtract(cc2, intersect2)) > mag(subtract(cc1, intersect1))) {
            intersect = intersect1;
        } else {
            intersect = intersect2;
        }
        return intersect;
    }

    public static boolean in_line_segment(PM_LINE l,
            PM_CARTESIAN c1, double tol) throws Exception {
        PM_CARTESIAN diff = subtract(c1, l.start);
        if (mag(diff) <= Double.MIN_NORMAL) {
            return true;
        }
        PM_CARTESIAN diffnorm = norm(diff);
        double vvdot = dot(diffnorm, l.uVec);
        if (vvdot <= 0) {
            return false;
        }
        if (mag(diff) > mag(subtract(l.end, l.start))) {
            return false;
        }
        PM_CARTESIAN p = point_on_line_segment(l.start, l.end, c1);
        return (Posemath.mag(Posemath.subtract(c1, p)) < tol);
    }

    PmRpy norm(PmRpy rpy) {
        PmRpy rpy_clone = rpy.clone();
        rpy_clone.norm();
        return rpy_clone;
    }
    
    
    
}
