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
   testpm.java

   Test code for Java version of pose math library

   Modification history:

   7-Feb-1997  FMP removed AXIS; mathQ -> pmQuat
   30-Jan-1997  FMP created
*/

package rcs.posemath;

class testpm
{

  static public void testAssert(boolean test) throws Exception
  {
    if(test)
      {
        return;
      }
    throw new Exception("Assertion Failed!");
  }

  static public void testAssert(boolean test, String msg)
  {
    if(test)
      {
        return;
      }
    System.out.println("Assertion failed! : "+msg);
    Thread.dumpStack();
  }

  static void testPrint()
  {
    try
      {
        PmCartesian  v = new PmCartesian(1.0, 2.0, 3.0);
        double dbl = 3.14;
        PmQuaternion quat = new PmQuaternion(1., 2., 3., 4.);
        PmRotationMatrix  mat = new PmRotationMatrix(
                                                     1., 2., 3.,
                                                     4., 5., 6.,
                                                     7., 8., 9.
                                                     );

        PmPose pose = new PmPose(1., 2., 3., 4., 5., 6., 7.);

        Posemath.pmQuatNorm(quat,quat);
        Posemath.pmQuatNorm(pose.rot, pose.rot);

        System.out.println("vector = "+v);
        System.out.println("quat = "+quat);
        System.out.println("mat = "+mat);
        System.out.println("pose = "+pose);
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }

  // PM_CARTESIAN tests
  static void testCart()
  {
    try
      {
        double d;
        PM_CARTESIAN v1 = new PM_CARTESIAN(1, 2, 3);
        PM_CARTESIAN v2 = new PM_CARTESIAN(1, 2, 3);
        PM_CARTESIAN pv;

        // test arg ctor
        testAssert(v1.equals(v2));

        // test new arg ctor
        pv = new PM_CARTESIAN(4, 5, 6);
        testAssert((new PM_CARTESIAN(4, 5, 6)).equals(pv));


        d = v2.x;
        v2.x = v2.z;
        v2.z = v2.y;
        v2.y = d;

        // test assignment
        v1 = new PM_CARTESIAN(0, 0, 0);
        pv = new PM_CARTESIAN(0, 0, 0);
        pv = v1 = (PM_CARTESIAN) v2.clone();
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v1));
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v2));
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(pv));

        // test unary operators
        pv = Posemath.neg(v1);
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v1));
        testAssert((new PM_CARTESIAN(-3, -1, -2)).equals(pv));
        pv = v1;
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v1));
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(pv));

        // test +/-
        Posemath.pmCartCartAdd(v1, v2, v1);
        testAssert((new PM_CARTESIAN(6, 2, 4)).equals(v1));
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v2));
        Posemath.pmCartCartSub(v2, v1, v2);
        testAssert((new PM_CARTESIAN(6, 2, 4)).equals(v1));
        testAssert((new PM_CARTESIAN(-3, -1, -2)).equals(v2));

        // test scalar *, /
        v1 = Posemath.divide(v1,2.);
        testAssert((new PM_CARTESIAN(3, 1, 2)).equals(v1));
        v2 = Posemath.multiply(v2,2.);
        testAssert((new PM_CARTESIAN(-6, -2, -4)).equals(v2));

        // test ==
        v1 = (PM_CARTESIAN) v2.clone();
        testAssert(v1.equals(v2));
        v1 = Posemath.add(v1, Posemath.multiply(new PM_CARTESIAN(1.,1.,1.), Posemath.V_FUZZ * 0.99));
        //v1 = v1 + PM_CARTESIAN(1, 1, 1) * V_FUZZ * 0.99;
        testAssert(v1.equals(v2));
        v1 = v2;
        v1 = Posemath.add(v1, Posemath.multiply(new PM_CARTESIAN(1.,1.,1.), Posemath.V_FUZZ * 1.01));
        //v1 = v1 + PM_CARTESIAN(1, 1, 1) * V_FUZZ * 1.01;
        testAssert(!v1.equals(v2));

        // test dot
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(4, 5, 6);
        testAssert(32 == Posemath.dot(v1, v2));

        // test cross
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(4, 5, 6);
        testAssert((new PM_CARTESIAN(-3, 6, -3)).equals(Posemath.cross(v1, v2)));

        // test mag
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(4, 5, 6);
        testAssert(Math.sqrt(14) == Posemath.mag(v1));

        // test norm
        v1 = Posemath.norm(new PM_CARTESIAN(1., 2., 3.));
        testAssert(1.0 == Posemath.mag(v1));

        // test disp
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(4, 5, 6);
        testAssert(Math.sqrt(27.) == Posemath.disp(v1, v2));

        // test inv
        v1 = new PM_CARTESIAN(1, 1, 1);
        testAssert(1. == Posemath.dot(v1, Posemath.inv(v1)));
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }

  static void testCyl()
  {
    try
      {

        PM_CYLINDRICAL c1;
        PM_CYLINDRICAL c2= new PM_CYLINDRICAL(Math.PI/2, 2, 3);
        PM_CYLINDRICAL pv;
        PM_CARTESIAN v1, v2;

        // test arg ctor
        testAssert((new PM_CYLINDRICAL(Math.PI/2, 2, 3)).equals(c2));

        // test new arg ctor
        pv = new PM_CYLINDRICAL(Math.PI/4, 5, 6);
        testAssert((new PM_CYLINDRICAL(Math.PI/4, 5, 6)).equals(pv));

        // test assignment
        c1 = new PM_CYLINDRICAL(Math.PI/2, 1, 2);
        c2 = new PM_CYLINDRICAL(0, 0, 0);
        pv = new PM_CYLINDRICAL(0, 0, 0);
        pv = c2 = c1;
        testAssert( (new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(c1) );
        testAssert( (new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(c2) );
        testAssert( (new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(pv) );

        // test unary operators
        pv = Posemath.neg(c1);
        testAssert((new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(c1));
        testAssert((new PM_CYLINDRICAL(-Math.PI/2, 1, -2)).equals(pv));
        pv = c1;
        testAssert((new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(c1));
        testAssert((new PM_CYLINDRICAL(Math.PI/2, 1, 2)).equals(pv));

        // test +/-
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(4, 5, 6);

//        c1 = Posemath.toCyl(v1);
//        c2 = Posemath.toCyl(v2);

        v1 = Posemath.add(v1, v2);
//        c1 = Posemath.add(c1, c2);

        testAssert((new PM_CARTESIAN(c1)).equals(v1));
        testAssert(c1.equals(new PM_CYLINDRICAL(v1)));

        // test scalar *, /
        c1 = new PM_CYLINDRICAL(-Math.PI/4, 2., 3.);
        c2 = Posemath.divide(c1,  2.0);
        testAssert((new PM_CYLINDRICAL(-Math.PI/4, 1, 1.5)).equals(c2));
        c2 = Posemath.multiply(c1, 2.0);
        testAssert((new PM_CYLINDRICAL(-Math.PI/4, 4, 6)).equals(c2));

        // test dot
        c1 = new PM_CYLINDRICAL(1, 2, 3);
        v1 = new PM_CARTESIAN(c1);
        c2 = new PM_CYLINDRICAL(4, 5, 6);
        v2 = new PM_CARTESIAN(c2);
        testAssert(Posemath.dot(c1, c2) == Posemath.dot(v1, v2));

        // test cross
        c1 = new PM_CYLINDRICAL(1, 2, 3);
        v1 = new PM_CARTESIAN(c1);
        c2 = new PM_CYLINDRICAL(4, 5, 6);
        v2 = new PM_CARTESIAN(c2);
        testAssert(Posemath.cross(c1, c2).equals(Posemath.cross(v1, v2)));

        // test mag
        c1 = new PM_CYLINDRICAL(1, 2, 3);
        testAssert(Math.sqrt(13) == Posemath.mag(c1));

        // test norm
        c1 = Posemath.norm(new PM_CYLINDRICAL(1., 2., 3.));
        testAssert(1.0 == Posemath.mag(c1));

        // test inv
        c1 = new PM_CYLINDRICAL(1., 1., 1.);
        testAssert(1. == Posemath.dot(c1, Posemath.inv(c1)));
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }

  static void testQuat()
  {
    try
      {
        double d;
        PM_CARTESIAN v1;
        PM_CARTESIAN v2;
        PM_QUATERNION q1;
        PM_QUATERNION q2 = new PM_QUATERNION(1, 0, 0, 0);
        PM_QUATERNION pq;

        // test arg ctor
        testAssert((new PM_QUATERNION(1, 0, 0, 0)).equals(q2));

        // test new arg ctor
        pq = new PM_QUATERNION(1, 0, 0, 0);
        testAssert((new PM_QUATERNION(1, 0, 0, 0)).equals(pq));

        // test indexing
        q1 = q2;
        testAssert(q1.equals(q2));

        // test assignment
        q1 = new PM_QUATERNION(1, 0, 0, 0);
        q2 = new PM_QUATERNION(2, 3, 4, 5);
        pq = new PM_QUATERNION(6, 7, 8, 9);
        pq = q2 = q1;
        testAssert((new PM_QUATERNION(1, 0, 0, 0)).equals(q1));
        testAssert((new PM_QUATERNION(1, 0, 0, 0)).equals(q2));
        testAssert((new PM_QUATERNION(1, 0, 0, 0)).equals(pq));

        // test rotation vector assignment
        /*                                 .
                                           quaternion buffs will know that 90  rotations about X, Y, and Z
                                           axes are
                                           (sqrt(2)/2, sqrt(2)/2,         0,         0),
                                           (sqrt(2)/2,         0, sqrt(2)/2,         0), and
                                           (sqrt(2)/2,         0,         0, sqrt(2)/2), respectively.
                                           */
        d = Math.sqrt(2.0) / 2.0;
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 1, 0, 0));
        testAssert((new PM_QUATERNION(d, d, 0, 0)).equals(q1));
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 0, 1, 0));
        testAssert((new PM_QUATERNION(d, 0, d, 0)).equals(q1));
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 0, 0, 1));
        testAssert((new PM_QUATERNION(d, 0, 0, d)).equals(q1));

        // test unary operators
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 1, 1, 1));
        q2 = q1;
        testAssert(q1.equals(q2));

        // test Q * Q and Q * V
        v1 = new PM_CARTESIAN(1, 2, 3);
        v2 = new PM_CARTESIAN(1, 2, 3);
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 0.5, 1, 1));
        q2 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 1, 0.5, 1));
        pq = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2, 1, 1, 0.5));
        v1 = Posemath.multiply(q1, v1);
        v1 = Posemath.multiply(q2, v1);
        v1 = Posemath.multiply(pq, v1);
        v2 = Posemath.multiply(pq , Posemath.multiply(q2 , Posemath.multiply(q1 , v2)));
        testAssert(v1.equals(v2));

        // test scaling
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2,       1., -1., 0.5));
        q2 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2 / 2.0, 1., -1., 0.5));
        q1 = Posemath.divide(q1,  2.0);
        testAssert(q1.equals(q2));
        q1 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2,       1., -1., 0.5));
        q2 = Posemath.toQuat(new PM_ROTATION_VECTOR(Math.PI/2 * 2.0, 1., -1., 0.5));
        q1 = Posemath.multiply(q1, 2.0);
        testAssert(q1.equals(q2));

        // test norm, isNorm
        q1.s = 1;
        q1.x = 2;
        q1.y = 3;
        q1.z = 4;
        testAssert(! Posemath.isNorm(q1));
        q1 = Posemath.norm(q1);
        testAssert(Posemath.isNorm(q1));

        // test inv
        q1 = new PM_QUATERNION(2, 3, 4, 5);
        q2 = Posemath.inv(q1);
        testAssert(Posemath.multiply(q1,q2).equals(new PM_QUATERNION(1, 0, 0, 0)));
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }

  static void testMat()
  {
    try
      {

        PM_ROTATION_MATRIX m1 = new PM_ROTATION_MATRIX(new PM_ROTATION_VECTOR(Math.PI/2, 1., 2., 3.));
        PM_ROTATION_MATRIX m2 = new PM_ROTATION_MATRIX(new PM_ROTATION_VECTOR(-Math.PI/4, 2., 0., -1.));
        PM_ROTATION_MATRIX m3;
        PM_QUATERNION q1 = new PM_QUATERNION(new PM_ROTATION_VECTOR(Math.PI/2, 1., 2., 3.));
        PM_QUATERNION q2 = new PM_QUATERNION(new PM_ROTATION_VECTOR(-Math.PI/4, 2., 0., -1.));
        PM_QUATERNION q3;

        // use implicit QQ mult
        m3 = Posemath.multiply(m1, m2);

        // use explicit QQ mult
        q3 = Posemath.multiply(q1, q2);

        // should be equal
        testAssert(q3.equals(m3));

        // set them equal via conversion
        q1 = Posemath.toQuat(m3);
        testAssert(q1.equals(q3));
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

  static void testHom()
  {
    try
      {
        PM_HOMOGENEOUS h1, h2;
        PM_POSE p1, p2;

        p1 = new PM_POSE(new PM_CARTESIAN(1., 2., 3.), new PM_QUATERNION(new PM_ROTATION_VECTOR(Math.PI/2, 1., 2., 3.)));
        h1 = Posemath.toHom(p1);
        testAssert(p1.equals(h1));

        //  Negation function not implemented for PM_HOMOGENEOUS
        // h2 = Posemath.neg(h1);
        // testAssert(Posemath.mult(h1,  h2).equals(new PM_POSE(0., 0., 0., 1., 0., 0., 0.)));
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }

  static void testOther()
  {
    try
      {

        PM_RPY rpy1 = new PM_RPY();
        PM_RPY rpy2 = new PM_RPY();
        PM_EULER_ZYZ zyz1 = new PM_EULER_ZYZ();
        PM_EULER_ZYZ zyz2 = new PM_EULER_ZYZ();
        PM_EULER_ZYX zyx1 = new PM_EULER_ZYX();
        PM_EULER_ZYX zyx2 = new PM_EULER_ZYX();
        PM_ROTATION_VECTOR v1;
        PM_QUATERNION q1, q2;

        v1 = new PM_ROTATION_VECTOR(Math.PI/2, 1., 2., 3.);

        /* -- not implemented
        rpy1 = new PM_RPY(v1);
        zyz1 = new PM_EULER_ZYZ(v1);
        zyx1 = new PM_EULER_ZYX(v1);
        rpy2 = new PM_RPY(v1);
        zyz2 = new PM_EULER_ZYZ(v1);
        zyx2 = new PM_EULER_ZYX(v1);

        testAssert(rpy1.equals(rpy2));
        testAssert(zyz1.equals(zyz2));
        testAssert(zyx1.equals(zyx2));
        */

        // : negation functions not implemented
        //rpy2 = - rpy1;
        //zyz2 = - zyz1;
        //zyx2 = - zyx1;

        //testAssert(rpy1 != rpy2);
        //testAssert(zyz1 != zyz2);
        //testAssert(zyx1 != zyx2);

        q1 = Posemath.toQuat(rpy1);
        q2 = Posemath.toQuat(zyz2);
        testAssert(Posemath.multiply(q1,q2).equals(new PM_QUATERNION(1, 0, 0, 0)));

        q1 = Posemath.toQuat(rpy1);
        q2 = Posemath.toQuat(zyx2);
        testAssert(Posemath.multiply(q1, q2).equals(new PM_QUATERNION(1, 0, 0, 0)));

        // -- need assignment operators for all types
        // zyz1 = zyx1;
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }

  }


  public static void main(String args[])
  {
    // testPrint();
    testCart();
    testQuat();
    testMat();
    testCyl();
    testHom();
    testOther();
  }
}
