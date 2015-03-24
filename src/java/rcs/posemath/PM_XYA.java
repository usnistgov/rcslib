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
package rcs.posemath;

import rcs.nml.NMLFormatConverter;

/**
 *
 * @author shackle
 */
public class PM_XYA implements Cloneable {

    public double x, y, a;

    public PM_XYA(double _x, double _y, double _a) {
        this.x = _x;
        this.y = _y;
        this.a = _a;
    }

    public PM_XYA() {
        this.x = 0;
        this.y = 0;
        this.a = 0;
    }

    public PM_XYA(PmCartesian c) {
        this.x = c.x;
        this.y = c.y;
        this.a = 0;
    }

    public PM_XYA(PmPose p) throws PmException {
        this.x = p.tran.x;
        this.y = p.tran.y;
        PmRpy rpy = new PmRpy();
        Posemath.pmQuatRpyConvert(p.rot, rpy);
        this.a = rpy.y;
    }

    public PM_XYA(PM_POSE p) throws PmException {
        this.x = p.tran.x;
        this.y = p.tran.y;
        PmRpy rpy = new PmRpy();
        Posemath.pmQuatRpyConvert(p.rot, rpy);
        this.a = rpy.y;
    }

    public PM_XYA inv() {
        PM_XYA ret = new PM_XYA();
        ret.x = -this.x * Math.cos(this.a) - this.y * Math.sin(this.a);
        ret.y = this.x * Math.sin(this.a) - this.y * Math.cos(this.a);
        ret.a = -this.a;
        return ret;
    }

    @Override
    public PM_XYA clone() {
        PM_XYA ret = null;
        try { 
            ret = (PM_XYA) super.clone();
        }
        catch(Exception e) {
            ret = new PM_XYA();
        }
        ret.x = this.x;
        ret.y = this.y;
        ret.a = this.a;
        return ret;
    }

    public void update(NMLFormatConverter nml_fc) {
        nml_fc.beginClass("PM_XYA", null);
        x = nml_fc.update_with_name("x", x);
        y = nml_fc.update_with_name("y", y);
        a = nml_fc.update_with_name("a", a);
        nml_fc.endClass("PM_XYA", null);
    }

    @Override
    public String toString() {
        return String.format(" { x=%+.3g, y=%+.3g, a=%+.3g (%.3g deg) } ", x, y, a,a*180/Math.PI);
    }

    static public PM_XYA valueOf(String s) {
        String vals[] = s.split("[ ,{}]+");
        PM_XYA xya = new PM_XYA();
        for(int i = 0; i < vals.length; i++) {
            if(vals[i].length() < 1) {
                if(vals.length > i + 1) {
                    System.arraycopy(vals, i+1, vals, i, vals.length - i-1);
                    i--;
                }
                continue;
            }
            try {
                if (vals[i].indexOf('=') > 0) {
                    if (vals[i].startsWith("x=")) {
                        xya.x = Double.valueOf(vals[i].substring(2));
                    }
                    if (vals[i].startsWith("y=")) {
                        xya.y = Double.valueOf(vals[i].substring(2));
                    }
                    if (vals[i].startsWith("a=")) {
                        xya.a = Double.valueOf(vals[i].substring(2));
                        if(i < vals.length - 1
                                && vals[i+1].startsWith("deg")) {
                            xya.a = Math.toRadians(xya.a);
                            i++;
                        }
                    }
                } else {
                    if (i == 0) {
                        xya.x = Double.valueOf(vals[i]);
                    }
                    if (i == 1) {
                        xya.y = Double.valueOf(vals[i]);
                    }
                    if (i == 2) {
                        xya.a = Double.valueOf(vals[i]);
                    }
                }
            } catch (Exception exception) {
                exception.printStackTrace();
            }
        }
        return xya;
    }

    /**
     * @return the x
     */
    public double getX() {
        return x;
    }

    /**
     * @param x the x to set
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * @return the y
     */
    public double getY() {
        return y;
    }

    /**
     * @param y the y to set
     */
    public void setY(double y) {
        this.y = y;
    }


    /**
     * @return the a
     */
    public double getA() {
        return a;
    }

    /**
     * @param a new value of angle
     */
    public void setA(double a) {
        this.a = a;
    }
}
