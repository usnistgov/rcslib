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
package rcs.nml;

/*
 *	New Java File starts here.
 *	This file should be named time_tracker.java
 */

// Import all NML, CMS, and RCS classes and interfaces
/**
 * Simple class to track wall-clock performance time.
 * All times are given in seconds but precise to .001 s.
 * (Exact accuracy depends on Operating System, JRE etc )
 * Absolute times are in seconds since Jan,1 1970 UTC aka "unix time" although
 * see the pages for System.currentTimeMillis()
 * 
 * @author Will Shackleford
 */
public class time_tracker implements Cloneable {

    /**
     * Number of times cycle was called since last reset.
     */
    public int count = 0;
    /**
     * Last time cycle was called
     */
    public double last = System.currentTimeMillis() * 1e-3;

    /**
     * Current time when cycle was called.
    */
    public double now = last;
    /**
     * Time when constructed or reset.
     */
    public double start = last;
    /**
     * Current time minus start.
     */
    public double elapsed = 0;
    /**
     * Minimum difference in time between consecutive calls to cycle()
     */
    public double min = Double.MAX_VALUE;
    /**
     * Maximum difference in time between consecutive calls to cycle()
     */
    public double max = 0;
    /**
     * Average difference in time between consecutive calls to cycle().
     */
    public double avg = 0;

    /**
     * Reset all statistics and start the clock from now.
     */
    public void reset() {
        count = 0;
        last = System.currentTimeMillis() * 1e-3;
        now = last;
        start = last;
        elapsed = 0;
        min = Double.MAX_VALUE;
        max = 0;
        avg = 0;
    }

    /**
     * Called periodically to measure the wall clock statistics of a periodic function.
     */
    public void cycle() {
        count++;
        now = System.currentTimeMillis() * 1e-3;
        double diff = (now - last);
        elapsed = (now - start);
        if (min > diff) {
            min = diff;
        }
        if (max < diff) {
            max = diff;
        }
        avg = elapsed / count;
        last = now;
    }

    // Constructor
    public time_tracker() {
        reset();
    }

    public void update(NMLFormatConverter nml_fc) {

        nml_fc.beginClass("time_tracker", null);
        count = nml_fc.update_with_name("count", count);
        last = nml_fc.update_with_name("last", last);
        now = nml_fc.update_with_name("now", now);
        start = nml_fc.update_with_name("start", start);
        elapsed = nml_fc.update_with_name("elapsed", elapsed);
        min = nml_fc.update_with_name("min", min);
        max = nml_fc.update_with_name("max", max);
        avg = nml_fc.update_with_name("avg", avg);

        nml_fc.endClass("time_tracker", null);

    }

    @Override
    public time_tracker clone() throws CloneNotSupportedException {
        time_tracker cloned_object = (time_tracker) super.clone();
        return cloned_object;
    }

    @Override
    public String toString() {
        return super.toString()
                +String.format(" {min=%.3f,max=%.3f,avg=%.3f,elapsed=%.3f,count=%d}",
                min,max,avg,elapsed,count);
    }
}

