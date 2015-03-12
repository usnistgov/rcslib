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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import rcs.posemath.PM_CARTESIAN;
import rcs.posemath.PM_LINE;
import rcs.posemath.PM_XYA;
import rcs.posemath.Posemath;


/**
 *
 * @author shackle
 */
public class LineFitTester {

    static ArrayList<PM_CARTESIAN> loadPoints(String fname)
            throws FileNotFoundException, IOException {
        ArrayList<PM_CARTESIAN> pts = new ArrayList<PM_CARTESIAN>();
        BufferedReader br = new BufferedReader(new FileReader(fname));
        String line = br.readLine();
        while (line != null) {
            try {
                String fields[] = line.split(",");
                PM_CARTESIAN c = new PM_CARTESIAN(Double.valueOf(fields[0]),
                        Double.valueOf(fields[1]), 0);
                pts.add(c);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                line = br.readLine();
            }
        }
        return pts;
    }

    static void writePoints(String fname, List<? extends PM_CARTESIAN> pts) throws FileNotFoundException {
        PrintStream ps = new PrintStream(new FileOutputStream(new File(fname)));
        for(PM_CARTESIAN c : pts) {
//            System.out.println("c = " + c);
            ps.println(c.x+","+c.y);
        }
        ps.close();
    }

    static public void main(String args[]) {
        try {
            ArrayList<PM_CARTESIAN> pts1 = loadPoints(args[0]);
            PM_CARTESIAN c_o = Posemath.centroid(pts1);
            System.out.println("c_o = " + c_o);
            List<PM_LINE> lines = Posemath.fitLines(pts1, 0.1,5);
            ArrayList<PM_CARTESIAN> out = new ArrayList<PM_CARTESIAN>();
            for(PM_LINE line : lines) {
                out.add(line.end);
                out.add(line.start);
            }
            writePoints(args[1],out);
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }
}
