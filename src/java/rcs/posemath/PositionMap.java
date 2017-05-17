/*
 * This software is public domain software, however it is preferred
 * that the following disclaimers be attached.
 * Software Copywrite/Warranty Disclaimer
 * 
 * This software was developed at the National Institute of Standards and
 * Technology by employees of the Federal Government in the course of their
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the
 * public domain.
 * 
 * This software is experimental. NIST assumes no responsibility whatsoever 
 * for its use by other parties, and makes no guarantees, expressed or 
 * implied, about its quality, reliability, or any other characteristic. 
 * We would appreciate acknowledgement if the software is used. 
 * This software can be redistributed and/or modified freely provided 
 * that any derivative works bear some notice that they are derived from it, 
 * and any modified versions bear some notice that they have been modified.
 * 
 *  See http://www.copyright.gov/title17/92chap1.html#105
 * 
 */
package rcs.posemath;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * This was an experiment that failed. preserved as commented out code only.
 * Ignore this class/file. It never worked but I may or not return to work on it again.
 * 
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class PositionMap {

//    final private List<PM_CARTESIAN> inList = new ArrayList<>();
//    final private List<PM_ROTATION_VECTOR> rotList = new ArrayList<>();
//    final private List<PM_ROTATION_VECTOR> rotCumulativeList = new ArrayList<>();
//    final private List<PM_CARTESIAN> rotatedInList = new ArrayList<>();
//    final private List<PM_CARTESIAN> transformedInList = new ArrayList<>();
//    final private List<PM_CARTESIAN> outList = new ArrayList<>();
//    final private List<PM_CARTESIAN> offsetList = new ArrayList<>();
//    private PM_POSE transform;
//
//    public PositionMap(List<PM_CARTESIAN> inList, List<PM_CARTESIAN> outList) throws PmException {
//        if (inList == null) {
//            throw new IllegalArgumentException("inlist == null");
//        }
//        if (outList == null) {
//            throw new IllegalArgumentException("outList == null");
//        }
//        if (inList.size() != outList.size()) {
//            throw new IllegalArgumentException("(inList.size()=" + inList.size() + ") !=  (ooutList.size()=" + outList.size() + ")");
//        }
//        this.inList.addAll(inList);
//        this.outList.addAll(outList);
//        PM_CARTESIAN rotCartSum = new PM_CARTESIAN();
//        rotList.add(new PM_ROTATION_VECTOR());
//        rotCumulativeList.add(new PM_ROTATION_VECTOR());
//        PM_ROTATION_VECTOR rotVec = new PM_ROTATION_VECTOR();
//        for (int i = 0; i < inList.size() - 1; i++) {
//            PM_ROTATION_VECTOR v = getCurrentRotV(i, rotVec);
//            
//        }
//        double rotSumMag = rotCartSum.mag();
//        if (rotSumMag < Posemath.DOUBLE_FUZZ) {
//            rotatedInList.addAll(inList);
//            transform = new PM_POSE();
//        } else {
//            PM_CARTESIAN rotSumUnit = rotCartSum.unit();
//            PM_ROTATION_VECTOR rv = new PM_ROTATION_VECTOR(rotSumMag, rotSumUnit.x, rotSumUnit.y, rotSumUnit.z);
//            transform = new PM_POSE(new PM_CARTESIAN(), Posemath.toQuat(rv));
//            for (int i = 0; i < inList.size(); i++) {
//                rotatedInList.add(Posemath.multiply(transform, inList.get(i)));
//            }
//        }
//        if (inList.size() > 0) {
//            PM_CARTESIAN inSum = new PM_CARTESIAN();
//            PM_CARTESIAN outSum = new PM_CARTESIAN();
//            for (int i = 0; i < inList.size(); i++) {
//                inSum = inSum.add(rotatedInList.get(i));
//                outSum = outSum.add(outList.get(i));
//            }
//            PM_CARTESIAN offset = outSum.subtract(inSum).multiply(1.0 / inList.size());
//            transform.tran = offset;
//        }
//        for (int i = 0; i < inList.size(); i++) {
//            transformedInList.add(Posemath.multiply(transform, inList.get(i)));
//            offsetList.add(outList.get(i).subtract(transformedInList.get(i)));
//        }
//    }
//
//    private PM_ROTATION_VECTOR getCurrentRotV(int i, PM_ROTATION_VECTOR rotVec) throws PmException {
//        PM_CARTESIAN ini = inList.get(i).rotate(rotVec);
//        PM_CARTESIAN inip1 = inList.get(i + 1).rotate(rotVec);
//        PM_CARTESIAN outi = outList.get(i);
//        PM_CARTESIAN outp1 = outList.get(i + 1);
//        PM_CARTESIAN inDiff = inip1.subtract(ini);
//        PM_CARTESIAN inDiffUnit = inDiff.unit();
//        PM_CARTESIAN outDiff = outp1.subtract(outi);
//        PM_CARTESIAN outDiffUnit = outDiff.unit();
//        PM_CARTESIAN unitCross = inDiffUnit.cross(outDiffUnit);
//        double angle = Math.atan2(unitCross.mag(), inDiffUnit.dot(outDiffUnit));
//        PM_CARTESIAN unitCrossUnit = unitCross.unit();
//        PM_ROTATION_VECTOR curRotVec = new PM_ROTATION_VECTOR(angle, unitCrossUnit.x, unitCrossUnit.y, unitCrossUnit.z);
//        return curRotVec;
////            PM_CARTESIAN rotCart = unitCrossUnit.multiply(angle);
////            if (rotCartSum.mag() > Posemath.CART_FUZZ) {
////                PM_CARTESIAN rotRemainder = rotCart.subtract(rotCartSum.project(rotCart));
////                rotCartSum = rotCartSum.add(rotCart);
////            } else {
////                rotCartSum = rotCart;
////            }
////            PM_CARTESIAN rotCartSumCum = rotCartSum.multiply(1.0 / rotList.size());
////            rotVec = new PM_ROTATION_VECTOR(angle, unitCrossUnit.x, unitCrossUnit.y, unitCrossUnit.z);
////            rotList.add(rotVec);
//////            PM_CARTESIAN rotCartSumUnit = rotCartSumCum.unit();
//////            PM_ROTATION_VECTOR rotCumulativeV = new PM_ROTATION_VECTOR(rotCartSumCum.mag(), rotCartSumUnit.x, rotCartSumUnit.y, rotCartSumUnit.z);
////            rotCumulativeList.add(rotVec);
//    }
//
//    public List<PM_CARTESIAN> getInList() {
//        return Collections.unmodifiableList(inList);
//    }
//
//    public List<PM_CARTESIAN> getRotatedInList() {
//        return Collections.unmodifiableList(rotatedInList);
//    }
//
//    public List<PM_CARTESIAN> getTransformedInList() {
//        return Collections.unmodifiableList(transformedInList);
//    }
//
//    public List<PM_CARTESIAN> getOutList() {
//        return Collections.unmodifiableList(outList);
//    }
//
//    public List<PM_CARTESIAN> getOffsetList() {
//        return Collections.unmodifiableList(offsetList);
//    }
//
//    public PM_POSE getTransform() {
//        return transform;
//    }
//
//    @Override
//    public String toString() {
//        return "PositionMap{" + "inList=" + inList + ", rotatedInList=" + rotatedInList + ", transormedInList=" + transformedInList + ", outList=" + outList + ", offsetList=" + offsetList + ", transform=" + transform + '}';
//    }
//
//    public static class PositionMapEntry {
//
//        private final PM_CARTESIAN in;
//        private final PM_CARTESIAN rotatedIn;
//        private final PM_CARTESIAN out;
//        private final PM_CARTESIAN transformedIn;
//        private final PM_CARTESIAN offset;
//        private final PM_ROTATION_VECTOR rot;
//        private final PM_ROTATION_VECTOR rotCumulative;
//
//        private PositionMapEntry(PM_CARTESIAN in, PM_CARTESIAN out, PM_ROTATION_VECTOR rot, PM_ROTATION_VECTOR rotCumulative, PM_CARTESIAN rotatedIn, PM_CARTESIAN transformedIn, PM_CARTESIAN offset) {
//            this.in = in;
//            this.rot = rot;
//            this.rotatedIn = rotatedIn;
//            this.out = out;
//            this.transformedIn = transformedIn;
//            this.offset = offset;
//            this.rotCumulative = rotCumulative;
//        }
//
//        @Override
//        public String toString() {
//            return "PositionMapEntry{" + "in=" + in + ", rotatedIn=" + rotatedIn + ", out=" + out + ", transformedIn=" + transformedIn + ", offset=" + offset + ", rot=" + rot + '}';
//        }
//
//        public String toCsvString() {
//            return in.x + "," + in.y + "," + in.z + ","
//                    + out.x + "," + out.y + "," + out.z + ","
//                    + rot.getRxDeg() + "," + rot.getRyDeg() + "," + rot.getRzDeg() + ","
//                    + rotCumulative.getRxDeg() + "," + rotCumulative.getRyDeg() + "," + rotCumulative.getRzDeg() + ","
//                    + rotatedIn.x + "," + rotatedIn.y + "," + rotatedIn.z + ","
//                    + transformedIn.x + "," + transformedIn.y + "," + transformedIn.z + ","
//                    + offset.x + "," + offset.y + "," + offset.z;
//        }
//    }
//
//    public static final String CSV_HEADER_STRING
//            = "in.x,in.y,in.z,"
//            + "out.x,out.y,out.z,"
//            + "rx,ry,rz,"
//            + "rcx,rcy,rcz,"
//            + "rotatedIn.x,rotatedIn.y,rotatedIn.z,"
//            + "transformedIn.x,transformedIn.y,transformedIn.z,"
//            + "offset.x,offset.y,offset.z";
//
//    public PositionMapEntry getEntry(int i) {
//        return new PositionMapEntry(inList.get(i), outList.get(i), rotList.get(i), rotCumulativeList.get(i), rotatedInList.get(i), transformedInList.get(i), offsetList.get(i));
//    }
//
//    public String getEntryCsvString(int i) {
//        return getEntry(i).toCsvString();
//    }
//
//    public void printCsvTable(PrintStream ps) {
//        ps.println(CSV_HEADER_STRING);
//        for (int i = 0; i < inList.size(); i++) {
//            ps.println(getEntryCsvString(i));
//        }
//    }
//
//    public void saveToCsvFile(File f) throws FileNotFoundException {
//        try (PrintStream ps = new PrintStream(f)) {
//            printCsvTable(ps);
//        }
//    }

}
