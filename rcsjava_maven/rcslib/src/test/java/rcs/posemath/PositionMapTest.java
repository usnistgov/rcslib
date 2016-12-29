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

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class PositionMapTest {
    
    public PositionMapTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
    }
    
    @AfterClass
    public static void tearDownClass() {
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

//    public static PM_CARTESIAN getRandomCart(Random rng,double scale) {
//        return new PM_CARTESIAN(rng.nextDouble()*scale,
//                0,//rng.nextDouble()*scale,
//                rng.nextDouble()*scale);
//    }
//    /**
//     * Test of getInList method, of class PositionMap.
//     */
//    @Test
//    public void testZeroOffset() throws PmException, IOException {
//        Random rng = new Random(101248);
//        PM_CARTESIAN randCart = new PM_CARTESIAN(); //getRandomCart(rng,10.0);
//        PM_RPY randRpy = new PM_RPY(0,(rng.nextDouble()-0.5)*2*Math.PI,0);//(rng.nextDouble()-0.5)*2*Math.PI, (rng.nextDouble()-0.5)*2*Math.PI);
//        PM_POSE testPose = new PM_POSE(randCart,randRpy);
//        PM_ROTATION_VECTOR rotV = new PM_ROTATION_VECTOR(testPose.rot);
//        System.out.println("rotV = " + rotV);
//        List<PM_CARTESIAN> inList = new ArrayList<>();
//        List<PM_CARTESIAN> outList = new ArrayList<>();
//        for (int i = 0; i < 2; i++) {
//            inList.add(getRandomCart(rng, 20.0));
//            outList.add(Posemath.multiply(testPose, inList.get(i)));
//        }
//        PositionMap pm  = new PositionMap(inList, outList);
//        PM_ROTATION_VECTOR rotV2 = new PM_ROTATION_VECTOR(pm.getTransform().rot);
//        System.out.println("rotV2 = " + rotV2);
//        PM_CARTESIAN rotCart = new PM_CARTESIAN(rotV.s*rotV.x, rotV.s*rotV.y,rotV.s*rotV.z);
//        System.out.println("rotCart = " + rotCart);
//        PM_CARTESIAN rotCart2 = new PM_CARTESIAN(rotV2.s*rotV2.x, rotV2.s*rotV2.y,rotV2.s*rotV2.z);
//        System.out.println("rotCart2 = " + rotCart2);
//        PM_CARTESIAN rotCartProj = rotCart.project(rotCart2);
//        System.out.println("rotCartProj = " + rotCartProj);
//        
//        System.out.println("pm.getTranform() = " + pm.getTransform());
//        File f = File.createTempFile("posemap", ".csv");
//        System.out.println("Saved to temp file:\n"+f.getCanonicalPath());
//        pm.saveToCsvFile(f);
//        Desktop.getDesktop().open(f);
//    }

    
    
}
