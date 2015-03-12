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

// Import all NML, CMS, and RCS classes and interfaces
import rcs.*;
import rcs.nml.*;
import rcs.utils.*;
import rcs.posemath.*;

import java.io.PrintStream;
import java.io.FileOutputStream;

public class nml_ex1_j_read
{
    public static void main(String args[])
    {

	try {
	    NMLConnection nmlc1 = new NMLConnection(new nml_ex1_MsgDict(),
						    "ex1_buf","nml_ex1_j_read","ex1.nml");
	    if(null == nmlc1)
		{
		    System.err.println("nmlc1 is null.");
		    System.exit(2);
		}
	    EXAMPLE_MSG ex_msg;
	    ex_msg = (EXAMPLE_MSG) nmlc1.read();
	    System.out.println("nml_test_java_dl_read.java : From nmlc1 : tst_msg="+ex_msg);
	    System.out.println("ex_msg.d="+ex_msg.d);
	    System.exit(0);
	}
	catch(Exception e)
	    {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
		e.printStackTrace();
		System.exit(255);
	    }
    }
}
