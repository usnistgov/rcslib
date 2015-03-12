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

public class nml_test_java_get_msg_type
{
    public static void main(String args[])
    {

	try {
	    int repeat_count = 0;
	    long delay = 10;
	    boolean debug=false;
	    if(args.length < 3)
		{
		    System.err.println("nml_test_java_read usage: <buffername> <processname> <configfile> <expected_msg_type>");
		    System.exit(1);
		}
	    try
		{
		    rcs.nml.debugInfo.debugPrintStream=new PrintStream(new FileOutputStream("nml_test_java_read."+args[0]+".log"));
		}
	    catch(Exception e)
		{
		    e.printStackTrace();
		}
	    long expected_msg_type = 0;
	    if(args.length > 3) {
		expected_msg_type = Integer.valueOf(args[3]);
	    }

	    NMLConnection nmlc1 = new NMLConnection(null,
						    args[0],args[1],args[2]);
	    if(null == nmlc1)
		{
		    System.err.println("nmlc1 is null.");
		    System.exit(2);
		}
	    long t = nmlc1.get_msg_type();
	    if(expected_msg_type != 0 && t != expected_msg_type) {
		System.err.printf("NMLConnection.get_msg_type() returned %d when %d was expected.\n", 
				  t,expected_msg_type);
		System.exit(1);
	    }
	    System.out.println(Long.toString(t));
	    System.exit(0);
	} catch(Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    e.printStackTrace();
	    System.exit(255);
	}
    }
}
