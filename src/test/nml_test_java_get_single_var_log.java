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

public class nml_test_java_get_single_var_log
{
    public static void main(String args[])
    {

	try {
	    if(args.length < 3)
		{
		    System.err.println("nml_test_java_dl_read usage: <buffername> <processname> <configfile>");
		    System.exit(1);
		}
	    try
		{
		    rcs.nml.debugInfo.debugPrintStream=new PrintStream(new FileOutputStream("nml_test_java_dl_read."+args[0]+".log"));
		}
	    catch(Exception e)
		{
		    e.printStackTrace();
		}
	    // if(false)
	    // 	{
	    // 	    NMLConnection.set_read_debug_on(true);
	    // 	    NMLFormatConverterBase.debug_on=true;
	    // 	    rcs.nml.debugInfo.debug_on=true;
	    // 	}
	    NMLConnection nmlc1 = new NMLConnection(new nml_test_format_MsgDict(),
						    args[0],args[1],args[2]);
	    if(null == nmlc1)
		{
		    System.err.println("nmlc1 is null.");
		    System.exit(2);
		}
	    
	    boolean something_bad_happened=false;
	    System.out.println("nml_test_java_dl_read.java : NMLConnection created.");
	    // nmlc1.setupSingleVarLog( String varname, int maxlogsize, double period, int type)
	    System.out.println("Calling nmlc1.setupSingleVarLog(\"d\",100,0.01,nml_test_format_MsgDict.TEST_MESSAGE_TYPE); . . .");
	    int var_list_id = nmlc1.setupSingleVarLog("d",100,0.01,
				    nml_test_format_MsgDict.TEST_MESSAGE_TYPE);
	    System.out.println("var_list_id="+var_list_id);
	    try
		{
		    Thread.sleep(500);
		}
	    catch(Exception e)
		{
		}
	    for(int i = 0; i < 20; i++)
		{
		    try
			{
			    Thread.sleep(500);
			}
		    catch(Exception e)
			{
			}
		    System.out.println("calling nmlc1.getSingleVarLog("+var_list_id+")");
		    NMLSingleVarLog slv  = nmlc1.getSingleVarLog(var_list_id);
		    System.out.println("slv = "+slv);
		}
	    nmlc1.closeSingleVarLog(var_list_id);
	    System.out.println("Exiting.");
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
