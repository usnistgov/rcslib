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

public class nml_test_java_read
{
    public static void main(String args[])
    {

	try {
	    int repeat_count = 0;
	    long delay = 10;
	    boolean debug=false;
	    if(args.length < 3)
		{
		    System.err.println("nml_test_java_read usage: <buffername> <processname> <configfile> <debug> <repeat_count> <delay>");
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
	    if(args.length > 3) {
		debug = Boolean.valueOf(args[3]);
	    }
	    if(args.length > 4) {
		repeat_count = Integer.valueOf(args[4]);
	    }
	    if(args.length > 5) {
		delay = Long.valueOf(args[5]);
	    }
	    if(debug)
		{
		    NMLConnection.set_config_debug_on(true);
		    NMLFormatConverterBase.debug_on=true;
		    rcs.nml.debugInfo.debug_on=true;
		    System.out.println("rcs.nml.debugInfo.debug_on="+rcs.nml.debugInfo.debug_on);
		}
	    NMLConnection nmlc1 = new NMLConnection(new nml_test_format_MsgDict(),
						    args[0],args[1],args[2]);
	    if(null == nmlc1) {
		    System.err.println("nmlc1 is null.");
		    System.exit(2);
            }
	    System.out.println("nml_test_java_read.java : NMLConnection created.");
	    int new_count = 0;
	    double new_min = Double.POSITIVE_INFINITY;
	    double new_max = 0;
	    double new_total = 0;
	    double new_total2 = 0;

	    int old_count = 0;
	    long last_etime = 0;
	    double old_min = Double.POSITIVE_INFINITY;
	    double old_max = 0;
	    double old_total = 0;
	    double old_total2 = 0;

	    int delay_count = 0;
	    double delay_total = 0;
	    double delay_total2 = 0;
	    double delay_min = Double.POSITIVE_INFINITY;
	    double delay_max = 0;
	    
	    System.out.println("repeat_count="+repeat_count);
	    while(repeat_count != 0) {
		if(repeat_count > 0) {
		    repeat_count--;
		}
		long stime = System.currentTimeMillis();
		TEST_MESSAGE tst_msg = (TEST_MESSAGE) nmlc1.read();
		long etime = System.currentTimeMillis();
		double diff = (etime -stime) *1e-3;
		if(null != tst_msg) {
		    new_count++;
		    if(new_min > diff) {
			new_min = diff;
		    }
		    if(new_max < diff) {
			new_max = diff; 
		    }
		    new_total += diff;
		    new_total2 += diff*diff;
		} else {
		    old_count++;
		    if(old_min > diff) {
			old_min = diff;
		    }
		    if(old_max < diff) {
			old_max = diff; 
		    }
		    old_total += diff;
		    old_total2 += diff*diff;
		}
		Thread.sleep(delay);
		delay_count++;
		if(last_etime > 0) {
		    diff = (stime - last_etime)*1e-3;
		    delay_total += diff;
		    delay_total2 += diff*diff;
		    if(delay_min > diff) {
			delay_min = diff;
		    }
		    if(delay_max < diff) {
			delay_max = diff;
		    }
		}
		last_etime = etime;
	    }
	    if(new_count > 0) {
		double new_sigma = Math.sqrt(new_total2-new_total*new_total/new_count)/new_count;
		System.out.printf("Timing info new : count=%d, min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
				  new_count,new_min,new_max,new_total/new_count,
				  new_sigma);

	    }
	    if(old_count > 0) {
		double old_sigma = Math.sqrt(old_total2-old_total*old_total/old_count)/old_count;
		System.out.printf("Timing info old : count=%d,min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
				  old_count,old_min,old_max,old_total/old_count,
				  old_sigma);
	    }
	    if(delay_count > 0) {
		double delay_sigma = Math.sqrt(delay_total2-delay_total*delay_total/delay_count)/delay_count;

		System.out.printf("Timing info delay : count=%d,min=%.4f,max=%.4f,avg=%.4f,sigma=%.4f\n",
				  delay_count,delay_min,delay_max,delay_total/delay_count,
				  delay_sigma);
	    }
	    System.exit(0);
	} catch(Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    e.printStackTrace();
	    System.exit(255);
	}
    }
}
