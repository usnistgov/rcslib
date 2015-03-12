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

public class nml_test_java_dl_write
{
    public static void main(String args[])
    {

	try {
	    if(args.length < 4)
		{
		    System.err.println("nml_test_java_dl_write usage: <buffername> <processname> <configfile> <lastvar>");
		    System.exit(1);
		}
	    try
		{
		    rcs.nml.debugInfo.debugPrintStream=new PrintStream(new FileOutputStream("nml_test_java_dl_write."+args[0]+".log"));
		}
	    catch(Exception e)
		{
		    e.printStackTrace();
		}
	    // if(false)
	    // 	{
	    // 	    NMLConnection.set_write_debug_on(true);
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
	    TEST_MESSAGE tst_msg = new TEST_MESSAGE();

	    // These will be sent.
	     tst_msg.enumtestvar = nml_test_format_MsgDict.aa;
	     tst_msg.enum_array[0] = nml_test_format_MsgDict.aa;
	     tst_msg.enum_array[4] = nml_test_format_MsgDict.bb;
	     tst_msg.enumtest_dla_length = 3;
	     tst_msg.enumtest_dla[0] = nml_test_format_MsgDict.aa;
	     tst_msg.enumtest_dla[1] = nml_test_format_MsgDict.bb;
	     tst_msg.enumtest_dla[2] = nml_test_format_MsgDict.aa;

	     // these will not be sent.
	     tst_msg.enumtest_dla[3] = nml_test_format_MsgDict.bb;
	     tst_msg.enumtest_dla[4] = nml_test_format_MsgDict.aa;
	     tst_msg.enumtest_dla[5] = nml_test_format_MsgDict.bb;
	     tst_msg.enumtest_dla[6] = nml_test_format_MsgDict.aa;
  
	    //this will be sent.
	    tst_msg.three_d_array[7] = 3.33;
	    tst_msg.cda_length = 3;
	    tst_msg.cda[0]= (byte) '0';
	    tst_msg.cda[1]= (byte) '1';
	    tst_msg.cda[2]=0;
	    tst_msg.cda[3]= (byte) '2';
	    tst_msg.cda[4]= (byte) '3';
	    tst_msg.cda[5]= (byte) '4';
	    tst_msg.cda[6]= (byte) '5';
	    tst_msg.cda[7]=0;

	    /* 
	     * FIXME: long long are more trouble to support than they are worth.
	     tst_msg.ll = -1*0x123456789L;
	     tst_msg.ull = 0x123456789L;
	     tst_msg.lla[0] = -1*0x123456789L;
	     tst_msg.ulla[0] = 0x123456789L;
	    */

	    // this will be sent.
	    tst_msg.sda_length=1;
	    tst_msg.sda[0].c= (byte) 'x';
  
	    // this will NOT be sent;
	    tst_msg.sda[1].c = (byte) 'y';
	    
	    // This will be sent.
	    tst_msg.do_int_size_test = true;
	    tst_msg.s_array[0] = tst_msg.smax = Short.MAX_VALUE;
	    tst_msg.s_array[1] = tst_msg.smin = Short.MIN_VALUE;
	    tst_msg.s_array[2] = 0;
	    tst_msg.i_smax = (int)  Short.MAX_VALUE;
	    tst_msg.i_smin = (int)  Short.MIN_VALUE;
	    tst_msg.i_array[0] = tst_msg.imax = Integer.MAX_VALUE;
	    tst_msg.i_array[1] = tst_msg.imin = Integer.MIN_VALUE;
	    tst_msg.i_array[2] = 0;
	    tst_msg.l_imax = (long) Integer.MAX_VALUE;
	    tst_msg.l_imin = (long) Integer.MIN_VALUE;
	    //tst_msg.l_array[0] = tst_msg.lmax = Long.MAX_VALUE;
	    //tst_msg.l_array[1] = tst_msg.lmin = Long.MIN_VALUE;
	    //tst_msg.l_array[2] = 0;
	    //tst_msg.d_lmax = (double) Long.MAX_VALUE;
	    //tst_msg.d_lmin = (double) Long.MIN_VALUE;

	    tst_msg.true_bool=true;
	    tst_msg.false_bool=false;
	    
	    tst_msg.sminusone=-1;
	    tst_msg.iminusone=-1;
	    tst_msg.lminusone=-1;
	    tst_msg.fminusone= (float)-1.0;
	    tst_msg.dminusone=-1.0;
	    
	    tst_msg.lastvar= ( new Integer(args[3])).intValue();
	    int t=0;

	    if((t = nmlc1.write(tst_msg)) != 0)
		{
		    System.out.println("nmlc1.write() returned "+t);
		    System.exit(255);
		}
	    System.exit(0);
	}
	catch(Exception e)
	    {
		e.printStackTrace();
		System.exit(255);
	    }
    }
}
