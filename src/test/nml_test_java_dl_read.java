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

public class nml_test_java_dl_read
{
    public static void main(String args[])
    {

	try {
	    boolean debug=false;
	    if(args.length < 3)
		{
		    System.err.println("nml_test_java_dl_read usage: <buffername> <processname> <configfile> <debug>");
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
	    if(args.length > 3) {
		debug = Boolean.valueOf(args[3]);
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
	    if(null == nmlc1)
		{
		    System.err.println("nmlc1 is null.");
		    System.exit(2);
		}
	    nmlc1.set_read_debug_on(true);
	    nmlc1.set_write_debug_on(true);

	    boolean something_bad_happened=false;
	    System.out.println("nml_test_java_dl_read.java : NMLConnection created.");
	    TEST_MESSAGE tst_msg=null;
	    int tries = 0;
	    while(tst_msg == null && tries < 5)
		{
		    tst_msg = (TEST_MESSAGE) nmlc1.read();
		    System.out.println("tst_msg="+tst_msg+", tries="+tries);
		    tries++;
		    Thread.sleep(100);
		}
	
	    System.out.println("tst_msg="+tst_msg);
	    System.out.println("tst_msg.message_length="+tst_msg.message_length);
	    System.out.println("tst_msg.message="+(new String(tst_msg.message,0,tst_msg.message_length)));
	    System.out.println("nml_test_java_dl_read.java : From nmlc1 : tst_msg="+tst_msg);
	    
	    System.out.println("tst_msg.three_d_array[7]="+tst_msg.three_d_array[7]);
	    if(tst_msg.three_d_array[7] == 3.33)
		{
		    System.out.println("GOOD\n");
		}
	    else
		{
		    System.out.println("BAD should have been 3.33\n");
		    something_bad_happened=true;
		}

	    // This should have been sent and should be 01
	    System.out.println("tst_msg.cda_length="+tst_msg.cda_length);
	    if(tst_msg.cda_length == 3)
		{
		    System.out.println("GOOD\n");
		}
	    else
		{
		    System.out.println("BAD should have been 3.\n");
		    something_bad_happened=true;
		}

	    // This should have been sent and should be 01
	    System.out.println("tst_msg.cda.length="+tst_msg.cda.length);
	    if(tst_msg.cda.length < 3)
		{
		    System.out.println("BAD should have been at least 3");
		    System.exit(3);
		}
	    byte cc= (byte) '0';	    
	    System.out.println("tst_msg.cda[0]="+tst_msg.cda[0]+", \'0\'="+cc);
	    cc= (byte) '1';
	    System.out.println("tst_msg.cda[1]="+tst_msg.cda[1]+", \'1\'="+cc);
	    System.out.println("tst_msg.cda[2]="+tst_msg.cda[2]);
	    if(tst_msg.cda[0] =='0' &&
	       tst_msg.cda[1] =='1' &&
	       tst_msg.cda[2] == 0)
		{
		    System.out.println("GOOD\n");
		}
	    else
		{
		    System.out.println("BAD should have been 01.\n");
		    something_bad_happened=true;
		}
      
	    // This string should NOT have been sent.
	    // avoid a possible pointer fault since really the value of cda[7] is undefined.
	    if(tst_msg.cda.length > 3)
		{
		    cc=(byte) '2';
		    System.out.println("tst_msg.cda[3]="+tst_msg.cda[3]+", \'2\'="+cc);
		}
	    if(tst_msg.cda.length > 4)
		{
		    cc= (byte) '3';
		    System.out.println("tst_msg.cda[4]="+tst_msg.cda[4]+", \'3\'="+cc);
		}
	    if(tst_msg.cda.length > 5)
		{
		    cc= (byte) '4';
		    System.out.println("tst_msg.cda[5]="+tst_msg.cda[5]+", \'4\'="+cc);
		}
	    if(tst_msg.cda.length > 6)
		{
		    cc= (byte) '5';
		    System.out.println("tst_msg.cda[6]="+tst_msg.cda[6]+", \'5\'="+cc);
		}
	    if(tst_msg.cda.length > 7)
		{
		    System.out.println("tst_msg.cda[7]="+tst_msg.cda[7]);
		}
	    if(tst_msg.cda.length < 8 ||
	       tst_msg.cda[3] != '2' ||
	       tst_msg.cda[4] != '3' ||
	       tst_msg.cda[5] != '4' ||
	       tst_msg.cda[6] != '5' ||
	       tst_msg.cda[7] != 0)
		{
		    System.out.println("GOOD unnecessary data was NOT transmitted.\n");
		}
	    else
		{
		    System.out.println("BAD unnecessary data was transmitted.\n");
		    something_bad_happened=true;
		}

	    // this should have been sent
	    System.out.println("tst_msg.sda_length="+
			       tst_msg.sda_length);
	    if(tst_msg.sda_length==1)
		{
		    System.out.println("GOOD\n");
		} 
	    else
		{
		    System.out.println("BAD should have been 2\n");
		    something_bad_happened=true;
		}
  
	    // this should have been sent and be x
	    cc= (byte) 'x';
	    System.out.println("tst_msg.sda[0].c="+tst_msg.sda[0].c+", \'x\'="+cc);
	    if(tst_msg.sda[0].c == 'x')
		{
		    System.out.println("GOOD\n");
		}
	    else
		{
		    System.out.println("BAD should have been"+'x');
		    something_bad_happened=true;
		}
  
	    // this should NOT have been sent.
	    cc= (byte) 'y';
	    System.out.println("tst_msg.sda[2].c="+ tst_msg.sda[1].c+", \'y\'="+cc);
	    if(tst_msg.sda[1].c != 'y')
		{
		    System.out.println("GOOD unneccessary data was NOT sent.\n");
		}
	    else
		{
		    System.out.println("BAD unneccessary data was sent.\n");
		    something_bad_happened=true;
		}

	    // This should have been sent its value depends on a command line arg.
	    if(tst_msg.true_bool)
		{
		    System.out.println("GOOD tst_msg.true_bool is true\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.true_bool is false\n");
		    something_bad_happened=true;
		}

	    if(!tst_msg.false_bool)
		{
		    System.out.println("GOOD tst_msg.false_bool is false\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.false_bool is true\n");
		    something_bad_happened=true;
		}

	    if(tst_msg.sminusone == -1)
		{
		    System.out.println("GOOD tst_msg.sminusone == -1\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.sminusone = "+tst_msg.sminusone+"\n");
		    something_bad_happened=true;
		}


	    if(tst_msg.iminusone == -1)
		{
		    System.out.println("GOOD tst_msg.iminusone == -1\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.iminusone = "+tst_msg.iminusone+"\n");
		    something_bad_happened=true;
		}

	    if(tst_msg.lminusone == -1)
		{
		    System.out.println("GOOD tst_msg.lminusone == -1\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.lminusone = "+tst_msg.lminusone+"\n");
		    something_bad_happened=true;
		}

	    if(tst_msg.fminusone == -1)
		{
		    System.out.println("GOOD tst_msg.fminusone == -1\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.fminusone = "+tst_msg.fminusone+"\n");
		    something_bad_happened=true;
		}

	    if(tst_msg.dminusone == -1)
		{
		    System.out.println("GOOD tst_msg.dminusone == -1\n");
		}
	    else
		{
		    System.out.println("BAD tst_msg.dminusone = "+tst_msg.dminusone+"\n");
		    something_bad_happened=true;
		}
	
	    /* 
	     * FIXME: long long are more trouble to support than they are worth.
	    System.out.println("tst_msg.ll="+tst_msg.ll);
	    if(tst_msg.ll == -1*0x123456789L) {
		System.out.println("GOOD\n");
	    }
	    else {
		System.out.println("BAD should have been "+(-1*0x123456789L));
		something_bad_happened=true;
	    }

	    System.out.println("tst_msg.ull="+tst_msg.ull);
	    if(tst_msg.ull == 0x123456789L) {
		System.out.println("GOOD\n");
	    }
	    else {
		System.out.println("BAD should have been "+0x123456789L);
		something_bad_happened=true;
	    }


	    System.out.println("tst_msg.lla[0]="+tst_msg.lla[0]);
	    if(tst_msg.lla[0] == -1*0x123456789L) {
		System.out.println("GOOD\n");
	    }
	    else {
		System.out.println("BAD should have been "+(-1*0x123456789L));
		something_bad_happened=true;
	    }

	    System.out.println("tst_msg.ulla[0]="+tst_msg.ulla[0]);
	    if(tst_msg.ulla[0] == 0x123456789L) {
		System.out.println("GOOD\n");
	    }
	    else {
		System.out.println("BAD should have been "+0x123456789L);
		something_bad_happened=true;
	    }
	    */

	    System.out.println("tst_msg.lastvar="+tst_msg.lastvar);
	    if(something_bad_happened)
		{
		    System.exit(4);
		}
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
