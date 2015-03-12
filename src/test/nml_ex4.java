// Import the custom message dictionary and message classes
import nml_ex1MsgDict;
import EXAMPLE_MSG;

// Import all NML, CMS, and RCS classes and interfaces
import rcs.*;
import rcs.nml.*;
import rcs.utils.*;


class nml_ex4
{
  public static void main(String args[]) throws Exception
  {	
    /* NMLConnection( message dictionary, buffer name, process name, configuration file ) */
    NMLConnection example_nml = new NMLConnection(new nml_ex1MsgDict(), 
						  "ex_buf1",
						  "ex2_proc", 
						  "ex_cfg.nml");
		
    EXAMPLE_MSG msg = new EXAMPLE_MSG();
    msg.f = (float) 3.14;
    msg.c = (byte) '1';
    msg.i = 1024;
    example_nml.write(msg);
 
   System.out.println("Sent message: f = "+msg.f+", c = "+msg.c+", i = "+msg.i);
  } 
}
