
// Import the custom message dictionary and message classes
import nml_ex1MsgDict;
import EXAMPLE_MSG;

// Import all NML, CMS, and RCS classes and interfaces
import rcs.*;
import rcs.nml.*;
import rcs.utils.*;


class nml_ex3
{
  public static void main(String args[])  throws Exception
  {	
    /* NMLConnection( message dictionary, buffer name, process name, configuration file ) */
    NMLConnection example_nml = new NMLConnection(new nml_ex1MsgDict(), 
						  "ex_buf1",
						  "ex3_java_proc", 
						  "ex_cfg.nml");
		
    EXAMPLE_MSG msg = null;
    while(msg == null)
    {
      msg = (EXAMPLE_MSG) example_nml.read();
      Thread.sleep(100);	/* Sleep for 100 milliseconds. */
    }
    /* Print something to the console to indicate a message was recieved. */
    System.out.println("Message recieved: f = "+msg.f+", c = "+msg.c+", i = "+msg.i);
  } 
}
