/* nml_ex2.java */

//Import the custom message dictionary
// Normally one would have something like import package.nml_ex1_MsgDict;
// but since we are not using packages we don't need the import.

// Import all NML, CMS, and RCS classes and interfaces
import rcs.nml.*;
	
public class nml_ex2
{
	public static void main(String args[]) throws Exception
	{	
		/* NML( format function, buffer name, process name, configuration file ) */
		NMLConnection example_nml = new NMLConnection(new nml_ex1_MsgDict(), "ex_buf1","ex2_proc", "ex_cfg.nml");
	} 
}
