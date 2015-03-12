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

package rcs.nml;

/**
 * Base class for NML command messages.
 * <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
*
* </pre>
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*
*/
public class RCS_CMD_MSG extends NMLmsg
{
    /**
     * Applications ussually increment the serial number each
     * time a command is sent, this number is ussually echoed
     * back in the status message so that supervisors can tell
     * which command is currently executing or just completed.
     */
    public int serial_number=0;

    protected RCS_CMD_MSG()
    {
    }

    public RCS_CMD_MSG(int _type)
    {
	super(_type);
	//rcs.nml.debugInfo.debugPrintStream .println("RCS_CMD_MSG("+_type+") constructed.");
    }


    public void update(NMLFormatConverter nml_fc)
    {
	if(!nml_fc.cmd_msg_updated || nml_fc.always_update_cmd_msg)
	    {
		//super.update(nml_fc);
		nml_fc.beginClass("RCS_CMD_MSG",null);
		serial_number = nml_fc.update_with_name("serial_number",serial_number);
		nml_fc.endClass("RCS_CMD_MSG",null);

		nml_fc.cmd_msg_updated = true;
	    }
    }

}
