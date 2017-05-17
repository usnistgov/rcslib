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

import rcs.nml.NMLmsg;
import rcs.nml.NML_ENUM_INFO;

/**
* Base class for NML status messages.
* <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
*
*
* </pre>
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*
*/
public class RCS_STAT_MSG extends NMLmsg
{
    public static NML_ENUM_INFO nml_enum_info_for_RCS_STATUS=null;

    /**
     * Subordinates ussually set this to the type of the command
     * they are currently executing or just completed.
     * The templates do this automatically.
     */
    public long command_type;

    /**
     * Subordinates ussually set this to the serial_number of the command
     * they are currently executing or just completed.
     * The templates do this automatically.
     */
    public int echo_serial_number;

    /**
     * Value for status if the module has not yet been initialized.
     */
    static public final int UNINITIALIZED_STATUS = -1;

    /**
     * Value for status if the module has completed the last command
     * it was given.
     */
    static public final int RCS_DONE = 1;

    /**
     * Value for status if the module is still executing the last command
     * it was given.
     */
    static public final int RCS_EXEC = 2;

    /**
     * Value for status if the module has encountered an error.
     */
    static public final int RCS_ERROR = 3;


    /**
     * The status of the module is ussually set to either
     * UNINITIALIZED_STATUS, RCS_DONE, RCS_EXEC, or RCS_ERROR, depending on
     * whether the module is currently executing a command
     * or has encountered an error.
     * The templates do this automatically.
     */
    public int status;

    /**
     * Modules that use simple state tables ussually provide
     * the current state here.
     * The templates do this automatically.
     */
    public int state;

    /**
     * Modules that use simple state tables ussually provide
     * the last line matched in the state table here.
     * The line here is meant as the number of if(STATE_MATCH(...)
     * encountered before one was true.
     * The templates do this automatically.
     */
    public int line;

    /**
     * Modules that use simple state tables ussually provide
     * the last line matched in the state table here.
     * The line here is meant as the line number in the source
     * code of the if(STATE_MATCH( . . .) that was true.
     * The templates do this automatically.
     * The diagnostics tool uses this  for the "state table"
     * display.
     */
    public int source_line;

    /**
     * Modules that use simple state tables may provide the
     * name of the source file of the state table that is
     * currently being used here.
     * The templates do this automatically.
     * The diagnostics tool uses this  for the "state table"
     * display.
     */
    public byte source_file[] = new byte[64];


    protected RCS_STAT_MSG()
    {
    }

    /**
     * Derived classes should use this constructor by placing
     *       super(_type) in the first line of thier constructors.
     *
     * @param _type the integer identifier of the derived class
     */
    //@SuppressWarnings("unchecked")
    public RCS_STAT_MSG(int _type)
    {
	super(_type);
	//rcs.nml.debugInfo.debugPrintStream .println("RCS_STAT_MSG("+_type+") constructed.");
	command_type = -1;
	state = -1;
	line = -1;
	status = UNINITIALIZED_STATUS;
	echo_serial_number = -1;
	for(int source_file_i=0; 
	    source_file_i < 64 && source_file_i < source_file.length; 
	    source_file_i++)
	    {
		source_file[source_file_i] = (byte) 0;
	    }
	if(nml_enum_info_for_RCS_STATUS==null)
	    {
		nml_enum_info_for_RCS_STATUS = new NML_ENUM_INFO();
		nml_enum_info_for_RCS_STATUS.name="RCS_STATUS";
		nml_enum_info_for_RCS_STATUS.string_to_int_hash = new java.util.Hashtable();
		nml_enum_info_for_RCS_STATUS.int_to_string_hash = new java.util.Hashtable();
		Integer I_RCS_STATUS=null;
		String Str_RCS_STATUS=null;
		I_RCS_STATUS= Integer.valueOf(2);
		Str_RCS_STATUS= "RCS_EXEC";
		nml_enum_info_for_RCS_STATUS.int_to_string_hash.put(I_RCS_STATUS,Str_RCS_STATUS);
		nml_enum_info_for_RCS_STATUS.string_to_int_hash.put(Str_RCS_STATUS,I_RCS_STATUS);
		I_RCS_STATUS= Integer.valueOf(1);
		Str_RCS_STATUS= "RCS_DONE";
		nml_enum_info_for_RCS_STATUS.int_to_string_hash.put(I_RCS_STATUS,Str_RCS_STATUS);
		nml_enum_info_for_RCS_STATUS.string_to_int_hash.put(Str_RCS_STATUS,I_RCS_STATUS);
		I_RCS_STATUS= Integer.valueOf(3);
		Str_RCS_STATUS= "RCS_ERROR";
		nml_enum_info_for_RCS_STATUS.int_to_string_hash.put(I_RCS_STATUS,Str_RCS_STATUS);
		nml_enum_info_for_RCS_STATUS.string_to_int_hash.put(Str_RCS_STATUS,I_RCS_STATUS);
		I_RCS_STATUS= Integer.valueOf(-1);
		Str_RCS_STATUS= "UNINITIALIZED_STATUS";
		nml_enum_info_for_RCS_STATUS.int_to_string_hash.put(I_RCS_STATUS,Str_RCS_STATUS);
		nml_enum_info_for_RCS_STATUS.string_to_int_hash.put(Str_RCS_STATUS,I_RCS_STATUS);
	    }
    }

    /**
     * update function for the RCS_STAT_MSG members. Derived
     * classes that have data members that should be communicated
     * should override this function and place super.update(nml_fc) in
     * the first line.
     *
     * @param nml_fc the NMLFormatConverter used to convert basic
     *               data types to neutral formats and back.
     */
    public void update(NMLFormatConverter nml_fc)
    {
	if(!nml_fc.stat_msg_updated || nml_fc.always_update_stat_msg)
	    {
		//super.update(nml_fc);
		nml_fc.beginClass("RCS_STAT_MSG",null);

		command_type  = nml_fc.update_with_name("command_type",command_type );
		echo_serial_number  = nml_fc.update_with_name("echo_serial_number",echo_serial_number );
		status = nml_fc.update_enumeration_with_name("status",status,nml_enum_info_for_RCS_STATUS);
		state  = nml_fc.update_with_name("state",state );
		line  = nml_fc.update_with_name("line",line );
		source_line  =nml_fc.update_with_name("source_line",source_line );
		nml_fc.update_with_name("source_file",source_file,64);
		nml_fc.stat_msg_updated = true;
		nml_fc.endClass("RCS_STAT_MSG",null);
	    }
    }
}
