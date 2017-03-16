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
 * A second version of the RCS_STAT_MSG used for base messages of RCS status
 * perhaps considered for use in moast.
 * WARNING: Maintaining this has proven somewhat difficult so it is recommended that
 * you use the original RCS_STAT_MSG or even derive your own status messages from
 * NMLmsg base class unless you are working with a project that has already adopted it.
 * 
 * @author Will Shackleford 
 */
public class RCS_STAT_MSG_V2 extends RCS_STAT_MSG
{

	public static NML_ENUM_INFO nml_enum_info_for_RCS_ADMIN_STATE=null;
	public int admin_state =2; /* enum RCS_ADMIN_STATE : ADMIN_INITIALIZED=2 */
	public time_tracker tt = new time_tracker();
	public int message_length = 0;
	public byte message[] = new byte[80]; /* NML_DYNAMIC_LENGTH_ARRAY */


	// Constructor 
    //    @SuppressWarnings("unchecked")
	public RCS_STAT_MSG_V2()
	{

		if(nml_enum_info_for_RCS_ADMIN_STATE==null)
		{
			nml_enum_info_for_RCS_ADMIN_STATE = new NML_ENUM_INFO();
			nml_enum_info_for_RCS_ADMIN_STATE.name="RCS_ADMIN_STATE";
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash = new java.util.Hashtable();
			Integer I_RCS_ADMIN_STATE=null;
			String Str_RCS_ADMIN_STATE=null;
			I_RCS_ADMIN_STATE= Integer.valueOf(2);
			Str_RCS_ADMIN_STATE= "ADMIN_INITIALIZED";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
			I_RCS_ADMIN_STATE= Integer.valueOf(1);
			Str_RCS_ADMIN_STATE= "ADMIN_UNINITIALIZED";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
			I_RCS_ADMIN_STATE= Integer.valueOf(3);
			Str_RCS_ADMIN_STATE= "ADMIN_SHUT_DOWN";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
		}

		for(int i_message = 0; i_message < 80; i_message++ )
		{
			message[i_message]  = 0;
		}

	}


	/*
	* WARNING
	* This class was derived from NMLmsg but no ID was found for it.
	* The CodeGenerator assumes that this class will be used as a base class for other NML message classes.
	* If this is not correct make sure that RCS_STAT_MSG_V2_TYPE was defined to be a unique integer greater than zero.
	*/

	// Constructor 
    //    @SuppressWarnings("unchecked")
	public RCS_STAT_MSG_V2(int nml_type)
	{
		super(nml_type);

		if(nml_enum_info_for_RCS_ADMIN_STATE==null)
		{
			nml_enum_info_for_RCS_ADMIN_STATE = new NML_ENUM_INFO();
			nml_enum_info_for_RCS_ADMIN_STATE.name="RCS_ADMIN_STATE";
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash = new java.util.Hashtable();
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash = new java.util.Hashtable();
			Integer I_RCS_ADMIN_STATE=null;
			String Str_RCS_ADMIN_STATE=null;
			I_RCS_ADMIN_STATE= Integer.valueOf(2);
			Str_RCS_ADMIN_STATE= "ADMIN_INITIALIZED";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
			I_RCS_ADMIN_STATE= Integer.valueOf(1);
			Str_RCS_ADMIN_STATE= "ADMIN_UNINITIALIZED";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
			I_RCS_ADMIN_STATE= Integer.valueOf(3);
			Str_RCS_ADMIN_STATE= "ADMIN_SHUT_DOWN";
			nml_enum_info_for_RCS_ADMIN_STATE.int_to_string_hash.put(I_RCS_ADMIN_STATE,Str_RCS_ADMIN_STATE);
			nml_enum_info_for_RCS_ADMIN_STATE.string_to_int_hash.put(Str_RCS_ADMIN_STATE,I_RCS_ADMIN_STATE);
		}

		for(int i_message = 0; i_message < 80; i_message++ )
		{
			message[i_message]  = 0;
		}

	}


	public void update(NMLFormatConverter nml_fc)
	{

		nml_fc.beginClass("RCS_STAT_MSG_V2","RCS_STAT_MSG");

		nml_fc.beginBaseClass("RCS_STAT_MSG");

		super.update(nml_fc);

		nml_fc.endBaseClass("RCS_STAT_MSG");
		admin_state = nml_fc.update_enumeration_with_name("admin_state",admin_state,nml_enum_info_for_RCS_ADMIN_STATE);
		nml_fc.beginClassVar("tt");
		tt.update(nml_fc);
		nml_fc.endClassVar("tt");
		message_length = nml_fc.update_dla_length_with_name("message_length",message_length);
		nml_fc.update_with_name("message",message,message_length);

		nml_fc.endClass("RCS_STAT_MSG_V2","RCS_STAT_MSG");

	}
}

