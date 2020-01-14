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
package diagapplet.CodeGen;

import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import java.util.*;
import rcs.nml.NMLFormatConverter;
import rcs.nml.NMLmsg;
import rcs.nml.NML_ENUM_INFO;

/*
 *
 * DiagNMLMsgDict
 *
 */
class ByteArrayObject {

    byte array[] = null;
};

class DiagNMLmsg extends NMLmsg {

    public ModuleInfo module_info = null;
    public boolean cmd_stream = false;
    public boolean stat_stream = false;
    static public volatile int failed_count = 0;
    StructureTypeInfo typeInfo = null;
    public static int MAX_MESSAGE_VARS = 4096;
    Vector byte_arrays = new Vector();
    byte bytes[] = new byte[MAX_MESSAGE_VARS];
    short shorts[] = new short[MAX_MESSAGE_VARS];
    int ints[] = new int[MAX_MESSAGE_VARS];
    long longs[] = new long[MAX_MESSAGE_VARS];
    float floats[] = new float[MAX_MESSAGE_VARS];
    double doubles[] = new double[MAX_MESSAGE_VARS];
    boolean booleans[] = new boolean[MAX_MESSAGE_VARS];
    int byte_arrays_in_message = 0;
    int bytes_in_message = 0;
    int shorts_in_message = 0;
    int ints_in_message = 0;
    int longs_in_message = 0;
    int floats_in_message = 0;
    int doubles_in_message = 0;
    int booleans_in_message = 0;

    public void tokensNotUsed(int num_tokens, String input_string, boolean warn_given) {
	if (!warn_given) {
	    Thread.dumpStack();
	    diagapplet.utils.DiagError.println("DiagNMLmsg.tokensNotUsed() called.");
	    diagapplet.utils.DiagError.println("num_tokens=" + num_tokens);
	    diagapplet.utils.DiagError.println("input_string=" + input_string);
	    diagapplet.utils.DiagError.println("typeInfo=" + typeInfo);
	}
	//	System.exit(1);
	}

    public void bytesNotUsed(int bytes_in_input_stream, String bufName, boolean warn_given) {
	if (!warn_given) {
	    Thread.dumpStack();
	    diagapplet.utils.DiagError.println("DiagNMLmsg.bytesNotUsed() called.");
	    diagapplet.utils.DiagError.println("typeInfo=" + typeInfo);
	}
	//System.exit(1);
	}
    boolean first_misc_error_occured = false;

    public void miscError() {
	if (!first_misc_error_occured) {
	    Thread.dumpStack();
	    diagapplet.utils.DiagError.println("DiagNMLmsg.miscError() called.");
	    diagapplet.utils.DiagError.println("typeInfo=" + typeInfo);
	}
	first_misc_error_occured = true;

	//System.exit(1);
	}

    public void miscError(Exception e) {
	if (!first_misc_error_occured) {
	    Thread.dumpStack();
	    diagapplet.utils.DiagError.println("DiagNMLmsg.miscError() called.");
	    diagapplet.utils.DiagError.println("typeInfo=" + typeInfo);
	    if (null != e) {
		e.printStackTrace();
	    }
	}
	first_misc_error_occured = true;
	//System.exit(1);
	}
    boolean mem_warn_given = false;

    boolean check_bytes_in_msg() {
	if (bytes_in_message >= bytes.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (4 * bytes.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (4 * bytes.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (4 * bytes_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (4 * bytes_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    byte new_bytes[] = new byte[2 * bytes.length];
	    for (int i = 0; i < bytes.length; i++) {
		new_bytes[i] = bytes[i];
	    }
	    bytes = new_bytes;
	}
	return false;
    }

    boolean check_shorts_in_msg() {
	if (shorts_in_message >= shorts.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (8 * shorts.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (8 * shorts.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (8 * shorts_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (8 * shorts_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    short new_shorts[] = new short[2 * shorts.length];
	    for (int i = 0; i < shorts.length; i++) {
		new_shorts[i] = shorts[i];
	    }
	    shorts = new_shorts;
	}
	return false;
    }

    boolean check_ints_in_msg() {
	if (ints_in_message >= ints.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (16 * ints.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * ints.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (16 * ints_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * ints_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    int new_ints[] = new int[2 * ints.length];
	    for (int i = 0; i < ints.length; i++) {
		new_ints[i] = ints[i];
	    }
	    ints = new_ints;
	}
	return false;
    }

    
        boolean check_longs_in_msg() {
	if (longs_in_message >= longs.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (16 * longs.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * longs.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (16 * longs_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * longs_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    long new_longs[] = new long[2 * longs.length];
	    for (int i = 0; i < longs.length; i++) {
		new_longs[i] = longs[i];
	    }
	    longs = new_longs;
	}
	return false;
    }

    boolean check_floats_in_msg() {
	if (floats_in_message >= floats.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (16 * floats.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * floats.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (16 * floats_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (16 * floats_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    float new_floats[] = new float[2 * floats.length];
	    for (int i = 0; i < floats.length; i++) {
		new_floats[i] = floats[i];
	    }
	    floats = new_floats;
	}
	return false;
    }



    boolean check_doubles_in_msg() {
	if (doubles_in_message >= doubles.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (32 * doubles.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (32 * doubles.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (32 * doubles_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (32 * doubles_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    double new_doubles[] = new double[2 * doubles.length];
	    for (int i = 0; i < doubles.length; i++) {
		new_doubles[i] = doubles[i];
	    }
	    doubles = new_doubles;
	}
	return false;
    }

    boolean check_booleans_in_msg() {
	if (booleans_in_message >= booleans.length) {
	    long fm = Runtime.getRuntime().freeMemory();
	    if (8 * booleans.length > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (8 * booleans.length) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    if (8 * booleans_in_message > fm) {
		if (!mem_warn_given) {
		    diagapplet.utils.DiagError.println("Need " + (8 * booleans_in_message) + " bytes but only " + fm + " free. (Try increasing max memory with -Xmx argument to java launcher.");
		    mem_warn_given = true;
		}
		return true;
	    }
	    boolean new_booleans[] = new boolean[2 * booleans.length];
	    for (int i = 0; i < booleans.length; i++) {
		new_booleans[i] = booleans[i];
	    }
	    booleans = new_booleans;
	}
	return false;
    }
    private static volatile boolean debug_on = false;
    public boolean too_many_vars_error_printed = false;

    public static boolean get_debug_on() {
	return debug_on;
    }

    public static void set_debug_on(boolean b) {
	debug_on = b;
    }

    DiagNMLmsg(int _type) {
	super(_type);
    }

    static public void DebugPrint(String s) {
	try {
	    if (!debug_on) {
		return;
	    }
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    static public void ErrorPrint(String s) {
	try {
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    if (debug_on) {
		System.out.println("ErrorPrint + " + ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
	    }
	    diagapplet.utils.DiagError.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
	    failed_count++;
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
    int consecutive_updates_with_errors = 0;

    HashMap<String,Long> tokenToPosMap = new HashMap<String,Long>();
    HashMap<Long,String> posToTokenMap = new HashMap<Long,String>();
    
    public void update(NMLFormatConverter NMLfc) {
	try {
	    NMLfc.set_diagnostics_mode(true);
	    if (debug_on) {
		try {
		    Thread.dumpStack();
		} catch (Exception e) {
		    e.printStackTrace();
		}
	    }

            tokenToPosMap = new HashMap<String,Long>();
            posToTokenMap = new HashMap<Long,String>();
	    byte_arrays_in_message = 0;
	    shorts_in_message = 0;
	    ints_in_message = 0;
	    floats_in_message = 0;
            longs_in_message = 0;
	    doubles_in_message = 0;
	    bytes_in_message = 0;
	    booleans_in_message = 0;

	    typeInfo = null;
	    Hashtable lengths_hashtable = new Hashtable();
	    boolean ndla_var_to_skip = false;
	    Long ltype = null;

	    if (debug_on) {
		DebugPrint("DiagNMLMsg.update() type=" + type + "ints[0]=" + ints[0]);
	    }

	    if (type < 1 && NMLfc.type_string != null) {
		if (debug_on) {
		    DebugPrint("DiagNMLMsg.update() NMLfc.type_string=" + NMLfc.type_string);
		}
		typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(NMLfc.type_string);
		type = (int) typeInfo.Id;
		NMLfc.msg_type = type;
	    } else {
		if (cmd_stream) {
		    ltype = Long.valueOf(type);
		    typeInfo = (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(ltype);
		    if (typeInfo == null && null != ModuleInfo.m_structInfoHashTable) {
			typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoHashTable.get(ltype);
		    }
		    if (typeInfo != null && typeInfo.conflicts) {
			if (null != module_info &&
				null != module_info.get_conflict_m_structInfoHashTable() &&
				null != module_info.get_conflict_m_structInfoHashTable().get(ltype)) {
			    typeInfo = (StructureTypeInfo) module_info.get_conflict_m_structInfoHashTable().get(ltype);
			}
		    }

		} else if (stat_stream && ModuleInfo.m_stat_structInfoHashTable != null) {
		    ltype = Long.valueOf(type);
		    typeInfo = (StructureTypeInfo) ModuleInfo.m_stat_structInfoHashTable.get(ltype);
		    if (typeInfo == null && null != ModuleInfo.m_structInfoHashTable) {
			typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoHashTable.get(ltype);
		    }
		    if (typeInfo != null && typeInfo.conflicts) {
			if (null != module_info &&
				null != module_info.get_conflict_m_structInfoHashTable() &&
				null != module_info.get_conflict_m_structInfoHashTable().get(ltype)) {
			    typeInfo = (StructureTypeInfo) module_info.get_conflict_m_structInfoHashTable().get(ltype);
			}
		    }
		} else {
		    ltype = Long.valueOf(type);
		    typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoHashTable.get(ltype);
		    if (typeInfo == null && null != ModuleInfo.m_stat_structInfoHashTable) {
			typeInfo = (StructureTypeInfo) ModuleInfo.m_stat_structInfoHashTable.get(ltype);
		    }
		    if (typeInfo == null && null != ModuleInfo.m_cmd_structInfoHashTable) {
			typeInfo = (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(ltype);
		    }
		    if (typeInfo != null && typeInfo.conflicts) {
			if (null != module_info &&
				null != module_info.get_conflict_m_structInfoHashTable() &&
				null != module_info.get_conflict_m_structInfoHashTable().get(ltype)) {
			    typeInfo = (StructureTypeInfo) module_info.get_conflict_m_structInfoHashTable().get(ltype);
			}
		    }
		}
	    }
	    if (debug_on) {
		DebugPrint("DiagNMLMsg.update() typeInfo=" + typeInfo);
	    }
	    if (null == typeInfo) {
		ErrorPrint("DiagNMLMsg.update() typeInfo=null, stat_stream=" + stat_stream + ", cmd_stream=" + cmd_stream + ", type=" + type + ",module_info=" + module_info + ", ltype=" + ltype);
		NMLfc.SetErrorInUpdate("DiagNMLMsg.update() typeInfo=null, stat_stream=" + stat_stream + ", cmd_stream=" + cmd_stream + ",type=" + type + ",module_info=" + module_info);
		return;
	    }
	    typeInfo.startInfoTokens();
	    String lastclassvarname = "";
	    boolean ndla_var = false;
	    if (debug_on) {
		DebugPrint("DiagNMLMsg.update(), NMLfc.beginClass(" + typeInfo.getName() + "," + typeInfo.DerivedFrom + ");");
	    }
	    NMLfc.beginClass(typeInfo.getName(), typeInfo.DerivedFrom);
	    String info_token = null;
	    NMLfc.error_in_update = false;
	    NMLfc.sending_short = module_info.sending_short_string;
	    STI_TokenizerInterface stiti = typeInfo.getInfoTokenizer();
	    int tok_number = 0;
	    while (stiti.hasMoreTokens()) {
		if (NMLfc.error_in_update) {
		    if (!NMLfc.sending_short) {
			ErrorPrint("DiagNmlMsg.java : info_token=" + info_token + ", NMLfc.error_in_update=" + NMLfc.error_in_update + ", ndla_var=" + ndla_var + ",ndla_var_to_skip=" + ndla_var_to_skip + ", lengths_hashtable=" + lengths_hashtable + ", typeInfo=" + typeInfo + ",module_info=" + module_info);

		    }
		    break;
		}
		ndla_var = false;
		info_token = stiti.nextToken();
                posToTokenMap.put(NMLfc.getPos(), info_token);
                tokenToPosMap.put(info_token,NMLfc.getPos());
		tok_number++;
		if (debug_on) {
		    DebugPrint("DiagNMLMsg.update(), info_token=" + info_token);
		}
		if (null != info_token &&
			info_token.startsWith(SplitInfoToken.ndla_string)) {
		    ndla_var = true;
		    info_token = info_token.substring(SplitInfoToken.ndla_string.length());
		}
		int array_length = 1;
		boolean an_array = false;
		int endvarnameindex = info_token.length() - 1;
		char c = info_token.charAt(endvarnameindex);
		if (debug_on) {
		    DebugPrint("DiagNMLMsg.update(), endvarnameindex=" + endvarnameindex + ",c=" + c);
		}
		endvarnameindex++;
		while (!(c >= 'a' && c <= 'z') && !(c >= 'A' && c <= 'Z') && endvarnameindex > 0) {
		    if (debug_on) {
			DebugPrint("DiagNMLMsg.update(), endvarnameindex=" + endvarnameindex + ",c=" + c);
		    }
		    endvarnameindex--;
		    c = info_token.charAt(endvarnameindex);
		}
		if (endvarnameindex < info_token.length() - 1) {
		    endvarnameindex++;
		    c = info_token.charAt(endvarnameindex);
		    if (debug_on) {
			DebugPrint("DiagNMLMsg.update(), endvarnameindex=" + endvarnameindex + ",c=" + c);
		    }
		    while (c >= '0' && c <= '9' && endvarnameindex < info_token.length() - 1) {
			if (debug_on) {
			    DebugPrint("DiagNMLMsg.update(), endvarnameindex=" + endvarnameindex + ",c=" + c);
			}
			endvarnameindex++;
			c = info_token.charAt(endvarnameindex);
		    }
		}
		int beginvarnameindex = info_token.lastIndexOf(' ', endvarnameindex - 1);
		String fullvarname = info_token.substring(beginvarnameindex + 1);
		String fullvarnoarray = info_token.substring(beginvarnameindex + 1, endvarnameindex);
		int fullvarnoarray_lsqpindex = fullvarnoarray.indexOf('[');
		while (fullvarnoarray_lsqpindex >= 0) {
		    int fullvarnoarray_rsqpindex =
			    fullvarnoarray.indexOf(']', fullvarnoarray_lsqpindex);
		    if (fullvarnoarray_rsqpindex < 0) {
			fullvarnoarray = fullvarnoarray.substring(0, fullvarnoarray_lsqpindex);
			break;
		    }
		    String tempfullvarnoarray =
			    fullvarnoarray.substring(0, fullvarnoarray_lsqpindex) +
			    fullvarnoarray.substring(fullvarnoarray_rsqpindex + 1);
		    fullvarnoarray = tempfullvarnoarray;
		    fullvarnoarray_lsqpindex = fullvarnoarray.indexOf('[', fullvarnoarray_lsqpindex + 1);
		}
		String varname = fullvarnoarray;
		String classvarname = "";
		int lastperiodindex = fullvarnoarray.lastIndexOf('.');
		if (lastperiodindex >= 0) {
		    varname = fullvarnoarray.substring(lastperiodindex + 1);
		    int fullvarname_last_index = fullvarname.lastIndexOf('.');
		    classvarname = fullvarname.substring(0, fullvarname_last_index);
		}
		if (debug_on) {
		    DebugPrint("DiagNMLMsg.update(), info_token=" + info_token + ",classvarname=" + classvarname + ",varname=" + varname + ",fullvarnoarray=" + fullvarnoarray + ",fullvarname=" + fullvarname + ",lastperiodindex=" + lastperiodindex + ",ndla_var=" + ndla_var);
		}
		ndla_var_to_skip = false;
		int force_char_array_len = -1;
		try {
		    if (ndla_var) {

			int lsqindex2 = fullvarname.indexOf('[');
			if (debug_on) {
			    DebugPrint("fullvarname=" + fullvarname + ", lsqindex2=" + lsqindex2);
			}
			while (lsqindex2 > 0) {
			    String varname2 = fullvarname.substring(0, lsqindex2);
			    int rsqindex2 = fullvarname.indexOf(']', lsqindex2);
			    if (debug_on) {
				DebugPrint("fullvarname=" + fullvarname + ", lsqindex2=" + lsqindex2 + ",rsqindex2=" + rsqindex2 + ",varname2=" + varname2);
			    }
			    if (rsqindex2 <= lsqindex2 + 1) {
				break;
			    }
			    String index_str2 = fullvarname.substring(lsqindex2 + 1, rsqindex2);
			    Integer I2 = (Integer) lengths_hashtable.get(varname2);
			    int i2_intval = -1;
			    int index_str2_intval = -1;
			    try {
				if (null != I2) {
				    i2_intval = I2.intValue();
				}
			    } catch (Exception e) {
				e.printStackTrace();
			    }
			    try {
				index_str2_intval = Integer.valueOf(index_str2).intValue();
			    } catch (Exception e) {
				e.printStackTrace();
			    }
			    if (debug_on) {
				DebugPrint("index_str2=" + index_str2 + ", I2=" + I2 + ", info_token=" + info_token + ", fullvarname.length()=" + fullvarname.length() + ", i2_intval=" + i2_intval + ", index_str2_intval=" + index_str2_intval);
			    }
			    if (null != I2 && index_str2_intval >= i2_intval) {
				ndla_var_to_skip = true;
				if ((info_token.indexOf(" char ") >= 0 || info_token.startsWith("char ")) &&
					rsqindex2 >= (fullvarname.length() - 1) &&
					i2_intval > 0) {
				    ndla_var_to_skip = false;
				    force_char_array_len = i2_intval;
				}
				break;
			    }
			    lsqindex2 = fullvarname.indexOf('[', rsqindex2);
			}
		    }
		} catch (Exception e) {
		    e.printStackTrace();
		}
		if (debug_on && NMLfc.get_use_string()) {
		    DebugPrint("NMLfc.get_token_count()=" + NMLfc.get_token_count());
		}
		if (ndla_var_to_skip) {
		    do {
			if (NMLfc.get_use_string()) {
			    if (NMLfc.get_decoding()) {
				NMLfc.throw_away_token();
			    } else {
				NMLfc.add_to_output_string("0,");
			    }
			    if (debug_on) {
				DebugPrint("NMLfc.get_token_count()=" + NMLfc.get_token_count());
			    }
			}
		    } while (stiti.skipInfoTokenInSameArray());

		    continue;
		}
		if (!classvarname.equals(lastclassvarname)) {
		    StringTokenizer tz = new StringTokenizer(classvarname, ".");
		    StringTokenizer tzl = new StringTokenizer(lastclassvarname, ".");
		    String first_class_to_begin = null;
		    String last_class_to_end = null;
		    while (tz.hasMoreTokens() && tzl.hasMoreTokens()) {
			String tk = tz.nextToken();
			String tkl = tzl.nextToken();
			if (!tk.equals(tkl)) {
			    first_class_to_begin = tk;
			    last_class_to_end = tkl;
			    break;
			}
		    }
		    Stack s = new Stack();
		    if (null != last_class_to_end) {
			s.push(last_class_to_end);
		    }
		    while (tzl.hasMoreTokens()) {
			String classtoend = tzl.nextToken();
			s.push(classtoend);
		    }
		    while (!s.empty()) {
			String classtoend = (String) s.pop();
			if (debug_on) {
			    DebugPrint("DiagNMLMsg.update(), NMLfc.endClassVar(" + classtoend + ");");
			}
			int bindex = classtoend.indexOf('[');
			if (bindex > 0) {
			    int endindex = classtoend.indexOf(']');
			    if (endindex > bindex) {
				String classVarIndexString = classtoend.substring(bindex + 1, endindex);
				NMLfc.set_classVarArrayIndex(Integer.valueOf(classVarIndexString).intValue());
			    }
			    classtoend = classtoend.substring(0, bindex);
			}
			NMLfc.endClassVar(classtoend);
		    }
		    if (null != first_class_to_begin) {
			int bindex = first_class_to_begin.indexOf('[');
			if (bindex > 0) {
			    int endindex = first_class_to_begin.indexOf(']');
			    if (endindex > bindex) {
				String classVarIndexString = first_class_to_begin.substring(bindex + 1, endindex);
				NMLfc.set_classVarArrayIndex(Integer.valueOf(classVarIndexString).intValue());
			    }
			    first_class_to_begin = first_class_to_begin.substring(0, bindex);
			}
			NMLfc.beginClassVar(first_class_to_begin);
		    }
		    while (tz.hasMoreTokens()) {
			String classtobegin = tz.nextToken();
			if (debug_on) {
			    DebugPrint("DiagNMLMsg.update(), NMLfc.beginClassVar(" + classtobegin + ");");
			}
			int bindex = classtobegin.indexOf('[');
			if (bindex > 0) {
			    int endindex = classtobegin.indexOf(']');
			    if (endindex > bindex) {
				String classVarIndexString = classtobegin.substring(bindex + 1, endindex);
				NMLfc.set_classVarArrayIndex(Integer.valueOf(classVarIndexString).intValue());
			    }
			    classtobegin = classtobegin.substring(0, bindex);
			}
			NMLfc.beginClassVar(classtobegin);
		    }
		}
		lastclassvarname = classvarname;
		boolean unsigned_var =
			(info_token.indexOf("unsigned ") != -1);
		int last_period = info_token.lastIndexOf('.');
		if (last_period < 0) {
		    last_period = 0;
		}
		int l_square_paren_index = info_token.substring(last_period).indexOf('[');
		int r_square_paren_index = info_token.substring(last_period).indexOf(']');
		int array_val = -1;
		if (r_square_paren_index > l_square_paren_index &&
			l_square_paren_index > 0) {
		    String array_length_string = info_token.substring(l_square_paren_index + 1 + last_period, r_square_paren_index + last_period);
		    array_val = Integer.valueOf(array_length_string).intValue();
		}
		NMLfc.set_array_val(array_val);
		if (-1 != info_token.indexOf("char ")) {
		    try {
			if (array_val > 0) {
			    array_length = array_val;
			    an_array = true;
			}
			if (!an_array) {
			    if (check_bytes_in_msg()) {
				if (!too_many_vars_error_printed) {
				    ErrorPrint("DiagNMLMsgDict.format() : Too many bytes.\n");
				    too_many_vars_error_printed = true;
				}
				NMLfc.SetErrorInUpdate("check_bytes_in_msg() failed.");
				return;
			    }
			    if (unsigned_var) {
				if (debug_on) {
				    DebugPrint("DiagNMLMsg.update(), update_unsigned(bytes[" + bytes_in_message + "]) = " + bytes[bytes_in_message]);
				}
				bytes[bytes_in_message] = NMLfc.update_unsigned_with_name(varname, bytes[bytes_in_message]);
			    } else {
				if (debug_on) {
				    DebugPrint("DiagNMLMsg.update(), update(bytes[" + bytes_in_message + "]) = " + bytes[bytes_in_message]);
				}
				bytes[bytes_in_message] = NMLfc.update_with_name(varname, bytes[bytes_in_message]);
			    }
			    bytes_in_message++;
			} else {

			    ByteArrayObject bao = null;
			    if (force_char_array_len > 0 && force_char_array_len < array_length) {
				array_length = force_char_array_len;
			    }
			    if (byte_arrays_in_message >= byte_arrays.size()) {
				bao = new ByteArrayObject();
				bao.array = new byte[array_length];
				byte_arrays.addElement(bao);
			    } else {
				bao = (ByteArrayObject) byte_arrays.elementAt(byte_arrays_in_message);
			    }
			    if (bao.array == null) {
				bao.array = new byte[array_length];
			    }
			    if (bao.array.length < array_length) {
				bao.array = new byte[array_length];
			    }
			    if (debug_on) {
				DebugPrint("DiagNMLMsg.update(), update(byte_arrays[" + byte_arrays_in_message + "], " + array_length + ")");
			    }
			    if (unsigned_var) {
				NMLfc.update_unsigned_with_name(varname, bao.array, array_length);
			    } else {
				NMLfc.update_with_name(varname, bao.array, array_length);
			    }
			    if (debug_on) {
				for (int ii = 0; ii < array_length; ii++) {
				    System.out.print((Integer.toString((int) bao.array[ii])) + " ");
				}
				DebugPrint("");
			    }
			    byte_arrays_in_message++;
			}
		    } catch (Exception e) {
			e.printStackTrace();
		    }
		    continue;
		}
		if (-1 != info_token.indexOf("short ")) {
		    if (check_shorts_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many shorts.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_shorts_in_msg() failed.");
			return;
		    }
		    if (unsigned_var) {
			if (debug_on && !NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update(), update_unsigned_with_name(" + varname + ",shorts[" + shorts_in_message + "] = " + shorts[shorts_in_message] + ")");
			}
			shorts[shorts_in_message] = NMLfc.update_unsigned_with_name(varname, shorts[shorts_in_message]);
			if (debug_on && NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update()," + shorts[shorts_in_message] + "= update_unsigned_with_name(" + varname + ",shorts[" + shorts_in_message + "])");
			}
		    } else {
			if (debug_on && !NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",shorts[" + shorts_in_message + "] = " + shorts[shorts_in_message] + ")");
			}
			shorts[shorts_in_message] = NMLfc.update_with_name(varname, shorts[shorts_in_message]);
			if (debug_on && NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update()," + shorts[shorts_in_message] + "= update_with_name(" + varname + ",shorts[" + shorts_in_message + "])");
			}
		    }
		    shorts_in_message++;
		    continue;
		}
                if (-1 != info_token.indexOf("long ")) {
		    if (check_longs_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many longs.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_longs_in_msg() failed.");
			return;
		    }
		    if (unsigned_var) {
			if (debug_on && !NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update(), update_unsigned_with_name(" + varname + ",longs[" + longs_in_message + "] = " + longs[longs_in_message] + ")");
			}
			longs[longs_in_message] = NMLfc.update_unsigned_with_name(varname, longs[longs_in_message]);
			if (debug_on && NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update()," + longs[longs_in_message] + "= update_unsigned_with_name(" + varname + ",longs[" + longs_in_message + "])");
			}
		    } else {
			if (debug_on && !NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",longs[" + longs_in_message + "] = " + longs[longs_in_message] + ")");
			}
			longs[longs_in_message] = NMLfc.update_with_name(varname, longs[longs_in_message]);
			if (debug_on && NMLfc.get_decoding()) {
			    DebugPrint("DiagNMLMsg.update()," + longs[longs_in_message] + "= update_with_name(" + varname + ",longs[" + longs_in_message + "])");
			}
		    }
		    longs_in_message++;
		    continue;
		}
		if (-1 != info_token.indexOf("float ")) {
		    if (check_floats_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many floats.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_floats_in_msg() failed.");
			return;
		    }
		    if (debug_on && !NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",floats[" + floats_in_message + "] = " + floats[floats_in_message] + ")");
		    }
		    floats[floats_in_message] = NMLfc.update_with_name(varname, floats[floats_in_message]);
		    if (debug_on && NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update()," + floats[floats_in_message] + "= update_with_name(" + varname + ",floats[" + floats_in_message + "])");
		    }
		    floats_in_message++;
		    continue;
		}
		if (-1 != info_token.indexOf("double ")) {
		    if (check_doubles_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many doubles.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_doubles_in_msg() failed.");
			return;
		    }
		    if (debug_on && !NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",doubles[" + doubles_in_message + "] = " + doubles[doubles_in_message] + ")");
		    }
		    doubles[doubles_in_message] = NMLfc.update_with_name(varname, doubles[doubles_in_message]);
		    if (debug_on && NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update()," + doubles[doubles_in_message] + "= update_with_name(" + varname + ",doubles[" + doubles_in_message + "])");
		    }
		    doubles_in_message++;
		    continue;
		}
		if (-1 != info_token.indexOf("bool ")) {
		    if (check_booleans_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many booleans.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_booleans_in_msg() failed.");
			return;
		    }
		    if (debug_on && !NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",booleans[" + booleans_in_message + "] = " + booleans[booleans_in_message] + ")");
		    }
		    booleans[booleans_in_message] = NMLfc.update_with_name(varname, booleans[booleans_in_message]);
		    if (debug_on && NMLfc.get_decoding()) {
			DebugPrint("DiagNMLMsg.update()," + booleans[booleans_in_message] + "= update_with_name(" + varname + ",booleans[" + booleans_in_message + "])");
		    }
		    booleans_in_message++;
		    continue;
		}
		if (-1 != info_token.indexOf("enum ")) {
		    if (check_ints_in_msg()) {
			if (!too_many_vars_error_printed) {
			    ErrorPrint("DiagNMLMsgDict.format() : Too many ints.\n");
			    too_many_vars_error_printed = true;
			}
			NMLfc.SetErrorInUpdate("check_ints_in_msg() failed");
			return;
		    }
		    if (debug_on) {
			DebugPrint("DiagNMLMsg.update(), update(ints[" + ints_in_message + "]) = " + ints[ints_in_message]);
		    }
		    StringTokenizer tokentokenizer = new StringTokenizer(info_token, ",[]; \t\r\n");
		    EnumTypeInfo eti = null;
		    while (tokentokenizer.hasMoreTokens() && null == eti) {
			String t = tokentokenizer.nextToken();
			eti = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(t);
		    }
		    if (null != eti) {
			NML_ENUM_INFO nei = new NML_ENUM_INFO();
			nei.name = eti.Name;
			nei.string_to_int_hash = eti.reverse_hashtable;
			nei.int_to_string_hash = eti.hashtable;
			ints[ints_in_message] = NMLfc.update_enumeration_with_name(varname, ints[ints_in_message], nei);
			ints_in_message++;
		    } else {
			if (unsigned_var) {
			    if (debug_on && !NMLfc.get_decoding()) {
				DebugPrint("DiagNMLMsg.update(), update_unsigned_with_name(" + varname + ",ints[" + ints_in_message + "] = " + ints[ints_in_message] + ")");
			    }
			    ints[ints_in_message] = NMLfc.update_unsigned_with_name(varname, ints[ints_in_message]);
			    if (debug_on && NMLfc.get_decoding()) {
				DebugPrint("DiagNMLMsg.update()," + ints[ints_in_message] + "= update_unsigned_with_name(" + varname + ",ints[" + ints_in_message + "])");
			    }
			} else {
			    if (debug_on && !NMLfc.get_decoding()) {
				DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",ints[" + ints_in_message + "] = " + ints[ints_in_message] + ")");
			    }
			    ints[ints_in_message] = NMLfc.update_with_name(varname, ints[ints_in_message]);

			    if (debug_on && NMLfc.get_decoding()) {
				DebugPrint("DiagNMLMsg.update()," + ints[ints_in_message] + "= update_with_name(" + varname + ",ints[" + ints_in_message + "])");
			    }
			}
			ints_in_message++;
		    }
		    continue;
		}
		if (check_ints_in_msg()) {
		    if (!too_many_vars_error_printed) {
			ErrorPrint("DiagNMLMsgDict.format() : Too many ints.\n");
			too_many_vars_error_printed = true;
		    }
		    NMLfc.SetErrorInUpdate("check_ints_in_msg() failed.");
		    return;
		}
		if (debug_on && !NMLfc.get_decoding()) {
		    DebugPrint("DiagNMLMsg.update(), update_with_name(" + varname + ",ints[" + ints_in_message + "] = " + ints[ints_in_message] + ")");
		}
//                if (info_token.indexOf("heartbeat") >= 0) {
//                    System.out.println("ints[ints_in_message] = " + ints[ints_in_message]);
//                }
		if (unsigned_var) {
		    ints[ints_in_message] = NMLfc.update_unsigned_with_name(varname, ints[ints_in_message]);
		} else {
		    ints[ints_in_message] = NMLfc.update_with_name(varname, ints[ints_in_message]);

		}
//                if (info_token.indexOf("heartbeat") >= 0) {
//                    System.out.println("ints[ints_in_message] = " + ints[ints_in_message]);
//                }
		if (debug_on && NMLfc.get_decoding()) {
		    DebugPrint("DiagNMLMsg.update()," + ints[ints_in_message] + "= update_with_name(" + varname + ",ints[" + ints_in_message + "])");
		}
		if (varname.endsWith("_length")) {
		    if (debug_on) {
			DebugPrint("lengths_hashtable.put(" + fullvarname.substring(0, fullvarname.length() - 7) + ", " + ints[ints_in_message] + ")");
		    }
		    lengths_hashtable.put(fullvarname.substring(0, fullvarname.length() - 7), (ints[ints_in_message]));
		}
		ints_in_message++;
		continue;
	    }
	    if (NMLfc.error_in_update) {
		if (!NMLfc.sending_short) {
		    ErrorPrint("DiagNmlMsg.java : info_token=" + info_token + ", NMLfc.error_in_update=" + NMLfc.error_in_update + ", ndla_var=" + ndla_var + ",ndla_var_to_skip=" + ndla_var_to_skip + ", typeInfo=" + typeInfo + ",module_info=" + module_info);
		}
	    }
	    if (null != lastclassvarname && lastclassvarname.length() > 0) {
		StringTokenizer tzl = new StringTokenizer(lastclassvarname, ".");
		Stack s = new Stack();
		while (tzl.hasMoreTokens()) {
		    String classtoend = tzl.nextToken();
		    s.push(classtoend);
		}
		while (!s.empty()) {
		    String classtoend = (String) s.pop();
		    int bindex = classtoend.indexOf('[');
		    if (bindex > 0) {
			int endindex = classtoend.indexOf(']');
			if (endindex > bindex) {
			    String classVarIndexString = classtoend.substring(bindex + 1, endindex);
			    NMLfc.set_classVarArrayIndex(Integer.valueOf(classVarIndexString).intValue());
			}
			classtoend = classtoend.substring(0, bindex);
		    }
		    if (debug_on) {
			DebugPrint("DiagNMLMsg.update(), NMLfc.endClassVar(" + classtoend + ");");
		    }
		    NMLfc.endClassVar(classtoend);
		}
	    }
	    NMLfc.endClass(typeInfo.getName(), typeInfo.DerivedFrom);
	    if (debug_on) {
		DebugPrint("DiagNMLMsg.update(), NMLfc.endClass(" + typeInfo.getName() + "," + typeInfo.DerivedFrom + ");");
	    }
	    NMLfc.error_in_update = false;
	    NMLfc.sending_short = false;
	    consecutive_updates_with_errors = 0;
	} catch (Exception e) {
	    consecutive_updates_with_errors++;
	    if (consecutive_updates_with_errors < 5) {
		//e.printStackTrace();
		ErrorPrint("consecutive_updates_with_errors=" + consecutive_updates_with_errors + "\n");
		ErrorPrint("typeInfo=" + typeInfo + "\n");
		ErrorPrint("this=" + this + "\n");
	    }
	}
	if (null != NMLfc) {
	    NMLfc.error_in_update = false;
	}
    }
}
