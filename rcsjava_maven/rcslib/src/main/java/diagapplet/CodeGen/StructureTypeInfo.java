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

import java.util.*;
import rcs.utils.StackTracePrinter;

/**
 * Class with information about one C++ structure.
 * @author Will Shackleford
 */
public class StructureTypeInfo implements Comparable {

    private String Name = "";
    public String NameSpace = "";
    public String CppQualifiedName = "";
    public boolean inside_namespace = false;
    public long Id = -2;
    public String type_id_string = null;
    public String RawInfo = "";
    public String HiddenInfo = "";
    public String StepTwoInfo = "";
    public String PreFinalPassInfo = "";
    public String CppUpdateFunction = null;
    public String C_UpdateFunction = null;
    public String CppConstructor = null;
    public String JavaDefinition = null;
    public String JavaClassArrayInitializers = "";
    public String DerivedFrom = null;
    public String UnqualifiedDerivedFrom = null;
    public String JavaUpdateFunction = null;
    private String BaseClassExpandedPreFinalPassInfo = null;
    private Hashtable last_structInfoByNameHashtable = null;
    private boolean last_skip_command_stat = false;
    public boolean is_rcs_cmd_msg = false;
    public boolean is_rcs_stat_msg = false;
    public final List<DefinedValue> usedDefinedValues=new ArrayList<>();

    public String getBaseClassExpandedPreFinalPassInfo(Hashtable structInfoByNameHashtable, boolean skip_command_stat) {
	if (null != BaseClassExpandedPreFinalPassInfo &&
		structInfoByNameHashtable == last_structInfoByNameHashtable &&
		last_skip_command_stat == skip_command_stat) {
	    return this.BaseClassExpandedPreFinalPassInfo;
	}
	String s = this.PreFinalPassInfo;
	String top_parent = this.DerivedFrom;
	while (null != top_parent) {
	    StructureTypeInfo parent_sti = (StructureTypeInfo) structInfoByNameHashtable.get(top_parent);
	    if (null != parent_sti) {
		if (skip_command_stat && (parent_sti.Name.equals("RCS_CMD_MSG") || parent_sti.Name.equals("RCS_STAT_MSG"))) {
		    top_parent = null;
		    break;
		}
		s = parent_sti.PreFinalPassInfo + s;
		top_parent = parent_sti.DerivedFrom;
		continue;
	    }
	    top_parent = null;
	    break;
	}
	this.BaseClassExpandedPreFinalPassInfo = s;
	last_structInfoByNameHashtable = structInfoByNameHashtable;
	last_skip_command_stat = skip_command_stat;
	return s;
    }
    public diagapplet.CodeGen.ModuleInfoInterface first_module_used_in = null;
    public String fromFileName = null;
    public int fromLineNumber = -1;
    public String fromLineText = "";
    public boolean generic = false;
    public boolean destructor_declared = false;
    public boolean constructor_declared = false;
    public boolean destructor_declared_and_not_inlined = false;
    public boolean constructor_declared_and_not_inlined = false;
    public boolean selected = false;
    public boolean is_nml_msg = false;
    public boolean is_union = false;
    public Hashtable VarnameToDefaultsHashTable = new Hashtable();
    public Hashtable VarnameOverridesHashTable = new Hashtable();
    public Hashtable VarnameAttributeInfoHashTable = new Hashtable();
    public Hashtable VarnameNDLAHashTable = new Hashtable();
    public Hashtable VarnameUnboundedHashTable = new Hashtable();
    public boolean have_initialize = false;
    public boolean c_struct_redefined = false;
    public int dependancy_rank = 0;
    public boolean dependancy_rank_determined = false;
    public int sti_number;
    public boolean on_aux_msg_list = false;
    static public boolean debug_on = false;
    public int estimated_size;
    public boolean conflicts = false;
    public boolean contains_pointers = false;
    public boolean contains_unrecognized_type = false;
    private int var_count_size = -1;

    public int get_var_count_size() {
	try {
	    if (var_count_size < 0) {
		STI_TokenizerInterface sti_ti = getInfoTokenizer();
		if (null != sti_ti) {
		    var_count_size = 0;
		    while (sti_ti.hasMoreTokens()) {
			sti_ti.throwAwayToken();
			var_count_size++;
		    }
		}
	    }
	    String top_parent = this.DerivedFrom;
	    String last_token = "";
	    while (null != top_parent) {
//                System.out.println("top_parent = " + top_parent);
		if (top_parent.equals("RCS_CMD_MSG")) {
		    this.is_rcs_cmd_msg = true;
		    break;
		}
		if (top_parent.equals("RCS_STAT_MSG")) {
		    this.is_rcs_stat_msg = true;
		    break;
		}
		StructureTypeInfo parentInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(top_parent);
		if (null == parentInfo) {
		    break;
		}
		top_parent = parentInfo.DerivedFrom;
	    }

	    if (is_rcs_cmd_msg) {
		var_count_size -= 1;
	    } else if (is_rcs_stat_msg) {
		var_count_size -= 7;
	    }
//            System.out.println("var_count_size = " + var_count_size);
//            System.out.println("Name = " + Name);
//            System.out.println("is_rcs_stat_msg = " + is_rcs_stat_msg);
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return var_count_size;
    }

    static String ias(int ia[]) {
	if (null == ia) {
	    return "{{!null!}}";
	}
	String s = "{";
	for (int i = 0; i < ia.length - 1; i++) {
	    s += ia[i] + ",";
	}
	s += ia[ia.length - 1] + "}";
	return s;
    }

    static public void ErrorPrint(String s) {
	try {
	    Throwable t = new Throwable();
	    System.err.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    static public void DebugPrint2(String s) {
	try {
	    Throwable t = new Throwable();
	    System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
	    System.out.println("time=" + System.currentTimeMillis());
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    static public void DebugPrint(String s) {
	try {
	    if (!debug_on) {
		return;
	    }
	    Throwable t = new Throwable();
	    System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);

	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public int compareTo(Object o) throws ClassCastException {
	StructureTypeInfo other_sti = (StructureTypeInfo) o;
	try {
	    if (dependancy_rank_determined &&
		    other_sti.dependancy_rank_determined &&
		    dependancy_rank != other_sti.dependancy_rank) {
		return dependancy_rank - other_sti.dependancy_rank;
	    }
	    return Name.compareTo(other_sti.Name);
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return (1);
    }

    public String toString() {
	int info_array_length = 0;
	if (info_array != null) {
	    info_array_length = info_array.length;
	}
	StringBuffer sb = new StringBuffer("{ \n\tName=" + Name + "\n");
	if (inside_namespace ||
		(null != NameSpace && NameSpace.length() > 0) ||
		(null != CppQualifiedName && CppQualifiedName.length() > 0 && !CppQualifiedName.equals(Name))) {
	    sb.append("\tNameSpace=" + NameSpace + "\n" +
		    "\tCppQualifiedName=" + CppQualifiedName + "\n");
	}
	sb.append("\tinside_namespace=" + inside_namespace + "\n" +
		"\tRawInfo = " + RawInfo + "\n" +
		"\tHiddenInfo = " + HiddenInfo + "\n" +
		"\tStepTwoInfo = " + StepTwoInfo + "\n" +
		"\tcurToken = " + curToken + "\n" +
		"\tlast_sout = " + last_sout + "\n" +
		"\tinfo_array.length=" + info_array_length + "\n" +
		"\tId = " + Id + "\n" +
		"\tDerivedFrom = " + DerivedFrom + "\n" +
		"\tfromFile = " + fromFileName + "\n" +
		"\tis_nml_msg = " + is_nml_msg + "\n" +
		"\tgeneric = " + generic + "\n" +
		"\tselected = " + selected + "\n" +
		"\tPreFinalPassInfo = " + PreFinalPassInfo + "\n" +
		"\tusedDefinedValues = " + usedDefinedValues + "\n" +
		"\tCppUpdateFunction = " + CppUpdateFunction + "\n" +
		"\tCppConstructor = " + CppConstructor + "\n" +
		"\tJavaDefinition = " + JavaDefinition + "\n" +
		"\tJavaClassArrayInitializers = " + JavaClassArrayInitializers + "\n" +
		"\tJavaUpdateFunction = " + JavaUpdateFunction + "\n" +
		"\tgeneric = " + generic + "\n" +
		"\tdestructor_declared_and_not_inlined = " + destructor_declared_and_not_inlined + "\n" +
		"\tconstructor_declared_and_not_inlined = " + constructor_declared_and_not_inlined + "\n" +
		"\ttype_id_string=" + type_id_string + "\n" +
		"\testimated_size=" + estimated_size + "\n" +
		"\tcontains_pointers=" + contains_pointers + "\n" +
		"}\n");
	return sb.toString();
    }
    StringTokenizer st = null;

    public void setInfo(String newinfo) {
	HiddenInfo = newinfo;
    }

    public void setPreFinalPassInfoToInfo() {
	PreFinalPassInfo = HiddenInfo;
    }
    String info_array[] = null;
    info_array_elem_info array_elem_info[] = null;
    boolean same_struct[] = null;
    String last_struct = null;

    public void startInfoTokens() {
	if (null == info_array && null != HiddenInfo) {
	    st = new StringTokenizer(HiddenInfo, ";");
	    info_array = new String[st.countTokens()];
	    array_elem_info = new info_array_elem_info[st.countTokens()];
	    same_struct = new boolean[st.countTokens()];
	    int i = 0;
	    while (st.hasMoreTokens()) {
		String s = st.nextToken();
		info_array[i] = s;
		array_elem_info[i] = parseToken(info_array[i], i);
		int spc_index = s.lastIndexOf(' ');
		int p_index = s.lastIndexOf('.');
		same_struct[i] = false;
		if (spc_index > 0 && p_index > spc_index) {
		    String struct_name = s.substring(spc_index + 1, p_index);
		    if (null != last_struct && struct_name.length() > 0 &&
			    struct_name.equals(last_struct) &&
			    s.indexOf('[', p_index) < 0) {
			same_struct[i] = true;
		    }
		    last_struct = struct_name;
		} else {
		    last_struct = null;
		}
		//DebugPrint2(("info_array["+i+"]="+info_array[i]+", array_elem_info["+i+"]="+array_elem_info[i]+",same_struct["+i+"]="+same_struct[i]+", last_struct="+last_struct );
		i++;
	    }
	}
	last_array_was_char_array = false;
    }
    int last_array_index = 0;
    int last_array_index_length = -1;
    int array_nesting = -1;
    int array_lengths[] = new int[16];
    int return_indexes[] = new int[16];
    String curToken = null;
    String sections[] = new String[16];
    String last_sout = null;
    boolean last_array_was_char_array = false;
    info_array_elem_info last_aei = null;

    info_array_elem_info parseToken(String s, int index) {
	//DebugPrint2("s="+s+", info_array_index="+info_array_index);
	try {
	    int open_square_index = s.indexOf('[');
	    int close_square_index = s.indexOf(']');
	    int last_close_square_index = s.lastIndexOf(' ');
	    if (open_square_index < 0 || close_square_index < 0) {
		last_array_was_char_array = false;
		return null;
	    }
	    if (last_close_square_index > open_square_index &&
		    open_square_index > 0) {
		last_close_square_index = s.lastIndexOf(' ', open_square_index);
	    }
	    info_array_elem_info new_aei = new info_array_elem_info();
	    new_aei.can_ndla_skip = false;
	    array_nesting = 0;
	    new_aei.varType = s.substring(0, last_close_square_index);
	    boolean new_section_seen = false;
	    new_aei.first_new_section = -1;
	    while (open_square_index > 0 && close_square_index > 0) {
		//DebugPrint2("open_square_index="+open_square_index);
		//DebugPrint2("close_square_index="+close_square_index);
		String new_section = null;
		try {
		    new_section = s.substring(last_close_square_index + 1, open_square_index);
		} catch (Exception e) {
		    ErrorPrint("last_close_sqare_index=" + last_close_square_index + ", open_square_index=" + open_square_index + "s=" + s);
		    e.printStackTrace();
		    break;
		}
		if (sections[array_nesting] == null || !new_section.equals(sections[array_nesting])) {
		    sections[array_nesting] = new_section;
		    if (!new_section_seen) {
			new_aei.first_new_section = array_nesting;
		    }
		    new_section_seen = true;

		}
		if (new_section_seen) {
		    return_indexes[array_nesting] = index;
		    String length_string = s.substring(open_square_index + 1, close_square_index).trim();
		    array_lengths[array_nesting] = Integer.valueOf(length_string).intValue();
		}
		last_close_square_index = close_square_index;
		open_square_index = s.indexOf('[', close_square_index);
		close_square_index = s.indexOf(']', open_square_index);
		array_nesting++;
		if (array_nesting >= array_lengths.length) {
		    int new_array_lengths[] = new int[array_lengths.length * 2];
		    for (int i = 0; i < array_lengths.length; i++) {
			new_array_lengths[i] = array_lengths[i];
		    }
		    array_lengths = new_array_lengths;
		}
		if (array_nesting >= return_indexes.length) {
		    int new_return_indexes[] = new int[return_indexes.length * 2];
		    for (int i = 0; i < return_indexes.length; i++) {
			new_return_indexes[i] = return_indexes[i];
		    }
		    return_indexes = new_return_indexes;
		}
		if (array_nesting >= sections.length) {
		    String new_sections[] = new String[sections.length * 2];
		    for (int i = 0; i < sections.length; i++) {
			new_sections[i] = sections[i];
		    }
		    sections = new_sections;
		}
	    }
	    if (!new_section_seen) {
		new_aei.first_new_section = array_nesting;
	    }
	    new_aei.tail = "";
	    if (last_close_square_index < s.length() - 1) {
		new_aei.tail = s.substring(last_close_square_index + 1);
	    }
	    new_aei.last_array_was_char_array = last_array_was_char_array;
	    if (new_aei.tail.length() < 1 &&
		    (new_aei.varType.equals("char") ||
		    new_aei.varType.indexOf(" char ") > 0 ||
		    new_aei.varType.endsWith(" char") ||
		    new_aei.varType.startsWith("char "))) {
		if (array_nesting > 1) {
		    new_aei.tail = sections[array_nesting - 1] + "[" + array_lengths[array_nesting - 1] + "]";
		}
		array_nesting--;
		last_array_was_char_array = true;
		new_aei.is_char_array = true;
		//DebugPrint2("last_array_was_char_array=true");
	    } else {
		last_array_was_char_array = false;
		new_aei.is_char_array = false;
	    }
	    if (array_nesting < 1) {
		return null;
	    }
	    if (array_nesting <= 0 &&
		    last_aei == null) {
		return null;
	    }
	    if (array_nesting > 0) {
		new_aei.array_lengths = new int[array_nesting];
		new_aei.return_indexes = new int[array_nesting];
		new_aei.sections = new String[array_nesting];
		for (int i = 0; i < array_nesting; i++) {
		    new_aei.sections[i] = sections[i];
		    new_aei.return_indexes[i] = return_indexes[i];
		    new_aei.array_lengths[i] = array_lengths[i];
		}
		if (new_aei.varType.startsWith(SplitInfoToken.ndla_string) &&
			!new_aei.varType.endsWith("_length") &&
			!new_aei.is_char_array &&
			!new_aei.last_array_was_char_array) {
		    new_aei.can_ndla_skip = true;
		}
	    }
	    if (array_nesting > 0) {
		last_aei = new_aei;
	    } else {
		last_aei = null;
	    }
	    for (int i = array_nesting; i < sections.length; i++) {
		sections[i] = null;
	    }
	    for (int i = array_nesting; i < array_lengths.length; i++) {
		array_lengths[i] = 0;
	    }
	    return new_aei;
	} catch (Exception e) {
	    ErrorPrint("s=" + s + ", index=" + index);
	    e.printStackTrace();
	}
	return null;
    }

    public boolean infoContains(String str) {
	try {
	    if (null == HiddenInfo) {
		return false;
	    }
	    if (HiddenInfo.indexOf(str) > 0) {
		return true;
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return false;
    }

    public STI_TokenizerInterface getInfoTokenizer() {
	startInfoTokens();
	return new STI_Tokenizer(this);
    }

    public String getName() {
        return Name;
    }

    public void setName(String Name) {
        if("MP_COORD".equals(Name)) {
            throw new RuntimeException("this="+this);
        }
        this.Name = Name;
    }
}
