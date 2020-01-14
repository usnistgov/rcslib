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

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.Iterator;

class Ada_Generator
{
    public static boolean debug_on=false;
   
    public static String ada_spec_string=null;
    public static String ada_package_string=null;
    static CodeGenCommonInterface2 cgc=null;

    private static String ConvertCppTypeToAdaType(String cpp_type)
    {
	try
	    {
		if(cpp_type.equals("unsigned"))
		    {
			return "unsigned";
		    }
		else if(cpp_type.equals("unsigned int"))
		    {
			return "unsigned";
		    }
		else if(cpp_type.equals("unsigned char"))
		    {
			return "unsigned_char";
		    }
		else if(cpp_type.equals("unsigned short"))
		    {
			return "unsigned_short";
		    }
		else if(cpp_type.equals("unsigned long"))
		    {
			return "unsigned_long";
		    }
		else if(cpp_type.equals("char"))
		    {
			return "char";
		    }
		else if(cpp_type.equals("short"))
		    {
			return "short";
		    }		
		else if(cpp_type.equals("int"))
		    {
			return "int";
		    }
		else if(cpp_type.equals("long"))
		    {
			return "long";
		    }
		else if(cpp_type.equals("float"))
		    {
			return "c_float";
		    }
		else if(cpp_type.equals("double"))
		    {
			return "double";
		    }
		else if(cpp_type.equals("long double"))
		    {
			return "long_double";
		    }
		else if(cpp_type.equals("bool"))
		    {
			return "Boolean";
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return cpp_type;
    }

    private static String capitalizeAdaTok(String tok)
    {
	try
	    {
		tok = tok.substring(0,1).toUpperCase() + tok.substring(1);
		int u_index = tok.indexOf('_');
		while(u_index >= 0 && u_index < tok.length() -1)
		    {
			tok = tok.substring(0,u_index+1)+tok.substring(u_index+1,u_index+2).toUpperCase()+tok.substring(u_index+2);
			u_index = tok.indexOf('_',u_index+1);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return tok;
    }

    private static String ConvertCppTokToAdaTok(String cpp_tok)
    {
	try
	    {
		SplitInfoToken spi = new SplitInfoToken(cpp_tok);
		boolean is_class  = cgc.CheckForCppClass(spi.cpp_type);
		boolean is_enum  = cgc.CheckForCppEnum(spi.cpp_type);
		String cpp_type=spi.cpp_type;
		String ada_type = ConvertCppTypeToAdaType(cpp_type);
		String ndla_comment = "";
		if(spi.ndla)
		    {
			ndla_comment = " -- "+SplitInfoToken.ndla_string+" -- ";
		    }
		if(spi.num_dims > 0)
		    {
			if(is_class || ada_type.equals("char") || is_enum)
			    {				
				return spi.variable_name+" : " + capitalizeAdaTok(ada_type)+"_Array(1.."+spi.dims_mult+");" +ndla_comment;
			    }
			else
			    {
				return spi.variable_name+" : Cms." +capitalizeAdaTok(ada_type)+"_Array(1.."+spi.dims_mult+");" +ndla_comment;
			    }				
		    }
		return spi.variable_name +" : " + ada_type+";"+ndla_comment;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }

    private static String ConvertCppTokToAdaUpdateCall(String cpp_tok, 
						       String prefix,
						       StringTokenizer st)
    {
	try
	    {
		SplitInfoToken spi = new SplitInfoToken(cpp_tok);
		String rv="";
		if(null != st && spi.variable_name.endsWith("_length") && spi.cpp_type.equals("int") && st.hasMoreTokens())
		    {
			try
			    {
				String nextTok = st.nextToken();
				SplitInfoToken nextSpi = new SplitInfoToken(nextTok);
				if(!nextSpi.ndla)
				    {
					rv = ConvertCppTokToAdaUpdateCall(cpp_tok,prefix,null);
				    }
				spi = nextSpi;
				cpp_tok = nextTok;
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }
		String cpp_type=spi.cpp_type;
		String ada_type = capitalizeAdaTok(ConvertCppTypeToAdaType(cpp_type));
		boolean is_class  = cgc.CheckForCppClass(spi.cpp_type);
		boolean is_enum  = cgc.CheckForCppEnum(spi.cpp_type);
		if(spi.ndla)
		    {
			rv += prefix+"Cms.Update_Dla_Length(Cms_Ptr,\""+spi.variable_name+"_length\", Msg."+spi.variable_name+"_Length);\n";
		    }
		if(spi.num_dims > 0)
		    {
			if(is_class)
			    {
				if(!spi.ndla)
				    {
					rv += prefix+"for "+spi.variable_name+"_Index in Msg."+spi.variable_name+"'Range\n";;
					rv += prefix+"loop\n";
					rv += prefix+"\tCms.Begin_Struct_Array_Elem(Cms_Ptr,\""+spi.variable_name+"\",int("+spi.variable_name+"_Index - Msg."+spi.variable_name+"'First));\n";
					rv += prefix+"\tUpdate_Internal_"+ada_type+"(Cms_Ptr, Msg."+spi.variable_name+"("+spi.variable_name+"_Index));\n";
					rv += prefix+"\tCms.End_Struct_Array_Elem(Cms_Ptr,\""+spi.variable_name+"\",int("+spi.variable_name+"_Index - Msg."+spi.variable_name+"'First));\n";
					rv += prefix+"end loop;\n";
					return rv;
				    }
				else
				    {
					rv += prefix+"Cms.Begin_Struct_Dynamic_Array(Cms_Ptr,\""+spi.variable_name+"\",Msg."+spi.variable_name+"_Length,Msg."+spi.variable_name+"'Length);\n";
					rv += prefix+"for "+spi.variable_name+"_Index in Msg."+spi.variable_name+"'Range\n";;
					rv += prefix+"loop\n";
					rv += prefix+"\tif int("+spi.variable_name+"_Index - Msg."+spi.variable_name+"'First) >= Msg."+spi.variable_name+"_Length then\n";
					rv += prefix+"\t\texit;\n";
					rv += prefix+"\tend if;\n";
					rv += prefix+"\tCms.Begin_Struct_Array_Elem(Cms_Ptr,\""+spi.variable_name+"\",int("+spi.variable_name+"_Index - Msg."+spi.variable_name+"'First));\n";
					rv += prefix+"\tUpdate_Internal_"+ada_type+"(Cms_Ptr, Msg."+spi.variable_name+"("+spi.variable_name+"_Index));\n";
					rv += prefix+"\tCms.End_Struct_Array_Elem(Cms_Ptr,\""+spi.variable_name+"\",int("+spi.variable_name+"_Index - Msg."+spi.variable_name+"'First));\n";
					rv += prefix+"end loop;\n";
					rv += prefix+"Cms.End_Struct_Dynamic_Array(Cms_Ptr,\""+spi.variable_name+"\",Msg."+spi.variable_name+"_Length,Msg."+spi.variable_name+"'Length);\n";
					return rv;
				    }

			    }
			else if(spi.ndla && !is_enum)
			    {
				rv +=prefix+"Cms.Update_"+ada_type+"_Dla(Cms_Ptr, \""+spi.variable_name+"\", Msg."+spi.variable_name+",Msg."+spi.variable_name+"_length,"+spi.dims_mult+");\n";
				return rv;
			    }
			else if(!spi.ndla && is_enum)
			    {
				rv += prefix+"Cms.Begin_Enumeration_Array(Cms_Ptr,\""+spi.variable_name+"\", Enum_"+ada_type+"_Info,"+spi.dims_mult+");\n";
				rv += prefix+"for "+spi.variable_name+"_Index in Msg."+spi.variable_name+"'Range\n";
				rv += prefix+"loop\n";
				rv += prefix+"\tMsg."+spi.variable_name+"("+spi.variable_name+"_Index) := Int_To_Enum_"+ada_type+"(\n";
				rv += prefix+"\t\tCms.Update_Enumeration_Array_Elem(Cms_Ptr, \""+spi.variable_name+"\", Enum_"+ada_type+"_To_Int(Msg."+spi.variable_name+"("+spi.variable_name+"_Index)),int("+spi.variable_name+"_Index-Msg."+spi.variable_name+"'First)));\n";
				rv += prefix+"end loop;\n";
				rv += prefix+"Cms.End_Enumeration_Array(Cms_Ptr,\""+spi.variable_name+"\", Enum_"+ada_type+"_Info,"+spi.dims_mult+");\n";
				return rv;
			    }
			else if(spi.ndla && is_enum)
			    {
				rv += prefix+"Cms.Begin_Enumeration_Dla(Cms_Ptr,\""+spi.variable_name+"\", Enum_"+ada_type+"_Info, Msg."+spi.variable_name+"_Length,"+spi.dims_mult+");\n";
				rv += prefix+"for "+spi.variable_name+"_Index in Msg."+spi.variable_name+"'Range\n";
				rv += prefix+"loop\n";
				rv += prefix+"\tif int("+spi.variable_name+"_Index-Msg."+spi.variable_name+"'First) >= Msg."+spi.variable_name+"_Length then\n";
				rv += prefix+"\t\texit;\n";
				rv += prefix+"\tend if;\n";
				rv += prefix+"\tMsg."+spi.variable_name+"("+spi.variable_name+"_Index) := Int_To_Enum_"+ada_type+"(\n";
				rv += prefix+"\t\tCms.Update_Enumeration_Array_Elem(Cms_Ptr, \""+spi.variable_name+"\", Enum_"+ada_type+"_To_Int(Msg."+spi.variable_name+"("+spi.variable_name+"_Index)),int("+spi.variable_name+"_Index-Msg."+spi.variable_name+"'First)));\n";
				rv += prefix+"end loop;\n";
				rv += prefix+"Cms.End_Enumeration_Dla(Cms_Ptr,\""+spi.variable_name+"\", Enum_"+ada_type+"_Info, Msg."+spi.variable_name+"_Length,"+spi.dims_mult+");\n";

				return rv;
			    }
			else
			    {
				rv += prefix+"Cms.Update_"+ada_type+"_Array(Cms_Ptr, \""+spi.variable_name+"\", Msg."+spi.variable_name+","+spi.dims_mult+");\n";
				return rv;
			    }
		    }
		else if(is_class)
		    {
			rv += prefix+"Cms.Begin_Class_Var(Cms_Ptr,\""+spi.variable_name+"\");\n";
			rv += prefix+"Update_Internal_"+cpp_type+"(Cms_Ptr,Msg."+spi.variable_name+");\n";
			rv += prefix+"Cms.End_Class_Var(Cms_Ptr,\""+spi.variable_name+"\");\n";
			return rv;
		    }
		else if(is_enum)
		    {
			rv += prefix+"Msg."+spi.variable_name+" := Int_To_Enum_"+ada_type+"(\n";
			rv += prefix+"\tCms.Update_Enumeration(Cms_Ptr, \""+spi.variable_name+"\", Enum_"+ada_type+"_To_Int(Msg."+spi.variable_name+"), Enum_"+ada_type+"_Info));\n";
			return rv;
		    }
		rv += prefix+"Cms.Update_"+ada_type+"(Cms_Ptr, \""+spi.variable_name+"\", Msg."+spi.variable_name+");\n";
		return rv;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }

    // @SuppressWarnings("unchecked")
   public static void GenerateAdaBody(String selected_classes[],
			       CodeGenCommonInterface2 _cgc,
			       String currentOutputFileName,
			       Hashtable enumInfoHashTable)
    {
	try
	    {
		cgc = _cgc;
		if(debug_on)
		    {
				System.out.println("CodeGenCommon.GenerateAdaSpeck() called.");
		    }
		if(debug_on)
		    {
			if(null == selected_classes)
			    {
				System.out.println("CodeGenCommon.GenerateAdaSpeck() : selected_classes = null;");
			    }
			else
			    {
				System.out.println("CodeGenCommon.GenerateAdaSpeck() : selected_classes.length = "+ 	selected_classes.length);
			    }
		    }

		if(selected_classes.length < 1)
		    {
			return;
		    }
		StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
		if(debug_on)
		    {
			System.out.println("type_info="+type_info);
			System.out.println("type_info.first_module_used_in="+type_info.first_module_used_in);
		    }
		String this_ada_package = currentOutputFileName;
		int ext_index = this_ada_package.lastIndexOf('.');
		if(ext_index > 0)
		    {
			this_ada_package = this_ada_package.substring(0,ext_index);
		    }
		cgc.WriteOutput("--\n--\tNew Ada Body File starts here.\n--\tThis file should be named "+currentOutputFileName+"\n");
		cgc.WriteOutput("--\tAutomatically generated by NML CodeGen Java Applet.\n");
		//cgc.WriteOutput("--\ton "+((new Date()).toString())+"\n--\n\n");
		cgc.WriteOutput("\n\n");
		cgc.WriteOutput("with Nml_Msg; use Nml_Msg;\n\n");
		cgc.WriteOutput("with Posemath_N_Ada;  use Posemath_N_Ada;\n\n");
		cgc.WriteOutput("with Cms;\n\n");
		if(ModuleInfo.headerFiles.size() > 1)
		    {
			cgc.WriteOutput("-- Include other package files  that contain message definitions we might need.\n");
			for(int i =0; i < ModuleInfo.headerFiles.size(); i++)
			    {
				String header = (String) ModuleInfo.headerFiles.elementAt(i);
				int pindex = header.indexOf('.');
				String headerbase=header;
				if(pindex > 0)
				    {
					headerbase=header.substring(0,pindex);
				    }
				String other_ada_package = headerbase+"_n_ada";
				if(other_ada_package.equalsIgnoreCase(this_ada_package))
				    {
					continue;
				    }
				cgc.WriteOutput("with "+other_ada_package+"; use "+other_ada_package+";\n");
			    }
		    }
		cgc.WriteOutput("\n");
		cgc.WriteOutput("--\tSome standard Ada Packages we always need.\n");
		cgc.WriteOutput("with Unchecked_Deallocation;\n");
		cgc.WriteOutput("with Unchecked_Conversion;\n");
		cgc.WriteOutput("with Interfaces.C; use Interfaces.C;\n");
		cgc.WriteOutput("with Interfaces.C.Strings; use Interfaces.C.Strings;\n");
		cgc.WriteOutput("\n");
		
		cgc.WriteOutput("package body "+this_ada_package+" is\n");

		try
		    {
			cgc.WriteOutput("\n");
			cgc.WriteOutput("\t-- Create some common variables  and functions needed for updating Enumeration types.\n");
			Enumeration enum_info_types = enumInfoHashTable.elements();
			while(enum_info_types.hasMoreElements())
			    {
				EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
				if(null == enum_info)
				    {
					continue;
				    }
				if(null == enum_info.reverse_hashtable)
				    {
					continue;
				    }
				if(enum_info.reverse_hashtable.size() < 1)
				    {
					continue;
				    }
				Enumeration enum_keys = enum_info.reverse_hashtable.keys();
				int max_key_length =1;
				int num_keys=0;
				while(enum_keys.hasMoreElements())
				    {
					String key = (String) enum_keys.nextElement();
					if(key.length() > max_key_length)
					    {
						max_key_length= key.length();
					    }
					num_keys++;
				    }
				enum_keys = enum_info.reverse_hashtable.keys();
				int name_list_length = (max_key_length+1)*(num_keys+1);
				cgc.WriteOutput("\tEnum_"+enum_info.Name+"_Name_List : constant Char_Array(1.."+name_list_length+") := (\n");
				String sout;
				TreeSet sts = new TreeSet();
				while(enum_keys.hasMoreElements())
				    {
					String key = (String) enum_keys.nextElement();
					sts.add(key);
				    }
				Iterator itr = sts.iterator();
				while(itr.hasNext())
				    {
					String s = (String) itr.next();;
					sout = "\t\t";
					for(int i = 0; i <= max_key_length; i++)
					    {
						if(s.length() <= i)
						    {
							sout += "nul,";
						    }
						else
						    {
							sout += "\'"+s.charAt(i)+"\',";
						    }
					    }
					cgc.WriteOutput(sout+"\n");
				    }
				sout = "\t\t";
				for(int i = 0; i <= max_key_length-1; i++)
				    {
					sout += "nul,";
				    }
				sout +="nul";
				cgc.WriteOutput(sout+"\n");
				cgc.WriteOutput("\t\t);\n");
				enum_keys = enum_info.reverse_hashtable.keys();
				cgc.WriteOutput("\tEnum_"+enum_info.Name+"_Int_List : constant Cms.Int_Array(1.."+(num_keys+1)+") := (\n");
				itr = sts.iterator();
				while(itr.hasNext())
				    {
					String key = (String) itr.next();
					int val=-1;
					try
					    {
						Integer Ival= (Integer) enum_info.reverse_hashtable.get(key);
						val = Ival.intValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					cgc.WriteOutput("\t\t"+val+", -- "+key+"\n");
				    }
				cgc.WriteOutput("\t\t-1\n");
				cgc.WriteOutput("\t\t);\n");
				itr = sts.iterator();
				while(itr.hasNext())
				    {
					String key = (String) itr.next();
					cgc.WriteOutput("\tenum_"+enum_info.Name+"_"+key+"_Key_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(\""+key+"\");\n");
				    }
				cgc.WriteOutput("\tfunction Enum_"+enum_info.Name+"_Symbol_Lookup(enum_int : in long) return Interfaces.C.Strings.chars_ptr;\n");
				cgc.WriteOutput("\tpragma Export(C,Enum_"+enum_info.Name+"_Symbol_Lookup,\"ada_"+enum_info.Name+"_"+this_ada_package+"_symbol_lookup\");\n");
				cgc.WriteOutput("\n");
				
				cgc.WriteOutput("\tfunction Enum_"+enum_info.Name+"_Symbol_Lookup(enum_int: in long) return Interfaces.C.Strings.chars_ptr is\n");
				cgc.WriteOutput("\tbegin\n");
				cgc.WriteOutput("\t\tcase enum_int is\n");
				int pos=0;
				itr = sts.iterator();
				while(itr.hasNext())
				    {
					String key = (String) itr.next();
					int val=-1;
					try
					    {
						Integer Ival= (Integer) enum_info.reverse_hashtable.get(key);
						val = Ival.intValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					pos++;
					cgc.WriteOutput("\t\t\twhen "+val+"\t=>\treturn enum_"+enum_info.Name+"_"+key+"_Key_Name; -- "+key+"\n");
				    }
				cgc.WriteOutput("\t\t\twhen others\t=>\treturn Null_Ptr;\n");
				cgc.WriteOutput("\t\tend case;\n");
				cgc.WriteOutput("\tend Enum_"+enum_info.Name+"_Symbol_Lookup;\n");

				cgc.WriteOutput("\n");
				
				cgc.WriteOutput("\tfunction Enum_"+enum_info.Name+"_To_Int(enum_val: in "+enum_info.Name+") return int is\n");
				cgc.WriteOutput("\tbegin\n");
				cgc.WriteOutput("\t\tcase enum_val is\n");
				enum_keys = enum_info.reverse_hashtable.keys();
				while(enum_keys.hasMoreElements())
				    {
					String key = (String) enum_keys.nextElement();
					int val=-1;
					try
					    {
						Integer Ival= (Integer) enum_info.reverse_hashtable.get(key);
						val = Ival.intValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					cgc.WriteOutput("\t\t\twhen "+key+"\t=>\treturn "+val+";\n");
				    }
				cgc.WriteOutput("\t\t\twhen Bad_"+enum_info.Name+"_Value\t=>\treturn -1;\n");
				cgc.WriteOutput("\t\tend case;\n");
				cgc.WriteOutput("\tend Enum_"+enum_info.Name+"_To_Int;\n");

				cgc.WriteOutput("\n");
				
				cgc.WriteOutput("\tfunction Int_To_Enum_"+enum_info.Name+"(enum_int: in int) return "+enum_info.Name+" is\n");
				cgc.WriteOutput("\tbegin\n");
				cgc.WriteOutput("\t\tcase enum_int is\n");
				enum_keys = enum_info.reverse_hashtable.keys();
				while(enum_keys.hasMoreElements())
				    {
					String key = (String) enum_keys.nextElement();
					int val=-1;
					try
					    {
						Integer Ival= (Integer) enum_info.reverse_hashtable.get(key);
						val = Ival.intValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					cgc.WriteOutput("\t\t\twhen "+val+"\t=>\treturn "+key+";\n");
				    }
				cgc.WriteOutput("\t\t\twhen others\t=>\treturn Bad_"+enum_info.Name+"_Value;\n");
				cgc.WriteOutput("\t\tend case;\n");
				cgc.WriteOutput("\tend Int_To_Enum_"+enum_info.Name+";\n");
				cgc.WriteOutput("\n");
				cgc.WriteOutput("\tEnum_"+enum_info.Name+"_Info : constant Cms.Cms_Enum_Info_Access := Cms.New_Cms_Enum_Info(\n");
				cgc.WriteOutput("\t\t\t\""+enum_info.Name+"\",\n");
				cgc.WriteOutput("\t\t\tEnum_"+enum_info.Name+"_Name_List,\n");
				cgc.WriteOutput("\t\t\tEnum_"+enum_info.Name+"_Int_List,\n");
				cgc.WriteOutput("\t\t\t"+(max_key_length+1)+",\n");
				cgc.WriteOutput("\t\t\t"+(num_keys+1)+",\n");
				cgc.WriteOutput("\t\t\tEnum_"+enum_info.Name+"_Symbol_Lookup'Access);\n");
				cgc.WriteOutput("\n");			
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }


		cgc.WriteOutput("\n\t-- Every NMLmsg type needs an update and an initialize function.\n");
		for(int i = 0; i < selected_classes.length; i++)
		    {
			type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
			String def1=type_info.PreFinalPassInfo;
			/* StructureTypeInfo ti = type_info;
			while(ti != null)
			    {
				if(null != ti.PreFinalPassInfo)
				    {
					def1 = ti.PreFinalPassInfo+";"+def1;
				    }
				if(ti.DerivedFrom != null)
				    {
					ti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(ti.DerivedFrom);
				    }
				else
				    {
					ti=null;
				    }
				    } */
			cgc.WriteOutput("\n");
			if(type_info.Id > 0)
			    {
				cgc.WriteOutput("\tprocedure Initialize(Msg : in out "+type_info.getName()+") is\n");
				cgc.WriteOutput("\tbegin\n");
				cgc.WriteOutput("\t\tMsg.NmlType := "+type_info.type_id_string+";\n");
				cgc.WriteOutput("\t\tMsg.Size := "+type_info.getName()+"'Size/8;\n");
				
				cgc.WriteOutput("\tend Initialize;\n");
				
				cgc.WriteOutput("\n");
			    }				
			cgc.WriteOutput("\tprocedure Update_"+type_info.getName()+"(Cms_Ptr : in Cms.Cms_Access; Msg : in "+type_info.getName()+"_Access) is\n");
			cgc.WriteOutput("\tbegin\n");
			cgc.WriteOutput("\t\tCms.Begin_Class(Cms_Ptr,\""+type_info.getName()+"\",\"\");\n");
			if(type_info.DerivedFrom != null && 
			   !type_info.DerivedFrom.equals("NMLmsg"))
			    {
				cgc.WriteOutput("\t\tCms.Begin_Base_Class(Cms_Ptr,\""+type_info.DerivedFrom+"\");\n");
				cgc.WriteOutput("\t\tUpdate_Internal_"+type_info.DerivedFrom+"(Cms_Ptr, "+type_info.DerivedFrom+"(Msg.all));\n");
				cgc.WriteOutput("\t\tCms.End_Base_Class(Cms_Ptr,\""+type_info.DerivedFrom+"\");\n");
			    }
			if(type_info.Id > 0)
			    {
				cgc.WriteOutput("\t\tMsg.NmlType := "+type_info.type_id_string+";\n");
				cgc.WriteOutput("\t\tMsg.Size := "+type_info.getName()+"'Size/8;\n");
			    }
			StringTokenizer st = new StringTokenizer(def1,";");
			while(st.hasMoreTokens())
			    {
				String tok = st.nextToken();
				String ada_tok = ConvertCppTokToAdaUpdateCall(tok,"\t\t",st);
				if(null != ada_tok)
				    {
					cgc.WriteOutput(ada_tok);
				    }
			    }
			cgc.WriteOutput("\t\tCms.End_Class(Cms_Ptr,\""+type_info.getName()+"\",\"\");\n");
			cgc.WriteOutput("\tend Update_"+type_info.getName()+";\n");
			cgc.WriteOutput("\n");

			cgc.WriteOutput("\tprocedure Update_Internal_"+type_info.getName()+"(Cms_Ptr : in Cms.Cms_Access; Msg : in out "+type_info.getName()+") is\n");
			cgc.WriteOutput("\tbegin\n");
			cgc.WriteOutput("\t\tCms.Begin_Class(Cms_Ptr,\""+type_info.getName()+"\",\"\");\n");
			if(type_info.DerivedFrom != null &&
			   !type_info.DerivedFrom.equals("NMLmsg"))
			    {
				cgc.WriteOutput("\t\tCms.Begin_Base_Class(Cms_Ptr,\""+type_info.DerivedFrom+"\");\n");
				cgc.WriteOutput("\t\tUpdate_Internal_"+type_info.DerivedFrom+"(Cms_Ptr, "+type_info.DerivedFrom+"(Msg));\n");
				cgc.WriteOutput("\t\tCms.End_Base_Class(Cms_Ptr,\""+type_info.DerivedFrom+"\");\n");
			    }
			if(type_info.Id > 0)
			    {
				cgc.WriteOutput("\t\tMsg.NmlType := "+type_info.type_id_string+";\n");
				cgc.WriteOutput("\t\tMsg.Size := "+type_info.getName()+"'Size/8;\n");
			    }
			st = new StringTokenizer(def1,";");
			while(st.hasMoreTokens())
			    {
				String tok = st.nextToken();
				String ada_tok = ConvertCppTokToAdaUpdateCall(tok,"\t\t",st);
				if(null != ada_tok)
				    {
					cgc.WriteOutput(ada_tok);
				    }
			    }
			cgc.WriteOutput("\t\tCms.End_Class(Cms_Ptr,\""+type_info.getName()+"\",\"\");\n");
			cgc.WriteOutput("\tend Update_Internal_"+type_info.getName()+";\n");

			cgc.WriteOutput("\n");
		    }
		cgc.WriteOutput("\n");		

		int longest_name_length=0;
		int number_of_names=0;
		TreeSet sts = new TreeSet();
		for(int i = 0; i < selected_classes.length; i++)
		    {
			StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
			if(null == typeInfo)
			    {
				continue;
			    }
			if(typeInfo.Id <= 0 || typeInfo.DerivedFrom == null)
			    {
				continue;
			    }
			number_of_names++;
			if(selected_classes[i].length() > longest_name_length)
			    {
				longest_name_length =selected_classes[i].length();
			    }
			sts.add(selected_classes[i]);
		    }

		cgc.WriteOutput("\n");
		int char_array_length = (number_of_names+1)*(longest_name_length+1);

		Iterator itr = sts.iterator();
		boolean empty_format=true;
		if(itr.hasNext())
		{
		    empty_format=false;
		    cgc.WriteOutput("\tNameList : constant Char_Array(1.."+char_array_length+") := (\n");
		    String sout = "\t\t";
		    while(itr.hasNext())
			{
			    String s = (String) itr.next();
			    sout = "\t\t";
			    for(int i = 0; i <= longest_name_length; i++)
				{
				    if(s.length() <= i)
					{
					    sout += "nul,";
					}
				    else
					{
					    sout += "\'"+s.charAt(i)+"\',";
					}
				}
			    sout+="\n";
			    cgc.WriteOutput(sout);
			}
		    sout = "\t\t";
		    for(int i = 0; i <= longest_name_length-1; i++)
			{
			    sout += "nul,";
			}
		    sout += "nul\n";
		    cgc.WriteOutput(sout);
		    cgc.WriteOutput("\t\t);\n");
		    cgc.WriteOutput("\n");

		    cgc.WriteOutput("\tIdList : constant Cms.Long_Array(1.."+(number_of_names+1)+") := (\n");
 
		    itr = sts.iterator();
		    int pos = 0;
		    while(itr.hasNext())
			{
			    String s = (String) itr.next();
			    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(s);
			    if(null != typeInfo)
				{
				    cgc.WriteOutput("\t\t"+typeInfo.type_id_string+", -- "+typeInfo.Id+", "+pos+"\n");
				}
			    pos++;
			}
		    cgc.WriteOutput("\t\t-1);\n");

		    cgc.WriteOutput("\n");

		    cgc.WriteOutput("\tSizeList : constant Cms.Size_T_Array(1.."+(number_of_names+1)+") := (\n");
 
		    itr = sts.iterator();
		    pos = 0;
		    while(itr.hasNext())
			{
			    String s = (String) itr.next();
			    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(s);
			    if(null != typeInfo)
				{
				    cgc.WriteOutput("\t\t"+typeInfo.getName()+"\'Size/8,\n");
				}
			    pos++;
			}
		    cgc.WriteOutput("\t\t0);\n");

		    itr = sts.iterator();
		    while(itr.hasNext())
			{
			    String s = (String) itr.next();
			    cgc.WriteOutput("\tSymbol_Lookup_"+s+"_Name : constant Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(\""+s+"\");\n");
			}
		    cgc.WriteOutput("\n");
		    cgc.WriteOutput("\tfunction Symbol_Lookup(Nml_Type : in long) return Interfaces.C.Strings.Chars_Ptr;\n");
		    cgc.WriteOutput("\tpragma Export(C,Symbol_Lookup,\"ada_"+this_ada_package+"_symbol_lookup\");\n");
		    cgc.WriteOutput("\n");

		    cgc.WriteOutput("\tfunction Symbol_Lookup(Nml_Type : in long) return Interfaces.C.Strings.Chars_Ptr is\n");
		    cgc.WriteOutput("\tbegin\n");
		    cgc.WriteOutput("\t\tcase Nml_Type is\n");
		    itr = sts.iterator();
		    pos = 0;
		    while(itr.hasNext())
			{
			    String s = (String) itr.next();
			    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(s);
			    if(null != typeInfo)
				{
				    cgc.WriteOutput("\t\t\t when "+typeInfo.type_id_string+"\t=>\treturn Symbol_Lookup_"+s+"_Name;\n");
				}
			    pos++;
			}
		    cgc.WriteOutput("\t\t\t when others\t=>\treturn Null_Ptr;\n");
		    cgc.WriteOutput("\t\tend case;\n");
		    cgc.WriteOutput("\tend Symbol_Lookup;\n");
		}
		cgc.WriteOutput("\n");
		
		cgc.WriteOutput("\tfunction Format(Nml_Type : in long;\n");
		cgc.WriteOutput("\t\t\tMsg : in NmlMsg_Access;\n");
		cgc.WriteOutput("\t\t\tCms_Ptr : in Cms.Cms_Access)\n");
		cgc.WriteOutput("\t\t\t\treturn int is\n");
		if(empty_format)
		    {
			cgc.WriteOutput("\tbegin\n");
			cgc.WriteOutput("\t\treturn 1;\n");
			cgc.WriteOutput("\tend Format;\n");
		    }
		else
		    {
			cgc.WriteOutput("\t\tChecked_Nml_Type : long;\n");		
			cgc.WriteOutput("\n");
			cgc.WriteOutput("\tbegin\n");
		
			cgc.WriteOutput("\t\tChecked_Nml_Type := Cms.Check_Type_Info(Cms_Ptr,Nml_Type,\n");
			cgc.WriteOutput("\t\t\tNmlMsg_Access_To_Limited_Controlled_Access(Msg),\n");
			cgc.WriteOutput("\t\t\t\""+this_ada_package+"\",\n");
			cgc.WriteOutput("\t\t\tSymbol_Lookup'Access,\n");
			cgc.WriteOutput("\t\t\tNameList,IdList,SizeList,"+(number_of_names+1)+","+(longest_name_length+1)+");\n");

			cgc.WriteOutput("\n");		
			cgc.WriteOutput("\t\tif Msg = Null then\n");
			cgc.WriteOutput("\t\t\treturn 0;\n");
			cgc.WriteOutput("\t\tend if;\n");
			cgc.WriteOutput("\n");

			cgc.WriteOutput("\t\tcase Checked_Nml_Type is\n");
			for(int i = 0; i < selected_classes.length; i++)
			    {
				StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
				if(null == typeInfo)
				    {
					continue;
				    }
				if(typeInfo.Id <= 0 || typeInfo.DerivedFrom == null)
				    {
					continue;
				    }
				cgc.WriteOutput("\t\t\twhen "+typeInfo.type_id_string+"\t=>\tUpdate_"+typeInfo.getName()+"(Cms_Ptr, NmlMsg_to_"+typeInfo.getName()+"(Msg));\n");
			    }
			cgc.WriteOutput("\t\t\twhen others\t=>\treturn 0;\n");
			cgc.WriteOutput("\t\tend case;\n");
			cgc.WriteOutput("\t\treturn 1;\n");
			cgc.WriteOutput("\tend Format;\n");
		    }
		cgc.WriteOutput("\n");
		cgc.WriteOutput("end "+this_ada_package+";\n");

		cgc.WriteOutput("\n");
		cgc.WriteOutput("-- End of Ada Body file  "+currentOutputFileName+"\n");
		cgc.WriteOutput("\n");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

   public static void GenerateAdaSpec(String selected_classes[],
			       CodeGenCommonInterface2 _cgc,
			       String currentOutputFileName,
			       Hashtable enumInfoHashTable)
    {
	try
	    {
		cgc = _cgc;
		if(debug_on)
		    {

			System.out.println("CodeGenCommon.GenerateAdaSpeck() called.");
		    }
		if(debug_on)
		    {
			if(null == selected_classes)
			    {
				System.out.println("CodeGenCommon.GenerateAdaSpeck() : selected_classes = null;");
			    }
			else
			    {
				System.out.println("CodeGenCommon.GenerateAdaSpeck() : selected_classes.length = "+ 	selected_classes.length);
			    }
		    }

		if(selected_classes.length < 1)
		    {
			return;
		    }
		StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
		if(debug_on)
		    {
			System.out.println("type_info="+type_info);
			System.out.println("type_info.first_module_used_in="+type_info.first_module_used_in);
		    }
		cgc.WriteOutput("--\n--\tNew Ada Spec File starts here.\n--\tThis file should be named "+currentOutputFileName+"\n");
		cgc.WriteOutput("--\tAutomatically generated by NML CodeGen Java Applet.\n");
		cgc.WriteOutput("with Nml_Msg; use Nml_Msg;\n");
                cgc.WriteOutput("with Cms;\n\n");
		cgc.WriteOutput("with Posemath_N_Ada;  use Posemath_N_Ada;\n\n");

		String this_ada_package = currentOutputFileName;
		int ext_index = this_ada_package.lastIndexOf('.');
		if(ext_index > 0)
		    {
			this_ada_package = this_ada_package.substring(0,ext_index);
		    }
		if(ModuleInfo.headerFiles.size() > 1)
		    {
			cgc.WriteOutput("-- Include other package files  that contain message definitions we might need.\n");
			for(int i =0; i < ModuleInfo.headerFiles.size(); i++)
			    {
				String header = (String) ModuleInfo.headerFiles.elementAt(i);
				int pindex = header.indexOf('.');
				String headerbase=header;
				if(pindex > 0)
				    {
					headerbase=header.substring(0,pindex);
				    }
				String other_ada_package = headerbase+"_n_ada";
				if(other_ada_package.equalsIgnoreCase(this_ada_package))
				    {
					continue;
				    }
				cgc.WriteOutput("with "+other_ada_package+"; use "+other_ada_package+";\n");
			    }
		    }
		cgc.WriteOutput("\n");
		cgc.WriteOutput("--\tSome standard Ada Packages we always need.\n");
		cgc.WriteOutput("with Unchecked_Deallocation;\n");
		cgc.WriteOutput("with Unchecked_Conversion;\n");
		cgc.WriteOutput("with Interfaces.C; use Interfaces.C;\n");
		cgc.WriteOutput("\n");

		cgc.WriteOutput("package "+this_ada_package+" is\n");
		cgc.WriteOutput("\n");
		
		try
		    {
			cgc.WriteOutput("\t-- Create Ada versions of the Enumeration types.\n");
			Enumeration enum_info_types = enumInfoHashTable.elements();
			while(enum_info_types.hasMoreElements())
			    {
				EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
				if(null == enum_info)
				    {
					continue;
				    }
				if(null == enum_info.reverse_hashtable)
				    {
					continue;
				    }
				if(enum_info.reverse_hashtable.size() < 1)
				    {
					continue;
				    }
				if(enum_info.Name.equals("RCS_STATUS"))
				    {
					continue;
				    }
				if(enum_info.Name.equals("RCS_ADMIN_STATE"))
				    {
					continue;
				    }
				cgc.WriteOutput("\ttype "+enum_info.Name+" is (\n");
				cgc.WriteOutput("\t\tBad_"+enum_info.Name+"_Value,\n");
				Enumeration enum_keys = enum_info.reverse_hashtable.keys();
				while(enum_keys.hasMoreElements())
				    {
					String key = (String) enum_keys.nextElement();
					int val=-1;
					try
					    {
						Integer Ival= (Integer) enum_info.reverse_hashtable.get(key);
						val = Ival.intValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					if(enum_keys.hasMoreElements())
					    {
						cgc.WriteOutput("\t\t"+key+", -- "+val+"\n");
					    }
					else
					    {
						cgc.WriteOutput("\t\t"+key+" --"+val+"\n");
					    }
				    }
				cgc.WriteOutput("\t\t);\n");
				cgc.WriteOutput("\ttype "+enum_info.Name+"_Array is array(Integer range <>) of "+enum_info.Name+";\n");
			    }
			cgc.WriteOutput("\n");
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }

		cgc.WriteOutput("\tfunction Format(Nml_Type : in long;\n");
		cgc.WriteOutput("\t\t\tMsg : in NmlMsg_Access;\n");
		cgc.WriteOutput("\t\t\tCms_Ptr : in Cms.Cms_Access)\n");
		cgc.WriteOutput("\t\t\t\treturn int;\n");
		
		cgc.WriteOutput("\n");
		cgc.WriteOutput("\tpragma Export(C,Format,\"ada_"+this_ada_package+"_format\");\n");
		cgc.WriteOutput("\n");
		
		cgc.WriteOutput("\n\t-- Redefine the C++ NML message classes as Ada Records.\n");
		for(int i = 0; i < selected_classes.length; i++)
		    {
			type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
			String def1= type_info.PreFinalPassInfo;
			/*
			StructureTypeInfo ti = type_info;
			while(ti != null)
			    {
				if(null != ti.PreFinalPassInfo)
				    {
					def1 = ti.PreFinalPassInfo+";"+def1;
				    }
				if(ti.DerivedFrom != null)
				    {
					ti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(ti.DerivedFrom);
				    }
				else
				    {
					ti=null;
				    }
			    }
			*/
			cgc.WriteOutput("\n");
			if(type_info.Id > 0)
			    {
				cgc.WriteOutput("\t"+type_info.type_id_string+" : constant := "+type_info.Id+";\n");
				cgc.WriteOutput("\n");
			    }
			if(type_info.DerivedFrom != null)
			    {
				cgc.WriteOutput("\ttype "+type_info.getName()+" is new "+type_info.DerivedFrom+" with ");
			    }
			else
			    {
				cgc.WriteOutput("\ttype "+type_info.getName()+" is ");
			    }				
			cgc.WriteOutput("\n\t\trecord\n");
			StringTokenizer st = new StringTokenizer(def1,";");
			if(st.hasMoreTokens())
			    {
				while(st.hasMoreTokens())
				    {
					String tok = st.nextToken();
					String ada_tok = ConvertCppTokToAdaTok(tok);
					if(null != ada_tok)
					    {
						cgc.WriteOutput("\t\t\t"+ada_tok+"\n");
					    }
				    }
			    }
			else
			    {
				cgc.WriteOutput("\t\t\tnull;\n");
			    }
			cgc.WriteOutput("\t\tend record;\n");
			cgc.WriteOutput("\n");
			cgc.WriteOutput("\ttype "+type_info.getName()+"_Access is access all "+type_info.getName()+";\n");

			if(type_info.Id > 0)
			    {
				cgc.WriteOutput("\tprocedure Initialize(Msg : in out "+type_info.getName()+");\n");
				cgc.WriteOutput("\tfunction NmlMsg_to_"+type_info.getName()+" is new Unchecked_Conversion(NmlMsg_Access,"+type_info.getName()+"_Access);\n");
			    }
			cgc.WriteOutput("\tprocedure Update_Internal_"+type_info.getName()+"(Cms_Ptr : in Cms.Cms_Access; Msg : in out "+type_info.getName()+");\n");
			cgc.WriteOutput("\tprocedure Free is new Unchecked_Deallocation("+type_info.getName()+","+type_info.getName()+"_Access);\n");
			cgc.WriteOutput("\ttype "+type_info.getName()+"_Array is array(Integer range <>) of "+type_info.getName()+";\n");
		    }

		cgc.WriteOutput("\n");		

		int longest_name_length=0;
		int number_of_names=0;
		for(int i = 0; i < selected_classes.length; i++)
		    {
			StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
			if(null == typeInfo)
			    {
				continue;
			    }
			if(typeInfo.Id <= 0 || typeInfo.DerivedFrom == null)
			    {
				continue;
			    }
			number_of_names++;
			if(selected_classes[i].length() > longest_name_length)
			    {
				longest_name_length =selected_classes[i].length();
			    }
		    }

		cgc.WriteOutput("\n");

		cgc.WriteOutput("end "+this_ada_package+";\n");

		cgc.WriteOutput("\n");
		cgc.WriteOutput("-- End of Ada spec file  "+currentOutputFileName+"\n");
		cgc.WriteOutput("\n");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


}
