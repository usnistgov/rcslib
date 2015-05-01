/*
 * WatchVar.java
 *
 * Created on December 26, 2006, 5:49 PM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */

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

package diagapplet.utils;

import diagapplet.CodeGen.EnumTypeInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import java.util.Vector;

/**
 * Class that encapsulates the information related to a single variable from
 * the table in WatchJPanel.
 * @see WatchTableModel
 * @author shackle
 */
final class WatchVar
{
    private String varname;
    private int var_number;
    private int struct_level;
    private boolean open=false;
    private boolean new_struct=false;
    private boolean to_string_set=false;
    private String prefix_string=null;
    private WatchVar parent=null;
    private Vector<WatchVar> children=null;
    private boolean ndla=false;
    private StructureTypeInfo sti=null;
    private boolean expanded=false;
    private String to_string=null;
    private String plotName=null;
    private boolean is_array=false;
    
    public boolean get_is_array()
    {
	return is_array;
    }
    
    public void set_is_array(boolean _is_array)
    {
	is_array = _is_array;
    }
    
    
    /** Creates a new instance of WatchVar */
    public WatchVar(String _varname)
    {
	set_varname(_varname);
    }
    
    public String get_plotName()
    {
	return plotName;
    }
    
    public void set_plotName(String _plotName)
    {
	plotName = _plotName;
    }
    
    public boolean get_expanded()
    {
	return expanded;
    }
    
    public void set_expanded(boolean _expanded)
    {
	expanded = _expanded;
    }
    public int get_var_count_size()
    {
	if(null != sti)
	{
	    return sti.get_var_count_size();
	}
	return 1;
    }
    public void set_sti(StructureTypeInfo _sti)
    {
	sti = _sti;
	set_new_struct(true);
    }
    
    public StructureTypeInfo get_sti()
    {
	return sti;
    }
    
    public void set_ndla(boolean _ndla)
    {
	ndla=_ndla;
    }
    
    public boolean get_ndla()
    {
	return ndla;
    }
    
    public WatchVar get_parent()
    {
	return null;
    }
    
    public void set_parent(WatchVar _parent)
    {
	//parent = _parent;
    }
    
    public String get_varname()
    {
	return varname;
    }
    
    public int get_num_children()
    {
	if(children == null)
	{
	    return 0;
	}
	return children.size();
    }
    
    public WatchVar get_child(int _i)
    {
	if(null == children)
	{
	    return null;
	}
	return children.elementAt(_i);
    }
    
    public void add_child(WatchVar child)
    {
	if(null == children)
	{
	    children = new Vector<WatchVar>();
	}
	children.add(child);
    }
    
    
    public void set_varname(String _varname)
    {
	varname=_varname;
	to_string_set=false;
    }
    
    public int get_var_number()
    {
	return var_number;
    }
    
    public void set_var_number(int _var_number)
    {
	var_number = _var_number;
    }
    
    public int get_struct_level()
    {
	return struct_level;
    }
    
    public void set_struct_level(int _struct_level)
    {
	struct_level = _struct_level;
	to_string_set=false;
    }
    
    
    public boolean get_new_struct()
    {
	return new_struct;
    }
    
    public void set_new_struct(boolean _new_struct)
    {
	new_struct = _new_struct;
	to_string_set=false;
    }
    
    public boolean get_open()
    {
	return open;
    }
    
    public void set_open(boolean _open)
    {
	open = _open;
	to_string_set=false;
    }
    
    private String prefix()
    {
	String p="";
	for(int i=0; i  <  struct_level; i++)
	{
	    p += " ";
	}
	if(new_struct)
	{
	    if(open)
	    {
		p += "[-]";
	    }
	    else
	    {
		p += "[+]";
	    }
	}
	else if(struct_level > 0)
	{
	    for(int i=0; i  <  struct_level; i++)
	    {
		p += "-";
	    }
	}
	prefix_string = p;
	return prefix_string;
    }
    
    private String varstring()
    {
	String vstring = varname;
	while(vstring.startsWith("NML_DYNAMIC_LENGTH_ARRAY ") ||
		vstring.startsWith("class ") ||
		vstring.startsWith("struct "))
	{
	    if(vstring.startsWith("NML_DYNAMIC_LENGTH_ARRAY "))
	    {
		vstring = vstring.substring(25);
		continue;
	    }
	    else if(vstring.startsWith("class "))
	    {
		vstring = vstring.substring(6);
		continue;
	    }
	    else if(vstring.startsWith("struct "))
	    {
		vstring = vstring.substring(7);
		continue;
	    }
	}
	return vstring;
    }
    
    public String toString()
    {
	if(to_string_set)
	{
	    return to_string;
	}
	to_string = (prefix()+varstring());
	to_string_set=true;
	return to_string;
    }
    
    /**
     * Holds value of property enum_info.
     */
    private EnumTypeInfo enum_info;
    
    /**
     * Getter for property enum_info.
     * @return Value of property enum_info.
     */
    public EnumTypeInfo getEnum_info()
    {
	return this.enum_info;
    }
    
    /**
     * Setter for property enum_info.
     * @param enum_info New value of property enum_info.
     */
    public void setEnum_info(EnumTypeInfo enum_info)
    {
	this.enum_info = enum_info;
    }
    
    /**
     * Holds value of property enumeration.
     */
    private boolean enumeration;
    
    /**
     * Getter for property enumeration.
     * @return Value of property enumeration.
     */
    public boolean isEnumeration()
    {
	return this.enumeration;
    }
    
    /**
     * Setter for property enumeration.
     * @param enumeration New value of property enumeration.
     */
    public void setEnumeration(boolean enumeration)
    {
	this.enumeration = enumeration;
    }
    
}
