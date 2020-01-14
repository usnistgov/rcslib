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
/*
 * WatchTableModel.java
 *
 * Created on December 26, 2006, 5:32 PM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */
package diagapplet.utils;

import diagapplet.CodeGen.EnumTypeInfo;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.swing.table.AbstractTableModel;

/**
 * Model used for the table in WatchJPanel
 * @author shackle
 */
class WatchTableModel
		extends AbstractTableModel {

	private static final long serialVersionUID = 2613935L;
	private Vector<WatchVar> watchVarVector = null;
	private Vector<WatchVar> watchVarVectorByGen = null;
	private String val_array[] = null;
	private WatchVar watchVarRoot = null;
//    private String last_struct=null;
	private int var_number = 0;
	private boolean editable = false;
	private String default_string;
//    private static long max_var_num=-1;
	private StructureTypeInfo sti = null;
	private Hashtable structInfoByNameHashtable = null;

	public Hashtable get_structInfoByNameHashtable() {
		return structInfoByNameHashtable;
	}

	/** Creates a new instance of WatchTableModel */
	public WatchTableModel() {
	}

	public StructureTypeInfo getStructureTypeInfo() {
		return sti;
	}

	public void set_editable(boolean _editable) {
		editable = _editable;
	}

	public boolean get_editable() {
		return editable;
	}

	public String getTypeInfoName() {
		if (null == sti) {
			return null;
		}
		return sti.getName();
	}

	public void ExpandAll() {
		for (int i = 0; i < watchVarVector.size(); i++) {
			WatchVar wv = watchVarVector.elementAt(i);
			if (null == wv) {
				continue;
			}
			if (!wv.get_new_struct()) {
				continue;
			}
			boolean was_open = wv.get_open();
			wv.set_open(true);
			if (wv.get_open() && !wv.get_expanded() && wv.get_new_struct()) {
				ExpandWatchVar(wv);
			}
			ResetWatchVarVector();
			this.fireTableCellUpdated(i, 0);
			if (wv.get_open()) {
				this.fireTableRowsInserted(i + 1, i + 1 + wv.get_num_children());
			} else {
				this.fireTableRowsDeleted(i + 1, i + 1 + wv.get_num_children());
			}
		}
	}

	public void CollapseAll() {
		for (int i = watchVarVector.size() - 1; i >= 0; i--) {
			if (i > watchVarVector.size() - 1) {
				i = watchVarVector.size() - 1;
			}
			WatchVar wv = watchVarVector.elementAt(i);
			if (null == wv) {
				continue;
			}
			if (!wv.get_new_struct()) {
				continue;
			}
			boolean was_open = wv.get_open();
			wv.set_open(false);
//            if (wv.get_open() && !wv.get_expanded() && wv.get_new_struct()) {
//                ExpandWatchVar(wv);
//            }
			ResetWatchVarVector();
			this.fireTableCellUpdated(i, 0);
			if (wv.get_open()) {
				this.fireTableRowsInserted(i + 1, i + 1 + wv.get_num_children());
			} else {
				this.fireTableRowsDeleted(i + 1, i + 1 + wv.get_num_children());
			}
			if (i > watchVarVector.size() - 1) {
				i = watchVarVector.size() - 1;
			}
		}
	}

	public void Clear() {
		if (null != val_array) {
			for (int i = 0; i < val_array.length; i++) {
				val_array[i] = default_string;
			}
			int wv_num = 0;
			for (WatchVar wv : watchVarVector) {
				if (wv.get_var_number() < val_array.length &&
						wv.get_var_number() >= 0 && !wv.get_new_struct()) {
					this.fireTableCellUpdated(wv_num, 1);
					break;
				}
				wv_num++;
			}
		}
	}

	public void setValueForVarNumber(int var_number, double value) {
		if (val_array != null && val_array.length > var_number) {
			val_array[var_number] = Double.toString(value);
			int wv_num = 0;
			for (WatchVar wv : watchVarVector) {
				if (wv.get_var_number() == var_number && !wv.get_new_struct()) {
					this.fireTableCellUpdated(wv_num, 1);
					break;
				}
				wv_num++;
			}
		}
	}

	public String getValueStringForVarNumber(int var_number) {
		if (val_array != null && val_array.length > var_number) {
			return val_array[var_number];
		}
		return null;
	}

	@Override
	public boolean isCellEditable(int row, int column) {
		if (!editable || column != 1 || null == watchVarVector || row > watchVarVector.size()) {
			return false;
		}
		if (!watchVarVector.elementAt(row).get_new_struct()) {
//	    if(!watchVarVector.elementAt(row).isEnumeration())
//	    {
//		val_array[watchVarVector.elementAt(row).get_var_number()] = "";
//	    }
			return true;
		}
		return false;
	}

	public void SetRowValue(int row, String s) {
		try {
			WatchVar wv = watchVarVector.elementAt(row);
			if (!wv.get_new_struct()) {
				val_array[wv.get_var_number()] = s;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public String getDataString() {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < val_array.length; i++) {
			final String sv= val_array[i];
			if (sv != null) {
				sb.append(',');
				sb.append(sv);
			}
		}
		return sb.toString();
	}

	public void SetDefault(String _default_string) {
		default_string = _default_string;
	}

	private void AddWatchVarToVector(WatchVar wv) {
		if (null != wv && wv.get_open()) {
			for (int i = 0; i < wv.get_num_children(); i++) {
				WatchVar wvc = wv.get_child(i);
				watchVarVector.add(wvc);
				AddWatchVarToVector(wvc);
			}
		}
	}

	private void ResetWatchVarVector() {
		watchVarVector = new Vector<WatchVar>();
		AddWatchVarToVector(watchVarRoot);
	}

	private void ExpandWatchVar(WatchVar wv) {
		StructureTypeInfo this_wv_sti = wv.get_sti();
		var_number = wv.get_var_number();
		StringTokenizer tokenizer = new StringTokenizer(this_wv_sti.getBaseClassExpandedPreFinalPassInfo(structInfoByNameHashtable, true), ";");
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			AddTypeString(token, wv);
		}
		wv.set_expanded(true);
	}

	public void ToggleOpen(int i) {
		if (null != watchVarVector &&
				i >= 0 &&
				i < watchVarVector.size()) {
			WatchVar wv = watchVarVector.elementAt(i);
			if (!wv.get_new_struct()) {
				return;
			}
			boolean was_open = wv.get_open();
			wv.set_open(!was_open);
			if (wv.get_open() && !wv.get_expanded() && wv.get_new_struct()) {
				ExpandWatchVar(wv);
			}
			ResetWatchVarVector();
			this.fireTableCellUpdated(i, 0);
			if (wv.get_open()) {
				this.fireTableRowsInserted(i + 1, i + wv.get_num_children());
			} else {
				this.fireTableRowsDeleted(i + 1, i + wv.get_num_children());
			}
		}
	}

	@Override
	public void setValueAt(Object value, int row, int col) {
		if (col == 1) {
			WatchVar wv = watchVarVector.elementAt(row);
			if (wv.get_new_struct()) {
				return;
			}
			if (value.toString().compareTo("") == 0 &&
					null != default_string) {
				val_array[wv.get_var_number()] = default_string;
			}
			val_array[wv.get_var_number()] = value.toString();
			fireTableCellUpdated(row, col);
		}
	}

	private int get_var_size(String s) {
		return 1;
	}

	private void parse_vtype(String s, WatchVar wv) {
		try {
			StringTokenizer tokenizer = new StringTokenizer(s, " ");
			while (tokenizer.hasMoreTokens()) {
				String token = tokenizer.nextToken();
				if (token.compareTo("unsigned") == 0) {
					return;
				} else if (token.compareTo("int") == 0) {
					return;
				} else if (token.compareTo("short") == 0) {
					return;
				} else if (token.compareTo("long") == 0) {
					return;
				} else if (token.compareTo("float") == 0) {
					return;
				} else if (token.compareTo("double") == 0) {
					return;
				} else if (token.compareTo("NML_DYNAMIC_LENGTH_ARRAY") == 0) {
					wv.set_ndla(true);
					continue;
				} else if (token.compareTo("class") == 0) {
					continue;
				} else if (token.compareTo("struct") == 0) {
					continue;
				} else if (token.compareTo("enum") == 0) {
					continue;
				} else if (structInfoByNameHashtable.containsKey(token)) {
					wv.set_new_struct(true);
					wv.set_sti((StructureTypeInfo) structInfoByNameHashtable.get(token));
					return;
				} else if (ModuleInfo.m_enumInfoHashTable.containsKey(token)) {
					EnumTypeInfo eti = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(token);
					wv.setEnum_info(eti);
					wv.setEnumeration(true);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void AddArrayElem(String vtype, String preLString, String postLString, int i, int lsquareBraceIndex, WatchVar wvParent) {
		String vname = preLString + "[" + i + "]" + postLString;
		lsquareBraceIndex = vname.indexOf('[', lsquareBraceIndex + 1);
		if (lsquareBraceIndex > 0) {
			ExpandArray(vtype, vname, lsquareBraceIndex, wvParent);
			return;
		}
		String s = vtype + " " + vname;
		PutWv(vtype, vname, s, wvParent, true);
	}
	private boolean bad_var_number_printed = false;

	private void PutWv(String vtype, String vname, String s, WatchVar wvParent, boolean is_array) {
//        if(true)
//        {
//            System.out.println("s = " + s);
//            System.out.println("var_number = " + var_number);
//        }
		WatchVar wv = new WatchVar(s);
		parse_vtype(s, wv);
		if (null != val_array && var_number >= val_array.length && !bad_var_number_printed) {
			diagapplet.utils.DiagError.println("s=" + s + ", wv=" + wv + ", var_number=" + var_number + ", val_array.length=" + val_array.length);
			diagapplet.utils.DiagError.PrintException(new Exception("Bad var number"));
			bad_var_number_printed = true;
		}
		wv.set_var_number(var_number);
		String plotName = vname;
		if (null != wvParent.get_plotName())//
		{
			plotName = wvParent.get_plotName() + "." + vname;
		}
		wv.set_plotName(plotName);
		wv.set_parent(wvParent);
		wv.set_struct_level(wvParent.get_struct_level() + 1);
		wv.set_is_array(is_array);
		wvParent.add_child(wv);
		int var_count = wv.get_var_count_size();
//        System.out.println("var_count = " + var_count);
		var_number += var_count;

	}

	private void ExpandArray(String vtype, String vname, int lsquareBraceIndex, WatchVar wvParent) {
		int rsquareBraceIndex = vname.indexOf(']', lsquareBraceIndex + 1);
		if (rsquareBraceIndex > lsquareBraceIndex) {
			String lenString = vname.substring(lsquareBraceIndex + 1, rsquareBraceIndex).trim();
			String preLString = vname.substring(0, lsquareBraceIndex);
			String postLString = vname.substring(rsquareBraceIndex + 1);
			int array_len = Integer.valueOf(lenString).intValue();
			for (int i = 0; i < array_len; i++) {
				AddArrayElem(vtype, preLString, postLString, i, lsquareBraceIndex, wvParent);
			}
		}
	}

	private void AddTypeString(String s, WatchVar wvParent) {
		int lsquareBraceIndex = s.indexOf('[');
		int spc_index = s.lastIndexOf(' ');
		if (lsquareBraceIndex > 0 && lsquareBraceIndex < spc_index && spc_index > 0) {
			spc_index = s.lastIndexOf(' ', lsquareBraceIndex - 1);
		}
		if (spc_index < 0) {
			System.err.println("Bad type string" + s);
			Thread.dumpStack();
			System.exit(1);
		}
		String vtype = s.substring(0, spc_index);
		String vname = s.substring(spc_index + 1);
		lsquareBraceIndex = vname.indexOf('[');
		if (lsquareBraceIndex > 0 && vtype.compareTo("char") != 0 && !vtype.endsWith(" char")) {
			ExpandArray(vtype, vname, lsquareBraceIndex, wvParent);
			return;
		}
		PutWv(vtype, vname, s, wvParent, wvParent.get_is_array());
	}

	public void SetTypeInfo(StructureTypeInfo _sti, Hashtable _structInfoByNameHashtable) {
		sti = _sti;
		bad_var_number_printed = false;
		structInfoByNameHashtable = _structInfoByNameHashtable;
		watchVarRoot = new WatchVar("//ROOT//");
		watchVarRoot.set_open(true);
		watchVarRoot.set_struct_level(-1);
		var_number = 0;
		val_array = null;
		if (null != sti) {
			String s = sti.getBaseClassExpandedPreFinalPassInfo(structInfoByNameHashtable, false);
			if(s.endsWith(";")) {
				s = s.substring(0, s.length()-1);
			}
			StringTokenizer tokenizer = new StringTokenizer(s, ";");
			while (tokenizer.hasMoreTokens()) {
				AddTypeString(tokenizer.nextToken(), watchVarRoot);
			}
			ResetWatchVarVector();
			val_array = new String[var_number];
			if (null != default_string) {
				for (int i = 0; i < val_array.length; i++) {
					val_array[i] = default_string;
				}
			}
		}
		this.fireTableDataChanged();
		this.fireTableStructureChanged();
	}
//    public void SetTypeInfo(Enumeration e)
//    {
//	var_number=0;
//	watchVarVector = null;
//	watchVarVectorByGen = new Vector<WatchVar>();
//	watchVarRoot = new WatchVar();
//	watchVarRoot.set_open(true);
//	watchVarRoot.set_varname("//ROOT//");
//	watchVarVectorByGen.add(watchVarRoot);
//	val_array=null;
//	last_struct = null;
//	long fm = Runtime.getRuntime().freeMemory();
//	max_var_num = fm/32;
//	while(e.hasMoreElements())
//	{
//	    Object obj = e.nextElement();
//	    AddTypeString(obj.toString());
//	    fm = Runtime.getRuntime().freeMemory();
//	    if(max_var_num > 0 && var_number >= max_var_num || fm < 1024)
//	    {
//		AddTypeString("x !MAX_NUMBER_OF_VARIABLES_REACHED!");
//		break;
//	    }
//	}
//	ResetWatchVarVector();
//	val_array = new String[var_number+1];
//	if(null != default_string)
//	{
//	    for(int i = 0; i < val_array.length; i++)
//	    {
//		val_array[i] = default_string;
//	    }
//	}
//	this.fireTableDataChanged();
//	this.fireTableStructureChanged();
//    }

	public void SetDataInfo(Enumeration e) {
		int i = 0;
		if (null == val_array || null == watchVarVector) {
			return;
		}
		bad_var_number_printed = false;
		while (i < val_array.length && e.hasMoreElements()) {
			Object obj = e.nextElement();
			val_array[i] = obj.toString();
//            if(i < 100)
//            {
//                System.out.println("val_array["+i+"]="+val_array[i]);
//            }
			i++;
		}
		for (i = 0; i < watchVarVector.size(); i++) {
			this.fireTableCellUpdated(i, 1);
		}
	//this.fireTableDataChanged();
	}

	public String getColumnName(int column) {
		if (column == 0) {
			return "Variable:";
		} else {
			return "Value:";
		}
	}

	public int getRowCount() {
		if (null == watchVarVector) {
			return 0;
		}
		return watchVarVector.size();
	}

	public int getColumnCount() {
		return 2;
	}

	public int getVarNumAt(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.get_var_number();
		}
		return -1;
	}

	public String getVarNameAt(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.get_plotName();
		}
		return null;
	}

	public boolean getIsNewStruct(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.get_new_struct();
		}
		return true;
	}

	public boolean getIsEnumeration(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.isEnumeration();
		}
		return true;
	}

	public EnumTypeInfo getEnumTypeInfo(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.getEnum_info();
		}
		return null;
	}

	public boolean getIsArray(int rowIndex) {
		WatchVar wv = watchVarVector.elementAt(rowIndex);
		if (null != wv) {
			return wv.get_is_array();
		}
		return true;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		WatchVar wv = null;
		try {
			if (null == watchVarVector) {
				return null;
			}
			wv = watchVarVector.elementAt(rowIndex);
			final int wv_var_number = wv.get_var_number();
			if (columnIndex == 0) {
				return wv.toString();
			} else {
				if (wv.get_new_struct()) {
					return null;
				} else if (wv.isEnumeration()) {
					try {
						EnumTypeInfo eti = wv.getEnum_info();
						String v = val_array[wv_var_number];
						if (null == eti || null == eti.hashtable || v == null || v.length() < 1) {
							return v;
						}
						Integer Ivalue = Integer.valueOf(v);
						String ev = (String) eti.hashtable.get(Ivalue);
						if (ev == null || ev.length() < 1) {
							return v;
						}
						return ev;
					} catch (Exception e) {
						e.printStackTrace();
						return val_array[wv_var_number];
					}
				} else {
					return val_array[wv_var_number];
				}
			}
		} catch (Exception e) {
			diagapplet.utils.DiagError.println("RowIndex =" + rowIndex + ", columnIndex=" + columnIndex + ", wv=" + wv);
			diagapplet.utils.DiagError.PrintException(e);
			return "";
		}
	}
}
