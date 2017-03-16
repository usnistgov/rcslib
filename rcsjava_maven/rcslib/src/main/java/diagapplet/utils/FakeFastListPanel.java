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

import java.util.*;

/**
 * Tracks the same data as FastListPane without any actual display.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class FakeFastListPanel implements FastListPanelInterface
{
    Vector items = new Vector();
    int last_line_selected = -1;
    Vector selected_indexes = null;
    Vector openStructures = null;
    boolean newOpenStructure = true;
    int lines_visible;
    int start_line = 0;
    int selected_line;
    int count = 0;
    static public boolean debug_on = false;
    static public final boolean display_on = true;
    boolean multipleSelections = true;
    int maxlinelength;
    boolean repaint_needed=false;

    FastListPanelItem flpItem = null;

    private void private_init()
    {
	start_line = 0;
	selected_line = -1;
	if(multipleSelections)
	    {
		selected_indexes = new Vector();
	    }
    }

    public FakeFastListPanel()
    {
	multipleSelections = true;
	private_init();
    }

    public FakeFastListPanel(int rows, int cols,  boolean set_multipleSelections, Object p)
    {
	multipleSelections = set_multipleSelections;
	private_init();
    }


    //    @SuppressWarnings("unchecked")
    public void add(String s)
    {
	if(debug_on)
	    {
		System.out.println("FastListPanel.add("+s+") ");
		System.out.println("\t items="+items);
		if(null != items)
		    {
			System.out.println("\t items.size() = "+items.size());
		    }
		System.out.println("\t selected_indexes ="+selected_indexes);
		if(null != selected_indexes)
		    {
			System.out.println("\t selected_indexes.size() = "+selected_indexes.size());
		    }
	    }
	if(null != s)
	    {
		flpItem = new FastListPanelItem();
		flpItem.s = s;
		flpItem.var_number = -1;
		items.addElement(flpItem);
		if(s.length() > maxlinelength)
		    {
			maxlinelength = s.length();
		    }
		repaint_needed = true;
	    }
    }

    public void add(String s, int index)
    {
	add(s,index,-1);
    }

    //    @SuppressWarnings("unchecked")
    public void add(String s, int index, int var_number)
    {
	try
	    {
		if(false)
		    {
			System.out.println("FastListPanel.add("+s+", "+index+", "+var_number+") ");
			System.out.println("\t items="+items);
			if(null != items)
			    {
				System.out.println("\t items.size() = "+items.size());
			    }
			System.out.println("\t selected_indexes ="+selected_indexes);
			if(null != selected_indexes)
			    {
				System.out.println("\t selected_indexes.size() = "+selected_indexes.size());
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	try
	    {
		if(null != s)
		    {
			if(index >= 0 && index < items.size())
			    {
				flpItem = new FastListPanelItem();
				flpItem.s = s;
				flpItem.var_number = var_number;
				items.insertElementAt(flpItem,index);
				for(int i = 0; i < selected_indexes.size(); i++)
				    {
					int old_index = ((Integer) selected_indexes.elementAt(i)).intValue();
					if(old_index >= index)
					    {
						selected_indexes.setElementAt(old_index+1,i);
					    }
				    }
			    }
			else
			    {
				flpItem = new FastListPanelItem();
				flpItem.s = s;
				flpItem.var_number = var_number;
				items.addElement(flpItem);
				if(s.length() > maxlinelength)
				    {
					maxlinelength = s.length();
				    }
			    }
			repaint_needed = true;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    //    @SuppressWarnings("unchecked")
    public void replaceItem(String s, int index)
    {
	if(null != s)
	    {
		flpItem = new FastListPanelItem();
		flpItem.s = s;
		flpItem.var_number = -1;
		items.setElementAt(flpItem,index);
		if(s.length() > maxlinelength)
		    {
			maxlinelength = s.length();
		    }
		repaint_needed = true;
	    }
    }

    //    @SuppressWarnings("unchecked")
    public void replaceItem(String s, int index, int var_number)
    {
	if(null != s)
	    {
		flpItem = new FastListPanelItem();
		flpItem.s = s;
		flpItem.var_number = var_number;
		items.setElementAt(flpItem,index);
		if(s.length() > maxlinelength)
		    {
			maxlinelength = s.length();
		    }
		repaint_needed = true;
	    }
    }

    public void removeAll()
    {
	clear();
    }

    public void clear()
    {
	items.removeAllElements();
	if(null != selected_indexes)
	    {
		selected_indexes.removeAllElements();
	    }
	maxlinelength = 10;
	repaint_needed = true;
    }



    //    @SuppressWarnings("unchecked")
    public void select(int index)
    {
	try
	    {
		last_line_selected = index;
		if(!multipleSelections)
		    {
			if(null != items && debug_on)
			    {
				System.out.println("FastListPanel.select("+index+"): items.size() ="+items.size());
			    }
			selected_line = index;
			if(selected_line < start_line || selected_line - start_line > lines_visible)
			    {
				start_line = selected_line - (selected_line%lines_visible);
			    }
			
			String structName = getSelectedItem();
			if(null != structName)
			    {
				if(structName.startsWith("[+] "))
				    {
					//AddOpenStructure(structName.substring(4));
					newOpenStructure = true;
				    }
				else if(structName.startsWith("[-] "))
				    {
					//RemoveOpenStructure(structName.substring(4));
					newOpenStructure = true;
				    }
			    }
		    }
		else
		    {
			selected_indexes.addElement(index);
		    }
		count++;
		repaint_needed = true;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void deselect(int index)
    {
	try
	    {
		if(!multipleSelections)
		    {
			if(selected_line == index)
			    {
				selected_line = -1;
			    }
		    }
		else
		    {
			for(int i = 0; i < selected_indexes.size(); i++)
			    {
				int s = ((Integer) selected_indexes.elementAt(i)).intValue();
				if(s == index)
				    {
					selected_indexes.removeElementAt(i);
				    }
			    }
		    }
		count++;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public boolean isSelected(int index)
    {
	try
	    {
		if(!multipleSelections)
		    {
			return index >= 0 && index == selected_line;
		    }
		else
		    {
			for(int i = 0; i < selected_indexes.size(); i++)
			    {
				int s = ((Integer) selected_indexes.elementAt(i)).intValue();
				if(s == index)
				    {
					return true;
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return false;
    }


    public int getSelectedIndex()
    {
	if(multipleSelections)
	    {
		return -1;
	    }
	return selected_line;
    }

    public String getSelectedItem()
    {
	if(multipleSelections)
	    {
		return null;
	    }
	try
	    {
		if(selected_line < 0 || selected_line >= items.size())
		    {
			System.out.println("Invalid selected_line = "+selected_line);
			return null;
		    }
		flpItem = (FastListPanelItem) items.elementAt(selected_line);
		if(null != flpItem && debug_on)
		    {
			if(null == flpItem.s)
			    {
				System.out.println("selected_line = "+selected_line);
			    }
			else
			    {
				System.out.println("selected item = "+flpItem.s);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return flpItem.s;
    }

    public int getSelectedVarNumber()
    {
	if(multipleSelections)
	    {
		return -1;
	    }
	try
	    {
		if(selected_line < 0 || selected_line >= items.size())
		    {
			System.out.println("Invalid selected_line = "+selected_line);
			return -1;
		    }
		flpItem = (FastListPanelItem) items.elementAt(selected_line);
		if(null != flpItem && debug_on)
		    {
			if(null == flpItem.s)
			    {
				System.out.println("selected_line = "+selected_line);
			    }
			else
			    {
				System.out.println("selected item = "+flpItem.s);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(debug_on)
	    {
		System.out.println("Selected Var Number = "+flpItem.var_number);
	    }
	return flpItem.var_number;
    }
    public int []getSelectedIndexes()
    {
	if(!multipleSelections)
	    {
		return null;
	    }
	if(null == selected_indexes)
	    {
		return null;
	    }
	try
	    {
		int indexes[] = new int[selected_indexes.size()];
		for(int i = 0; i < selected_indexes.size(); i++)
		    {
			indexes[i] = ((Integer) selected_indexes.elementAt(i)).intValue();
		    }
		return indexes;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }

    public String[] getSelectedItems()
    {
	if(!multipleSelections)
	    {
		return null;
	    }
	if(null == selected_indexes)
	    {
		return null;
	    }
	try
	    {
		String selected_items[] = new String[selected_indexes.size()];
		for(int i = 0; i < selected_indexes.size(); i++)
		    {
			selected_items[i] = ((FastListPanelItem) items.elementAt(((Integer) selected_indexes.elementAt(i)).intValue())).s;
		    }
		return selected_items;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }

    public Object[] getSelectedObjects()
    {
	return getSelectedItems();
    }

    public String getItem(int index)
    {
	String str = null;
	try
	    {
		if(index < 0)
		    {
			return null;
		    }
		flpItem = (FastListPanelItem) items.elementAt(index);
		if(null != flpItem)
		    {
			str = flpItem.s;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return str;
    }

    public int getVarNumber(int index)
    {
	int var_num = 0;
	try
	    {
		if(index < 0)
		    {
			return -1;
		    }
		var_num = ((FastListPanelItem) items.elementAt(index)).var_number;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return var_num;
    }

    public int countItems()
    {
	return items.size();
    }

    public int getItemCount()
    {
	return items.size();
    }
}
