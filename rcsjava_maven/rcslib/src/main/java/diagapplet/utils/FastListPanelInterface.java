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

/**
 * Common interface to FastListPanel and FakeFastListPanel.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public interface FastListPanelInterface
{
//    Vector items = new Vector();
//    int last_line_selected = -1;
//    Vector selected_indexes = null;
//    Vector openStructures = null;
//    boolean newOpenStructure = true;
//    int lines_visible=0;
//    int start_line = 0;
//    int selected_line=0;
//    int count = 0;
//    static public boolean debug_on = false;
//    static public final boolean display_on = true;
//    boolean multipleSelections = false;
//    int maxlinelength=0;
//    boolean repaint_needed=false;
//
//    FastListPanelItem flpItem = null;
    public void add(String s);
    public void add(String s, int index);
    public void add(String s, int index, int var_number);
    public void replaceItem(String s, int index);
    public void replaceItem(String s, int index, int var_number);
    public void removeAll();
    public void clear();
    public void select(int index);
    public void deselect(int index);
    public boolean isSelected(int index);
    public int getSelectedIndex();
    public String getSelectedItem();
    public int getSelectedVarNumber();
    public int []getSelectedIndexes();
    public String[] getSelectedItems();
    public Object[] getSelectedObjects();
    public String getItem(int index);
    public int getVarNumber(int index);
    public int countItems();
    public int getItemCount();
}
