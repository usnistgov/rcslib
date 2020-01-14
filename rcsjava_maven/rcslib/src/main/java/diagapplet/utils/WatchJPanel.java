/*
//The NIST RCS (Real-time Control Systems)
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
 * WatchJPanel.java
 *
 * Created on December 26, 2006, 5:19 PM
 */
package diagapplet.utils;

import diagapplet.CodeGen.EnumTypeInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import java.awt.Desktop;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.text.DateFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.Vector;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import rcs.nml.NMLMessageDictionary;
import rcs.nml.NMLmsg;
import rcs.nml.PackedFileReader;
import rcs.nml.PackedFileWriter;
import rcs.nml.XMLFileReader;
import rcs.nml.XMLFileWriter;

/**
 * Swing JPanel that replaces FastListPanel in newer diagnostics tools for
 * displaying lists of variables in hierarchical structures. 
 * @author  shackle
 */
public class WatchJPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 2613934L;
    private WatchTableModel wtm = null;
    private ListSelectionModel rowSM = null;
    TableCellRenderer tcr = null;

    /** Creates new form WatchJPanel */
    public WatchJPanel() {
	initComponents();
	wtm = new WatchTableModel();
	jTable1.setModel(wtm);
	wtm.fireTableDataChanged();
	wtm.fireTableStructureChanged();
	jTable1.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
//Ask to be notified of selection changes.
	rowSM = jTable1.getSelectionModel();
//	rowSM.addListSelectionListener(new ListSelectionListener()
//	{
//	    public void valueChanged(ListSelectionEvent e)
//	    {
//		//Ignore extra messages.
//		if (e.getValueIsAdjusting()) return;
//
//		ListSelectionModel lsm =
//			(ListSelectionModel)e.getSource();
//		if (!lsm.isSelectionEmpty())
//		{
//		    int selectedRow = lsm.getMinSelectionIndex();
//		    SelectRow(selectedRow);
//		}
//	    }
//	});
//	tcr = jTable1.getDefaultRenderer(Object.class);
//	jTable1.setDefaultRenderer(Object.class,
//		new TableCellRenderer()
//		{
//			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
//			{
//			    if(javax.swing.JComponent.class.isInstance(value))
//			    {
//				return (Component) value;
//			    }
//			    return tcr.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
//			}
//		} 
//	);
	//AddTableListener();
	}
    private Runnable changeIdRunnable = null;

    public void setChangeIdRunnable(Runnable _r) {
	changeIdRunnable = _r;
    }

    public TableCellEditor getTableColumnCellEditor(int column) {
	TableColumnModel tcm = jTable1.getColumnModel();
	TableColumn tc = tcm.getColumn(column);
	TableCellEditor tce = tc.getCellEditor();
	return tce;
    }
//	private TableColumnModel last_set_tcm = null;
//	private TableColumn last_tc0 = null;
//	private TableColumn last_tc1 = null;
//	private TableCellEditor last_set_tce = null;
//	private boolean editable = false;

    public void setTableCellEditor(int column, TableCellEditor _tce) {
	try {
	    TableColumnModel tcm = jTable1.getColumnModel();
//			last_set_tcm = tcm;
	    TableColumn tc = tcm.getColumn(column);
//			if (column == 1) {
//				last_tc1 = tc;
//			}
//			if (column == 0) {
//				last_tc0 = tc;
//			}
	    tc.setCellEditor(_tce);
//			last_set_tce = _tce;
//			editable = true;
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void SetDefault(String default_string) {
	wtm.SetDefault(default_string);
    }

    public void setValueForVarNumber(int var_number, double value) {
	wtm.setValueForVarNumber(var_number, value);
    }

    public String getValueStringForVarNumber(int var_number) {
	return wtm.getValueStringForVarNumber(var_number);
    }

    public void SetRowValue(int row, String s) {
	wtm.SetRowValue(row, s);
    }

    public void setEditable(boolean _editable) {
	wtm.set_editable(_editable);
    }

    public void addListSelectionListener(ListSelectionListener _lsl) {
	rowSM.addListSelectionListener(_lsl);
    }

    public int GetVarNum() {
	last_selected_row = rowSM.getMinSelectionIndex();
	if (last_selected_row < 0 || null == wtm) {
	    return -1;
	}
	return wtm.getVarNumAt(last_selected_row);
    }

    public String GetSelectedValue() {
	try {
	    int vn = this.GetVarNum();
	    if (vn > 0) {
		return this.getValueStringForVarNumber(this.GetVarNum());
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return null;
    }

    public String GetVarName() {
	last_selected_row = rowSM.getMinSelectionIndex();
	if (last_selected_row < 0) {
	    return null;
	}
	return wtm.getVarNameAt(last_selected_row);
    }

    public boolean GetIsNewStruct() {
	last_selected_row = rowSM.getMinSelectionIndex();
	if (last_selected_row < 0) {
	    return true;
	}
	return wtm.getIsNewStruct(last_selected_row);
    }

    public boolean GetIsEnumeration(int row) {
	return wtm.getIsEnumeration(row);
    }

    public EnumTypeInfo getEnumTypeInfo(int row) {
	return wtm.getEnumTypeInfo(row);
    }

    public boolean GetIsArray() {
	last_selected_row = rowSM.getMinSelectionIndex();
	if (last_selected_row < 0) {
	    return true;
	}
	return wtm.getIsArray(last_selected_row);
    }
    private NMLMessageDictionary nml_dict = null;

    public void setNmlMessageDictionary(NMLMessageDictionary _nml_dict) {
	this.nml_dict = _nml_dict;
    }

    public void Clear() {
	if (null != wtm) {
	    wtm.Clear();
	}
    }
    private int last_selected_row = -1;

    public StructureTypeInfo getStructureTypeInfo() {
	if (null != wtm) {
	    return wtm.getStructureTypeInfo();
	}
	return null;
    }

    public void SelectRow(int selectedRow) {

	if (null != wtm) {
	    wtm.ToggleOpen(selectedRow);
	    if (jTable1.getSelectionModel().getMinSelectionIndex() != selectedRow) {
		jTable1.getSelectionModel().setLeadSelectionIndex(selectedRow);
	    }
	}
	last_selected_row = selectedRow;
    }
    private Hashtable hashtable_by_id = null;

    public void set_hashtable_by_id(Hashtable _hashtable_by_id) {
	this.hashtable_by_id = _hashtable_by_id;
    }
    private long last_id = -1;

    public void SetTypeInfo(StructureTypeInfo _sti, Hashtable _ht) {
	if ((_sti != null || _ht != null) && wtm != null) {
	    wtm.SetTypeInfo(_sti, _ht);
	}
	if (this.changeIdRunnable != null && last_id != this.getId()) {
	    last_id = this.getId();
	    this.changeIdRunnable.run();
	}
    }

    public void SetDataInfo(Enumeration e) {
	if (e != null && wtm != null) {
	    wtm.SetDataInfo(e);
	}
    }
    private String last_saved_message_string_to_show = null;
    private long last_saved_message_id = -1;

    private void EnableMatchingPreviousMessages(String s) {
	try {
	    if (null == JMenuPreviousMessages) {
		return;
	    }
	    for (int i = 0; i < JMenuPreviousMessages.getItemCount(); i++) {
		JMenuItem jmi = JMenuPreviousMessages.getItem(i);
		if (jmi.getText().indexOf(s) >= 0) {
		    jmi.setEnabled(true);
		} else {
		    jmi.setEnabled(false);
		}
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private void EnableAllPreviousMessages() {
	try {
	    if (null == JMenuPreviousMessages) {
		return;
	    }
	    for (int i = 0; i < JMenuPreviousMessages.getItemCount(); i++) {
		JMenuItem jmi = JMenuPreviousMessages.getItem(i);
		jmi.setEnabled(true);
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private void SaveMessage(String s, String s_to_show) {
	try {
	    StructureTypeInfo stm = wtm.getStructureTypeInfo();
	    if (null != stm) {
		final File f = File.createTempFile("diag_saved_message_" + stm.getName() + "_", ".txt");
		this.DumpDataStringFile(f, s);
		if (null == JMenuPreviousMessages) {
		    JMenuPreviousMessages = new JMenu("Previous Messages");
		}
		String time_string = "";
		try {
		    // I don't like the default timezone java chooses either set your TZ environment variable or live with it being write for this programmer.
		    DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
		    String TZ_env = System.getenv("TZ");
		    if (null == TZ_env) {
			TZ_env = "America/New_York";
		    }
		    df.setTimeZone(TimeZone.getTimeZone(TZ_env));
		    time_string = df.format(new Date()) + " : ";
		} catch (Exception e) {
		    e.printStackTrace();
		}
		JMenuItem jmi = new JMenuItem(time_string + stm.getName() + " : " + s_to_show);
		jmi.addActionListener(new ActionListener() {

		    @Override
		    public void actionPerformed(ActionEvent e) {
			try {
			    LoadDataStringFile(f);
			} catch (Exception exception) {
			    exception.printStackTrace();
			}
			jpop = null;
		    }
		});
		JMenuPreviousMessages.add(jmi);
	    }
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    public long getId() {
	return wtm.getStructureTypeInfo().Id;
    }

    public String getDataString() {
	String s = wtm.getDataString();
	try {
	    String s_to_show = s;
	    if (s_to_show.length() > 72) {
		s_to_show = s_to_show.substring(0, 70) + "...";
	    }
	    long id = this.wtm.getStructureTypeInfo().Id;
	    if (this.recordAll && this.wtm.get_editable() && s != null && id > 0) {
		if (last_saved_message_string_to_show == null || s_to_show.compareTo(last_saved_message_string_to_show) != 0 || id != last_saved_message_id) {
		    SaveMessage(s, s_to_show);
		    last_saved_message_string_to_show = s_to_show;
		    last_saved_message_id = id;
		}
	    }

	} catch (Exception e) {
	    e.printStackTrace();
	}
	return s;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();

        jScrollPane1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jScrollPane1MouseClicked(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                jScrollPane1MousePressed(evt);
            }
        });

        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null}
            },
            new String [] {
                "Variable", "Value"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.Object.class
            };
            boolean[] canEdit = new boolean [] {
                false, true
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_ALL_COLUMNS);
        jTable1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jTable1MouseClicked(evt);
            }
            public void mousePressed(java.awt.event.MouseEvent evt) {
                jTable1MousePressed(evt);
            }
        });
        jScrollPane1.setViewportView(jTable1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 294, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 187, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jTable1MouseClicked(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jTable1MouseClicked
    {//GEN-HEADEREND:event_jTable1MouseClicked
	if (evt.isPopupTrigger()) {
	    show_popup(evt);
	} else {
	    wtm.ToggleOpen(jTable1.getSelectionModel().getMinSelectionIndex());
	}
    }//GEN-LAST:event_jTable1MouseClicked
    private JPopupMenu jpop = null;

    public void DumpText() {
	try {
	    String prefix = "diag_text_dump_";
	    if (null != wtm) {
		prefix += this.wtm.getTypeInfoName() + "_";
	    }
	    File tf = File.createTempFile(prefix, ".txt");
	    FileOutputStream fos = new FileOutputStream(tf);
	    PrintStream ps = new PrintStream(fos);
//			diagapplet.utils.DiagError.println("WatchJPanel.DumpText():");
	    if (null != wtm) {
//				diagapplet.utils.DiagError.println(this.wtm.getTypeInfoName());
		ps.println(this.wtm.getTypeInfoName());
	    }
//			diagapplet.utils.DiagError.println("\t" + jTable1.getColumnName(0) + ",\t" + jTable1.getColumnName(1));
	    ps.println("\t" + jTable1.getColumnName(0) + ",\t" + jTable1.getColumnName(1));
	    for (int i = 0; i < this.jTable1.getRowCount(); i++) {
		String var_name = (String) jTable1.getValueAt(i, 0);
		if (!var_name.trim().startsWith("[+]")) {
		    String val = (String) this.jTable1.getValueAt(i, 1);
		    String s = "\t" + var_name + ",\t" + val;
		    if (val == null) {
			s = "\t" + var_name + ",";
		    }
//					diagapplet.utils.DiagError.println(s);
		    ps.println(s);
		}
	    }
//			diagapplet.utils.DiagError.println("");
	    ps.close();
	    fos.close();
	    ps = null;
	    fos = null;
	    Desktop.getDesktop().open(tf);
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    private void PrintTypes(PrintStream ps, final Hashtable ht, final String info_string, Vector<StructureTypeInfo> stis_used_vector) throws Exception {
	StringTokenizer string_tokenizer = new StringTokenizer(info_string, ";\r\n");
	while (string_tokenizer.hasMoreTokens()) {
	    String token = string_tokenizer.nextToken();
	    if (token.indexOf('[') > 0) {
		token = token.substring(0, token.indexOf('['));
	    }
	    if (token.lastIndexOf(' ') > 0) {
		token = token.substring(0, token.lastIndexOf(' '));
	    }
	    StringTokenizer st2 = new StringTokenizer(token, " \t");
	    while (st2.hasMoreTokens()) {
		String token_part = st2.nextToken();
		final StructureTypeInfo sti2 = (StructureTypeInfo) ht.get(token_part);
		if (null != sti2) {
		    boolean already_printed = false;
		    for (int i = 0; i < stis_used_vector.size(); i++) {
			final StructureTypeInfo sti_for_compare = stis_used_vector.elementAt(i);
			if (sti_for_compare.equals(sti2) ||
				sti_for_compare.getName().compareTo(sti2.getName()) == 0) {
			    already_printed = true;
			    break;
			}
		    }
		    if (!already_printed) {
			ps.println("# " + sti2.getName() + "<" + sti2.fromFileName + ":" + sti2.fromLineNumber + "> -- " + sti2.PreFinalPassInfo);
			ps.println("#[" + sti2.getName() + " :Id=" + sti2.Id + ":HiddenInfo=" + sti2.HiddenInfo);
			stis_used_vector.addElement(sti2);
			PrintTypes(ps, ht, sti2.PreFinalPassInfo, stis_used_vector);
		    }
		}
	    }
	}
    }

    public void DumpDataStringFile(File f, String data_string) {
	try {
	    FileOutputStream fos = new FileOutputStream(f);
	    PrintStream ps = new PrintStream(fos);
//			diagapplet.utils.DiagError.println("WatchJPanel.DumpDataString():");
	    try {
		// I don't like the default timezone java chooses either set your TZ environment variable or live with it being write for this programmer.
		DateFormat df = DateFormat.getDateTimeInstance(DateFormat.FULL, DateFormat.FULL);
		String TZ_env = System.getenv("TZ");
		if (null == TZ_env) {
		    TZ_env = "America/New_York";
		}
		df.setTimeZone(TimeZone.getTimeZone(TZ_env));
		ps.println("# " + df.format(new Date()));
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	    final StructureTypeInfo sti = this.wtm.getStructureTypeInfo();
	    ps.println("# Name=" + sti.getName());
	    ps.println("# Id=" + sti.Id);
	    final Hashtable ht = wtm.get_structInfoByNameHashtable();
	    if (null != sti.DerivedFrom && sti.DerivedFrom.compareTo("NMLmsg") != 0) {
		StructureTypeInfo stip = (StructureTypeInfo) ht.get(sti.DerivedFrom);
		while (null != stip) {
		    ps.println("# " + stip.getName() + "<" + stip.fromFileName + ":" + stip.fromLineNumber + "> -- " + stip.PreFinalPassInfo);
		    if (null == stip.DerivedFrom || stip.DerivedFrom.compareTo("NMLmsg") == 0) {
			stip = null;
			break;
		    } else {
			stip = (StructureTypeInfo) ht.get(stip.DerivedFrom);
		    }
		}
	    }
	    String info_string = sti.PreFinalPassInfo;
	    ps.println("# " + sti.getName() + "<" + sti.fromFileName + ":" + sti.fromLineNumber + "> -- " + info_string);
	    ps.println("#[" + sti.getName() + " :Id=" + sti.Id + ":HiddenInfo=" + sti.HiddenInfo);
	    Vector<StructureTypeInfo> stis_used_vector = new Vector<StructureTypeInfo>();
	    PrintTypes(ps, ht, info_string, stis_used_vector);
//			diagapplet.utils.DiagError.println(data_string);
	    ps.print(data_string);
	    ps.close();
	    fos.close();
	    ps = null;
	    fos = null;
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void DumpDataString() {
	try {
	    String prefix = "diag_text_data_string_";
	    if (null != wtm) {
		prefix += this.wtm.getTypeInfoName() + "_";
	    }
	    File tf = File.createTempFile(prefix, ".txt");
	    if (null == last_data_string_dir) {
		last_data_string_dir = tf.getParentFile();
	    }
	    String data_string = getDataString();
	    DumpDataStringFile(tf, data_string);
	    Desktop.getDesktop().open(tf);
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }
    private static File last_data_string_dir = null;

    public void LoadDataString() {
	try {
	    JFileChooser jchooser = new JFileChooser();
	    FileNameExtensionFilter txt_filter = new FileNameExtensionFilter("txt", "txt");
	    jchooser.addChoosableFileFilter(txt_filter);
	    jchooser.setMultiSelectionEnabled(false);
	    if (null != last_data_string_dir) {
		jchooser.setCurrentDirectory(last_data_string_dir);
	    }
	    jchooser.showOpenDialog(this);
	    File f = jchooser.getSelectedFile();
	    LoadDataStringFile(f);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void LoadDataStringFile(File f) {
	try {
	    last_data_string_dir = null;
	    if (null != f) {
		last_data_string_dir = f.getParentFile();
//				diagapplet.utils.DiagError.println("WatchJPanel.LoadDataStringFile(): File = " + f.getAbsolutePath());

		FileInputStream fis = new FileInputStream(f);
		InputStreamReader isr = new InputStreamReader(fis);
		BufferedReader br = new BufferedReader(isr);
		String line = br.readLine();
		int line_count = 0;
		Hashtable new_ht = null;
		StructureTypeInfo sti = null;
		StructureTypeInfo last_sti = null;
		String name = null;
		long id = -1;
		boolean name_found_yet = false;
		while (line.startsWith("#")) {
		    line = br.readLine();
		    line_count++;
		    if (line_count < 3) {
			int name_eq_index = line.indexOf("Name=");
			if (name_eq_index > 1) {
			    name = line.substring(name_eq_index + 5).trim();
			}
			int id_eq_index = line.indexOf("Id=");
			if (id_eq_index > 1) {
			    id = Long.valueOf(line.substring(id_eq_index + 3).trim());
			}
		    } else {
			try {
			    sti = LineToStructureTypeInfo(line, last_sti, name_found_yet);
			    if (sti != null && null != sti.getName()) {
				if (new_ht == null) {
				    new_ht = new Hashtable();
				}
				new_ht.put(sti.getName(), sti);
				if (sti.getName().compareTo(name) == 0) {
				    name_found_yet = true;
				}
//								if (this.wtm.get_structInfoByNameHashtable() != null &&
//										this.wtm.get_structInfoByNameHashtable().get(sti.Name) != null) {
//									StructureTypeInfo sti_for_compare =
//											(StructureTypeInfo) this.wtm.get_structInfoByNameHashtable().get(sti.Name);
//									if (sti_for_compare.PreFinalPassInfo.compareTo(sti.PreFinalPassInfo) != 0) {
//										diagapplet.utils.DiagError.println("WatchJPanel.LoadDataString():");
//										diagapplet.utils.DiagError.println("sti_for_compare.PreFinalPassInfo=" + sti_for_compare.PreFinalPassInfo);
//										diagapplet.utils.DiagError.println("sti.PreFinalPassInfo=" + sti.PreFinalPassInfo);
//									}
////								}
				last_sti = sti;
			    }
			} catch (Exception e) {
			    e.printStackTrace();
			}
		    }
		}
		br.close();
		isr.close();
		fis.close();
		if (null != name && null != new_ht) {
		    sti = (StructureTypeInfo) new_ht.get(name);
		    if (sti.Id < 0 && id > 0) {
			sti.Id = id;
		    }
		    if (null != sti && sti.Id != wtm.getStructureTypeInfo().Id) {
			this.SetTypeInfo(sti, new_ht);
		    }
		}
//				diagapplet.utils.DiagError.println("WatchJPanel.LoadDataString():");
//				diagapplet.utils.DiagError.println(line);
		StringTokenizer data_tokenizer = new StringTokenizer(line, ",");
		this.SetDataInfo(data_tokenizer);
	    }

	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    private StructureTypeInfo LineToStructureTypeInfo(String line, StructureTypeInfo last_sti, boolean name_found_yet) {
	try {
	    StructureTypeInfo sti = null;
	    if (line.startsWith("# ")) {
		sti = new StructureTypeInfo();
		if (!name_found_yet) {
		    if (null != last_sti) {
			sti.DerivedFrom = last_sti.getName();
		    } else {
			sti.DerivedFrom = "NMLmsg";
		    }
		    sti.is_nml_msg = true;
		}
		line = line.substring(2);
		int ltindex = line.indexOf('<');
		if (ltindex < 1) {
		    return null;
		}
		sti.setName(line.substring(0, ltindex));
		line = line.substring(ltindex + 1);
		int colonindex = line.indexOf(':');
		if (colonindex < 1) {
		    return null;
		}
		sti.fromFileName = line.substring(0, colonindex);
		line = line.substring(colonindex + 1);
		int gtindex = line.indexOf('>');
		if (gtindex < 1) {
		    return null;
		}
		sti.fromLineNumber = Integer.valueOf(line.substring(0, gtindex));
		line = line.substring(gtindex + 1);
		int dashdashindex = line.indexOf("--");
		if (dashdashindex < 1) {
		    return null;
		}
		sti.PreFinalPassInfo = line.substring(dashdashindex + 2).trim();
		return sti;
	    } else if (line.startsWith("#[") && null != last_sti) {
		line = line.substring(2);
		int colon_index1 = line.indexOf(':');
		if (colon_index1 < 1) {
		    return null;
		}
		String name_check = line.substring(0, colon_index1).trim();
		if (name_check.compareTo(last_sti.getName()) != 0) {
//					System.out.println("name_check=\n\"" + name_check + "\" not equal to \n\"" + last_sti.Name + "\"");
		    return null;
		}
		line = line.substring(colon_index1 + 1);
		if (!line.startsWith("Id=")) {
		    return null;
		}
		line = line.substring(3);
		int colon_index2 = line.indexOf(':');
		if (colon_index2 < 1) {
		    return null;
		}
		last_sti.Id = Long.valueOf(line.substring(0, colon_index2));
		int eq_index = line.indexOf('=');
		if (eq_index >= 0) {
		    last_sti.HiddenInfo = line.substring(eq_index + 1).trim();
		}
		return last_sti;
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return null;
    }

    private String replaceDoubleCommas(String sin) {
	int dcIndex = -1;
	while (-1 != (dcIndex = sin.indexOf(",,"))) {
	    String firstPartString = sin.substring(0, dcIndex);
	    String secondPartString = "";
	    if (dcIndex + 2 < sin.length()) {
		secondPartString = sin.substring(dcIndex + 2);
	    }
	    sin = firstPartString + ",(null)," + secondPartString;
	}
	return sin;
    }

    private void LoadPackedFile() {
	try {
	    JFileChooser jchooser = new JFileChooser();
	    String sti_name = null;
	    if (null != this.wtm.getStructureTypeInfo()) {
		StructureTypeInfo sti = this.wtm.getStructureTypeInfo();
		sti_name = sti.getName();
	    }
	    if (null != sti_name) {
		FileNameExtensionFilter fne_filter =
			new FileNameExtensionFilter(sti_name, sti_name);
		jchooser.addChoosableFileFilter(fne_filter);
	    }
	    jchooser.setMultiSelectionEnabled(false);
	    if (null != last_data_string_dir) {
		jchooser.setCurrentDirectory(last_data_string_dir);
	    }
	    jchooser.showOpenDialog(this);
	    File f = jchooser.getSelectedFile();
	    LoadPackedFile(f);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private void SavePackedFile() {
	try {
	    JFileChooser jchooser = new JFileChooser();
	    String sti_name = null;
	    if (null != this.wtm.getStructureTypeInfo()) {
		StructureTypeInfo sti = this.wtm.getStructureTypeInfo();
		sti_name = sti.getName();
	    }
	    FileNameExtensionFilter fne_filter = null;
	    if (null != sti_name) {
		fne_filter =
			new FileNameExtensionFilter(sti_name, sti_name);
		jchooser.addChoosableFileFilter(fne_filter);
	    }
	    jchooser.setMultiSelectionEnabled(false);
	    if (null != last_data_string_dir) {
		jchooser.setCurrentDirectory(last_data_string_dir);
	    }
	    if (JFileChooser.APPROVE_OPTION != jchooser.showSaveDialog(this)) {
		return;
	    }
	    File f = jchooser.getSelectedFile();
	    try {
		if (null != fne_filter &&
			jchooser.getFileFilter().equals(fne_filter) &&
			!f.getName().endsWith("." + sti_name)) {
		    f = new File(f.getParentFile(), f.getName() + "." + sti_name);
		}
	    } catch (Exception e) {
		e.printStackTrace();
	    }
	    SavePackedFile(f);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void LoadPackedFile(File f) {
	try {
	    last_data_string_dir = null;
	    if (null != f) {
		last_data_string_dir = f.getParentFile();
//				diagapplet.utils.DiagError.println("WatchJPanel.LoadPackedFile(): File = " + f.getAbsolutePath());

		PackedFileReader pfr = new PackedFileReader(this.nml_dict, false);
		NMLmsg msg = pfr.ReadFile(f);
		if (null == msg) {
		    JOptionPane.showMessageDialog(this, "File not recognized or could not be read.");
		    return;
		}
		String msgString = pfr.convertMsgToString(msg);
		String sData2 = replaceDoubleCommas(msgString);
		if (sData2.endsWith(",")) {
		    sData2 = sData2.substring(0, sData2.length() - 1);
		}
		StringTokenizer stData = new StringTokenizer(sData2, ",");
		String idString = stData.nextToken().trim();
		Long idLong = Long.valueOf(idString);
		Hashtable ht = this.hashtable_by_id;
		if (null == ht) {
		    JOptionPane.showMessageDialog(this, "Could not load file (" + f.getName() + "). Structure hashtable not initialized.");
		    return;
		}
		StructureTypeInfo sti = (StructureTypeInfo) ht.get(idLong);
		if (null == sti) {
		    JOptionPane.showMessageDialog(this, "Could not load file (" + f.getName() + "). Structure for Id(" + idLong + ")  not found.");
		    return;
		}
		this.SetTypeInfo(sti, wtm.get_structInfoByNameHashtable());
		stData.nextToken(); // throw away size token.
		this.SetDataInfo(stData);
	    }
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    public void SavePackedFile(File f) {
	try {
	    last_data_string_dir = null;
	    if (null != f) {
		last_data_string_dir = f.getParentFile();
//				diagapplet.utils.DiagError.println("WatchJPanel.SavePackedFile(): File = " + f.getAbsolutePath());

		PackedFileWriter pfw = new PackedFileWriter(this.nml_dict, false);
		String ds = this.getDataString();
		String s = null;
		if (ds.startsWith(",")) {
		    s = wtm.getStructureTypeInfo().Id + ",0" + ds;
		} else {
		    s = wtm.getStructureTypeInfo().Id + ",0," + ds;
		}
		NMLmsg msg = pfw.convertStringToMsg(s);
		pfw.WriteFile(f, msg);
	    }
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    private void LoadXMLFile() {
	try {
	    JFileChooser jchooser = new JFileChooser();
	    FileNameExtensionFilter fne_filter =
		    new FileNameExtensionFilter("xml", "xml");
	    jchooser.addChoosableFileFilter(fne_filter);
	    jchooser.setMultiSelectionEnabled(false);
	    if (null != last_data_string_dir) {
		jchooser.setCurrentDirectory(last_data_string_dir);
	    }
	    jchooser.showOpenDialog(this);
	    File f = jchooser.getSelectedFile();
	    LoadXMLFile(f);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private void ShowXMLFile() {
	try {
	    File f = File.createTempFile(wtm.getStructureTypeInfo().getName() + "_", ".xml");
	    SaveXMLFile(f);
	    Desktop.getDesktop().open(f);
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    private void SaveXMLFile() {
	try {
	    JFileChooser jchooser = new JFileChooser();
	    FileNameExtensionFilter fne_filter =
		    new FileNameExtensionFilter("xml", "xml");
	    jchooser.addChoosableFileFilter(fne_filter);
	    jchooser.setMultiSelectionEnabled(false);
	    if (null != last_data_string_dir) {
		jchooser.setCurrentDirectory(last_data_string_dir);
	    }
	    if (JFileChooser.APPROVE_OPTION != jchooser.showSaveDialog(this)) {
		return;
	    }
	    File f = jchooser.getSelectedFile();

	    if (jchooser.getFileFilter().equals(fne_filter) && !f.getName().endsWith(".xml")) {
		f = new File(f.getParentFile(), f.getName() + ".xml");
	    }
	    SaveXMLFile(f);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void LoadXMLFile(File f) {
	try {
	    last_data_string_dir = null;
	    if (null != f) {
		last_data_string_dir = f.getParentFile();
//				diagapplet.utils.DiagError.println("WatchJPanel.LoadXMLFile(): File = " + f.getAbsolutePath());

		XMLFileReader xml_fr = new XMLFileReader(this.nml_dict);

		if (null != JCheckBoxMenuItemXmlAddArrayIndexes) {
		    xml_fr.set_add_array_indexes_to_name(JCheckBoxMenuItemXmlAddArrayIndexes.getState());
		}

		NMLmsg msg = xml_fr.ReadFile(f);
		if (null == msg) {
		    JOptionPane.showMessageDialog(this, "File not recognized or could not be read.");
		    return;
		}
		String msgString = xml_fr.convertMsgToString(msg);
		String sData2 = replaceDoubleCommas(msgString);
		if (sData2.endsWith(",")) {
		    sData2 = sData2.substring(0, sData2.length() - 1);
		}
		StringTokenizer stData = new StringTokenizer(sData2, ",");
		String idString = stData.nextToken().trim();
		Long idLong = Long.valueOf(idString);
		Hashtable ht = this.hashtable_by_id;
		if (null == ht) {
		    JOptionPane.showMessageDialog(this, "Could not load file (" + f.getName() + "). Structure hashtable not initialized.");
		    return;
		}
		StructureTypeInfo sti = (StructureTypeInfo) ht.get(idLong);
		if (null == sti) {
		    JOptionPane.showMessageDialog(this, "Could not load file (" + f.getName() + "). Structure for Id(" + idLong + ")  not found.");
		    return;
		}
		this.SetTypeInfo(sti, wtm.get_structInfoByNameHashtable());
		stData.nextToken(); // throw away size token.
		this.SetDataInfo(stData);
	    }
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    public void SaveXMLFile(File f) {
	try {
	    last_data_string_dir = null;
	    if (null != f) {
		last_data_string_dir = f.getParentFile();
//				diagapplet.utils.DiagError.println("WatchJPanel.SaveXMLFile(): File = " + f.getAbsolutePath());

		XMLFileWriter xml_fw = new XMLFileWriter(this.nml_dict);
		if (null != JCheckBoxMenuItemXmlAddArrayIndexes) {
		    xml_fw.set_add_array_indexes_to_name(JCheckBoxMenuItemXmlAddArrayIndexes.getState());
		}
		String ds = this.getDataString();
		String s = null;
		if (ds.startsWith(",")) {
		    s = wtm.getStructureTypeInfo().Id + ",0" + ds;
		} else {
		    s = wtm.getStructureTypeInfo().Id + ",0," + ds;
		}
		NMLmsg msg = xml_fw.convertStringToMsg(s);
		xml_fw.WriteFile(f, msg);
	    }
	    jpop = null;
	    //JOptionPane.showMessageDialog(this.getParent(), "Text saved to "+tf.getAbsolutePath()+" and err log.");
	} catch (Exception exception) {
	    exception.printStackTrace();
	}
    }

    public void ExpandAll() {
	this.wtm.ExpandAll();
	jpop = null;
    }

    public void CollapseAll() {
	this.wtm.CollapseAll();
	jpop = null;
    }
    private boolean recordAll = true;

    public void setRecordAll(boolean _recordAll) {
	this.recordAll = _recordAll;
    }
    private static JMenu JMenuPreviousMessages = null;
    private JCheckBoxMenuItem JCheckBoxMenuItemXmlAddArrayIndexes = null;

    private void popup_create() {
	jpop = new JPopupMenu();

	final String cur_value = this.GetSelectedValue();
	if (null != cur_value) {
	    final File f = new File(cur_value);
	    if (f.exists() && f.canRead()) {
		JMenuItem jpopOpenFileMenuItem = new JMenuItem("Open " + cur_value);
		jpopOpenFileMenuItem.setToolTipText("Open " + cur_value + " with the viewer/editor appropriate to it's extention as configured for your operating system.");
		jpopOpenFileMenuItem.addActionListener(new ActionListener() {

		    @Override
		    public void actionPerformed(ActionEvent e) {
			try {
			    Desktop.getDesktop().open(f);
			} catch (Exception exception) {
			    exception.printStackTrace();
			}
			jpop = null;
		    }
		});
		jpop.add(jpopOpenFileMenuItem);
	    }
	    JMenuItem jpopCopyMenuItem = new JMenuItem("Copy");
	    jpopCopyMenuItem.setToolTipText("Copy the text \"" + cur_value + "\" to the clipboard.");
	    jpopCopyMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    try {
			Clipboard clipboard = getToolkit().getSystemClipboard();
			clipboard.setContents(new StringSelection(cur_value), null);
		    } catch (Exception exception) {
			exception.printStackTrace();
		    }
		    jpop = null;
		}
	    });
	    jpop.add(jpopCopyMenuItem);
	}
	final StructureTypeInfo sti = this.getStructureTypeInfo();
	if (null != sti) {
	    final String from_header = sti.fromFileName;
	    if (null != from_header) {
		final File header_f = new File(from_header);
		if (header_f.exists() && header_f.canRead()) {
		    JMenuItem jpopOpenHeaderMenuItem = new JMenuItem("Open Header: " + from_header);
		    jpopOpenHeaderMenuItem.setToolTipText("Open the C++ header file that defines this message with the viewer/editor appropriate to it's extention as configured for your operating system.");
		    jpopOpenHeaderMenuItem.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
			    try {
				Desktop.getDesktop().open(header_f);
			    } catch (Exception exception) {
				exception.printStackTrace();
			    }
			    jpop = null;
			}
		    });
		    jpop.add(jpopOpenHeaderMenuItem);
		}
	    }
	}
	JMenuItem jpopDumpTextMenuItem = new JMenuItem("Dump Text");
	jpopDumpTextMenuItem.setToolTipText("Open a text editor and dump expanded variables from this message to it.");
	jpopDumpTextMenuItem.addActionListener(new ActionListener() {

	    @Override
	    public void actionPerformed(ActionEvent e) {
		DumpText();
	    }
	});
	jpop.add(jpopDumpTextMenuItem);

	JMenuItem jpopDumpDataStringMenuItem = new JMenuItem("Dump DataString");
	jpopDumpDataStringMenuItem.addActionListener(new ActionListener() {

	    @Override
	    public void actionPerformed(ActionEvent e) {
		DumpDataString();
	    }
	});
	jpop.add(jpopDumpDataStringMenuItem);
	final boolean editable = this.wtm.get_editable();

	if (editable) {
	    JMenuItem jpopLoadDataStringMenuItem = new JMenuItem("Load DataString");
	    jpopLoadDataStringMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    LoadDataString();
		}
	    });
	    jpop.add(jpopLoadDataStringMenuItem);
	}

	if (null != this.nml_dict && null != this.hashtable_by_id) {
	    if (editable) {
		JMenuItem jpopLoadPackedFileMenuItem = new JMenuItem("Open PackedFile ...");
		jpopLoadPackedFileMenuItem.setToolTipText("Open a PackedMessageFile for any known message type.");
		jpopLoadPackedFileMenuItem.addActionListener(new ActionListener() {

		    @Override
		    public void actionPerformed(ActionEvent e) {
			LoadPackedFile();
		    }
		});
		jpop.add(jpopLoadPackedFileMenuItem);

		JMenuItem jpopLoadXMLFileMenuItem = new JMenuItem("Open XMLFile ...");
		jpopLoadXMLFileMenuItem.setToolTipText("Open a XmlMessageFile for any known message type.");
		jpopLoadXMLFileMenuItem.addActionListener(new ActionListener() {

		    @Override
		    public void actionPerformed(ActionEvent e) {
			LoadXMLFile();
		    }
		});
		jpop.add(jpopLoadXMLFileMenuItem);

		JCheckBoxMenuItem jpopRecordAllMenuCheckboxItem = new JCheckBoxMenuItem("Record all");
		jpopRecordAllMenuCheckboxItem.setSelected(this.recordAll);
		jpopRecordAllMenuCheckboxItem.addActionListener(new ActionListener() {

		    @Override
		    public void actionPerformed(ActionEvent e) {
			JCheckBoxMenuItem jcbmi = (JCheckBoxMenuItem) e.getSource();
			setRecordAll(jcbmi.isSelected());
		    }
		});
	    }

	    JMenuItem jpopsavePackedFileMenuItem = new JMenuItem("Save PackedFile ...");
	    jpopsavePackedFileMenuItem.setToolTipText("Save the current message in a PackedMessageFile.");
	    jpopsavePackedFileMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    SavePackedFile();
		}
	    });
	    jpop.add(jpopsavePackedFileMenuItem);

	    JMenuItem jpopsaveXMLFileMenuItem = new JMenuItem("Save XMLFile ...");
	    jpopsaveXMLFileMenuItem.setToolTipText("Save the current message in a XmlMessageFile.");
	    jpopsaveXMLFileMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    SaveXMLFile();
		}
	    });
	    jpop.add(jpopsaveXMLFileMenuItem);

	    JMenuItem jpopshowXMLFileMenuItem = new JMenuItem("Show XMLFile ...");
	    jpopshowXMLFileMenuItem.setToolTipText("Show the current message in a XmlMessageFile.");
	    jpopshowXMLFileMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    ShowXMLFile();
		}
	    });
	    jpop.add(jpopshowXMLFileMenuItem);

	    if (null == JCheckBoxMenuItemXmlAddArrayIndexes) {
		JCheckBoxMenuItemXmlAddArrayIndexes = new JCheckBoxMenuItem("XML: Add array indexes", true);
	    }
	    jpop.add(JCheckBoxMenuItemXmlAddArrayIndexes);
	}

	if (editable) {
	    JMenuItem jpopClearMenuItem = new JMenuItem("Clear");
	    jpopClearMenuItem.addActionListener(new ActionListener() {

		@Override
		public void actionPerformed(ActionEvent e) {
		    Clear();
		}
	    });
	    jpop.add(jpopClearMenuItem);
	}

	JMenuItem jpopExpandAllMenuItem = new JMenuItem("Expand All");
	jpopExpandAllMenuItem.addActionListener(new ActionListener() {

	    @Override
	    public void actionPerformed(ActionEvent e) {
		ExpandAll();
	    }
	});
	jpop.add(jpopExpandAllMenuItem);

	JMenuItem jpopCollapseAllMenuItem = new JMenuItem("Collapse All");
	jpopCollapseAllMenuItem.addActionListener(new ActionListener() {

	    @Override
	    public void actionPerformed(ActionEvent e) {
		CollapseAll();
	    }
	});
	jpop.add(jpopCollapseAllMenuItem);

	if (null != JMenuPreviousMessages) {
	    jpop.add(JMenuPreviousMessages);
	}
	jpop.addComponentListener(new ComponentAdapter() {

	    @Override
	    public void componentHidden(ComponentEvent e) {
		super.componentHidden(e);
		jpop = null;
	    }
	});

    }

    private void show_popup(java.awt.event.MouseEvent evt) {
	try {
	    if (null == jpop) {
		popup_create();
	    }
	    if (null != JMenuPreviousMessages) {
		StructureTypeInfo stm = wtm.getStructureTypeInfo();
		if (null != stm && null != stm.getName()) {
		    EnableMatchingPreviousMessages(stm.getName() + " :");
		} else {
		    EnableAllPreviousMessages();
		}
	    }
	    if (null != jpop) {
		jpop.show(evt.getComponent(), evt.getX(), evt.getY());
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

private void jScrollPane1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jScrollPane1MouseClicked
    if (evt.isPopupTrigger()) {
	show_popup(evt);
    }
}//GEN-LAST:event_jScrollPane1MouseClicked

private void jTable1MousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jTable1MousePressed
    if (evt.isPopupTrigger()) {
	show_popup(evt);
    }
}//GEN-LAST:event_jTable1MousePressed

private void jScrollPane1MousePressed(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jScrollPane1MousePressed
    if (evt.isPopupTrigger()) {
	show_popup(evt);
    }
}//GEN-LAST:event_jScrollPane1MousePressed
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables
}
