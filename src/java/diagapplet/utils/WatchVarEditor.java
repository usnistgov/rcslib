/*
 * WatchVarEditor.java
 *
 * Created on January 21, 2007, 9:26 AM
 *
 *
 * The NIST RCS (Real-time Control Systems)
 * library is public domain software, however it is preferred
 * that the following disclaimers be attached.
 *
 * Software Copywrite/Warranty Disclaimer
 *
 *   This software was developed at the National Institute of Standards and
 * Technology by employees of the Federal Government in the course of their
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the
 * public domain. NIST Real-Time Control System software is an experimental
 * system. NIST assumes no responsibility whatsoever for its use by other
 * parties, and makes no guarantees, expressed or implied, about its
 * quality, reliability, or any other characteristic. We would appreciate
 * acknowledgement if the software is used. This software can be
 * redistributed and/or modified freely provided that any derivative works
 * bear some notice that they are derived from it, and any modified
 * versions bear some notice that they have been modified.
 *
 */
package diagapplet.utils;

import diagapplet.CodeGen.EnumTypeInfo;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Collections;
import java.util.Vector;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.table.TableCellEditor;

/**
 * Cell Editor for the variables in WatchJPanel table.
 * @author shackle
 */
public class WatchVarEditor
		extends AbstractCellEditor
		implements TableCellEditor, ActionListener, ItemListener {

	private JComboBox jcb;
	private JTextField jtf;
	private boolean is_enum;
	private EnumTypeInfo eti = null;
	private boolean val_was_zero = false;

	/** Creates a new instance of WatchVarEditor */
	public WatchVarEditor() {
	}

	//Implement the one CellEditor method that AbstractCellEditor doesn't.
	@Override
	public Object getCellEditorValue() {
		try {
			if (is_enum) {
				String v = (String) jcb.getSelectedItem();
				try {
					Integer I = (Integer) eti.reverse_hashtable.get(v);
					if (null == I) {
						return v;
					}
					return I.toString();
				} catch (Exception e) {
					e.printStackTrace();
				}
				return v;
			} else {
				jtfSelectAll();
				return jtf.getText();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	// Border as set in Table's default GenericEditor
	Border editorBorder = BorderFactory.createLineBorder(Color.BLACK);
	//Implement the one method defined by TableCellEditor.
	@Override
	public Component getTableCellEditorComponent(JTable table,
			Object value,
			boolean isSelected,
			int row,
			int column) {
		WatchTableModel wtm = (WatchTableModel) table.getModel();
		is_enum = wtm.getIsEnumeration(row);
		if (is_enum) {
			jtf = null;
			eti = wtm.getEnumTypeInfo(row);
			Vector<String> lst = new Vector<String>();
			lst.addAll(eti.hashtable.values());
			Collections.sort(lst);
			jcb = new JComboBox(lst.toArray());
			String vs = (String) value;
			if (!eti.hashtable.contains(vs)) {
				jcb.insertItemAt(vs, 0);
			}
			jcb.setEditable(true);
			jcb.setSelectedItem(vs);
			jcb.getEditor().addActionListener(new ActionListener() {

				@Override
				public void actionPerformed(ActionEvent e) {
					jcb.insertItemAt(jcb.getEditor().getItem(), 0);
					jcb.setSelectedIndex(0);
					fireEditingStopped();
				}
			});
			jcb.addActionListener(this);
			jcb.addItemListener(this);
			return jcb;
		} else {
			jcb = null;
			final String s = value.toString();
			if (s.compareTo("0") == 0) {
				this.val_was_zero = true;
				jtf = new JTextField("");
			} else {
				this.val_was_zero = false;
				jtf = new JTextField(s);
				jtf.setSelectionStart(0);
				jtf.setSelectionEnd(s.length());
			}
			jtf.addActionListener(this);
			Border border = UIManager.getBorder("Table.cellNoFocusBorder");
			if (border == null) {
				border = editorBorder;
			} else {
				// use compound with LAF to reduce "jump" text when starting edits
				border = BorderFactory.createCompoundBorder(editorBorder, border);
			}
			jtf.setBorder(border);
			return jtf;
		}
	}

	private void jtfSelectAll() {
		try {
			if (null != jtf) {
				final String s = jtf.getText();
				if (s.length() == 0 && val_was_zero) {
					jtf.setText("0");
					jtf.setSelectionStart(0);
					jtf.setSelectionEnd(1);
					return;
				}
				jtf.setSelectionStart(0);
				jtf.setSelectionEnd(s.length());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		jtfSelectAll();
		fireEditingStopped();
	}

	@Override
	public void itemStateChanged(ItemEvent e) {
		jtfSelectAll();
		fireEditingStopped();
	}
}
