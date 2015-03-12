/*
 * ColorEditor.java
 *
 * Created on January 3, 2007, 5:33 PM
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
package diagapplet.plotter;

/**
 *
 * @author shackle
 */
/* 
 * ColorEditor.java (compiles with releases 1.3 and 1.4) is used by 
 * TableDialogEditDemo.java.
 * 
 *Copied from:
 * http://java.sun.com/docs/books/tutorial/uiswing/components/table.html
 */
import javax.swing.AbstractCellEditor;
import javax.swing.table.TableCellEditor;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JTable;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

class ColorEditor extends AbstractCellEditor
		implements TableCellEditor {

	Color currentColor;
	JButton button;
	JColorChooser colorChooser;
	JDialog dialog=null;

	protected static final String EDIT = "edit";

	public ColorEditor() {
		//Set up the editor (from the table's point of view),
		//which is a button.
		//This button brings up the color chooser dialog,
		//which is the editor from the user's point of view.
		java.awt.EventQueue.invokeLater(new Runnable() {

			@Override
			public void run() {
				button = new JButton();
				button.setActionCommand(EDIT);
				button.addActionListener(new ActionListener() {

					/**
					 * Handles events from the editor button and from
					 * the dialog's OK button.
					 */
					@Override
					public void actionPerformed(ActionEvent e) {
						if (EDIT.equals(e.getActionCommand())) {
							//The user has clicked the cell, so
							//bring up the dialog.
							button.setBackground(currentColor);
							//Set up the dialog that the button brings up.
							if(null == colorChooser) {
								colorChooser = new JColorChooser();
							}
							colorChooser.setColor(currentColor);
							if (null == dialog) {
								dialog = JColorChooser.createDialog(button,
										"Pick a Color",
										true, //modal
										colorChooser,
										this, //OK button handler
										null); //no CANCEL button handler
							}
							dialog.setVisible(true);

							//Make the renderer reappear.
							fireEditingStopped();

						} else { //User pressed dialog's "OK" button.
							if(null != colorChooser)
							{
								currentColor = colorChooser.getColor();
							}
							if(null != dialog)
							{
								dialog.setVisible(false);
								dialog.dispose();
								dialog=null;
							}
							colorChooser=null;
						}
					}
				});
				button.setBorderPainted(false);
			}
		});
	}

	//Implement the one CellEditor method that AbstractCellEditor doesn't.
	public Object getCellEditorValue() {
		return currentColor;
	}

	//Implement the one method defined by TableCellEditor.
	public Component getTableCellEditorComponent(JTable table,
			Object value,
			boolean isSelected,
			int row,
			int column) {
		currentColor = (Color) value;
		return button;
	}
}
