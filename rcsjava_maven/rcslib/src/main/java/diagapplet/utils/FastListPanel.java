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
import java.awt.*;
import java.awt.event.*;
import rcs.utils.StackTracePrinter;

/**
 * AWT Panel that was used as replacement for AWT List as it could be updated faster than the 
 * older AWT List implementation.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class FastListPanel extends Panel implements MouseListener, ItemSelectable, FastListPanelInterface, KeyListener {

	Vector items = new Vector();
	int num_items = 0;
	int last_line_selected = -1;
	public Vector selected_indexes = null;
	public Vector openStructures = null;
	public boolean newOpenStructure = true;
	public FastListContainer myContainer = null;
	Dimension d;
	int font_height = 10;
	int font_width = 10;
	int selected_index;
	public int lines_visible;
	public int start_line = 0;
	int chars_visible = 10;
	int maxlinelength = 10;
	int char_offset = 0;
	int font_descent = 0;
	int selected_line;
	Color select_color;
	public volatile int count = 0;
	public boolean repaint_needed = true;
	boolean m_useColor = true;
	boolean multipleSelections = false;
	public static boolean debug_on = false;
	public static boolean display_on = true;
	Container trueParent = null;
	public volatile boolean list_changed = true;
	int max_lines_listed = 0;
	FastListPanelItem flpItem = null;
	/**
	 *   Unique id for this class, Exception implements Serializable interface see documentation
	 * for serializable interface for recommendations regarding this variable.
	 */
	private static final long serialVersionUID = 2613899L;

	static public void DebugPrint(String s) {
		try {
			if (!debug_on) {
				return;
			}
			DebugPrint2(s);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static public void DebugPrint2(String s) {
		try {
			Throwable t = new Throwable();
			System.out.println(StackTracePrinter.ThrowableToShortList(t)  + " " + s);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	static public void ErrorPrint(String s) {
		try {
			Throwable t = new Throwable();
			if (debug_on) {
				System.out.println("ErrorPrint + " + StackTracePrinter.ThrowableToShortList(t) + " " + s);
			}
			System.err.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("unchecked")
	public synchronized void AddOpenStructure(String struct) {
		if (null == openStructures) {
			openStructures = new Vector();
		}
		if (debug_on) {
			DebugPrint("AddOpenStructure(" + struct + ")");
		}
		openStructures.addElement(struct);
		list_changed = true;
	}

	public synchronized boolean IsOpenStructure(String struct) {
		if (null == openStructures) {
			return false;
		}
		if (debug_on) {
			DebugPrint("IsOpenStructure(" + struct + ")");
		}
		for (int i = 0; i < openStructures.size(); i++) {
			String openStruct = (String) openStructures.elementAt(i);
			if (openStruct.equals(struct)) {
				return true;
			}
		}
		return false;
	}

	public synchronized void RemoveOpenStructure(String struct) {
		if (null == openStructures) {
			return;
		}
		for (int i = 0; i < openStructures.size(); i++) {
			String openStruct = (String) openStructures.elementAt(i);
			if (openStruct.equals(struct)) {
				openStructures.removeElementAt(i);
				return;
			}
		}
		list_changed = true;
		if (debug_on) {
			DebugPrint("RemoveOpenStructure " + struct);
		}
		return;
	}
	static public Font common_font = null;
	static public int common_font_height = -1;
	static public int common_font_width = -1;

	public String toString() {
		String s = super.toString() + "{\n";
		s += "\tnum_items=" + num_items + ",\n";
		if (null != items) {
			for (int i = 0; i < items.size() && i < num_items; i++) {
				s += "\t\titems.get(" + i + ")=" + items.get(i) + ",\n";
				if (i > 5 && i < num_items - 5) {
					s += "\t\t . . .\n";
					i = num_items - 4;
				}
			}
		}
		s += "}\n";
		return s;
	}

	public FastListPanel(int rows, int cols, boolean set_multipleSelections, Container p) {
		super();
		if (debug_on) {
			Thread.dumpStack();
			DebugPrint("FastListPanel(int rows=" + rows + ", int cols=" + cols + ",  boolean set_multipleSelections=" + set_multipleSelections + ", Container p=" + p + ") called.\n");
		}
		//    enableEvents(ItemEvent.ITEM_STATE_CHANGED);
		start_line = 0;
		selected_line = -1;
		multipleSelections = set_multipleSelections;
		if (multipleSelections) {
			selected_indexes = new Vector();
		}
		try {
			if (display_on) {
				try {
					if (null == common_font) {
						common_font = new Font("Monospaced", Font.PLAIN, 12);
						common_font_height = -1;
						common_font_width = -1;
					}
					if (null != common_font) {
						setFont(common_font);
					}
				} catch (Exception e) {
				}
				trueParent = p;
				if (display_on) {
					CheckForColor();
					if (m_useColor) {
						setBackground(Color.lightGray);
					} else {
						setBackground(Color.white);
					}
				}
				//DebugPrint("FastListPanel("+rows+", "+cols+")");
				font_height = 8;
				lines_visible = rows;
				chars_visible = cols;

				if (display_on) {
					if (null == common_font) {
						common_font = getFont();
						common_font_height = -1;
						common_font_width = -1;
					}
					if (null != common_font &&
							(common_font_width < 1 || common_font_height < 1)) {
						try {
							FontMetrics fm = getFontMetrics(common_font);
							if (debug_on) {
								DebugPrint("fm=" + fm);
							}
							if (null != fm) {
								font_height = fm.getAscent() + fm.getDescent() + 1;
								font_descent = fm.getDescent();
								if (font_height < 8) {
									font_height = 8;
								}
							}
							String testString = "1234567890!@#$%^&*()abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
							int test_string_width = fm.stringWidth(testString);
							font_width = test_string_width / testString.length();
							if (debug_on) {
								DebugPrint("font_width=" + font_width);
							}

							if (font_width < 8) {
								font_width = 8;
							}
						} catch (Exception e) {
							e.printStackTrace();
							font_height = 8;
							font_width = 3;
						}
						common_font_width = font_width;
						common_font_height = font_height;
					} else if (null != common_font) {
						font_height = common_font_height;
						font_width = common_font_width;
					}
					//          font_height += font_height/4;
					if (debug_on) {
						DebugPrint2("font_height = " + font_height);
						DebugPrint2("font_width = " + font_width);
					}
					int new_height = font_height * (rows + 1);
					if (new_height > 1024) {
						new_height = 1024;
					}
					d = getSize();
					if (null != d) {
						lines_visible = (new_height / font_height) - 1;
					}
					int new_width = font_width * (cols + 1);
					if (new_width < 150) {
						new_width = 150;
					}
					if (new_width > 400) {
						new_width = 400;
					}
					d = new Dimension(new_width, new_height);
					if (debug_on) {
						DebugPrint2("d=" + d);
					}
					setSize(new_width, new_height);
					if (m_useColor) {
						select_color = Color.red;
					} else {
						select_color = Color.white;
					}
				}
				addMouseListener(this);
				addKeyListener(this);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void CheckForColor() {
		try {
			if (!display_on) {
				return;
			}
			java.awt.Toolkit tk = java.awt.Toolkit.getDefaultToolkit();
			if (tk == null) {
				return;
			}
			java.awt.image.ColorModel cm = tk.getColorModel();
			if (cm == null) {
				return;
			}
			if (cm.getPixelSize() < 2) {
				m_useColor = false;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public Dimension getPreferredSize() {
		if (debug_on) {
			DebugPrint("d=" + d);
		}
		return d;
	}

	public Dimension getMinimumSize() {
		if (debug_on) {
			DebugPrint("d=" + d);
		}
		return d;
	}

	@SuppressWarnings("unchecked")
	public void add(String s) {
		if (debug_on) {
			DebugPrint("FastListPanel.add(" + s + ") ");
			DebugPrint("\t items=" + items);
			if (null != items) {
				DebugPrint("\t items.size() = " + items.size());
			}
			DebugPrint("\t selected_indexes =" + selected_indexes);
			DebugPrint("\t num_items=" + num_items);
			if (null != selected_indexes) {
				DebugPrint("\t selected_indexes.size() = " + selected_indexes.size());
			}
		}
		if (null != s) {
			synchronized (this) {
				if (num_items == items.size()) {
					flpItem = new FastListPanelItem();
					flpItem.s = s;
					flpItem.var_number = -1;
					items.addElement(flpItem);
					if (s.length() > maxlinelength) {
						maxlinelength = s.length();
					}
				} else {
					flpItem = (FastListPanelItem) items.elementAt(num_items);
					flpItem.s = s;
					flpItem.var_number = -1;
					items.setElementAt(flpItem, num_items);
					if (s.length() > maxlinelength) {
						maxlinelength = s.length();
					}
				}
				num_items++;
				list_changed = true;
			}
			repaint_needed = true;
		}
		if (display_on) {
			if (null != myContainer) {
				myContainer.UpdateScrollbars();
			}
		}
		if (debug_on) {
			DebugPrint("FastListPanel.add(" + s + ") ");
			DebugPrint("\t items=" + items);
			if (null != items) {
				DebugPrint("\t items.size() = " + items.size());
			}
			DebugPrint("\t selected_indexes =" + selected_indexes);
			DebugPrint("\t num_items=" + num_items);
			if (null != selected_indexes) {
				DebugPrint("\t selected_indexes.size() = " + selected_indexes.size());
			}
		}
	}

	public void add(String s, int index) {
		add(s, index, -1);
	}

	@SuppressWarnings("unchecked")
	public void add(String s, int index, int var_number) {
		try {
			if (null == items) {
				items = new Vector();
			}
			if (multipleSelections && selected_indexes == null) {
				selected_indexes = new Vector();
			}
			if (false) {
				DebugPrint2("FastListPanel.add(" + s + ", " + index + ", " + var_number + ") ");
				Thread.dumpStack();
				DebugPrint("\t items=" + items);
				if (null != items) {
					DebugPrint("\t items.size() = " + items.size());
				}
				DebugPrint("\tnum_items=" + num_items);
				DebugPrint("\t selected_indexes =" + selected_indexes);
				if (null != selected_indexes) {
					DebugPrint("\t selected_indexes.size() = " + selected_indexes.size());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		try {
			if (null != s) {
				synchronized (this) {
					if (index >= 0 && index < items.size() && index < num_items) {
						flpItem = new FastListPanelItem();
						flpItem.s = s;
						flpItem.var_number = var_number;
						items.insertElementAt(flpItem, index);
						if (null != selected_indexes) {
							for (int i = 0; i < selected_indexes.size(); i++) {
								int old_index = ((Integer) selected_indexes.elementAt(i)).intValue();
								if (old_index >= index) {
									selected_indexes.setElementAt(old_index + 1, i);
								}
							}
						}
						num_items++;
					} else {
						if (num_items == items.size()) {
							flpItem = new FastListPanelItem();
							flpItem.s = s;
							flpItem.var_number = var_number;
							items.addElement(flpItem);
							if (s.length() > maxlinelength) {
								maxlinelength = s.length();
							}
						} else {
							flpItem = (FastListPanelItem) items.elementAt(num_items);
							flpItem.s = s;
							flpItem.var_number = var_number;
							items.setElementAt(flpItem, num_items);
							if (s.length() > maxlinelength) {
								maxlinelength = s.length();
							}
						}
						num_items++;
					}
					repaint_needed = true;
					if (display_on) {
						if (null != myContainer) {
							myContainer.UpdateScrollbars();
						}
					}
				}
				list_changed = true;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@SuppressWarnings("unchecked")
	public void replaceItem(String s, int index) {
		if (null != s) {
			synchronized (this) {
				if (debug_on) {
					DebugPrint("FastListPanel.replaceItem(" + s + ", " + index + ") ");
				//Thread.dumpStack();
				}

				flpItem = (FastListPanelItem) items.elementAt(index);
				if (null == flpItem) {
					flpItem = new FastListPanelItem();
				}
				flpItem.s = s;
				flpItem.var_number = -1;
				items.setElementAt(flpItem, index);
				if (s.length() > maxlinelength) {
					maxlinelength = s.length();
					if (null != myContainer) {
						myContainer.UpdateScrollbars();
					}
				}
				repaint_needed = true;
			}
			list_changed = true;
		}
	}

	@SuppressWarnings("unchecked")
	public void replaceItem(String s, int index, int var_number) {
		if (null != s) {
			synchronized (this) {
				if (debug_on) {
					DebugPrint("FastListPanel.replaceItem(" + s + ", " + index + ", " + var_number + ") ");
				//Thread.dumpStack();
				}

				flpItem = (FastListPanelItem) items.elementAt(index);
				if (null == flpItem) {
					flpItem = new FastListPanelItem();
				}
				flpItem.s = s;
				flpItem.var_number = var_number;
				items.setElementAt(flpItem, index);
				if (s.length() > maxlinelength) {
					maxlinelength = s.length();
					if (null != myContainer) {
						myContainer.UpdateScrollbars();
					}
				}
				repaint_needed = true;
			}
			list_changed = true;
		}
	}

	public void removeAll() {
		clear();
	}

	public void clear() {
		synchronized (this) {
			//		Thread.dumpStack();
			//items.removeAllElements();
			num_items = 0;
			if (s_array != null) {
				for (int i = 0; i < s_array.length; i++) {
					s_array[i] = "";
				}
			}
			if (b_array != null) {
				for (int i = 0; i < b_array.length; i++) {
					b_array[i] = false;
				}
			}
			if (null != selected_indexes) {
				selected_indexes.removeAllElements();
			}
			if (null != myContainer) {
				myContainer.UpdateScrollbars();
			}
			maxlinelength = 10;
			repaint_needed = true;
			list_changed = true;
		}
	}

	@SuppressWarnings("unchecked")
	public void select(int index) {
		try {
			if (debug_on) {
				DebugPrint("FastListPanel.select(" + index + "): items =" + items);
			//Thread.dumpStack();
			}
			synchronized (this) {
				last_line_selected = index;
				if (!multipleSelections) {
					if (null != items && debug_on) {
						DebugPrint("FastListPanel.select(" + index + "): items.size() =" + items.size());
					}
					selected_line = index;
					if (selected_line < start_line || selected_line - start_line > lines_visible) {
						start_line = selected_line - (selected_line % lines_visible);
					}

					String structName = getSelectedItem();
					if (null != structName) {
						if (structName.startsWith("[+] ")) {
							AddOpenStructure(structName.substring(4));
							newOpenStructure = true;
						} else if (structName.startsWith("[-] ")) {
							RemoveOpenStructure(structName.substring(4));
							newOpenStructure = true;
						}
					}
				} else {
					selected_indexes.addElement(index);
				}
				count++;
				repaint_needed = true;
				if (null == getSelectedItem()) {
					char_offset = 0;
				} else if (char_offset > getSelectedItem().length() - (chars_visible / 2)) {
					char_offset = getSelectedItem().length() - (chars_visible / 2);
				}
				if (char_offset < 0) {
					char_offset = 0;
				}
				if (null != myContainer) {
					myContainer.UpdateScrollbars();
				}
				makeVisible(last_line_selected);
				repaint();
				list_changed = true;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void deselect(int index) {
		try {
			if (debug_on) {
				DebugPrint("FastListPanel.deselect(" + index + "): items =" + items);
				Thread.dumpStack();
			}
			synchronized (this) {
				if (!multipleSelections) {
					if (selected_line == index) {
						selected_line = -1;
					}
				} else {
					for (int i = 0; i < selected_indexes.size(); i++) {
						int s = ((Integer) selected_indexes.elementAt(i)).intValue();
						if (s == index) {
							selected_indexes.removeElementAt(i);
						}
					}
				}
				count++;
				if (display_on) {
					repaint_needed = true;
					repaint();
				}
			}
			list_changed = true;
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public synchronized void makeVisible(int index) {
		if (index < start_line || index - start_line + 1 > lines_visible) {
			if (start_line != (index - (lines_visible / 2))) {
				start_line = index - (lines_visible / 2);
				list_changed = true;
			}
		}
		if (start_line < 0) {
			start_line = 0;
		}
		if (start_line > items.size() - lines_visible + 1) {
			start_line = items.size() - lines_visible + 1;
			if (start_line < 0) {
				start_line = 0;
			}
		}
		int max_line_len = 0;
		for (int i = start_line; i < start_line + lines_visible; i++) {
			String s = getSelectedItem();
			if (null == s) {
				break;
			}
			if (s.length() > max_line_len) {
				max_line_len = s.length();
			}
		}
		if (char_offset > (max_line_len + 1 - chars_visible)) {
			char_offset = (max_line_len + 1 - chars_visible);
		}
		if (char_offset < 0) {
			char_offset = 0;
		}
		if (display_on) {
			if (null != myContainer) {
				myContainer.UpdateScrollbars();
			}
			repaint_needed = true;
			repaint();
		}
	}

	public boolean isSelected(int index) {
		try {
			synchronized (this) {
				if (!multipleSelections) {
					return index >= 0 && index == selected_line;
				} else {
					for (int i = 0; i < selected_indexes.size(); i++) {
						int s = ((Integer) selected_indexes.elementAt(i)).intValue();
						if (s == index) {
							return true;
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	public void mousePressed(MouseEvent evt) {
	}

	public void mouseEntered(MouseEvent evt) {
	}

	public void mouseExited(MouseEvent evt) {
	}

	public void mouseReleased(MouseEvent evt) {
	}

	public void mouseClicked(MouseEvent evt) {
		try {
			synchronized (this) {
				//			int x = evt.getX();
				int y = evt.getY();
				if (selected_line > items.size()) {
					selected_line = -1;
				}
				if (last_line_selected > items.size()) {
					last_line_selected = -1;
				}
				if (start_line > items.size() - lines_visible + 1) {
					start_line = items.size() - lines_visible + 1;
					if (start_line < 0) {
						start_line = 0;
					}
				}
				if (debug_on) {
					DebugPrint("FastListPanel.mousePressed(" + evt + ")");
				}
				if (font_height <= 0) {
					return;
				}
				int new_selected_index = (y - font_height / 2) / font_height + start_line;
				if (debug_on) {
					DebugPrint("new_selected_index = " + new_selected_index);
				}
				if (!multipleSelections) {
					select(new_selected_index);
					if (null != _ItemListener) {
						ItemEvent tempEvt =
								new ItemEvent(
								(ItemSelectable) this, // source
								ItemEvent.ITEM_STATE_CHANGED, // id
								(Object) Integer.valueOf(new_selected_index), // item
								ItemEvent.SELECTED); // state-change
						//DebugPrint("Posting "+tempEvt+" to "+trueParent);
						_ItemListener.itemStateChanged(tempEvt);
					}
				} else {
					if (!isSelected(new_selected_index)) {
						if (evt.isShiftDown() && last_line_selected >= 0 &&
								last_line_selected < getItemCount()) {
							int s_line = last_line_selected + 1;
							int n_line = new_selected_index;
							if (start_line > new_selected_index) {
								n_line = last_line_selected - 1;
								s_line = new_selected_index;
							}
							for (int i = s_line; i <= n_line; i++) {
								select(i);
								if (null != _ItemListener) {
									ItemEvent tempEvt =
											new ItemEvent(
											(ItemSelectable) this, // source
											ItemEvent.ITEM_STATE_CHANGED, // id
											(Object) Integer.valueOf(i), // item
											ItemEvent.SELECTED); // state-change
									//DebugPrint("Posting "+tempEvt+" to "+trueParent);
									_ItemListener.itemStateChanged(tempEvt);
								}
							}
						} else {
							select(new_selected_index);
							if (null != _ItemListener) {
								ItemEvent tempEvt =
										new ItemEvent(
										(ItemSelectable) this, // source
										ItemEvent.ITEM_STATE_CHANGED, // id
										(Object) Integer.valueOf(new_selected_index), // item
										ItemEvent.SELECTED); // state-change
								//DebugPrint("Posting "+tempEvt+" to "+trueParent);
								_ItemListener.itemStateChanged(tempEvt);
							}
						}
					} else {
						deselect(new_selected_index);
						if (null != _ItemListener) {
							ItemEvent tempEvt =
									new ItemEvent(
									(ItemSelectable) this, // source
									ItemEvent.ITEM_STATE_CHANGED, // id
									(Object) Integer.valueOf(new_selected_index), // item
									ItemEvent.DESELECTED); // state-change
							//DebugPrint("Posting "+tempEvt+" to "+trueParent);
							_ItemListener.itemStateChanged(tempEvt);
						}
					}
				}
				if (evt.getClickCount() > 1 && _ActionListener != null) {
					ActionEvent actEvt = new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "DblClick");
					_ActionListener.actionPerformed(actEvt);
				}
				if (display_on) {
					repaint_needed = true;
					repaint();
				}
			}
			list_changed = true;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return;
	}

	public int getSelectedIndex() {
		if (multipleSelections) {
			return -1;
		}
		return selected_line;
	}

	public String getSelectedItem() {
		if (multipleSelections) {
			return null;
		}
		try {
			synchronized (this) {
				if (selected_line < 0 || selected_line >= items.size() || selected_line >= num_items) {
					if (debug_on) {
						DebugPrint("Invalid selected_line = " + selected_line);
					}
					return null;
				}
				flpItem = (FastListPanelItem) items.elementAt(selected_line);
				if (null != flpItem && debug_on) {
					if (null == flpItem.s) {
						DebugPrint("selected_line = " + selected_line);
					} else {
						DebugPrint("selected item = " + flpItem.s);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return flpItem.s;
	}

	public int getSelectedVarNumber() {
		if (multipleSelections) {
			return -1;
		}
		try {
			synchronized (this) {
				if (selected_line < 0 || selected_line >= items.size() || selected_line >= num_items) {
					ErrorPrint("Invalid selected_line = " + selected_line + ", this=" + this.toString());
					return -1;
				}
				flpItem = (FastListPanelItem) items.elementAt(selected_line);
				//System.out.println("flpItem="+flpItem);
				if (null != flpItem && debug_on) {
					if (null == flpItem.s) {
						DebugPrint("selected_line = " + selected_line);
					} else {
						DebugPrint("selected item = " + flpItem.s);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (debug_on) {
			DebugPrint("Selected Var Number = " + flpItem.var_number);
		}
		return flpItem.var_number;
	}

	public int[] getSelectedIndexes() {
		if (!multipleSelections) {
			return null;
		}
		if (null == selected_indexes) {
			return null;
		}
		try {
			synchronized (this) {
				int indexes[] = new int[selected_indexes.size()];
				for (int i = 0; i < selected_indexes.size(); i++) {
					indexes[i] = ((Integer) selected_indexes.elementAt(i)).intValue();
				}
				return indexes;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public String[] getSelectedItems() {
		if (!multipleSelections) {
			return null;
		}
		if (null == selected_indexes) {
			return null;
		}
		try {
			synchronized (this) {
				String selected_items[] = new String[selected_indexes.size()];
				for (int i = 0; i < selected_indexes.size(); i++) {
					selected_items[i] = ((FastListPanelItem) items.elementAt(((Integer) selected_indexes.elementAt(i)).intValue())).s;
				}
				return selected_items;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public Object[] getSelectedObjects() {
		return getSelectedItems();
	}

	public String getItem(int index) {
		String str = null;
		try {
			synchronized (this) {
				if (index < 0) {
					return null;
				}
				flpItem = (FastListPanelItem) items.elementAt(index);
				if (null != flpItem) {
					str = flpItem.s;
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return str;
	}

	public int getVarNumber(int index) {
		int var_num = 0;
		try {
			synchronized (this) {
				if (index < 0) {
					return -1;
				}
				var_num = ((FastListPanelItem) items.elementAt(index)).var_number;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return var_num;
	}

	public int countItems() {
		if (num_items > items.size()) {
			num_items = items.size();
		}
		return num_items;
	}

	public int getItemCount() {
		if (num_items > items.size()) {
			num_items = items.size();
		}
		return num_items;
	}
	String s_array[] = null;
	boolean b_array[] = null;
	Color select_background_color = new Color(0);

	public void paint(Graphics g) {
		try {
			if (!display_on) {
				return;
			}
			repaint_needed = false;
			int i = start_line;
			int j = 0;
			if (null == items) {
				return;
			}
			if (items.size() <= 0) {
				return;
			}
			if (null == s_array || s_array.length != lines_visible) {
				s_array = new String[lines_visible];
				b_array = new boolean[lines_visible];
			}
			if (list_changed) {
				synchronized (this) {
					max_lines_listed = 0;
					for (; i < items.size() && i < num_items && j < lines_visible; i++, j++) {
						s_array[j] = ((FastListPanelItem) items.elementAt(i)).s;
						b_array[j] = isSelected(i);
						max_lines_listed = j;
					}
					list_changed = false;
				}
			}
			for (j = 0; j <= max_lines_listed; j++) {
				if (b_array[j]) {
					g.setColor(select_background_color);
					g.fillRect(0, j * font_height + font_descent + 1, getSize().width, font_height);
					if (multipleSelections) {
						g.setColor(Color.white);
						if (j > 0) {
							g.drawLine(0, j * font_height + font_descent + 1, getSize().width, j * font_height + font_descent + 1);
						}
					}
					g.setColor(select_color);
				} else {
					g.setColor(Color.black);
					if (j > 0) {
						g.drawLine(0, j * font_height + font_descent + 1, getSize().width, j * font_height + font_descent + 1);
					}
				}
				String s = s_array[j];
				if (s != null && s.length() > char_offset && char_offset >= 0) {
					g.drawString(s.substring(char_offset), 5, j * font_height + font_height);
				}
			//DebugPrint(((String)items.elementAt(i).));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
	ItemListener _ItemListener = null;

	public void addItemListener(ItemListener l) {
		_ItemListener = AWTEventMulticaster.add(_ItemListener, l);
	}

	public void removeItemListener(ItemListener l) {
		_ItemListener = AWTEventMulticaster.remove(_ItemListener, l);
	}
	ActionListener _ActionListener = null;

	public void addActionListener(ActionListener l) {
		_ActionListener = AWTEventMulticaster.add(_ActionListener, l);
	}

	public void removeActionListener(ActionListener l) {
		_ActionListener = AWTEventMulticaster.remove(_ActionListener, l);
	}

	/*
	public void processEvent(AWTEvent e)
	{
	// when event occurs which causes "action" semantic
	if (_ItemListener != null && e.getId() == ItemEvent.ITEM_STATE_CHANGED)
	{
	_ItemListener.itemStateChanged((ItemEvent) e);
	return;
	}
	super.processEvent(e);

	}
	 */
	public void keyPressed(KeyEvent e) {
		handleKeyEvent(e);
	}

	public void keyReleased(KeyEvent e) {
		//handleKeyEvent(e);
	}

	public void keyTyped(KeyEvent e) {
		//handleKeyEvent(e);
	}

	public void handleKeyEvent(KeyEvent e) {
		try {
			if (debug_on) {
				DebugPrint("e = " + e + ", e.getKeyChar()=" + e.getKeyChar() + ", KeyEvent.VK_F1=" + KeyEvent.VK_F1 + ", KeyEvent.VK_F7=" + KeyEvent.VK_F7);
			}
			switch (e.getKeyCode()) {
				case KeyEvent.VK_DOWN:
					if (getSelectedIndex() < getItemCount()) {
						select(getSelectedIndex() + 1);
					}
					break;

				case KeyEvent.VK_UP:
					if (getSelectedIndex() > 0) {
						select(getSelectedIndex() - 1);
					}
					break;
			}
		} catch (Exception excep) {
			excep.printStackTrace();
		}
	}
}
