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
package diagapplet;

import java.awt.*;
import java.awt.event.*;
import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.util.StringTokenizer;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.StructureTypeInfo;
import diagapplet.CodeGen.STI_TokenizerInterface;
import diagapplet.utils.URLLoadInfoPanelInterface;
import java.awt.geom.AffineTransform;
import java.awt.print.Printable;
import java.awt.print.PrinterJob;
import javax.swing.SwingWorker;

class HierarchyDraw
        // implements MouseListener, MouseMotionListener,
        implements ComponentListener, Printable {

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613931L;
    public volatile diagapplet.diag_update_interface diag_update_object = null;
    public Component component_to_draw_on = null;
    ModuleInfo moduleShowingCommands = null;
    static public boolean debug_on = false;
    static public boolean updating_hierarchy = true;
    static public boolean interrupt_loading = false;
    public boolean force_next_repaint = true;
    public volatile boolean painting = false;
    public int highlighted_command;
    public long new_threshold = 1500;
    public static Vector<Vector<ModuleInfo>> modules_by_generation = null;
    public static Vector<ModuleInfo> modules = null;
    public int max_number_in_generation = 0;
    public int max_generations = -1;
    public int current_module = 0;
    public int scroll_x = 0;
    public int scroll_y = 0;
    public int lastx = -1;
    public int lasty = -1;
    public int min_y = -1;
    public int max_y = -1;
    public int min_x = -1;
    public int max_x = -1;
    private int last_min_x = -2;
    private int last_max_x = -2;
    private int last_min_y = -2;
    private int last_max_y = -2;
    public boolean use_color = true;
    public static int MODULE_WIDTH = 160;
    public static int MODULE_HEIGHT = 60;
    public static int MODULE_XOFFSET = -10;
    public static int MODULE_YOFFSET = -15;
    public static int MODULE_X_SPACING = 50;
    public static int MODULE_Y_SPACING = 40;
    public static int MAX_HEIGHT = 1200;
    public static int MAX_WIDTH = 1500;
    public static int MIN_HEIGHT = 100;
    public static int MIN_WIDTH = 100;
    public static boolean partial_paint = false;
    volatile int paint_hierarchy_count = 0;
    volatile int mouse_clicked_count = 0;
    volatile int last_mouse_clicked_count = 0;
    volatile int mouse_clicked_x = 0;
    volatile int mouse_clicked_y = 0;
    volatile int mouse_pressed_count = 0;
    volatile int last_mouse_pressed_count = 0;
    volatile int mouse_pressed_x = 0;
    volatile int mouse_pressed_y = 0;
    volatile int mouse_released_count = 0;
    volatile int last_mouse_released_count = 0;
    volatile int mouse_released_x = 0;
    volatile int mouse_released_y = 0;
    volatile int mouse_dragged_count = 0;
    volatile int last_mouse_dragged_count = 0;
    volatile int mouse_dragged_x = 0;
    volatile int mouse_dragged_y = 0;
    Dimension d = null;
    Image buffer_image = null;
    public static boolean use_buffer_image = false;
    public diagapplet.utils.FastListPanelInterface modulesList = null;
    public Scrollbar horzScrollbar = null;
    public Scrollbar vertScrollbar = null;
    public boolean design_mode = false;
    public int repaint_count = 10;
    public boolean list_modules_by_number = false;
    protected diagapplet.utils.CountList modulesCountList = null;

    static public void ErrorPrint(String s) {
        try {
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            if (debug_on) {
                System.out.println("ErrorPrint + " + ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
            }
            System.err.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
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
            StackTraceElement ste[] = t.getStackTrace();
            System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static public void DebugPrint2(String s) {
        try {
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void setCountList(diagapplet.utils.CountList cli) {
        modulesCountList = cli;
    }

    public diagapplet.utils.CountListInterface getCountList() {
        return modulesCountList;
    }

    int getModulesListItemCount() {
        if (null != modulesList) {
            return modulesList.getItemCount();
        } else if (null != modulesCountList) {
            return modulesCountList.getItemCount();
        }
        return 0;
    }

    String getModulesListItem(int k) {
        if (null != modulesList) {
            return modulesList.getItem(k);
        } else if (null != modulesCountList) {
            return modulesCountList.getItem(k);
        }
        return null;
    }

    public String getModulesListSelectedItem() {
        if (null != modulesList) {
            return modulesList.getSelectedItem();
        } else if (null != modulesCountList) {
            return modulesCountList.getSelectedItem();
        }
        return null;
    }

    public void monitored_repaint() {
        try {
            if (debug_on) {
                Thread.dumpStack();
            }
            component_to_draw_on.repaint();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public ModuleInfo getModuleByPosition(int x, int y) {
        try {
            Rectangle rect = component_to_draw_on.getBounds();

            int xoffset = 0; //rect.x -scroll_x;
            int yoffset = 0; //rect.y -scroll_y;
            if (null != modules) {
                for (ModuleInfo module : modules) {
                    if (x > xoffset + module.x + MODULE_XOFFSET
                            && x < xoffset + module.x + MODULE_XOFFSET + MODULE_WIDTH
                            && y > yoffset + module.y + MODULE_YOFFSET
                            && y < yoffset + module.y + MODULE_YOFFSET + MODULE_HEIGHT) {
                        return module;
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public void clear() {
        if (null != modules_by_generation) {
            modules_by_generation.removeAllElements();
            modules_by_generation = null;
        }
        if (null != modules) {
            Enumeration moduleEnumeration = modules.elements();
            while (moduleEnumeration.hasMoreElements()) {
                ModuleInfo modInfo = (ModuleInfo) moduleEnumeration.nextElement();
                modInfo.generation = 0;
                modInfo.max_children_in_generation = 0;
                modInfo.x = 0;
                modInfo.y = 0;
                modInfo.parent = null;
                modInfo.positioned = false;
            }
            modules.removeAllElements();
            modules = null;
        }
        reset();
        System.runFinalization();
        modules = new Vector<ModuleInfo>();
        modules_by_generation = new Vector<Vector<ModuleInfo>>();
    }

    public HierarchyDraw(Component _c) {
        component_to_draw_on = _c;
//	component_to_draw_on.addMouseListener(this);
//	component_to_draw_on.addMouseMotionListener(this);
        component_to_draw_on.addComponentListener(this);
        reset();
    }

    public void reset() {
        max_number_in_generation = 0;
        max_generations = -1;
        current_module = 0;
        scroll_x = 0;
        scroll_y = 0;
        lastx = -1;
        lasty = -1;
        min_y = -1;
        max_y = -1;
        min_x = -1;
        max_x = -1;
    }

    public Dimension getPreferredSize() {
        return d;
    }

    public Dimension getMinimumSize() {
        return d;
    }

//    public void mouseEntered(MouseEvent evt)
//    {
//    }
//    public void mouseExited(MouseEvent evt)
//    {
//    }
    public int computeMaxY() {
        try {
            int maxy = d.height * 2;
            ModuleInfo maxy_module = null;
            if (null != modules) {
                for (int i = 0; i < modules.size(); i++) {
                    ModuleInfo module = modules.elementAt(i);
                    if (maxy < module.y) {
                        maxy = module.y;
                        if (debug_on) {
                            maxy_module = module;
                        }
                    }
                }
            }
            maxy += 2 * Math.abs(MODULE_YOFFSET) + 2 * Math.abs(MODULE_HEIGHT);
            if (debug_on) {
                DebugPrint("HierarchyPanel.computeMaxY() returning " + maxy + " : maxy_module=" + maxy_module + ".");
            }
            return maxy;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return max_y;
    }

    public int computeMaxX() {
        try {
            if (null == d) {
                d = component_to_draw_on.getPreferredSize();
            }
            int maxx = d.width * 2;
            ModuleInfo maxx_module = null;
            if (null != modules) {
                for (int i = 0; i < modules.size(); i++) {
                    ModuleInfo module = modules.elementAt(i);
                    if (maxx < module.x) {
                        if (debug_on) {
                            maxx_module = module;
                        }
                        maxx = module.x;
                    }
                }
            }
            maxx += 2 * Math.abs(MODULE_XOFFSET) + 2 * Math.abs(MODULE_WIDTH);
            if (debug_on) {
                DebugPrint("HierarchyPanel.computeMaxX() returning " + maxx + " : maxx_module=" + maxx_module + ".");
            }
            return maxx;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return max_x;
    }

    
    long last_notify_millis = 0;
    long last_paint_millis = 0;
    SwingWorker modulesUpdaterSwingWorker = null;
    private boolean modules_cmd_status_updated = false;

    public boolean UpdateDisplay(boolean force_repaint) {
        ModuleInfo moduleToUpdate;
        boolean statFound = false;
        boolean negative_one_status_found = false;
        boolean repaint_needed = force_repaint;
        int start_module;
        int modules_checked = 0;
//	if(diagappletMain.sending_command)
//	    {
//		try
//		    {
//			Thread.sleep(10);
//		    }
//		catch(Exception e)
//		    {
//		    }
//		return false;
//	    }
        if (repaint_count > 0) {
            repaint_needed = force_repaint = true;
        }
//	if(mouse_pressed_count > mouse_released_count &&
//	   mouse_pressed_count > last_mouse_pressed_count)
//	    {
//		handle_mousePressed();
//	    }
//	else if(mouse_released_count > last_mouse_released_count)
//	    {
//		handle_mouseReleased();
//	    }
//	else if(mouse_dragged_count > last_mouse_dragged_count)
//	    {
//		handle_mouseDragged();
//	    }
//	else if(mouse_clicked_count > last_mouse_clicked_count)
//	    {
//		handle_mouseClicked();
//	    }

        if (null != modules && !modules_cmd_status_updated) {
            if (modulesUpdaterSwingWorker != null && !modulesUpdaterSwingWorker.isCancelled() && !modulesUpdaterSwingWorker.isDone()) {
                return false;
            }
            modulesUpdaterSwingWorker = new SwingWorker<Void, Void>() {

                @Override
                public Void doInBackground() {
                    try {
                        for (ModuleInfo mi : modules) {
                            if (!mi.is_connected) {
                                continue;
                            }
                            if (!component_to_draw_on.isVisible()) {
                                return null;
                            }
                            mi.updateCmdData();
                            Thread.sleep(30);
                            if (!component_to_draw_on.isVisible()) {
                                return null;
                            }
                            mi.updateStatData();
                            Thread.sleep(30);
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                    return null;
                }

                @Override
                protected void done() {
                    modules_cmd_status_updated = true;
                }
            };
            modulesUpdaterSwingWorker.execute();
        }

        if (null == modules) {
            if (painting) {
                return repaint_needed;
            }
            if (repaint_needed) {
                Graphics cur_g = component_to_draw_on.getGraphics();
                if (null != cur_g) {
                    paintHierarchy(cur_g);
                    if (!painting) {
                        try {
                            synchronized (cur_g) {
                                cur_g.notifyAll();
                            }
                        } catch (Exception eNotify) {
                        }
                    }
                } else {
                    component_to_draw_on.repaint();
                }
            }
            return repaint_needed;
        }
        if (modules.size() < 1) {
            if (painting) {
                return repaint_needed;
            }
            if (repaint_needed) {
                Graphics cur_g = component_to_draw_on.getGraphics();
                if (null != cur_g) {
                    paintHierarchy(cur_g);
                    if (!painting) {
                        try {
                            synchronized (cur_g) {
                                cur_g.notifyAll();
                            }
                        } catch (Exception eNotify) {
                        }
                    }
                } else {
                    component_to_draw_on.repaint();
                }
            }
            return repaint_needed;
        }
        if (current_module > 0) {
            start_module = current_module - 1;
        } else {
            start_module = modules.size() - 1;
        }

        if (debug_on) {
            DebugPrint("HierarchyPanel.UpdateDisplay() called with lastx=" + lastx + ",scroll_x=" + scroll_x + ",lasty=" + lasty + ",scroll_y=" + scroll_y + ", size=" + component_to_draw_on.getSize());
        }
        if (lastx != scroll_x) {
            repaint_needed = true;
            lastx = scroll_x;
        }
        if (lasty != scroll_y) {
            repaint_needed = true;
            lasty = scroll_y;
        }

        while ((current_module != start_module || modules_checked < 1)) {
            try {
                if (debug_on) {
                    DebugPrint("in-loop: current_module=" + current_module + ", start_module=" + start_module + ",modules_checked=" + modules_checked + ", repaint_needed=" + repaint_needed);
                }
                current_module++;
                modules_checked++;
                if (current_module >= modules.size()) {
                    current_module = 0;
                }
                if (current_module < 0) {
                    current_module = 0;
                }
                moduleToUpdate = modules.elementAt(current_module);
                if (null == moduleToUpdate) {
                    break;
                }
                if (!moduleToUpdate.is_connected) {
                    continue;
                }
                if (debug_on) {
                    DebugPrint("HierarchyPanel.update(): moduleToUpdate.Name = " + moduleToUpdate.Name);
                    DebugPrint("HierarchyPanel.update(): updateCmdData()");
                }
//		if(moduleToUpdate != moduleShowingCommands)
//		{
//		    moduleToUpdate.updateCmdData();
//		}
                if (debug_on) {
                    DebugPrint("HierarchyPanel.update(): updateStatData()");
                }
//		moduleToUpdate.updateStatData();
                long cur_time = System.currentTimeMillis();
                moduleToUpdate.new_command = false;
                if (moduleToUpdate.last_serial_number_displayed != moduleToUpdate.serial_number) {
                    repaint_needed = true;
                    moduleToUpdate.last_new_command_time = cur_time;
                    moduleToUpdate.new_command = true;
                }
                if (cur_time - moduleToUpdate.last_new_command_time < new_threshold && cur_time >= moduleToUpdate.last_new_command_time) {
                    moduleToUpdate.new_command = true;
                }
                if (moduleToUpdate.new_command != moduleToUpdate.new_command_drawn) {
                    repaint_needed = true;
                }
                moduleToUpdate.new_status = false;
                if (moduleToUpdate.last_echo_serial_number_displayed != moduleToUpdate.echo_serial_number) {
                    repaint_needed = true;
                    moduleToUpdate.last_new_stat_time = cur_time;
                    moduleToUpdate.new_status = true;
                }
                if (cur_time - moduleToUpdate.last_new_stat_time < new_threshold && cur_time >= moduleToUpdate.last_new_stat_time) {
                    moduleToUpdate.new_status = true;
                }
                if (moduleToUpdate.new_status != moduleToUpdate.new_status_drawn) {
                    //DebugPrint("module = "+moduleToUpdate.Name+", new_status = "+moduleToUpdate.new_status+", new_status_drawn = "+moduleToUpdate.new_status_drawn);
                    //DebugPrint("module = "+moduleToUpdate.Name+", last_new_stat_time = "+moduleToUpdate.last_new_stat_time+", cur_time = "+cur_time+", diff ="+(cur_time-moduleToUpdate.last_new_stat_time));
                    repaint_needed = true;
                }
                if (null != moduleToUpdate.cmdData) {
                    int index = moduleToUpdate.cmdData.indexOf(',');
                    if (index > 0) {
                        String idString = moduleToUpdate.cmdData.substring(0, index);
                        while (true) {
                            if (idString.length() < 1) {
                                break;
                            }
                            if (idString.charAt(0) != ' '
                                    && idString.charAt(0) != '+') {
                                break;
                            }
                            idString = idString.substring(1);
                        }
                        Long idLong = Long.valueOf(rcs.utils.StrToLong.convert(idString));
                        if (moduleToUpdate != null
                                && ModuleInfo.m_cmd_structInfoHashTable != null) {
                            StructureTypeInfo cmdInfo =
                                    (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(idLong);
                            if (cmdInfo.conflicts) {
                                if (null != moduleToUpdate
                                        && null != moduleToUpdate.get_conflict_m_structInfoHashTable()
                                        && null != moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong)) {
                                    cmdInfo = (StructureTypeInfo) moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong);
                                }
                            }
                            if (null != cmdInfo) {
                                moduleToUpdate.cmd_msg_type = idLong.longValue();
                                if (null != cmdInfo.getName()) {
                                    if (null == moduleToUpdate.lastCmdName) {
                                        moduleToUpdate.lastCmdName = cmdInfo.getName();
                                        repaint_needed = true;
                                    } else if (0 != cmdInfo.getName().compareTo(moduleToUpdate.lastCmdName)) {
                                        moduleToUpdate.lastCmdName = cmdInfo.getName();
                                        repaint_needed = true;
                                    }
                                } else {
                                    if (null == moduleToUpdate.lastCmdName) {
                                        moduleToUpdate.lastCmdName = idString;
                                        repaint_needed = true;
                                    } else if (0 != idString.compareTo(moduleToUpdate.lastCmdName)) {
                                        moduleToUpdate.lastCmdName = idString;
                                        repaint_needed = true;
                                    }
                                }

                            } else {
                                if (null == moduleToUpdate.lastCmdName) {
                                    moduleToUpdate.lastCmdName = idString;
                                    repaint_needed = true;
                                } else if (0 != idString.compareTo(moduleToUpdate.lastCmdName)) {
                                    moduleToUpdate.lastCmdName = idString;
                                    repaint_needed = true;
                                }
                            }
                        }
                    }
                }

                // STATUS
                if (null != moduleToUpdate.statData) {
                    negative_one_status_found = false;
                    int dcIndex = -1;
                    while (-1 != (dcIndex = moduleToUpdate.statData.indexOf(",,"))) {
                        String firstPartString = moduleToUpdate.statData.substring(0, dcIndex);
                        String secondPartString = "";
                        if (dcIndex + 2 < moduleToUpdate.statData.length()) {
                            secondPartString = moduleToUpdate.statData.substring(dcIndex + 2);
                        }
                        moduleToUpdate.statData = firstPartString + ",(null)," + secondPartString;
                    }
                    int index = moduleToUpdate.statData.indexOf(',');
                    if (index > 0) {
                        String idString = moduleToUpdate.statData.substring(0, index);
                        while (true) {
                            if (idString.length() < 1) {
                                break;
                            }
                            if (idString.charAt(0) != ' '
                                    && idString.charAt(0) != '+') {
                                break;
                            }
                            idString = idString.substring(1);
                        }
                        Long idLong = Long.valueOf(0);
                        try {
                            idLong = Long.valueOf(rcs.utils.StrToLong.convert(idString));
                        } catch (Exception e) {
                            System.err.println("Invalid idString in statData " + moduleToUpdate.statData);
                            e.printStackTrace();
                            break;
                        }
                        if (null == ModuleInfo.m_stat_structInfoHashTable) {
                            ModuleInfo.m_stat_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
                        }
                        StructureTypeInfo statInfo = (StructureTypeInfo) ModuleInfo.m_stat_structInfoHashTable.get(idLong);
                        if (null != statInfo) {
                            if (statInfo.conflicts) {
                                if (null != moduleToUpdate
                                        && null != moduleToUpdate.get_conflict_m_structInfoHashTable()
                                        && null != moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong)) {
                                    statInfo = (StructureTypeInfo) moduleToUpdate.get_conflict_m_structInfoHashTable().get(idLong);
                                }
                            }
                            moduleToUpdate.stat_msg_type = idLong.longValue();
                            moduleToUpdate.status_type_name = statInfo.getName();
                            StringTokenizer dataTokenizer =
                                    new StringTokenizer(moduleToUpdate.statData, ",");
                            statInfo.startInfoTokens();
                            String dataString = dataTokenizer.nextToken();
                            dataString = dataTokenizer.nextToken();
                            if (debug_on) {
                                DebugPrint("HierarchyPanel.update(): statInfo.RawInfo = " + statInfo.RawInfo);
                                DebugPrint("HierarchyPanel.update(): statInfo.HiddenInfo = " + statInfo.HiddenInfo);
                                DebugPrint("HierarchyPanel.update(): moduleToUpdate.statData = " + moduleToUpdate.statData);
                            }
                            STI_TokenizerInterface stiti = statInfo.getInfoTokenizer();
                            while (dataTokenizer.hasMoreTokens() && stiti.hasMoreTokens() && !statFound) {
                                dataString = dataTokenizer.nextToken();
                                if (dataString == null) {
                                    break;
                                }
                                String infoString = stiti.nextToken();
                                if (infoString == null) {
                                    break;
                                }
                                if (infoString.endsWith(" status") || infoString.endsWith(".status")) {
                                    if (debug_on) {
                                        DebugPrint("HierarchyPanel.update(): infoString = " + infoString + ", dataString = " + dataString);
                                    }
                                    while (true) {
                                        if (dataString.length() < 1) {
                                            break;
                                        }
                                        if (dataString.charAt(0) != ' '
                                                && dataString.charAt(0) != '+') {
                                            break;
                                        }
                                        dataString = dataString.substring(1);
                                    }
                                    Long statLong = Long.valueOf(0);
                                    try {
                                        statLong = Long.valueOf(rcs.utils.StrToLong.convert(dataString));
                                        switch (statLong.intValue()) {
                                            case -1:
                                                negative_one_status_found = true;
                                                statFound = false;
                                                break;

                                            case 0:
                                                //DebugPrint("HierarchyPanel.update(): this status variable is 0 so keep looking.");
                                                statFound = false;
                                                break;
                                            case 1:
                                                if (null == moduleToUpdate.lastStatName) {
                                                    moduleToUpdate.lastStatName = "DONE";
                                                    repaint_needed = true;
                                                } else if (0 != moduleToUpdate.lastStatName.compareTo("DONE")) {
                                                    moduleToUpdate.lastStatName = "DONE";
                                                    repaint_needed = true;
                                                }
                                                statFound = true;
                                                break;

                                            case 2:
                                                if (null == moduleToUpdate.lastStatName) {
                                                    moduleToUpdate.lastStatName = "EXEC";
                                                    repaint_needed = true;
                                                } else if (0 != moduleToUpdate.lastStatName.compareTo("EXEC")) {
                                                    moduleToUpdate.lastStatName = "EXEC";
                                                    repaint_needed = true;
                                                }
                                                statFound = true;
                                                break;

                                            case 3:
                                                if (null == moduleToUpdate.lastStatName) {
                                                    moduleToUpdate.lastStatName = "ERROR";
                                                    repaint_needed = true;
                                                } else if (0 != moduleToUpdate.lastStatName.compareTo("ERROR")) {
                                                    moduleToUpdate.lastStatName = "ERROR";
                                                    repaint_needed = true;
                                                }
                                                statFound = true;
                                                break;
                                            default:
                                                statFound = true;
                                                if (null == moduleToUpdate.lastStatName) {
                                                    moduleToUpdate.lastStatName = dataString;
                                                    repaint_needed = true;
                                                } else if (0 != moduleToUpdate.lastStatName.compareTo(dataString)) {
                                                    moduleToUpdate.lastStatName = dataString;
                                                    repaint_needed = true;
                                                }
                                                break;
                                        }
                                    } catch (Exception e) {
                                        e.printStackTrace();
                                    }
                                }
                            }
                            if (negative_one_status_found && !statFound) {
                                if (null == moduleToUpdate.lastStatName) {
                                    moduleToUpdate.lastStatName = "UNINITIALIZED";
                                    repaint_needed = true;
                                } else if (0 != moduleToUpdate.lastStatName.compareTo("UNINITIALIZED")) {
                                    moduleToUpdate.lastStatName = "UNINITIALIZED";
                                    repaint_needed = true;
                                }
                            }
                        } else {
                            String temp;
                            temp = "unknown NML msg(" + idString + ")";
                            if (null == moduleToUpdate.lastStatName) {
                                moduleToUpdate.lastStatName = temp;
                                repaint_needed = true;
                            } else if (0 != moduleToUpdate.lastStatName.compareTo(temp)) {
                                moduleToUpdate.lastStatName = temp;
                                repaint_needed = true;
                            }
                        }
                    }
                }
            } catch (Exception e) {
                System.err.println("Error updating module = " + current_module);
                e.printStackTrace();
                break;
            }
        }
        if (debug_on) {
            DebugPrint("finished: current_module=" + current_module + ", start_module=" + start_module + ",modules_checked=" + modules_checked);
            DebugPrint("repaint_needed=" + repaint_needed + ", painting=" + painting + ", last_notify_millis="
                    + (System.currentTimeMillis() - last_notify_millis)
                    + ", last_paint_millis=" + (System.currentTimeMillis() - last_paint_millis));
            DebugPrint("max_x=" + max_x + ", max_y=" + max_y);
        }
        if (max_x != last_max_x || max_y != last_max_y) {
            component_to_draw_on.setPreferredSize(new Dimension(max_x, max_y));
            component_to_draw_on.invalidate();
        }
        last_min_x = min_x;
        last_max_x = max_x;
        last_min_y = min_y;
        last_max_y = max_y;
        modules_cmd_status_updated = false;

        if (painting) {
            return repaint_needed;
        }
        if (repaint_needed
                || (System.currentTimeMillis() - last_notify_millis > 100)
                || (System.currentTimeMillis() - last_paint_millis > 5000)) {
            if (force_repaint || force_next_repaint
                    || (System.currentTimeMillis() - last_paint_millis > 5000)) {
                component_to_draw_on.repaint();
            } else {
                Graphics cur_g = component_to_draw_on.getGraphics();
                if (null != cur_g) {
                    partial_paint =
                            ((repaint_needed && paint_hierarchy_count % 5 != 0)
                            || (moduleShowingCommands != null));
                    if (debug_on) {
                        DebugPrint("partial_paint=" + partial_paint);
                    }
                    paintHierarchy(cur_g);
                    partial_paint = false;
                    if (System.currentTimeMillis() - last_notify_millis > 100 && !painting) {
                        try {
                            synchronized (cur_g) {
                                cur_g.notifyAll();
                            }
                            last_notify_millis = System.currentTimeMillis();
                        } catch (Exception eNotify) {
                        }
                    }
                } else {
                    component_to_draw_on.repaint();
                }
            }
        }
        return repaint_needed;
    }
    public Frame parentFrame = null;
    boolean printing = false;

    public void print() {
        int orig_scroll_x = this.scroll_x;
        int orig_scroll_y = this.scroll_y;
        boolean orig_use_color = this.use_color;
        boolean orig_printing = this.printing;
        try {
            printing=true;
            PrinterJob job = PrinterJob.getPrinterJob();
            if (null == job) {
                DebugPrint("PrintJob is null. (Canceled?)");
                return;
            }
            job.setPrintable(this);
            boolean doPrint = job.printDialog();
            if (doPrint) {
                job.print();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            scroll_x = orig_scroll_x;
            scroll_y = orig_scroll_y;
            this.use_color = orig_use_color;
            this.printing = orig_printing;
        }
    }
    Dimension last_temp_d = null;
    public boolean need_clear = true;

    public void paint(Graphics g) {
        try {
            partial_paint = false;
            if (false) {
                Dimension dp = component_to_draw_on.getPreferredSize();
                Dimension ds = component_to_draw_on.getSize();
                Rectangle rect = component_to_draw_on.getBounds();
                //Thread.dumpStack();
                System.err.println("clearing :  dp=" + dp + ", ds=" + ds + ", rect=" + rect);
                int wmax = (int) dp.getWidth();
                if ((int) ds.getWidth() > wmax) {
                    wmax = (int) ds.getWidth();
                }
                int hmax = (int) dp.getHeight();
                if ((int) ds.getHeight() > wmax) {
                    hmax = (int) ds.getHeight();
                }

                if (hmax < rect.height) {
                    hmax = rect.height;
                }
                if (wmax < rect.width) {
                    wmax = rect.width;
                }
                g.setColor(component_to_draw_on.getBackground());
                g.fillRect(0, 0, wmax, hmax);
                need_clear = false;
            }
            paintHierarchy(g);
            if (null != modules && modules.size() > 0) {
                force_next_repaint = false;
            }
            last_notify_millis = System.currentTimeMillis();
            last_paint_millis = last_notify_millis;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void paintHierarchy(Graphics g) {
        try {
            if (g == null) {
                return;
            }
            long m1, m2, m3, m4;
            Graphics current_g = g;
            long cur_time = System.currentTimeMillis();
            m1 = cur_time;
            m2 = cur_time;
            m3 = cur_time;
            if (updating_hierarchy || painting) {
                return;
            }
            painting = true;
            if (null == modules) {
                return;
            }
            paint_hierarchy_count++;
            if (debug_on) {
                DebugPrint("paint_hierarchy_count=" + paint_hierarchy_count);
            }
            if (repaint_count > 0) {
                repaint_count--;
            }
            Rectangle rect = component_to_draw_on.getBounds();

            int xoffset = 0;//rect.x -scroll_x;
            Dimension temp_d = component_to_draw_on.getSize();
            int yoffset = 0;//rect.y -scroll_y;
            //System.err.println("rect="+rect+", d="+d);
            if (use_buffer_image) {
                m1 = System.currentTimeMillis();
                if (null != buffer_image) {
                    g.drawImage(buffer_image, 0, 0, temp_d.width, temp_d.height, component_to_draw_on);
                }
                m2 = System.currentTimeMillis();
                if (buffer_image == null
                        || (last_temp_d != null && (last_temp_d.width != temp_d.width || last_temp_d.height != temp_d.height))) {
                    buffer_image = component_to_draw_on.createImage(temp_d.width, temp_d.height);
                }
                m3 = System.currentTimeMillis();
                current_g = buffer_image.getGraphics();
                last_temp_d = temp_d;
            }
            g.setColor(Color.BLACK);
            current_g.setColor(Color.black);
            for (int i = 0; i < modules.size(); i++) {
                ModuleInfo module = modules.elementAt(i);
                if (null == module) {
                    continue;
                }
                if (module.x + MODULE_WIDTH < rect.x) {
                    continue;
                }
                if (module.x > rect.x + rect.width) {
                    continue;
                }
                if (module.y + MODULE_HEIGHT < rect.y) {
                    continue;
                }
                if (module.y > rect.y + rect.height) {
                    continue;
                }
                if (debug_on) {
                    DebugPrint("1227: i=" + i + ",partial_paint=" + partial_paint);
                }
                if (partial_paint && i == 0) {
                    module = modules.elementAt(current_module);
                } else if (partial_paint && i == 1
                        && null != moduleShowingCommands
                        && moduleShowingCommands != modules.elementAt(current_module)) {
                    module = moduleShowingCommands;
                } else if (partial_paint && i > 0) {
                    break;
                }
                module.line_to_sub_drawn = false;
                if ((!module.new_status && !module.new_command && module.echo_serial_number == module.last_echo_serial_number_displayed) || !use_color || printing) {
                    if(printing) {
                        g.setColor(Color.BLACK);
                    }
                    current_g.drawRect(xoffset + module.x + MODULE_XOFFSET - 1, yoffset + module.y + MODULE_YOFFSET - 1, MODULE_WIDTH + 1, MODULE_HEIGHT + 1);
                    if (null != module.parent) {
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + module.y + MODULE_YOFFSET,
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);

                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);

                        current_g.drawLine(xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET,
                                yoffset + module.parent.y + MODULE_YOFFSET + MODULE_HEIGHT);
                    }
                }
            }
            if (use_color) {
                current_g.setColor(Color.magenta);
            }

            if (null == modules) {
                return;
            }
            for (int i = 0; i < modules.size(); i++) {
                ModuleInfo module = modules.elementAt(i);

                if (debug_on) {
                    DebugPrint("1278: i=" + i + ",partial_paint=" + partial_paint);
                }

                if (partial_paint && i == 0) {
                    module = modules.elementAt(current_module);
                } else if (partial_paint && i == 1
                        && null != moduleShowingCommands
                        && moduleShowingCommands != modules.elementAt(current_module)) {
                    module = moduleShowingCommands;
                } else if (partial_paint && i > 0) {
                    break;
                }
                if (null == module) {
                    continue;
                }
                if (null == module.ModulesReadingAuxOutput) {
                    continue;
                }
                for (int j = 0; j < module.ModulesReadingAuxOutput.size(); j++) {
                    ModuleInfo auxmodule = (ModuleInfo) module.ModulesReadingAuxOutput.elementAt(j);
                    current_g.drawLine(xoffset + auxmodule.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + auxmodule.y + MODULE_YOFFSET,
                            xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET + MODULE_HEIGHT);
                }
            }

            for (int i = 0; i < modules.size(); i++) {
                ModuleInfo module = modules.elementAt(i);

                if (debug_on) {
                    DebugPrint("1317: i=" + i + ",partial_paint=" + partial_paint);
                }
                if (partial_paint && i == 0) {
                    module = modules.elementAt(current_module);
                } else if (partial_paint && i == 1
                        && null != moduleShowingCommands
                        && moduleShowingCommands != modules.elementAt(current_module)) {
                    module = moduleShowingCommands;
                } else if (partial_paint && i > 0) {
                    break;
                }

                if (null == module) {
                    continue;
                }
                if (partial_paint && use_color && !printing) {
                    current_g.setColor(this.component_to_draw_on.getBackground());
                    current_g.fillRect(xoffset + module.x + MODULE_XOFFSET,
                            yoffset + module.y + MODULE_YOFFSET - component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight(),
                            MODULE_WIDTH,
                            component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight());
                }
                if (use_color && module.is_connected) {
                    boolean new_command = module.new_command;
                    boolean new_status = module.new_status;
                    if (module.last_serial_number_displayed != module.serial_number) {
                        current_g.setColor(Color.red);
                        module.last_serial_number_displayed = module.serial_number;
                        module.last_new_command_time = cur_time;
                        new_command = true;
                    }
                    if (cur_time - module.last_new_command_time < new_threshold && cur_time > module.last_new_command_time) {
                        current_g.setColor(Color.red);
                        new_command = true;
                    }
                    module.last_serial_number_displayed = module.serial_number;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.serial_number),
                                xoffset + module.x,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                    if (new_command && null != module.parent) {
                        current_g.drawRect(xoffset + module.x + MODULE_XOFFSET - 1, yoffset + module.y + MODULE_YOFFSET - 1, MODULE_WIDTH + 1, MODULE_HEIGHT + 1);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + module.y + MODULE_YOFFSET,
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2 - 1,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2 - 1,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + module.parent.y + MODULE_YOFFSET + MODULE_HEIGHT);
                    }
                    current_g.setColor(Color.black);
                    if (module.last_echo_serial_number_displayed != module.echo_serial_number) {
                        current_g.setColor(Color.yellow);
                        module.last_echo_serial_number_displayed = module.echo_serial_number;
                        module.last_new_echo_serial_time = cur_time;
                        new_status = true;
                    }
                    if (cur_time - module.last_new_echo_serial_time < new_threshold && cur_time > module.last_new_echo_serial_time) {
                        current_g.setColor(Color.yellow);
                        new_status = true;
                    }
                    module.last_echo_serial_number_displayed = module.echo_serial_number;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.echo_serial_number),
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET + 5,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                    if (cur_time - module.last_new_stat_time < new_threshold && cur_time > module.last_new_stat_time) {
                        current_g.setColor(Color.yellow);
                        new_status = true;
                    }
                    if (new_status && !new_command && null != module.parent) {
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET,
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.parent.y + MODULE_YOFFSET + MODULE_HEIGHT);
                    }
                    module.new_status_drawn = module.new_status = new_status;
                    module.new_command_drawn = module.new_command = new_command;
                } else {
                    module.last_serial_number_displayed = module.serial_number;
                    module.last_echo_serial_number_displayed = module.echo_serial_number;
                    module.new_status_drawn = module.new_status;
                    module.new_command_drawn = module.new_command;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.echo_serial_number),
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET + 5,
                                yoffset + module.y + MODULE_YOFFSET);
                        current_g.drawString(String.valueOf(module.serial_number),
                                xoffset + module.x,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                }
                boolean module_done_or_executing = false;
                if (!use_color) {
                    current_g.setColor(Color.white);
                } else {
                    if (module.lastStatName == null) {
                        current_g.setColor(Color.pink);
                    } else if (module.lastStatName.startsWith("ERROR")) {
                        current_g.setColor(Color.red);
                    } else if (module.lastStatName.startsWith("EXEC")) {
                        current_g.setColor(Color.green);
                        module_done_or_executing = true;
                    } else if (module.lastStatName.startsWith("DONE")) {
                        current_g.setColor(Color.white);
                        module_done_or_executing = true;
                    } else {
                        current_g.setColor(Color.pink);
                    }
                }
                if ((cur_time - module.last_new_stat_time) > (1000 + new_threshold) && module.last_new_stat_time < last_notify_millis && module_done_or_executing) {
                    current_g.setColor(Color.yellow);
                }
                if (!module.is_connected) {
                    current_g.setColor(Color.lightGray);
                }
                boolean is_selected = false;
                if (null != modulesList) {
                    if (module.Name.equals(getModulesListSelectedItem())) {
                        is_selected = true;
                    }
                }
                Color textColor = Color.black;
                if (is_selected && !printing) {
                    Color curColor = current_g.getColor();
                    current_g.setColor(Color.darkGray);
                    current_g.fillRect(xoffset + module.x + MODULE_XOFFSET - (MODULE_X_SPACING / 4),
                            yoffset + module.y + MODULE_YOFFSET - (MODULE_Y_SPACING / 4),
                            MODULE_WIDTH + (MODULE_X_SPACING / 2),
                            MODULE_HEIGHT + (MODULE_Y_SPACING / 2));
                    current_g.setColor(curColor);
                }

                if (!printing) {
                    current_g.fillRect(xoffset + module.x + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET, MODULE_WIDTH, MODULE_HEIGHT);
                } else {
                    current_g.setColor(Color.BLACK);
                    g.setColor(Color.BLACK);
                    current_g.drawRect(xoffset + module.x + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET, MODULE_WIDTH, MODULE_HEIGHT);
                }
                if(use_color) {
                    current_g.setColor(textColor);
                }
                else {
                    current_g.setColor(Color.BLACK);
                }
                if (use_color && module.is_connected) {
                    boolean new_command = module.new_command;
                    boolean new_status = module.new_status;
                    if (module.last_serial_number_displayed != module.serial_number) {
                        current_g.setColor(Color.red);
                        module.last_serial_number_displayed = module.serial_number;
                        module.last_new_command_time = cur_time;
                        new_command = true;
                    }
                    if (cur_time - module.last_new_command_time < new_threshold && cur_time > module.last_new_command_time) {
                        current_g.setColor(Color.red);
                        new_command = true;
                    }
                    module.last_serial_number_displayed = module.serial_number;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.serial_number),
                                xoffset + module.x,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                    if (new_command && null != module.parent) {
                        current_g.drawRect(xoffset + module.x + MODULE_XOFFSET - 1, yoffset + module.y + MODULE_YOFFSET - 1, MODULE_WIDTH + 1, MODULE_HEIGHT + 1);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + module.y + MODULE_YOFFSET,
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2 - 1,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2 - 1,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET - 1, yoffset + module.parent.y + MODULE_YOFFSET + MODULE_HEIGHT);
                    }
                    current_g.setColor(Color.black);
                    if (module.last_echo_serial_number_displayed != module.echo_serial_number) {
                        current_g.setColor(Color.yellow);
                        module.last_echo_serial_number_displayed = module.echo_serial_number;
                        module.last_new_echo_serial_time = cur_time;
                        new_status = true;
                    }
                    if (cur_time - module.last_new_echo_serial_time < new_threshold && cur_time > module.last_new_echo_serial_time) {
                        current_g.setColor(Color.yellow);
                        new_status = true;
                    }
                    module.last_echo_serial_number_displayed = module.echo_serial_number;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.echo_serial_number),
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET + 5,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                    if (cur_time - module.last_new_stat_time < new_threshold && cur_time > module.last_new_stat_time) {
                        current_g.setColor(Color.yellow);
                        new_status = true;
                    }
                    if (new_status && !new_command && null != module.parent) {
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET,
                                xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2);
                        current_g.drawLine(xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + (module.y + module.parent.y) / 2 + MODULE_YOFFSET + MODULE_HEIGHT / 2,
                                xoffset + module.parent.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.parent.y + MODULE_YOFFSET + MODULE_HEIGHT);
                    }
                    module.new_status_drawn = module.new_status = new_status;
                    module.new_command_drawn = module.new_command = new_command;
                } else {
                    module.last_serial_number_displayed = module.serial_number;
                    module.last_echo_serial_number_displayed = module.echo_serial_number;
                    module.new_status_drawn = module.new_status;
                    module.new_command_drawn = module.new_command;
                    if (!printing) {
                        current_g.drawString(String.valueOf(module.echo_serial_number), xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET + 5,
                                yoffset + module.y + MODULE_YOFFSET);
                        current_g.drawString(String.valueOf(module.serial_number), xoffset + module.x,
                                yoffset + module.y + MODULE_YOFFSET);
                    }
                }
                current_g.setColor(textColor);
                try {
                    current_g.drawString(module.Name, xoffset + module.x,
                            yoffset + module.y + (component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight()) / 2);
                    if (null != module.lastCmdName) {
                        String cmd_name = module.lastCmdName;
                        if (null != module.common_cmd_base
                                && cmd_name.length() > module.common_cmd_base.length()
                                && cmd_name.substring(0, module.common_cmd_base.length()).equalsIgnoreCase(module.common_cmd_base)) {
                            cmd_name = cmd_name.substring(module.common_cmd_base.length());
                        } else if (cmd_name.length() > module.Name.length()
                                && cmd_name.substring(0, module.Name.length()).equalsIgnoreCase(module.Name)) {
                            cmd_name = cmd_name.substring(module.Name.length());
                        }
                        if (cmd_name.startsWith("_")) {
                            cmd_name = cmd_name.substring(1);
                        }
                        current_g.drawString(cmd_name, xoffset + module.x, yoffset + module.y + (2 * (component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight())));
                    }
                    if (null != module.lastStatName) {
                        current_g.drawString(module.lastStatName, xoffset + module.x, yoffset + module.y + ((int) 3.5 * (component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight())));
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            if (null != moduleShowingCommands) {
                int cmd_height = (int) (1.5 * (component_to_draw_on.getFontMetrics(component_to_draw_on.getFont()).getHeight()));
                if (debug_on) {
                    DebugPrint("cmd_height=" + cmd_height);
                }
                for (int cmd_num = 0; cmd_num < moduleShowingCommands.cmdsAvailable.size(); cmd_num++) {
                    String command_string = (String) moduleShowingCommands.cmdsAvailable.elementAt(cmd_num);
                    if (highlighted_command != cmd_num) {
                        current_g.setColor(Color.white);
                    } else {
                        current_g.setColor(Color.black);
                    }
                    if(!printing) {
                        current_g.fillRect(xoffset + moduleShowingCommands.x + MODULE_XOFFSET, yoffset + moduleShowingCommands.y + MODULE_YOFFSET + MODULE_HEIGHT + cmd_num * cmd_height, MODULE_WIDTH, cmd_height);
                    }
                    if (highlighted_command != cmd_num) {
                        current_g.setColor(Color.black);
                    } else {
                        current_g.setColor(Color.white);
                    }
                    current_g.drawRect(xoffset + moduleShowingCommands.x + MODULE_XOFFSET, yoffset + moduleShowingCommands.y + MODULE_YOFFSET + MODULE_HEIGHT + cmd_num * cmd_height, MODULE_WIDTH, cmd_height);
                    try {
                        if (null != moduleShowingCommands.common_cmd_base
                                && command_string.length() > moduleShowingCommands.common_cmd_base.length()
                                && command_string.substring(0, moduleShowingCommands.common_cmd_base.length()).equalsIgnoreCase(moduleShowingCommands.common_cmd_base)) {
                            command_string = command_string.substring(moduleShowingCommands.common_cmd_base.length());
                        } else if (command_string.length() > moduleShowingCommands.Name.length()
                                && command_string.substring(0, moduleShowingCommands.Name.length()).equalsIgnoreCase(moduleShowingCommands.Name)) {
                            command_string = command_string.substring(moduleShowingCommands.Name.length());
                        }
                        if (command_string.startsWith("_")) {
                            command_string = command_string.substring(1);
                        }
                        if (command_string.indexOf('=') > 2) {
                            command_string = command_string.substring(0, command_string.indexOf('='));
                        }
                        current_g.drawString(command_string,
                                xoffset + moduleShowingCommands.x + MODULE_XOFFSET + 5,
                                yoffset + moduleShowingCommands.y + MODULE_YOFFSET + MODULE_HEIGHT + (cmd_num + 1) * cmd_height - 2);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }

            for (int i = 0; i < modules.size(); i++) {
                ModuleInfo module = modules.elementAt(i);

                if (debug_on) {
                    DebugPrint("1670: i=" + i + ",partial_paint=" + partial_paint);
                }

                if (partial_paint && i == 0) {
                    module = modules.elementAt(current_module);
                } else if (partial_paint && i == 1
                        && null != moduleShowingCommands
                        && moduleShowingCommands != modules.elementAt(current_module)) {
                    module = moduleShowingCommands;
                } else if (partial_paint && i > 0) {
                    break;
                }

                if (null == module) {
                    continue;
                }
                if (null == module.ModulesReadingAuxOutput) {
                    continue;
                }
                line_style = "11100000";
                if (use_color) {
                    current_g.setColor(Color.magenta);
                }
                for (int j = 0; j < module.ModulesReadingAuxOutput.size(); j++) {
                    ModuleInfo auxmodule = (ModuleInfo) module.ModulesReadingAuxOutput.elementAt(j);
                    EnhancedDrawLine(current_g, xoffset + auxmodule.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + auxmodule.y + MODULE_YOFFSET,
                            xoffset + module.x + MODULE_WIDTH / 2 + MODULE_XOFFSET, yoffset + module.y + MODULE_YOFFSET + MODULE_HEIGHT);
                }
            }
            if (debug_on) {
                m4 = System.currentTimeMillis();
                DebugPrint("HierarchyPanel.paint() time =" + (m4 - m1) + " ms.");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        partial_paint = false;
        painting = false;
    }
    int line_width = 1;
    String line_style = null;

    private void EnhancedDrawLine(Graphics g, int x1, int y1, int x2, int y2) {
        int temp;
        int x_low;
        int x_high;
        int x_pos;
        int y_low;
        int y_high;
        int y_pos;
        int x_diff;
        int y_diff;
        int i;
        double m;
        double inverse_m;
        int starting_i = 0;
        try {
            if (x2 > x1) {
                x_low = x1;
                x_high = x2;
            } else {
                x_low = x2;
                x_high = x1;
            }
            if (y2 > y1) {
                y_low = y1;
                y_high = y2;
            } else {
                y_low = y2;
                y_high = y1;
            }
            x_diff = x_high - x_low;
            y_diff = y_high - y_low;

            // Handle vertical lineNumber and sindgle point case separately.
            if (x_diff < 1) {
                // Handle sindgle point case separately.
                if (y_diff < 1) {
                    g.fillRect(x_low, y_low, 1, 1);
                    return;
                }
                g.fillRect(x_low, y_low, line_width, y_diff);
                return;
            }
            // Handle horizontal lineNumber case separately.
            if (y_diff < 1) {
                g.fillRect(x_low, y_low, x_diff, line_width);
                return;
            }

            if (y_diff > x_diff) {
                // Swap the points to make the higher point (x1,y1)
                if (y1 > y2) {
                    temp = x2;
                    x2 = x1;
                    x1 = temp;
                    temp = y2;
                    y2 = y1;
                    y1 = temp;
                }
                if (y1 > 0) {
                    starting_i = 0;
                } else {
                    starting_i = -y1;
                }
                for (i = starting_i; i < y_diff; i++) {
                    if (line_style != null) {
                        if (line_style.charAt(i % line_style.length()) == '0') {
                            continue;
                        }
                    }
                    inverse_m = ((double) (x2 - x1)) / ((double) (y2 - y1));
                    y_pos = y1 + i;
                    if (y_pos > d.height) {
                        break;
                    }
                    x_pos = (int) (x1 + inverse_m * i);
                    if (x_pos > d.width + line_width / 2 && inverse_m > 0) {
                        break;
                    }
                    if (x_pos < -line_width / 2 && inverse_m < 0) {
                        break;
                    }
                    g.fillRect(x_pos - line_width / 2, y_pos, line_width, 1);
                }
            } else {
                // Swap the points to make the one to the left (x1,y1)
                if (x1 > x2) {
                    temp = x2;
                    x2 = x1;
                    x1 = temp;
                    temp = y2;
                    y2 = y1;
                    y1 = temp;
                }
                if (x1 > 0) {
                    starting_i = 0;
                } else {
                    starting_i = -x1;
                }
                for (i = starting_i; i < x_diff; i++) {
                    if (line_style != null) {
                        if (line_style.charAt(i % line_style.length()) == '0') {
                            continue;
                        }
                    }
                    m = ((double) (y2 - y1)) / ((double) (x2 - x1));
                    x_pos = x1 + i;
                    if (x_pos > d.width) {
                        break;
                    }
                    y_pos = (int) (y1 + m * i);
                    if (y_pos > d.height + line_width / 2 && m > 0) {
                        break;
                    }
                    if (x_pos < -line_width / 2 && m < 0) {
                        break;
                    }
                    g.fillRect(x_pos, y_pos - line_width / 2, 1, line_width);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private ModuleInfo FindParent(String child, Hashtable modulesHashTable) {
        String moduleName;
        ModuleInfo moduleInfo;
        try {
            if (debug_on) {
                DebugPrint("FindParent(" + child + ") called.");
            }
            if (modulesHashTable.size() < 1) {
                return null;
            }
            int number_of_modules = getModulesListItemCount();
            for (int i = 0; i < number_of_modules; i++) {
                moduleName = getModulesListItem(i);
                DebugPrint("moduleName=" + moduleName);
                if (null == moduleName) {
                    break;
                }
                moduleInfo = (ModuleInfo) modulesHashTable.get(moduleName);
                if (null == moduleInfo) {
                    continue;
                }
                int number_of_children = moduleInfo.children_names.size();
                if (debug_on) {
                    DebugPrint("moduleInfo=" + moduleInfo);
                    DebugPrint("moduleInfo.children_names=" + moduleInfo.children_names);
                }
                for (int ii = 0; ii < number_of_children; ii++) {
                    if (debug_on) {
                        DebugPrint("moduleInfo.children_names.elementAt(" + ii + ")=" + moduleInfo.children_names.elementAt(ii));
                    }
                    String test_string = (String) moduleInfo.children_names.elementAt(ii);
                    if (0 == test_string.compareTo(child)) {
                        if (debug_on) {
                            DebugPrint("FindParent(" + child + ") returning " + moduleInfo + ".");
                        }
                        return moduleInfo;
                    }
                }
            }
            if (debug_on) {
                DebugPrint("FindParent(" + child + ") returning null.");
            }
            return null;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public void FindAllParents(Hashtable modulesHashTable, URLLoadInfoPanelInterface loadingPanel) {
        String moduleName;
        ModuleInfo parent;
        ModuleInfo module;
        //	  boolean orig_debug_on=debug_on;
        try {
            //debug_on=true;
            if (debug_on) {
                DebugPrint("FindAllParents called.");
            }
            clear();
            int modulesHashTable_size = modulesHashTable.size();
            if (modulesHashTable_size < 1) {
                return;
            }
            max_x = 0;
            int number_of_modules = getModulesListItemCount();
            if (null != loadingPanel) {
                loadingPanel.set_URLname("Sorting Modules . . . ");
                loadingPanel.set_bytes_read(0);
                loadingPanel.set_content_length(number_of_modules);
                loadingPanel.repaint();
            }
            if (null != modules_by_generation) {
                modules_by_generation.removeAllElements();
                modules_by_generation = null;
            }
            modules_by_generation = new Vector<Vector<ModuleInfo>>();
            max_generations = -1;
            for (int ij = 0; ij < number_of_modules; ij++) {
                if (interrupt_loading) {
                    return;
                }
                moduleName = getModulesListItem(ij);
                if (null == moduleName) {
                    break;
                }
                module = (ModuleInfo) modulesHashTable.get(moduleName);
                if (null == module) {
                    continue;
                }
                if (!module.preset_x) {
                    module.x = 0;
                }
                if (!module.preset_y) {
                    module.y = 0;
                }
                module.generation = 0;
            }
            for (int i = 0; i < number_of_modules; i++) {
                if (interrupt_loading) {
                    return;
                }
                moduleName = getModulesListItem(i);
                if (null == moduleName) {
                    break;
                }
                parent = FindParent(moduleName, modulesHashTable);
                module = (ModuleInfo) modulesHashTable.get(moduleName);
                if (null == module) {
                    continue;
                }
                if (parent != null) {
                    module.generation = parent.generation + 1;
                    module.parent = parent;
                }
                if (module.generation > max_generations) {
                    for (int ii = max_generations; ii < module.generation + 1; ii++) {
                        modules_by_generation.addElement(new Vector<ModuleInfo>());
                    }
                    max_generations = module.generation;
                }
                if (modules_by_generation.size() > module.generation) {
                    Vector<ModuleInfo> generation_vector = modules_by_generation.elementAt(module.generation);
                    generation_vector.addElement(module);
                    modules.addElement(module);
                    if (generation_vector.size() > max_number_in_generation) {
                        max_number_in_generation = generation_vector.size();
                    }
                }
                if (null != loadingPanel) {
                    loadingPanel.set_bytes_read(loadingPanel.get_bytes_read() + 1);
                    loadingPanel.set_content_length(number_of_modules);
                    loadingPanel.repaint();
                }
            }
            boolean need_correction = true;
            while (need_correction) {
                need_correction = false;
                for (int i = 0; i < number_of_modules; i++) {
                    moduleName = getModulesListItem(i);
                    if (null == moduleName) {
                        continue;
                    }
                    module = (ModuleInfo) modulesHashTable.get(moduleName);
                    if (null == module) {
                        continue;
                    }
                    if (null == module.parent) {
                        module.generation = 0;
                        continue;
                    }
                    if (module.generation < module.parent.generation + 1) {
                        module.generation = module.parent.generation + 1;
                        need_correction = true;
                        break;
                    }
                }
            }
            if (modules_by_generation.size() > 0) {
                Vector<ModuleInfo> first_generation_vector = modules_by_generation.elementAt(0);
                if (null == first_generation_vector) {
                    return;
                }
                if (null != loadingPanel) {
                    loadingPanel.set_URLname("Positioning Modules . . . ");
                    loadingPanel.set_bytes_read(0);
                    loadingPanel.set_content_length(number_of_modules);
                    loadingPanel.repaint();
                }
                int xpos = 100;
                for (int iii = 0; iii < first_generation_vector.size(); iii++) {
                    if (interrupt_loading) {
                        return;
                    }
                    module = first_generation_vector.elementAt(iii);
                    if (null != module) {
                        xpos = PositionChildren(module, xpos, modulesHashTable, loadingPanel);
                    }
                    xpos = (int) (max_x + MODULE_WIDTH * 1.5);
                }
            }
            for (int i = 0; i < number_of_modules; i++) {
                moduleName = getModulesListItem(i);
                if (null == moduleName) {
                    continue;
                }
                module = (ModuleInfo) modulesHashTable.get(moduleName);
                if (null == module) {
                    continue;
                }
                if (null == module.AuxOutputNames) {
                    continue;
                }
                if (null == module.ModulesReadingAuxOutput) {
                    module.ModulesReadingAuxOutput = new Vector();
                }
                module.ModulesReadingAuxOutput.removeAllElements();
                for (int j = 0; j < module.AuxOutputNames.size(); j++) {
                    String auxOutput = (String) module.AuxOutputNames.elementAt(j);
                    if (debug_on) {
                        DebugPrint(module.Name + " writes to auxiliary channel " + auxOutput);
                    }
                    for (int k = 0; k < number_of_modules; k++) {
                        String auxmoduleName = getModulesListItem(k);
                        if (null == auxmoduleName) {
                            continue;
                        }
                        ModuleInfo auxmodule = (ModuleInfo) modulesHashTable.get(auxmoduleName);
                        if (null == auxmodule) {
                            continue;
                        }
                        if (null == auxmodule.AuxInputNames) {
                            continue;
                        }
                        for (int l = 0; l < auxmodule.AuxInputNames.size(); l++) {
                            String auxInput = (String) auxmodule.AuxInputNames.elementAt(l);
                            if (debug_on) {
                                DebugPrint(auxmodule.Name + " reads from auxiliary channel " + auxInput);
                            }
                            if (auxInput.equals(auxOutput)) {
                                DebugPrint(module.Name + " and " + auxmodule.Name + " share auxiliary channel " + auxInput);
                                module.ModulesReadingAuxOutput.addElement(auxmodule);
                                break;
                            }
                        }
                    }
                }
            }
            max_x += MODULE_WIDTH + MODULE_XOFFSET;
            if (min_x > -(MODULE_XOFFSET)) {
                min_x += MODULE_XOFFSET;
            } else {
                min_x = 0;
            }

            max_y += MODULE_HEIGHT + MODULE_YOFFSET;
            if (min_y > -(MODULE_YOFFSET)) {
                min_y += MODULE_YOFFSET;
            } else {
                min_y = 0;
            }
            Dimension d = component_to_draw_on.getPreferredSize();
            if (debug_on) {
                DebugPrint("Setting up Hierarchy Panel.");
                if (null != d) {
                    DebugPrint("m_hierarchyInnerPanel.preferredSize().width = " + d.width);
                    DebugPrint("m_hierarchyInnerPanel.preferredSize().height = " + d.height);
                }
                DebugPrint("min_x = " + min_x);
                DebugPrint("max_x = " + max_x);
                DebugPrint("min_y = " + min_y);
                DebugPrint("max_y = " + max_y);
            }
            int hscroll_current = min_x - 10;
            int hscroll_max = max_x + 20 + 2 * MODULE_WIDTH + MODULE_XOFFSET;
            if (hscroll_max < hscroll_current + 20 + 2 * MODULE_WIDTH + MODULE_XOFFSET) {
                hscroll_max = hscroll_current + 20 + 2 * MODULE_WIDTH + MODULE_XOFFSET;
            }
            int hscroll_min = min_x - 10 + MODULE_XOFFSET;
            int hscroll_visible = d.width;
            if (hscroll_visible > (hscroll_max - hscroll_min) / 2) {
                hscroll_visible = (hscroll_max - hscroll_min) / 2;
            }
            if (hscroll_current < hscroll_min) {
                hscroll_current = hscroll_min;
            }
            int hmaxx = computeMaxX();
            if (hscroll_max < hmaxx) {
                hscroll_max = hmaxx;
            }
            if (debug_on) {
                DebugPrint("hscroll_current = " + hscroll_current);
                DebugPrint("hscroll_visible = " + hscroll_visible);
                DebugPrint("hscroll_min = " + hscroll_min);
                DebugPrint("hscroll_max = " + hscroll_max);
            }
            if (null != horzScrollbar) {
                if (debug_on) {
                    DebugPrint("HierarchyPanel.FindAllParents calling horzScrollbar.setValues(" + hscroll_current + ", " + hscroll_visible + ", " + (hscroll_min - hscroll_visible) + ", " + (hscroll_max + hscroll_visible) + ");");
                }
                horzScrollbar.setValues(hscroll_current, hscroll_visible, (hscroll_min - hscroll_visible), hscroll_max + hscroll_visible);
                scroll_x = horzScrollbar.getValue();
            }
            int vscroll_current = min_y - 25;
            int vscroll_max = max_y + 10 + MODULE_HEIGHT + MODULE_YOFFSET;
            if (vscroll_max < vscroll_current + 20 + MODULE_HEIGHT + MODULE_YOFFSET) {
                vscroll_max = vscroll_current + 20 + MODULE_HEIGHT + MODULE_YOFFSET;
            }
            int vscroll_min = min_y - 10 + MODULE_YOFFSET;
            int vscroll_visible = d.height;
            if (vscroll_visible > (vscroll_max - vscroll_min) / 2) {
                vscroll_visible = (vscroll_max - vscroll_min) / 2;
            }
            if (vscroll_current < vscroll_min) {
                vscroll_current = vscroll_min;
            }
            int hmaxy = computeMaxY();
            if (vscroll_max < hmaxy) {
                vscroll_max = hmaxy;
            }
            if (debug_on) {
                DebugPrint("vscroll_current = " + vscroll_current);
                DebugPrint("vscroll_visible = " + vscroll_visible);
                DebugPrint("vscroll_min = " + vscroll_min);
                DebugPrint("vscroll_max = " + vscroll_max);
            }
            if (null != vertScrollbar) {
                if (debug_on) {
                    DebugPrint("HierarchyPanel.FindAllParents calling vertScrollbar.setValues(" + vscroll_current + ", " + vscroll_visible + ", " + (vscroll_min - vscroll_visible) + ", " + (vscroll_max + vscroll_visible) + ");");
                }
                vertScrollbar.setValues(vscroll_current, vscroll_visible, (vscroll_min - vscroll_visible), (vscroll_max + vscroll_visible));
                scroll_y = vertScrollbar.getValue();
            }
            if (debug_on) {
                DebugPrint("FindAllParents returning.");
                DebugPrint("diag_update_object=" + diag_update_object);
            }
            if (null != diag_update_object) {
                diag_update_object.set_need_update();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        //debug_on=orig_debug_on;
    }

    private int PositionChildren(ModuleInfo module, int x, Hashtable modulesHashTable, URLLoadInfoPanelInterface loadingPanel) {
        int begin_x = x;
        try {
            if (debug_on) {
                DebugPrint("PositionChildren(" + module + "," + x + " ...) called.");
            }
            if (interrupt_loading) {
                return 0;
            }
            for (int i = 0; i < module.children_names.size(); i++) {
                String child_name = (String) module.children_names.elementAt(i);
                if (module.IsAncestor(child_name)) {
                    ErrorPrint("module " + module + " has child with same name as ancestor : " + child_name);
                    break;
                }
                if (null == child_name) {
                    break;
                }
                ModuleInfo child_module = (ModuleInfo) modulesHashTable.get(child_name);
                if (null == child_module) {
                    continue;
                }
                if (i > 0) {
                    x = PositionChildren(child_module, max_x + MODULE_WIDTH + MODULE_X_SPACING, modulesHashTable, loadingPanel);
                } else {
                    x = PositionChildren(child_module, max_x + MODULE_WIDTH + MODULE_X_SPACING, modulesHashTable, loadingPanel);
                }
                if (i == 0) {
                    begin_x = x;
                }
            }
            int end_x = x;
            if (!module.preset_x) {
                module.x = (begin_x + end_x) / 2;
            }
            if (!module.preset_y) {
                module.y = module.generation * (MODULE_HEIGHT + MODULE_Y_SPACING) + MODULE_Y_SPACING;
            }
            if (min_x < 0 || min_x > module.x) {
                min_x = module.x;
            }
            if (max_x < 0 || max_x < module.x) {
                max_x = module.x;
            }
            if (min_y < 0 || min_y > module.y) {
                min_y = module.y;
            }
            if (max_y < 0 || max_y < module.y) {
                max_y = module.y;
            }
            if (null != loadingPanel) {
                loadingPanel.set_bytes_read(loadingPanel.get_bytes_read() + 1);
                loadingPanel.repaint();
            }
            if (debug_on) {
                //Thread.dumpStack();
                DebugPrint("PositionChildren(" + module + "," + x + " ...) returning " + module.x + " .");
            }
            return module.x;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public void componentResized(ComponentEvent evt) {
        if (debug_on) {
            Thread.dumpStack();
        }
    }

    public void componentShown(ComponentEvent evt) {
        if (debug_on) {
            Thread.dumpStack();
        }
    }

    public void componentHidden(ComponentEvent evt) {
        if (debug_on) {
            Thread.dumpStack();
        }
    }

    public void componentMoved(ComponentEvent evt) {
        if (debug_on) {
            //Thread.dumpStack();
            UpdateDisplay(true);
        }
    }

    @Override
    public int print(Graphics g, PageFormat pf, int page) throws PrinterException {
        boolean orig_use_color = this.use_color;
        int orig_scroll_x = this.scroll_x;
        int orig_scroll_y = this.scroll_y;
        boolean orig_printing = this.printing;
        try {
            this.use_color = false;
            if (page > 0) { /* We have only one page, and 'page' is zero-based */
                return Printable.NO_SUCH_PAGE;
            }

            /* User (0,0) is typically outside the imageable area, so we must
             * translate by the X and Y values in the PageFormat to avoid clipping
             */
            Graphics2D g2d = (Graphics2D) g;
            AffineTransform orig_at = g2d.getTransform();
            int max_x = Integer.MIN_VALUE;
            int min_x = Integer.MAX_VALUE;
            int max_y = Integer.MIN_VALUE;
            int min_y = Integer.MAX_VALUE;
            for (int i = 0; i < modules.size(); i++) {
                ModuleInfo module = modules.elementAt(i);
                if(module.x - MODULE_WIDTH < min_x) {
                    min_x = module.x - MODULE_WIDTH;
                }
                if(module.x + MODULE_WIDTH  > max_x) {
                    max_x = module.x + MODULE_WIDTH;
                }
                if(module.y - MODULE_HEIGHT < min_y) {
                    min_y = module.y - MODULE_HEIGHT;
                }
                if(module.y + MODULE_HEIGHT  > max_y) {
                    max_y = module.y + MODULE_HEIGHT;
                }
            }
            max_x += 10;
            min_x -= 10;
            max_y += 10;
            min_y -= 10;
            scroll_x = min_x;
            scroll_y = min_y;
            double scale_w= pf.getImageableWidth()/(max_x-min_x);
            double scale_h = pf.getImageableHeight()/(max_y-min_y);
            double scale = scale_w;
            if(scale_h < scale_w) {
                scale = scale_h;
            }
            g2d.scale(scale,scale);
            g2d.translate(pf.getImageableX()-min_x, pf.getImageableY()-min_y);
            this.paintHierarchy(g);
            g2d.setTransform(orig_at);

            /* tell the caller that this page is part of the printed document */
            return Printable.PAGE_EXISTS;
        } catch (Exception e) {
            e.printStackTrace();
            return Printable.NO_SUCH_PAGE;
        } finally {
            this.use_color = orig_use_color;
            this.scroll_x = orig_scroll_x;
            this.scroll_y = orig_scroll_y;
            this.printing = orig_printing;
        }
    }
}
