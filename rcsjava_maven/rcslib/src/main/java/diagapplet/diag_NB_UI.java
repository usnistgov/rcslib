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
 * diag_NB_UI.java
 *
 * Created on December 9, 2006, 5:44 PM
 */
package diagapplet;

import diagapplet.CodeGen.BufferInfo;
import diagapplet.CodeGen.DiagNMLMsgDictInterface;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.STI_TokenizerInterface;
import diagapplet.CodeGen.StructureTypeInfo;
import diagapplet.plotter.PlotData;
import diagapplet.utils.WatchVarEditor;
import java.awt.Desktop;
import java.awt.Dimension;
import java.awt.DisplayMode;
import java.awt.Graphics;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.XMLDecoder;
import java.beans.XMLEncoder;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Queue;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.jar.Attributes;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.LookAndFeel;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.SwingWorker.StateValue;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import rcs.nml.NMLBufferConfigInfo;
import rcs.nml.NMLConnection;
import rcs.nml.NMLConnectionInterface;
import rcs.nml.NMLException;
import rcs.nml.NMLSingleVarLog;
import rcs.utils.URL_and_FileLoader;

/**
 * Main class used for running the newer Swing based diagnostics tool. 
 * The new version of the tool does not support being run as an applet.
 * @author  shackle
 */
public class diag_NB_UI extends javax.swing.JFrame {

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613940L;
    private diagapplet.CodeGen.CodeGenCommonInterface cgc = null;
    private diagapplet.HierarchyLoad_NB_UI hierarchy_loader = null;
    private diagapplet.HierarchyDraw hierarchyDraw = null;
    private diagapplet.utils.JListFastListPanel modulesList = null;
    private Vector<String> cmdToSendVariables = null;
    private Vector<String> cmdToSendValues = null;
    private ModuleInfo curModule = null;
    //    private String lastCmdToSendString=null;
    //private AbstractTableModel cmdToSendModel=null;
    private File recentFile = null;
    private Vector<String> recentVector = null;
    private SwingWorker ExecutingBackgroundSwingWorker = null;
    private Queue<SwingWorker> backgroundSwingWorkerQueue = null;
    private javax.swing.Timer javax_swing_timer = null;
    private int refreshTime_milliseconds = 100;
    private Hashtable<String, PlotTracker> plot_tracker_hashtable = null;
    private int cmd_selected_var_number;
    private String cmd_selected_var_name = null;
    private int status_selected_var_number;
    private String status_selected_var_name = null;
    private int aux_selected_var_number;
    private String aux_selected_var_name = null;
    private int cycle_count = 0;
    private boolean first_nml_single_var_log_timestamp_set = false;
    private double first_nml_single_var_log_timestamp = 0.0;
    private int new_point_count = 0;
    private int total_point_count = 0;
    private int last_total_point_count = -1;
    private boolean fit_done = false;
    private String last_hierarchy_loaded = null;
    private diagPreserve dp = null;
    private String cmdToSendIdString = null;
    private String statusToSendIdString = null;
    private String auxMsgToSendIdString = null;
    private long last_status_id_long = 0;
    private long last_cmd_id_long = 0;
    private long last_aux_id_long = 0;
    private long last_cmds_count_update_time = -1;
    private long last_stats_count_update_time = -1;
    private long last_aux_count_update_time = -1;
    private Hashtable<String, BufferInfo> auxBuffersHashtable = null;
    private BufferInfo curAuxBuffer = null;
    private static diagapplet.CodeGen.DiagNMLMsgDictCreator codegen_diag_dict_creator = null;
    private boolean double_buffer_nml = false;
    private boolean plot_updated = false;
    private int num_connected = 0;
    private static String args[] = null;
    private long refreshTime_millis = 100;
    private String input_headers = null;
    private String message_files = null;
    boolean list_modules_by_number = false;
    private boolean connectOnStartup = false;
    private boolean param_set_diag_minimized = false;
    private boolean param_set_diag_auto_connect_disconnect = false;
    private boolean dp_plot_list_loaded = false;
    private int cmd_to_send_set_count = 0;
    private int cmd_to_send_sent_count = 0;
    private String cmdToSendString = null;
    private ModuleInfo cmdToSendModule = null;
    private int status_to_send_set_count = 0;
    private int status_to_send_sent_count = 0;
    private String statusToSendString = null;
    private int aux_to_send_set_count = 0;
    private int aux_to_send_sent_count = 0;
    private String auxToSendString = null;
    private WatchVarEditor cmdToSendWve = null;
    private WatchVarEditor auxToSendWve = null;
    private boolean delayed_job_to_do = false;
    private NmlInfo ni_to_override_connected = null;
    private boolean ni_connect_override = false;
    private boolean tab_selected = false;
    private Vector<JCheckBoxMenuItem> jcbmi_laf_vector = null;

    private class ExtraTabInfo {

        Class cls;
        int tab_number;
        String name;
        JPanel jp;
        Method connect_method;
        Method disconnect_method;
        Method update_method;
    }
    private static int except_count = 0;
    private static boolean shutting_down = false;

    /**
     * Prints a specially formatted version of the Exception/Throwable including stack tracke both to
     * stderr and appended to
     * the Error Panel TextField of the First created instance of diag_NB_UI if one
     * exists. (Ussually there is only one diag_NB_UI)
     * Also the first 4 exceptions open Popup Message dialogs.
     * @param e -- the exception/throwable to print.
     */
    static public void PrintException(Throwable e) {
        if (shutting_down || cancel_connect_to_all_called) {
            return;
        }
        try {
            try {
                NMLException ne = (NMLException) e;
                if (ne.internal_exception != null) {
                    PrintException(ne.internal_exception);
                }
            } catch (Exception ex1) {
            }

            try {
                Throwable cause = e.getCause();
                while (cause != null) {
                    PrintException(cause);
                    cause = cause.getCause();
                }
            } catch (Exception ex1) {
            }

            String s = e.getMessage();
            if (null == s) {
                s = "";
            }
            StackTraceElement ste[] = e.getStackTrace();

            String sfull = " RCS Diagnostics ERROR: " + s;
            if (ste != null && ste.length >= 2 && ste[1] != null && ste[1].getFileName() != null) {
                sfull = ste[1].getFileName() + ":" + ste[1].getLineNumber() + sfull;
            }
            diagapplet.utils.DiagError.println(sfull);
            for (StackTraceElement st : ste) {
                diagapplet.utils.DiagError.println("\t" + st);
            }
            e.printStackTrace();
            if (s.length() > 250) {
                s = s.substring(0, 250);
            }
            final String msgString = s;
            if (except_count < 4 && msgString != null && msgString.length() > 0
                    && null != main_diag_NB_UI
                    && null != main_diag_NB_UI.jTabbedPane1) {
                java.awt.EventQueue.invokeLater(new Runnable() {

                    @Override
                    public void run() {
                        try {
                            main_diag_NB_UI.jTabbedPane1.setSelectedComponent(main_diag_NB_UI.jPanelErrorPanel);
                            JOptionPane.showMessageDialog(main_diag_NB_UI, msgString, "RCS_Diagnostics Exception", JOptionPane.ERROR_MESSAGE);
                        } catch (Exception ex) {
                            ex.printStackTrace();
                        }
                    }
                });
            }
            except_count++;
        } catch (Exception ee) {
            ee.printStackTrace();
        }
    }
    private static int print_error_count = 0;

    /**
     * Prints a specially formatted version of the Exception/Throwable including stack tracke both to
     * stderr and appended to
     * the Error Panel TextField of the First created instance of diag_NB_UI if one
     * exists. (Ussually there is only one diag_NB_UI)
     * Also the first 4 strings with open Popup Message dialogs.
     * @param s -- error string to print.
     */
    static public void PrintError(String s) {
        if (shutting_down) {
            return;
        }
        try {

            String sfull = " RCS Diagnostics ERROR: " + s + "\n";
            StackTraceElement ste[] = Thread.currentThread().getStackTrace();
            if (ste != null && ste.length >= 3 && ste[2] != null && ste[2].getFileName() != null) {
                sfull = ste[2].getFileName() + ":" + ste[2].getLineNumber() + sfull;
            }
            diagapplet.utils.DiagError.println(sfull);
            if (s.length() > 72) {
                s = s.substring(0, 68) + " . . . ";
            }
            final String msgString = s;
            if (print_error_count < 4 && msgString != null && msgString.length() > 0
                    && null != main_diag_NB_UI
                    && null != main_diag_NB_UI.jTabbedPane1) {
                java.awt.EventQueue.invokeLater(new Runnable() {

                    @Override
                    public void run() {
                        try {
                            main_diag_NB_UI.jTabbedPane1.setSelectedComponent(main_diag_NB_UI.jPanelErrorPanel);
                            JOptionPane.showMessageDialog(main_diag_NB_UI, msgString, "RCS_Diagnostics Exception", JOptionPane.ERROR_MESSAGE);
                        } catch (Exception ex) {
                            ex.printStackTrace();
                        }
                    }
                });
            }
            print_error_count++;
        } catch (Exception ee) {
            ee.printStackTrace();
        }
    }
    Vector<ExtraTabInfo> extraTabVector = null;

    // The following code starts the applet running within the frame window.
    /** Creates new form diag_NB_UI */
    public diag_NB_UI() {
        shutting_down = false;
        diagapplet.utils.DiagError.shutting_down = false;
        rcs.nml.NMLConnection.do_not_print_errors = false;
        initComponents();
        modulesList = new diagapplet.utils.JListFastListPanel(jListModules);
        ModuleInfo.application_type = ModuleInfo.RCS_DIAGNOSTICS_APPLICATION_TYPE;
        cgc = new diagapplet.CodeGen.CodeGenCommon();
        codegen_diag_dict_creator = new diagapplet.CodeGen.DiagNMLMsgDictCreator();
        cgc.set_diag_dict_creator(codegen_diag_dict_creator);
        cgc.set_nml_creator(rcs.nml.NMLConnection.Creator);
        cgc.set_ClassList(new diagapplet.utils.FakeFastListPanel());
        cgc.set_m_modulesList(modulesList);
        hierarchyDraw = new diagapplet.HierarchyDraw(jPaintablePanelHierarchyInner);
        HierarchyDraw.debug_on = false;
        hierarchyDraw.modulesList = modulesList;
        jPaintablePanelHierarchyInner.set_painter(
                new PainterInterface() {

                    private Rectangle selected_rect = null;

                    @Override
                    public void set_selected_rect(Rectangle rect) {
                        this.selected_rect = rect;
                    }

                    @Override
                    public Rectangle get_selected_rect() {
                        return this.selected_rect;
                    }

                    @Override
                    public void paintComponent(Graphics g) {
                        hierarchyDraw.paint(g);
                    }
                });

        javax_swing_timer = new javax.swing.Timer(refreshTime_milliseconds,
                new ActionListener() {

                    @Override
                    public void actionPerformed(ActionEvent evt) {
                        run_cycle();
                    }
                });
        this.watchJPanelDetailsStatus.addListSelectionListener(new ListSelectionListener() {

            @Override
            public void valueChanged(ListSelectionEvent e) {
                status_selected_var_name = watchJPanelDetailsStatus.GetVarName();
                status_selected_var_number = watchJPanelDetailsStatus.GetVarNum();
                UpdatePlotStatusVarButton();
                UpdatePlotStatusArrayButton();
            }
        });
        this.watchJPanelDetailsCmd.addListSelectionListener(new ListSelectionListener() {

            @Override
            public void valueChanged(ListSelectionEvent e) {
                cmd_selected_var_name = watchJPanelDetailsCmd.GetVarName();
                cmd_selected_var_number = watchJPanelDetailsCmd.GetVarNum();
                UpdatePlotCmdVarButton();
                UpdatePlotCmdArrayButton();
            }
        });
        this.watchJPanelAuxView.addListSelectionListener(new ListSelectionListener() {

            @Override
            public void valueChanged(ListSelectionEvent e) {
                aux_selected_var_name = watchJPanelAuxView.GetVarName();
                aux_selected_var_number = watchJPanelAuxView.GetVarNum();
                UpdatePlotAuxViewVarButton();
                UpdatePlotAuxViewArrayButton();
            }
        });
        plotter_NB_UI1.addClearActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                ClearPlots();
            }
        });
        dp = new diagPreserve();
        Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {

            @Override
            public void run() {
                diagapplet.utils.DiagError.shutting_down = true;
                shutting_down = true;
                rcs.nml.NMLConnection.do_not_print_errors = true;
                SaveDiagPreserve();
                WriteLastDirs();
            }
        }));
        ReadRecentVector();
        watchJPanelCmdToSend.setEditable(true);
        watchJPanelCmdToSend.SetDefault("0");
        cmdToSendWve = new WatchVarEditor();
        watchJPanelCmdToSend.setTableCellEditor(1, cmdToSendWve);
        watchJPanelAuxMsgToSend.setEditable(true);
        watchJPanelAuxMsgToSend.SetDefault("0");
        auxToSendWve = new WatchVarEditor();
        watchJPanelAuxMsgToSend.setTableCellEditor(1, auxToSendWve);
        plotter_NB_UI1.AddDeleteActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                DeleteDeleteMePlotTrackers();
            }
        });
        SetIcon();
        jTableNML.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        jTableNML.getSelectionModel().addListSelectionListener(new ListSelectionListener() {

            @Override
            public void valueChanged(ListSelectionEvent e) {
                ListSelectionModel lsm =
                        (ListSelectionModel) e.getSource();
                int row = lsm.getMinSelectionIndex();
                SelectNmlTableRow(row);
            }
        });
        TableRowSorter<TableModel> sorter =
                new TableRowSorter<TableModel>(jTableNML.getModel());
        sorter.setComparator(3, new Comparator() {

            @Override
            public int compare(Object o1, Object o2) {
                try {
                    String s1 = o1.toString();
                    String s2 = o2.toString();
                    int ltindex = s1.indexOf('<');
                    if (ltindex > 0) {
                        s1 = s1.substring(0, ltindex);
                    }
                    ltindex = s2.indexOf('<');
                    if (ltindex > 0) {
                        s2 = s2.substring(0, ltindex);
                    }
                    return Integer.valueOf(s1).intValue() - Integer.valueOf(s2).intValue();
                } catch (NumberFormatException numberFormatException) {
                    numberFormatException.printStackTrace();
                    return o1.toString().compareTo(o2.toString());
                }
            }
        });
        sorter.setSortsOnUpdates(true);
        jTableNML.setRowSorter(sorter);
        diag_common.PrintMemUsage("Finished diag_NB_UI constructor :");
        InputMap Imap = this.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap Amap = this.getRootPane().getActionMap();
        Imap.put(KeyStroke.getKeyStroke("F1"), "hierarchy");
        Amap.put("hierarchy", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actEvent) {
                jTabbedPane1.setSelectedComponent(jPanelHierarchyOuter);
            }
        });
        Imap.put(KeyStroke.getKeyStroke("F2"), "details");
        Amap.put("details", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actEvent) {
                jTabbedPane1.setSelectedComponent(jPanelDetails);
            }
        });
        Imap.put(KeyStroke.getKeyStroke("F3"), "aux");
        Amap.put("aux", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actEvent) {
                jTabbedPane1.setSelectedComponent(jPanelAuxChannels);
            }
        });
        Imap.put(KeyStroke.getKeyStroke("F4"), "nml");
        Amap.put("nml", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actEvent) {
                jTabbedPane1.setSelectedComponent(jPanelNML);
            }
        });
        Imap.put(KeyStroke.getKeyStroke("F5"), "graph");
        Amap.put("graph", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent actEvent) {
                jTabbedPane1.setSelectedComponent(plotter_NB_UI1);
            }
        });
        jCheckBoxMenuItemFullscreen.setAccelerator(KeyStroke.getKeyStroke("F11"));
        jCheckBoxMenuItemConnected.setAccelerator(KeyStroke.getKeyStroke("F12"));
        jCheckBoxMenuItemShowAdjusterWindow.setAccelerator(KeyStroke.getKeyStroke("F9"));
        jTableNML.getModel().addTableModelListener(new TableModelListener() {

            @Override
            public void tableChanged(TableModelEvent e) {
                jTableNMLChanged(e);
            }
        });
        check_running_file();
        if (param_set_diag_minimized
                || diagapplet.CodeGen.StringFuncs.getenv("DIAG_MINIMIZED") != null) {
            this.setExtendedState(java.awt.Frame.ICONIFIED);
        }
        diagapplet.utils.DiagError.AddDiagErrorAppender(new diagapplet.utils.DiagErrorAppendInterface() {

            @Override
            public void AppendError(final String s) {
                try {
                    if (null != jTextAreaErrs) {
                        java.awt.EventQueue.invokeLater(new Runnable() {

                            @Override
                            public void run() {
                                try {
                                    if (cancel_connect_to_all_called) {
                                        return;
                                    }
                                    jTextAreaErrs.append(s + "\n");
                                } catch (Exception e) {
                                    e.printStackTrace();
                                }
                            }
                        });
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
        rcs.nml.NMLConnection.AddNMLErrorAppender(new rcs.nml.NMLErrorAppender() {

            @Override
            public void AppendError(String s) {
                try {
                    if (null != jTextAreaErrs) {
                        jTextAreaErrs.append(s + "\n");
                    }
                } catch (Exception e) {
                    PrintException(e);
                }
            }
        });
        parseArgs();
        if ((null != input_headers && input_headers.length() > 0)
                || (null != message_files && message_files.length() > 0)) {
            java.awt.EventQueue.invokeLater(new Runnable() {

                @Override
                public void run() {
                    try {
                        System.out.println("input_headers = " + input_headers);
                        StringTokenizer st = new StringTokenizer(input_headers, File.pathSeparator);
                        while (st.hasMoreElements()) {
                            LoadInputHeader(st.nextToken());
                        }
                        System.out.println("message_files = " + message_files);
                        st = new StringTokenizer(message_files, File.pathSeparator);
                        while (st.hasMoreElements()) {
                            String message_file_name = st.nextToken();
                            if (message_file_name.startsWith("~/")) {
                                message_file_name = System.getProperty("user.home") + message_file_name.substring(1);
                            }
                            if (message_file_name.endsWith(".xml")) {
                                OpenXMLMessageFile(new File(message_file_name));
                            } else {
                                OpenPackedMessageFile(new File(message_file_name));
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            });
        }
        UIManager.LookAndFeelInfo installed_lafs[] =
                UIManager.getInstalledLookAndFeels();
        LookAndFeel current_laf = UIManager.getLookAndFeel();
        String cur_laf_name = current_laf.getName();
        if (installed_lafs != null) {
            jcbmi_laf_vector = new Vector<JCheckBoxMenuItem>();
            for (int i = 0; i < installed_lafs.length; i++) {
                UIManager.LookAndFeelInfo laf = installed_lafs[i];
                final String laf_name = laf.getClassName();
                JCheckBoxMenuItem jcbmi = new JCheckBoxMenuItem(laf.getName());
                jcbmi_laf_vector.add(jcbmi);
                final JFrame jf_to_update = this;
                final JCheckBoxMenuItem jcbmi_to_set = jcbmi;
                boolean is_current = (cur_laf_name.compareTo(laf.getName()) == 0 || current_laf.getClass().getCanonicalName().compareTo(laf_name) == 0);
                jcbmi.setSelected(is_current);
                jcbmi.addActionListener(new ActionListener() {

                    @Override
                    public void actionPerformed(ActionEvent e) {
                        try {
                            UIManager.setLookAndFeel(laf_name);
                            SwingUtilities.updateComponentTreeUI(jf_to_update);
                            jf_to_update.pack();
                            File pref_laf = new File(System.getProperty("user.home"), ".java_diag_preferred_laf");
                            PrintStream ps = new PrintStream(new FileOutputStream(pref_laf));
                            ps.println(laf_name);
                            ps.close();
                            for (int i = 0; i < jcbmi_laf_vector.size(); i++) {
                                jcbmi_laf_vector.elementAt(i).setSelected(false);
                            }
                            jcbmi_to_set.setSelected(true);
                        } catch (Exception exception) {
                            exception.printStackTrace();
                        }
                    }
                });
                this.jMenuLookAndFeel.add(jcbmi);
            }
        }
        this.watchJPanelCmdToSend.setChangeIdRunnable(new Runnable() {

            @Override
            public void run() {
                UpdateCmdToSendType();
            }
        });
    }
    private long last_cmd_to_send_id = -1;

    private void UpdateCmdToSendType() {
        long id = watchJPanelCmdToSend.getId();
        if (id != last_cmd_to_send_id && id > 0) {
            String new_cmdToSendIdString = Long.toString(id);
            last_cmd_to_send_id = id;
            if (new_cmdToSendIdString.compareTo(this.cmdToSendIdString) != 0) {
                this.cmdToSendIdString = new_cmdToSendIdString;
                StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(Long.valueOf(id));
                if (null != sti) {
                    String name = sti.getName();
                    if (name.compareTo(this.jTextFieldCmdToSendMsgType.getText()) != 0) {
                        this.jTextFieldCmdToSendMsgType.setText(sti.getName());
                    }
                }
            }
        }
    }

    private boolean backgroundQueueEmpty() {
        if (null == this.ExecutingBackgroundSwingWorker
                || this.ExecutingBackgroundSwingWorker.isCancelled()
                || this.ExecutingBackgroundSwingWorker.isDone()) {
            if (null == this.backgroundSwingWorkerQueue) {
                return true;
            } else {
                if (this.backgroundSwingWorkerQueue.size() >= 1) {
                    return false;
                } else {
                    return true;
                }
            }
        }
//        System.out.println("this.ExecutingBackgroundSwingWorker = " + this.ExecutingBackgroundSwingWorker);
        return false;
    }

    private void check_running_file() {
        if (HierarchyDraw.updating_hierarchy || running_file_written) {
            return;
        }
        if (this.backgroundQueueEmpty()) {
            write_running_file("diag.running");
        }
    }
    StackTraceElement bgExecuteStackTrace[] = null;
    Thread bgExecuteThread = null;
    long bgExecute_time = 0;
    StackTraceElement bgQueueAddExecuteStackTrace[] = null;
    Thread bgQueueAddExecuteThread = null;
    long bgQueueAddExecute_time = 0;
    StackTraceElement bgNextExecuteStackTrace[] = null;
    Thread bgNextExecuteThread = null;
    long bgNextExecute_time = 0;
    StackTraceElement last_bgExecuteStackTrace[] = null;
    Thread last_bgExecuteThread = null;
    long last_bgExecute_time = 0;

    synchronized private void ExecuteBackgroundSwingWorkerImmediate(SwingWorker backgroundSwingWorkerToRun) throws Exception {
        if (this.ExecutingBackgroundSwingWorker != null
                && !this.ExecutingBackgroundSwingWorker.isCancelled()
                && !this.ExecutingBackgroundSwingWorker.isDone()) {
            throw new Exception("Already executing background swing worker");
        }
        if (backgroundSwingWorkerToRun.getState() != StateValue.PENDING) {
            throw new Exception("backGroundSwingWorker.getState() = " + backgroundSwingWorkerToRun.getState());
        }
        this.ExecutingBackgroundSwingWorker = backgroundSwingWorkerToRun;

        last_bgExecuteThread = bgExecuteThread;
        last_bgExecuteStackTrace = bgExecuteStackTrace;
        last_bgExecute_time = bgExecute_time;

        bgExecuteThread = Thread.currentThread();
        bgExecuteStackTrace = bgExecuteThread.getStackTrace();
        bgExecute_time = System.currentTimeMillis();

        this.ExecutingBackgroundSwingWorker.execute();
    }

    synchronized private void ExecuteBackgroundSwingWorkerIfIdle(SwingWorker backgroundSwingWorkerToRun) throws Exception {
        if (!this.backgroundQueueEmpty()) {
            return;
        }
        this.ExecuteBackgroundSwingWorkerImmediate(backgroundSwingWorkerToRun);
    }
    private int last_queue_add_size = -1;

    synchronized private void QueueSwingWorker(SwingWorker backgroundSwingWorkerToRun) throws Exception {
        if (this.backgroundQueueEmpty()) {
            this.ExecuteBackgroundSwingWorkerImmediate(backgroundSwingWorkerToRun);
            return;
        }
        if (null == this.backgroundSwingWorkerQueue) {
            this.backgroundSwingWorkerQueue = new LinkedList<SwingWorker>();
        }
        bgQueueAddExecuteThread = Thread.currentThread();
        bgQueueAddExecuteStackTrace = bgExecuteThread.getStackTrace();
        bgQueueAddExecute_time = System.currentTimeMillis();
        this.backgroundSwingWorkerQueue.add(backgroundSwingWorkerToRun);
//        System.err.println("backgroundSwingWorkerQueue="+backgroundSwingWorkerQueue);
//        System.err.println("backgroundSwingWorkerQueue.size()="+backgroundSwingWorkerQueue.size());
        last_queue_add_size = backgroundSwingWorkerQueue.size();
    }

    private void jTableNMLChanged(TableModelEvent e) {
        try {
            int row = e.getFirstRow();
            int column = e.getColumn();
            if (column != 0) {
                return;
            }
            if (row != jTableNML.getSelectedRow()) {
                return;
            }
            DefaultTableModel model = (DefaultTableModel) e.getSource();
            //String columnName = model.getColumnName(column);
            Object data = model.getValueAt(row, column);
            Boolean B = (Boolean) data;
            String buffer_name = (String) model.getValueAt(row, 1);
            NmlInfo ni = nml_hashtable.get(buffer_name);
            if (null == ni) {
                String s = "nml_hashtable.get(" + buffer_name + ") returned null\n\nnml_hashtable.keys()=";
                for (String k : nml_hashtable.keySet()) {
                    s += "\t" + k + ",\n";
                }
                this.jTextPaneNmlError.setText(s);
                return;
            }
            ni_connect_override = B.booleanValue();
            if (ni.connected != ni_connect_override) {
                ni_to_override_connected = ni;
            }
        } catch (Exception except) {
            except.printStackTrace();
        }
    }
    static private boolean running_file_written = false;

    private void write_running_file(String running_file_name) {
        try {
            running_file_written = true;
            File f = new File(running_file_name);
            FileOutputStream fos = new FileOutputStream(f);
            PrintWriter pos = new PrintWriter(fos);
            pos.println("");
            pos.println(this.getClass().toString());
            pos.println("Hierarchy : " + last_hierarchy_loaded);
            pos.println("NML Config: " + ModuleInfo.DefaultNMLConfigurationFile);
            pos.close();
            fos.close();
            pos = null;
            fos = null;
            diagapplet.utils.DiagError.println("wrote " + running_file_name + "\n");
        } catch (Exception e) {
            PrintException(e);
        }
    }
    private static Vector<String> recognized_argsV = null;

    private static void add_recognized_arg(String s) {
        if (null == recognized_argsV) {
            recognized_argsV = new Vector<String>();
        }
        recognized_argsV.add(s);
    }

    private void AddExtraTab(String tab) throws Exception {
        if (null != this.last_hierarchy_loaded && new File(this.last_hierarchy_loaded).exists()) {
            rcs.utils.URL_and_FileLoader.AddToSearchPath(new File(this.last_hierarchy_loaded).getParent());
        }
        String tab_name = null;
        String class_name = null;
        String jar_url_list = null;
        boolean selected = false;
        if (tab.startsWith("inline:")) {
            StringTokenizer tokenizer = new StringTokenizer(tab.substring(7), ",");
            tab_name = tokenizer.nextToken();
            if (tokenizer.hasMoreTokens()) {
                class_name = tokenizer.nextToken();
            }
            if (tokenizer.hasMoreTokens()) {
                jar_url_list = tokenizer.nextToken();
            }
            if (tokenizer.hasMoreTokens()) {
                selected = Boolean.parseBoolean(tokenizer.nextToken());
            }
        } else {
            rcs.utils.URL_and_FileLoader ufl = new URL_and_FileLoader(tab);
            if (!ufl.TryNameSucceeded) {
                diagapplet.utils.DiagError.println("Can't open ExtraTab file " + tab);
                throw new Exception("Can't open ExtraTab file " + tab);
            }
            String line = ufl.readLine();
            int line_count = 0;
            while (line != null) {
                line_count++;
                line = line.trim();
                int eq_index = line.indexOf('=');
                if (line.length() < 1) {
                    line = ufl.readLine();
                    continue;
                } else if (line.charAt(0) == '#') {
                    line = ufl.readLine();
                    continue;
                } else if (eq_index > 0) {
                    String var_name = line.substring(0, eq_index).trim();
                    String var_value = line.substring(eq_index + 1).trim();
                    if (var_name.compareTo("tab_name") == 0) {
                        tab_name = var_value;
                        line = ufl.readLine();
                        continue;
                    } else if (var_name.compareTo("class_name") == 0) {
                        class_name = var_value;
                        line = ufl.readLine();
                        continue;
                    } else if (var_name.compareTo("jar_url") == 0) {
                        if (null == jar_url_list) {
                            jar_url_list = "";
                        }
                        jar_url_list += ";" + var_value;
                        line = ufl.readLine();
                        continue;
                    } else if (var_name.compareTo("selected") == 0) {
                        selected = Boolean.parseBoolean(var_value);
                        line = ufl.readLine();
                        continue;
                    }
                }
                throw new Exception("Unrecognize line " + line + " in " + tab + ":" + line_count);
            }
            ufl.close();
            ufl = null;
        }
        if (null != extraTabVector) {
            for (ExtraTabInfo eti : extraTabVector) {
                if (eti.name.compareTo(tab_name) == 0) {
                    return;
                }
            }
        }
        URLClassLoader ucl = null;
        Class cls = null;
        if (null != jar_url_list) {
            StringTokenizer jar_url_list_tokenizer = new StringTokenizer(jar_url_list, ";");
            URL urls[] = new URL[jar_url_list_tokenizer.countTokens()];
            for (int i = 0; i < urls.length; i++) {
                String jar_url = jar_url_list_tokenizer.nextToken();
                if (jar_url.indexOf(':') < 0) {
                    if (jar_url.startsWith("ftp.")) {
                        jar_url = "ftp://" + jar_url;
                    } else if (jar_url.startsWith("www.")) {
                        jar_url = "http://" + jar_url;
                    } else if (jar_url.startsWith("/")) {
                        jar_url = "file://" + jar_url;
                    } else if (new File(jar_url).exists()) {
                        jar_url = "file://" + new File(jar_url).getCanonicalPath();
                    } else if (new File(new File(this.last_hierarchy_loaded).getParentFile(), jar_url).exists()) {
                        jar_url = "file://" + new File(new File(this.last_hierarchy_loaded).getParentFile(), jar_url).getCanonicalPath();
                    }
                }
                urls[i] = new URL(jar_url);
            }
            ucl = new URLClassLoader(urls);
            for (URL url_in_list : ucl.getURLs()) {
                System.out.println("Searching for classes in " + url_in_list);
            }
        }
        if (null == class_name) {
            throw new Exception("AddExtraTab(" + tab + ") class_name == null.");
        }
        if (ucl != null) {
            cls = ucl.loadClass(class_name);
        } else {
            cls = ClassLoader.getSystemClassLoader().loadClass(class_name);
        }
        if (null == cls) {
            throw new Exception("AddExtraTab(" + tab + ") can not load class " + class_name);
        }
//        System.out.println("cls=" + cls);
        ExtraTabInfo eti = new ExtraTabInfo();
        eti.jp = (JPanel) cls.getDeclaredConstructor().newInstance();
        eti.cls = cls;
        eti.connect_method = cls.getMethod("Connect");
        eti.disconnect_method = cls.getMethod("Disconnect");
        eti.update_method = cls.getMethod("Update");
        if (null == tab_name) {
            tab_name = class_name;
        }
        eti.name = tab_name;
        eti.tab_number = jTabbedPane1.getTabCount();
        jTabbedPane1.addTab(eti.name, eti.jp);
        if (null == extraTabVector) {
            extraTabVector = new Vector<ExtraTabInfo>();
        }
        extraTabVector.add(eti);
        if (selected) {
            this.jTabbedPane1.setSelectedIndex(eti.tab_number);
        }
        if (this.jCheckBoxMenuItemConnected.getState() && null != eti.connect_method) {
            eti.connect_method.invoke(eti.jp);
        }
    }

    private void AddExtraAction(String action) {
        try {
            if (null != this.last_hierarchy_loaded && new File(this.last_hierarchy_loaded).exists()) {
                rcs.utils.URL_and_FileLoader.AddToSearchPath(new File(this.last_hierarchy_loaded).getParent());
            }
            String action_type = null;
            String action_name = null;
            String class_name = null;
            String method_name = "main";
            String args_string = "";
            String jar_url_list = null;
            if (action.startsWith("inline:")) {
                StringTokenizer tokenizer = new StringTokenizer(action.substring(7), ",");
                action_type = tokenizer.nextToken();
                action_name = tokenizer.nextToken();
                if (tokenizer.hasMoreTokens()) {
                    class_name = tokenizer.nextToken();
                }
                if (tokenizer.hasMoreTokens()) {
                    method_name = tokenizer.nextToken();
                }
                if (tokenizer.hasMoreTokens()) {
                    jar_url_list = tokenizer.nextToken();
                }
                while (tokenizer.hasMoreTokens()) {
                    args_string += tokenizer.nextToken();
                }
            } else {
                rcs.utils.URL_and_FileLoader ufl = new URL_and_FileLoader(action);
                if (!ufl.TryNameSucceeded) {
                    diagapplet.utils.DiagError.println("Can't open ExtraTab file " + action);
                    throw new Exception("Can't open ExtraTab file " + action);
                }
                String line = ufl.readLine();
                int line_count = 0;
                while (line != null) {
                    line_count++;
                    line = line.trim();
                    int eq_index = line.indexOf('=');
                    if (line.length() < 1) {
                        line = ufl.readLine();
                        continue;
                    } else if (line.charAt(0) == '#') {
                        line = ufl.readLine();
                        continue;
                    } else if (eq_index > 0) {
                        String var_name = line.substring(0, eq_index).trim();
                        String var_value = line.substring(eq_index + 1).trim();
                        if (var_name.compareTo("action_name") == 0) {
                            action_name = var_value;
                            line = ufl.readLine();
                            continue;
                        } else if (var_name.compareTo("action_type") == 0) {
                            action_type = var_value;
                            line = ufl.readLine();
                            continue;
                        } else if (var_name.compareTo("class_name") == 0) {
                            class_name = var_value;
                            line = ufl.readLine();
                            continue;
                        } else if (var_name.compareTo("method_name") == 0) {
                            method_name = var_value;
                            line = ufl.readLine();
                            continue;
                        } else if (var_name.compareTo("jar_url") == 0) {
                            if (null == jar_url_list) {
                                jar_url_list = "";
                            }
                            jar_url_list += ";" + var_value;
                            line = ufl.readLine();
                            continue;
                        } else if (var_name.compareTo("arg") == 0) {
                            args_string += " " + var_value;
                            line = ufl.readLine();
                            continue;
                        }
                    }
                    throw new Exception("Unrecognize line " + line + " in " + action + ":" + line_count);
                }
                ufl.close();
                ufl = null;
            }
            if (null != action_type && action_type.compareTo("java") == 0) {

                URLClassLoader ucl = null;
                Class cls = null;
                if (null != jar_url_list) {
                    StringTokenizer jar_url_list_tokenizer = new StringTokenizer(jar_url_list, ";");
                    Vector<URL> urlsv = new Vector<URL>();
                    int jar_url_list_length = jar_url_list_tokenizer.countTokens();
                    for (int i = 0; i < jar_url_list_length; i++) {
                        String jar_url = jar_url_list_tokenizer.nextToken();
                        if (jar_url.indexOf(':') < 0) {
                            if (jar_url.startsWith("ftp.")) {
                                jar_url = "ftp://" + jar_url;
                            } else if (jar_url.startsWith("www.")) {
                                jar_url = "http://" + jar_url;
                            } else if (jar_url.startsWith("/")) {
                                jar_url = "file://" + jar_url;
                            } else if (new File(jar_url).exists()) {
                                jar_url = "file://" + new File(jar_url).getCanonicalPath();
                            } else if (new File(new File(this.last_hierarchy_loaded).getParentFile(), jar_url).exists()) {
                                jar_url = "file://" + new File(new File(this.last_hierarchy_loaded).getParentFile(), jar_url).getCanonicalPath();
                            }
                        }
                        URL u = new URL(jar_url);
                        urlsv.add(u);
                        if (class_name == null) {
                            Manifest mf = (new JarInputStream(u.openConnection().getInputStream())).getManifest();
                            System.out.println("urls[i]=" + u + ", mf=" + mf + ", mf.getMainAttributes()=" + mf.getMainAttributes().keySet());
                            class_name = (String) mf.getMainAttributes().getValue(Attributes.Name.MAIN_CLASS);
                            String cp = mf.getMainAttributes().getValue(Attributes.Name.CLASS_PATH);
                            System.out.println("cp = " + cp);
                            StringTokenizer st = new StringTokenizer(cp, " ");
                            String jar_url_base = jar_url.substring(0, jar_url.lastIndexOf('/'));
                            while (st.hasMoreTokens()) {
                                String tok = st.nextToken();
                                URL u2 = new URL(jar_url_base + "/" + tok);
                                System.out.println("u2 = " + u2);
                                urlsv.add(u2);
                            }
                        }
                    }
                    URL urls[] = new URL[urlsv.size()];
                    for (int i = 0; i < urlsv.size(); i++) {
                        urls[i] = urlsv.elementAt(i);
                    }
                    System.out.println("urls = " + urls);
                    //Array new URL[jar_url_list_tokenizer.countTokens()];
                    ucl = new URLClassLoader(urls);

                    for (URL url_in_list : ucl.getURLs()) {
                        System.out.println("Searching for classes in " + url_in_list);
                    }
                }
                if (ucl != null) {
                    cls = ucl.loadClass(class_name);
                } else {
                    cls = ClassLoader.getSystemClassLoader().loadClass(class_name);
                }
                //System.out.println("cls=" + cls);
                Method method_array[] = cls.getMethods();
                Method temp_method = null;
                try {
                    temp_method = cls.getMethod(method_name);
                } catch (NoSuchMethodException nsme) {
                }
                if (null == temp_method) {
                    for (Method method_to_test : method_array) {
                        if (method_to_test.getDeclaringClass().equals(cls)
                                && (method_to_test.getModifiers() & Modifier.STATIC) == Modifier.STATIC) {
                            //System.out.println("method: " + method_to_test);
                            if (method_to_test.getName().compareTo(method_name) == 0) {
                                temp_method = method_to_test;
                            }
                        }
                    }
                }
                if (null == temp_method) {
                    throw new Exception("Could not find method for AddExtraAction(" + action + ")");
                }
                if ((temp_method.getModifiers() & Modifier.STATIC) != Modifier.STATIC) {
                    throw new Exception("Method for AddExtraAction(" + action + ") must be STATIC. temp_method =" + temp_method);
                }
                if (null != temp_method) {
                    StringTokenizer args_tokenizer = new StringTokenizer(args_string, " \n\r");
                    final String method_args[] = new String[args_tokenizer.countTokens() > 1 ? args_tokenizer.countTokens() : 1];
                    method_args[0] = "";
                    for (int i = 0; i < method_args.length && i < args_tokenizer.countTokens(); i++) {
                        method_args[i] = args_tokenizer.nextToken();
                    }
                    Class params[] = temp_method.getParameterTypes();
                    System.out.println(params);
//                if (params.length == 1) {
//                    System.out.println("params[0]=" + params[0]);
//                }
                    boolean has_string_array_arg = (params.length == 1
                            && params[0].isArray()
                            && params[0].equals(String[].class));
                    final Object method_arg = (has_string_array_arg ? (java.lang.Object) method_args : null);
                    final Method mtd = temp_method;
                    jMenuExtras.add(new JMenuItem(new AbstractAction(action_name) {

                        private Object obj = null;

                        @Override
                        public void actionPerformed(ActionEvent e) {
                            try {
                                Thread t = new Thread(new Runnable() {

                                    @Override
                                    public void run() {
                                        try {
                                            mtd.invoke(null, method_arg);
                                        } catch (Exception e) {
                                            PrintException(e);
                                        }
                                    }
                                });
                                t.start();
                            } catch (Exception excep) {
                                excep.printStackTrace();
                            }
                        }
                    }));
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }
    private String cmd_line = "";

    private void LoadInputHeader(String header) {
        System.out.println("LoadInputHeader(" + header + ")");
        if (null == this.curModule) {
            this.curModule = new ModuleInfo(cgc.get_diag_dict_creator(), cgc.get_nml_creator());
        }
        this.curModule.LoadAuxTypeFile(header);
        this.curModule.AddAllAuxMessages();
        if (null == auxBuffersHashtable) {
            auxBuffersHashtable = new Hashtable<String, BufferInfo>();
        }

        Enumeration aux_buffers_elements = this.auxBuffersHashtable.elements();
        while (aux_buffers_elements.hasMoreElements()) {
            BufferInfo bi = (BufferInfo) aux_buffers_elements.nextElement();
            if (null == bi.mi) {
                bi.mi = this.curModule;
            } else {
                bi.mi.AddAllAuxMessages();
            }
        }
    }

    private void parseArgs() {
        try {
            diagapplet.utils.DiagError.println("Id=" + IdString);
            diagapplet.utils.DiagError.println("rcs.RCS_VERSION.info_string=" + rcs.RCS_VERSION.info_string);
            if (null != args && args.length > 0) {
                for (String arg : args) {
                    cmd_line += " " + arg;
                }
            }
            diagapplet.utils.DiagError.println("cmd_line=" + cmd_line);
            URLClassLoader ucl = null;
            String panel_name = null;
            //System.out.println("args=" + args + ", args.length=" + args.length);
            if (null != args && args.length > 0) {
                for (String arg : args) {
                    try {
                        //System.out.println("arg=" + arg);
                        if (arg.indexOf('=') < 0 && !arg.startsWith("-")) {
                            if (arg.endsWith(".diag") || arg.endsWith(".cfg") || arg.endsWith(".nml")) {
                                LoadHierarchy(arg);
                                add_recognized_arg(arg);
                            }
                        } else if (arg.startsWith("panel_jar=")) {
                            try {
                                String jar_url = arg.substring(10);
                                URL urls[] = new URL[1];
                                urls[0] = new URL(jar_url);
                                ucl = new URLClassLoader(urls);
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("panel_name=")) {
                            try {
                                panel_name = arg.substring(11);
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("panel_class=")) {
                            try {
                                String cls_name = arg.substring(12);
                                Class cls = null;
                                if (ucl != null) {
                                    cls = ucl.loadClass(cls_name);
                                } else {
                                    cls = ClassLoader.getSystemClassLoader().loadClass(cls_name);
                                }
                                ExtraTabInfo eti = new ExtraTabInfo();
                                eti.jp = (JPanel) cls.getDeclaredConstructor().newInstance();
                                eti.cls = cls;
                                eti.connect_method = cls.getMethod("Connect");
                                eti.disconnect_method = cls.getMethod("Disconnect");
                                eti.update_method = cls.getMethod("Update");
                                if (null == panel_name) {
                                    panel_name = cls_name;
                                }
                                eti.name = panel_name;
                                eti.tab_number = jTabbedPane1.getTabCount();
                                jTabbedPane1.addTab(eti.name, eti.jp);
                                if (null == extraTabVector) {
                                    extraTabVector = new Vector<ExtraTabInfo>();
                                }
                                extraTabVector.add(eti);
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("extra_action=")) {
                            try {
                                AddExtraAction(arg.substring(13));
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("extra_panel=")) {
                            try {
                                AddExtraTab(arg.substring(12));
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("input_header=")) {
                            try {
                                LoadInputHeader(arg.split("=")[1]);
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            add_recognized_arg(arg);
                        } else if (arg.startsWith("--help")) {
                            LoadParameters();
                            PrintHelp();
                            System.exit(0);
                        }
                    } catch (Exception e) {
                        PrintException(e);
                    }
                }
                LoadParameters();
                if (recognized_argsV == null
                        || args.length > recognized_argsV.size()) {
                    PrintUnrecognizedArgs();
                    PrintHelp();
                    System.exit(1);
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void PrintUnrecognizedArgs() {
        if (null != args && args.length > 0) {
            for (String arg : args) {
                if (null == recognized_argsV
                        || !recognized_argsV.contains(arg)) {
                    diagapplet.utils.DiagError.println("Unrecognized command line argument : " + arg);
                }
            }
        }
    }

    String GetParameter(String paramName) {
        if (null != args) {
            boolean return_next_arg = false;
            for (String arg : args) {
                if (arg.startsWith("--" + paramName + "=")
                        || arg.startsWith(paramName + "=")) {
                    add_recognized_arg(arg);
                    return arg.substring(arg.indexOf('=') + 1);
                } else if (arg.compareTo("--" + paramName) == 0) {
                    add_recognized_arg(arg);
                    return_next_arg = true;
                    continue;
                } else if (return_next_arg) {
                    add_recognized_arg(arg);
                    return arg;
                }
            }
        }
        if (null != cgc) {
            String s = cgc.GetParameter(paramName, null);
            if (s != null) {
                return s;
            }
        }
        return null;
    }

    static private class ParamHelpInfo {

        String param;
        String paramHelp;
        String default_value_string;
        Class cls;

        public ParamHelpInfo(String _param, String _paramHelp, String _default_value_string, Class _cls) {
            param = _param;
            paramHelp = _paramHelp;
            default_value_string = _default_value_string;
            cls = _cls;
        }
    };
    static private Hashtable<String, ParamHelpInfo> paramHelpInfoHashtable = null;

    static private void PrintHelp() {
        System.out.println("Usage: <option1=val1> <option2=val2> . . .  [Diag_FILE]");
        System.out.println("Posible options:\n");
        if (null != paramHelpInfoHashtable) {
            for (ParamHelpInfo phi : paramHelpInfoHashtable.values()) {
                System.out.println("\t" + phi.param + "\t -- \t" + phi.paramHelp + "\n\t\t default:" + phi.default_value_string + "\tType :\t" + phi.cls.getName());
            }
        }
        System.out.println("");
    }

    static private void addParamInfo(String _param, String _paramHelp, String _default_value_string, Class _cls) {
        if (paramHelpInfoHashtable == null) {
            paramHelpInfoHashtable = new Hashtable<String, ParamHelpInfo>();
        }
        if (!paramHelpInfoHashtable.contains(_param)) {
            paramHelpInfoHashtable.put(_param, new ParamHelpInfo(_param, _paramHelp, _default_value_string, _cls));
        }
    }

    int GetIntegerParam(String paramName, String paramHelp, int default_value) {
        addParamInfo(paramName, paramHelp, Integer.toString(default_value), Integer.class);
        String s = GetParameter(paramName);
        if (s != null) {
            return Integer.valueOf(s).intValue();
        }
        return default_value;
    }

    String GetStringParam(String paramName, String paramHelp, String default_value) {
        addParamInfo(paramName, paramHelp, default_value, String.class);
        String s = GetParameter(paramName);
        if (s != null) {
            return s;
        }
        return default_value;
    }

    double GetDoubleParam(String paramName, String paramHelp, double default_value) {
        addParamInfo(paramName, paramHelp, Double.toString(default_value), Double.class);
        String s = GetParameter(paramName);
        if (s != null) {
            return Double.valueOf(s).doubleValue();
        }
        return default_value;
    }

    boolean GetBooleanParam(String paramName, String paramHelp, boolean default_value) {
        addParamInfo(paramName, paramHelp, Boolean.toString(default_value), Boolean.class);
        String s = GetParameter(paramName);
        if (s != null) {
            return Boolean.valueOf(s).booleanValue();
        }
        return default_value;
    }

    private void LoadParameters() {
        try {
            HierarchyDraw.MODULE_WIDTH = GetIntegerParam("MODULE_WIDTH", "Width in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_WIDTH);
            HierarchyDraw.MODULE_HEIGHT = GetIntegerParam("MODULE_HEIGHT", "height in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_HEIGHT);
            HierarchyDraw.MODULE_XOFFSET = GetIntegerParam("MODULE_XOFFSET", "x offset in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_XOFFSET);
            HierarchyDraw.MODULE_YOFFSET = GetIntegerParam("MODULE_YOFFSET", "y offset in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_YOFFSET);
            HierarchyDraw.MODULE_X_SPACING = GetIntegerParam("MODULE_X_SPACING", "y spacing between modules in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_X_SPACING);
            HierarchyDraw.MODULE_Y_SPACING = GetIntegerParam("MODULE_Y_SPACING", "y spacing between modules  in pixels for each module in hierarchy view.", HierarchyDraw.MODULE_Y_SPACING);
            double refreshTime = GetDoubleParam("refreshTime", "period as a float in seconds to wait between refreshes.", ((double) refreshTime_millis) / 1000.0);
            refreshTime_millis = (long) (refreshTime * 1000.0);
            list_modules_by_number = GetBooleanParam("ListModulesByNumber", "sort children of each module by module number", list_modules_by_number);
            ModuleInfo.no_errlog = GetBooleanParam("no_errlog", "Assume there is no errlog buffer or ignore it if it exists.", ModuleInfo.no_errlog);
            final boolean origConnectOnStartup = connectOnStartup;
            connectOnStartup = GetBooleanParam("ConnectOnStartup", "connect immediately after startup", connectOnStartup);
            if (origConnectOnStartup == connectOnStartup) {
                connectOnStartup = GetBooleanParam("connectOnStartup", "connect immediately after startup", connectOnStartup);
            }
            int selectTab = GetIntegerParam("SelectTab", "select a tab at startup", -1);
            if (selectTab >= 0 && selectTab < jTabbedPane1.getTabCount()) {
                jTabbedPane1.setSelectedIndex(selectTab);
                tab_selected = true;
            }
            param_set_diag_minimized = GetBooleanParam("DIAG_MINIMIZED", "minimize gui at startup", param_set_diag_minimized);
            param_set_diag_auto_connect_disconnect = GetBooleanParam("DIAG_AUTO_CONNECT_DISCONNECT", "connect to NML channels only as needed and disconnect as soon as not needed", param_set_diag_auto_connect_disconnect);
            if (null != jCheckBoxMenuItemAutoConnectDisconnectAsNeeded) {
                jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.setSelected(param_set_diag_auto_connect_disconnect);
            }
            input_headers = GetStringParam("input_headers", "C++ header files to load for message structure info.", "");
            message_files = GetStringParam("message_files", "Packed or XML message files to open on startup.", "");
            diagapplet.utils.DiagError.println("LoadParameters() finished. : selectTab=" + selectTab + ", param_set_diag_auto_connect_disconnect=" + param_set_diag_auto_connect_disconnect + ", connectOnStartup=" + connectOnStartup);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void SelectNmlTableRow(int r) {
        try {
            if (r < 0 || r > jTableNML.getRowCount()) {
                return;
            }
            final String bufName = (String) jTableNML.getValueAt(r, 1);
            if (null == bufName) {
                PrintError("jTableNML.getValueAt(" + r + ", 1) returned null.");
                return;
            }
            NmlInfo ni = nml_hashtable.get(bufName);
            if (null == ni) {
                String s = "nml_hashtable.get(" + bufName + ") returned null\n\nnml_hashtable.keys()=";
                for (String k : nml_hashtable.keySet()) {
                    s += "\t" + k + ",\n";
                }
                this.jTextPaneNmlError.setText(s);
                return;
            }
            if (null == ni.nml) {
                this.jTextPaneNmlError.setText("for " + bufName + " : ni.nml == null");
                return;
            }
            jLabelNMLBufferLine.setText(ni.nml.getBufferLine());
            jLabelNMLConfigFile.setText(ni.nml.get_configuration_file());
            String es = ni.nml.get_last_exception_string();
            if (es == null) {
                es = "";
            }
            this.jTextPaneNmlError.setText(es);
            if (null != this.auxBuffersHashtable
                    && null != this.jListAuxChannels
                    && this.auxBuffersHashtable.containsKey(bufName)) {
                no_select_aux_channel_load_nml_table = true;
                this.jListAuxChannels.setSelectedValue(bufName, true);
                this.curAuxBuffer = this.auxBuffersHashtable.get(bufName);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void SetIcon() {
        try {
            java.net.URL icon_url = this.getClass().getClassLoader().getResource("diagapplet/diagicon.png");
            if (null != icon_url) {
                this.setIconImage(javax.imageio.ImageIO.read(icon_url.openStream()));
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void DeleteDeleteMePlotTrackers() {
        try {
            boolean deleteme_found = true;
            while (deleteme_found) {
                deleteme_found = false;
                for (PlotTracker pt : plot_tracker_hashtable.values()) {
                    if (pt.plot_data.delete_me) {
                        plot_tracker_hashtable.remove(pt.plot_data.name);
                        deleteme_found = true;
                        break;
                    }
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }
    private static final String last_dir_eq_string = "last_dir=";
    private static final String last_nml_dir_eq_string = "last_nml_dir=";
    private static final String last_header_dir_eq_string = "last_header_dir=";
    private static final String last_dirs_filename = ".diag_last_dirs.txt";

    private void ReadLastDirs() {
        try {
            File lastDirFile = new File(System.getProperty("user.home"), last_dirs_filename);
            if (!lastDirFile.exists()) {
                return;
            }
            BufferedReader br = new BufferedReader(new FileReader(lastDirFile));
            String line = "";
            while (line != null) {
                line = br.readLine();
                if (line == null) {
                    break;
                }
                if (line.startsWith(last_dir_eq_string)) {
                    last_dir = new File(line.substring(last_dir_eq_string.length()));
                    if (!last_dir.exists() || !last_dir.canRead() || !last_dir.isDirectory()) {
                        last_dir = null;
                    }
                    continue;
                }
                if (line.startsWith(last_nml_dir_eq_string)) {
                    last_nml_dir = new File(line.substring(last_nml_dir_eq_string.length()));
                    if (!last_nml_dir.exists() || !last_nml_dir.canRead() || !last_nml_dir.isDirectory()) {
                        last_nml_dir = null;
                    }
                    continue;
                }
                if (line.startsWith(last_nml_dir_eq_string)) {
                    last_nml_dir = new File(line.substring(last_nml_dir_eq_string.length()));
                    if (!last_nml_dir.exists() || !last_nml_dir.canRead() || !last_nml_dir.isDirectory()) {
                        last_nml_dir = null;
                    }
                    continue;
                }
                if (line.startsWith(last_header_dir_eq_string)) {
                    last_header_dir = new File(line.substring(last_header_dir_eq_string.length()));
                    if (!last_header_dir.exists() || !last_header_dir.canRead() || !last_header_dir.isDirectory()) {
                        last_header_dir = null;
                    }
                    continue;
                }
            }
            br.close();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void WriteLastDirs() {
        try {
            if (null == last_dir) {
                return;
            }
            File lastDirFile = new File(System.getProperty("user.home"), last_dirs_filename);
            PrintStream ps = new PrintStream(new FileOutputStream(lastDirFile));
            if (this.last_dir.exists() && this.last_dir.canRead() && this.last_dir.isDirectory()) {
                ps.println(last_dir_eq_string + this.last_dir.getAbsolutePath());
            }
            if (null != this.last_nml_dir && this.last_nml_dir.exists() && this.last_nml_dir.canRead() && this.last_nml_dir.isDirectory()) {
                ps.println(last_nml_dir_eq_string + this.last_nml_dir.getAbsolutePath());
            }
            if (null != this.last_header_dir && this.last_header_dir.exists() && this.last_header_dir.canRead() && this.last_header_dir.isDirectory()) {
                ps.println(last_header_dir_eq_string + this.last_header_dir.getAbsolutePath());
            }
            ps.close();
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }

    @SuppressWarnings("unchecked")
    private void ReadRecentVector() {
        recentFile = new File(System.getProperty("user.home"), ".diag_recent_hierarchies.xml");
        try {
            this.ReadLastDirs();
            if (recentFile.exists() && recentFile.canRead()) {
                XMLDecoder decoder = new XMLDecoder(
                        new BufferedInputStream(
                        new FileInputStream(recentFile)));
                recentVector = (Vector<String>) decoder.readObject();
                decoder.close();
                for (String s : recentVector) {
                    JMenuItem jmi = new JMenuItem(s);
                    jmi.addActionListener(new ActionListener() {

                        @Override
                        public void actionPerformed(ActionEvent e) {
                            LoadHierarchy(((JMenuItem) e.getSource()).getText());
                        }
                    });
                    jMenuFileRecent.add(jmi);
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
        if (null == recentVector) {
            recentVector = new Vector<String>();
        }
    }

    private Vector<plotTrackerPreserve> PlotTrackersToPlotList() {
        if (null == plot_tracker_hashtable) {
            return null;
        }
        Vector<plotTrackerPreserve> PlotList = new Vector<plotTrackerPreserve>();
        for (PlotTracker pt : plot_tracker_hashtable.values()) {
            plotTrackerPreserve ptP = new plotTrackerPreserve();
            if (null != pt.auxBufferInfo) {
                ptP.setBufferName(pt.auxBufferInfo.Name);
            }
            if (null != pt.module) {
                ptP.setModuleName(pt.module.Name);
            }
            if (null != pt.structName) {
                ptP.setStiName(pt.structName);
            }
            ptP.setVariableName(pt.varName);
            ptP.setIsArray(pt.array_type);
            ptP.setIsAux(pt.is_aux_channel);
            ptP.setIsCmd(pt.is_cmd_value);
            ptP.setVariableNumber(pt.var_number);
            ptP.setMsgType(pt.msg_type);
            PlotList.add(ptP);
        }
        return PlotList;
    }

    private plotSetPreserve getCurrentPlotSet() {
        plotSetPreserve psP = new plotSetPreserve();
        psP.setPlotList(PlotTrackersToPlotList());
        psP.setGraphFunction(plotter_NB_UI1.getGraphFunction());
        psP.setFuncArg(plotter_NB_UI1.getFuncArg());
        return psP;
    }
    protected boolean Automatically_Keep_and_Use_PlotSets;

    /**
     * Get the value of Automatically_Keep_and_Use_PlotSets
     *
     * @return the value of Automatically_Keep_and_Use_PlotSets
     */
    public boolean isAutomatically_Keep_and_Use_PlotSets() {
        return Automatically_Keep_and_Use_PlotSets;
    }

    /**
     * Set the value of Automatically_Keep_and_Use_PlotSets
     *
     * @param Automatically_Keep_and_Use_PlotSets new value of Automatically_Keep_and_Use_PlotSets
     */
    public void setAutomatically_Keep_and_Use_PlotSets(boolean Automatically_Keep_and_Use_PlotSets) {
        this.Automatically_Keep_and_Use_PlotSets = Automatically_Keep_and_Use_PlotSets;
    }

    private void SaveDiagPreserve() {
        this.WriteLastDirs();
        shutting_down = true;
        diagapplet.utils.DiagError.shutting_down = true;
        if (null != dp && null != last_hierarchy_loaded) {
            try {
                dp.setAutomatically_Keep_and_Use_PlotSets(this.isAutomatically_Keep_and_Use_PlotSets());
                dp.setAuto_connect_disconnect(this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected());
                dp.setSelectedTab(jTabbedPane1.getSelectedIndex());
                dp.setDefaultNMLConfigFile(ModuleInfo.DefaultNMLConfigurationFile);
                Hashtable ht = cgc.get_m_modulesHashTable();
                if (null != ht) {
                    Vector<modulePreserve> modules_vector = new Vector<modulePreserve>();
                    for (Object obj : ht.values()) {
                        ModuleInfo mi = (ModuleInfo) obj;
                        if (null != mi.previous_commands) {
                            modulePreserve mp = new modulePreserve();
                            Hashtable<Long, String> PreviousCommands = new Hashtable<Long, String>();
                            PreviousCommands.putAll(mi.previous_commands);
                            mp.setPreviousCommands(PreviousCommands);
                            mp.setName(mi.Name);
                            mp.setLastSelectedCommandIndex(mi.last_selected_command_index);
                            modules_vector.add(mp);
                        }
                    }
                    dp.setModulesVector(modules_vector);
                }
                if (null != this.auxBuffersHashtable) {
                    Vector<auxBufferPreserve> aux_vector = new Vector<auxBufferPreserve>();
                    for (BufferInfo bi : this.auxBuffersHashtable.values()) {
                        auxBufferPreserve abp = new auxBufferPreserve();
                        abp.setName(bi.Name);
                        Hashtable<Long, String> previousMessages = new Hashtable<Long, String>();
                        final Hashtable bpmht = bi.getPreviousMessagesHashtable();
                        if (null != bpmht) {
                            previousMessages.putAll(bpmht);
                        }
                        abp.setPreviousMessages(previousMessages);
                        abp.setLastSelectedMessageIndex(bi.last_selected_msg_index);
                        aux_vector.add(abp);
                    }
                    dp.setAuxBuffersVector(aux_vector);
                }
                if (this.Automatically_Keep_and_Use_PlotSets) {
                    if (null != plot_tracker_hashtable) {
                        dp.setPlotSet(getCurrentPlotSet());
                    } else {
                        dp.setPlotSet(null);
                    }
                } else {
                    plot_tracker_hashtable = null;
                    dp.setPlotSet(null);
                }
                diagPreserve dp_to_save = dp;
                dp = null;
                File f = new File(last_hierarchy_loaded);
                String sbase = f.getName();
                int pindex = sbase.indexOf('.');
                if (pindex > 0) {
                    sbase = sbase.substring(0, pindex);
                }
                File homeDir = new File(System.getProperty("user.home"));
                File fsave = new File(System.getProperty("user.home"), ".diag_" + sbase + ".xml");
                XMLEncoder encoder = new XMLEncoder(
                        new BufferedOutputStream(
                        new FileOutputStream(fsave)));
                encoder.writeObject(dp_to_save);
                encoder.close();
                this.WriteLastDirs();
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                DisconnectFromAll();
            }
        }
    } // end SaveDiagPreserve()

    @Override
    protected void finalize() throws Throwable {
        try {
            shutting_down = true;
            diagapplet.utils.DiagError.shutting_down = true;
            rcs.nml.NMLConnection.do_not_print_errors = true;
            System.out.println("Calling finalize.");
            SaveDiagPreserve();
            WriteLastDirs();
        } catch (Exception e) {
            PrintException(e);
        }
        super.finalize();
    }

    private void ClearPlots() {
        try {
            if (null != plot_tracker_hashtable) {
                for (PlotTracker plot_tracker : plot_tracker_hashtable.values()) {
                    if (Thread.interrupted()) {
                        return;
                    }
                    if (plot_tracker.nml_for_get_single_var_log != null) {
                        plot_tracker.nml_for_get_single_var_log.closeSingleVarLog(plot_tracker.nml_single_var_log_number);
                        plot_tracker.nml_for_get_single_var_log.disconnect();
                        plot_tracker.nml_for_get_single_var_log = null;
                    }
                }
            }
            for (BufferInfo bi : auxBuffersHashtable.values()) {
                bi.max_plot_var_number = -1;
            }
            for (Object obj : cgc.get_m_modulesHashTable().values()) {
                ModuleInfo mi = (ModuleInfo) obj;
                mi.max_cmd_var_number = -1;
                mi.max_stat_var_number = -1;
            }
            AutoSavePlotSet();
        } catch (Exception e) {
            PrintException(e);
        } finally {
            plot_tracker_hashtable = null;
            fit_done = false;
            total_point_count = 0;
            last_total_point_count = -1;
            first_nml_single_var_log_timestamp_set = false;
            first_nml_single_var_log_timestamp = 0.0;
        }
    }

    private void UpdatePlotCmdVarButton() {
        if (!this.watchJPanelDetailsCmd.GetIsNewStruct()) {
            this.jButtonPlotCmdVar.setText("Plot the variable " + cmd_selected_var_name + "  (#" + cmd_selected_var_number + ")");
            this.jButtonPlotCmdVar.setEnabled(true);
        } else {
            this.jButtonPlotCmdVar.setText("Plot variable. (disabled)");
            this.jButtonPlotCmdVar.setEnabled(false);
        }
    }

    private void UpdatePlotCmdArrayButton() {
        if (!this.watchJPanelDetailsCmd.GetIsNewStruct()
                && this.watchJPanelDetailsCmd.GetIsArray()) {
            this.jButtonPlotCmdArray.setText("Plot the array " + cmd_selected_var_name + "  (#" + cmd_selected_var_number + ")");
            this.jButtonPlotCmdArray.setEnabled(true);
        } else {
            this.jButtonPlotCmdArray.setText("Plot array. (disabled)");
            this.jButtonPlotCmdArray.setEnabled(false);
        }
    }

    private void UpdatePlotStatusVarButton() {
        if (!this.watchJPanelDetailsStatus.GetIsNewStruct()) {
            this.jButtonPlotStatusVar.setText("Plot the variable " + status_selected_var_name + "  (#" + status_selected_var_number + ")");
            this.jButtonPlotStatusVar.setEnabled(true);
        } else {
            this.jButtonPlotStatusVar.setText("Plot variable. (disabled)");
            this.jButtonPlotStatusVar.setEnabled(false);
        }
    }

    private void UpdatePlotStatusArrayButton() {
        if (!this.watchJPanelDetailsStatus.GetIsNewStruct()
                && this.watchJPanelDetailsStatus.GetIsArray()) {
            this.jButtonPlotStatusArray.setText("Plot the array " + status_selected_var_name + "  (#" + status_selected_var_number + ")");
            this.jButtonPlotStatusArray.setEnabled(true);
        } else {
            this.jButtonPlotStatusArray.setText("Plot array. (disabled)");
            this.jButtonPlotStatusArray.setEnabled(false);
        }
    }

    private void UpdatePlotAuxViewVarButton() {
        if (!this.watchJPanelAuxView.GetIsNewStruct()) {
            this.jButtonPlotAuxVar.setText("Plot the variable " + aux_selected_var_name + "  (#" + aux_selected_var_number + ")");
            if (this.curAuxBuffer != null && this.curAuxBuffer.read_nml != null && curAuxBuffer.read_nml.is_connected()) {
                this.jButtonPlotAuxVar.setEnabled(true);
            }
        } else {
            this.jButtonPlotAuxVar.setText("Plot variable. (disabled)");
            this.jButtonPlotAuxVar.setEnabled(false);
        }
    }

    private void UpdatePlotAuxViewArrayButton() {
        if (!this.watchJPanelAuxView.GetIsNewStruct()
                && this.watchJPanelAuxView.GetIsArray()) {
            this.jButtonPlotAuxArray.setText("Plot the array " + aux_selected_var_name + "  (#" + aux_selected_var_number + ")");
            if (this.curAuxBuffer != null && this.curAuxBuffer.read_nml != null && curAuxBuffer.read_nml.is_connected()) {
                this.jButtonPlotAuxArray.setEnabled(true);
            }
        } else {
            this.jButtonPlotAuxArray.setText("Plot array. (disabled)");
            this.jButtonPlotAuxArray.setEnabled(false);
        }
    }

    private void StartPlottingArray(StructureTypeInfo ti, BufferInfo bi, ModuleInfo mi, long msg_type, int variable_number, String variable_name, boolean is_cmd, boolean is_aux) {
        try {
            //String plot_tracker.name = "";
            PlotTracker plot_tracker = new PlotTracker();
            plot_tracker.plot_data = new PlotData();
            plot_tracker.var_number = variable_number;
            plot_tracker.module = mi;
            plot_tracker.auxBufferInfo = bi;
            plot_tracker.varName = variable_name;
            plot_tracker.structName = ti.getName();
            int index = variable_name.indexOf('=');
            if (index > 0) {
                variable_name = variable_name.substring(0, index);
            }
            index = variable_name.lastIndexOf(' ');
            if (index > 0) {
                variable_name = variable_name.substring(index + 1);
            }
            plot_tracker.is_cmd_value = is_cmd;
            plot_tracker.is_aux_channel = is_aux;
            plot_tracker.msg_type = msg_type;
            if (is_aux) {
                plot_tracker.name = bi.Name + "." + variable_name + "[]";
                if (variable_number > bi.max_plot_var_number) {
                    bi.max_plot_var_number = variable_number;
                }
                bi.has_been_plotted = false;
                if (null != bi.read_nml) {
                    bi.read_nml.set_last_id_read(-1);
                }
            } else {
                if (is_cmd) {
                    plot_tracker.name = mi.Name + ".cmd." + variable_name + "[]";
                    if (variable_number > mi.max_cmd_var_number) {
                        mi.max_cmd_var_number = variable_number;
                    }
                } else {
                    plot_tracker.name = mi.Name + ".stat." + variable_name + "[]";
                    if (variable_number > mi.max_stat_var_number) {
                        mi.max_stat_var_number = variable_number;
                    }
                }
            }
            index = variable_name.indexOf('[');
            String slen_var = variable_name.substring(0, index) + "_length";
            while (index > 0) {
                int index2 = variable_name.indexOf(']', index);
                if (index2 < 0) {
                    break;
                }
                variable_name = variable_name.substring(0, index) + "[]" + variable_name.substring(index2 + 1);
                index = variable_name.indexOf('[', index + 1);
            }
            plot_tracker.array_type = true;
            //String s2 = variable_name;
            int i = 1;
            boolean first_found = false;
            boolean second_found = false;
            boolean variable_number_found = false;
            ti.startInfoTokens();
            STI_TokenizerInterface stiti = ti.getInfoTokenizer();
            String first_found_string = null;
            String second_found_string = null;
            String break_string = null;
            while (stiti.hasMoreTokens()) {
                i++;
                String s = stiti.nextToken();
                if (s.indexOf(slen_var) >= 0) {
                    plot_tracker.ndla_length_var_num = i;
                }
                if (is_aux) {
                    if (plot_tracker.ndla_length_var_num > plot_tracker.auxBufferInfo.max_plot_var_number) {
                        plot_tracker.auxBufferInfo.max_plot_var_number = plot_tracker.ndla_length_var_num;
                    }
                }
                if (second_found && plot_tracker.skip_size > 1
                        && ((i - plot_tracker.min_var_num) % plot_tracker.skip_size) != 0) {
                    continue;
                }
                index = s.indexOf('[');
                while (index > 0) {
                    int index2 = s.indexOf(']', index);
                    if (index2 < 0) {
                        break;
                    }
                    s = s.substring(0, index) + "[]" + s.substring(index2 + 1);
                    index = s.indexOf('[', index + 1);
                }
                int spc_index;
                String orig_s = s;
                spc_index = s.lastIndexOf(' ');
                if (spc_index > 0) {
                    s = s.substring(spc_index);
                    s = s.trim();
                }
                if (s.equals(variable_name)) {
                    if (!first_found) {
                        first_found = true;
                        first_found_string = s;
                        if (orig_s.indexOf("NML_DYNAMIC_LENGTH_ARRAY") >= 0) {
                            plot_tracker.ndla = true;
                        }
                        plot_tracker.min_var_num = i;
                    } else if (!second_found) {
                        second_found = true;
                        second_found_string = s;
                        plot_tracker.skip_size = i - plot_tracker.min_var_num;
                    }
                    if (i == variable_number) {
                        variable_number_found = true;
                    }
                } else if (second_found) {
                    break_string = s;
                    plot_tracker.max_var_num = i;
                    break;
                }
            }
            if (plot_tracker.max_var_num < plot_tracker.min_var_num) {
                plot_tracker.max_var_num = i;
            }
            if (is_aux) {
                if (plot_tracker.max_var_num > plot_tracker.auxBufferInfo.max_plot_var_number) {
                    plot_tracker.auxBufferInfo.max_plot_var_number = plot_tracker.max_var_num;
                }
            } else if (is_cmd) {
                if (plot_tracker.max_var_num > plot_tracker.module.max_cmd_var_number) {
                    plot_tracker.module.max_cmd_var_number = plot_tracker.max_var_num;
                }
            } else {
                if (plot_tracker.max_var_num > plot_tracker.module.max_stat_var_number) {
                    plot_tracker.module.max_stat_var_number = plot_tracker.max_var_num;
                }
            }

            plotter_NB_UI1.AddArrayPlot(plot_tracker.plot_data, plot_tracker.name);
            if (null == plot_tracker_hashtable) {
                plot_tracker_hashtable = new Hashtable<String, PlotTracker>();
            }
            plot_tracker_hashtable.put(plot_tracker.name, plot_tracker);
            plot_tracker.array_size = (plot_tracker.max_var_num - plot_tracker.min_var_num) / plot_tracker.skip_size;
            plot_tracker.plot_data.set_size(plot_tracker.array_size);
            if (is_aux) {
                this.jButtonPlotAuxArray.setText("Plot array. (already plotted)");
                this.jButtonPlotAuxArray.setEnabled(false);
            } else if (is_cmd) {
                this.jButtonPlotCmdArray.setText("Plot array. (already plotted)");
                this.jButtonPlotCmdArray.setEnabled(false);
            } else {
                this.jButtonPlotStatusArray.setText("Plot array. (already plotted)");
                this.jButtonPlotStatusArray.setEnabled(false);
            }
            //plotter_NB_UI1.checkComboBoxFunc(-1);
        } catch (Exception e) {
            diag_common.ErrorPrint("Error trying to start plotting for " + variable_name);
            PrintException(e);
        }
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
    private boolean update_current_status_next = false;
    private int last_next_bg_queue_size = 0;

    synchronized private void NextBackgroundSwingWorker() {
        try {
            bgNextExecuteThread = Thread.currentThread();
            bgNextExecuteStackTrace = bgNextExecuteThread.getStackTrace();
            bgNextExecute_time = System.currentTimeMillis();
            if (this.backgroundSwingWorkerQueue == null) {
                last_next_bg_queue_size = 0;
                this.ExecutingBackgroundSwingWorker = null;
                return;
            }
            last_next_bg_queue_size = this.backgroundSwingWorkerQueue.size();
            if (this.backgroundSwingWorkerQueue.size() < 1) {
                this.backgroundSwingWorkerQueue = null;
                this.ExecutingBackgroundSwingWorker = null;
                return;
            }
            if (this.backgroundSwingWorkerQueue.size() < 1) {
                this.backgroundSwingWorkerQueue = null;
                return;
            }
            this.ExecuteBackgroundSwingWorkerImmediate(this.backgroundSwingWorkerQueue.remove());
        } catch (Exception ex) {
            PrintException(ex);
        }
    }

    private String LoadCurrentStatus_Background_Do() {
        try {
            if (status_to_send_set_count != status_to_send_sent_count
                    && statusToSendString != null) {
                String tmpString = statusToSendString;
                statusToSendString = null;
                status_to_send_sent_count = status_to_send_set_count;
                curModule.writeStat(tmpString);
            }
            String statusDataString = curModule.updateStatData();
            return (statusDataString);
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }

    private void LoadCurrentStatus_Background_Done(String statusDataString) {
        try {
            update_current_status_next = false;
            if (null != statusDataString
                    || (last_status_id_long < 0 && curModule.statData != null)) {
                if (null == statusDataString) {
                    statusDataString = curModule.statData;
                }
                String sData2 = replaceDoubleCommas(statusDataString);
                StringTokenizer stData = new StringTokenizer(sData2, ",");
                String idString = stData.nextToken().trim();
                Long idLong = Long.valueOf(idString);
                boolean new_type = false;
                if (last_status_id_long < 1
                        || idLong.longValue() != last_status_id_long) {
                    new_type = true;
                }
                if (new_type) {
                    if (null == ModuleInfo.m_stat_structInfoHashTable) {
                        ModuleInfo.m_stat_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
                    }
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_stat_structInfoHashTable.get(idLong);
                    Hashtable ht = ModuleInfo.m_stat_structInfoHashTable;
                    if (typeInfo.conflicts) {
                        if (null != curModule
                                && null != curModule.get_conflict_m_structInfoHashTable()
                                && null != curModule.get_conflict_m_structInfoHashTable().get(idLong)) {
                            typeInfo = (StructureTypeInfo) curModule.get_conflict_m_structInfoHashTable().get(idLong);
                            ht = curModule.get_conflict_m_structInfoHashTable();
                        }
                    }
                    if (null != typeInfo) {
                        watchJPanelDetailsStatus.setNmlMessageDictionary(curModule.get_stat_msg_dict());
                        watchJPanelDetailsStatus.set_hashtable_by_id(ModuleInfo.m_stat_structInfoHashTable);
                        watchJPanelDetailsStatus.SetTypeInfo(typeInfo,
                                ModuleInfo.m_structInfoByNameHashTable);
                    }
                    jTextFieldCurrentStatusType.setText(typeInfo.getName());
                    last_status_id_long = idLong.longValue();
                    curModule.stat_msg_type = last_status_id_long;
                    jButtonPlotStatusVar.setText("Plot variable. (disabled)");
                    jButtonPlotStatusVar.setEnabled(false);
                    jButtonPlotStatusArray.setText("Plot array. (disabled)");
                    jButtonPlotStatusArray.setEnabled(false);
                }
                stData.nextToken(); // throw away size
                if (System.currentTimeMillis() - last_stats_count_update_time > 500) {
                    jLabelStatusRecvd.setText(Integer.toString(curModule.m_stat_read_Connection.get_last_id_read()));
                    last_stats_count_update_time = System.currentTimeMillis();
                }
                watchJPanelDetailsStatus.SetDataInfo(stData);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void LoadCurrentStatus() {
        try {
            if (!this.backgroundQueueEmpty()) {
                return;
            }
            if (null == curModule) {
                return;
            }
            SwingWorker backgroundSwingWorkerToRun =
                    new SwingWorker<String, Void>() {

                        @Override
                        public String doInBackground() {
                            return LoadCurrentStatus_Background_Do();
                        }

                        @Override
                        protected void done() {
                            try {
                                LoadCurrentStatus_Background_Done(get());
                            } catch (Exception ex) {
                                PrintException(ex);
                            }
                            NextBackgroundSwingWorker();
                        }
                    };
            ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void SendPreviouslySetCmdToSend() {
        if (cmd_to_send_set_count != cmd_to_send_sent_count
                && cmdToSendModule != null
                && cmdToSendString != null) {
            String tmpString = cmdToSendString;
            int orig_cmd_to_send_sent_count = cmd_to_send_set_count;
            int orig_cmds_sent = cmdToSendModule.commands_sent;
            ModuleInfo origCmdToSendModule = cmdToSendModule;
            cmdToSendString = null;
            cmdToSendModule = null;
            cmd_to_send_sent_count = cmd_to_send_set_count;
            try {
                origCmdToSendModule.writeCmd(tmpString);
            } catch (Exception e) {
                cmdToSendString = tmpString;
                cmdToSendModule = origCmdToSendModule;
                cmd_to_send_sent_count = orig_cmd_to_send_sent_count;
                delayed_job_to_do = true;
                PrintException(e);
                return;
            }
            if (origCmdToSendModule.commands_sent == orig_cmds_sent) {
                cmdToSendString = tmpString;
                cmdToSendModule = origCmdToSendModule;
                cmd_to_send_sent_count = orig_cmd_to_send_sent_count;
                delayed_job_to_do = true;
            }
        }
    }

    private String LoadCurrentCommand_Background_Do() {
        try {
            SendPreviouslySetCmdToSend();
            String cmdDataString = curModule.updateCmdData();
            return (cmdDataString);
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }
    private Long last_bad_cmd_idLong = null;

    private void LoadCurrentCommand_Background_Done(String cmdDataString) {
        try {
            update_current_status_next = true;
            if (null != cmdDataString
                    || (last_cmd_id_long < 0 && curModule.cmdData != null)) {
                if (null == cmdDataString) {
                    cmdDataString = curModule.cmdData;
                }
                String sData2 = replaceDoubleCommas(cmdDataString);
                StringTokenizer stData = new StringTokenizer(sData2, ",");
                String idString = stData.nextToken().trim();
                Long idLong = Long.valueOf(idString);
                boolean new_type = false;
                if (last_cmd_id_long < 1
                        || idLong.longValue() != last_cmd_id_long) {
                    new_type = true;
                }
                if (new_type) {
                    if (null == ModuleInfo.m_cmd_structInfoHashTable) {
                        ModuleInfo.m_cmd_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
                    }
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_cmd_structInfoHashTable.get(idLong);
                    if (typeInfo == null) {
                        if (!idLong.equals(last_bad_cmd_idLong)) {
                            last_bad_cmd_idLong = idLong;
                            diagapplet.utils.DiagError.println("LoadCurrentCommand: Can't get type info for type " + idLong + " curModule=" + this.curModule.Name);
                        }
                        return;
                    }
                    Hashtable ht = ModuleInfo.m_cmd_structInfoHashTable;
                    if (typeInfo.conflicts) {
                        if (null != curModule
                                && null != curModule.get_conflict_m_structInfoHashTable()
                                && null != curModule.get_conflict_m_structInfoHashTable().get(idLong)) {
                            typeInfo = (StructureTypeInfo) curModule.get_conflict_m_structInfoHashTable().get(idLong);
                            ht = curModule.get_conflict_m_structInfoHashTable();
                        }
                    }
                    if (null != typeInfo) {
                        watchJPanelDetailsCmd.setNmlMessageDictionary(curModule.get_cmd_msg_dict());
                        watchJPanelDetailsCmd.set_hashtable_by_id(ModuleInfo.m_cmd_structInfoHashTable);
                        watchJPanelDetailsCmd.SetTypeInfo(typeInfo, ModuleInfo.m_structInfoByNameHashTable);
                    }
                    jTextFieldCurrentCommandMsgType.setText(typeInfo.getName());
                    last_cmd_id_long = idLong.longValue();
                    curModule.cmd_msg_type = last_cmd_id_long;
                    jButtonPlotCmdVar.setText("Plot this cmd variable");
                    jButtonPlotCmdVar.setEnabled(false);
                }
                stData.nextToken(); // throw away size
                if (System.currentTimeMillis() - last_cmds_count_update_time > 500) {
                    jLabelCmdsRecvd.setText(Integer.toString(curModule.m_cmd_read_Connection.get_last_id_read()));
                    last_cmds_count_update_time = System.currentTimeMillis();
                }
                watchJPanelDetailsCmd.SetDataInfo(stData);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void LoadCurrentCommand() {
        try {
            if (!this.backgroundQueueEmpty()) {
                return;
            }
            if (null == curModule) {
                return;
            }
            SwingWorker backgroundSwingWorkerToRun =
                    new SwingWorker<String, Void>() {

                        @Override
                        public String doInBackground() {
                            return LoadCurrentCommand_Background_Do();
                        }

                        @Override
                        protected void done() {
                            try {
                                //Thread.dumpStack();
                                LoadCurrentCommand_Background_Done(get());
                            } catch (Exception ex) {
                                PrintException(ex);
                            }
                            NextBackgroundSwingWorker();
                        }
                    };
            ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void UpdateDetailsDisplay() {
        if (this.jCheckBoxDetailsModuleConnected.isSelected()) {
            if (update_current_status_next) {
                LoadCurrentStatus();
            } else {
                LoadCurrentCommand();
            }
        }
    }

    private String LoadAuxView_Background_Do() {
        try {
            if (aux_to_send_set_count != aux_to_send_sent_count
                    && auxToSendString != null) {
                String tmpString = auxToSendString;
                auxToSendString = null;
                aux_to_send_sent_count = aux_to_send_set_count;
                curAuxBuffer.writeDataString(tmpString);
            }
            String auxDataString = curAuxBuffer.getMessageData(curAuxBuffer.new_data_count);
            return (auxDataString);
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }
    private long last_bad_type_time = 0;
    private boolean handling_missing_type = false;

    private void LoadAuxView_Background_Done(String auxDataString) {
        try {
            if (handling_missing_type) {
                return;
            }
            if (null != auxDataString || (last_aux_id_long < 0 && curAuxBuffer.message_data != null)) {
                if (null == auxDataString) {
                    auxDataString = curAuxBuffer.message_data;
                }
                String sData2 = replaceDoubleCommas(auxDataString);
                StringTokenizer stData = new StringTokenizer(sData2, ",");
                String idString = stData.nextToken().trim();
                Long idLong = Long.valueOf(idString);
                boolean new_type = false;
                if (last_aux_id_long < 1
                        || idLong.longValue() != last_aux_id_long) {
                    new_type = true;
                }
                if (new_type) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoHashTable.get(idLong);
                    if(null == typeInfo) {
                        if(!curAuxBuffer.bufferline_headers_checked) {
                            String hdrs[] = curAuxBuffer.getBufferLineHeaders();
                            if(hdrs != null) {
                                for(String hdr : hdrs) {
                                    curModule.LoadAuxTypeFile(hdr);
                                }
                            }
                            UpdateAuxInfo();
                            curAuxBuffer.bufferline_headers_checked = true;
                            typeInfo = (StructureTypeInfo)
                                    ModuleInfo.m_structInfoHashTable.get(idLong);
                        }
                    }
                    if (null == typeInfo) {
                        final String s =
                                "LoadAuxView: Can't get type info for type " + idLong + " curAuxBuffer=" + this.curAuxBuffer.Name;
                        diagapplet.utils.DiagError.println(s);
                        if (System.currentTimeMillis() - last_bad_type_time > 5000) {
                            handling_missing_type = true;
                            final JFrame jf = this;
                            java.awt.EventQueue.invokeLater(new Runnable() {

                                @Override
                                public void run() {
                                    last_bad_type_time = System.currentTimeMillis();
                                    JOptionPane.showMessageDialog(jf, s + "\n Please select header file with type definition.\n");
                                    last_bad_type_time = System.currentTimeMillis();
                                    BrowseOpenInputHeaders();
                                    handling_missing_type = false;
                                    last_bad_type_time = System.currentTimeMillis();
                                }
                            });
                        }
                        last_bad_type_time = System.currentTimeMillis();
                        return;
                    }
                    Hashtable ht = ModuleInfo.m_structInfoHashTable;
                    if (typeInfo.conflicts) {
                        if (null != curModule
                                && null != curModule.get_conflict_m_structInfoHashTable()
                                && null != curModule.get_conflict_m_structInfoHashTable().get(idLong)) {
                            typeInfo = (StructureTypeInfo) curAuxBuffer.mi.get_conflict_m_structInfoHashTable().get(idLong);
                        }
                    }
                    if (null != typeInfo) {
                        watchJPanelAuxView.setNmlMessageDictionary(curAuxBuffer.read_msg_dict);
                        watchJPanelAuxView.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
                        watchJPanelAuxView.SetTypeInfo(typeInfo,
                                ModuleInfo.m_structInfoByNameHashTable);
                    }
                    jLabelAuxType.setText(typeInfo.getName());
                    last_aux_id_long = idLong.longValue();
                    jButtonPlotAuxVar.setText("Plot this status variable");
                    jButtonPlotAuxVar.setEnabled(false);
                }
                stData.nextToken(); // throw away size
                if (System.currentTimeMillis() - last_aux_count_update_time > 500 && null != curAuxBuffer.read_nml) {
                    jLabelAuxRecvd.setText(Integer.toString(curAuxBuffer.read_nml.get_last_id_read()));
                    last_aux_count_update_time = System.currentTimeMillis();
                }
                watchJPanelAuxView.SetDataInfo(stData);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void LoadAuxView() {
        try {
            if (!this.jCheckBoxAuxChannelConnected.isSelected()) {
                return;
            }
            if (!this.backgroundQueueEmpty()) {
                return;
            }
            if (null == curAuxBuffer || curAuxBuffer.read_nml == null) {
                return;
            }
            SwingWorker backgroundSwingWorkerToRun =
                    new SwingWorker<String, Void>() {

                        @Override
                        public String doInBackground() {
                            return LoadAuxView_Background_Do();
                        }

                        @Override
                        protected void done() {
                            try {
                                LoadAuxView_Background_Done(get());
                            } catch (Exception ex) {
                                PrintException(ex);
                            }
                            try {
                                NextBackgroundSwingWorker();
                            } catch (Exception ex2) {
                                PrintException(ex2);
                            }
                        }
                    };
            ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void CheckVarForPlotting(long _msg_type, BufferInfo bi, ModuleInfo mi, String s, int _var_number, boolean is_cmd, boolean is_aux) {
        for (PlotTracker plot_tracker : plot_tracker_hashtable.values()) {
            if (plot_tracker.using_nml_single_var_log) {
                continue;
            }
            if (plot_tracker.is_aux_channel != is_aux) {
                continue;
            }
            if (plot_tracker.is_cmd_value != is_cmd) {
                continue;
            }
            if (plot_tracker.msg_type != _msg_type) {
                continue;
            }
            if (is_aux) {
                if (!plot_tracker.auxBufferInfo.equals(bi)) {
                    continue;
                }
            } else if (!plot_tracker.module.equals(mi)) {
                continue;
            }
            if (plot_tracker.ndla_length_var_num == _var_number && plot_tracker.ndla) {
                plot_tracker.cur_ndla_len = Integer.valueOf(s).intValue();
                plot_tracker.plot_data.set_size(plot_tracker.cur_ndla_len);
                plot_tracker.cur_ndla_len_var_found = true;
                plot_tracker.updated = true;
            }
            int a_index = _var_number - plot_tracker.min_var_num;
            if (plot_tracker.skip_size > 1) {
                if ((a_index % plot_tracker.skip_size) != 0) {
                    continue;
                }
                a_index /= plot_tracker.skip_size;
            }
            if (a_index < 0 || a_index > plot_tracker.array_size) {
                continue;
            }
            if (plot_tracker.ndla && a_index >= plot_tracker.cur_ndla_len) {
                continue;
            }
            if (_var_number >= plot_tracker.min_var_num
                    && _var_number <= plot_tracker.max_var_num) {
                double value = Double.valueOf(s).doubleValue();
                plotter_NB_UI1.AddPointToArrayPlot(plot_tracker.plot_data, a_index, value);
                new_point_count++;
                total_point_count++;
            }
        }
    }

    private void UpdatePlots() {
        new_point_count = 0;
        if (plotter_NB_UI1.get_paused()) {
            return;
        }
        if (null != plot_tracker_hashtable) {
            for (PlotTracker plot_tracker : plot_tracker_hashtable.values()) {
                plot_tracker.updated = false;
                plot_tracker.cur_ndla_len_var_found = false;
                if (null != plot_tracker.auxBufferInfo) {
                    plot_tracker.auxBufferInfo.has_been_plotted = false;
                }
            }
            for (PlotTracker plot_tracker : plot_tracker_hashtable.values()) {
                if (Thread.interrupted()) {
                    return;
                }
                if (plot_tracker.updated) {
                    continue;
                }
                if (jTabbedPane1.getSelectedIndex() != 4
                        && plot_tracker.array_type) {
                    continue;
                }
                if (plot_tracker.using_nml_single_var_log) {
                    if (plot_tracker.nml_single_var_log_number < 0) {
                        try {
                            if (System.currentTimeMillis() - plot_tracker.last_reconnect_time_millis > 2000) {
                                plot_tracker.last_reconnect_time_millis = System.currentTimeMillis();
                                plot_tracker.nml_for_get_single_var_log.connectNoThrow();
                                if (plot_tracker.nml_for_get_single_var_log.is_connected()) {
                                    plot_tracker.nml_single_var_log_number =
                                            plot_tracker.nml_for_get_single_var_log.setupSingleVarLog(plot_tracker.vname, 500, 0.01, (int) plot_tracker.msg_type);
                                }
                                if (plot_tracker.nml_single_var_log_number < 0) {
                                    plot_tracker.nml_for_get_single_var_log.disconnect();
                                }
                                plot_tracker.last_reconnect_time_millis = System.currentTimeMillis();
                            }
                        } catch (Exception e) {
                        }
                        continue;
                    }
                    NMLSingleVarLog svl = plot_tracker.nml_for_get_single_var_log.getSingleVarLog(plot_tracker.nml_single_var_log_number);
                    if (null != svl && null != svl.items_list) {
                        for (int jj = 0; jj < svl.items_list.length && jj < svl.last_items_sent_size; jj++) {
                            if (!first_nml_single_var_log_timestamp_set) {
                                first_nml_single_var_log_timestamp = svl.items_list[jj].timestamp;
                                first_nml_single_var_log_timestamp_set = true;
                            }
                            double time_for_plot = svl.items_list[jj].timestamp - first_nml_single_var_log_timestamp;
                            this.plotter_NB_UI1.AddPointToPlot(plot_tracker.plot_data, time_for_plot, svl.items_list[jj].value, true);
                            new_point_count++;
                            total_point_count++;
                            //plotter_NB_UI1.AddPointToPlot(plot_tracker.plot_data,time_for_plot,svl.items_list[jj].value,true);
                        }
                        //plotter_NB_UI1.SetUpdateScaleForAllDplotters();
                    }
                    plot_tracker.updated = true;
                    continue;
                } else if (plot_tracker.is_aux_channel) {
                    if (this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                            && plot_tracker.auxBufferInfo != null
                            && plot_tracker.auxBufferInfo.read_nml != null
                            && plot_tracker.auxBufferInfo.read_nml.is_connected()) {
                        plot_tracker.auxBufferInfo.read_nml.connectNoThrow();
                    }
                    String msgString = null;
                    if (!plot_tracker.auxBufferInfo.has_been_plotted) {
                        msgString = plot_tracker.auxBufferInfo.getMessageData(plot_tracker.auxBufferInfo.new_data_count);
                        plot_tracker.auxBufferInfo.has_been_plotted = true;
                    }
                    if (null == msgString && plot_tracker.last_new_data_count != plot_tracker.auxBufferInfo.new_data_count) {
                        msgString = plot_tracker.auxBufferInfo.message_data;
                    }
                    if (null != msgString) {
                        String msgStringCopy = new String(msgString.toCharArray());
                        StringTokenizer tokenizer = new StringTokenizer(msgStringCopy, ",");
                        String idString = tokenizer.nextToken();
                        long msg_type = Long.valueOf(idString).longValue();
                        tokenizer.nextToken(); // skip size;
                        int var_number = 2;
                        while (tokenizer.hasMoreTokens()) {
                            CheckVarForPlotting(msg_type, plot_tracker.auxBufferInfo, null, tokenizer.nextToken(), var_number, false, true);
                            var_number++;
                            if (var_number > plot_tracker.auxBufferInfo.max_plot_var_number) {
                                break;
                            }
                        }
                    }
                    plot_tracker.last_new_data_count = plot_tracker.auxBufferInfo.new_data_count;
                    continue;
                } else if (plot_tracker.is_cmd_value) {
                    if (this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                            && plot_tracker.module != null
                            && plot_tracker.module.is_connected) {
                        plot_tracker.module.connect();
                    }
                    String msgString = null;
                    if (!plot_tracker.module.cmd_has_been_plotted) {
                        msgString = plot_tracker.module.updateCmdData();
                        plot_tracker.module.cmd_has_been_plotted = true;
                    }
                    if (null == msgString && plot_tracker.last_new_data_count != plot_tracker.module.new_cmd_count) {
                        msgString = plot_tracker.module.cmdData;
                    }
                    if (null != msgString) {
                        StringTokenizer tokenizer = new StringTokenizer(msgString, ",");
                        String idString = tokenizer.nextToken();
                        long msg_type = Long.valueOf(idString).longValue();
                        tokenizer.nextToken(); // skip size;
                        int var_number = 2;
                        while (tokenizer.hasMoreTokens()) {
                            CheckVarForPlotting(msg_type, null, plot_tracker.module, tokenizer.nextToken(), var_number, true, false);
                            var_number++;
                            if (var_number > plot_tracker.module.max_cmd_var_number) {
                                break;
                            }
                        }
                    }
                    plot_tracker.last_new_data_count = plot_tracker.module.new_cmd_count;
                    continue;
                } else {
                    if (this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                            && plot_tracker.module != null
                            && plot_tracker.module.is_connected) {
                        plot_tracker.module.connect();
                    }
                    String msgString = null;
                    if (!plot_tracker.module.stat_has_been_plotted) {
                        msgString = plot_tracker.module.updateStatData();
                        plot_tracker.module.stat_has_been_plotted = true;
                    }
                    if (null == msgString && plot_tracker.last_new_data_count != plot_tracker.module.new_stat_count) {
                        msgString = plot_tracker.module.statData;
                    }
                    if (null != msgString) {
                        StringTokenizer tokenizer = new StringTokenizer(msgString, ",");
                        String idString = tokenizer.nextToken();
                        long msg_type = Long.valueOf(idString).longValue();
                        tokenizer.nextToken(); // skip size;
                        int var_number = 2;
                        while (tokenizer.hasMoreTokens()) {
                            CheckVarForPlotting(msg_type, null, plot_tracker.module, tokenizer.nextToken(), var_number, false, false);
                            var_number++;
                            if (var_number > plot_tracker.module.max_stat_var_number) {
                                break;
                            }
                        }
                    }
                    plot_tracker.last_new_data_count = plot_tracker.module.new_stat_count;
                    continue;
                }
            }
        }
        if (new_point_count > 0) {
            plotter_NB_UI1.refresh();
        }
    }

    private class NmlInfo {

        public String name;
        public NMLConnectionInterface nml;
        public ModuleInfo mi;
        public BufferInfo bi;
        public boolean is_aux;
        public boolean is_cmd;
        public Hashtable ht;
        public int row;
        public int msg_count;
        public int msg_type;
        public boolean connected;
    }
    private Hashtable<String, NmlInfo> nml_hashtable = null;
    private int last_nml_table_row_updated = 0;
    private int connected_nml_table_updates = 0;
    private int nonconnected_nml_table_updates = 0;

    private void UpdateNmlTableRow(DefaultTableModel table_model, NmlInfo ni) throws NMLException {
//        String bufName = ni.name;
        int row = ni.row;
        NMLConnectionInterface nml = ni.nml;
        table_model.setValueAt(Boolean.valueOf(nml.is_connected()), row, 0);
        if (nml.is_connected()) {
            String typeName = null;
            try {
                Hashtable ht = ni.ht;
                if (null != ht && ni.msg_type > 0) {
                    StructureTypeInfo sti = (StructureTypeInfo) ht.get(Long.valueOf(ni.msg_type));
                    if (null != sti) {
                        typeName = sti.getName();
                    }
                }
            } catch (Exception e) {
                PrintException(e);
            }
            if (null != typeName) {
                table_model.setValueAt(Integer.toString(ni.msg_type) + "<" + typeName + ">", row, 3);
            } else {
                table_model.setValueAt(Integer.toString(ni.msg_type), row, 3);
            }
            table_model.setValueAt(Integer.valueOf(ni.msg_count), row, 4);
            connected_nml_table_updates++;
        } else {
            nonconnected_nml_table_updates++;
        }
    }

    private abstract class NmlInfoPublisher {

        abstract public void publish_ni(NmlInfo ni);
    }

    private Void UpdateNmlTable_Background_Do(NmlInfoPublisher nip) {
        try {
            for (NmlInfo ni : nml_hashtable.values()) {
                if (ni_to_override_connected != null) {
                    NmlInfo temp_ni = ni_to_override_connected;
                    ni_to_override_connected = null;
                    if (ni_connect_override) {
                        if (!temp_ni.nml.is_connected()) {
                            int orig_allocation_size_max = temp_ni.nml.getAllocation_size_max();
                            try {
                                temp_ni.nml.setAllocation_size_max(256);
                                temp_ni.nml.connect();

                            } catch (Exception e) {
                                PrintException(e);
                            }
                            try {
                                temp_ni.nml.setAllocation_size_max(256);
                                temp_ni.mi.connect();
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            temp_ni.nml.setAllocation_size_max(orig_allocation_size_max);
                        }
                    } else {
                        temp_ni.nml.disconnect();
                    }
                    temp_ni.connected = temp_ni.nml.is_connected();
                    if (temp_ni.connected) {
                        temp_ni.msg_type = temp_ni.nml.get_msg_type();
                        temp_ni.msg_count = temp_ni.nml.get_msg_count();
//                        if(temp_ni.msg_count > 1 && temp_ni.msg_type < 1)
//                        {
////                            System.out.println("temp_ni = " + temp_ni);
////                            System.out.println("temp_ni.nml.get_buffer_name() = " + temp_ni.nml.get_buffer_name());
////                            System.out.println("temp_ni.nml.getBufferLine() = " + temp_ni.nml.getBufferLine());
//                            temp_ni.msg_type = temp_ni.nml.get_msg_type();
//                            temp_ni.msg_count = temp_ni.nml.get_msg_count();
//                        }
                    }
                    nip.publish_ni(temp_ni);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                    if (jTabbedPane1.getSelectedIndex() != 3) {
                        return null;
                    }
                }
                boolean orig_connected_state = ni.connected;
                int orig_msg_type = ni.msg_type;
                int orig_msg_count = ni.msg_count;
                ni.connected = ni.nml.is_connected();
                if (ni.connected) {
                    ni.msg_type = ni.nml.get_msg_type();
                    ni.msg_count = ni.nml.get_msg_count();
//                    if(ni.msg_count > 1 && ni.msg_type < 1)
//                        {
////                            System.out.println("ni = " + ni);
////                            System.out.println("ni.nml.get_buffer_name() = " + ni.nml.get_buffer_name());
////                            System.out.println("ni.nml.getBufferLine() = " + ni.nml.getBufferLine());
//                            ni.msg_type = ni.nml.get_msg_type();
//                            ni.msg_count = ni.nml.get_msg_count();
//                        }
                    if (!checked_sleep(10)) {
                        return null;
                    }
                    if (Thread.interrupted() || jTabbedPane1.getSelectedIndex() != 3) {
                        return null;
                    }

                }
                if (orig_connected_state != ni.connected
                        || orig_msg_type != ni.msg_type
                        || orig_msg_count != ni.msg_count) {
                    nip.publish_ni(ni);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }

    private void UpdateNmlTable() {
        try {
            if (!this.backgroundQueueEmpty()) {
                return;
            }
//            int starting_row = last_nml_table_row_updated;
            connected_nml_table_updates = 0;
            nonconnected_nml_table_updates = 0;
            final DefaultTableModel nml_table_model = (DefaultTableModel) jTableNML.getModel();
            if (nml_table_model.getRowCount() < 1) {
                return;
            }
            last_nml_table_row_updated++;
            SwingWorker backgroundSwingWorkerToRun = new SwingWorker<Void, NmlInfo>() {

                @Override
                public Void doInBackground() {
                    NmlInfoPublisher nip = new NmlInfoPublisher() {

                        @Override
                        public void publish_ni(NmlInfo ni) {
                            publish(ni);
                        }
                    };
                    return UpdateNmlTable_Background_Do(nip);
                }

                @Override
                public void process(List<NmlInfo> ni_list) {
                    if (Thread.interrupted() || jTabbedPane1.getSelectedIndex() != 3) {
                        return;
                    }
                    for (NmlInfo ni : ni_list) {
                        try {
                            UpdateNmlTableRow(nml_table_model, ni);
                        } catch (Exception e) {
                            PrintException(e);
                        }
                        if (Thread.interrupted() || jTabbedPane1.getSelectedIndex() != 3) {
                            return;
                        }
                    }
                }

                @Override
                protected void done() {
                    UpdateConnectedDisplays();
                    TableRowSorter<TableModel> sorter = (TableRowSorter<TableModel>) jTableNML.getRowSorter();
                    sorter.setSortsOnUpdates(false);
                    NextBackgroundSwingWorker();
                }
            };

            ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
        } catch (Exception ex) {
            PrintException(ex);
        }
    }

    private void AddNmlByNameToTable(DefaultTableModel table_model, String name) {
        Object obj[] = new Object[table_model.getColumnCount()];
        obj[0] = Boolean.valueOf(false);
        obj[1] = name;
        obj[2] = "";
        obj[3] = "";
        obj[4] = Integer.valueOf(-1);
        table_model.addRow(obj);
    }

    private void AddNmlToTable(DefaultTableModel table_model, NmlInfo ni) throws NMLException {
        NMLConnectionInterface nml = ni.nml;
        if (null == nml) {
            Thread.dumpStack();
            System.exit(1);
        }
        if (true) {
            Hashtable ht = ni.ht;
            Object obj[] = new Object[table_model.getColumnCount()];
            obj[0] = Boolean.valueOf(nml.is_connected());
            if (nml.is_connected()) {
                num_connected++;
            }
//            //Thread.dumpStack();
//            System.out.println("nml="+nml);
//            System.out.println("num_connected="+num_connected);
            obj[1] = nml.get_buffer_name();
            obj[2] = nml.get_host();
            String typeName = null;
            if (null != ht && ni.msg_type > 0) {
                StructureTypeInfo sti = (StructureTypeInfo) ht.get(Long.valueOf(ni.msg_type));
                if (null != sti) {
                    typeName = sti.getName();
                }
            }
            if (null != typeName) {
                obj[3] = Integer.toString(ni.msg_type) + "<" + typeName + ">";
            } else {
                obj[3] = Integer.toString(ni.msg_type);
            }
            obj[4] = Integer.valueOf(ni.msg_count);
            ni.row = table_model.getRowCount();
            nml_hashtable.put(nml.get_buffer_name(), ni);
            table_model.addRow(obj);
        } else {
            AddNmlByNameToTable(table_model, ni.name);
        }
    }

    private void LoadNmlTable() {
        DefaultTableModel nml_table_model = (DefaultTableModel) jTableNML.getModel();
        nml_table_model.setRowCount(0);
        nml_hashtable = null;
        num_connected = 0;
        Vector<NmlInfo> nmlInfoVector = new Vector<NmlInfo>();
        Hashtable ht = cgc.get_m_modulesHashTable();
        if (null != ht) {
            for (Object obj : ht.values()) {
                try {
                    ModuleInfo mi = (ModuleInfo) obj;
                    if (null == ModuleInfo.m_cmd_structInfoHashTable) {
                        ModuleInfo.m_cmd_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
                    }
                    if (null == ModuleInfo.m_stat_structInfoHashTable) {
                        ModuleInfo.m_stat_structInfoHashTable = ModuleInfo.m_structInfoHashTable;
                    }
                    if (!mi.no_cmd) {
                        NmlInfo cmd_ni = new NmlInfo();
                        cmd_ni.nml = mi.m_cmd_read_Connection;
                        cmd_ni.name = cmd_ni.nml.get_buffer_name();
                        if (cmd_ni.name == null) {
                            diagapplet.utils.DiagError.println("Module : " + mi.Name + " has null cmd buffer name.");
                            continue;
                        }
                        cmd_ni.mi = mi;
                        cmd_ni.is_aux = false;
                        cmd_ni.is_cmd = true;
                        cmd_ni.ht = ModuleInfo.m_cmd_structInfoHashTable;
                        if (null != cmd_ni.nml) {
                            nmlInfoVector.add(cmd_ni);
                        }
                    }

                    if (!mi.no_stat) {
                        NmlInfo stat_ni = new NmlInfo();
                        stat_ni.nml = mi.m_stat_read_Connection;
                        stat_ni.name = stat_ni.nml.get_buffer_name();
                        if (stat_ni.name == null) {
                            diagapplet.utils.DiagError.println("Module : " + mi.Name + " has null stat buffer name.");
                            continue;
                        }
                        stat_ni.mi = mi;
                        stat_ni.is_aux = false;
                        stat_ni.is_cmd = false;
                        stat_ni.ht = ModuleInfo.m_stat_structInfoHashTable;
                        if (null != stat_ni.nml) {
                            nmlInfoVector.add(stat_ni);
                        }
                    }
                } catch (Exception e) {
                    PrintException(e);
                }
            }
        }
        if (null != auxBuffersHashtable) {
            for (BufferInfo bi : auxBuffersHashtable.values()) {
                try {
                    if (null != bi.read_nml) {
                        NmlInfo aux_ni = new NmlInfo();
                        aux_ni.nml = bi.read_nml;
                        aux_ni.name = aux_ni.nml.get_buffer_name();
                        aux_ni.bi = bi;
                        aux_ni.mi = (ModuleInfo) bi.mi;
                        aux_ni.is_aux = true;
                        aux_ni.is_cmd = false;
                        aux_ni.ht = ModuleInfo.m_structInfoHashTable;
                        if (null != aux_ni.nml) {
                            nmlInfoVector.add(aux_ni);
                        }
                    } else {
                        NmlInfo aux_ni = new NmlInfo();
                        aux_ni.name = bi.Name;
                        aux_ni.bi = bi;
                        aux_ni.mi = (ModuleInfo) bi.mi;
                        aux_ni.is_aux = true;
                        aux_ni.is_cmd = false;
                        aux_ni.ht = ModuleInfo.m_structInfoHashTable;
                        if (null != aux_ni.nml) {
                            nmlInfoVector.add(aux_ni);
                        }
                    }
                } catch (Exception e) {
                    PrintException(e);
                }
            }
        }
        TableRowSorter<TableModel> sorter = (TableRowSorter<TableModel>) jTableNML.getRowSorter();
        sorter.setSortsOnUpdates(true);
        final int selected_c = jTableNML.getSelectedColumn();
        Comparator ct = null;
        try {
            if (selected_c >= 0 && selected_c < jTableNML.getColumnCount()) {
                ct = sorter.getComparator(selected_c);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        final Comparator c = ct;
        Comparator<NmlInfo> cn = new Comparator<NmlInfo>() {

            @Override
            public int compare(NmlInfo o1, NmlInfo o2) {
                if (null != c) {
                    switch (selected_c) {
                        case 0:
                            return c.compare(o1.connected, o2.connected);

                        case 1:
                            return c.compare(o1.name, o2.name);

                        case 2:
                            return c.compare(o1.nml.get_host(), o2.nml.get_host());

                        case 3:
                            return c.compare(o1.msg_type, o2.msg_type);

                        case 4:
                            return c.compare(o1.msg_count, o2.msg_count);
                    }
                } else {
                    switch (selected_c) {
                        case 0: {
                            int r = 0;
                            if (o1.connected) {
                                r++;
                            }
                            if (o2.connected) {
                                r--;
                            }
                            return r;
                        }


                        case 1:
                            return o1.name.compareTo(o2.name);

                        case 2:
                            return o1.nml.get_host().compareTo(o2.nml.get_host());

                        case 3:
                            return o1.msg_type - o2.msg_type;

                        case 4:
                            return o1.msg_count - o2.msg_count;
                    }
                }
                return o1.name.compareTo(o2.name);
            }
        };
        Collections.sort(nmlInfoVector, cn);
        nml_hashtable = new Hashtable<String, NmlInfo>();
        for (NmlInfo ni : nmlInfoVector) {
            try {
                AddNmlToTable(nml_table_model, ni);
            } catch (Exception e) {
                PrintException(e);
            }
        }
        UpdateConnectedDisplays();
        if (num_connected > 0 && !javax_swing_timer.isRunning() && this.getExtendedState() != JFrame.ICONIFIED) {
            javax_swing_timer.start();
        } else if (num_connected == 0 && null == extraTabVector && javax_swing_timer.isRunning()) {
            javax_swing_timer.stop();
        }
    }

    private void UpdateTitle() {
        String hier = "";
        String nml = "";

        if (null != last_hierarchy_loaded) {
            try {
                hier = new File(last_hierarchy_loaded).getName();
            } catch (Exception e) {
            }
        }
        if (null != ModuleInfo.DefaultNMLConfigurationFile) {
            try {
                nml = new File(ModuleInfo.DefaultNMLConfigurationFile).getName();
            } catch (Exception e) {
            }
        }
        setTitle(hier + " - " + nml + " - (" + num_connected + "/" + jTableNML.getRowCount() + ") : RCS Diagnostics");
    }
    private int last_num_connected_ucd = -1;
    private int last_table_row_count = -1;

    private void UpdateConnectedDisplays() {
        UpdateTitle();
        int tcr = jTableNML.getRowCount();
        if (num_connected != last_num_connected_ucd
                || last_table_row_count != tcr) {
            last_num_connected_ucd = num_connected;
            last_table_row_count = tcr;
            jLabelNMLNumConnectedOfTotal.setText("Connected " + num_connected + " out of " + tcr);
            if (num_connected > 0 && !this.jCheckBoxMenuItemConnected.isSelected()) {
                this.jCheckBoxMenuItemConnected.setSelected(true);
            } else if (num_connected < 1 && this.jCheckBoxMenuItemConnected.isSelected()) {
                this.jCheckBoxMenuItemConnected.setSelected(false);
            }
        }
    }

    private void PerformDelayedJobs() {
        SendPreviouslySetCmdToSend();
        delayed_job_to_do = false;
    }
    private static long last_errlog_check_time = System.currentTimeMillis();
    private static int errlog_count = 0;

    private void UpdateErrlog() {
        if (System.currentTimeMillis() - last_errlog_check_time < 500) {
            return;
        }
        String s = ModuleInfo.readErrlogData();
        if (null != s) {
            errlog_count++;
            s = "Message " + errlog_count + " from NML errlog channel :" + s;
            diagapplet.utils.DiagError.println(s);
            if (errlog_count < 20) {
                JOptionPane.showMessageDialog(this, s);
            }
        }
        last_errlog_check_time = System.currentTimeMillis();
    }

    private void run_cycle() {
        try {
            HideConnectingDialog();
            if (!plotter_NB_UI1.get_paused()) {
                UpdatePlots();
            }
            check_running_file();
            cycle_count++;
            int current_tab = this.jTabbedPane1.getSelectedIndex();
//	    System.out.println("current_tab="+current_tab+", cycle_count="+cycle_count);
            if (total_point_count > 20 && !fit_done && !plotter_NB_UI1.get_paused()) {
                this.plotter_NB_UI1.FitToGraph();
                fit_done = true;
            }
            if (null != module_cmd_popup && !module_cmd_popup.isVisible()) {
                module_cmd_popup = null;
            }
            switch (current_tab) {
                case 0:
                    if (null == module_cmd_popup) {
                        hierarchyDraw.UpdateDisplay(false);
                    }
                    break;

                case 1:
                    UpdateDetailsDisplay();
                    break;

                case 2:
                    LoadAuxView();
                    break;

                case 3:
                    UpdateNmlTable();
                    break;

                case 4:
                    this.plotter_NB_UI1.CheckRecalcPlots();
                    if (total_point_count > last_total_point_count && !plotter_NB_UI1.get_paused() && !plotter_NB_UI1.isLocked()) {
                        if (this.plotter_NB_UI1.get_array_mode()) {
                            this.plotter_NB_UI1.FitToGraph();
                        } else {
                            this.plotter_NB_UI1.ScrollRight();
                            this.plotter_NB_UI1.FitY();
                        }
                        last_total_point_count = total_point_count;
                    }
                    break;



                default:
                    break;
            }
            if (this.backgroundQueueEmpty()) {
                if (delayed_job_to_do) {
                    SwingWorker backgroundSwingWorkerToRun = new SwingWorker<Void, Void>() {

                        @Override
                        protected Void doInBackground() throws Exception {
                            PerformDelayedJobs();
                            return null;
                        }

                        @Override
                        protected void done() {
                            NextBackgroundSwingWorker();
                        }
                    };
                    ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
                } else {
                    if (null != extraTabVector) {
                        for (final ExtraTabInfo eti : extraTabVector) {
                            if (eti.tab_number == current_tab) {
                                if (eti.update_method != null) {
                                    SwingWorker backgroundSwingWorkerToRun = new SwingWorker<Void, Void>() {

                                        @Override
                                        protected Void doInBackground() throws Exception {
                                            eti.update_method.invoke(eti.jp);
                                            UpdateErrlog();
                                            return null;
                                        }

                                        @Override
                                        protected void done() {
                                            NextBackgroundSwingWorker();
                                        }
                                    };
                                    ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
                                }
                                break;
                            }
                        }
                    } else if (ModuleInfo.is_check_errlog_flag_set()
                            && System.currentTimeMillis() - last_errlog_check_time > 500
                            && (this.jCheckBoxMenuItemConnected.isSelected() || this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected())) {
                        SwingWorker backgroundSwingWorkerToRun = new SwingWorker<Void, Void>() {

                            @Override
                            protected Void doInBackground() throws Exception {
                                UpdateErrlog();
                                return null;
                            }

                            @Override
                            protected void done() {
                                NextBackgroundSwingWorker();
                            }
                        };
                        ExecuteBackgroundSwingWorkerIfIdle(backgroundSwingWorkerToRun);
                    }
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jDialogConnecting = new javax.swing.JDialog();
        jPanel2 = new javax.swing.JPanel();
        jLabel17 = new javax.swing.JLabel();
        jLabel18 = new javax.swing.JLabel();
        jLabel19 = new javax.swing.JLabel();
        jLabel20 = new javax.swing.JLabel();
        jProgressBarConnectToAll = new javax.swing.JProgressBar();
        jLabelNumBuffers = new javax.swing.JLabel();
        jLabelNumTried = new javax.swing.JLabel();
        jLabelNumFailed = new javax.swing.JLabel();
        jLabel24 = new javax.swing.JLabel();
        jLabelCurBuffer = new javax.swing.JLabel();
        jLabelNumConnected = new javax.swing.JLabel();
        jButtonCancel = new javax.swing.JButton();
        jProgressBarConnectToAllConnected = new javax.swing.JProgressBar();
        jProgressBarConnectToAllFailed = new javax.swing.JProgressBar();
        jButtonSkipOne = new javax.swing.JButton();
        jFrameAdjustVar = new javax.swing.JFrame();
        jLabel10 = new javax.swing.JLabel();
        jLabel26 = new javax.swing.JLabel();
        jLabel27 = new javax.swing.JLabel();
        jTextFieldAdjustVarIncrement = new javax.swing.JTextField();
        jTextFieldAdjustVarMin = new javax.swing.JTextField();
        jTextFieldAdjustVarMax = new javax.swing.JTextField();
        jComboBoxSelectAdjustVar = new javax.swing.JComboBox();
        jScrollBarAdjustVar = new javax.swing.JScrollBar();
        jCheckBoxSendEachAdjustmentChange = new javax.swing.JCheckBox();
        jButtonAdjusterClose = new javax.swing.JButton();
        jLabel13 = new javax.swing.JLabel();
        jFormattedTextFieldAdjusterValue = new javax.swing.JFormattedTextField();
        jTextField3 = new javax.swing.JTextField();
        buttonGroupAuxCmdStatNormal = new javax.swing.ButtonGroup();
        jTabbedPane1 = new javax.swing.JTabbedPane();
        jPanelHierarchyOuter = new javax.swing.JPanel();
        jScrollPaneHierarchy = new javax.swing.JScrollPane();
        jPaintablePanelHierarchyInner = new diagapplet.JPaintablePanel();
        jPanelDetails = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        watchJPanelCmdToSend = new diagapplet.utils.WatchJPanel();
        jLabel4 = new javax.swing.JLabel();
        watchJPanelDetailsCmd = new diagapplet.utils.WatchJPanel();
        jLabel6 = new javax.swing.JLabel();
        watchJPanelDetailsStatus = new diagapplet.utils.WatchJPanel();
        jButtonSendDetailsCommand = new javax.swing.JButton();
        jLabel9 = new javax.swing.JLabel();
        jLabelCmdsRecvd = new javax.swing.JLabel();
        jLabel11 = new javax.swing.JLabel();
        jLabelStatusRecvd = new javax.swing.JLabel();
        jButtonPlotStatusVar = new javax.swing.JButton();
        jButtonPlotStatusArray = new javax.swing.JButton();
        jButtonPlotCmdArray = new javax.swing.JButton();
        jButtonPlotCmdVar = new javax.swing.JButton();
        jButtonAdjustCmdToSend = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jScrollPaneModulesList = new javax.swing.JScrollPane();
        jListModules = new javax.swing.JList();
        jLabel2 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jListCmdsAvailable = new javax.swing.JList();
        jLabel1 = new javax.swing.JLabel();
        jCheckBoxModifyStatus = new javax.swing.JCheckBox();
        jCheckBoxDetailsModuleConnected = new javax.swing.JCheckBox();
        jScrollPaneDetailsText = new javax.swing.JScrollPane();
        jTextPaneDetailsBottom = new javax.swing.JTextPane();
        jTextFieldCurrentCommandMsgType = new javax.swing.JTextField();
        jTextFieldCurrentStatusType = new javax.swing.JTextField();
        jTextFieldCmdToSendMsgType = new javax.swing.JTextField();
        jPanelAuxChannels = new javax.swing.JPanel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jListAuxChannels = new javax.swing.JList();
        jLabel5 = new javax.swing.JLabel();
        jLabel7 = new javax.swing.JLabel();
        jScrollPane3 = new javax.swing.JScrollPane();
        jListAuxMessagesAvailable = new javax.swing.JList();
        jLabel8 = new javax.swing.JLabel();
        jLabelAuxMsgToSendType = new javax.swing.JLabel();
        jButtonSendAuxMsg = new javax.swing.JButton();
        watchJPanelAuxMsgToSend = new diagapplet.utils.WatchJPanel();
        jLabel12 = new javax.swing.JLabel();
        jLabelAuxRecvd = new javax.swing.JLabel();
        jLabel16 = new javax.swing.JLabel();
        jLabelAuxType = new javax.swing.JLabel();
        jButtonPlotAuxArray = new javax.swing.JButton();
        jButtonPlotAuxVar = new javax.swing.JButton();
        watchJPanelAuxView = new diagapplet.utils.WatchJPanel();
        jScrollPane6 = new javax.swing.JScrollPane();
        jTextPaneAuxBottom = new javax.swing.JTextPane();
        jCheckBoxAuxChannelConnected = new javax.swing.JCheckBox();
        jPanel3 = new javax.swing.JPanel();
        jRadioButtonAuxCmd = new javax.swing.JRadioButton();
        jRadioButtonAuxStat = new javax.swing.JRadioButton();
        jRadioButtonAuxNormal = new javax.swing.JRadioButton();
        jPanelNML = new javax.swing.JPanel();
        jLabelNMLBufferLine = new javax.swing.JLabel();
        jScrollPane4 = new javax.swing.JScrollPane();
        jTextPaneNmlError = new javax.swing.JTextPane();
        jLabel15 = new javax.swing.JLabel();
        jLabel21 = new javax.swing.JLabel();
        jScrollPane5 = new javax.swing.JScrollPane();
        jTableNML = new javax.swing.JTable();
        jLabel25 = new javax.swing.JLabel();
        jLabelNMLConfigFile = new javax.swing.JLabel();
        jLabelNMLNumConnectedOfTotal = new javax.swing.JLabel();
        plotter_NB_UI1 = new diagapplet.plotter.plotter_NB_UI();
        jPanelErrorPanel = new javax.swing.JPanel();
        jScrollPane7 = new javax.swing.JScrollPane();
        jTextAreaErrs = new javax.swing.JTextArea();
        jMenuBarMain = new javax.swing.JMenuBar();
        jMenuFile = new javax.swing.JMenu();
        jMenuItemFileOpen = new javax.swing.JMenuItem();
        jMenuItemFileExit = new javax.swing.JMenuItem();
        jMenuFileRecent = new javax.swing.JMenu();
        jMenuItemFileDebugDump = new javax.swing.JMenuItem();
        jMenuItemDumpStruct = new javax.swing.JMenuItem();
        jMenuItemOpenMsgFile = new javax.swing.JMenuItem();
        jMenuItemFileEditDiagCfg = new javax.swing.JMenuItem();
        jMenuItemFileEditNml = new javax.swing.JMenuItem();
        jMenuItemPrintHierarchy = new javax.swing.JMenuItem();
        jMenuView = new javax.swing.JMenu();
        jCheckBoxMenuItemFullscreen = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemShowAdjusterWindow = new javax.swing.JCheckBoxMenuItem();
        jMenuLookAndFeel = new javax.swing.JMenu();
        jMenuConnections = new javax.swing.JMenu();
        jCheckBoxMenuItemConnected = new javax.swing.JCheckBoxMenuItem();
        jMenuItemOpenNmlConfigFile = new javax.swing.JMenuItem();
        jMenuConnectionsRecentNmlFiles = new javax.swing.JMenu();
        jCheckBoxMenuItemAutoConnectDisconnectAsNeeded = new javax.swing.JCheckBoxMenuItem();
        jMenuItemOpenNmlCfgsvrConnection = new javax.swing.JMenuItem();
        jMenuItemOpenInputHeader = new javax.swing.JMenuItem();
        jMenuPlotting = new javax.swing.JMenu();
        jMenuItemPlottingSavePlotSet = new javax.swing.JMenuItem();
        jMenuItemPlottingOpenPlotSet = new javax.swing.JMenuItem();
        jMenuPlottingRecentPlotSets = new javax.swing.JMenu();
        jCheckBoxMenuItemAutoSavePlotSets = new javax.swing.JCheckBoxMenuItem();
        jMenuExtras = new javax.swing.JMenu();
        jMenuHelp = new javax.swing.JMenu();
        jMenuItemHelpRcsLibrary = new javax.swing.JMenuItem();

        jDialogConnecting.setTitle("Connecting To All Channels/Modules . . .");
        jDialogConnecting.setResizable(false);
        jDialogConnecting.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosed(java.awt.event.WindowEvent evt) {
                jDialogConnectingWindowClosed(evt);
            }
            public void windowClosing(java.awt.event.WindowEvent evt) {
                jDialogConnectingWindowClosing(evt);
            }
        });

        jPanel2.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

        jLabel17.setText("Connected:");

        jLabel18.setText("Tried:");

        jLabel19.setText("Total:");

        jLabel20.setText("Currently Trying . . . :");

        jLabelNumBuffers.setText("<<Number of Buffers >>");

        jLabelNumTried.setText("<< Number tried >>");

        jLabelNumFailed.setText("<<Number Failed >>");

        jLabel24.setText("Failed:");

        jLabelCurBuffer.setText("<<Current Buffer>>");

        jLabelNumConnected.setText("<<Number Connected >>");

        jButtonCancel.setText("Cancel");
        jButtonCancel.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCancelActionPerformed(evt);
            }
        });

        jButtonSkipOne.setText("Skip One");
        jButtonSkipOne.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSkipOneActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel19)
                            .addComponent(jLabel18)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabel24))
                            .addComponent(jLabel20)
                            .addComponent(jLabel17))
                        .addGap(34, 34, 34)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelCurBuffer)
                            .addComponent(jLabelNumBuffers)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabelNumConnected)
                                    .addComponent(jLabelNumTried)
                                    .addComponent(jLabelNumFailed))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jProgressBarConnectToAll, javax.swing.GroupLayout.DEFAULT_SIZE, 162, Short.MAX_VALUE)
                                    .addComponent(jProgressBarConnectToAllConnected, javax.swing.GroupLayout.DEFAULT_SIZE, 162, Short.MAX_VALUE)
                                    .addComponent(jProgressBarConnectToAllFailed, javax.swing.GroupLayout.DEFAULT_SIZE, 162, Short.MAX_VALUE)))))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addComponent(jButtonSkipOne)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonCancel)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel17)
                        .addComponent(jLabelNumConnected))
                    .addComponent(jProgressBarConnectToAllConnected, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabelNumFailed)
                            .addComponent(jLabel24))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabelNumTried)
                            .addComponent(jLabel18))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabelNumBuffers)
                            .addComponent(jLabel19))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel20)
                            .addComponent(jLabelCurBuffer)))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGap(8, 8, 8)
                        .addComponent(jProgressBarConnectToAllFailed, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jProgressBarConnectToAll, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 27, Short.MAX_VALUE)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonCancel)
                    .addComponent(jButtonSkipOne))
                .addContainerGap())
        );

        javax.swing.GroupLayout jDialogConnectingLayout = new javax.swing.GroupLayout(jDialogConnecting.getContentPane());
        jDialogConnecting.getContentPane().setLayout(jDialogConnectingLayout);
        jDialogConnectingLayout.setHorizontalGroup(
            jDialogConnectingLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jDialogConnectingLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jDialogConnectingLayout.setVerticalGroup(
            jDialogConnectingLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jDialogConnectingLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jFrameAdjustVar.setTitle("Adjust : ");
        jFrameAdjustVar.setMinimumSize(new java.awt.Dimension(400, 100));
        jFrameAdjustVar.setResizable(false);

        jLabel10.setText("Max:");

        jLabel26.setText("Min:");

        jLabel27.setText("Increment:");

        jTextFieldAdjustVarIncrement.setText("1");
        jTextFieldAdjustVarIncrement.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldAdjustVarIncrementActionPerformed(evt);
            }
        });

        jTextFieldAdjustVarMin.setText("0");
        jTextFieldAdjustVarMin.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldAdjustVarMinActionPerformed(evt);
            }
        });

        jTextFieldAdjustVarMax.setText("100");
        jTextFieldAdjustVarMax.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jTextFieldAdjustVarMaxActionPerformed(evt);
            }
        });

        jComboBoxSelectAdjustVar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jComboBoxSelectAdjustVarActionPerformed(evt);
            }
        });

        jScrollBarAdjustVar.setOrientation(javax.swing.JScrollBar.HORIZONTAL);
        jScrollBarAdjustVar.addAdjustmentListener(new java.awt.event.AdjustmentListener() {
            public void adjustmentValueChanged(java.awt.event.AdjustmentEvent evt) {
                jScrollBarAdjustVarAdjustmentValueChanged(evt);
            }
        });

        jCheckBoxSendEachAdjustmentChange.setText("Send Each Change");
        jCheckBoxSendEachAdjustmentChange.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));

        jButtonAdjusterClose.setText("Close");
        jButtonAdjusterClose.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAdjusterCloseActionPerformed(evt);
            }
        });

        jLabel13.setText("Value:");

        jFormattedTextFieldAdjusterValue.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jFormattedTextFieldAdjusterValueActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jFrameAdjustVarLayout = new javax.swing.GroupLayout(jFrameAdjustVar.getContentPane());
        jFrameAdjustVar.getContentPane().setLayout(jFrameAdjustVarLayout);
        jFrameAdjustVarLayout.setHorizontalGroup(
            jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jFrameAdjustVarLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jComboBoxSelectAdjustVar, 0, 548, Short.MAX_VALUE)
                    .addGroup(jFrameAdjustVarLayout.createSequentialGroup()
                        .addComponent(jLabel26)
                        .addGap(14, 14, 14)
                        .addComponent(jTextFieldAdjustVarMin, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel27)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldAdjustVarIncrement, javax.swing.GroupLayout.PREFERRED_SIZE, 68, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 133, Short.MAX_VALUE)
                        .addComponent(jLabel10)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldAdjustVarMax, javax.swing.GroupLayout.PREFERRED_SIZE, 67, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addComponent(jScrollBarAdjustVar, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 548, Short.MAX_VALUE)
                    .addGroup(jFrameAdjustVarLayout.createSequentialGroup()
                        .addGroup(jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                            .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jFrameAdjustVarLayout.createSequentialGroup()
                                .addComponent(jLabel13)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jFormattedTextFieldAdjusterValue))
                            .addComponent(jCheckBoxSendEachAdjustmentChange, javax.swing.GroupLayout.Alignment.LEADING))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 314, Short.MAX_VALUE)
                        .addComponent(jButtonAdjusterClose)))
                .addContainerGap())
        );
        jFrameAdjustVarLayout.setVerticalGroup(
            jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jFrameAdjustVarLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxSelectAdjustVar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel26)
                    .addComponent(jTextFieldAdjustVarMin, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel27)
                    .addComponent(jTextFieldAdjustVarIncrement, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jTextFieldAdjustVarMax, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel10))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollBarAdjustVar, javax.swing.GroupLayout.PREFERRED_SIZE, 33, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jCheckBoxSendEachAdjustmentChange)
                    .addComponent(jButtonAdjusterClose))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jFrameAdjustVarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel13)
                    .addComponent(jFormattedTextFieldAdjusterValue, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTextField3.setText("jTextField3");

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
        setTitle("RCS/NML Diagnostics");
        addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentMoved(java.awt.event.ComponentEvent evt) {
                formComponentMoved(evt);
            }
            public void componentResized(java.awt.event.ComponentEvent evt) {
                formComponentResized(evt);
            }
        });
        addWindowStateListener(new java.awt.event.WindowStateListener() {
            public void windowStateChanged(java.awt.event.WindowEvent evt) {
                formWindowStateChanged(evt);
            }
        });

        jTabbedPane1.setMinimumSize(new java.awt.Dimension(0, 0));
        jTabbedPane1.addChangeListener(new javax.swing.event.ChangeListener() {
            public void stateChanged(javax.swing.event.ChangeEvent evt) {
                jTabbedPane1StateChanged(evt);
            }
        });

        jScrollPaneHierarchy.setBackground(new java.awt.Color(51, 153, 255));
        jScrollPaneHierarchy.setOpaque(false);

        jPaintablePanelHierarchyInner.setBackground(new java.awt.Color(102, 153, 255));
        jPaintablePanelHierarchyInner.setDoubleBuffered(false);
        jPaintablePanelHierarchyInner.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mousePressed(java.awt.event.MouseEvent evt) {
                jPaintablePanelHierarchyInnerMousePressed(evt);
            }
            public void mouseReleased(java.awt.event.MouseEvent evt) {
                jPaintablePanelHierarchyInnerMouseReleased(evt);
            }
        });
        jPaintablePanelHierarchyInner.addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentMoved(java.awt.event.ComponentEvent evt) {
                jPaintablePanelHierarchyInnerComponentMoved(evt);
            }
        });

        javax.swing.GroupLayout jPaintablePanelHierarchyInnerLayout = new javax.swing.GroupLayout(jPaintablePanelHierarchyInner);
        jPaintablePanelHierarchyInner.setLayout(jPaintablePanelHierarchyInnerLayout);
        jPaintablePanelHierarchyInnerLayout.setHorizontalGroup(
            jPaintablePanelHierarchyInnerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 1410, Short.MAX_VALUE)
        );
        jPaintablePanelHierarchyInnerLayout.setVerticalGroup(
            jPaintablePanelHierarchyInnerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 614, Short.MAX_VALUE)
        );

        jScrollPaneHierarchy.setViewportView(jPaintablePanelHierarchyInner);

        javax.swing.GroupLayout jPanelHierarchyOuterLayout = new javax.swing.GroupLayout(jPanelHierarchyOuter);
        jPanelHierarchyOuter.setLayout(jPanelHierarchyOuterLayout);
        jPanelHierarchyOuterLayout.setHorizontalGroup(
            jPanelHierarchyOuterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelHierarchyOuterLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPaneHierarchy, javax.swing.GroupLayout.DEFAULT_SIZE, 696, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelHierarchyOuterLayout.setVerticalGroup(
            jPanelHierarchyOuterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelHierarchyOuterLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPaneHierarchy, javax.swing.GroupLayout.DEFAULT_SIZE, 595, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("<F1> Hierarchy", jPanelHierarchyOuter);

        jPanelDetails.setToolTipText("Show all status variables for a single module/send commands to a single module.");
        jPanelDetails.setMinimumSize(new java.awt.Dimension(0, 0));

        jLabel3.setText("Msg To Send:");

        jLabel4.setText("Current Command:");
        jLabel4.setMinimumSize(new java.awt.Dimension(0, 15));

        jLabel6.setText("Current Status: ");
        jLabel6.setMinimumSize(new java.awt.Dimension(0, 15));

        jButtonSendDetailsCommand.setText("Send");
        jButtonSendDetailsCommand.setToolTipText("Send the command below.");
        jButtonSendDetailsCommand.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSendDetailsCommandActionPerformed(evt);
            }
        });

        jLabel9.setText("Command Count:");

        jLabelCmdsRecvd.setText("<<Cmds Recieved >>");

        jLabel11.setText("Status Count:");

        jLabelStatusRecvd.setText("<<Stats Received >>");

        jButtonPlotStatusVar.setText("Plot this status variable");
        jButtonPlotStatusVar.setToolTipText("Add a plot of the selected status variable versus time to the graph tab.");
        jButtonPlotStatusVar.setEnabled(false);
        jButtonPlotStatusVar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotStatusVarActionPerformed(evt);
            }
        });

        jButtonPlotStatusArray.setText("Plot this status array");
        jButtonPlotStatusArray.setToolTipText("Add a plot of the selected array versus the array index to the graph tab.");
        jButtonPlotStatusArray.setEnabled(false);
        jButtonPlotStatusArray.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotStatusArrayActionPerformed(evt);
            }
        });

        jButtonPlotCmdArray.setText("Plot this command array");
        jButtonPlotCmdArray.setToolTipText("Add a plot of the selected array versus the array index to the graph tab.");
        jButtonPlotCmdArray.setEnabled(false);
        jButtonPlotCmdArray.setMinimumSize(new java.awt.Dimension(0, 25));
        jButtonPlotCmdArray.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotCmdArrayActionPerformed(evt);
            }
        });

        jButtonPlotCmdVar.setText("Plot this command variable.");
        jButtonPlotCmdVar.setToolTipText("Add a plot of the selected variable versus time to the graph tab.");
        jButtonPlotCmdVar.setEnabled(false);
        jButtonPlotCmdVar.setMinimumSize(new java.awt.Dimension(0, 25));
        jButtonPlotCmdVar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotCmdVarActionPerformed(evt);
            }
        });

        jButtonAdjustCmdToSend.setText("Adjust");
        jButtonAdjustCmdToSend.setToolTipText("Open Adjuster window with slider for modifying continuos variable values.");
        jButtonAdjustCmdToSend.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAdjustCmdToSendActionPerformed(evt);
            }
        });

        jListModules.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jListModules.setToolTipText("Select the module to view or send commands to.");
        jListModules.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListModulesValueChanged(evt);
            }
        });
        jScrollPaneModulesList.setViewportView(jListModules);

        jLabel2.setText("Msgs Available:");

        jListCmdsAvailable.setSelectionMode(javax.swing.ListSelectionModel.SINGLE_SELECTION);
        jListCmdsAvailable.setToolTipText("Select the command to send.");
        jListCmdsAvailable.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListCmdsAvailableValueChanged(evt);
            }
        });
        jScrollPane1.setViewportView(jListCmdsAvailable);

        jLabel1.setText("Module:");

        jCheckBoxModifyStatus.setText("Modify Stat.");
        jCheckBoxModifyStatus.setToolTipText("Select mode to modify the status of this module instead of sending commands.");
        jCheckBoxModifyStatus.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBoxModifyStatus.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxModifyStatusActionPerformed(evt);
            }
        });

        jCheckBoxDetailsModuleConnected.setText("Connected");
        jCheckBoxDetailsModuleConnected.setToolTipText("Connect or Disconnect from the command and Status NML channels for this module.");
        jCheckBoxDetailsModuleConnected.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBoxDetailsModuleConnected.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxDetailsModuleConnectedActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jCheckBoxModifyStatus)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jCheckBoxDetailsModuleConnected))
                    .addComponent(jLabel2)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 346, Short.MAX_VALUE)
                    .addComponent(jScrollPaneModulesList, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 346, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jCheckBoxModifyStatus)
                    .addComponent(jCheckBoxDetailsModuleConnected))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPaneModulesList, javax.swing.GroupLayout.DEFAULT_SIZE, 76, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 77, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTextPaneDetailsBottom.setEditable(false);
        jScrollPaneDetailsText.setViewportView(jTextPaneDetailsBottom);

        jTextFieldCurrentCommandMsgType.setEditable(false);
        jTextFieldCurrentCommandMsgType.setText("<< Current Command Type>>");

        jTextFieldCurrentStatusType.setEditable(false);
        jTextFieldCurrentStatusType.setText("<< Current Status Type >>");

        jTextFieldCmdToSendMsgType.setEditable(false);
        jTextFieldCmdToSendMsgType.setText("<<Cmd To Send Msg Type >>");

        javax.swing.GroupLayout jPanelDetailsLayout = new javax.swing.GroupLayout(jPanelDetails);
        jPanelDetails.setLayout(jPanelDetailsLayout);
        jPanelDetailsLayout.setHorizontalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPaneDetailsText, javax.swing.GroupLayout.DEFAULT_SIZE, 704, Short.MAX_VALUE)
                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                        .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonPlotCmdArray, javax.swing.GroupLayout.DEFAULT_SIZE, 370, Short.MAX_VALUE)
                            .addComponent(jButtonPlotCmdVar, javax.swing.GroupLayout.DEFAULT_SIZE, 370, Short.MAX_VALUE)
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                    .addComponent(jLabel9, javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel4, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                        .addGap(4, 4, 4)
                                        .addComponent(jLabelCmdsRecvd, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jTextFieldCurrentCommandMsgType, javax.swing.GroupLayout.DEFAULT_SIZE, 188, Short.MAX_VALUE))))
                            .addComponent(watchJPanelDetailsCmd, 0, 0, Short.MAX_VALUE)
                            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonPlotStatusArray, javax.swing.GroupLayout.DEFAULT_SIZE, 322, Short.MAX_VALUE)
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addComponent(jLabel6, javax.swing.GroupLayout.PREFERRED_SIZE, 117, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldCurrentStatusType, javax.swing.GroupLayout.DEFAULT_SIZE, 193, Short.MAX_VALUE))
                            .addComponent(jButtonPlotStatusVar, javax.swing.GroupLayout.DEFAULT_SIZE, 322, Short.MAX_VALUE)
                            .addComponent(watchJPanelDetailsStatus, javax.swing.GroupLayout.DEFAULT_SIZE, 322, Short.MAX_VALUE)
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addComponent(jLabel3)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jTextFieldCmdToSendMsgType, javax.swing.GroupLayout.DEFAULT_SIZE, 51, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonAdjustCmdToSend, javax.swing.GroupLayout.PREFERRED_SIZE, 75, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonSendDetailsCommand))
                            .addComponent(watchJPanelCmdToSend, javax.swing.GroupLayout.DEFAULT_SIZE, 322, Short.MAX_VALUE)
                            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                                .addComponent(jLabel11)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelStatusRecvd)))))
                .addContainerGap())
        );
        jPanelDetailsLayout.setVerticalGroup(
            jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelDetailsLayout.createSequentialGroup()
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel3, javax.swing.GroupLayout.PREFERRED_SIZE, 15, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonSendDetailsCommand)
                            .addComponent(jButtonAdjustCmdToSend)
                            .addComponent(jTextFieldCmdToSendMsgType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(watchJPanelCmdToSend, javax.swing.GroupLayout.DEFAULT_SIZE, 204, Short.MAX_VALUE))
                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                        .addGap(20, 20, 20)
                        .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jTextFieldCurrentCommandMsgType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jTextFieldCurrentStatusType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel9)
                    .addComponent(jLabel11)
                    .addComponent(jLabelCmdsRecvd)
                    .addComponent(jLabelStatusRecvd))
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(watchJPanelDetailsCmd, javax.swing.GroupLayout.DEFAULT_SIZE, 167, Short.MAX_VALUE))
                    .addGroup(jPanelDetailsLayout.createSequentialGroup()
                        .addGap(6, 6, 6)
                        .addComponent(watchJPanelDetailsStatus, 0, 0, Short.MAX_VALUE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonPlotStatusVar)
                    .addComponent(jButtonPlotCmdVar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelDetailsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonPlotStatusArray)
                    .addComponent(jButtonPlotCmdArray, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPaneDetailsText, javax.swing.GroupLayout.PREFERRED_SIZE, 44, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("<F2> Cmd/Status Details", jPanelDetails);

        jListAuxChannels.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListAuxChannelsValueChanged(evt);
            }
        });
        jScrollPane2.setViewportView(jListAuxChannels);

        jLabel5.setText("Auxilliary Channels:");

        jLabel7.setText("Available Messages:");

        jListAuxMessagesAvailable.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListAuxMessagesAvailableValueChanged(evt);
            }
        });
        jScrollPane3.setViewportView(jListAuxMessagesAvailable);

        jLabel8.setText("Msg to Send:");

        jLabelAuxMsgToSendType.setText("<< Msg to Send Type >> ");
        jLabelAuxMsgToSendType.setMinimumSize(new java.awt.Dimension(0, 15));

        jButtonSendAuxMsg.setText("Send");
        jButtonSendAuxMsg.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSendAuxMsgActionPerformed(evt);
            }
        });

        jLabel12.setText("Message Count:");

        jLabelAuxRecvd.setText("<< Message Count >>");

        jLabel16.setText("Message Type:");

        jLabelAuxType.setText("<< Message Type >>");

        jButtonPlotAuxArray.setText("Plot This Array");
        jButtonPlotAuxArray.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotAuxArrayActionPerformed(evt);
            }
        });

        jButtonPlotAuxVar.setText("Plot This Variable");
        jButtonPlotAuxVar.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonPlotAuxVarActionPerformed(evt);
            }
        });

        jTextPaneAuxBottom.setEditable(false);
        jScrollPane6.setViewportView(jTextPaneAuxBottom);

        jCheckBoxAuxChannelConnected.setText("Connected");
        jCheckBoxAuxChannelConnected.setBorder(javax.swing.BorderFactory.createEmptyBorder(0, 0, 0, 0));
        jCheckBoxAuxChannelConnected.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxAuxChannelConnectedActionPerformed(evt);
            }
        });

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        buttonGroupAuxCmdStatNormal.add(jRadioButtonAuxCmd);
        jRadioButtonAuxCmd.setText("Cmd");

        buttonGroupAuxCmdStatNormal.add(jRadioButtonAuxStat);
        jRadioButtonAuxStat.setText("Stat");

        buttonGroupAuxCmdStatNormal.add(jRadioButtonAuxNormal);
        jRadioButtonAuxNormal.setSelected(true);
        jRadioButtonAuxNormal.setText("Normal");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAuxNormal)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAuxCmd)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAuxStat)
                .addContainerGap(86, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonAuxCmd)
                    .addComponent(jRadioButtonAuxStat)
                    .addComponent(jRadioButtonAuxNormal))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelAuxChannelsLayout = new javax.swing.GroupLayout(jPanelAuxChannels);
        jPanelAuxChannels.setLayout(jPanelAuxChannelsLayout);
        jPanelAuxChannelsLayout.setHorizontalGroup(
            jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelAuxChannelsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane6, javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel5)
                                    .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))
                                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                        .addGap(12, 12, 12)
                                        .addComponent(jLabel7)
                                        .addGap(83, 83, 83))
                                    .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jScrollPane3, 0, 0, Short.MAX_VALUE))))
                            .addComponent(watchJPanelAuxView, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                .addComponent(jLabel8)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelAuxMsgToSendType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jButtonSendAuxMsg))
                            .addComponent(watchJPanelAuxMsgToSend, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonPlotAuxVar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jButtonPlotAuxArray, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(jLabel12)
                                            .addComponent(jLabel16))
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                            .addComponent(jLabelAuxRecvd)
                                            .addComponent(jLabelAuxType)))
                                    .addComponent(jCheckBoxAuxChannelConnected)
                                    .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addGap(0, 0, Short.MAX_VALUE)))))
                .addGap(0, 0, 0))
        );
        jPanelAuxChannelsLayout.setVerticalGroup(
            jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jLabel7)
                    .addComponent(jLabel8)
                    .addComponent(jLabelAuxMsgToSendType, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonSendAuxMsg))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(watchJPanelAuxMsgToSend, javax.swing.GroupLayout.DEFAULT_SIZE, 258, Short.MAX_VALUE)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 258, Short.MAX_VALUE)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 258, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanelAuxChannelsLayout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jCheckBoxAuxChannelConnected)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel12)
                            .addComponent(jLabelAuxRecvd))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanelAuxChannelsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel16)
                            .addComponent(jLabelAuxType))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPlotAuxVar)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPlotAuxArray))
                    .addComponent(watchJPanelAuxView, javax.swing.GroupLayout.DEFAULT_SIZE, 259, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane6, javax.swing.GroupLayout.PREFERRED_SIZE, 30, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("<F3> Aux. Channels", jPanelAuxChannels);

        jLabelNMLBufferLine.setText("<< Buffer Line >>");

        jScrollPane4.setViewportView(jTextPaneNmlError);

        jLabel15.setText("Errors:");

        jLabel21.setText("Buffer Line:");

        jTableNML.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {
                "Connected", "Buffer Name ", "Host", "Message Type", "Message Count"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.Boolean.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Integer.class
            };
            boolean[] canEdit = new boolean [] {
                true, false, false, false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        jScrollPane5.setViewportView(jTableNML);

        jLabel25.setText("NML Configuration: ");

        jLabelNMLConfigFile.setText("<< NML Configuration File >>");

        jLabelNMLNumConnectedOfTotal.setText("<< Number Connected of Total >>");

        javax.swing.GroupLayout jPanelNMLLayout = new javax.swing.GroupLayout(jPanelNML);
        jPanelNML.setLayout(jPanelNMLLayout);
        jPanelNMLLayout.setHorizontalGroup(
            jPanelNMLLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelNMLLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelNMLLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane4, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 696, Short.MAX_VALUE)
                    .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 696, Short.MAX_VALUE)
                    .addComponent(jLabelNMLNumConnectedOfTotal, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, 696, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanelNMLLayout.createSequentialGroup()
                        .addComponent(jLabel21)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelNMLBufferLine, javax.swing.GroupLayout.DEFAULT_SIZE, 586, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, jPanelNMLLayout.createSequentialGroup()
                        .addComponent(jLabel25)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelNMLConfigFile, javax.swing.GroupLayout.DEFAULT_SIZE, 514, Short.MAX_VALUE))
                    .addComponent(jLabel15, javax.swing.GroupLayout.Alignment.LEADING))
                .addContainerGap())
        );
        jPanelNMLLayout.setVerticalGroup(
            jPanelNMLLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelNMLLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelNMLLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel25)
                    .addComponent(jLabelNMLConfigFile))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabelNMLNumConnectedOfTotal)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane5, javax.swing.GroupLayout.DEFAULT_SIZE, 379, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel15)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane4, javax.swing.GroupLayout.DEFAULT_SIZE, 114, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanelNMLLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel21)
                    .addComponent(jLabelNMLBufferLine))
                .addContainerGap())
        );

        jTabbedPane1.addTab("<F4> NML Table", jPanelNML);
        jTabbedPane1.addTab("<F5> Graph", plotter_NB_UI1);

        jTextAreaErrs.setColumns(20);
        jTextAreaErrs.setEditable(false);
        jTextAreaErrs.setRows(5);
        jScrollPane7.setViewportView(jTextAreaErrs);

        javax.swing.GroupLayout jPanelErrorPanelLayout = new javax.swing.GroupLayout(jPanelErrorPanel);
        jPanelErrorPanel.setLayout(jPanelErrorPanelLayout);
        jPanelErrorPanelLayout.setHorizontalGroup(
            jPanelErrorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelErrorPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane7, javax.swing.GroupLayout.DEFAULT_SIZE, 696, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanelErrorPanelLayout.setVerticalGroup(
            jPanelErrorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelErrorPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane7, javax.swing.GroupLayout.DEFAULT_SIZE, 595, Short.MAX_VALUE)
                .addContainerGap())
        );

        jTabbedPane1.addTab("Errs", jPanelErrorPanel);

        jMenuFile.setMnemonic('f');
        jMenuFile.setText("File");

        jMenuItemFileOpen.setMnemonic('o');
        jMenuItemFileOpen.setText("Open");
        jMenuItemFileOpen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFileOpenActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemFileOpen);

        jMenuItemFileExit.setMnemonic('x');
        jMenuItemFileExit.setText("Exit");
        jMenuItemFileExit.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFileExitActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemFileExit);

        jMenuFileRecent.setMnemonic('r');
        jMenuFileRecent.setText("Recent");
        jMenuFile.add(jMenuFileRecent);

        jMenuItemFileDebugDump.setText("Debug Dump");
        jMenuItemFileDebugDump.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFileDebugDumpActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemFileDebugDump);

        jMenuItemDumpStruct.setText("Dump Struct");
        jMenuItemDumpStruct.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemDumpStructActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemDumpStruct);

        jMenuItemOpenMsgFile.setText("Open Message File");
        jMenuItemOpenMsgFile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemOpenMsgFileActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemOpenMsgFile);

        jMenuItemFileEditDiagCfg.setText("Open Diag. Config. File in External Editor");
        jMenuItemFileEditDiagCfg.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFileEditDiagCfgActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemFileEditDiagCfg);

        jMenuItemFileEditNml.setText("Open NML Config. File in External Editor");
        jMenuItemFileEditNml.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemFileEditNmlActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemFileEditNml);

        jMenuItemPrintHierarchy.setText("Print Hierarchy . . .");
        jMenuItemPrintHierarchy.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemPrintHierarchyActionPerformed(evt);
            }
        });
        jMenuFile.add(jMenuItemPrintHierarchy);

        jMenuBarMain.add(jMenuFile);

        jMenuView.setMnemonic('v');
        jMenuView.setText("View");

        jCheckBoxMenuItemFullscreen.setText("Full Screen");
        jCheckBoxMenuItemFullscreen.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItemFullscreenActionPerformed(evt);
            }
        });
        jMenuView.add(jCheckBoxMenuItemFullscreen);

        jCheckBoxMenuItemShowAdjusterWindow.setText("Show Adjuster Window");
        jCheckBoxMenuItemShowAdjusterWindow.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItemShowAdjusterWindowActionPerformed(evt);
            }
        });
        jMenuView.add(jCheckBoxMenuItemShowAdjusterWindow);

        jMenuLookAndFeel.setText("Look and Feel");
        jMenuView.add(jMenuLookAndFeel);

        jMenuBarMain.add(jMenuView);

        jMenuConnections.setMnemonic('n');
        jMenuConnections.setText("Connections");

        jCheckBoxMenuItemConnected.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F12, 0));
        jCheckBoxMenuItemConnected.setMnemonic('c');
        jCheckBoxMenuItemConnected.setText("Connected");
        jCheckBoxMenuItemConnected.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jCheckBoxMenuItemConnectedItemStateChanged(evt);
            }
        });
        jCheckBoxMenuItemConnected.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItemConnectedActionPerformed(evt);
            }
        });
        jMenuConnections.add(jCheckBoxMenuItemConnected);

        jMenuItemOpenNmlConfigFile.setText("Open NML Config File . . . ");
        jMenuItemOpenNmlConfigFile.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemOpenNmlConfigFileActionPerformed(evt);
            }
        });
        jMenuConnections.add(jMenuItemOpenNmlConfigFile);

        jMenuConnectionsRecentNmlFiles.setText("Recent NML  Configs");
        jMenuConnections.add(jMenuConnectionsRecentNmlFiles);

        jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.setText("Automatically Connect/Disconnect As Needed");
        jMenuConnections.add(jCheckBoxMenuItemAutoConnectDisconnectAsNeeded);

        jMenuItemOpenNmlCfgsvrConnection.setText("Open nmlcfgsvr connection . . .");
        jMenuItemOpenNmlCfgsvrConnection.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemOpenNmlCfgsvrConnectionActionPerformed(evt);
            }
        });
        jMenuConnections.add(jMenuItemOpenNmlCfgsvrConnection);

        jMenuItemOpenInputHeader.setText("Open Input Header(s) . . .");
        jMenuItemOpenInputHeader.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemOpenInputHeaderActionPerformed(evt);
            }
        });
        jMenuConnections.add(jMenuItemOpenInputHeader);

        jMenuBarMain.add(jMenuConnections);

        jMenuPlotting.setMnemonic('p');
        jMenuPlotting.setText("Plotting");

        jMenuItemPlottingSavePlotSet.setText("Save Plot Set . . .");
        jMenuItemPlottingSavePlotSet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemPlottingSavePlotSetActionPerformed(evt);
            }
        });
        jMenuPlotting.add(jMenuItemPlottingSavePlotSet);

        jMenuItemPlottingOpenPlotSet.setText("Open Plot Set . . .");
        jMenuItemPlottingOpenPlotSet.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemPlottingOpenPlotSetActionPerformed(evt);
            }
        });
        jMenuPlotting.add(jMenuItemPlottingOpenPlotSet);

        jMenuPlottingRecentPlotSets.setText("Recent Plot Sets");
        jMenuPlotting.add(jMenuPlottingRecentPlotSets);

        jCheckBoxMenuItemAutoSavePlotSets.setText("Automatically Save Plot Sets");
        jCheckBoxMenuItemAutoSavePlotSets.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxMenuItemAutoSavePlotSetsActionPerformed(evt);
            }
        });
        jMenuPlotting.add(jCheckBoxMenuItemAutoSavePlotSets);

        jMenuBarMain.add(jMenuPlotting);

        jMenuExtras.setText("Extras");
        jMenuBarMain.add(jMenuExtras);

        jMenuHelp.setMnemonic('h');
        jMenuHelp.setText("Help");

        jMenuItemHelpRcsLibrary.setText("RCS Library");
        jMenuItemHelpRcsLibrary.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jMenuItemHelpRcsLibraryActionPerformed(evt);
            }
        });
        jMenuHelp.add(jMenuItemHelpRcsLibrary);

        jMenuBarMain.add(jMenuHelp);

        setJMenuBar(jMenuBarMain);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 732, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTabbedPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 685, Short.MAX_VALUE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void ConnectCurAuxBuffer() throws Exception {
        ConnectCurAuxBuffer(false,false);
    }
    
    private void ConnectCurAuxBuffer(boolean iscmd, boolean isstat) throws Exception {
        BufferInfo bi = curAuxBuffer;
        if (null == bi || bi.isConnected()) {
            return;
        }
        DiagNMLMsgDictInterface msg_dict = bi.read_msg_dict;
        if(iscmd) {
            msg_dict = (DiagNMLMsgDictInterface) ((ModuleInfo)bi.mi).get_cmd_msg_dict();
            bi.read_nml = null;
        } else if (isstat) {
            msg_dict = (DiagNMLMsgDictInterface) ((ModuleInfo)bi.mi).get_stat_msg_dict();
            bi.read_nml = null;
        }
        if (null == bi.read_nml && null != msg_dict) {
            bi.read_nml = new NMLConnection();
            bi.read_nml.set_buffer_name(bi.Name);
            bi.read_nml.set_process_name("jdiag");
            bi.read_nml.set_configuration_file(bi.configFile);
            bi.read_nml.SetMessageDictionary(msg_dict);
            bi.read_nml.ReadNMLConfigurationFileNoThrow();
            bi.read_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
        }

        if (null != bi.read_nml) {
            bi.read_nml.connect();
        }
        if (!double_buffer_nml) {
            bi.write_nml = bi.read_nml;
        } else if (null == bi.write_nml && null != bi.write_msg_dict) {
            bi.write_nml = new NMLConnection();
            bi.write_nml.set_buffer_name(bi.Name);
            bi.write_nml.set_process_name("jdiag");
            bi.write_nml.set_configuration_file(bi.configFile);
            bi.write_nml.SetMessageDictionary(bi.read_msg_dict);
            bi.write_nml.ReadNMLConfigurationFileNoThrow();
            bi.write_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
        }
        if (bi.write_nml != null && bi.write_nml != bi.read_nml && !double_buffer_nml) {
            bi.write_nml.connect();
        }
        boolean connected_now = bi.isConnected();
        if (connected_now) {
            this.jCheckBoxAuxChannelConnected.setSelected(true);
        } else {
            throw new Exception("Buffer " + bi.Name + " still not connected after call to ConnectCurAuxBuffer()");
        }
    }

    private void DisconnectCurAuxBuffer() {
        if (null != curAuxBuffer.read_nml) {
            curAuxBuffer.read_nml.disconnect();
        }
        if (null != curAuxBuffer.write_nml && curAuxBuffer.write_nml != curAuxBuffer.read_nml) {
            curAuxBuffer.read_nml.disconnect();
        }
    }

    private void jCheckBoxAuxChannelConnectedActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxAuxChannelConnectedActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxAuxChannelConnectedActionPerformed
        try {
            if (curAuxBuffer != null) {
                if (this.jCheckBoxAuxChannelConnected.isSelected()) {
                    ConnectCurAuxBuffer(this.jRadioButtonAuxCmd.isSelected(),
                            this.jRadioButtonAuxStat.isSelected());
                } else {
                    DisconnectCurAuxBuffer();
                }
                LoadNmlTable();
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_jCheckBoxAuxChannelConnectedActionPerformed

    private void jCheckBoxDetailsModuleConnectedActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxDetailsModuleConnectedActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxDetailsModuleConnectedActionPerformed
        try {
            if (curModule != null) {
                if (jCheckBoxDetailsModuleConnected.isSelected()) {
                    curModule.connect();
                } else {
                    curModule.disconnect();
                }
                LoadNmlTable();
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_jCheckBoxDetailsModuleConnectedActionPerformed

    private void formWindowStateChanged(java.awt.event.WindowEvent evt)//GEN-FIRST:event_formWindowStateChanged
    {//GEN-HEADEREND:event_formWindowStateChanged
        try {
            dp.setWindow_state(evt.getNewState());
            if (evt.getNewState() == JFrame.ICONIFIED) {
                if (this.plot_tracker_hashtable == null || this.plot_tracker_hashtable.size() < 1) {
                    if (javax_swing_timer.isRunning()) {
                        javax_swing_timer.stop();
                    }
                }
            } else if (evt.getOldState() == JFrame.ICONIFIED && dp.get_connected()) {
                if (!javax_swing_timer.isRunning()) {
                    javax_swing_timer.start();
                }
            }
        } catch (Exception e) {
            diagapplet.utils.DiagError.println("evt = " + evt);
            PrintException(e);
        }
    }//GEN-LAST:event_formWindowStateChanged

    private void jButtonAdjustCmdToSendActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonAdjustCmdToSendActionPerformed
    {//GEN-HEADEREND:event_jButtonAdjustCmdToSendActionPerformed
        try {
            jCheckBoxMenuItemShowAdjusterWindow.setSelected(true);
            LoadAdjusterWindow();
            jFrameAdjustVar.setVisible(true);
            jFrameAdjustVar.pack();

        } catch (Exception exception) {
            PrintException(exception);
        }
    }//GEN-LAST:event_jButtonAdjustCmdToSendActionPerformed

    private void jComboBoxSelectAdjustVarActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jComboBoxSelectAdjustVarActionPerformed
    {//GEN-HEADEREND:event_jComboBoxSelectAdjustVarActionPerformed
        int var_num = jComboBoxSelectAdjustVar.getSelectedIndex();
        if (var_num >= 0) {
            double value = Double.valueOf(watchJPanelCmdToSend.getValueStringForVarNumber(var_num)).doubleValue();
            SetjScrollBarAdjustVarValue(value);
            SetAdjusterTitle(value);
        }
    }//GEN-LAST:event_jComboBoxSelectAdjustVarActionPerformed

    private void jButtonAdjusterCloseActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonAdjusterCloseActionPerformed
    {//GEN-HEADEREND:event_jButtonAdjusterCloseActionPerformed
        jFrameAdjustVar.setVisible(false);
        jCheckBoxMenuItemShowAdjusterWindow.setSelected(false);
    }//GEN-LAST:event_jButtonAdjusterCloseActionPerformed

    private void jTextFieldAdjustVarMaxActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldAdjustVarMaxActionPerformed
    {//GEN-HEADEREND:event_jTextFieldAdjustVarMaxActionPerformed
        SetjScrollBarAdjustVarValues();
    }//GEN-LAST:event_jTextFieldAdjustVarMaxActionPerformed

    private void jTextFieldAdjustVarMinActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldAdjustVarMinActionPerformed
    {//GEN-HEADEREND:event_jTextFieldAdjustVarMinActionPerformed
        SetjScrollBarAdjustVarValues();
    }//GEN-LAST:event_jTextFieldAdjustVarMinActionPerformed

    private void SetjScrollBarAdjustVarValues() {
        try {
            int ivalue = jScrollBarAdjustVar.getValue();
            int imin = jScrollBarAdjustVar.getMinimum();
            int imax = jScrollBarAdjustVar.getMaximum();
            String sMax = jTextFieldAdjustVarMax.getText();
            double max = Double.valueOf(sMax).doubleValue();
            String sMin = jTextFieldAdjustVarMin.getText();
            double min = Double.valueOf(sMin).doubleValue();
            double diff = max - min;
            int idiff = imax - imin;
            String sInc = jTextFieldAdjustVarIncrement.getText();
            double new_inc = Double.valueOf(sInc).doubleValue();
            double ratio_over = ((double) (imax - ivalue)) / ((double) idiff);
//            double ratio_under = ((double) (ivalue)) / ((double) idiff);
            double value = ((double) ivalue - imin) * (diff) / ((double) (idiff));
            double new_count = diff / new_inc;
//            boolean was_sending_each_adjustment = jCheckBoxSendEachAdjustmentChange.isSelected();
            jCheckBoxSendEachAdjustmentChange.setSelected(false);
            jScrollBarAdjustVar.setMaximum((int) (new_count));
            jScrollBarAdjustVar.setMinimum((int) 0);
            int new_ivalue = (int) ((value - min) / new_inc);
            jScrollBarAdjustVar.setValue(new_ivalue);
            SetAdjusterTitle(value);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void SetjScrollBarAdjustVarValue(double new_value) {
        try {
//            int ivalue = jScrollBarAdjustVar.getValue();
            int imin = jScrollBarAdjustVar.getMinimum();
            int imax = jScrollBarAdjustVar.getMaximum();
            String sMax = jTextFieldAdjustVarMax.getText();
            double max = Double.valueOf(sMax).doubleValue();
            if (new_value > max) {
                max = new_value;
                jTextFieldAdjustVarMax.setText(Double.toString(new_value));
            }
            String sMin = jTextFieldAdjustVarMin.getText();
            double min = Double.valueOf(sMin).doubleValue();
            if (new_value < min) {
                min = new_value;
                jTextFieldAdjustVarMax.setText(Double.toString(new_value));
            }
            double diff = max - min;
            int idiff = imax - imin;
            String sInc = jTextFieldAdjustVarIncrement.getText();
            double new_inc = Double.valueOf(sInc).doubleValue();
            double new_count = diff / new_inc;
            boolean was_sending_each_adjustment = jCheckBoxSendEachAdjustmentChange.isSelected();
            jCheckBoxSendEachAdjustmentChange.setSelected(false);
            jScrollBarAdjustVar.setMaximum((int) (new_count));
            jScrollBarAdjustVar.setMinimum((int) 0);
            int new_ivalue = (int) ((new_value - min) / new_inc);
            jScrollBarAdjustVar.setValue(new_ivalue);
            SetAdjusterTitle(new_value);
            if (was_sending_each_adjustment) {
                jCheckBoxSendEachAdjustmentChange.setSelected(true);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void jTextFieldAdjustVarIncrementActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jTextFieldAdjustVarIncrementActionPerformed
    {//GEN-HEADEREND:event_jTextFieldAdjustVarIncrementActionPerformed
        SetjScrollBarAdjustVarValues();
    }//GEN-LAST:event_jTextFieldAdjustVarIncrementActionPerformed

    private void SetAdjusterTitle(double value) {
        jFrameAdjustVar.setTitle("Adjuster : " + jComboBoxSelectAdjustVar.getSelectedItem() + " = " + value);
    }
    private int last_jScrollBarAdjustVar_ivalue = -99;

    private void jScrollBarAdjustVarAdjustmentValueChanged(java.awt.event.AdjustmentEvent evt)//GEN-FIRST:event_jScrollBarAdjustVarAdjustmentValueChanged
    {//GEN-HEADEREND:event_jScrollBarAdjustVarAdjustmentValueChanged
        try {
            if (evt.getSource() == this.jScrollBarAdjustVar) {
                int ivalue = jScrollBarAdjustVar.getValue();
                if (ivalue != last_jScrollBarAdjustVar_ivalue) {
                    int imin = jScrollBarAdjustVar.getMinimum();
                    int imax = jScrollBarAdjustVar.getMaximum();
                    String sMax = jTextFieldAdjustVarMax.getText();
                    double max = Double.valueOf(sMax).doubleValue();
                    String sMin = jTextFieldAdjustVarMin.getText();
                    double min = Double.valueOf(sMin).doubleValue();
                    double value = ((double) ivalue - imin) * (max - min) / ((double) (imax - imin));
                    this.jFormattedTextFieldAdjusterValue.setText(Double.toString(value));
                    int var_number = jComboBoxSelectAdjustVar.getSelectedIndex();
                    watchJPanelCmdToSend.setValueForVarNumber(var_number, value);
                    SetAdjusterTitle(value);
                    if (jCheckBoxSendEachAdjustmentChange.isSelected()) {
                        if (jCheckBoxModifyStatus.isSelected()) {
                            SendDetailsStatus();
                        } else {
                            SendDetailsCommand();
                        }
                    }
                    last_jScrollBarAdjustVar_ivalue = ivalue;
                }
            }

        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }//GEN-LAST:event_jScrollBarAdjustVarAdjustmentValueChanged

    private void LoadAdjusterWindow() {
        try {
            if (jCheckBoxMenuItemShowAdjusterWindow.isSelected()) {
                jComboBoxSelectAdjustVar.setEnabled(false);
                jTextFieldAdjustVarMax.setEnabled(false);
                jTextFieldAdjustVarMin.setEnabled(false);
                jTextFieldAdjustVarIncrement.setEnabled(false);
                jScrollBarAdjustVar.setEnabled(false);
                jComboBoxSelectAdjustVar.removeAllItems();
                if (null != curModule && null != cmdToSendIdString) {
                    Long idLong = Long.valueOf(cmdToSendIdString);
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoHashTable.get(idLong);
                    STI_TokenizerInterface tokenizer = typeInfo.getInfoTokenizer();
                    while (tokenizer.hasMoreTokens()) {
                        jComboBoxSelectAdjustVar.addItem(tokenizer.nextToken());
                    }
                    int var_num = watchJPanelCmdToSend.GetVarNum();
                    if (var_num >= 0) {
                        jComboBoxSelectAdjustVar.setSelectedIndex(var_num);
                        try {
                            SetjScrollBarAdjustVarValue(Double.valueOf(watchJPanelCmdToSend.getValueStringForVarNumber(var_num)).doubleValue());
                        } catch (Exception e) {
                            PrintException(e);
                        }
                    }
                    jComboBoxSelectAdjustVar.setEnabled(true);
                    jTextFieldAdjustVarMax.setEnabled(true);
                    jTextFieldAdjustVarMin.setEnabled(true);
                    jTextFieldAdjustVarIncrement.setEnabled(true);
                    jScrollBarAdjustVar.setEnabled(true);
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void jCheckBoxMenuItemShowAdjusterWindowActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxMenuItemShowAdjusterWindowActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxMenuItemShowAdjusterWindowActionPerformed
        try {
            if (jCheckBoxMenuItemShowAdjusterWindow.isSelected()) {
                LoadAdjusterWindow();
                jFrameAdjustVar.setVisible(true);
                jFrameAdjustVar.pack();
            } else {
                jFrameAdjustVar.setVisible(false);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_jCheckBoxMenuItemShowAdjusterWindowActionPerformed

    private void formComponentMoved(java.awt.event.ComponentEvent evt)//GEN-FIRST:event_formComponentMoved
    {//GEN-HEADEREND:event_formComponentMoved
        if (this.getExtendedState() == JFrame.NORMAL) {
            dp.setMoved(true);
            Point p = this.getLocation();
            dp.setX(p.x);
            dp.setY(p.y);
        }
    }//GEN-LAST:event_formComponentMoved

    private void formComponentResized(java.awt.event.ComponentEvent evt)//GEN-FIRST:event_formComponentResized
    {//GEN-HEADEREND:event_formComponentResized
        try {
            if (null != dp && this.getExtendedState() == JFrame.NORMAL) {
                dp.setResized(true);
                Dimension s = getSize();
                dp.setWidth(s.width);
                dp.setHeight(s.height);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_formComponentResized

    private void jCheckBoxMenuItemFullscreenActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxMenuItemFullscreenActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxMenuItemFullscreenActionPerformed
        if (jCheckBoxMenuItemFullscreen.isSelected()) {
            SetFullScreenMode();
        } else {
            ExitFullScreen();
        }

    }//GEN-LAST:event_jCheckBoxMenuItemFullscreenActionPerformed

    private void jCheckBoxModifyStatusActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxModifyStatusActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxModifyStatusActionPerformed
        if (jCheckBoxModifyStatus.isSelected()) {
            jListCmdsAvailable.setListData(curModule.statMsgsAvailable);
        } else {
            jListCmdsAvailable.setListData(curModule.cmdsAvailable);
        }
        this.jTextFieldCmdToSendMsgType.setText("");
        this.watchJPanelCmdToSend.Clear();
        if (!jCheckBoxModifyStatus.isSelected()) {
            this.jListCmdsAvailable.setSelectedIndex(curModule.last_selected_command_index);
        }
    }//GEN-LAST:event_jCheckBoxModifyStatusActionPerformed

    private void LoadPlotSetFile(String filename) throws Exception {

        XMLDecoder decoder = null;
        try {
            decoder = new XMLDecoder(
                    new BufferedInputStream(
                    new FileInputStream(filename)));
            plotSetPreserve psP = (plotSetPreserve) decoder.readObject();
            LoadPlotSet(psP);
        } catch (Exception e) {
            if (null != decoder) {
                decoder.close();
                decoder = null;
            }
            throw e;
        } finally {
            if (null != decoder) {
                decoder.close();
                decoder = null;
            }
        }
        AddRecentPlotSet(filename);
    }

    private void jMenuItemPlottingOpenPlotSetActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemPlottingOpenPlotSetActionPerformed
    {//GEN-HEADEREND:event_jMenuItemPlottingOpenPlotSetActionPerformed
        try {
            FileNameExtensionFilter plot_set_filter = new FileNameExtensionFilter("plot_set", "plot_set");
            JFileChooser chooser = new JFileChooser();
            chooser.addChoosableFileFilter(plot_set_filter);
            if (last_dir != null) {
                chooser.setCurrentDirectory(last_dir);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
            int returnVal = chooser.showOpenDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                System.out.println("You chose to open this file: "
                        + chooser.getSelectedFile().getPath());
                last_dir = chooser.getCurrentDirectory();
                rcs.utils.URL_and_FileLoader.current_directory = last_dir.getPath();
                rcs.utils.URL_and_FileLoader.AddToSearchPath(last_dir.getPath());
                LoadPlotSetFile(chooser.getSelectedFile().getPath());
                jTabbedPane1.setSelectedComponent(plotter_NB_UI1);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_jMenuItemPlottingOpenPlotSetActionPerformed

    private void AddRecentPlotSetMenuItem(final String filename) {
        try {
            JMenuItem jmi = new JMenuItem(filename);
            jmi.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {


                    try {
                        LoadPlotSetFile(filename);
                        jTabbedPane1.setSelectedComponent(plotter_NB_UI1);
                    } catch (Exception except) {
                        except.printStackTrace();
                    }
                }
            });
            jMenuPlottingRecentPlotSets.add(jmi);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void AddRecentPlotSet(final String filename) {
        Vector<String> recentPlotSets = dp.getRecentPlotSets();
        if (recentPlotSets == null) {
            recentPlotSets = new Vector<String>();
        }
        if (!recentPlotSets.contains(filename)) {
            AddRecentPlotSetMenuItem(filename);
            recentPlotSets.insertElementAt(filename, 0);
            dp.setRecentPlotSets(recentPlotSets);
        }
    }

    private void SavePlotSet(String fileName) {
        try {
            Vector<String> recentPlotSets = dp.getRecentPlotSets();
            if (recentPlotSets == null) {
                recentPlotSets = new Vector<String>();
            }
            plotSetPreserve psP = getCurrentPlotSet();
            if (null != psP) {
                XMLEncoder encoder = new XMLEncoder(
                        new BufferedOutputStream(
                        new FileOutputStream(fileName)));
                encoder.writeObject(psP);
                encoder.close();
            }
            AddRecentPlotSet(fileName);
        } catch (Exception e) {
            PrintException(e);
        }
    }

    public void AutoSavePlotSet() {
        if (this.Automatically_Keep_and_Use_PlotSets) {
            String plot_set_name = "diag_" + (new Date()).toString().replace(' ', '_').replace(':', '_').replace('/', '_').replace('.', '_').replace(',', '_') + ".plot_set";
            SavePlotSet(new File(System.getProperty("user.dir"), plot_set_name).getAbsolutePath());
        }
    } // end AutoSavePlotSet()

    private void jMenuItemPlottingSavePlotSetActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemPlottingSavePlotSetActionPerformed
    {//GEN-HEADEREND:event_jMenuItemPlottingSavePlotSetActionPerformed
        FileNameExtensionFilter plot_set_filter = new FileNameExtensionFilter("plot_set", "plot_set");
        JFileChooser chooser = new JFileChooser();
        if (last_dir != null) {
            chooser.setCurrentDirectory(last_dir);
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        chooser.addChoosableFileFilter(plot_set_filter);
        int returnVal = chooser.showSaveDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: "
                    + chooser.getSelectedFile().getPath());
            last_dir = chooser.getCurrentDirectory();
            String path = chooser.getSelectedFile().getPath();
            if (!path.endsWith(".plot_set")) {
                path += ".plot_set";
            }
            SavePlotSet(path);
        }
    }//GEN-LAST:event_jMenuItemPlottingSavePlotSetActionPerformed

    private void jButtonPlotCmdArrayActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotCmdArrayActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotCmdArrayActionPerformed
        StartPlottingArray(watchJPanelDetailsCmd.getStructureTypeInfo(), null, curModule, last_cmd_id_long, cmd_selected_var_number, cmd_selected_var_name, true, false);
        jButtonPlotCmdArray.setEnabled(false);
    }//GEN-LAST:event_jButtonPlotCmdArrayActionPerformed

    private void jButtonPlotCmdVarActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotCmdVarActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotCmdVarActionPerformed
        StartPlottingVariable(null, curModule, last_cmd_id_long, cmd_selected_var_number, cmd_selected_var_name, true, false);
        jButtonPlotCmdVar.setEnabled(false);
    }//GEN-LAST:event_jButtonPlotCmdVarActionPerformed

    private void jButtonPlotStatusArrayActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotStatusArrayActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotStatusArrayActionPerformed
        StartPlottingArray(watchJPanelDetailsStatus.getStructureTypeInfo(), null, curModule, last_status_id_long, status_selected_var_number, status_selected_var_name, false, false);
        jButtonPlotStatusArray.setEnabled(false);
    }//GEN-LAST:event_jButtonPlotStatusArrayActionPerformed

    private void jPaintablePanelHierarchyInnerMousePressed(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jPaintablePanelHierarchyInnerMousePressed
    {//GEN-HEADEREND:event_jPaintablePanelHierarchyInnerMousePressed
        if (evt.isPopupTrigger()) {
            ShowHierarchyModuleCmdsPopup(evt.getX(), evt.getY());
        }
    }//GEN-LAST:event_jPaintablePanelHierarchyInnerMousePressed

    private void SendCmdByString(ModuleInfo mi, String cmd_string) {
        try {
//	    System.out.println("Send "+ cmd_string +" to " + mi.Name);
            mi.serial_number++;
            int eq_index = cmd_string.indexOf('=');
            if (eq_index > 0) {
                boolean module_found = false;
                for (int i = 0; i < this.jListModules.getVisibleRowCount(); i++) {
                    String name_from_list = (String) this.jListModules.getModel().getElementAt(i);
                    if (name_from_list.compareTo(mi.Name) == 0) {
                        this.jListModules.setSelectedIndex(i);
                        SelectModule(mi.Name);
                        module_found = true;
                        break;
                    }
                }
                boolean command_found = false;
                if (module_found) {
                    for (int i = 0; i < this.jListCmdsAvailable.getVisibleRowCount(); i++) {
                        String command_from_list = (String) this.jListCmdsAvailable.getModel().getElementAt(i);
                        if (command_from_list.compareTo(cmd_string) == 0) {
                            this.jListCmdsAvailable.setSelectedIndex(i);
                            if (jCheckBoxModifyStatus.isSelected()) {
                                this.jCheckBoxModifyStatus.setSelected(false);
                            }
                            LoadCmdToSend();
                            command_found = true;
                            break;
                        }
                    }
                }
                if (command_found) {
                    SendDetailsCommand();
                } else {
                    mi.serial_number++;
                    String idString = cmd_string.substring(eq_index + 1);
                    long cmd_id = Long.valueOf(idString).longValue();
                    mi.writeCmd(cmd_id + ",0," + mi.serial_number);
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }
    private JPopupMenu module_cmd_popup = null;

    private void ShowHierarchyModuleCmdsPopup(int x, int y) {
        try {
            final ModuleInfo mi = hierarchyDraw.getModuleByPosition(x, y);
            if (null != mi) {
                module_cmd_popup = new JPopupMenu();
                for (Object cmd_obj : mi.cmdsAvailable) {
                    final String cmd_string = (String) cmd_obj;
                    JMenuItem jmi = new JMenuItem(cmd_string);
                    jmi.addActionListener(new ActionListener() {

                        @Override
                        public void actionPerformed(ActionEvent e) {
                            SendCmdByString(mi, cmd_string);
                            if (null != module_cmd_popup) {
                                module_cmd_popup.setVisible(false);
                                module_cmd_popup = null;
                            }
                        }
                    });
                    module_cmd_popup.add(jmi);
                }
                module_cmd_popup.show(jPaintablePanelHierarchyInner, x, y);
            }
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private void jPaintablePanelHierarchyInnerMouseReleased(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jPaintablePanelHierarchyInnerMouseReleased
    {//GEN-HEADEREND:event_jPaintablePanelHierarchyInnerMouseReleased
        if (evt.isPopupTrigger()) {
            ShowHierarchyModuleCmdsPopup(evt.getX(), evt.getY());
        }
    }//GEN-LAST:event_jPaintablePanelHierarchyInnerMouseReleased
    private File last_nml_dir = null;

    private void jMenuItemOpenNmlConfigFileActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemOpenNmlConfigFileActionPerformed
    {//GEN-HEADEREND:event_jMenuItemOpenNmlConfigFileActionPerformed
//        javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {
//
//            @Override
//            public boolean accept(File f) {
//                return (f.isDirectory() ||
//                        f.getName().endsWith(".nml") ||
//                        f.getName().endsWith(".diag") ||
//                        f.getName().endsWith(".cfg"));
//            }
//
//            @Override
//            public String getDescription() {
//                return ".nml, .cfg or .diag";
//            }
//        };
        JFileChooser chooser = new JFileChooser();
        if (last_nml_dir != null) {
            chooser.setCurrentDirectory(last_nml_dir);
        } else if (last_dir != null) {
            chooser.setCurrentDirectory(last_dir);
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }
        FileNameExtensionFilter nml_filter = new FileNameExtensionFilter("nml", "nml");
        chooser.addChoosableFileFilter(nml_filter);
        int returnVal = chooser.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: "
                    + chooser.getSelectedFile().getPath());
            last_nml_dir = chooser.getCurrentDirectory();
            if (null == last_dir || !last_dir.canRead() || !last_dir.isDirectory()) {
                last_dir = last_nml_dir;
            }
            rcs.utils.URL_and_FileLoader.current_directory = last_nml_dir.getAbsolutePath();
            rcs.utils.URL_and_FileLoader.AddToSearchPath(rcs.utils.URL_and_FileLoader.current_directory);
            SetNMLConfigFile(chooser.getSelectedFile().getPath());
        }
    }//GEN-LAST:event_jMenuItemOpenNmlConfigFileActionPerformed

    /**
     * Searches the NML configuration file and adds a new AuxBuffer for each buffer that
     * is not queued and has the "header=" option set.
     * @param configuration_file configuration file to load
     */
    public void LoadAuxBuffersFromNmlConfigFile(String configuration_file) {
        try {
            LoadAuxChannelsHashtable();
            rcs.nml.NMLConfigInfo config_info = rcs.nml.NMLConnection.GetConfigInfo(configuration_file);
            if (null == config_info) {
                return;
            }
            Enumeration buffers_enum = config_info.buffer_configurations.elements();
            String dir = new File(configuration_file).getParent();
            if (config_info.include_dir_vector != null) {
                for (int i = 0; i < config_info.include_dir_vector.size(); i++) {
                    String s = (String) config_info.include_dir_vector.elementAt(i);
                    URL_and_FileLoader.AddToSearchPath(s);
                    File parentFile = new File(configuration_file).getAbsoluteFile().getParentFile();
                    while (s.startsWith("../")) {
                        if (null == parentFile) {
                            diagapplet.utils.DiagError.println("configuration_file=" + configuration_file + ", s=" + s + ",dir=" + dir);
                            break;
                        }
                        parentFile = parentFile.getParentFile();
                        s = s.substring(3);
                    }
                    URL_and_FileLoader.AddToSearchPath(parentFile.getAbsolutePath() + File.separatorChar + s);
                }
            }
            if (config_info.header_dir_vector != null) {
                for (int i = 0; i < config_info.header_dir_vector.size(); i++) {
                    String s = (String) config_info.header_dir_vector.elementAt(i);
                    URL_and_FileLoader.AddToSearchPath(s);
                    File parentFile = new File(configuration_file).getParentFile();
                    while (s.startsWith("../")) {
                        if (null == parentFile) {
                            break;
                        }
                        parentFile = parentFile.getParentFile();
                        s = s.substring(3);
                    }
                    URL_and_FileLoader.AddToSearchPath(parentFile.getAbsolutePath() + File.separatorChar + s);
                }
            }
            while (buffers_enum.hasMoreElements()) {
                rcs.nml.NMLBufferConfigInfo bufferConfigInfo = (NMLBufferConfigInfo) buffers_enum.nextElement();
                if (null == bufferConfigInfo
                        || null == bufferConfigInfo.buffer_line) {
                    continue;
                }
                if (bufferConfigInfo.buffer_line.indexOf("queue") > 0) {
                    continue;
                }
                if (auxBuffersHashtable != null
                        && auxBuffersHashtable.contains(bufferConfigInfo.buffer_name)) {
                    continue;
                }
                Hashtable ht = cgc.get_m_modulesHashTable();
                if (ht != null && ht.contains(bufferConfigInfo.buffer_name)) {
                    continue;
                }
                int header_start_index = bufferConfigInfo.buffer_line.indexOf("header=");
                if (header_start_index > 0) {
                    String h = bufferConfigInfo.buffer_line.substring(header_start_index + 7);
                    int sindex = h.indexOf(' ');
                    if (sindex > 0) {
                        h = h.substring(0, sindex);
                    }
                    h = h.trim();
                    cgc.AddAuxBufferModule(bufferConfigInfo.buffer_name, h, configuration_file);
                } else {
                    cgc.AddAuxBufferModule(bufferConfigInfo.buffer_name, null, configuration_file);
                }
            }
            LoadAuxChannelsHashtable();
            LoadAuxJList();
        } catch (Exception e) {
            PrintException(e);
        }
    }

    private abstract class ConnectStatusPublisher {

        abstract public void publish_cs(diag_NB_UI_ConnectToAll_Status cs);

        abstract public void setProgress_cs(int progress);
    }

    private Void SetNMLConfigFile_Background_Do(ConnectStatusPublisher csp, String filename) {
        try {
            diag_NB_UI_ConnectToAll_Status connect_status = new diag_NB_UI_ConnectToAll_Status();
            Hashtable ht = cgc.get_m_modulesHashTable();
            connect_status.total = 0;
            if (null != ht) {
                connect_status.total += ht.size();
            }
            if (null != auxBuffersHashtable) {
                connect_status.total += auxBuffersHashtable.size();
            }
            if (null != plot_tracker_hashtable) {
                connect_status.total += plot_tracker_hashtable.size();
            }
            connect_status.progress = 0;
            connect_status.tried = 0;
            connect_status.failed = 0;
            connect_status.connected = 0;
            csp.publish_cs(connect_status);
            if (!checked_sleep(10)) {
                return null;
            }
            if (null != ht) {
                for (Object obj : ht.values()) {
                    ModuleInfo mi = (ModuleInfo) obj;
                    connect_status.bufName = mi.Name;
                    csp.publish_cs(connect_status);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                    try {
                        mi.SetNMLConfigFile(filename);
                        if (mi.is_connected) {
                            connect_status.connected++;
                        } else {
                            connect_status.failed++;
                        }
                    } catch (Exception e) {
                        PrintException(e);
                    }
                    connect_status.tried++;
                    connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                    if (Thread.interrupted()) {
                        return null;
                    }
                    csp.setProgress_cs(connect_status.progress);
                    csp.publish_cs(connect_status);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                }
            }
            if (null != auxBuffersHashtable) {
                for (BufferInfo bi : auxBuffersHashtable.values()) {
                    connect_status.bufName = bi.Name;
                    try {
                        if (null == bi.read_nml && null != bi.read_msg_dict) {
                            bi.configFile = filename;
                            bi.read_nml = new NMLConnection();
                            bi.read_nml.set_buffer_name(bi.Name);
                            bi.read_nml.set_process_name("jdiag");
                            bi.read_nml.set_configuration_file(bi.configFile);
                            bi.read_nml.SetMessageDictionary(bi.read_msg_dict);
                            bi.read_nml.ReadNMLConfigurationFileNoThrow();
                            bi.read_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
                        } else if (null != bi.read_nml) {
                            bi.read_nml.connectNoThrow();
                        }
                        if (!double_buffer_nml) {
                            bi.write_nml = bi.read_nml;
                        } else if (null == bi.write_nml && null != bi.write_msg_dict) {
                            bi.configFile = filename;
                            bi.write_nml = new NMLConnection();
                            bi.write_nml.set_buffer_name(bi.Name);
                            bi.write_nml.set_process_name("jdiag");
                            bi.write_nml.set_configuration_file(bi.configFile);
                            bi.write_nml.SetMessageDictionary(bi.read_msg_dict);
                            bi.write_nml.ReadNMLConfigurationFileNoThrow();
                            bi.write_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
                        } else if (bi.write_nml != null) {
                            bi.write_nml.connectNoThrow();
                        }
                        csp.publish_cs(connect_status);
                        bi.SetNMLConfigFile(filename);
                        if (bi.read_nml.is_connected()) {
                            connect_status.connected++;
                        } else {
                            connect_status.failed++;
                        }
                        connect_status.tried++;
                        connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                        if (Thread.interrupted()) {
                            return null;
                        }
                        csp.setProgress_cs(connect_status.progress);
                        csp.publish_cs(connect_status);
                        if (!checked_sleep(10)) {
                            return null;
                        }
                    } catch (Exception e) {
                        PrintException(e);
                    }
                }
            }
            if (null != plot_tracker_hashtable) {
                for (PlotTracker pt : plot_tracker_hashtable.values()) {
                    connect_status.bufName = "Plot connect for " + pt.plot_data.name;
                    csp.publish_cs(connect_status);
                    if (null != pt.nml_for_get_single_var_log) {
                        pt.nml_for_get_single_var_log.connectNoThrow();
                        if (pt.nml_for_get_single_var_log.is_connected()) {
                            pt.nml_single_var_log_number =
                                    pt.nml_for_get_single_var_log.setupSingleVarLog(pt.vname, 500, 0.01, (int) pt.msg_type);
                            if (pt.nml_single_var_log_number > 0) {
                                connect_status.connected++;
                            } else {
                                connect_status.failed++;
                                pt.nml_for_get_single_var_log.disconnect();
                            }
                        } else {
                            connect_status.failed++;
                        }
                    } else {
                        connect_status.failed++;
                    }
                    if (Thread.interrupted()) {
                        return null;
                    }
                    connect_status.tried++;
                    connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                    csp.setProgress_cs(connect_status.progress);
                    csp.publish_cs(connect_status);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }



        return null;
    }

    synchronized private void SetNMLConfigFile(final String filename) {
        try {
            ModuleInfo.DefaultNMLConfigurationFile = filename;
            String cfgstring = filename;
            if (!filename.startsWith("nmlcfgsvr:")) {
                cfgstring = (new File(filename)).getAbsolutePath();
            }

            if (null != dp) {
                final String fullPath = cfgstring;
                if (!dp.get_recentNmlConfigFileVector().contains(fullPath)) {
                    dp.get_recentNmlConfigFileVector().insertElementAt(fullPath, 0);
                    JMenuItem jmi = new JMenuItem(fullPath);
                    jmi.addActionListener(new ActionListener() {

                        @Override
                        public void actionPerformed(ActionEvent e) {
                            SetNMLConfigFile(fullPath);
                        }
                    });
                    this.jMenuConnectionsRecentNmlFiles.add(jmi);
                }

            }
            nml_hashtable = null;
            ((DefaultTableModel) jTableNML.getModel()).setRowCount(0);
            LoadAuxBuffersFromNmlConfigFile(cfgstring);

            this.jDialogConnecting.pack();
            this.jDialogConnecting.setVisible(true);
            SwingWorker backgroundSwingWorkerToQueue = new SwingWorker<Void, diag_NB_UI_ConnectToAll_Status>() {

                @Override
                public Void doInBackground() {
                    ConnectStatusPublisher csp = new ConnectStatusPublisher() {

                        @Override
                        public void publish_cs(diag_NB_UI_ConnectToAll_Status cs) {
                            if (cancel_connect_to_all_called) {
                                return;
                            }
                            publish(cs);
                        }

                        @Override
                        public void setProgress_cs(int progress) {
                            setProgress(progress);
                        }
                    };
                    return SetNMLConfigFile_Background_Do(csp, filename);
                }

                @Override
                public void process(List<diag_NB_UI_ConnectToAll_Status> sl) {
                    diag_NB_UI_ConnectToAll_Status connect_status = sl.get(sl.size() - 1);
                    UpdateConnectToAllStatus(connect_status);
                }

                @Override
                protected void done() {
                    if (!javax_swing_timer.isRunning() && getExtendedState() != JFrame.ICONIFIED) {
                        javax_swing_timer.start();
                    }
                    dp.set_connected(true);
                    LoadNmlTable();
                    diag_common.PrintMemUsage("Finished SetNMLConfigFile()");
                    jDialogConnecting.setVisible(false);
                    NextBackgroundSwingWorker();
                }
            };
            QueueSwingWorker(backgroundSwingWorkerToQueue);
        } catch (Exception ex) {
            diag_NB_UI.PrintException(ex);
        }

    }

    private void jButtonPlotAuxArrayActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotAuxArrayActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotAuxArrayActionPerformed
        StartPlottingArray(watchJPanelAuxView.getStructureTypeInfo(), curAuxBuffer, curModule, last_aux_id_long, aux_selected_var_number, aux_selected_var_name, false, true);
    }//GEN-LAST:event_jButtonPlotAuxArrayActionPerformed

    private void jButtonPlotAuxVarActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotAuxVarActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotAuxVarActionPerformed
        StartPlottingVariable(curAuxBuffer, curModule, last_aux_id_long, aux_selected_var_number, aux_selected_var_name, false, true);
        jButtonPlotAuxVar.setEnabled(false);
    }//GEN-LAST:event_jButtonPlotAuxVarActionPerformed

    private void jButtonSendAuxMsgActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonSendAuxMsgActionPerformed
    {//GEN-HEADEREND:event_jButtonSendAuxMsgActionPerformed
        SendAuxMsg();
    }//GEN-LAST:event_jButtonSendAuxMsgActionPerformed

    private void jListAuxMessagesAvailableValueChanged(javax.swing.event.ListSelectionEvent evt)//GEN-FIRST:event_jListAuxMessagesAvailableValueChanged
    {//GEN-HEADEREND:event_jListAuxMessagesAvailableValueChanged
        LoadAuxMsgToSend();
    }//GEN-LAST:event_jListAuxMessagesAvailableValueChanged

    synchronized private Void SelectAuxChannel_Background_Do(ConnectStatusPublisher csp) {
        try {
            //Thread.dumpStack();
            diag_NB_UI_ConnectToAll_Status connect_status = new diag_NB_UI_ConnectToAll_Status();
            connect_status.total = 1;
            connect_status.progress = 0;
            connect_status.tried = 0;
            connect_status.failed = 0;
            connect_status.connected = 0;
            connect_status.bufName = curAuxBuffer.Name;
            csp.publish_cs(connect_status);
            if (!checked_sleep(10)) {
                return null;
            }
            ConnectCurAuxBuffer();
            connect_status.tried = 1;
            if (curAuxBuffer.isConnected()) {
                connect_status.progress = 100;
            } else {
                connect_status.failed = 1;
            }
            csp.publish_cs(connect_status);
            if (!checked_sleep(10)) {
                return null;
            }
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }
    private boolean no_select_aux_channel_load_nml_table = false;

    synchronized private void SelectAuxChannel_Background_Done() {
        HideConnectingDialog();
        jDialogConnecting.setVisible(false);
        if (!javax_swing_timer.isRunning() && getExtendedState() != JFrame.ICONIFIED) {
            javax_swing_timer.start();
        }

        dp.set_connected(true);
        if (!no_select_aux_channel_load_nml_table) {
            LoadNmlTable();
        }
        if (null != curAuxBuffer) {
            Vector<String> v = new Vector<String>();
            for (Object o : curAuxBuffer.getMsgsAvailable()) {
                if (!v.contains(o.toString())) {
                    v.add(o.toString());
                }

            }
            Collections.sort(v);
            jListAuxMessagesAvailable.setListData(v);
            jCheckBoxAuxChannelConnected.setSelected(curAuxBuffer.isConnected());
        } else {
            jCheckBoxAuxChannelConnected.setSelected(false);
        }

        jLabelAuxMsgToSendType.setText("");
        jLabelAuxRecvd.setText("0");
        jLabelAuxType.setText("");
        last_aux_id_long = -1;
        if (null == curAuxBuffer) {
            jTextPaneAuxBottom.setText("curAuxBuffer == null");
            //throw new Exception("curAuxBuffer == null, auxName="+auxName);
        } else if (null == curAuxBuffer.read_nml) {
            jTextPaneAuxBottom.setText("curAuxBuffer.read_nml == null");
            //throw new Exception("curAuxBuffer.read_nml == null, auxName="+auxName);
        } else {
            jTextPaneAuxBottom.setText(curAuxBuffer.read_nml.getBufferLine());
        }
    }

    private void SelectAuxChannel(String auxName) throws Exception {

        if (null != auxMsgToSendIdString && null != curAuxBuffer) {
            auxToSendString = auxMsgToSendIdString + ",0" + watchJPanelAuxMsgToSend.getDataString();
            curAuxBuffer.SetPreviousMessage(auxToSendString);
        }
        auxMsgToSendIdString = null;
        dp.setSelectedAuxChannel(auxName);
        watchJPanelAuxView.Clear();
        watchJPanelAuxMsgToSend.Clear();
        if (null != curAuxBuffer && this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected() && curAuxBuffer.isConnected()) {
            if (curAuxBuffer.Name.compareTo(auxName) != 0) {
                DisconnectCurAuxBuffer();
                LoadNmlTable();
            }

        }
        curAuxBuffer = auxBuffersHashtable.get(auxName);
        if (null != curAuxBuffer
                && this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected() && !curAuxBuffer.isConnected()
                && this.jTabbedPane1.getSelectedComponent() == this.jPanelAuxChannels) {
            //ShowConnectingDialog();
            SwingWorker backgroundSwingWorkerToQueue =
                    new SwingWorker<Void, diag_NB_UI_ConnectToAll_Status>() {

                        @Override
                        public Void doInBackground() {
                            ConnectStatusPublisher csp = new ConnectStatusPublisher() {

                                @Override
                                public void publish_cs(diag_NB_UI_ConnectToAll_Status cs) {
                                    publish(cs);
                                }

                                @Override
                                public void setProgress_cs(int progress) {
                                    setProgress(progress);
                                }
                            };
                            return SelectAuxChannel_Background_Do(csp);
                        }

                        @Override
                        public void process(List<diag_NB_UI_ConnectToAll_Status> sl) {
                            diag_NB_UI_ConnectToAll_Status connect_status = sl.get(sl.size() - 1);
                            UpdateConnectToAllStatus(connect_status);
                        }

                        @Override
                        protected void done() {
                            SelectAuxChannel_Background_Done();
                            if (curAuxBuffer.last_selected_msg_index >= 0) {
                                jListAuxMessagesAvailable.setSelectedIndex(curAuxBuffer.last_selected_msg_index);
                            }
                            NextBackgroundSwingWorker();
                        }
                    };
            QueueSwingWorker(backgroundSwingWorkerToQueue);
        } else {
            SelectAuxChannel_Background_Done();
            if (curAuxBuffer.last_selected_msg_index >= 0) {
                this.jListAuxMessagesAvailable.setSelectedIndex(curAuxBuffer.last_selected_msg_index);
            }
        }

    }

    private void jListAuxChannelsValueChanged(javax.swing.event.ListSelectionEvent evt)//GEN-FIRST:event_jListAuxChannelsValueChanged
    {//GEN-HEADEREND:event_jListAuxChannelsValueChanged
        try {
            SelectAuxChannel(jListAuxChannels.getSelectedValue().toString());
            this.jRadioButtonAuxNormal.setSelected(true);
        } catch (Exception e) {
            PrintException(e);
        }
    }//GEN-LAST:event_jListAuxChannelsValueChanged

    private void jTabbedPane1StateChanged(javax.swing.event.ChangeEvent evt)//GEN-FIRST:event_jTabbedPane1StateChanged
    {//GEN-HEADEREND:event_jTabbedPane1StateChanged
        no_select_aux_channel_load_nml_table = false;
        if (this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()) {
            int current_tab = this.jTabbedPane1.getSelectedIndex();
            switch (current_tab) {
                case 0:
                    ConnectToAllModules();
                    DisconnectFromAllAuxChannel();
                    break;

                case 1:
                    DisconnectFromAllModules();
                    DisconnectFromAllAuxChannel();
                    if (null != curModule) {
                        curModule.connect();
                    }

                    break;

                case 2:
                    DisconnectFromAllModules();
                    DisconnectFromAllAuxChannel();
                    try {
                        ConnectCurAuxBuffer();
                    } catch (Exception e) {
                        PrintException(e);
                    }

                    break;

                case 3:
                    ConnectToAll();
                    break;

                default:
                    break;
            }

            LoadNmlTable();
            if (current_tab == 3) {
                this.jTableNML.getRowSorter().allRowsChanged();
            }
        }
    }//GEN-LAST:event_jTabbedPane1StateChanged

    private void SendDetailsStatus() {
        try {
            String data_to_send = watchJPanelCmdToSend.getDataString();

            statusToSendString =
                    statusToSendIdString + ",0" + data_to_send;
            status_to_send_set_count++;
//curModule.writeStat(statusString);
        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void SendDetailsCommand() {
        try {
            cmdToSendString = null;
            cmdToSendModule =
                    curModule;
            String data_to_send = watchJPanelCmdToSend.getDataString();
            curModule.serial_number++;
            int cindex = data_to_send.indexOf(',', 1);
            int data_set_serial_number = -1;
            if (cindex > 0) {
                data_set_serial_number = Double.valueOf(data_to_send.substring(1, cindex)).intValue();
            } else {
                data_set_serial_number = Double.valueOf(data_to_send.substring(1)).intValue();
            }

            if (data_set_serial_number > curModule.serial_number) {
                curModule.serial_number = data_set_serial_number;
            } else {
                if (cindex > 0) {
                    data_to_send = "," + curModule.serial_number + data_to_send.substring(cindex);
                } else {
                    data_to_send = "," + curModule.serial_number;
                }

                watchJPanelCmdToSend.SetDataInfo(new StringTokenizer(data_to_send, ","));
            }

            cmdToSendIdString = Long.toString(watchJPanelCmdToSend.getId());
            cmdToSendString = cmdToSendIdString + ",0" + data_to_send;
//	    curModule.writeCmd(cmdString);
            curModule.previous_commands.put(Long.valueOf(cmdToSendIdString), cmdToSendString);
            cmd_to_send_set_count++;

            delayed_job_to_do =
                    true;
        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void SendAuxMsg() {
        try {
            auxToSendString = auxMsgToSendIdString + ",0" + watchJPanelAuxMsgToSend.getDataString();
            //curAuxBuffer.write_nml.writeDataString(cmdString);
            aux_to_send_set_count++;

        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void jButtonSendDetailsCommandActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonSendDetailsCommandActionPerformed
    {//GEN-HEADEREND:event_jButtonSendDetailsCommandActionPerformed
        if (jCheckBoxModifyStatus.isSelected()) {
            SendDetailsStatus();
        } else {
            SendDetailsCommand();
        }
    }//GEN-LAST:event_jButtonSendDetailsCommandActionPerformed

    private void jButtonPlotStatusVarActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonPlotStatusVarActionPerformed
    {//GEN-HEADEREND:event_jButtonPlotStatusVarActionPerformed
        StartPlottingVariable(curAuxBuffer, curModule, last_status_id_long, status_selected_var_number, status_selected_var_name, false, false);
        jButtonPlotStatusVar.setEnabled(false);
    }//GEN-LAST:event_jButtonPlotStatusVarActionPerformed

    @SuppressWarnings("unchecked")
    private void StartPlottingVariable(BufferInfo bi, ModuleInfo mi, long msg_type, int var_num, String varname, boolean is_cmd, boolean is_aux) {
        if (var_num < 0 || varname == null) {
            return;
        }

        var_num += 2;

        PlotTracker pt = new PlotTracker();
        pt.varName = varname;
        pt.var_number = var_num;
        int sp_index = varname.lastIndexOf(' ');
        String vname = varname;
        if (sp_index > 0) {
            vname = varname.substring(sp_index + 1);
        }

        pt.using_nml_single_var_log = true;
        pt.module = mi;
        pt.msg_type = msg_type;
        pt.is_cmd_value = is_cmd;
        pt.is_aux_channel = is_aux;
        if (is_aux) {
            pt.nml_for_get_single_var_log = bi.createExtraChannel();
            pt.name = bi.Name + "." + varname;
        } else if (is_cmd) {
            pt.nml_for_get_single_var_log = mi.createExtraCommandChannel();
            pt.name = mi.Name + ".cmd." + vname;
            mi.cmd_plotting_variables.put(Integer.valueOf(var_num), pt);
            if (var_num > mi.max_cmd_var_number) {
                mi.max_cmd_var_number = var_num;
            }

            mi.cmd_has_been_plotted = false;
        } else {
            pt.nml_for_get_single_var_log = mi.createExtraStatusChannel();
            pt.name = mi.Name + ".stat." + vname;
            mi.stat_plotting_variables.put(Integer.valueOf(var_num), pt);
            if (var_num > mi.max_stat_var_number) {
                mi.max_stat_var_number = var_num;
            }

            mi.stat_has_been_plotted = false;
        }

        pt.vname = vname;
        pt.nml_single_var_log_number =
                pt.nml_for_get_single_var_log.setupSingleVarLog(pt.vname, 500, 0.01, (int) pt.msg_type);
        if (pt.nml_single_var_log_number > 0) {
            pt.using_nml_single_var_log = true;
        }

        pt.plot_data = new PlotData();
        this.plotter_NB_UI1.AddPlot(pt.plot_data, pt.name);
        if (null == plot_tracker_hashtable) {
            plot_tracker_hashtable = new Hashtable<String, PlotTracker>();
        }

        plot_tracker_hashtable.put(pt.name, pt);
    }

    private void jButtonCancelActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jButtonCancelActionPerformed
    {//GEN-HEADEREND:event_jButtonCancelActionPerformed
        CancelConnectToAll();
    }//GEN-LAST:event_jButtonCancelActionPerformed

    private void UpdateConnectToAllStatus(diag_NB_UI_ConnectToAll_Status connect_to_all_status) {
        try {
            if (cancel_connect_to_all_called) {
                return;
            }
            jLabelCurBuffer.setText(connect_to_all_status.bufName);
            jLabelNumTried.setText(Integer.toString(connect_to_all_status.tried));
            jLabelNumBuffers.setText(Integer.toString(connect_to_all_status.total));
            jLabelNumFailed.setText(Integer.toString(connect_to_all_status.failed));
            jLabelNumConnected.setText(Integer.toString(connect_to_all_status.connected));
            jProgressBarConnectToAll.setValue(connect_to_all_status.progress);
            if (connect_to_all_status.total > 1) {
                this.jProgressBarConnectToAllConnected.setValue((100 * connect_to_all_status.connected) / connect_to_all_status.total);
                this.jProgressBarConnectToAllFailed.setValue((100 * connect_to_all_status.failed) / connect_to_all_status.total);
            }
        } catch (Exception ex) {
            PrintException(ex);
        }

    }

    private void DisconnectFromAllModules() {
        try {
            for (Object obj : cgc.get_m_modulesHashTable().values()) {
                ModuleInfo mi = (ModuleInfo) obj;
                mi.disconnect();
            }

        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void DisconnectFromAllAuxChannel() {
        try {
            if (null != auxBuffersHashtable) {
                for (BufferInfo bi : auxBuffersHashtable.values()) {
                    if (null != bi.read_nml) {
                        bi.read_nml.disconnect();
                    }

                    if (null != bi.write_nml) {
                        bi.write_nml.disconnect();
                    }

                }
            }
        } catch (Exception e) {
            PrintException(e);
        }

    }

    synchronized private Void ConnectToAllModules_Background_Do(ConnectStatusPublisher csp) {
        try {
            skip_once_flag_set = false;
            ////Thread.dumpStack();
            diag_NB_UI_ConnectToAll_Status connect_status = new diag_NB_UI_ConnectToAll_Status();
            Hashtable ht = null;
            connect_status.total = 0;
            if (null != cgc) {
                ht = cgc.get_m_modulesHashTable();
                if (null != ht) {
                    connect_status.total = ht.size();
                }

            }
            connect_status.progress = 0;
            connect_status.tried = 0;
            connect_status.failed = 0;
            connect_status.connected = 0;
            csp.publish_cs(connect_status);
            if (!checked_sleep(10)) {
                return null;
            }
            if (null != ht) {
                Collection ht_objects = ht.values();
                for (Object obj : ht_objects) {
                    ModuleInfo mi = (ModuleInfo) obj;
                    connect_status.bufName = mi.Name;
                    csp.publish_cs(connect_status);
                    if (!checked_sleep(10)) {
                        return null;
                    }
                    try {
                        if (mi.connect() >= 0) {
                            connect_status.connected++;
                        } else {
                            connect_status.failed++;
                        }

                    } catch (Exception e) {
                        PrintException(e);
                    }

                    connect_status.tried++;
                    connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                    csp.setProgress_cs(connect_status.progress);
                    csp.publish_cs(connect_status);
                    try {
                        if (!checked_sleep(10)) {
                            return null;
                        }
                    } catch (Exception e) {
                    }
                    if (Thread.interrupted() && !skip_once_flag_set) {
                        break;
                    }
                    skip_once_flag_set = false;
                }

            }
        } catch (Exception e) {
            PrintException(e);
        }
        return null;
    }

    private void ConnectToAllModules_Background_Done() {
        jDialogConnecting.setVisible(false);
        if (!javax_swing_timer.isRunning() && getExtendedState() != JFrame.ICONIFIED) {
            javax_swing_timer.start();
        }

        dp.set_connected(true);
        LoadNmlTable();
        if (!dp_plot_list_loaded && this.Automatically_Keep_and_Use_PlotSets) {
            try {
                final plotSetPreserve psp = dp.getPlotSet();
                if (null != psp) {
                    java.awt.EventQueue.invokeLater(new Runnable() {

                        @Override
                        public void run() {
                            try {
                                LoadPlotSet(psp);
                            } catch (Exception e) {
                                PrintException(e);
                                dp.setPlotSet(null);
                            }
                        }
                    });
                }
            } catch (Exception e) {
                PrintException(e);
                dp.setPlotSet(null);
            } finally {
                dp_plot_list_loaded = true;
            }

        }
        diag_common.PrintMemUsage("Finished ConnectToAll:");
        if (!running_file_written) {
            if (backgroundQueueEmpty()) {
                write_running_file("diag.running");
            }
        }
        if (null == curModule) {
            jCheckBoxDetailsModuleConnected.setSelected(false);
        } else {
            jCheckBoxDetailsModuleConnected.setSelected(curModule.is_connected);
        }

        if (null == curAuxBuffer) {
            jCheckBoxAuxChannelConnected.setSelected(false);
        } else {
            jCheckBoxAuxChannelConnected.setSelected(curAuxBuffer.isConnected());
        }
    }

    private void ConnectToAllModules() {
        try {
            final long stime = System.currentTimeMillis();
            if (javax_swing_timer.isRunning()) {
                javax_swing_timer.stop();
            }
////Thread.dumpStack();
            this.jDialogConnecting.pack();
            this.jDialogConnecting.setVisible(true);
            diag_common.PrintMemUsage("Starting ConnectToAllModules:");
            SwingWorker backgroundSwingWorkerToQueue =
                    new SwingWorker<Void, diag_NB_UI_ConnectToAll_Status>() {

                        @Override
                        public Void doInBackground() {
                            ConnectStatusPublisher csp = new ConnectStatusPublisher() {

                                @Override
                                public void publish_cs(diag_NB_UI_ConnectToAll_Status cs) {
                                    if (cancel_connect_to_all_called) {
                                        return;
                                    }
                                    publish(cs);
                                }

                                @Override
                                public void setProgress_cs(int progress) {
                                    setProgress(progress);
                                }
                            };
                            return ConnectToAllModules_Background_Do(csp);
                        }

                        @Override
                        public void process(List<diag_NB_UI_ConnectToAll_Status> sl) {
                            diag_NB_UI_ConnectToAll_Status connect_status = sl.get(sl.size() - 1);
                            UpdateConnectToAllStatus(connect_status);
                        }

                        @Override
                        protected void done() {
                            ConnectToAllModules_Background_Done();
                            NextBackgroundSwingWorker();
                        }
                    };
            QueueSwingWorker(backgroundSwingWorkerToQueue);
        } catch (Exception ex) {
            PrintException(ex);
        }

    }

    private void ShowConnectingDialog() {
        try {
            java.awt.EventQueue.invokeLater(new Runnable() {

                @Override
                public void run() {
                    try {
                        jDialogConnecting.pack();
                        jDialogConnecting.setVisible(true);
                    } catch (Exception ex) {
                        ex.printStackTrace();

                    }
                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();

        }

    }

    private void HideConnectingDialog() {
        try {
            java.awt.EventQueue.invokeLater(new Runnable() {

                @Override
                public void run() {
                    try {
                        jDialogConnecting.setVisible(false);
                    } catch (Exception ex) {
                        ex.printStackTrace();

                    }

                }
            });
        } catch (Exception ex) {
            ex.printStackTrace();

        }

    }
    private Thread connectToAllThread = null;
    private boolean skip_once_flag_set = false;

    private boolean checked_sleep(int millis) {
        if (cancel_connect_to_all_called) {
            if(diag_common.get_debug_on()) {
               Thread.dumpStack();
            }
            skip_once_flag_set = false;
            return false;
        }
        try {
            Thread.sleep(millis);
        } catch (Exception e) {
        }
        if (Thread.interrupted() && !skip_once_flag_set && !this.cancel_connect_to_all_called) {
            return false;
        }
        skip_once_flag_set = false;
        return true;
    }

    private Void ConnectToAll_Background_Do(ConnectStatusPublisher csp) {
        try {
            connectToAllThread = Thread.currentThread();
            diag_NB_UI_ConnectToAll_Status connect_status = new diag_NB_UI_ConnectToAll_Status();
            Hashtable ht = null;
            connect_status.total = 0;
            connect_status.bufName = " initializing . . . ";
            boolean limit_size = this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                    && this.jTabbedPane1.getSelectedComponent().equals(this.jPanelNML);
            if (!jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                    || jTabbedPane1.getSelectedIndex() == 3) {
                if (null != cgc) {
                    ht = cgc.get_m_modulesHashTable();
                    if (null != ht) {
                        connect_status.total = ht.size();
                    }

                }
                if (null != auxBuffersHashtable) {
                    connect_status.total += auxBuffersHashtable.size();
                }

            }
            if (null != plot_tracker_hashtable) {
                connect_status.total += plot_tracker_hashtable.size();
            }

            connect_status.progress = 0;
            connect_status.tried = 0;
            connect_status.failed = 0;
            connect_status.connected = 0;
            csp.publish_cs(connect_status);
            if (!checked_sleep(10)) {
                return null;
            }
            if (null != extraTabVector) {
                connect_status.total += extraTabVector.size();
                for (ExtraTabInfo eti : extraTabVector) {
                    connect_status.bufName = eti.name;
                    csp.publish_cs(connect_status);
                    if (null != eti.connect_method) {
                        eti.connect_method.invoke(eti.jp);
                        connect_status.connected++;
                        connect_status.tried++;
                        connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                        csp.setProgress_cs(connect_status.progress);
                    }

                    csp.publish_cs(connect_status);
                }

            }
            if (!jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected()
                    || jTabbedPane1.getSelectedIndex() == 3) {
                if (null != ht) {
                    Collection ht_objects = ht.values();
                    for (Object obj : ht_objects) {
                        if (!checked_sleep(10)) {
                            return null;
                        }
                        ModuleInfo mi = (ModuleInfo) obj;
                        if (mi.no_cmd && mi.no_stat) {
                            connect_status.total--;
                            continue;
                        }
                        connect_status.bufName = mi.Name;
                        csp.publish_cs(connect_status);
                        if (!checked_sleep(10)) {
                            return null;
                        }
                        try {
                            if (mi.connect() >= 0) {
                                connect_status.connected++;
                            } else {
                                connect_status.failed++;
                            }

                        } catch (Exception e) {
                            PrintException(e);
                        }

                        connect_status.tried++;
                        if (connect_status.total > 0) {
                            connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                            csp.setProgress_cs(connect_status.progress);
                        }
                        csp.publish_cs(connect_status);
                    }
                }
                if (null != auxBuffersHashtable) {
                    for (BufferInfo bi : auxBuffersHashtable.values()) {
                        if (!checked_sleep(10)) {
                            return null;
                        }
                        if (bi.isConnected()) {
                            connect_status.total--;
                            continue;
                        }
                        connect_status.bufName = bi.Name;
                        csp.publish_cs(connect_status);
                        try {
                            int orig_allocation_size_max = -1;
                            if (null != bi && null != bi.read_nml) {
                                orig_allocation_size_max = bi.read_nml.getAllocation_size_max();
                            }
                            if (null == bi.read_nml
                                    && null != bi.configFile
                                    && null != bi.read_msg_dict) {
                                bi.read_nml = new NMLConnection();
                                bi.read_nml.set_buffer_name(bi.Name);
                                bi.read_nml.set_process_name("jdiag");
                                bi.read_nml.set_configuration_file(bi.configFile);
                                bi.read_nml.SetMessageDictionary(bi.read_msg_dict);
                                if (limit_size) {
                                    bi.read_nml.setAllocation_size_max(256);
                                }
                                bi.read_nml.ReadNMLConfigurationFileNoThrow();
                                bi.read_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
                                if (limit_size) {
                                    bi.read_nml.setAllocation_size_max(orig_allocation_size_max);
                                }
                            }
                            if (bi.read_nml != null && !bi.read_nml.is_connected()) {
                                if (limit_size) {
                                    bi.read_nml.setAllocation_size_max(256);
                                }
                                bi.read_nml.connect();
                                if (limit_size) {
                                    bi.read_nml.setAllocation_size_max(orig_allocation_size_max);
                                }
                            }

                            if (!double_buffer_nml) {
                                bi.write_nml = bi.read_nml;
                            } else if (null == bi.write_nml && null != bi.configFile && null != bi.write_msg_dict) {
                                bi.write_nml = new NMLConnection();
                                bi.write_nml.set_buffer_name(bi.Name);
                                bi.write_nml.set_process_name("jdiag");
                                bi.write_nml.set_configuration_file(bi.configFile);
                                bi.write_nml.SetMessageDictionary(bi.read_msg_dict);
                                if (limit_size) {
                                    bi.write_nml.setAllocation_size_max(256);
                                }
                                bi.write_nml.ReadNMLConfigurationFileNoThrow();
                                bi.write_nml.SetFormatConvertErrCallback(BufferInfo.get_nml_format_err_callback());
                                if (limit_size) {
                                    bi.write_nml.setAllocation_size_max(orig_allocation_size_max);
                                }
                            }
                            if (!double_buffer_nml && bi.write_nml != null && !bi.write_nml.is_connected()) {
                                if (limit_size) {
                                    bi.write_nml.setAllocation_size_max(256);
                                }
                                bi.write_nml.connect();
                                if (limit_size) {
                                    bi.write_nml.setAllocation_size_max(orig_allocation_size_max);
                                }
                            }

                            if (bi.isConnected()) {
                                connect_status.connected++;
                            } else {
                                connect_status.failed++;
                            }
                        } catch (Exception e) {
                            if (!skip_once_flag_set) {
                                PrintException(e);
                            }
                            connect_status.failed++;
                        }
                        skip_once_flag_set = false;
                        connect_status.tried++;
                        if (connect_status.total > 0) {
                            connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                            csp.setProgress_cs(connect_status.progress);
                        }
                        csp.publish_cs(connect_status);
                    }
                }
            }
            if (null != plot_tracker_hashtable) {
                for (PlotTracker pt : plot_tracker_hashtable.values()) {
                    connect_status.bufName = "Plot connect for " + pt.plot_data.name;
                    csp.publish_cs(connect_status);
                    if (null != pt.nml_for_get_single_var_log) {
                        pt.nml_for_get_single_var_log.connectNoThrow();
                        if (pt.nml_for_get_single_var_log.is_connected()) {
                            pt.nml_single_var_log_number =
                                    pt.nml_for_get_single_var_log.setupSingleVarLog(pt.vname, 500, 0.01, (int) pt.msg_type);
                            if (pt.nml_single_var_log_number > 0) {
                                connect_status.connected++;
                            } else {
                                connect_status.failed++;
                            }

                        } else {
                            connect_status.failed++;
                        }

                    } else {
                        connect_status.failed++;
                    }

                    connect_status.tried++;
                    if (connect_status.total > 0 && connect_status.total <= connect_status.tried) {
                        connect_status.progress = (100 * connect_status.tried) / connect_status.total;
                        csp.setProgress_cs(connect_status.progress);
                        csp.publish_cs(connect_status);
                    }

                    if (!checked_sleep(10)) {
                        return null;
                    }
                }

            }
        } catch (Exception e) {
            PrintException(e);
        } finally {
            this.connectToAllThread = null;
        }
        return null;
    }

    private void ConnectToAll_Background_Done() {
        if (!javax_swing_timer.isRunning() && getExtendedState() != JFrame.ICONIFIED) {
            javax_swing_timer.start();
        }

        dp.set_connected(true);

        LoadNmlTable();
        if (num_connected > 0) {
            this.jCheckBoxMenuItemConnected.setSelected(true);
        }
        if (!dp_plot_list_loaded) {
            try {
                final plotSetPreserve psp = dp.getPlotSet();
                if (null != psp) {
                    java.awt.EventQueue.invokeLater(new Runnable() {

                        @Override
                        public void run() {
                            try {
                                LoadPlotSet(psp);
                            } catch (Exception e) {
                                PrintException(e);
                                dp.setPlotSet(null);
                            }
                        }
                    });
                }
            } catch (Exception e) {
                PrintException(e);
                dp.setPlotSet(null);
            } finally {
                dp_plot_list_loaded = true;
            }

        }
        diag_common.PrintMemUsage("Finished ConnectToAll:");
        if (!running_file_written) {
            if (backgroundQueueEmpty()) {
                write_running_file("diag.running");
            }

        }
        if (null == curModule) {
            jCheckBoxDetailsModuleConnected.setSelected(false);
        } else {
            jCheckBoxDetailsModuleConnected.setSelected(curModule.is_connected);
        }

        if (null == curAuxBuffer) {
            jCheckBoxAuxChannelConnected.setSelected(false);
        } else {
            jCheckBoxAuxChannelConnected.setSelected(curAuxBuffer.isConnected());
        }

        HideConnectingDialog();
    }

    synchronized private void ConnectToAll() {
        try {
            cancel_connect_to_all_called = false;
            final long stime = System.currentTimeMillis();
            if (javax_swing_timer.isRunning()) {
                javax_swing_timer.stop();
            }
            this.jCheckBoxMenuItemConnected.setSelected(false);
            ShowConnectingDialog();
            diagapplet.utils.DiagError.println("Starting ConnectToAll ...\n");
            diag_common.PrintMemUsage("Starting ConnectToAll:");
            LoadNmlTable();
            SwingWorker backgroundSwingWorkerToQueue =
                    new SwingWorker<Void, diag_NB_UI_ConnectToAll_Status>() {

                        @Override
                        public Void doInBackground() {
                            ConnectStatusPublisher csp = new ConnectStatusPublisher() {

                                private int last_failed = 0;

                                @Override
                                public void publish_cs(diag_NB_UI_ConnectToAll_Status cs) {
                                    if (cancel_connect_to_all_called) {
                                        diagapplet.utils.DiagError.println("cancel_connect_to_all called.");
                                        diagapplet.utils.DiagError.println("\tConnectToAll_STatus:" + cs);
                                        return;
                                    }
                                    if (cs.failed > last_failed || cs.tried >= cs.total - 1 || cs.connected < 1) {
                                        diagapplet.utils.DiagError.println("\tConnectToAll_STatus:" + cs);
                                        last_failed = cs.failed;
                                    }
                                    publish(cs);
                                }

                                @Override
                                public void setProgress_cs(int progress) {
                                    setProgress(progress);
                                }
                            };
                            return ConnectToAll_Background_Do(csp);
                        }

                        @Override
                        public void process(List<diag_NB_UI_ConnectToAll_Status> sl) {
                            diag_NB_UI_ConnectToAll_Status connect_status = sl.get(sl.size() - 1);
                            UpdateConnectToAllStatus(connect_status);
                        }

                        @Override
                        protected void done() {
                            ConnectToAll_Background_Done();
                            diagapplet.utils.DiagError.println("Finished ConnectToAll.\n");
                            NextBackgroundSwingWorker();
                        }
                    };
            QueueSwingWorker(backgroundSwingWorkerToQueue);
            //this.PrintToStreamAndErrLog(System.err, "backgroundSwingWorker.getState()="+backgroundSwingWorker.getState());
        } catch (Exception ex) {
            PrintException(ex);
        } finally {
            cancel_connect_to_all_called = false;
        }
    }

    private void CancelBackgroundWorkers() {
        if (null != this.ExecutingBackgroundSwingWorker
                && !this.ExecutingBackgroundSwingWorker.isCancelled()
                && !this.ExecutingBackgroundSwingWorker.isDone()) {
            diagapplet.utils.DiagError.println("Cancelling backgroundSwingWorker");
            this.ExecutingBackgroundSwingWorker.cancel(true);
        }
        if (null != this.backgroundSwingWorkerQueue) {
            //Thread.dumpStack();
            diagapplet.utils.DiagError.println("Setting backgroundSwingWorkerQueue of size " + this.backgroundSwingWorkerQueue.size() + " to null;");
            this.backgroundSwingWorkerQueue.clear();
            this.backgroundSwingWorkerQueue = null;
        }
        this.ExecutingBackgroundSwingWorker = null;
        this.delayed_job_to_do = false;
    }

    private void ConnectToAllSkipOne() {
        try {
            if (null != this.connectToAllThread) {
                skip_once_flag_set = true;
                this.connectToAllThread.interrupt();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    static private boolean cancel_connect_to_all_called = false;

    synchronized private void CancelConnectToAll() {
        try {
            jDialogConnecting.setVisible(false);
            diagapplet.utils.DiagError.println("Cancelling ConnectToAll()");
            cancel_connect_to_all_called = true;
            skip_once_flag_set = false;
            CancelBackgroundWorkers();
            skip_once_flag_set = false;
            if (javax_swing_timer.isRunning()) {
                javax_swing_timer.stop();
            }
            if (null != dp) {
                dp.set_connected(false);
            }
            skip_once_flag_set = false;
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            diagapplet.utils.DiagError.println("Finished CancelConnectToAll()");
        }
    }

    synchronized private void DisconnectFromAll() {
        try {
            CancelConnectToAll();
            this.jCheckBoxMenuItemConnected.setSelected(false);
            DefaultTableModel nml_table_model = (DefaultTableModel) jTableNML.getModel();
            for (int i = 0; i
                    < jTableNML.getRowCount(); i++) {
                nml_table_model.setValueAt(Boolean.valueOf(false), i, 0);
            }

            if (null != cgc) {
                Hashtable ht = cgc.get_m_modulesHashTable();
                if (null != ht) {
                    Collection ht_objects = ht.values();
                    for (Object obj : ht_objects) {
                        ModuleInfo mi = (ModuleInfo) obj;
                        mi.disconnect();
                    }

                }
            }
            if (null != auxBuffersHashtable) {
                for (BufferInfo bi : auxBuffersHashtable.values()) {
                    if (null != bi.read_nml) {
                        bi.read_nml.disconnect();
                    }

                    if (null != bi.write_nml) {
                        bi.write_nml.disconnect();
                    }

                }
            }
            if (null != plot_tracker_hashtable) {
                for (PlotTracker pt : plot_tracker_hashtable.values()) {
                    if (null != pt.nml_for_get_single_var_log) {
                        pt.nml_for_get_single_var_log.closeSingleVarLog(pt.nml_single_var_log_number);
                        pt.nml_for_get_single_var_log.disconnect();
                    }

                }
            }
            if (null != extraTabVector) {
                for (ExtraTabInfo eti : extraTabVector) {
                    if (null != eti.disconnect_method) {
                        eti.disconnect_method.invoke(eti.jp);
                    }

                }
            }
            num_connected = 0;
            UpdateConnectedDisplays();
            if (javax_swing_timer.isRunning()) {
                javax_swing_timer.stop();
            }

            jCheckBoxDetailsModuleConnected.setSelected(false);
            jCheckBoxAuxChannelConnected.setSelected(false);
        } catch (Exception e) {
            PrintException(e);
        } finally {
            if (null != dp) {
                dp.set_connected(false);
            }

        }
    }

    private void jCheckBoxMenuItemConnectedItemStateChanged(java.awt.event.ItemEvent evt)//GEN-FIRST:event_jCheckBoxMenuItemConnectedItemStateChanged
    {//GEN-HEADEREND:event_jCheckBoxMenuItemConnectedItemStateChanged
    }//GEN-LAST:event_jCheckBoxMenuItemConnectedItemStateChanged

    private void jMenuItemFileExitActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemFileExitActionPerformed
    {//GEN-HEADEREND:event_jMenuItemFileExitActionPerformed
        System.exit(0);
    }//GEN-LAST:event_jMenuItemFileExitActionPerformed

    private void ExitFullScreen() {
        GraphicsDevice myDevice = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        DisplayMode newDisplayMode;

        DisplayMode oldDisplayMode = myDevice.getDisplayMode();
        this.dispose();
        this.setUndecorated(false);
        myDevice.setFullScreenWindow(null);
        this.setVisible(true);
        validate();
    }

    private void SetFullScreenMode() {

        GraphicsDevice myDevice = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
        DisplayMode newDisplayMode;

        DisplayMode oldDisplayMode = myDevice.getDisplayMode();
        this.dispose();
        this.setUndecorated(true);
        myDevice.setFullScreenWindow(this);
        this.setVisible(true);
        validate();
    }

    private void LoadStatToSend() {
        if (null == curModule) {
            return;
        }
//	WatchVarEditor  wve = (WatchVarEditor) watchJPanelCmdToSend.getTableColumnCellEditor(0);
//	System.out.println("LoadStatToSend : wve="+wve);
        watchJPanelCmdToSend.setTableCellEditor(1, cmdToSendWve);
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("Loading command to send");
        }

        String statusString;
        StructureTypeInfo typeInfo;

        statusString =
                (String) jListCmdsAvailable.getSelectedValue();
        if (null == statusString) {
            return;
        }

        String s2 = null;
        int eq_index = statusString.indexOf('=');
        if (eq_index < 0) {
            s2 = statusString.substring(0, eq_index);
        } else {
            s2 = statusString;
        }

        this.jTextFieldCmdToSendMsgType.setText(s2);
        curModule.last_selected_command_index = jListCmdsAvailable.getSelectedIndex();
        Long idLong = Long.valueOf(0);
        String idString;

        int index = statusString.indexOf("=");
        if (index < 1) {
            return;
        }

        idString = statusString.substring(index + 1);
        statusToSendIdString =
                idString;
        idLong =
                Long.valueOf(idString);
        typeInfo =
                (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(statusString.substring(0, index));
        Hashtable ht = ModuleInfo.m_structInfoByNameHashTable;
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("typeInfo=" + typeInfo);
        }

        if (typeInfo == null) {
            return;
        }

        StringTokenizer data_tokenizer = null;
        try {
            if (last_status_id_long == idLong.longValue()) {
                String prevStatusString = (String) curModule.statData;
                if (null != prevStatusString) {
                    data_tokenizer = new StringTokenizer(prevStatusString, ",");
                    data_tokenizer.nextToken();
                    data_tokenizer.nextToken();
                }

            }
        } catch (Exception e) {
            PrintException(e);
        }
        watchJPanelCmdToSend.setNmlMessageDictionary(curModule.get_cmd_msg_dict());
        watchJPanelCmdToSend.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
        watchJPanelCmdToSend.SetTypeInfo(typeInfo, ht);
        watchJPanelCmdToSend.SetDataInfo(data_tokenizer);
//	wve = (WatchVarEditor) watchJPanelCmdToSend.getTableColumnCellEditor(0);
//	System.out.println("LoadCmdToSend : wve="+wve);
        watchJPanelCmdToSend.setTableCellEditor(1, cmdToSendWve);
    }

    private void SavePreviousCmd() {
        try {
            if (null != cmdToSendIdString && null != curModule
                    && null != this.jCheckBoxModifyStatus && !jCheckBoxModifyStatus.isSelected()) {
                final String data_string = watchJPanelCmdToSend.getDataString();
                final Long Id = Long.valueOf(cmdToSendIdString);
                if (null != data_string && Id.longValue() > 0 && data_string.length() > 0) {
                    curModule.previous_commands.put(Id, cmdToSendIdString + ",0" + data_string);
                }
            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }

    private void LoadCmdToSend() {
        if (null == curModule) {
            return;
        }


//	WatchVarEditor  wve = (WatchVarEditor) watchJPanelCmdToSend.getTableColumnCellEditor(0);
//	System.out.println("LoadCmdToSend : wve="+wve);
        watchJPanelCmdToSend.setTableCellEditor(1, cmdToSendWve);
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("Loading command to send");
        }

        String cmdString;
        cmdString =
                (String) jListCmdsAvailable.getSelectedValue();
        if (null == cmdString) {
            return;
        }

        String s2 = null;
        int eq_index = cmdString.indexOf('=');
        if (eq_index > 0) {
            s2 = cmdString.substring(0, eq_index);
        } else {
            s2 = cmdString;
        }

        this.jTextFieldCmdToSendMsgType.setText(s2);
        curModule.last_selected_command_index = jListCmdsAvailable.getSelectedIndex();
        String idString;

        int index = cmdString.indexOf("=");
        if (index < 1) {
            return;
        }

        idString = cmdString.substring(index + 1);
        cmdToSendIdString =
                idString;
        Long idLong = Long.valueOf(cmdToSendIdString);
        StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(cmdString.substring(0, index));
        Hashtable ht = ModuleInfo.m_structInfoByNameHashTable;
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("typeInfo=" + typeInfo);
        }

        if (typeInfo == null) {
            return;
        }

        StringTokenizer data_tokenizer = null;
        try {
            String prevCmdString = (String) curModule.previous_commands.get(idLong);
            if (null != prevCmdString) {
                data_tokenizer = new StringTokenizer(prevCmdString, ",");
                data_tokenizer.nextToken();
                data_tokenizer.nextToken();
            }

        } catch (Exception e) {
            PrintException(e);
        }

        watchJPanelCmdToSend.setNmlMessageDictionary(curModule.get_cmd_msg_dict());
        watchJPanelCmdToSend.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
        watchJPanelCmdToSend.SetTypeInfo(typeInfo, ht);
        watchJPanelCmdToSend.SetDataInfo(data_tokenizer);
//	wve = (WatchVarEditor) watchJPanelCmdToSend.getTableColumnCellEditor(0);
//	System.out.println("LoadCmdToSend : wve="+wve);
        watchJPanelCmdToSend.setTableCellEditor(1, cmdToSendWve);
        LoadAdjusterWindow();
    }

    private void LoadAuxMsgToSend() {
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("Loading command to send");
        }
        if (null != auxMsgToSendIdString) {
            auxToSendString = auxMsgToSendIdString + ",0" + watchJPanelAuxMsgToSend.getDataString();
            curAuxBuffer.SetPreviousMessage(auxToSendString);
        }

        String auxMsgString;
        StructureTypeInfo typeInfo;

        watchJPanelCmdToSend.setTableCellEditor(1, auxToSendWve);
        auxMsgString =
                (String) jListAuxMessagesAvailable.getSelectedValue();
        if (null == auxMsgString) {
            return;
        }

        this.jLabelAuxMsgToSendType.setText(auxMsgString);
        Long idLong = Long.valueOf(0);
        String idString;

        int index = auxMsgString.indexOf("=");
        if (index < 1) {
            return;
        }

        idString = auxMsgString.substring(index + 1);
        auxMsgToSendIdString =
                idString;
        idLong =
                Long.valueOf(idString);
        typeInfo =
                (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(auxMsgString.substring(0, index));
        Hashtable ht = ModuleInfo.m_structInfoByNameHashTable;
        if (diag_common.get_debug_on()) {
            diag_common.DebugPrint("typeInfo=" + typeInfo);
        }

        if (typeInfo == null) {
            return;
        }

        diagapplet.CodeGen.STI_TokenizerInterface infoTokenizer = typeInfo.getInfoTokenizer();
        watchJPanelAuxMsgToSend.setNmlMessageDictionary(this.curAuxBuffer.write_msg_dict);
        watchJPanelAuxMsgToSend.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
        watchJPanelAuxMsgToSend.SetTypeInfo(typeInfo, ht);
        String s = curAuxBuffer.GetPreviousMessage(idLong);
        if (s != null && s.length() > 3) {
            StringTokenizer data_tokenizer = new StringTokenizer(s, ",");
            data_tokenizer.nextToken();
            data_tokenizer.nextToken();
            if (data_tokenizer.hasMoreTokens()) {
                watchJPanelAuxMsgToSend.SetDataInfo(data_tokenizer);
            }
        }
        watchJPanelCmdToSend.setTableCellEditor(1, auxToSendWve);
        curAuxBuffer.last_selected_msg_index = this.jListAuxMessagesAvailable.getSelectedIndex();
    }

    private void jListCmdsAvailableValueChanged(javax.swing.event.ListSelectionEvent evt)//GEN-FIRST:event_jListCmdsAvailableValueChanged
    {//GEN-HEADEREND:event_jListCmdsAvailableValueChanged
        if (jCheckBoxModifyStatus.isSelected()) {
            LoadStatToSend();
        } else {
            SavePreviousCmd();
            LoadCmdToSend();
        }
    }//GEN-LAST:event_jListCmdsAvailableValueChanged

    private void SelectModule(String modName) {

        this.SavePreviousCmd();
        try {
            if (null != curModule && modName.compareTo(curModule.Name) == 0) {
                return;
            }

            cmdToSendString = null;
            statusToSendString =
                    null;
            dp.setSelectedModule(modName);
            if (jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected() && curModule != null && curModule.is_connected) {
                curModule.disconnect();
                LoadNmlTable();
            }

            curModule = (ModuleInfo) cgc.get_m_modulesHashTable().get(modName);
            if (jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.isSelected() && curModule != null && !curModule.is_connected) {
                curModule.connect();
                LoadNmlTable();
            }

            if (jCheckBoxModifyStatus.isSelected()) {
                jListCmdsAvailable.setListData(curModule.statMsgsAvailable);
            } else {
                jListCmdsAvailable.setListData(curModule.cmdsAvailable);
            }

            this.jButtonPlotCmdVar.setText("Plot variable. (disabled)");
            this.jButtonPlotCmdVar.setEnabled(false);
            this.jButtonPlotCmdArray.setText("Plot array. (disabled)");
            this.jButtonPlotCmdArray.setEnabled(false);
            this.jButtonPlotStatusVar.setText("Plot variable. (disabled)");
            this.jButtonPlotStatusVar.setEnabled(false);
            this.jButtonPlotStatusArray.setText("Plot array. (disabled)");
            this.jButtonPlotStatusArray.setEnabled(false);
//	    this.jLabelCmdBufferLine.setText(curModule.m_cmd_read_Connection.getBufferLine());
//	    this.jLabelStatusBufferLine.setText(curModule.m_stat_read_Connection.getBufferLine());
            this.jTextPaneDetailsBottom.setText(curModule.m_cmd_read_Connection.getBufferLine() + "\n" + curModule.m_stat_read_Connection.getBufferLine() + "\n");
            this.jLabelCmdsRecvd.setText("0");

            this.jTextFieldCmdToSendMsgType.setText("");
            this.jTextFieldCurrentCommandMsgType.setText("");
            this.jLabelStatusRecvd.setText("0");
            this.jTextFieldCurrentStatusType.setText("");
            this.watchJPanelCmdToSend.Clear();
            this.watchJPanelDetailsCmd.Clear();
            this.watchJPanelDetailsStatus.Clear();
            if (!jCheckBoxModifyStatus.isSelected()) {
                this.jListCmdsAvailable.setSelectedIndex(curModule.last_selected_command_index);
            }

            last_status_id_long = -1;
            last_cmd_id_long =
                    -1;
            this.jCheckBoxDetailsModuleConnected.setSelected(curModule.is_connected);
        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void jListModulesValueChanged(javax.swing.event.ListSelectionEvent evt)//GEN-FIRST:event_jListModulesValueChanged
    {//GEN-HEADEREND:event_jListModulesValueChanged
        SelectModule((String) jListModules.getSelectedValue());
    }//GEN-LAST:event_jListModulesValueChanged
    static int count = 0;

    private void jPaintablePanelHierarchyInnerComponentMoved(java.awt.event.ComponentEvent evt)//GEN-FIRST:event_jPaintablePanelHierarchyInnerComponentMoved
    {//GEN-HEADEREND:event_jPaintablePanelHierarchyInnerComponentMoved
        count++;
        hierarchyDraw.need_clear = true;
    }//GEN-LAST:event_jPaintablePanelHierarchyInnerComponentMoved

    synchronized private void LoadHierarchy(final String s) {
        diagapplet.utils.DiagError.println("Loading " + s + " . . .\n");
        DisconnectFromAll();
        last_hierarchy_loaded = s;
        if (null == hierarchy_loader) {
            String fullPath = new File(s).getAbsolutePath();
            if (!recentVector.contains(fullPath)) {
                recentVector.add(fullPath);
                JMenuItem recentMenuItemToAdd = new JMenuItem(fullPath);
                recentMenuItemToAdd.addActionListener(new ActionListener() {

                    @Override
                    public void actionPerformed(ActionEvent e) {
                        LoadHierarchy(((JMenuItem) e.getSource()).getText());
                    }
                });
                jMenuFileRecent.add(recentMenuItemToAdd);
                try {
                    XMLEncoder encoder = new XMLEncoder(
                            new BufferedOutputStream(
                            new FileOutputStream(recentFile)));
                    encoder.writeObject(recentVector);
                    encoder.close();
                } catch (Exception e) {
                    PrintException(e);
                }
                this.WriteLastDirs();
            }
            if (s.endsWith(".nml")) {
                SetNMLConfigFile(s);
                return;
            } else {
                try {
                    File f = new File(s);
                    String sbase = f.getName();
                    int pindex = sbase.indexOf('.');
                    if (pindex > 0) {
                        sbase = sbase.substring(0, pindex);
                    }

                    File homeDir = new File(System.getProperty("user.home"));
                    File fsave = new File(System.getProperty("user.home"), ".diag_" + sbase + ".xml");
                    if (fsave.exists() && fsave.canRead()) {
                        XMLDecoder decoder = new XMLDecoder(
                                new BufferedInputStream(
                                new FileInputStream(fsave)));
                        diagPreserve saved_dp = (diagPreserve) decoder.readObject();
                        if (null != saved_dp) {
                            if (null != saved_dp.getDefaultNMLConfigFile()) {
                                ModuleInfo.DefaultNMLConfigurationFile = saved_dp.getDefaultNMLConfigFile();
                            }
                        }
                        decoder.close();
                        decoder = null;
                    }

                } catch (Exception e) {
                    PrintException(e);
                }
            }
            try {
                HierarchyDraw.updating_hierarchy = true;
                hierarchy_loader =
                        new diagapplet.HierarchyLoad_NB_UI();
                hierarchy_loader.setDefaultCloseOperation(HierarchyLoad_NB_UI.HIDE_ON_CLOSE);
                hierarchy_loader.pack();
                hierarchy_loader.setVisible(true);
                hierarchy_loader.toFront();
                SwingWorker backgroundSwingWorkerToQueue =
                        hierarchy_loader.CreateLoadHierarchySwingWorker(cgc, s,
                        new Runnable() {

                            @Override
                            public void run() {
                                FinishLoadHierarchy();
                                NextBackgroundSwingWorker();
                            }
                        });
                QueueSwingWorker(backgroundSwingWorkerToQueue);
            } catch (Exception ex) {
                PrintException(ex);
            }
        }

    }

    private void AddAuxChannel(String aux_name, ModuleInfo mi) {
        if (null == auxBuffersHashtable) {
            auxBuffersHashtable = new Hashtable<String, BufferInfo>();
        }

        if (!auxBuffersHashtable.containsKey(aux_name)) {
            BufferInfo bi = new BufferInfo();
            bi.Name = aux_name;
            bi.mi = mi;
            bi.configFile = mi.NMLConfigurationFile;
            bi.read_msg_dict = mi.get_aux_diag_msg_read_dict();
            bi.write_msg_dict = mi.get_aux_diag_msg_write_dict();
            auxBuffersHashtable.put(bi.Name, bi);
        }

    }

    private void LoadAuxChannelsHashtable() {
        try {
            if (null != cgc && null != cgc.get_m_modulesHashTable()) {
                for (Object mi_obj : cgc.get_m_modulesHashTable().values()) {
                    ModuleInfo mi = (ModuleInfo) mi_obj;
                    if (null != mi.AuxInputNames) {
                        for (Object auxInput_obj : mi.AuxInputNames) {
                            String auxInputName = (String) auxInput_obj;
                            AddAuxChannel(auxInputName, mi);
                        }

                    }
                    if (null != mi.AuxOutputNames) {
                        for (Object auxOutput_obj : mi.AuxOutputNames) {
                            String auxOutputName = (String) auxOutput_obj;
                            AddAuxChannel(auxOutputName, mi);
                        }

                    }
                }
            }
        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void LoadAuxJList() {
        try {
            if (null != auxBuffersHashtable) {
                List<String> lst = Collections.list(auxBuffersHashtable.keys());
                Collections.sort(lst);
                jListAuxChannels.setListData(lst.toArray());
            }

        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void LoadPlotSet(plotSetPreserve psP) throws Exception {
        try {
            if (null != psP) {
                Vector<plotTrackerPreserve> PlotList = psP.getPlotList();
                Vector<plotTrackerPreserve> ptps_to_remove = null;
                if (null != PlotList) {
                    for (plotTrackerPreserve ptP : PlotList) {
                        boolean is_aux = ptP.isIsAux();
                        boolean is_cmd = ptP.isIsCmd();
                        String variable_name = ptP.getVariableName();
                        int variable_number = ptP.getVariableNumber();
                        String modName = ptP.getModuleName();
                        ModuleInfo mi = null;
                        if (null != modName) {
                            mi = (ModuleInfo) cgc.get_m_modulesHashTable().get(modName);
                        }

                        String bufName = ptP.getBufferName();
                        BufferInfo bi = null;
                        if (is_aux) {
                            if (null != bufName && null != auxBuffersHashtable) {
                                bi = auxBuffersHashtable.get(bufName);
                            }

                            if (null == bi) {
                                diagapplet.utils.DiagError.println("Can't get BufferInfo for " + bufName + " refered to in LoadPlotSet(" + ptP + ")");
                                if (ptps_to_remove == null) {
                                    ptps_to_remove = new Vector<plotTrackerPreserve>();
                                }

                                ptps_to_remove.add(ptP);
                                continue;
                            }

                        }
                        long msg_type = ptP.getMsgType();
                        if (ptP.isIsArray()) {
                            StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(ptP.getStiName());
                            this.StartPlottingArray(sti, bi, mi, msg_type, variable_number, variable_name, is_cmd, is_aux);
                        } else {
                            this.StartPlottingVariable(bi, mi, msg_type, variable_number, variable_name, is_cmd, is_aux);
                        }

                    }
                    UpdatePlots();
                    plotter_NB_UI1.setFuncArg(psP.getFuncArg());
                    plotter_NB_UI1.setGraphFunction(psP.getGraphFunction());
                }

                if (null != ptps_to_remove) {
                    psP.getPlotList().removeAll(ptps_to_remove);
                }

            }
        } catch (Exception e) {
            PrintException(e);
        }

    }

    private void FinishLoadHierarchy() {
        boolean do_connect_all = false;
        if (null != hierarchy_loader) {
            LoadParameters();
            if (list_modules_by_number) {
                for (Object obj : cgc.get_m_modulesHashTable().values()) {
                    ModuleInfo mi = (ModuleInfo) obj;
                    if (null != mi.children_names) {
                        Collections.sort(mi.children_names, new Comparator<String>() {

                            @Override
                            public int compare(String o1, String o2) {
                                ModuleInfo mi1 = (ModuleInfo) cgc.get_m_modulesHashTable().get(o1);
                                ModuleInfo mi2 = (ModuleInfo) cgc.get_m_modulesHashTable().get(o2);
                                return (mi1.module_number - mi2.module_number);
                            }
                        });
                    }

                }
            }
            hierarchyDraw.FindAllParents(cgc.get_m_modulesHashTable(), null);
            HierarchyDraw.updating_hierarchy = false;
            hierarchyDraw.need_clear = true;
            LoadAuxChannelsHashtable();
            LoadAuxJList();
            XMLDecoder decoder = null;
            try {
                File f = new File(last_hierarchy_loaded);
                String sbase = f.getName();
                int pindex = sbase.indexOf('.');
                if (pindex > 0) {
                    sbase = sbase.substring(0, pindex);
                }

                File homeDir = new File(System.getProperty("user.home"));
                try {
                    File fsave = new File(System.getProperty("user.home"), ".diag_" + sbase + ".xml");
                    if (fsave.exists() && fsave.canRead()) {
                        decoder = new XMLDecoder(
                                new BufferedInputStream(
                                new FileInputStream(fsave)));
                        diagPreserve saved_dp = (diagPreserve) decoder.readObject();
                        if (null != saved_dp) {
                            dp = saved_dp;
                            this.setAutomatically_Keep_and_Use_PlotSets(dp.isAutomatically_Keep_and_Use_PlotSets());
                            if (null != this.jCheckBoxMenuItemAutoSavePlotSets) {
                                this.jCheckBoxMenuItemAutoSavePlotSets.setSelected(this.Automatically_Keep_and_Use_PlotSets);
                            }
                            if (!this.Automatically_Keep_and_Use_PlotSets) {
                                this.plot_tracker_hashtable = null;
                                dp.setPlotSet(null);
                            }
                            if (null == ModuleInfo.DefaultNMLConfigurationFile
                                    && null != dp.getDefaultNMLConfigFile()) {
                                this.SetNMLConfigFile(dp.getDefaultNMLConfigFile());
                            }
                            if (dp.isResized()) {
                                this.setSize(dp.getWidth(), dp.getHeight());
                            }

                            if (dp.isMoved()) {
                                this.setLocation(dp.getX(), dp.getY());
                            }

                            if (param_set_diag_minimized
                                    || diagapplet.CodeGen.StringFuncs.getenv("DIAG_MINIMIZED") != null) {
                                this.setExtendedState(java.awt.Frame.ICONIFIED);
                            } else if (dp.getWindow_state() != JFrame.NORMAL && this.getExtendedState() == JFrame.NORMAL) {
                                this.setExtendedState(dp.getWindow_state());
                            }

                            if (param_set_diag_auto_connect_disconnect
                                    || diagapplet.CodeGen.StringFuncs.getenv("DIAG_AUTO_CONNECT_DISCONNECT") != null) {
                                this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.setSelected(true);
                                dp.setAuto_connect_disconnect(true);
                            } else {
                                this.jCheckBoxMenuItemAutoConnectDisconnectAsNeeded.setSelected(dp.isAuto_connect_disconnect());
                            }

                            if (dp.get_connected() && !jCheckBoxMenuItemConnected.isSelected()) {
                                //ConnectToAll();
                                do_connect_all = true;
                            }

                            for (final String cf : dp.get_recentNmlConfigFileVector()) {
                                JMenuItem jmi = new JMenuItem(cf);
                                jmi.addActionListener(new ActionListener() {

                                    @Override
                                    public void actionPerformed(ActionEvent e) {
                                        SetNMLConfigFile(cf);
                                    }
                                });
                                this.jMenuConnectionsRecentNmlFiles.add(jmi);
                            }

                            if (null != dp.getRecentPlotSets()) {
                                for (String filename : dp.getRecentPlotSets()) {
                                    AddRecentPlotSetMenuItem(filename);
                                }

                            }
                            try {
                                if (null != dp.getModulesVector()) {
                                    for (modulePreserve mp : dp.getModulesVector()) {
                                        ModuleInfo mi = (ModuleInfo) cgc.get_m_modulesHashTable().get(mp.getName());
                                        if (null != mi) {
                                            mi.previous_commands = mp.getPreviousCommands();
                                            mi.last_selected_command_index = mp.getLastSelectedCommandIndex();
                                        }

                                    }
                                }
                            } catch (Exception e) {
                                PrintException(e);
                            }
                            try {
                                if (null != dp.getAuxBuffersVector()) {
                                    for (auxBufferPreserve abp : dp.getAuxBuffersVector()) {
                                        BufferInfo bi = (BufferInfo) this.auxBuffersHashtable.get(abp.getName());
                                        if (null != bi) {
                                            bi.setPreviousMessagesHashtable(abp.getPreviousMessages());
                                            bi.last_selected_msg_index = abp.getLastSelectedMessageIndex();
                                        }
                                    }
                                }
                            } catch (Exception e) {
                                PrintException(e);
                            }

                            if (dp.getSelectedModule() != null) {
                                jListModules.setSelectedValue(dp.getSelectedModule(), true);
                            }

                            if (dp.getSelectedAuxChannel() != null) {
                                jListAuxChannels.setSelectedValue(dp.getSelectedAuxChannel(), true);
                            }

                            if (!tab_selected && dp.getSelectedTab() >= 0 && dp.getSelectedTab() < jTabbedPane1.getTabCount()) {
                                jTabbedPane1.setSelectedIndex(dp.getSelectedTab());
                            }

                        }
                    }
                } catch (Exception ex) {
                    PrintException(ex);
                }
                try {
                    if (null != cgc && null != cgc.get_extraActionsVector()) {
                        for (Object obj : cgc.get_extraActionsVector()) {
                            this.AddExtraAction((String) obj);
                        }
                    }
                } catch (Exception e) {
                    PrintException(e);
                }
                try {
                    if (null != cgc && null != cgc.get_extraTabsVector()) {
                        for (Object obj : cgc.get_extraTabsVector()) {
                            this.AddExtraTab((String) obj);
                        }
                    }
                } catch (Exception e) {
                    PrintException(e);
                }
                if (connectOnStartup && !jCheckBoxMenuItemConnected.getState()) {
                    //ConnectToAll();
                    do_connect_all = true;
                }

            } catch (Exception e) {
                PrintException(e);
            } finally {
                try {
                    if (null != decoder) {
                        decoder.close();
                        decoder =
                                null;
                    }

                } catch (Exception e) {
                }
            }

            hierarchyDraw.UpdateDisplay(true);
            hierarchy_loader.setVisible(false);
            diagapplet.utils.DiagError.println("Finished loading " + last_hierarchy_loaded + "\n");
            if (do_connect_all) {
                ConnectToAll();
            } else {
                check_running_file();
            }
        }
    } // end FinishLoadHierarchy()
    private File last_dir = null;

    private void jMenuItemFileOpenActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemFileOpenActionPerformed
    {//GEN-HEADEREND:event_jMenuItemFileOpenActionPerformed

        JFileChooser chooser = new JFileChooser();
        if (last_dir != null) {
            chooser.setCurrentDirectory(last_dir);
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        FileNameExtensionFilter nml_filter = new FileNameExtensionFilter("nml", "nml");
        FileNameExtensionFilter cfg_filter = new FileNameExtensionFilter("cfg", "cfg");
        FileNameExtensionFilter diag_filter = new FileNameExtensionFilter("diag", "diag");

        chooser.addChoosableFileFilter(nml_filter);
        chooser.addChoosableFileFilter(cfg_filter);
        chooser.addChoosableFileFilter(diag_filter);
        int returnVal = chooser.showOpenDialog(this);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            System.out.println("You chose to open this file: "
                    + chooser.getSelectedFile().getPath());
            last_dir =
                    chooser.getCurrentDirectory();
            rcs.utils.URL_and_FileLoader.current_directory = last_dir.getPath();
            rcs.utils.URL_and_FileLoader.AddToSearchPath(last_dir.getPath());
            LoadHierarchy(chooser.getSelectedFile().getPath());
        }
    }//GEN-LAST:event_jMenuItemFileOpenActionPerformed

    private void jCheckBoxMenuItemConnectedActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jCheckBoxMenuItemConnectedActionPerformed
    {//GEN-HEADEREND:event_jCheckBoxMenuItemConnectedActionPerformed
        if (jCheckBoxMenuItemConnected.getState()) {
            ConnectToAll();
        } else {
            DisconnectFromAll();
        }
    }//GEN-LAST:event_jCheckBoxMenuItemConnectedActionPerformed

    private static void PrintToStreamAndErrLog(PrintStream ps, String str) {
        diagapplet.utils.DiagError.println(str);
        ps.println(str);
    }
    private final static String IdString = "$Id: diag_NB_UI.java 2438 2014-11-06 15:21:34Z shackle $ " + diagapplet.diag_NB_UI.class;

    private void jMenuItemFileDebugDumpActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemFileDebugDumpActionPerformed
    {//GEN-HEADEREND:event_jMenuItemFileDebugDumpActionPerformed
        DebugDump();
    }

    private void DebugDump() {
        try {
            final Map<Thread, StackTraceElement[]> allStackTraces = Thread.getAllStackTraces();
            final Runtime rt = Runtime.getRuntime();
            final Properties p = System.getProperties();
            final long cur_time = System.currentTimeMillis();
            final File f = new File("diag_debug_" + cur_time + ".txt");
            System.out.println("writing to " + f.getAbsolutePath());
            diagapplet.utils.DiagError.println("Wrote debug info to " + f.getAbsolutePath());
            FileOutputStream fos = new FileOutputStream(f);
            PrintStream ps = new PrintStream(fos);
            PrintToStreamAndErrLog(ps, "Beginning of " + f.getAbsolutePath());
            ps.println("previous jTextAreaErrs:\n" + this.jTextAreaErrs.getText() + "\nEnd jTextAreaErrs\n");
            PrintToStreamAndErrLog(ps, "ID=" + IdString);
            PrintToStreamAndErrLog(ps, "rcs.RCS_VERSION.info_string=" + rcs.RCS_VERSION.info_string);
            PrintToStreamAndErrLog(ps, "");
            PrintToStreamAndErrLog(ps, "this=" + this);
            PrintToStreamAndErrLog(ps, "this.hierarchy_loader=" + this.hierarchy_loader);
            PrintToStreamAndErrLog(ps, "diagapplet.CodeGen.ModuleInfo.last_loading_module=" + diagapplet.CodeGen.ModuleInfo.get_last_loading_module_string());
            PrintToStreamAndErrLog(ps, "this.curModule=" + this.curModule);
            PrintToStreamAndErrLog(ps, "this.num_connected=" + num_connected);
            PrintToStreamAndErrLog(ps, "this.ExecutingBackgroundSwingWorker=" + ExecutingBackgroundSwingWorker);
            if (null != ExecutingBackgroundSwingWorker) {
                PrintToStreamAndErrLog(ps, "this.ExecutingBackgroundSwingWorker.getState()=" + ExecutingBackgroundSwingWorker.getState());
                PrintToStreamAndErrLog(ps, "this.ExecutingBackgroundSwingWorker.getGetProgress()=" + ExecutingBackgroundSwingWorker.getProgress());
            }
            PrintToStreamAndErrLog(ps, "cur_time=" + cur_time);
            if (null != this.curAuxBuffer) {
                PrintToStreamAndErrLog(ps, "this.curAuxBuffer.read_nml=" + this.curAuxBuffer.read_nml);
                PrintToStreamAndErrLog(ps, "this.curAuxBuffer.write_nml=" + this.curAuxBuffer.write_nml);
                PrintToStreamAndErrLog(ps, "");
            }

            PrintToStreamAndErrLog(ps, "this.backgroundSwingWorkerQueue=" + this.backgroundSwingWorkerQueue);
            if (null != this.backgroundSwingWorkerQueue) {
                for (SwingWorker worker : this.backgroundSwingWorkerQueue) {
                    PrintToStreamAndErrLog(ps, "\tswing_worker:" + worker);
                }
            }
            PrintToStreamAndErrLog(ps, "this.last_queue_add_size=" + this.last_queue_add_size);
            PrintToStreamAndErrLog(ps, "this.last_next_bg_queue_size=" + this.last_next_bg_queue_size);


            PrintToStreamAndErrLog(ps, "this.bgExecuteThread=" + this.bgExecuteThread);
            PrintToStreamAndErrLog(ps, "cur_time - this.bgExecute_time=" + (cur_time - this.bgExecute_time));
            PrintToStreamAndErrLog(ps, "this.bgExecuteStackTrace=" + this.bgExecuteStackTrace);
            if (null != this.bgExecuteStackTrace) {
                for (StackTraceElement ste : this.bgExecuteStackTrace) {
                    PrintToStreamAndErrLog(ps, "\t" + ste.toString());
                }

            }

            PrintToStreamAndErrLog(ps, "this.bgQueueAddExecuteThread=" + this.bgQueueAddExecuteThread);
            PrintToStreamAndErrLog(ps, "cur_time - this.bgQueueAddExecute_time=" + (cur_time - this.bgQueueAddExecute_time));
            PrintToStreamAndErrLog(ps, "this.bgQueueAddExecuteStackTrace=" + this.bgQueueAddExecuteStackTrace);
            if (null != this.bgQueueAddExecuteStackTrace) {
                for (StackTraceElement ste : this.bgQueueAddExecuteStackTrace) {
                    PrintToStreamAndErrLog(ps, "\t" + ste.toString());
                }

            }

            PrintToStreamAndErrLog(ps, "this.bgNextExecuteThread=" + this.bgNextExecuteThread);
            PrintToStreamAndErrLog(ps, "cur_time - this.bgNextExecute_time=" + (cur_time - this.bgNextExecute_time));
            PrintToStreamAndErrLog(ps, "this.bgNextExecuteStackTrace=" + this.bgNextExecuteStackTrace);

            if (null != this.bgNextExecuteStackTrace) {
                for (StackTraceElement ste : this.bgNextExecuteStackTrace) {
                    PrintToStreamAndErrLog(ps, "\t" + ste.toString());
                }
            }

            PrintToStreamAndErrLog(ps, "this.last_bgExecuteStackTrace=" + this.last_bgExecuteStackTrace);
            PrintToStreamAndErrLog(ps, "this.last_bgExecuteThread=" + this.last_bgExecuteThread);
            PrintToStreamAndErrLog(ps, "cur_time - this.last_bgExecute_time=" + (cur_time - this.last_bgExecute_time));
            if (null != this.last_bgExecuteStackTrace) {
                for (StackTraceElement ste : this.last_bgExecuteStackTrace) {
                    PrintToStreamAndErrLog(ps, "\t" + ste.toString());
                }
            }

            PrintToStreamAndErrLog(ps, "this.javax_swing_timer=" + this.javax_swing_timer);
            if (null != this.javax_swing_timer) {

                PrintToStreamAndErrLog(ps, "this.javax_swing_timer.isRunning()=" + this.javax_swing_timer.isRunning());
                PrintToStreamAndErrLog(ps, "this.javax_swing_timer.getDelay()=" + this.javax_swing_timer.getDelay());
            }

            PrintToStreamAndErrLog(ps, "");
            PrintToStreamAndErrLog(ps, "this.plot_tracker_hashtable=" + this.plot_tracker_hashtable);
            if (null != this.plot_tracker_hashtable) {
                for (PlotTracker pt : plot_tracker_hashtable.values()) {
                    PrintToStreamAndErrLog(ps, "\tPlotTracker : " + pt);
                }

            }
            PrintToStreamAndErrLog(ps, "");
            PrintToStreamAndErrLog(ps, "this.auxBuffersHashtable=" + this.auxBuffersHashtable);
            if (null != this.auxBuffersHashtable) {
                for (BufferInfo bi : auxBuffersHashtable.values()) {
                    PrintToStreamAndErrLog(ps, "\tBufferInfo : " + bi);
                }

            }
            PrintToStreamAndErrLog(ps, "this.cgc=" + this.cgc);
            if (null != this.cgc) {
                PrintToStreamAndErrLog(ps, "this.cgc.get_optionsHashTable()=" + this.cgc.get_optionsHashTable());
                if (null != this.cgc.get_optionsHashTable()) {
                    for (Object key : cgc.get_optionsHashTable().keySet()) {
                        Object val = cgc.get_optionsHashTable().get(key);
                        PrintToStreamAndErrLog(ps, "\t" + key + "=" + val);
                    }

                }
                PrintToStreamAndErrLog(ps, "");
                PrintToStreamAndErrLog(ps, "this.cgc.get_m_modulesHashTable()=" + this.cgc.get_m_modulesHashTable());
                if (null != this.cgc.get_m_modulesHashTable()) {
                    for (Object obj : cgc.get_m_modulesHashTable().values()) {
                        ModuleInfo mi = (ModuleInfo) obj;
                        PrintToStreamAndErrLog(ps, "\tModuleInfo : " + mi);
                    }

                }
            }


            PrintToStreamAndErrLog(ps, "System.getProperies() = " + p);
            p.list(ps);
            PrintToStreamAndErrLog(ps, "");

            PrintToStreamAndErrLog(ps, "Runtime.getRuntime()= +" + rt);
            PrintToStreamAndErrLog(ps, "Runtime.getRuntime().availableProcessors() = " + rt.availableProcessors());
            PrintToStreamAndErrLog(ps, "Runtime.getRuntime().freeMemory() = " + rt.freeMemory());
            PrintToStreamAndErrLog(ps, "Runtime.getRuntime().maxMemory() = " + rt.maxMemory());
            PrintToStreamAndErrLog(ps, "Runtime.getRuntime().totalMemory() = " + rt.totalMemory());
            PrintToStreamAndErrLog(ps, "");

            for (Thread t : allStackTraces.keySet()) {
                ps.print("");
                PrintToStreamAndErrLog(ps, "Thread : " + t);
                StackTraceElement[] stea = allStackTraces.get(t);
                for (StackTraceElement ste : stea) {
                    PrintToStreamAndErrLog(ps, "\t" + ste.toString());
                }

                PrintToStreamAndErrLog(ps, "");
            }

            PrintToStreamAndErrLog(ps, "End of " + f.getAbsolutePath());
            PrintToStreamAndErrLog(ps, "");
            ps.close();
            fos.close();
            this.jTabbedPane1.setSelectedComponent(this.jPanelErrorPanel);
            JOptionPane.showMessageDialog(this, "Wrote debug info to " + f.getAbsolutePath(), this.getTitle() + " info", JOptionPane.PLAIN_MESSAGE);
        } catch (Exception ex) {
            PrintException(ex);
        }

    }//GEN-LAST:event_jMenuItemFileDebugDumpActionPerformed
    private String nmlcfgsvrStr = null;

    private void jMenuItemOpenNmlCfgsvrConnectionActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jMenuItemOpenNmlCfgsvrConnectionActionPerformed
    {//GEN-HEADEREND:event_jMenuItemOpenNmlCfgsvrConnectionActionPerformed
        try {
            String nmlcfgsvr_options = JOptionPane.showInputDialog("nmlcfgsvr options string ie \"host:port\"");
            if (nmlcfgsvr_options != null) {
                nmlcfgsvrStr = "nmlcfgsvr:" + nmlcfgsvr_options;
            } else {
                nmlcfgsvrStr = "";
            }

            this.SetNMLConfigFile(nmlcfgsvrStr);
        } catch (Exception ex) {
            PrintException(ex);
        }

    }//GEN-LAST:event_jMenuItemOpenNmlCfgsvrConnectionActionPerformed

    private void jFormattedTextFieldAdjusterValueActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jFormattedTextFieldAdjusterValueActionPerformed
    {//GEN-HEADEREND:event_jFormattedTextFieldAdjusterValueActionPerformed
        try {
            if (evt.getSource() == this.jFormattedTextFieldAdjusterValue) {
                double value = Double.valueOf(this.jFormattedTextFieldAdjusterValue.getText());
                SetjScrollBarAdjustVarValue(value);
            }

        } catch (Exception exception) {
            PrintException(exception);
        }

    }//GEN-LAST:event_jFormattedTextFieldAdjusterValueActionPerformed

private void jMenuItemDumpStructActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemDumpStructActionPerformed
    try {
        String struct_name = JOptionPane.showInputDialog("Name of structure to dump.");
        StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(struct_name);
        if (null != sti) {
            diagapplet.utils.DiagError.println(sti.toString());
        } else {
            JOptionPane.showMessageDialog(this, "Can't get info for " + struct_name + " : options printed to errlog.");
            Enumeration keys = ModuleInfo.m_structInfoByNameHashTable.keys();
            while (keys.hasMoreElements()) {
                diagapplet.utils.DiagError.println(keys.nextElement().toString());
            }
        }
    } catch (Exception e) {
        e.printStackTrace();
    }
}//GEN-LAST:event_jMenuItemDumpStructActionPerformed
    private File last_header_dir = null;

    private void UpdateAuxInfo() {
        this.curModule.AddAllAuxMessages();
            if (null == auxBuffersHashtable) {
                auxBuffersHashtable = new Hashtable<String, BufferInfo>();
            }

            Enumeration aux_buffers_elements = this.auxBuffersHashtable.elements();
            while (aux_buffers_elements.hasMoreElements()) {
                BufferInfo bi = (BufferInfo) aux_buffers_elements.nextElement();
                if (null == bi.mi) {
                    bi.mi = this.curModule;
                } else {
                    bi.mi.AddAllAuxMessages();
                }
            }
    }
    private void BrowseOpenInputHeaders() {
        JFileChooser jchooser = new JFileChooser();
        FileNameExtensionFilter h_filter = new FileNameExtensionFilter("h", "h");
        FileNameExtensionFilter hpp_filter = new FileNameExtensionFilter("hpp", "hpp");
        FileNameExtensionFilter hh_filter = new FileNameExtensionFilter("hh", "hh");
        jchooser.addChoosableFileFilter(h_filter);
        jchooser.addChoosableFileFilter(hpp_filter);
        jchooser.addChoosableFileFilter(hh_filter);
        jchooser.setMultiSelectionEnabled(true);
        if (null != this.last_header_dir) {
            jchooser.setCurrentDirectory(this.last_header_dir);
        } else if (null != this.last_dir) {
            jchooser.setCurrentDirectory(last_dir);
        }
        jchooser.showOpenDialog(this);
        if (null == this.curModule) {
            this.curModule = new ModuleInfo(cgc.get_diag_dict_creator(), cgc.get_nml_creator());
        }
        File fA[] = jchooser.getSelectedFiles();
        if (null != fA && fA.length > 0) {
            this.last_header_dir = fA[0].getParentFile();
            if (null == this.last_dir) {
                this.last_dir = this.last_header_dir;
            }
            rcs.utils.URL_and_FileLoader.current_directory = last_header_dir.getAbsolutePath();
            rcs.utils.URL_and_FileLoader.AddToSearchPath(rcs.utils.URL_and_FileLoader.current_directory);
            for (int fAi = 0; fAi < fA.length; fAi++) {
                File f = fA[fAi];
                String abs_path = f.getAbsolutePath();
                diagapplet.utils.DiagError.println("Loading " + fAi + " of " + fA.length + " : " + abs_path);
                this.curModule.LoadAuxTypeFile(abs_path);
            }
            UpdateAuxInfo();
        }
    }

private void jMenuItemOpenInputHeaderActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemOpenInputHeaderActionPerformed
    this.BrowseOpenInputHeaders();
}//GEN-LAST:event_jMenuItemOpenInputHeaderActionPerformed

    private void OpenPackedMessageFile(File f) {
        if (null != last_dir) {
            rcs.utils.URL_and_FileLoader.current_directory = last_dir.getPath();
            rcs.utils.URL_and_FileLoader.AddToSearchPath(last_dir.getPath());
        }
        rcs.nml.NMLMessageDictionary dict = this.curModule.get_aux_diag_msg_read_dict();
        MessageFileJFrame pjf = new MessageFileJFrame();
        pjf.setNmlMessageDictionary(dict);
        pjf.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
        pjf.SetTypeInfo(null, ModuleInfo.m_structInfoByNameHashTable);
        pjf.LoadPackedFile(f);
        pjf.setTitle(f.getName());
        pjf.setVisible(true);
        pjf.toFront();
        this.setExtendedState(java.awt.Frame.ICONIFIED);
        pjf.parent = this;
    }

    private void OpenXMLMessageFile(File f) {
        if (null != last_dir) {
            rcs.utils.URL_and_FileLoader.current_directory = last_dir.getPath();
            rcs.utils.URL_and_FileLoader.AddToSearchPath(last_dir.getPath());
        }
        rcs.nml.NMLMessageDictionary dict = this.curModule.get_aux_diag_msg_read_dict();
        MessageFileJFrame pjf = new MessageFileJFrame();
        pjf.setNmlMessageDictionary(dict);
        pjf.set_hashtable_by_id(ModuleInfo.m_structInfoHashTable);
        pjf.SetTypeInfo(null, ModuleInfo.m_structInfoByNameHashTable);
        pjf.LoadXMLFile(f);
        pjf.setTitle(f.getName());
        pjf.setVisible(true);
        pjf.toFront();
        this.setExtendedState(java.awt.Frame.ICONIFIED);
        pjf.parent = this;
    }

	private void jMenuItemOpenMsgFileActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemOpenMsgFileActionPerformed

            if (null == this.curModule) {
                JOptionPane.showMessageDialog(this, "Select an input header or open a diagonsticts configuration file before openning the packed file.");
                return;
            }
            JFileChooser chooser = new JFileChooser();
            if (last_dir != null) {
                chooser.setCurrentDirectory(last_dir);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            int returnVal = chooser.showOpenDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                System.out.println("You chose to open this file: "
                        + chooser.getSelectedFile().getPath());
                last_dir =
                        chooser.getCurrentDirectory();
                File f = chooser.getSelectedFile();
                if (f.getName().endsWith(".xml")) {
                    OpenXMLMessageFile(f);
                } else {
                    OpenPackedMessageFile(f);
                }
            }
}//GEN-LAST:event_jMenuItemOpenMsgFileActionPerformed

private void jMenuItemHelpRcsLibraryActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemHelpRcsLibraryActionPerformed
    try {
        Desktop.getDesktop().browse(new URI("https://www.nist.gov/el/intelligent-systems-division-73500/networked-control-systems-group/real-time-control-systems"));
    } catch (Exception exception) {
        exception.printStackTrace();
    }
}//GEN-LAST:event_jMenuItemHelpRcsLibraryActionPerformed

private void jDialogConnectingWindowClosed(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_jDialogConnectingWindowClosed
    CancelConnectToAll();
}//GEN-LAST:event_jDialogConnectingWindowClosed

private void jDialogConnectingWindowClosing(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_jDialogConnectingWindowClosing
    CancelConnectToAll();
}//GEN-LAST:event_jDialogConnectingWindowClosing

private void jMenuItemFileEditDiagCfgActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemFileEditDiagCfgActionPerformed
    try {
        File f = new File(this.last_hierarchy_loaded);
        Desktop.getDesktop().open(f);
    } catch (Exception exception) {
        PrintException(exception);
    }
}//GEN-LAST:event_jMenuItemFileEditDiagCfgActionPerformed

private void jMenuItemFileEditNmlActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemFileEditNmlActionPerformed
    try {
        File f = new File(this.curModule.NMLConfigurationFile);
        final Desktop d = Desktop.getDesktop();
        if (!Desktop.isDesktopSupported() || d == null) {
            PrintError("Desktop class not supported on this platform.");
        } else {
            d.open(f);
        }
    } catch (Exception exception) {
        PrintException(exception);
    }
}//GEN-LAST:event_jMenuItemFileEditNmlActionPerformed

private void jButtonSkipOneActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSkipOneActionPerformed
    ConnectToAllSkipOne();
}//GEN-LAST:event_jButtonSkipOneActionPerformed

private void jCheckBoxMenuItemAutoSavePlotSetsActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxMenuItemAutoSavePlotSetsActionPerformed
    this.setAutomatically_Keep_and_Use_PlotSets(this.jCheckBoxMenuItemAutoSavePlotSets.isSelected());
}//GEN-LAST:event_jCheckBoxMenuItemAutoSavePlotSetsActionPerformed

private void jMenuItemPrintHierarchyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jMenuItemPrintHierarchyActionPerformed
    boolean was_running = this.javax_swing_timer != null && this.javax_swing_timer.isRunning();
    try {
        if (was_running) {
            this.javax_swing_timer.stop();
        }
        this.hierarchyDraw.print();
    } catch (Exception e) {
        e.printStackTrace();
    } finally {
        if (was_running) {
            this.javax_swing_timer.restart();
        }
    }
}//GEN-LAST:event_jMenuItemPrintHierarchyActionPerformed
    private static diag_NB_UI main_diag_NB_UI = null;

//	private static void SetupSigHandler()
//	{
//		try {
//			Class sigClass = Class.forName("sun.misc.Signal");
//			System.out.println("sigClass = " + sigClass);
//			Method sig_methods[]= sigClass.getDeclaredMethods();
//			System.out.println("sig_methods = " + sig_methods);
//			for(Method m: sig_methods)
//			{
//				System.out.println("m = " + m);
//			}
//			Class sigHandlerClass = Class.forName("sun.misc.SignalHandler");
//			Method sig_handle_methods[]= sigHandlerClass.getDeclaredMethods();
//			System.out.println("sig_handle_methods = " + sig_handle_methods);
//			for(Method m: sig_handle_methods)
//			{
//				System.out.println("m = " + m);
//			}
//			Constructor sigConstructor = sigClass.getConstructor(String.class);
//			System.out.println("sigConstructor = " + sigConstructor);
//			Object sigIntObj = sigConstructor.newInstance("INT");
//			System.out.println("sigIntObj = " + sigIntObj);
//		} catch (Exception exception) {
//			exception.printStackTrace();
//		}
//	}
    static private void SetPreferredLookAndFeel() {
        try {
            String pref_laf_string = null;
            File pref_laf = new File(System.getProperty("user.home"), ".java_diag_preferred_laf");
            if (pref_laf.exists() && pref_laf.canRead()) {
                BufferedReader br = new BufferedReader(new FileReader(pref_laf));
                pref_laf_string = br.readLine().trim();
                br.close();
                try {
                    if (null != pref_laf_string && pref_laf_string.length() > 1) {
                        UIManager.setLookAndFeel(pref_laf_string);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                    try {
                        pref_laf.delete();
                    } catch (Exception e2) {
                        e2.printStackTrace();
                    }
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                }
            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }

    /**
     * Main function --normally opens a new Diagnostics Window
 run with --help to see available command lineNumber options.
     * @param _args command lineNumber arguments
     */
    public static void main(String _args[]) {

        NMLConnection.setDefault_poll_state(true);
//		SetupSigHandler();
        main_diag_NB_UI = null;
        diag_NB_UI.args = _args;
        SetPreferredLookAndFeel();


        java.awt.EventQueue.invokeLater(new Runnable() {

            @Override
            public void run() {
                try {
                    main_diag_NB_UI = new diag_NB_UI();
                    main_diag_NB_UI.setVisible(true);
                } catch (Exception ex) {
                    ex.printStackTrace();
                }

            }
        });
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupAuxCmdStatNormal;
    private javax.swing.JButton jButtonAdjustCmdToSend;
    private javax.swing.JButton jButtonAdjusterClose;
    private javax.swing.JButton jButtonCancel;
    private javax.swing.JButton jButtonPlotAuxArray;
    private javax.swing.JButton jButtonPlotAuxVar;
    private javax.swing.JButton jButtonPlotCmdArray;
    private javax.swing.JButton jButtonPlotCmdVar;
    private javax.swing.JButton jButtonPlotStatusArray;
    private javax.swing.JButton jButtonPlotStatusVar;
    private javax.swing.JButton jButtonSendAuxMsg;
    private javax.swing.JButton jButtonSendDetailsCommand;
    private javax.swing.JButton jButtonSkipOne;
    private javax.swing.JCheckBox jCheckBoxAuxChannelConnected;
    private javax.swing.JCheckBox jCheckBoxDetailsModuleConnected;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemAutoConnectDisconnectAsNeeded;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemAutoSavePlotSets;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemConnected;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemFullscreen;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemShowAdjusterWindow;
    private javax.swing.JCheckBox jCheckBoxModifyStatus;
    private javax.swing.JCheckBox jCheckBoxSendEachAdjustmentChange;
    private javax.swing.JComboBox jComboBoxSelectAdjustVar;
    private javax.swing.JDialog jDialogConnecting;
    private javax.swing.JFormattedTextField jFormattedTextFieldAdjusterValue;
    private javax.swing.JFrame jFrameAdjustVar;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel11;
    private javax.swing.JLabel jLabel12;
    private javax.swing.JLabel jLabel13;
    private javax.swing.JLabel jLabel15;
    private javax.swing.JLabel jLabel16;
    private javax.swing.JLabel jLabel17;
    private javax.swing.JLabel jLabel18;
    private javax.swing.JLabel jLabel19;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel20;
    private javax.swing.JLabel jLabel21;
    private javax.swing.JLabel jLabel24;
    private javax.swing.JLabel jLabel25;
    private javax.swing.JLabel jLabel26;
    private javax.swing.JLabel jLabel27;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLabel jLabelAuxMsgToSendType;
    private javax.swing.JLabel jLabelAuxRecvd;
    private javax.swing.JLabel jLabelAuxType;
    private javax.swing.JLabel jLabelCmdsRecvd;
    private javax.swing.JLabel jLabelCurBuffer;
    private javax.swing.JLabel jLabelNMLBufferLine;
    private javax.swing.JLabel jLabelNMLConfigFile;
    private javax.swing.JLabel jLabelNMLNumConnectedOfTotal;
    private javax.swing.JLabel jLabelNumBuffers;
    private javax.swing.JLabel jLabelNumConnected;
    private javax.swing.JLabel jLabelNumFailed;
    private javax.swing.JLabel jLabelNumTried;
    private javax.swing.JLabel jLabelStatusRecvd;
    private javax.swing.JList jListAuxChannels;
    private javax.swing.JList jListAuxMessagesAvailable;
    private javax.swing.JList jListCmdsAvailable;
    private javax.swing.JList jListModules;
    private javax.swing.JMenuBar jMenuBarMain;
    private javax.swing.JMenu jMenuConnections;
    private javax.swing.JMenu jMenuConnectionsRecentNmlFiles;
    private javax.swing.JMenu jMenuExtras;
    private javax.swing.JMenu jMenuFile;
    private javax.swing.JMenu jMenuFileRecent;
    private javax.swing.JMenu jMenuHelp;
    private javax.swing.JMenuItem jMenuItemDumpStruct;
    private javax.swing.JMenuItem jMenuItemFileDebugDump;
    private javax.swing.JMenuItem jMenuItemFileEditDiagCfg;
    private javax.swing.JMenuItem jMenuItemFileEditNml;
    private javax.swing.JMenuItem jMenuItemFileExit;
    private javax.swing.JMenuItem jMenuItemFileOpen;
    private javax.swing.JMenuItem jMenuItemHelpRcsLibrary;
    private javax.swing.JMenuItem jMenuItemOpenInputHeader;
    private javax.swing.JMenuItem jMenuItemOpenMsgFile;
    private javax.swing.JMenuItem jMenuItemOpenNmlCfgsvrConnection;
    private javax.swing.JMenuItem jMenuItemOpenNmlConfigFile;
    private javax.swing.JMenuItem jMenuItemPlottingOpenPlotSet;
    private javax.swing.JMenuItem jMenuItemPlottingSavePlotSet;
    private javax.swing.JMenuItem jMenuItemPrintHierarchy;
    private javax.swing.JMenu jMenuLookAndFeel;
    private javax.swing.JMenu jMenuPlotting;
    private javax.swing.JMenu jMenuPlottingRecentPlotSets;
    private javax.swing.JMenu jMenuView;
    private diagapplet.JPaintablePanel jPaintablePanelHierarchyInner;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanelAuxChannels;
    private javax.swing.JPanel jPanelDetails;
    private javax.swing.JPanel jPanelErrorPanel;
    private javax.swing.JPanel jPanelHierarchyOuter;
    private javax.swing.JPanel jPanelNML;
    private javax.swing.JProgressBar jProgressBarConnectToAll;
    private javax.swing.JProgressBar jProgressBarConnectToAllConnected;
    private javax.swing.JProgressBar jProgressBarConnectToAllFailed;
    private javax.swing.JRadioButton jRadioButtonAuxCmd;
    private javax.swing.JRadioButton jRadioButtonAuxNormal;
    private javax.swing.JRadioButton jRadioButtonAuxStat;
    private javax.swing.JScrollBar jScrollBarAdjustVar;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JScrollPane jScrollPane4;
    private javax.swing.JScrollPane jScrollPane5;
    private javax.swing.JScrollPane jScrollPane6;
    private javax.swing.JScrollPane jScrollPane7;
    private javax.swing.JScrollPane jScrollPaneDetailsText;
    private javax.swing.JScrollPane jScrollPaneHierarchy;
    private javax.swing.JScrollPane jScrollPaneModulesList;
    private javax.swing.JTabbedPane jTabbedPane1;
    private javax.swing.JTable jTableNML;
    private javax.swing.JTextArea jTextAreaErrs;
    private javax.swing.JTextField jTextField3;
    private javax.swing.JTextField jTextFieldAdjustVarIncrement;
    private javax.swing.JTextField jTextFieldAdjustVarMax;
    private javax.swing.JTextField jTextFieldAdjustVarMin;
    private javax.swing.JTextField jTextFieldCmdToSendMsgType;
    private javax.swing.JTextField jTextFieldCurrentCommandMsgType;
    private javax.swing.JTextField jTextFieldCurrentStatusType;
    private javax.swing.JTextPane jTextPaneAuxBottom;
    private javax.swing.JTextPane jTextPaneDetailsBottom;
    private javax.swing.JTextPane jTextPaneNmlError;
    private diagapplet.plotter.plotter_NB_UI plotter_NB_UI1;
    private diagapplet.utils.WatchJPanel watchJPanelAuxMsgToSend;
    private diagapplet.utils.WatchJPanel watchJPanelAuxView;
    private diagapplet.utils.WatchJPanel watchJPanelCmdToSend;
    private diagapplet.utils.WatchJPanel watchJPanelDetailsCmd;
    private diagapplet.utils.WatchJPanel watchJPanelDetailsStatus;
    // End of variables declaration//GEN-END:variables
}

class diag_NB_UI_ConnectToAll_Status {

    String bufName;
    int total;
    int failed;
    int connected;
    int tried;
    int progress;

    @Override
    public String toString() {
        String s = "ConnectToAll status: bufName=" + bufName + ", total=" + total + ", failed=" + failed + ", connected=" + connected + ", tried=" + tried + ", progress=" + progress;
        return s;
    }
}
