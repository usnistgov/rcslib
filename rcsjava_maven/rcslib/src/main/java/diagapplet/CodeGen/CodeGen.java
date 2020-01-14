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



 *///******************************************************************************
// CodeGen.java:        Applet
//
//******************************************************************************
package diagapplet.CodeGen;

import java.awt.Button;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.Insets;
import java.awt.Frame;
import java.awt.Checkbox;
import java.awt.Label;
import java.awt.Graphics;
import java.awt.FileDialog;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Properties;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.io.FileOutputStream;
import diagapplet.utils.FastListPanelInterface;
import diagapplet.utils.FastListPanel;
import diagapplet.utils.FastListContainer;
import diagapplet.utils.CountButton;
import diagapplet.utils.ModifiedFileDialog;
import diagapplet.utils.URLLoadInfoPanel;
import java.awt.Container;

import rcs.utils.URL_and_FileLoader;

/**
 * Main Class for applet CodeGen only needed for Graphical CodeGen and RCS Design Tool.
 * not needed for newer recommended CodeGenCmdLine
 * @author shackle
 */
public class CodeGen extends Container implements Runnable, ActionListener {

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613915L;
    static private final diagapplet.CodeGen.StringFuncs sf = new diagapplet.CodeGen.StringFuncs();
    private CodeGenCommonInterface2 cgc = null;
    private GridBagLayout Layout = null;
    private CountButton m_browseLocalButton = null;
    private Button SaveFileButton = null;
    private Button GenerateJavaClassdefButton = null;
    private Button GenerateJavaMessageDictButton = null;
    private Button GenerateCppUpdateFunctionButton = null;
    private Button GenerateCppFormatFunctionButton = null;
    private Button GenerateCppConstructorsButton = null;
    private Checkbox DebugCheckbox = null;
    private Label LabelA = null;
    private Label LabelB = null;
    private CodeGenTextAreaWrapper codeTextArea = null;
    private CodeGenTextFieldWrapper configFileTextField = null;
    private FastListPanel ClassList = null;
    private FastListContainer ClassListContainer = null;
    private Label includePathLabel = null;
    private CodeGenTextFieldWrapper includePathField = null;
    private Button ClearButton = null;
    private CountButton m_hierarchyFileLoadButton = null;
    private boolean repaint_needed = true;
    private boolean ready_to_load = false;
    /**
     * Set to true to have many fuctions print degub info to System.out.
     */
    static public boolean debug_on = false;
    private boolean m_fStandAlone = false;

    /**
     * Set the m_fStandAlone property. When the property is true the gui will not attempt
     * to resize itself or parse applet options.
     * Used only by the Design tool.
     * @param b whether to assume standalone mode
     */
    public void set_m_fStandAlone(boolean b) {
        m_fStandAlone = b;
    }    // THREAD SUPPORT:
    //            m_NGIS_GUI      is the Thread object for the applet
    //--------------------------------------------------------------------------
    private volatile Thread m_CodeGen = null;    // Parameter names.  To change a name of a parameter, you need only make
    // a single change.  Simply modify the value of the parameter string below.
    //--------------------------------------------------------------------------
    private final static String PARAM_HierarchyFile = "HierarchyFile";
    private final static String PARAM_ConfigFile = "ConfigFile";
    private final static String PARAM_HeaderFile = "HeaderFile";
    private final static String PARAM_HFile = "HFile";
    private final static String PARAM_HHFile = "HHFile";
    private final static String PARAM_ScriptFile = "script";
    private final static String PARAM_DebugOn = "debug_on";
    private final static String PARAM_UpdateWithName = "update_with_name";
    private final static String PARAM_DlaLengthInit = "dla_length_init";
    private final static String PARAM_DisplayOn = "display_on";
    private final static String PARAM_UseDefaultTypes = "UseDefaultTypes";
    private static long last_bell_millis = 0;

    /**
     * Ring a bell to indicate an error if it has been more than 2.5 seconds since the 
     * last error. 
     * (Time limit prevents long list of errors from being really annoying.)
     */
    public void RingBell() {
        try {
            if (Math.abs(last_bell_millis - System.currentTimeMillis()) > 2500) {
                Toolkit tk = Toolkit.getDefaultToolkit();
                tk.beep();
                last_bell_millis = System.currentTimeMillis();
            }
        } catch (Throwable t) {
        }
    }

    /***
     * From old VisualJ STANDALONE APPLICATION SUPPORT
     *    The GetParameter() method is a replacement for the getParameter() method
     * defined by Applet. This method returns the value of the specified parameter;
     * unlike the original getParameter() method, this method works when the applet
     * is run as a standalone application, as well as when run within an HTML page.
     * This method is called by GetParameters().
     *---------------------------------------------------------------------------
     * @param strName name of parameter to retrieve
     * @param args  command lineNumber args if run from main()
     * @return value corresponding to strName
     */
    public String GetParameter(String strName, String args[]) {
        return cgc.GetParameter(strName, args);
    }

    /***
     * From old VisualJ STANDALONE APPLICATION SUPPORT
     *    The GetParameters() method retrieves the values of each of the applet's
     * parameters and stores them in variables. This method works both when the
 applet is run as a standalone application and when it's run within an HTML
 page.  When the applet is run as a standalone application, this method is
 called by the main() method, which passes it the command-lineNumber arguments.
 When the applet is run within an HTML page, this method is called by the
 init() method with args == null.
---------------------------------------------------------------------------
     * @param args  command lineNumber args if called from main()
     */
    public void GetParameters(String args[]) {
        cgc.GetParameters(args);
    }
    static private String orig_args[];
    // Create Toplevel Window to contain applet CodeGen
    //----------------------------------------------------------------------
    static private CodeGenFrame frame;
    static private CodeGen applet_CodeGen;    // The following code starts the applet running within the frame window.
    static private DiagNMLMsgDictCreator codegen_diag_dict_creator = new DiagNMLMsgDictCreator();

    /**
     * Run the graphical tool.
     * @deprecated Use CodeGenCmdLine instead.
     * @param args cmd lineNumber arguments
     */
    public static void main(String args[]) {
        boolean print_prompt = true;
        String includePath = "";

        for (int i = 0; i < args.length; i++) {
            if (args[i].startsWith("-I") && args[i].length() > 2) {
                URL_and_FileLoader.AddToSearchPath(args[i].substring(2));
                includePath += args[i].substring(2) + ";";
                continue;
            }
            if (args[i].indexOf('=') < 0) {
                if (args[i].endsWith(".gen")) {
                    args[i] = "script=" + args[i];
                } else {
                    args[i] = "HHFile=" + args[i];
                }
            }
            if (args[i].equals("debug_on=true")) {
                debug_on = true;
            }
            if (args[i].equals("noprompt")) {
                print_prompt = false;
            }
        }

        orig_args = args;

        if (debug_on) {
            System.out.println("applet_CodeGen = new CodeGen();");
        }
        applet_CodeGen = new CodeGen();

        applet_CodeGen.m_fStandAlone = true;

        if (debug_on) {
            System.out.println("applet_CodeGen.GetParameters(args);");
        }
        applet_CodeGen.GetParameters(args);

        if (debug_on) {
            System.out.println("applet_CodeGen.cgc.get_script_file() = " + applet_CodeGen.cgc.get_script());
        }
        applet_CodeGen.cgc.set_diag_dict_creator(codegen_diag_dict_creator);
        applet_CodeGen.cgc.set_nml_creator(rcs.nml.NMLConnection.Creator);
        applet_CodeGen.cgc.set_print_prompt(print_prompt);
        if (null == applet_CodeGen.cgc.get_ClassList()) {
            if (debug_on) {
                System.out.println("	applet_CodeGen.cgc.set_ClassList( new diagapplet.utils.FakeFastListPanel());");
            }
            applet_CodeGen.cgc.set_ClassList(new diagapplet.utils.FakeFastListPanel());
        }
        if (!applet_CodeGen.cgc.get_display_on() && applet_CodeGen.cgc.get_script() == null) {
            applet_CodeGen.cgc.set_script(applet_CodeGen.cgc.createScript(args));
        }
        if (debug_on) {
            System.out.println("applet_CodeGen.cgc.get_script_file() = " + applet_CodeGen.cgc.get_script());
        }

        if (applet_CodeGen.cgc.get_script() != null) {
            if (applet_CodeGen.cgc.get_script().length() < 1 || applet_CodeGen.cgc.get_display_on()) {
                ShowCodeGenStandalone();
            } else {
                applet_CodeGen.init();
            }
        } else {
            ShowCodeGenStandalone();
        }
        applet_CodeGen.cgc.set_RunIndependantly(true);
        if (debug_on) {
            System.out.println("applet_CodeGen.start();");
        }
//        applet_CodeGen.start();
    }

    private static void ShowCodeGenStandalone() {
        try {
            // Must show Frame before we size it so insets() will return valid values
            //----------------------------------------------------------------------
            if (debug_on) {
                System.out.println("frame = new CodeGenFrame(\"CodeGen(" + rcs.RCS_VERSION.version_string + ")\");");
            }
            frame = new CodeGenFrame("CodeGen(" + rcs.RCS_VERSION.version_string + ")");

            if (debug_on) {
                System.out.println("frame.setVisible(true);");
            }
            frame.setVisible(true);
            Insets frame_insets = frame.getInsets();

            if (debug_on) {
                System.out.println("frame.setSize(" + (frame_insets.left + frame_insets.right + 1000) + ", " + (frame_insets.top + frame_insets.bottom + 400) + "););");
            }
            frame.setSize(frame_insets.left + frame_insets.right + 1000,
                    frame_insets.top + frame_insets.bottom + 400);

            if (debug_on) {
                System.out.println("frame.add(\"Center\", applet_CodeGen);");
            }
            frame.add("Center", applet_CodeGen);

            if (debug_on) {
                System.out.println("applet_CodeGen.init();");
            }
            applet_CodeGen.init();

            if (debug_on) {
                System.out.println("frame.setVisible(true);");
            }
            frame.setVisible(true);

            if (debug_on) {
                System.out.println("frame.pack();");
            }
            frame.pack();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void SaveGeneratedCode() {
        try {
            Frame dummy_frame = new Frame();
            ModifiedFileDialog file_dialog = new ModifiedFileDialog(dummy_frame, "Save Generated Code As", FileDialog.SAVE);

            try {
                Properties props = System.getProperties();
                String cur_dir = props.getProperty("user.dir");
                if (null != cur_dir) {
                    file_dialog.setDirectory(cur_dir);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

            if (!cgc.get_first_java_class()) {
                file_dialog.setFile(cgc.get_javaFileName());
            } else if (!cgc.get_first_cpp_function()) {
                file_dialog.setFile(cgc.get_cppFileName());
            }
            file_dialog.setVisible(true);
            file_dialog.wait_for_done();
            //String file_name = file_dialog.getFile();
            //String dir_name = file_dialog.getDirectory();
            String file_name = file_dialog.filename;
            FileOutputStream fos = new FileOutputStream(file_name);
            String generated_string = cgc.get_codeTextArea().getText();
            if (debug_on) {
                System.out.println("Saving Generated code of " + generated_string.length() + " bytes to " + file_name + ".\n");
            }
            byte b[] = generated_string.getBytes();
            fos.write(b, 0, generated_string.length());
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    /**
     * Used by RCS Design tool. Initializes the class list.
     * @throws java.lang.Exception when class list can not be initialized
     */
    public void InitializeClassList() throws Exception {
        cgc.InitializeClassList();
    }

    private void OpenFileDialog() {
        try {
            Frame tempframe = new Frame();
            ModifiedFileDialog open_file_dialog = new ModifiedFileDialog(tempframe, "Open Architecture File", FileDialog.LOAD);
            open_file_dialog.setVisible(true);
            open_file_dialog.wait_for_done();
            String file_name = open_file_dialog.filename;
            if (file_name == null) {
                System.out.println("Open file canceled.");
                return;
            }
            if (file_name.length() < 1) {
                System.out.println("Open file canceled.");
                return;
            }
            //file_name = open_file_dialog.getDirectory() + open_file_dialog.getFile();
            System.out.println("Reading " + file_name + " . . .");
            open_file_dialog.dispose();
            if (null == file_name) {
                return;
            }
            cgc.set_m_hierarchyFile(file_name);
            cgc.set_m_ConfigFile(file_name);
            cgc.get_configFileTextField().setText(file_name);
            cgc.get_configFileTextField().repaint();
            cgc.set_reload_hierarchy_needed(true);
            ModuleInfo.ClearStaticData();
            if (null != cgc.get_codeTextArea()) {
                cgc.get_codeTextArea().setText("");
            }
            if (null != cgc.get_ClassList()) {
                cgc.get_ClassList().removeAll();
            }
            ResetGenericClasses();
            NewFunction();
            cgc.set_reload_hierarchy_needed(true);
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return;
    }

    public void actionPerformed(ActionEvent evt) {
        if (cgc.get_running()) {
            return;
        }
        repaint_needed = true;
        if (evt.getSource() == (SaveFileButton)) {
            synchronized (this) {
                SaveGeneratedCode();
            }
        }
        if (evt.getSource() == (m_browseLocalButton)) {
            synchronized (this) {
                OpenFileDialog();
            }
        }
        if (evt.getSource() == GenerateJavaClassdefButton) {
            synchronized (this) {
                cgc.set_generate_java_classes_needed(true);
                NewFunction();
            }
            return;
        }

        if (evt.getSource() == GenerateJavaMessageDictButton) {
            synchronized (this) {
                cgc.set_generate_java_dictionary_needed(true);
                NewFunction();
                return;
            }
        }
        if (evt.getSource() == (GenerateCppUpdateFunctionButton)) {
            synchronized (this) {
                cgc.set_generate_cpp_update_functions_needed(true);
                NewFunction();
                return;
            }
        }
        if (evt.getSource() == (GenerateCppFormatFunctionButton)) {
            synchronized (this) {
                cgc.set_generate_cpp_format_function_needed(true);
                NewFunction();
                return;
            }
        }
        if (evt.getSource() == (GenerateCppConstructorsButton)) {
            synchronized (this) {
                cgc.set_generate_cpp_constructors_needed(true);
                NewFunction();
                return;
            }
        }
        if (evt.getSource() == (DebugCheckbox)) {
            synchronized (this) {
                debug_on = DebugCheckbox.getState();
                cgc.set_debug_on(debug_on);
                ModuleInfo.debug_on = debug_on;
                C_Generator.debug_on = debug_on;
                Ada_Generator.debug_on = debug_on;
                URL_and_FileLoader.debug_on = debug_on;
            }
        }
        if (evt.getSource() == (ClearButton)) {
            synchronized (this) {
                cgc.ClearAll();
            }
        }
        if (evt.getSource() == (m_hierarchyFileLoadButton)) {
            synchronized (this) {
                if (debug_on) {
                    System.out.println("CodeGen.m_hierarchyFileLoadButton pressed.");
                }
                if (null != configFileTextField) {
                    cgc.set_m_hierarchyFile(configFileTextField.getText());
                    cgc.set_m_ConfigFile(cgc.get_m_hierarchyFile());
                }
                if (cgc.get_is_loading_hierarchy() || cgc.get_generating_code() || cgc.get_running_script()) {
                    if (debug_on) {
                        System.err.println("Interrupted File Load.");
                    }
                    if (null != m_CodeGen) {
                        //m_CodeGen.stop();
                        m_CodeGen = null;
                    }
                    cgc.set_is_loading_hierarchy(false);
                    cgc.set_generating_code(false);
                    cgc.set_running_script(false);
                    cgc.set_generate_java_classes_needed(false);
                    cgc.set_generate_java_dictionary_needed(false);
                    cgc.set_generate_cpp_update_functions_needed(false);
                    cgc.set_generate_cpp_format_function_needed(false);
                    cgc.set_generate_cpp_constructors_needed(false);
                    m_hierarchyFileLoadButton.setBackground(Color.white);
                    m_hierarchyFileLoadButton.setLabel("LOAD");
                    repaint();
                    m_hierarchyFileLoadButton.repaint();
                } else {
                    cgc.set_reload_hierarchy_needed(true);
                    cgc.set_force_reload_file(true);
                    ModuleInfo.ClearStaticData();
                    if (null != cgc.get_codeTextArea()) {
                        cgc.get_codeTextArea().setText("");
                    }
                    if (null != cgc.get_ClassList()) {
                        cgc.get_ClassList().removeAll();
                    }
                    cgc.ResetGenericClasses();
                    NewFunction();
                }
            }
        }
        if (evt.getSource() == (configFileTextField)) {
            synchronized (this) {
                cgc.set_m_hierarchyFile(configFileTextField.getText());
                cgc.set_m_ConfigFile(cgc.get_m_hierarchyFile());
                if (!cgc.get_is_loading_hierarchy()) {
                    cgc.set_reload_hierarchy_needed(true);
                }
                ModuleInfo.ClearStaticData();
                if (null != cgc.get_codeTextArea()) {
                    cgc.get_codeTextArea().setText("");
                }
                if (null != cgc.get_ClassList()) {
                    cgc.get_ClassList().removeAll();
                }
                cgc.ResetGenericClasses();
                NewFunction();
            }
        }
    }

    // CodeGen Class Constructor
    //--------------------------------------------------------------------------
    public CodeGen() {
        cgc = new CodeGenCommon();
        cgc.set_diag_dict_creator(codegen_diag_dict_creator);
        cgc.set_nml_creator(rcs.nml.NMLConnection.Creator);
        ResetGenericClasses();
    }

    private void ResetGenericClasses() {
        cgc.ResetGenericClasses();
    }

    /***
     * 
     * Old VisualJ APPLET INFO SUPPORT:
     *            The getAppletInfo() method returns a string describing the applet's
     * author, copyright date, or miscellaneous information.
     *--------------------------------------------------------------------------
     */
    public String getAppletInfo() {
        return "Name: CodeGen\r\n" +
                "Author: Will Shackleford\r\n" +
                "Created with Microsoft Visual J++ Version 1.0\r\n" +
                "";
    }

    /*** 
     * 
     * Old VisualJ PARAMETER SUPPORT
     *            The getParameterInfo() method returns an array of strings describing
     * the parameters understood by this applet.
     *
     * CodeGen Parameter Information:
     *  { "Name", "Type", "Description" },
     *--------------------------------------------------------------------------
     */
    public String[][] getParameterInfo() {
        String[][] info = {
            {PARAM_ConfigFile, "String", "The URL or name of the Configuration file which is parsed to determine which header files to read, which modules are children of other modules, etc."},
            {PARAM_DebugOn, "boolean", "Should additional debug messages be printed to the JavaConsole or stdout?"},
            {PARAM_DisplayOn, "boolean", "If started from the command line, should the graphical interface be shown?"},
            {PARAM_UpdateWithName, "boolean", "Should we use the newer update functions that include a name argument."},
            {PARAM_DlaLengthInit, "int", "Integer to set the initial length of all dynamic length arrays to"}
        };
        return info;
    }

    
    public void init() {
        if (!m_fStandAlone && !cgc.get_inside_diagapplet()) {
            GetParameters(null);        // If you use a ResourceWizard-generated "control creator" class to
        // arrange controls in your applet, you may want to call its
        // CreateControls() method from within this method. Remove the following
        // call to resize() before adding the call to CreateControls();
        // CreateControls() does its own resizing.
        //----------------------------------------------------------------------
        }
        if (cgc.get_display_on()) {

            if (!m_fStandAlone) {
                setSize(1000, 400);
            }
            if (null == cgc.get_m_modulesHashTable()) {
                cgc.set_m_modulesHashTable(new Hashtable());
            }
            GridBagConstraints c = new GridBagConstraints();
            c.gridy = 0;
            Layout = new GridBagLayout();
            setLayout(Layout);
            if (!cgc.get_inside_diagapplet()) {
                configFileTextField = new CodeGenTextFieldWrapper(cgc.get_m_ConfigFile());
                cgc.set_configFileTextField(configFileTextField);
                add(configFileTextField);
                c.gridx = 0;
                c.gridwidth = 5;
                c.fill = GridBagConstraints.HORIZONTAL;
                Layout.setConstraints(configFileTextField, c);
                configFileTextField.addActionListener(this);
                m_hierarchyFileLoadButton = new CountButton("LOAD");
                cgc.set_m_hierarchyFileLoadButton(m_hierarchyFileLoadButton);
                add(m_hierarchyFileLoadButton);
                c.gridx = 5;
                c.gridwidth = 1;
                c.fill = GridBagConstraints.HORIZONTAL;
                Layout.setConstraints(m_hierarchyFileLoadButton, c);
                m_hierarchyFileLoadButton.addActionListener(this);
                m_browseLocalButton = new CountButton("BROWSE");
                add(m_browseLocalButton);
                c.gridx = GridBagConstraints.RELATIVE;
                c.gridwidth = GridBagConstraints.REMAINDER;
                c.fill = GridBagConstraints.HORIZONTAL;
                Layout.setConstraints(m_browseLocalButton, c);
                m_browseLocalButton.addActionListener(this);

                // New Line
                c.gridy++;

                // Percent Loaded Bar
                cgc.set_m_loadingPanel(new URLLoadInfoPanel(600, 50));
                add((URLLoadInfoPanel) cgc.get_m_loadingPanel());
                c.gridx = 0;
                c.gridwidth = 6;
                c.fill = GridBagConstraints.BOTH;
                Layout.setConstraints((URLLoadInfoPanel) cgc.get_m_loadingPanel(), c);

                ModuleInfo.debug_on = debug_on;
                DebugCheckbox = new Checkbox("Debug", null, debug_on);
                add(DebugCheckbox);
                c.gridx = 6;
                c.gridwidth = 1;
                c.fill = GridBagConstraints.BOTH;
                Layout.setConstraints(DebugCheckbox, c);

                // New Line
                c.gridy++;

            }
            SaveFileButton = new Button("Save");
            add(SaveFileButton);
            c.gridx = 0;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(SaveFileButton, c);
            SaveFileButton.addActionListener(this);
            ClearButton = new Button("Clear");
            add(ClearButton);
            c.gridx = 1;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(ClearButton, c);
            ClearButton.addActionListener(this);
            GenerateJavaClassdefButton = new Button("Add Java Class(es)");
            add(GenerateJavaClassdefButton);
            c.gridx = 2;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(GenerateJavaClassdefButton, c);
            GenerateJavaClassdefButton.addActionListener(this);
            GenerateJavaMessageDictButton = new Button("Add Java Message Dictionary");
            add(GenerateJavaMessageDictButton);
            c.gridx = 3;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(GenerateJavaMessageDictButton, c);
            GenerateJavaMessageDictButton.addActionListener(this);
            GenerateCppUpdateFunctionButton = new Button("Add C++ Update Function(s)");
            GenerateCppUpdateFunctionButton.addActionListener(this);
            add(GenerateCppUpdateFunctionButton);
            c.gridx = 4;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(GenerateCppUpdateFunctionButton, c);
            GenerateCppUpdateFunctionButton.addActionListener(this);
            GenerateCppFormatFunctionButton = new Button("Add C++ Format Function");
            add(GenerateCppFormatFunctionButton);
            c.gridx = 5;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(GenerateCppFormatFunctionButton, c);
            GenerateCppFormatFunctionButton.addActionListener(this);
            GenerateCppConstructorsButton = new Button("Add C++ Ctors/Dtors");
            add(GenerateCppConstructorsButton);
            c.gridx = 6;
            c.gridwidth = 1;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(GenerateCppConstructorsButton, c);
            GenerateCppConstructorsButton.addActionListener(this);


            c.gridy++;
            includePathLabel = new Label("Include Path");
            add(includePathLabel);
            c.gridx = 0;
            c.gridwidth = 4;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(includePathLabel, c);

            c.gridy++;
            includePathField = new CodeGenTextFieldWrapper(cgc.get_includePath());
            cgc.set_includePathField(includePathField);
            add(includePathField);
            c.gridx = 0;
            c.gridwidth = 4;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(includePathField, c);


            // New Line
            c.gridy++;

            LabelA = new Label("Available Classes");
            add(LabelA);
            c.gridx = 0;
            c.gridwidth = 4;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(LabelA, c);
            LabelB = new Label("Generated Code");
            add(LabelB);
            c.gridx = 4;
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(LabelB, c);

            // New Line
            c.gridy++;

            ClassList = new FastListPanel(15, 40, true, this);
            for (int jj = 0; jj < 25; jj++) {
                ClassList.add("ZZZZZZZZZZZZZZZZZZZZZZ");
            }
            ClassListContainer = new FastListContainer(ClassList);
            add(ClassListContainer);
            c.gridx = 0;
            c.gridwidth = 4;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(ClassListContainer, c);
            // ClassList.addListener(this); not necessary.
            codeTextArea = new CodeGenTextAreaWrapper(15, 40);
            cgc.set_codeTextArea(codeTextArea);
            add(codeTextArea);
            c.gridx = 4;
            c.gridwidth = GridBagConstraints.REMAINDER;
            c.fill = GridBagConstraints.BOTH;
            Layout.setConstraints(codeTextArea, c);


            repaint_needed = true;
        } else {
            ClassList = new FastListPanel(15, 40, true, this);
        }

        if (cgc.get_inside_diagapplet()) {
            cgc.set_reload_hierarchy_needed(false);
        }
        NewFunction();
    }

    /**
     * See CodeGenCommon.LoadHierarchy()
     * @throws java.lang.Exception when hiearchy can not be loaded
     */
    public void LoadHierarchy() throws Exception {
        cgc.LoadHierarchy();

    }

    // Place additional applet clean up code here.  destroy() is called when
    // when you applet is terminating and being unloaded.
    //-------------------------------------------------------------------------
    public void destroy() {
    }

    // CodeGen Paint Handler
    //--------------------------------------------------------------------------
    public void paint(Graphics g) {
        if (debug_on) {
            System.out.println("CodeGen repainted.");
        }
        repaint_needed = false;
    }
    
    /**
     * Run a .gen script file. The script file may contain a list of headers to parse in what order and
     * which update/format files functions to generate to which output files in what order.
     * @param new_script_file script file to run
     */
    public void RunScriptFile(String new_script_file) {
        cgc.RunScript(new_script_file);
    }

    private void RunScriptFile() throws Exception {
        cgc.RunScript();
    }

    private void NewFunction() {
        cgc.set_running(true);
        if (null != m_CodeGen) {
            //m_CodeGen.stop();
            m_CodeGen = null;
            System.runFinalization();
        }
        m_CodeGen = new Thread(this);
        if (null != m_CodeGen) {
            m_CodeGen.start();
        }
        cgc.set_run_needed(true);
    }

    private void UpdateDisplay() {
        try {

            if (cgc.get_script() != null && !cgc.get_script_file_ran()) {
                cgc.set_running_script(true);
                RunScriptFile();
                return;
            }
            cgc.set_running_script(false);
            if (cgc.get_reload_hierarchy_needed() && (ready_to_load || isVisible()) && !cgc.get_inside_diagapplet()) {
                if (null != cgc.get_configFileTextField()) {
                    cgc.set_m_ConfigFile(cgc.get_configFileTextField().getText());
                    if (null != cgc.get_m_ConfigFile()) {
                        if (cgc.get_m_ConfigFile().length() > 0) {
                            cgc.set_m_hierarchyFile(cgc.get_m_ConfigFile());
                        }
                    }
                }
                if (null != cgc.get_includePathField()) {
                    cgc.set_includePath(cgc.get_includePathField().getText());
                }
                if (cgc.get_includePath().length() > 2 && !cgc.get_includePath().equals(cgc.get_lastIncludePath())) {
                    cgc.set_lastIncludePath(cgc.get_includePath());
                    StringTokenizer pathTokenizer =
                            new StringTokenizer(cgc.get_includePath(), "\n\r\t,;");
                    while (pathTokenizer.hasMoreTokens()) {
                        String token = pathTokenizer.nextToken();
                        URL_and_FileLoader.AddToSearchPath(token);
                    }
                }
                cgc.LoadHierarchy();
                repaint_needed = true;
            }
            if (cgc.get_generate_java_classes_needed()) {
                cgc.GenerateJavaClasses();
                repaint_needed = true;
            }
            if (cgc.get_generate_java_dictionary_needed()) {
                cgc.GenerateJavaMessageDict();
                repaint_needed = true;
            }
            if (cgc.get_generate_cpp_update_functions_needed()) {
                cgc.GenerateCppUpdateFunctions();
                repaint_needed = true;
            }
            if (cgc.get_generate_cpp_format_function_needed()) {
                cgc.GenerateCppFormatFunction();
                repaint_needed = true;
            }
            if (cgc.get_generate_cpp_constructors_needed()) {
                cgc.GenerateCppConstructors();
                repaint_needed = true;
            }
            if (repaint_needed) {
                repaint(500);
            }
            cgc.set_run_needed(
                    cgc.get_generate_java_dictionary_needed() || cgc.get_generate_java_classes_needed() || cgc.get_generate_cpp_update_functions_needed() || cgc.get_generate_cpp_format_function_needed() || cgc.get_generate_cpp_constructors_needed() || cgc.get_reload_hierarchy_needed() || (cgc.get_script() != null && !cgc.get_script_file_ran()));
        } catch (Exception e) {
            e.printStackTrace();
        }

    }
    
    private int codegen_cycles = 0;
    /***
    * Old VisualJ THREAD SUPPORT
    *            The run() method is called when the applet's thread is started. If
    * your applet performs any ongoing activities without waiting for user
    * input, the code for implementing that behavior typically goes here. For
    * example, for an applet that performs animation, the run() method controls
    * the display of images.
    *--------------------------------------------------------------------------
    */
    public void run() {
        cgc.set_running(true);
        cgc.set_run_needed(true);
        if (cgc.get_display_on() == false && cgc.get_script() != null) {
            try {
                while (!cgc.get_script_file_ran()) {
                    cgc.RunScript();
                }
            } catch (Exception exception) {
                exception.printStackTrace();
                System.exit(1);
            }
            System.exit(0);
        }
        while (cgc.get_run_needed()) {
            try {
                cgc.set_run_needed(false);
                if (debug_on) {
                    System.out.println("CodeGen cycles." + codegen_cycles);
                    codegen_cycles++;
                }
                synchronized (this) {
                    UpdateDisplay();
                }
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
//                stop();
            }
        }
        cgc.set_running(false);
    }

    /**
     * Set the preserve modules hashtable property, see CodeGenCommon.set_preserve_modules_hashtable()
     * @param b whether to preserve modules
     */
    public void set_preserve_modules_hashtable(boolean b) {
        cgc.set_preserve_modules_hashtable(b);
    }

    /**
     * Set the inside_diagapplet property, see CodeGenCommon.set_preserve_modules_hashtable()
     * @param b whether being run inside diagapplet
     */
    public void set_inside_diagapplet(boolean b) {
        cgc.set_inside_diagapplet(b);
    }
    

    /**
     * Set the hierarchyFile. Used only by Design tool.
     * @param str new hierarchy file name
     */
    public void set_m_hierarchyFile(String str) {
        cgc.set_m_hierarchyFile(str);
    }

    /**
     * Set the configFile. Used only by Design tool.
     * @param str new configuration file name
     */
    public void set_m_ConfigFile(String str) {
        cgc.set_m_ConfigFile(str);
    }

    /**
     * Set the modulesHashTable. Used only by Design tool.
     * @param ht new modules hash table
     */
    public void set_m_modulesHashTable(java.util.Hashtable ht) {
        cgc.set_m_modulesHashTable(ht);
    }

    /**
     * Set the serversHashTable. Used only by Design tool.
     * @param ht new servers hashtable
     */
    public void set_serversHashtable(java.util.Hashtable ht) {
        cgc.set_serversHashtable(ht);
    }

    /**
     * Set the loadingPanel. Used only by Design tool.
     * @param lp new loadingPanel
     */
    public void set_m_loadingPanel(diagapplet.utils.URLLoadInfoPanelInterface lp) {
        cgc.set_m_loadingPanel(lp);
    }
    
    /**
     * Set the ClassList. Used only by Design tool.
     * @param flp new fastlistpanel
     */
    public void set_ClassList(FastListPanelInterface flp) {
        cgc.set_ClassList(flp);
    }

    /**
     * Set modulesCountList. Used only by Design Tool.
     * @param lst new modules countlist
     */
    public void set_m_modulesCountList(diagapplet.utils.CountListInterface lst) {
        cgc.set_m_modulesCountList(lst);
    }
}
