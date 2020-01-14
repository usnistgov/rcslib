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
// CodeGenCommon.java:        Applet
//
//******************************************************************************
package diagapplet.CodeGen;

import java.util.Hashtable;
import java.util.Vector;
import java.util.StringTokenizer;
import java.util.Enumeration;

import java.io.FileOutputStream;
import java.io.File;

import rcs.utils.URL_and_FileLoader;
import rcs.nml.NMLConnectionCreatorInterface;

import diagapplet.utils.FastListPanelInterface;

//==============================================================================
import java.io.FileInputStream;
import java.util.ArrayList;
import rcs.utils.StackTracePrinter;

/**
 * Parses NML C++ header files and .gen files to generate C++ format and update
 * functions, C,Java, and Ada class definitions and information for the
 * Diagnostics and Design tools.
 *
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class CodeGenCommon implements CodeGenCommonInterface2 {

    private volatile boolean rcs_status_enum_needed = false;
    private volatile boolean rcs_admin_state_enum_needed = false;
    static public boolean force_keep_status_enum = false;
    static public boolean force_keep_admin_state_enum = false;
    static public boolean debug_estimated_size = false;
    static public boolean add_java_getters_and_setters = true;
    static private volatile boolean RunIndependantly = false;
    static private Hashtable optionsHashtable = null;
    static private Vector extraActionsVector = null;
    /**
     * Set the name of the only output file to generate.
     *
     * @param _output_filename The name of the only output file to generate,
     * must end in .cc or .cpp, sets to generate only C++ code.
     * @author Will Shackleford <shackle@nist.gov>
     */
    private static String forced_output_filename = null;

    public static void SetOutputFileName(String _output_filename) {
        forced_output_filename = _output_filename;
    }

    public Vector get_extraActionsVector() {
        return extraActionsVector;
    }
    static private Vector extraTabsVector = null;

    public Vector get_extraTabsVector() {
        return extraTabsVector;
    }
    private String optionsInfo = null;
    static private String includePath = "";
    static private rcs.nml.NMLConnectionCreatorInterface nml_creator = null;
    static private diagapplet.CodeGen.DiagNMLMsgDictCreatorInterface diag_dict_creator = null;
    private boolean ignore_options = false;
    static private boolean display_on = false;
    private volatile boolean generate_java_classes_needed = false;
    private volatile boolean generate_java_dictionary_needed = false;
    private volatile boolean generate_cpp_update_functions_needed = false;
    private volatile boolean generate_cpp_format_function_needed = false;
    private volatile boolean generate_cpp_constructors_needed = false;
    private volatile boolean run_needed = true;
    private volatile boolean running_script = false;
    private volatile boolean generating_code = false;
    private String java_package_name = null;
    private volatile boolean script_file_ran = false;
    private Vector generic_classes = null;
    private String script = null;
    private String output_file_name = null;
    private FileOutputStream fos = null;
    private File current_directory = null;
    private volatile boolean running = false;
    private boolean inside_diagapplet = false;
    private diagapplet.utils.FastListPanelInterface ClassList = null;
    private ModuleInfo m_currentModule = null;
    private boolean first_cpp_function = true;
    private boolean first_java_class = true;
    private boolean last_type_converted_was_class = false;
    private boolean skip_type = false;
    private String cppFileName = null;
    private String javaFileName = null;
    private Hashtable m_modulesHashTable = null;
    private Hashtable serversHashtable = null;
    private volatile boolean is_loading_hierarchy = false;
    private volatile static boolean interrupt_loading = false;
    private boolean m_hierarchyLoadedOnce = false;
    private int HTTPport = 0;
    private String m_systemHost = null;
    private diagapplet.utils.URLLoadInfoPanelInterface m_loadingPanel = null;
    private boolean reload_hierarchy_needed = true;
    private String m_hierarchyFile;
    private boolean force_reload_file = false;
    static private boolean debug_on = false;
    static private boolean UseDefaultTypes = true;
    static private boolean update_with_name = false;
    static private boolean create_print_sizes_file = false;
    static private boolean select_from_all_files = false;
    static private boolean generate_for_all_langs = false;
    static private boolean generate_for_c = false;
    static private boolean generate_for_cpp = true;
    static private boolean generate_for_java = false;
    static private boolean generate_for_ada = false;
    static private boolean generate_symbol_lookups = false;
    static private boolean generate_all_enum_symbol_lookups = false;
    static private boolean add_set_header = false;

    static public boolean get_generate_symbol_lookups() {
        return generate_symbol_lookups;
    }

    static public void set_generate_symbol_lookups(boolean _generate_symbol_lookups) {
        generate_symbol_lookups = _generate_symbol_lookups;
    }

    static public void set_generate_all_enum_symbol_lookups(boolean _generate_all_enum_symbol_lookups) {
        generate_all_enum_symbol_lookups = _generate_all_enum_symbol_lookups;
        if (generate_all_enum_symbol_lookups) {
            ModuleInfo.generate_enum_symbol_lookup = true;
            generate_symbol_lookups = true;
            no_enums = false;
            force_keep_status_enum = true;
            force_keep_admin_state_enum = true;
        }
    }

    static public boolean get_generate_all_enum_symbol_lookups() {
        return generate_all_enum_symbol_lookups;
    }

    static public void set_add_set_header(boolean _add_set_header) {
        add_set_header = _add_set_header;
    }

    static public boolean get_add_set_header() {
        return add_set_header;
    }
    static private String sizes_file_name = "print_nmlmsg_sizes.cc";
    private boolean print_script = false;
    static final private boolean prefix_nonmsg_update = false;
    static private int dla_length_init = 0;
    static private String m_ConfigFile = "default.cfg";
    private diagapplet.utils.FastListPanelInterface serversList = null;
    private diagapplet.utils.FastListPanelInterface m_modulesList = null;
    private diagapplet.utils.CountListInterface m_modulesCountList = null;
    static private CodeGenTextFieldInterface includePathField = null;
    private CodeGenTextFieldInterface configFileTextField = null;
    private boolean GetParametersFirstTime = true;
    private String temp_script_file = null;
    public String last_java_classname = null;
    private CodeGenBellRingerInterface bell_ringer = null;
    private CodeGenTextAreaInterface codeTextArea = null;
    private diagapplet.utils.CountButtonInterface m_hierarchyFileLoadButton = null;
    private boolean preserve_modules_hashtable = false;
    private String lastIncludePath = "";
    private int java_classes_written = 0;
    private int total_java_classes_written = 0;
    private int cpp_updates_written = 0;
    private int total_cpp_updates_written = 0;
    String format_function_name = null;
    String c_format_function_name = null;
    String ada_format_function_name = null;
    String format_function_name_base = null;
    static public boolean no_enums = false;
    static public boolean no_swig = false;
    static public boolean no_format = false;

    static public void DebugPrint(String s) {
        try {
            if (!debug_on) {
                return;
            }
            Throwable t = new Throwable();
            System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static public void DebugPrint2(String s) {
        try {
            Throwable t = new Throwable();
            System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
            diagapplet.utils.DiagError.println(s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    static private volatile StructureTypeInfo cur_type_info = null;
    static private volatile StructureTypeInfo last_type_info = null;
    private static int error_count = 0;

    public int get_error_count() {
        return error_count;
    }
    static boolean cmd_line_printed = false;

    static public void ErrorPrint(String s) {
        try {

            Throwable t = new Throwable();
            error_count++;
            String sfull = s + "\n    trace=" + StackTracePrinter.ThrowableToShortList(t) + ", error_count=" + error_count + "\n";
            if (debug_on) {
                System.out.println("cur_type_info =" + cur_type_info);
                System.out.println(sfull);
            }
            if (null != cur_type_info) {
                sfull = cur_type_info.fromFileName + ":" + cur_type_info.fromLineNumber + " " + cur_type_info.fromLineText + "\n"
                        + cur_type_info.fromFileName + ":" + cur_type_info.fromLineNumber + " " + sfull;
            } else if (null != last_type_info) {
                sfull = last_type_info.fromFileName + ":" + last_type_info.fromLineNumber + " " + last_type_info.fromLineText + "\n"
                        + last_type_info.fromFileName + ":" + last_type_info.fromLineNumber + " (cur_type_info == null, using last_type_info)" + sfull;
            }
            if (!cmd_line_printed) {
                diagapplet.utils.DiagError.println(orig_args_one_string);
                cmd_line_printed = true;
            }
            diagapplet.utils.DiagError.println(sfull);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static public void WarningPrint(String s) {
        try {
            Throwable t = new Throwable();
            String sfull = StackTracePrinter.ThrowableToShortList(t) + " CODEGEN WARNING: (" + error_count + ") " + s;
            if (debug_on) {
                System.out.println("cur_type_info =" + cur_type_info);
                System.out.println(sfull);
            }
            if (null != cur_type_info) {
                sfull = cur_type_info.fromFileName + ":" + cur_type_info.fromLineNumber + " " + sfull;
            }
            if (!cmd_line_printed) {
                diagapplet.utils.DiagError.println(orig_args_one_string);
            }
            diagapplet.utils.DiagError.println(sfull);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void setVisible(boolean visible) {
    }

    public void RingBell() {
        try {
            if (null != bell_ringer) {
                bell_ringer.RingBell();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String GetParameter(String strName, String args[]) {

        String res = StringFuncs.GetParameter(strName, args, optionsHashtable, null);
        if (orig_args_one_string.indexOf(strName) < 0 && null != res) {
            orig_args_one_string += " " + strName + "=" + res;
        }
        //System.out.println("res="+res+", strName="+strName+", args="+args);
        return res;
    }

    public void GeneratePrintSizesFile() {
        try {
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            if (selected_classes.length < 1) {
                return;
            }
            //		FileOutputStream sizes_fos =null;
            //sizes_fos = new FileOutputStream(new File(sizes_file_name));
            WriteOutput("*/*\n");
            if (null != this.currentOutputFileName) {
                WriteOutput("\t* File:" + this.currentOutputFileName + "\n");
            }
            WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
            WriteOutput("*\twith command line arguments : " + orig_args_one_string + "\n");
//            WriteOutput("*\tRCS_VERSION=" + rcs.RCS_VERSION.version_string);
            if (null != prev_lines_of_script) {
                WriteOutput("*\n");
                WriteOutput("*\t.gen script :\n");
                if (null != lines_of_script) {
                    for (int i = 0; i < lines_of_script.length; i++) {
                        String s = (String) lines_of_script[i];
                        WriteOutput("*\t\t" + i + ":" + s + "\n");
                    }
                } else {
                    for (int i = 0; i < prev_lines_of_script.size(); i++) {
                        String s = (String) prev_lines_of_script.get(i);
                        WriteOutput("*\t\t" + i + ":" + s + "\n");
                    }
                }
                WriteOutput("*\n");
            }
            WriteOutput("*/\n\n");

            WriteOutput("#include <stdlib.h>\n");
            WriteOutput("#include <stdio.h>\n");
            WriteOutput("#include \"rcs.hh\"\n");
            for (int i = 0; i < ModuleInfo.headerFiles.size(); i++) {
                String header = (String) ModuleInfo.headerFiles.elementAt(i);
                WriteOutput("#include \"" + header + "\"\n");
            }
            WriteOutput("\n");
            WriteOutput("int main(int argc, const char **argv)\n");
            WriteOutput("{\n");
            WriteOutput("\tprintf(\"sizeof(NMLmsg)=%d\\n\",sizeof(NMLmsg));\n");
            WriteOutput("\tprintf(\"sizeof(RCS_CMD_MSG)=%d\\n\",sizeof(RCS_CMD_MSG));\n");
            WriteOutput("\tprintf(\"sizeof(RCS_STAT_MSG)=%d\\n\",sizeof(RCS_STAT_MSG));\n");
            for (int i = 0; i < selected_classes.length; i++) {
                StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null == typeInfo) {
                    continue;
                }
                if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                    continue;
                }
                WriteOutput("\tprintf(\"sizeof(" + selected_classes[i] + ")=%d\\n\",sizeof(" + selected_classes[i] + "));\n");
            }
            WriteOutput("\n");
            WriteOutput("}\n");
            WriteOutput("\n");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // STANDALONE APPLICATION SUPPORT
    //    The GetParameters() method retrieves the values of each of the applet's
    // parameters and stores them in variables. This method works both when the
    // applet is run as a standalone application and when it's run within an HTML
    // page.  When the applet is run as a standalone application, this method is
    // called by the main() method, which passes it the command-lineNumber arguments.
    // When the applet is run within an HTML page, this method is called by the
    // init() method with args == null.
    //------------------------------------------------------------------------
    private String orig_args[] = null;
    private static String orig_args_one_string = "";

    private String FileToString(String filename) {
        FileInputStream fis = null;
        try {
            File f = new File(filename);
            fis = new FileInputStream(f);
            byte b[] = new byte[(int) f.length()];
            fis.read(b);
            fis.close();
            return new String(b);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public void GetParameters(String args[]) {
        try {
            if (null == args) {
                return;
            }
            orig_args = args;
            String fullargstring = "";
            for (int j = 0; j < args.length; j++) {
                orig_args_one_string += " " + args[j];
                fullargstring += "args[" + j + "]=" + args[j] + ", ";
            }
            if (debug_on) {
                DebugPrint("GetParameters(" + fullargstring + ")");
            }
            // Query values of all Parameters
            //--------------------------------------------------------------
            String param;
            String unused_args[] = null;

            // HierarchyConfigurationFile: The URL or name of the Configuration file which is parsed to determine which header files to read, which modules are children of other modules, etc.
            //--------------------------------------------------------------
            if (GetParametersFirstTime) {
                unused_args = new String[args.length];
                System.arraycopy(args, 0, unused_args, 0, args.length);

                param = StringFuncs.GetParameter(PARAM_ConfigFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    m_ConfigFile = param;
                }
                param = StringFuncs.GetParameter(PARAM_HierarchyFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    m_ConfigFile = param;
                }
                param = StringFuncs.GetParameter(PARAM_HeaderFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    m_ConfigFile = param;
                }
                param = StringFuncs.GetParameter(PARAM_HFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    m_ConfigFile = param;
                }
                param = StringFuncs.GetParameter(PARAM_HHFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    m_ConfigFile = param;
                }
                param = StringFuncs.GetParameter(PARAM_ScriptFile, args, optionsHashtable, unused_args);
                if (param != null) {
                    script = FileToString(param);
                }
                param = StringFuncs.GetParameter(PARAM_DisplayOn, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        display_on = Boolean.valueOf(param).booleanValue();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

            }

            param = StringFuncs.GetParameter("add_java_getters_and_setters", args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    add_java_getters_and_setters = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter("debug_estimated_size", args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    debug_estimated_size = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_DebugOn, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    debug_on = Boolean.valueOf(param).booleanValue();
                    ModuleInfo.debug_on = debug_on;
                    C_Generator.debug_on = debug_on;
                    Ada_Generator.debug_on = debug_on;
                    URL_and_FileLoader.debug_on = debug_on;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_UpdateWithName, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    update_with_name = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_CreatePrintSizesFile, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    create_print_sizes_file = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_SizesFileName, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    sizes_file_name = param;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_SelectFromAllFiles, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    select_from_all_files = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_GenerateSymbolLookups, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    generate_symbol_lookups = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_GenerateAllEnumSymbolLookups, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    set_generate_all_enum_symbol_lookups(Boolean.valueOf(param).booleanValue());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_AddSetHeader, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    set_add_set_header(Boolean.valueOf(param).booleanValue());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_GenerateForAllLangs, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    generate_for_all_langs = Boolean.valueOf(param).booleanValue();
                    if (generate_for_all_langs) {
                        generate_for_cpp = true;
                        generate_for_c = true;
                        generate_for_ada = true;
                        generate_for_java = true;
                    }

                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            if (!generate_for_all_langs) {
                param = StringFuncs.GetParameter(PARAM_GenerateForCPP, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        generate_for_cpp = Boolean.valueOf(param).booleanValue();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                param = StringFuncs.GetParameter(PARAM_GenerateForC, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        generate_for_c = Boolean.valueOf(param).booleanValue();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                param = StringFuncs.GetParameter(PARAM_GenerateForJava, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        generate_for_java = Boolean.valueOf(param).booleanValue();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                param = StringFuncs.GetParameter(PARAM_JavaPackageName, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        java_package_name = param;
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }

                param = StringFuncs.GetParameter(PARAM_GenerateForAda, args, optionsHashtable, unused_args);
                if (param != null) {
                    try {
                        generate_for_ada = Boolean.valueOf(param).booleanValue();
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            param = StringFuncs.GetParameter("force_keep_status_enum", args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    force_keep_status_enum = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            param = StringFuncs.GetParameter("force_keep_admin_state_enum", args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    force_keep_admin_state_enum = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            param = StringFuncs.GetParameter(PARAM_PrintScript, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    print_script = Boolean.valueOf(param).booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_DlaLengthInit, args, optionsHashtable, unused_args);
            if (param != null) {
                try {
                    dla_length_init = Integer.valueOf(param).intValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            param = StringFuncs.GetParameter(PARAM_UseDefaultTypes, args, optionsHashtable, unused_args);
            if (param != null) {
                UseDefaultTypes = Boolean.valueOf(param).booleanValue();
                ModuleInfo.UseDefaultTypes = UseDefaultTypes;
                if (!UseDefaultTypes) {
                    ResetGenericClasses();
                }
            }

            param = StringFuncs.GetParameter(PARAM_NoErrlog, args, optionsHashtable, unused_args);
            if (param != null) {
                ModuleInfo.no_errlog = Boolean.valueOf(param).booleanValue();
            }

            if (null != unused_args) {
                for (int j = 0; j < unused_args.length; j++) {
                    if (null != unused_args[j]
                            && unused_args[j].indexOf('=') > 0
                            && !unused_args[j].startsWith("-D")) {
                        WarningPrint("CommandLine argument " + unused_args[j] + " not used. (possibly mispelled?)");
                    }
                }
            }
            GetParametersFirstTime = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    static boolean help_printed = false;

    static public void printHelp() {
        help_printed = true;
        System.out.println("CodeGenCmdLine.java : \r\n");
        System.out.println("\t\t" + rcs.RCS_VERSION.version_string + " \r\n");
        System.out.println("\t\t" + ModuleInfo.Id + " \r\n");
        System.out.println("\r\n");
        System.out.println("usage: -I<include_directory> [param=value] [param=value] ... <header> <header> ... \r\n");

        System.out.println("\t" + CodeGenCommonInterface.PARAM_ScriptFile + "=<value>\t--\t set script file\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_DebugOn + "=<value>\t--\t set to true to print extensive \r\n\t\t\tdebug output\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_UpdateWithName + "=<value>\t--\t set to false to use the \r\n\t\t\tolder style CMS::update() functions rather than \r\n\t\t\tCMS::update_with_name() functions. \r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_CreatePrintSizesFile + "=<value>\t--\t set to true to \r\n\t\t\tcreate a seperate file just for printing message sizes\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_SelectFromAllFiles + "=<value>\t--\t if more than one \r\n\t\t\theader is given and no script is used set to true to \r\n\t\t\tinclude messages from all headers instead of just the last. \r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_SizesFileName + "=<value>\t--\t when creating a file to \r\n\t\t\tprint sizes, use a name other than print_nmlmsg_sizes.cc\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateSymbolLookups + "=<value>\t--\t  generate symbol lookup functions for enums and NML Type IDs\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateAllEnumSymbolLookups + "=<value>\t--\t  genertate all enum symbol lookups including otherwise excluded default and unused types.\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateForC + "=<value>\t--\t when no script is given, \r\n\t\t\tgenerate code for C\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateForAllLangs + "=<value>\t--\t  when no script is given, \r\n\t\t\tgenerate code for C,C++, Ada & Java\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateForCPP + "=<value>\t--\t when no script is given, \r\n\t\t\tset to false to NOT generate code for C++\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateForJava + "=<value>\t--\t when no script is given, \r\n\t\t\tgenerate code for Java\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_JavaPackageName + "=<value>\t--\t put generated java files \r\n\t\t\tin given package\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_GenerateForAda + "=<value>\t--\twhen no script is given, \r\n\t\t\tgenerate code for Ada \r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_DlaLengthInit + "=<value>\t--\t set a default length other \r\n\t\t\tthan 0 for dynamic length arrays\r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_UseDefaultTypes + "=<value>\t--\t use some default \r\n\t\t\tbuiltin types \r\n");
        System.out.println("\t" + CodeGenCommonInterface.PARAM_PrintScript + "=<value>\t--\t set to true to print \r\n\t\t\tcommands as they are executed from a script. \r\n");
        System.out.println("\r\n");
        System.out.println("Other options:");
//        System.out.println("\t--Exclude <pattern>\t--ignore header files matching pattern.");
    }
    // Create Toplevel Window to contain applet CodeGen
    //----------------------------------------------------------------------

    public String createScript(String args[]) {
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.createScriptFile(" + args + ")");
            }
            StringBuffer sb = new StringBuffer();
            String last_header = null;
            String str;
            byte b[];
            boolean header_found = false;

            for (int i = 0; i < args.length; i++) {
                if (debug_on) {
                    DebugPrint("args[" + i + "]=" + args[i]);
                }
                if (args[i].compareTo("-I") == 0
                        || args[i].compareTo("--Exclude") == 0) {
                    i++;
                    continue;
                }
                if (args[i].equalsIgnoreCase("--help")
                        || args[i].equalsIgnoreCase("-help")
                        || args[i].equalsIgnoreCase("help")
                        || args[i].equalsIgnoreCase("HHFile=--help")
                        || args[i].equalsIgnoreCase("HHFile=-help")
                        || args[i].equalsIgnoreCase("HHFile=help")) {
                    if (!help_printed) {
                        CodeGenCommon.printHelp();
                    }
                    continue;
                }
                if ((args[i].indexOf('=') < 0
                        || args[i].startsWith("HHFile=")
                        || args[i].startsWith("HFile="))
                        && (args[i].endsWith(".hh")
                        || args[i].endsWith(".h")
                        || args[i].endsWith(".hpp")
                        || args[i].endsWith(".hp")
                        || args[i].endsWith(".HP")
                        || args[i].endsWith(".HH")
                        || args[i].endsWith(".H")
                        || args[i].endsWith(".HPP"))) {
                    last_header = args[i];
                    if (last_header.indexOf('=') > 0) {
                        last_header = last_header.substring(last_header.indexOf('=') + 1);
                    }
                    header_found = true;
                    str = "load " + last_header + "\n";
                    sb.append(str);
                }
            }
            if (!header_found) {
                if (!help_printed) {
                    String s = "Bad command line arguments:";
                    for (int j = 0; j < args.length; j++) {
                        s += " " + args[j] + " ";
                    }
                    ErrorPrint(s);
                    ErrorPrint("Either provide a C/C++ header file OR an NML CodeGen script with \"script=<file>\".");
                    CodeGenCommon.printHelp();
                }
                System.exit(1);
            }
            if (last_header.indexOf('/') > 0) {
                last_header = last_header.substring(last_header.lastIndexOf('/') + 1);
            }
            if (last_header.indexOf('\\') > 0) {
                last_header = last_header.substring(last_header.lastIndexOf('\\') + 1);
            }
            String c_file = null;
            String c_header = null;
            String cpp_file = null;
            String cpp_protos_header_file = null;
            String java_file = null;
            if (forced_output_filename == null) {
                cpp_protos_header_file = last_header.substring(0, last_header.lastIndexOf('.')) + "_n_codegen_protos.hh\n";
                cpp_file = last_header.substring(0, last_header.lastIndexOf('.')) + "_n.cc";
                c_file = last_header.substring(0, last_header.lastIndexOf('.')) + "_c_n.c";
                c_header = last_header.substring(0, last_header.lastIndexOf('.')) + "_c_n.h";
                java_file = cpp_file.substring(0, cpp_file.lastIndexOf('.') - 1) + "MsgDict.java";
            } else {
                generate_for_all_langs = false;
                generate_for_c = false;
                generate_for_cpp = false;
                generate_for_java = false;
                create_print_sizes_file = false;
                generate_for_ada = false;
                if (forced_output_filename.endsWith(".c")) {
                    generate_for_c = true;
                    c_file = forced_output_filename;
                } else if (forced_output_filename.endsWith(".cc")
                        || forced_output_filename.endsWith(".cpp")) {
                    generate_for_cpp = true;
                    cpp_file = forced_output_filename;
                } else if (forced_output_filename.endsWith("MsgDict.java")) {
                    generate_for_java = true;
                    java_file = forced_output_filename;
                } else {
                    System.err.println("Type of output file :" + forced_output_filename + " not recognized.");
                    System.exit(1);
                }
            }
            if (!select_from_all_files) {
                str = "clear\n";
                sb.append(str);
                for (int i = 0; i < args.length; i++) {
                    if ((args[i].indexOf('=') < 0
                            || args[i].startsWith("HHFile=")
                            || args[i].startsWith("HFile="))
                            && (args[i].endsWith(".hh")
                            || args[i].endsWith(".h")
                            || args[i].endsWith(".hp")
                            || args[i].endsWith(".hpp"))) {
                        last_header = args[i];
                        if (last_header.indexOf('=') > 0) {
                            last_header = last_header.substring(last_header.indexOf('=') + 1);

                        }
                        str = "select_from_file " + last_header + "\n";
                        sb.append(str);
                    }
                }
            } else {
                str = "select_from_all\n";
                sb.append(str);
            }
            if (create_print_sizes_file) {
                str = "generate C++ print_sizes>" + sizes_file_name + "\n";
                sb.append(str);
            }
            if (generate_for_c || generate_for_all_langs) {
                if (c_header != null) {
                    str = "generate C protos>" + c_header + "\n";
                    sb.append(str);
                }
                if (c_file != null) {
                    str = "generate C format>" + c_file + "\n";
                    sb.append(str);
                    str = "generate C update>" + c_file + "\n";
                    sb.append(str);
                }
            }
            if (generate_for_cpp || generate_for_all_langs) {
                if (null != cpp_protos_header_file) {
                    str = "generate C++ protos>" + cpp_protos_header_file;
                    sb.append(str);
                }
                if (null != cpp_file) {
                    str = "generate C++ format>" + cpp_file + "\n";
                    sb.append(str);
                    str = "generate C++ update>" + cpp_file + "\n";
                    sb.append(str);
                    str = "generate C++ constructor>" + cpp_file + "\n";
                    sb.append(str);
                }
            }
            if (generate_for_java || generate_for_all_langs) {
                if (null != java_package_name) {
                    str = "package " + java_package_name + "\n";
                    sb.append(str);
                }
                str = "generate Java dict>" + java_file + "\n";
                sb.append(str);
                str = "generate Java classes >*\n";
                sb.append(str);
            }
            if (generate_for_ada || generate_for_all_langs) {
                String ada_spec_file = cpp_file.substring(0, cpp_file.lastIndexOf('.')) + "_ada.ads";
                str = "generate Ada spec>" + ada_spec_file + "\n";
                sb.append(str);
                String ada_body_file = cpp_file.substring(0, cpp_file.lastIndexOf('.')) + "_ada.adb";
                str = "generate Ada body>" + ada_body_file + "\n";
                sb.append(str);
            }
            str = "exit\n";
            sb.append(str);
            return sb.toString();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public boolean CheckForCppEnum(String cpp_type) {
        if (null == cpp_type) {
            ErrorPrint("Attempt to check for C++ enumeration class when type is null");
            return false;
        }
        try {
            StringTokenizer cpp_tokenizer = new StringTokenizer(cpp_type, " \t\r\n.,=;[]");
            String cpp_token = null;
            last_type_converted_was_class = false;
            while (cpp_tokenizer.hasMoreTokens()) {
                cpp_token = cpp_tokenizer.nextToken();
                if (cpp_token.equals("double")) {
                    return false;
                }
                if (cpp_token.equals("bool")) {
                    return false;
                }
                if (cpp_token.equals("float")) {
                    return false;
                }
                if (cpp_token.equals("short")) {
                    return false;
                }
                if (cpp_token.equals("long")) {
                    return false;
                }
                if (cpp_token.equals("char")) {
                    return false;
                }
                if (cpp_token.equals("enum")) {
                    return true;
                }
                if (cpp_token.equals("int")) {
                    return false;
                }
                if (null != ModuleInfo.m_structInfoByNameHashTable.get(cpp_token)) {
                    return false;
                }
                if (null != ModuleInfo.m_enumInfoHashTable.get(cpp_token)) {
                    return true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean CheckForCppClass(String cpp_type) {
        if (null == cpp_type) {
            ErrorPrint("Attempt to check for C++ class when type is null");
            return false;
        }
        try {
            StringTokenizer cpp_tokenizer = new StringTokenizer(cpp_type, " \t\r\n.,=;[]");
            String cpp_token = null;
            last_type_converted_was_class = false;
            while (cpp_tokenizer.hasMoreTokens()) {
                cpp_token = cpp_tokenizer.nextToken();
                if (cpp_token.equals("unsigned")) {
                    return false;
                }
                if (cpp_token.equals("double")) {
                    return false;
                }
                if (cpp_token.equals("float")) {
                    return false;
                }
                if (cpp_token.equals("short")) {
                    return false;
                }
                if (cpp_token.equals("long")) {
                    return false;
                }
                if (cpp_token.equals("char")) {
                    return false;
                }
                if (cpp_token.equals("enum")) {
                    return false;
                }
                if (cpp_token.equals("int")) {
                    return false;
                }
                if (cpp_token.equals("bool")) {
                    return false;
                }
                if (null != ModuleInfo.m_structInfoByNameHashTable.get(cpp_token)) {
                    return true;
                }
                if (null != ModuleInfo.m_enumInfoHashTable.get(cpp_token)) {
                    return false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public boolean CheckForCppPosemathClass(String cpp_type) {
        boolean is_posemath_class = CheckForCppPosemathClassHidden(cpp_type);
        if (debug_on) {
            DebugPrint("CheckForCppPosemathClass(" + cpp_type + ") returning " + is_posemath_class);
        }
        return is_posemath_class;
    }

    private boolean CheckForCppPosemathClassHidden(String cpp_type) {

        if (null == cpp_type) {
            ErrorPrint("Attempt to check for C++ posemath class when type is null");
            return false;
        }
        try {
            StringTokenizer cpp_tokenizer = new StringTokenizer(cpp_type, " \t\r\n.,=;[]");
            String cpp_token = null;
            last_type_converted_was_class = false;
            while (cpp_tokenizer.hasMoreTokens()) {
                cpp_token = cpp_tokenizer.nextToken();
                if (cpp_token.equals("struct")) {
                    continue;
                }
                if (cpp_token.equals("class")) {
                    continue;
                }
                if (cpp_token.equals("CMS_DURATION")) {
                    return true;
                }
                if (cpp_token.equals("CMS_DATE_TIME")) {
                    return true;
                }
                if (cpp_token.equals("CMS_TIME")) {
                    return true;
                }
                if (cpp_token.equals("CMS_DATE")) {
                    return true;
                }
                if (cpp_token.equals("PM_CARTESIAN")) {
                    return true;
                }
                if (cpp_token.equals("PM_SPERICAL")) {
                    return true;
                }
                if (cpp_token.equals("PM_CYLINDRICAL")) {
                    return true;
                }
                if (cpp_token.equals("PM_ROTATION_VECTOR")) {
                    return true;
                }
                if (cpp_token.equals("PM_ROTATION_MATRIX")) {
                    return true;
                }
                if (cpp_token.equals("PM_QUATERNION")) {
                    return true;
                }
                if (cpp_token.equals("PM_EULER_ZYZ")) {
                    return true;
                }
                if (cpp_token.equals("PM_EULER_ZYX")) {
                    return true;
                }
                if (cpp_token.equals("PM_RPY")) {
                    return true;
                }
                if (cpp_token.equals("PM_POSE")) {
                    return true;
                }
                if (cpp_token.equals("PM_XYA")) {
                    return true;
                }
                if (cpp_token.equals("PM_HOMOGENEOUS")) {
                    return true;
                }
                return false;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public String GetEnumTypeName(String vardef) {
        if (vardef == null) {
            return null;
        }
        try {
            StringTokenizer tokenizer = new StringTokenizer(vardef, " \t\r\n[]()/*;,.=");
            String token = null;
            if (!tokenizer.hasMoreTokens()) {
                return null;
            }
            token = tokenizer.nextToken();
            if (token.equals(ndla_string) && tokenizer.hasMoreTokens()) {
                token = tokenizer.nextToken();
            }
            if (token.equals("enum") && tokenizer.hasMoreTokens()) {
                token = tokenizer.nextToken();
            }
            if (!tokenizer.hasMoreTokens()) {
                return null;
            }
            if (null == ModuleInfo.m_enumInfoHashTable.get(token)) {
                return null;
            } else {
                return token;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean VarIsUnsigned(String vardef) {
        if (vardef == null) {
            return false;
        }
        try {
            int uindex = vardef.indexOf("unsigned");
            if (uindex < 0) {
                return false;
            }
            if (uindex > 0) {
                char c1 = vardef.charAt(uindex - 1);
                if (c1 != ' ' && c1 != '\t' && c1 != '\r' && c1 != '\n' && c1 != ';') {
                    return false;
                }
            }
            char c2 = vardef.charAt(uindex + 8);
            if (c2 == ' ' || c2 == '\t' || c2 == '\r' || c2 == '\n') {
                return true;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean VarIsLongLong(String vardef) {
        if (vardef == null) {
            return false;
        }
        try {
            StringTokenizer st = new StringTokenizer(vardef, " \t;");
            int long_count = 0;
            while (st.hasMoreTokens()) {
                String tok = st.nextToken();
                if (tok.compareTo("long") == 0) {
                    long_count++;
                    if (long_count >= 2) {
                        return true;
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public String GetCppVarDef(String var, StructureTypeInfo type_info) {
        int vindex = -1;
        int sindex = -1;
        int s2index = -1;
        try {
            vindex = type_info.PreFinalPassInfo.indexOf(" " + var + ";");
            if (vindex < 0) {
                vindex = type_info.PreFinalPassInfo.indexOf(var);
            }
            if (vindex < 0) {
                ErrorPrint("GetCppVarDef couldn't find " + var);
                return null;
            }
            char c;
            if (vindex > 0) {
                c = type_info.PreFinalPassInfo.charAt(vindex - 1);
            } else {
                c = ' ';
            }
            char ce = type_info.PreFinalPassInfo.charAt(vindex + var.length());
            while ((c != ' ' && c != '\t' && c != ',')
                    || (ce != ' ' && ce != '\t' && ce != ',' && ce != '[' && ce != ';')) {
                vindex = type_info.PreFinalPassInfo.indexOf(var, vindex + 1);
                if (vindex < 0) {
                    ErrorPrint("GetCppVarDef couldn't find " + var);
                    return null;
                }
                //ErrorPrint("c="+c+",ce="+ce);
                c = type_info.PreFinalPassInfo.charAt(vindex - 1);
                ce = type_info.PreFinalPassInfo.charAt(vindex + var.length());
            }
            if (vindex < 0) {
                ErrorPrint("GetCppVarDef couldn't find " + var);
                return null;
            }
            sindex = type_info.PreFinalPassInfo.indexOf(';', vindex);
            if (sindex < 0) {
                ErrorPrint("GetCppVarDef couldn't end of definition for " + var);
                return null;
            }
            s2index = type_info.PreFinalPassInfo.lastIndexOf(';', vindex);
            if (s2index < 0) {
                s2index = 0;
            }
            String def = type_info.PreFinalPassInfo.substring(s2index, sindex);
            return def;
        } catch (Exception e) {
            ErrorPrint("var=" + var + ",type_info.Name=" + type_info.getName());
            e.printStackTrace();
        }
        return null;
    }

    public String ConvertCppTypeToJavaType(String cpp_type) {
        StringTokenizer cpp_tokenizer = new StringTokenizer(cpp_type, " \t\r\n.,=;[]");
        String cpp_token = null;
        last_type_converted_was_class = false;
        skip_type = false;
        while (cpp_tokenizer.hasMoreTokens()) {
            cpp_token = cpp_tokenizer.nextToken();
            if (cpp_token.equals("double")) {
                return "double";
            }
            if (cpp_token.equals("float")) {
                return "float";
            }
            if (cpp_token.equals("short")) {
                return "short";
            }
            if (cpp_token.equals("long")) {
                return "long";
            }
            if (cpp_token.equals("char")) {
                return "byte";
            }
            if (cpp_token.equals("bool")) {
                return "boolean";
            }
            if (cpp_token.equals("enum")) {
                return "int";
            }
            if (cpp_token.equals("int")) {
                return "int";
            }
            if (cpp_token.equals("friend")) {
                skip_type = true;
                return "";
            }
            final StructureTypeInfo typeFromStructInfoByNameHashTable = ModuleInfo.m_structInfoByNameHashTable.get(cpp_token);
            if (null != typeFromStructInfoByNameHashTable) {
                if (debug_on) {
                    DebugPrint("cpp_type= \"" + cpp_token + "\" is in ModuleInfo.m_structInfoByNameHashTable.");
                }
                last_type_converted_was_class = true;
                return cpp_token;
            }
            if (is_generic(cpp_token)) {
                if (debug_on) {
                    DebugPrint("cpp_type= \"" + cpp_token + "\" is generic.");
                }
                last_type_converted_was_class = true;
                return cpp_token;
            }
            if (null != ModuleInfo.m_enumInfoHashTable.get(cpp_token)) {
                return "int";
            }
        }
        if (cpp_type.compareTo("unsigned") == 0) {
            return "int";
        }
        ErrorPrint("Unrecognized type :" + cpp_type + " m_structInfoByNameHashTable.keySet = " + ModuleInfo.m_structInfoByNameHashTable.keySet());
        return "CODEGEN_UNRECOGNIZED_TYPE";
    }

    public boolean CheckForJavaStatic(String java_type) {
        StringTokenizer java_tokenizer = new StringTokenizer(java_type, " \t\r\n.,=;[]");
        String java_token = null;
        last_type_converted_was_class = false;
        while (java_tokenizer.hasMoreTokens()) {
            java_token = java_tokenizer.nextToken();
            if (java_token.equals("static")) {
                return true;
            }
        }
        return false;
    }

    public boolean CheckForJavaClass(String java_type) {
        StringTokenizer java_tokenizer = new StringTokenizer(java_type, " \t\r\n.,=;[]");
        String java_token = null;
        last_java_classname = null;
        last_type_converted_was_class = false;
        while (java_tokenizer.hasMoreTokens()) {
            java_token = java_tokenizer.nextToken();
            if (java_token.equals("boolean")) {
                return false;
            }
            if (java_token.equals("double")) {
                return false;
            }
            if (java_token.equals("byte")) {
                return false;
            }
            if (java_token.equals("float")) {
                return false;
            }
            if (java_token.equals("short")) {
                return false;
            }
            if (java_token.equals("long")) {
                return false;
            }
            if (java_token.equals("char")) {
                return false;
            }
            if (java_token.equals("enum")) {
                return false;
            }
            if (java_token.equals("int")) {
                return false;
            }
            if (null != ModuleInfo.m_structInfoByNameHashTable.get(java_token)) {
                last_java_classname = java_token;
                return true;
            }
            if (is_generic(java_token)) {
                last_java_classname = java_token;
                return true;
            }
        }
        DebugPrint("Unrecognized type:" + java_type);
        return false;
    }
    // @SuppressWarnings("unchecked")

    public void CreateJavaDefinition(StructureTypeInfo type_info) {
        try {
            last_type_info = cur_type_info;
            cur_type_info = type_info;
            StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            if (debug_on) {
                DebugPrint("Creating JavaDefinition for " + type_info.getName());
            }
            String info_token = null;
            int r_squareParamIndex = -1;
            int l_squareParamIndex = -1;
            int lastSpaceIndex = -1;
            String array_length_string = null;
            boolean dynamic_array = false;
            boolean unbounded_array = false;
            String variable_name = null;
            String cpp_type = null;
            String java_type = null;
            int num_dims = 0;
            Vector enums_used_in_this_class = null;
            type_info.JavaDefinition = "";

            String orig_info_token = null;
            //		String trimmed_orig_info_token = null;

            // If this class was derived from another just eat the members of the base class.
            while (info_tokenizer.hasMoreTokens()) {
                info_token = info_tokenizer.nextToken();
                if (debug_on) {
                    DebugPrint("info_token=" + info_token);
                }
                int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
                if (nml_dynamic_length_array_index >= 0) {
                    info_token = info_token.substring(nml_dynamic_length_array_index + ndla_string.length());
                    dynamic_array = true;
                } else {
                    dynamic_array = false;
                }
                int nml_unbounded_length_array_index = info_token.indexOf(unbounded_string);
                if (nml_unbounded_length_array_index >= 0) {
                    info_token = info_token.substring(nml_unbounded_length_array_index + ndla_string.length());
                    unbounded_array = true;
                } else {
                    unbounded_array = false;
                }
                orig_info_token = info_token;
                info_token = info_token.trim();
                //trimmed_orig_info_token = info_token;
                array_length_string = null;
                variable_name = null;
                cpp_type = null;
                java_type = null;
                num_dims = 0;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                l_squareParamIndex = info_token.indexOf('[');
                if (l_squareParamIndex > 0) {
                    array_length_string = info_token.substring(l_squareParamIndex);
                    info_token = info_token.substring(0, l_squareParamIndex);
                    l_squareParamIndex = 0;
                    r_squareParamIndex = array_length_string.lastIndexOf(']');
                    if (r_squareParamIndex > 0 && r_squareParamIndex < array_length_string.length()) {
                        array_length_string = array_length_string.substring(0, r_squareParamIndex + 1);
                    } else {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- array_length_string (" + array_length_string + ") needs another ].");

                        RingBell();
                        continue;
                    }
                    num_dims = 0;
                    if (debug_on) {
                        DebugPrint("r_squareParamIndex=" + r_squareParamIndex + ", l_squareParamIndex=" + l_squareParamIndex + ", num_dims=" + num_dims);
                    }
                    String new_array_length_string = "";
                    int array_length = 1;
                    ;
                    while (r_squareParamIndex > 0 && l_squareParamIndex >= 0) {
                        // ErrorPrint("array_length_string="+array_length_string+", l_squareParamIndex="+l_squareParamIndex);
                        String this_dim_array_string = array_length_string.substring(l_squareParamIndex);
                        if (debug_on) {
                            DebugPrint("this_dim_array_string=" + this_dim_array_string);
                        }
                        int this_dim_length = ModuleInfo.doArrayLengthMath(this_dim_array_string);
                        array_length *= this_dim_length;
                        new_array_length_string += Integer.toString(this_dim_length);
                        l_squareParamIndex = array_length_string.indexOf('[', l_squareParamIndex + 1);
                        r_squareParamIndex = array_length_string.indexOf(']', l_squareParamIndex + 1);
                        if (-1 != r_squareParamIndex && -1 != l_squareParamIndex) {
                            new_array_length_string += "][";
                        }
                        num_dims++;
                        if (debug_on) {
                            DebugPrint("this_dim_length=" + this_dim_length + ", r_squareParamIndex=" + r_squareParamIndex + ", l_squareParamIndex=" + l_squareParamIndex + ", num_dims=" + num_dims);
                        }
                    }
                    array_length_string = Integer.toString(array_length);
                    num_dims = 1;
                }
                lastSpaceIndex = info_token.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *0");
                    RingBell();
                    continue;
                }
                variable_name = info_token.substring(lastSpaceIndex + 1);
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs another space. L1143--11%");
                        RingBell();
                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                }
                variable_name = variable_name.trim();
                if (variable_name.length() < 1) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- no variable_name.");
                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character.");
                    RingBell();
                    continue;
                }
                if (!Character.isJavaIdentifierStart(variable_name.charAt(0))) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character at start (" + variable_name.charAt(0) + ")");
                    RingBell();
                    continue;
                }
                if (debug_on) {
                    DebugPrint("variable_name=" + variable_name);
                }
                cpp_type = info_token.substring(0, lastSpaceIndex);
                if (cpp_type.indexOf('*') >= 0) {
                    type_info.contains_pointers = true;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('?') >= 0
                        || cpp_type.indexOf('-') >= 0
                        || cpp_type.indexOf('\\') >= 0
                        || cpp_type.indexOf('/') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('=') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                        || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                        || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                        || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                        || cpp_type.indexOf(',') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains illegal character.");
                    RingBell();
                    continue;
                }
                if (debug_on) {
                    DebugPrint("cpp_type=" + cpp_type);
                }
                java_type = ConvertCppTypeToJavaType(cpp_type);
                if (debug_on) {
                    DebugPrint("java_type=" + java_type);
                }
                if (java_type == null
                        || java_type.equals("CODEGEN_UNRECOGNIZED_TYPE")) {
                    type_info.contains_unrecognized_type = true;
                    type_info.JavaDefinition = null;
                    ErrorPrint("No java type for cpp_type=" + cpp_type + ", orig_info_token=" + orig_info_token);
                    return;
                }
                if (skip_type) {
                    skip_type = false;
                    continue;
                }
                boolean is_enum = false;
                if (!last_type_converted_was_class) {
                    is_enum = CheckForCppEnum(cpp_type);
                }
                String enum_name = cpp_type;
                int lastspaceindex = -1;
                EnumTypeInfo enum_info = null;
                boolean this_enum_already_used = false;
                Enumeration enum_keys = null;
                String keystring = null;
                String keytouse = null;
                int ival = -1;
                int ival2 = -1;
                Integer Ival = null;
                if (is_enum) {
                    lastspaceindex = cpp_type.lastIndexOf(' ');
                    if (lastspaceindex > 0) {
                        enum_name = enum_name.substring(lastspaceindex + 1);
                    }
                    enum_info
                            = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                    if (null == enum_info) {
                        DebugPrint("Can't get enum_info for " + enum_name);
                        type_info.JavaClassArrayInitializers += "\t\t/* Bad enumeration name" + enum_name + " */\n\t\t;;\n";
                    } else {
                        if (null == enums_used_in_this_class) {
                            enums_used_in_this_class = new Vector();
                        }
                        for (int ei = 0; ei < enums_used_in_this_class.size(); ei++) {
                            EnumTypeInfo compare_enum_info
                                    = (EnumTypeInfo) enums_used_in_this_class.elementAt(ei);
                            if (compare_enum_info.equals(enum_info)) {
                                this_enum_already_used = true;
                                break;
                            }
                        }
                        if (!this_enum_already_used) {
                            enums_used_in_this_class.addElement(enum_info);
                            type_info.JavaDefinition += "\n\tpublic static NML_ENUM_INFO nml_enum_info_for_" + enum_info.Name + "=null;\n";
                            type_info.JavaClassArrayInitializers += "\n\t\tif(nml_enum_info_for_" + enum_info.Name + "==null)\n";
                            type_info.JavaClassArrayInitializers += "\t\t{\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + " = new NML_ENUM_INFO();\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + ".name=\"" + enum_info.Name + "\";\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + ".string_to_int_hash = new java.util.Hashtable();\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + ".int_to_string_hash = new java.util.Hashtable();\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tInteger I_" + enum_info.Name + "=null;\n";
                            type_info.JavaClassArrayInitializers += "\t\t\tString Str_" + enum_info.Name + "=null;\n";
                            enum_keys = enum_info.reverse_hashtable.keys();
                            keytouse = keystring;
                            ival = -1;
                            while (enum_keys.hasMoreElements()) {
                                keystring = (String) enum_keys.nextElement();
                                Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                ival2 = Ival.intValue();
                                type_info.JavaClassArrayInitializers += "\t\t\tI_" + enum_info.Name + "=  (" + ival2 + ");\n";
                                type_info.JavaClassArrayInitializers += "\t\t\tStr_" + enum_info.Name + "= \"" + keystring + "\";\n";
                                type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + ".int_to_string_hash.put(I_" + enum_info.Name + ",Str_" + enum_info.Name + ");\n";
                                type_info.JavaClassArrayInitializers += "\t\t\tnml_enum_info_for_" + enum_info.Name + ".string_to_int_hash.put(Str_" + enum_info.Name + ",I_" + enum_info.Name + ");\n";
                            }
                            type_info.JavaClassArrayInitializers += "\t\t}\n\n";
                        }
                    }
                }
                if (null != array_length_string) {
                    if (debug_on) {
                        DebugPrint("java_type=" + java_type + ", variable_name=" + variable_name + ", array_length_string=" + array_length_string);
                    }
                    type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name;
                    for (int i = 0; i < num_dims; i++) {
                        type_info.JavaDefinition += "[]";
                    }
                    array_length_string = m_currentModule.ReplaceDefinedValues(array_length_string, 0, null);
                    if (debug_on) {
                        DebugPrint("After ReplaceDefinedValues : java_type=" + java_type + ", variable_name=" + variable_name + ", array_length_string=" + array_length_string);
                    }
                    if (num_dims < 2) {
                        array_length_string = Integer.toString(ModuleInfo.doArrayLengthMath(array_length_string));
                        if (debug_on) {
                            DebugPrint("After doArrayLengthMath : java_type=" + java_type + ", variable_name=" + variable_name + ", array_length_string=" + array_length_string);
                        }
                    }
                    if (!dynamic_array) {
                        type_info.JavaDefinition += " = new " + java_type + "[" + array_length_string + "];\n";
                    } else {
                        type_info.JavaDefinition += " = new " + java_type + "[" + array_length_string + "]; /* " + ndla_string + " */\n";
                    }
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (defv != null
                            && java_type.equals("byte")
                            && cpp_type.equals("char")) {
                        try {
                            int al = Integer.parseInt(array_length_string);
                            type_info.JavaClassArrayInitializers += "\n\t\t\t/* set by default=" + defv + " comment */\n";
                            int defvi = 0;
                            for (defvi = 0; defvi < defv.length() && defvi < al - 1; defvi++) {
                                type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[" + defvi + "]  =  (" + java_type + ")" + "'" + defv.charAt(defvi) + "';\n";
                            }
                            if (defvi < al - 1) {
                                type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[" + defvi + "]  =  (" + java_type + ") 0;\n";
                            }
                            type_info.JavaClassArrayInitializers += "\n";
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        continue;
                    }

                    type_info.JavaClassArrayInitializers += "\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + array_length_string + "; i_" + variable_name + "++ )\n\t\t{\n";
                    if (last_type_converted_was_class) {
                        if (num_dims > 1) {
                            throw new Exception("CodeGenerator can not handle multi-dimensional arrays of Classes");
                        }
                        type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "] = new " + java_type + "();\n";
                    } else {
                        if (defv == null) {
                            if (is_enum) {
                                enum_keys = enum_info.reverse_hashtable.keys();
                                keystring = (String) enum_keys.nextElement();
                                keytouse = keystring;
                                ival = -1;
                                while (enum_keys.hasMoreElements()) {
                                    ival2 = -1;
                                    Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                    ival2 = Ival.intValue();
                                    if (ival == -1) {
                                        ival = ival2;
                                    }
                                    if (ival2 == 0) {
                                        keytouse = keystring;
                                        ival = 0;
                                        break;
                                    }
                                    keystring = (String) enum_keys.nextElement();
                                }
                                type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  = " + ival + "; /* enum " + enum_name + " : " + keytouse + "=" + ival + " */\n";
                            } else {
                                if (java_type.equals("boolean")) {
                                    type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  = false;\n";
                                } else {
                                    type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  = 0;\n";
                                }
                            }
                        } else {
                            if (is_enum) {
                                enum_name = cpp_type;
                                lastspaceindex = cpp_type.lastIndexOf(' ');
                                if (lastspaceindex > 0) {
                                    enum_name = enum_name.substring(lastspaceindex + 1);
                                }
                                enum_info
                                        = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                Ival = (Integer) enum_info.reverse_hashtable.get(defv);
                                if (Ival != null) {
                                    ival = Ival.intValue();
                                    type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  =  (" + java_type + ")" + ival + "; /* set by default=" + defv + " comment, enum " + enum_name + " : " + defv + "=" + ival + " */\n";
                                } else {
                                    type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  = (" + java_type + ") " + defv + ";\n /* set by default=" + defv + " comment */";
                                }
                            } else {
                                type_info.JavaClassArrayInitializers += "\t\t\t" + variable_name + "[i_" + variable_name + "]  = (" + java_type + ") " + defv + ";\n /* set by default=" + defv + " comment */";
                            }
                        }
                    }
                    type_info.JavaClassArrayInitializers += "\t\t}\n";
                } else if (last_type_converted_was_class) {
                    if (unbounded_array) {
                        type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + "[] = null; /* " + unbounded_string + " */\n";
                    } else {
                        type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = new " + java_type + "();\n";
                    }
                } else {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (defv == null) {
                        if (unbounded_array) {
                            type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + "[] = null; /* " + unbounded_string + " */\n";
                        } else {
                            if (is_enum) {
                                enum_name = cpp_type;
                                lastspaceindex = cpp_type.lastIndexOf(' ');
                                if (lastspaceindex > 0) {
                                    enum_name = enum_name.substring(lastspaceindex + 1);
                                }
                                enum_info
                                        = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                if (null == enum_info) {
                                    DebugPrint("Can't get enum_info for " + enum_name);
                                    type_info.JavaDefinition += "\t/* Bad enumeration name" + enum_name + " */\n";
                                } else {
                                    enum_keys = enum_info.reverse_hashtable.keys();
                                    keystring = (String) enum_keys.nextElement();
                                    keytouse = keystring;
                                    ival = -1;
                                    while (enum_keys.hasMoreElements()) {
                                        ival2 = -1;
                                        Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                        ival2 = Ival.intValue();
                                        if (ival == -1) {
                                            ival = ival2;
                                        }
                                        if (ival2 == 0) {
                                            keytouse = keystring;
                                            ival = 0;
                                            break;
                                        }
                                        keystring = (String) enum_keys.nextElement();
                                    }
                                    type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " =" + ival + "; /* enum " + enum_name + " : " + keytouse + "=" + ival + " */\n";
                                }
                            } else {
                                if (type_info.VarnameNDLAHashTable.containsKey(variable_name)) {
                                    type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = " + dla_length_init + ";\n";
                                } else {
                                    if (java_type.equals("boolean")) {
                                        type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = false;\n";
                                    } else {
                                        type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = 0;\n";
                                    }
                                }
                            }
                        }
                    } else {
                        if (unbounded_array) {
                            type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + "[] = null; /* " + unbounded_string + " */\n";
                        } else {
                            if (is_enum) {
                                enum_name = cpp_type;
                                lastspaceindex = cpp_type.lastIndexOf(' ');
                                if (lastspaceindex > 0) {
                                    enum_name = enum_name.substring(lastspaceindex + 1);
                                }
                                enum_info
                                        = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                Ival = (Integer) enum_info.reverse_hashtable.get(defv);
                                if (Ival != null) {
                                    ival = Ival.intValue();
                                    type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = (" + java_type + ") " + ival + "; /* set by default=" + defv + " comment, enum " + enum_name + " : " + defv + "=" + ival + " */\n";
                                } else {
                                    type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = (" + java_type + ") " + defv + ";\n/* set by default comment */ ";
                                }
                            } else {
                                type_info.JavaDefinition += "\tpublic " + java_type + " " + variable_name + " = (" + java_type + ") " + defv + ";/* set by default comment */\n";
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            ErrorPrint("Error Generating Java Class Definition for " + type_info.getName());
            ErrorPrint("type_info.DerivedFrom = " + type_info.DerivedFrom);
            ErrorPrint("type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo);
            ErrorPrint("type_info.HiddenInfo = " + type_info.HiddenInfo);
            ErrorPrint("type_info.RawInfo = " + type_info.RawInfo);
            ErrorPrint("type_info.JavaDefinition = " + type_info.JavaDefinition);
            RingBell();
            e.printStackTrace();
        } finally {
            cur_type_info = null;
        }
    }

    public void CreateJavaUpdateFunction(StructureTypeInfo type_info) {
        String info_token = null;
        int r_squareParamIndex = -1;
        int l_squareParamIndex = -1;
        int lastSpaceIndex = -1;
        String array_length_string = null;
        String variable_name = null;
        String java_type = null;
        String init_string = null;
        int eqIndex = -1;
        //	int num_dims = 0;
        type_info.JavaUpdateFunction = "";
        String orig_info_token = null;
        //String trimmed_orig_info_token=null;
        int array_length = 1;
        boolean is_class = false;
        boolean dynamic_array = false;
        boolean unbounded_array = false;
        String union_selector_var_name = null;
        try {
            if (null == type_info) {
                ErrorPrint("Attempt to CreateJavaUpdateFunction with null type_info");
                return;
            }
            if (null == type_info.JavaDefinition) {
                ErrorPrint("Attempt to CreateJavaUpdateFunction with null type_info.JavaDefinition  : type_info=" + type_info);
                return;
            }
            StringTokenizer line_tokenizer = new StringTokenizer(type_info.JavaDefinition, "\r\n");
            if (debug_on) {
                DebugPrint("Creating JavaUpdateFunction for " + type_info.getName());
            }
            if (update_with_name && m_currentModule.definedValues.containsKey("DO_NOT_ADD_INDEXES_TO_ARRAY_NAMES")) {
                type_info.JavaUpdateFunction
                        += "\n\t\tnml_fc.add_array_indexes_to_name=false;\n";
            }
            EnumTypeInfo union_enum_info = null;
            if (update_with_name) {
                if (!type_info.is_union) {
                    if (null != type_info.DerivedFrom) {
                        type_info.JavaUpdateFunction
                                += "\n\t\tnml_fc.beginClass(\"" + type_info.getName() + "\",\"" + type_info.UnqualifiedDerivedFrom + "\");\n";

                        type_info.JavaUpdateFunction += "\n\t\tnml_fc.beginBaseClass(\"" + type_info.UnqualifiedDerivedFrom + "\");\n";
                        type_info.JavaUpdateFunction += "\n\t\tsuper.update(nml_fc);\n";
                        type_info.JavaUpdateFunction += "\n\t\tnml_fc.endBaseClass(\"" + type_info.UnqualifiedDerivedFrom + "\");\n";
                    } else {
                        type_info.JavaUpdateFunction
                                += "\n\t\tnml_fc.beginClass(\"" + type_info.getName() + "\",null);\n";
                    }
                } else {
                    String union_enum_name = StringFuncs.replaceFirstInString(type_info.getName(), "Union", "_UNION_ENUM");
                    union_enum_info = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(union_enum_name);
                    if (union_enum_info == null) {
                        ErrorPrint("Bad union definition " + type_info.getName() + ", no corresponding enum " + union_enum_name);
                        return;
                    }
                    type_info.JavaUpdateFunction
                            += "\n\t\tnml_fc.beginUnion(\"" + type_info.getName() + "\");\n";

                    type_info.JavaUpdateFunction
                            += "\n\t\tswitch(union_selector)\n\t\t{\n";
                }
            } else if (type_info.is_union) {
                String union_enum_name = StringFuncs.replaceFirstInString(type_info.getName(), "Union", "_UNION_ENUM");
                union_enum_info = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(union_enum_name);
                if (union_enum_info == null) {
                    ErrorPrint("Bad union definition " + type_info.getName() + ", no corresponding enum " + union_enum_name);
                    return;
                }
                type_info.JavaUpdateFunction
                        += "\n\t\tswitch(union_selector)\n\t\t{\n";
            }
            while (line_tokenizer.hasMoreTokens()) {
                String line_token = line_tokenizer.nextToken();
                int nml_dynamic_length_array_index = line_token.indexOf(ndla_string);
                if (nml_dynamic_length_array_index >= 0) {
                    dynamic_array = true;
                } else {
                    dynamic_array = false;
                }
                int nml_unbounded_length_array_index = line_token.indexOf(unbounded_string);
                if (nml_unbounded_length_array_index >= 0) {
                    unbounded_array = true;
                } else {
                    unbounded_array = false;
                }
                int semicolon_index = line_token.indexOf(';');
                if (semicolon_index >= 0) {
                    info_token = line_token.substring(0, semicolon_index);
                } else {
                    continue;
                }
                orig_info_token = info_token;
                info_token = info_token.trim();
                //trimmed_orig_info_token=info_token;
                array_length_string = null;
                variable_name = null;
                java_type = null;
                //num_dims = 0;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                eqIndex = info_token.indexOf('=');
                if (eqIndex < 0) {
                    ErrorPrint("Java variable not initialized. " + info_token);
                    RingBell();
                    continue;
                }
                init_string = info_token.substring(eqIndex + 1);
                info_token = info_token.substring(0, eqIndex);
                l_squareParamIndex = init_string.indexOf('[');
                r_squareParamIndex = init_string.indexOf(']', l_squareParamIndex);
                array_length = 1;
                while (-1 != r_squareParamIndex && -1 != l_squareParamIndex) {
                    array_length_string = init_string.substring(l_squareParamIndex + 1, r_squareParamIndex);
                    array_length_string = m_currentModule.ReplaceDefinedValues(array_length_string, 0, null);
                    int this_dim_length = ModuleInfo.doArrayLengthMath(array_length_string);
                    array_length *= this_dim_length;
                    if (debug_on) {
                        DebugPrint("this_dim_length = " + this_dim_length + ", array_length = " + array_length + ", info_token = " + init_string);
                    }
                    l_squareParamIndex = init_string.indexOf('[', l_squareParamIndex + 1);
                    r_squareParamIndex = init_string.indexOf(']', l_squareParamIndex + 1);
                }
                lastSpaceIndex = info_token.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *1");
                    RingBell();
                    continue;
                }
                variable_name = info_token.substring(lastSpaceIndex + 1);
                l_squareParamIndex = variable_name.indexOf('[');
                if (l_squareParamIndex > 0) {
                    variable_name = variable_name.substring(0, l_squareParamIndex);
                }
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") for in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs another space. L1703--19%");
                        RingBell();
                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                    l_squareParamIndex = variable_name.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        variable_name = variable_name.substring(0, l_squareParamIndex);
                    }
                }
                variable_name = variable_name.trim();
                if (variable_name.length() < 1) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- no variable_name\n");
                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character.");
                    RingBell();
                    continue;
                }
                if (!Character.isJavaIdentifierStart(variable_name.charAt(0))) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character at start (" + variable_name.charAt(0) + ").");

                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    RingBell();
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character.");
                    continue;
                }
                String ovn_variable_name = variable_name;
                String override_name = (String) type_info.VarnameOverridesHashTable.get(variable_name);
                if (override_name != null) {
                    ovn_variable_name = override_name;
                }
                java_type = info_token.substring(0, lastSpaceIndex);
                if (CheckForJavaStatic(java_type)) {
                    continue;
                }
                is_class = CheckForJavaClass(java_type);
                if (debug_on) {
                    DebugPrint("CheckForJavaClass(" + java_type + ") returned " + is_class);
                }
                if (java_type.startsWith("public ")) {
                    java_type = java_type.substring(7);
                }
                String cpp_var_def = null;
                String att_add = "";
                if (type_info.VarnameAttributeInfoHashTable.containsKey(variable_name)) {
                    att_add = "_attribute";
                } else if (variable_name.endsWith("_length")
                        && variable_name.length() > 7
                        && (type_info.VarnameNDLAHashTable.containsKey(variable_name.substring(0, variable_name.length() - 7)) || type_info.VarnameNDLAHashTable.containsKey(variable_name))) {
                    att_add = "_dla_length";
                }
                if (update_with_name) {
                    cpp_var_def = GetCppVarDef(variable_name, type_info);
                }
                if (type_info.is_union) {
                    final String nameWithReplacements = StringFuncs.replaceAllInString(type_info.getName(), "Union", "");
                    if (null == union_enum_info) {
                        throw new NullPointerException("union_enum_info");
                    }
                    if (!union_enum_info.reverse_hashtable.containsKey("UNION_" + nameWithReplacements + "_SELECTED_" + variable_name)) {
                        continue;
                    }
                    if (null == type_info.JavaUpdateFunction) {
                        throw new NullPointerException("type_info.JavaUpdateFunction");
                    }
                    type_info.JavaUpdateFunction
                            += "\n\t\t\tcase UNION_" + nameWithReplacements + "_SELECTED_" + variable_name + ":\n";
                }
                if (!is_class && !type_info.is_union && update_with_name) {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (null != defv) {
                        type_info.JavaUpdateFunction += "\t\tnml_fc.next_update_default(\"" + defv + "\");\n";
                    }
                }
                boolean var_is_unsigned = VarIsUnsigned(cpp_var_def);
                boolean var_is_long_long = VarIsLongLong(cpp_var_def);
                if (array_length > 1) {
                    array_length_string = String.valueOf(array_length);
                    if (null != m_currentModule) {
                        if (null != m_currentModule.definedValues) {
                            DefinedValue dv = (DefinedValue) m_currentModule.definedValues.get(variable_name + "_ARRAY_LENGTH_VARIABLE");
                            if (null != dv) {
                                array_length_string = dv.value;
                            }
                        }
                    }

                    if (is_class) {
                        if (update_with_name) {
                            if (!dynamic_array) {
                                type_info.JavaUpdateFunction += "\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + array_length_string + "; i_" + variable_name + "++ )";
                                type_info.JavaUpdateFunction += "\n\t\t{\n\t\t\tnml_fc.beginClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t\t\t";
                                type_info.JavaUpdateFunction += variable_name + "[i_" + variable_name + "].update(nml_fc);\n";
                                type_info.JavaUpdateFunction += "\n\t\t\tnml_fc.endClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t\t}\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + variable_name + "_length; i_" + variable_name + "++ )";
                                type_info.JavaUpdateFunction += "\n\t\t{\n\t\t\tnml_fc.beginClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t\t\t";
                                type_info.JavaUpdateFunction += variable_name + "[i_" + variable_name + "].update(nml_fc);\n";
                                type_info.JavaUpdateFunction += "\n\t\t\tnml_fc.endClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t\t}\n";
                            }
                        } else {
                            if (!dynamic_array) {
                                type_info.JavaUpdateFunction += "\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + array_length_string + "; i_" + variable_name + "++ ) " + variable_name + "[i_" + variable_name + "].update(nml_fc);\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + variable_name + "_length; i_" + variable_name + "++ ) " + variable_name + "[i_" + variable_name + "].update(nml_fc);\n";
                            }
                        }

                    } else {
                        if (update_with_name) {
                            if (!dynamic_array) {
                                if (var_is_unsigned) {
                                    if (var_is_long_long) {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_unsigned_ll_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + array_length_string + ");\n";
                                    } else {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_unsigned_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + array_length_string + ");\n";
                                    }
                                } else {
                                    if (var_is_long_long) {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_ll_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + array_length_string + ");\n";
                                    } else {
                                        String enum_type_name = GetEnumTypeName(cpp_var_def);
                                        if (enum_type_name == null) {
                                            type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + array_length_string + ");\n";
                                        } else {
                                            type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_enumeration_array_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + array_length_string + ",nml_enum_info_for_" + enum_type_name + ");\n";
                                        }
                                    }
                                }
                            } else {
                                if (var_is_unsigned) {
                                    type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_unsigned_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + variable_name + "_length);\n";
                                } else {
                                    String enum_type_name = GetEnumTypeName(cpp_var_def);
                                    if (enum_type_name == null) {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + variable_name + "_length);\n";
                                    } else {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update" + att_add + "_enumeration_array_with_name(\"" + ovn_variable_name + "\"," + variable_name + "," + variable_name + "_length,nml_enum_info_for_" + enum_type_name + ");\n";
                                    }
                                }
                            }
                        } else {
                            if (!dynamic_array) {
                                if (var_is_unsigned) {
                                    if (var_is_long_long) {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update_unsigned_ll(" + variable_name + "," + array_length_string + ");\n";
                                    } else {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update_unsigned(" + variable_name + "," + array_length_string + ");\n";
                                    }
                                } else {
                                    if (var_is_long_long) {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update_ll(" + variable_name + "," + array_length_string + ");\n";
                                    } else {
                                        type_info.JavaUpdateFunction += "\t\tnml_fc.update(" + variable_name + "," + array_length_string + ");\n";
                                    }
                                }
                            } else {
                                if (var_is_unsigned) {
                                    type_info.JavaUpdateFunction += "\t\tnml_fc.update_unsigned(" + variable_name + "," + variable_name + "_length);\n";
                                } else {
                                    type_info.JavaUpdateFunction += "\t\tnml_fc.update(" + variable_name + "," + variable_name + "_length);\n";
                                }
                            }
                        }
                    }
                    if (type_info.is_union) {
                        type_info.JavaUpdateFunction
                                += "\t\t\tbreak;\n";
                    }
                } else if (is_class) {
                    if (unbounded_array) {
                        type_info.JavaUpdateFunction += "\t\tint " + variable_name + "_length_of_unbounded =\n\t\t\tnml_fc.get_length_of_unbounded(\"" + java_type + "\",\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                        type_info.JavaUpdateFunction += "\t\tif(" + variable_name + "_length_of_unbounded > 0)\n\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\tif(null == " + variable_name + ")\n\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t" + variable_name + "= new " + java_type + "[" + variable_name + "_length_of_unbounded];\n";
                        type_info.JavaUpdateFunction += "\t\t\t\tfor(int i_new_" + variable_name + "=0; i_new_" + variable_name + " < " + variable_name + "_length_of_unbounded; i_new_" + variable_name + "++)\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t\t" + variable_name + "[i_new_" + variable_name + "] = new " + java_type + "();\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t}\n";
                        type_info.JavaUpdateFunction += "\t\t\t}// endif(null == " + variable_name + ")\n";
                        type_info.JavaUpdateFunction += "\t\t\telse if(" + variable_name + ".length != " + variable_name + "_length_of_unbounded)\n\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t" + variable_name + "= new " + java_type + "[" + variable_name + "_length_of_unbounded];\n";
                        type_info.JavaUpdateFunction += "\t\t\t\tfor(int i_new_" + variable_name + "=0; i_new_" + variable_name + " < " + variable_name + "_length_of_unbounded; i_new_" + variable_name + "++)\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t\t" + variable_name + "[i_new_" + variable_name + "] = new " + java_type + "();\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t}\n";
                        type_info.JavaUpdateFunction += "\t\t\t}// endif(" + variable_name + ".length < " + variable_name + "_length_of_unbounded)\n";
                        type_info.JavaUpdateFunction += "\t\t} //endif(" + variable_name + "_length_of_unbounded > 0)\n";
                        type_info.JavaUpdateFunction += "\t\tif(null != " + variable_name + ")\n\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\tfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + variable_name + ".length; i_" + variable_name + "++ )\n";
                        type_info.JavaUpdateFunction += "\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\tif(" + variable_name + "[i_" + variable_name + "] != null)\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t{\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t\tnml_fc.beginClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t\t" + variable_name + "[i_" + variable_name + "].update(nml_fc);\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t\tnml_fc.endClassArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                        type_info.JavaUpdateFunction += "\t\t\t\t} //endif(" + variable_name + "[i_" + variable_name + "] != null)\n";
                        type_info.JavaUpdateFunction += "\t\t\t} // endfor(int i_" + variable_name + " = 0; i_" + variable_name + " < " + variable_name + ".length; i_" + variable_name + "++ )\n";

                        type_info.JavaUpdateFunction += "\t\t} // endif(null != " + variable_name + ")\n";

                    } else {
                        if (update_with_name) {
                            if (union_selector_var_name == null) {
                                type_info.JavaUpdateFunction += "\t\tnml_fc.beginClassVar(\"" + ovn_variable_name + "\");\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\tnml_fc.beginUnionVar(\"" + ovn_variable_name + "\");\n";
                            }
                        }
                        if (union_selector_var_name == null) {
                            type_info.JavaUpdateFunction += "\t\t" + variable_name + ".update(nml_fc);\n";
                        } else {
                            type_info.JavaUpdateFunction += "\t\t" + variable_name + ".update_union(" + union_selector_var_name + ",nml_fc);\n";
                        }
                        if (update_with_name) {
                            if (union_selector_var_name == null) {
                                type_info.JavaUpdateFunction += "\t\tnml_fc.endClassVar(\"" + ovn_variable_name + "\");\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\tnml_fc.endUnionVar(\"" + ovn_variable_name + "\");\n";
                            }
                        }
                    }
                    union_selector_var_name = null;
                } else {
                    if (update_with_name) {
                        String unbounded_add = "";
                        if (unbounded_array) {
                            unbounded_add = "_unbounded";
                        }
                        if (var_is_unsigned) {
                            if (var_is_long_long) {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update" + unbounded_add + att_add + "_unsigned_ll_with_name(\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update" + unbounded_add + att_add + "_unsigned_with_name(\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                            }
                        } else {
                            String enum_type_name = GetEnumTypeName(cpp_var_def);
                            if (enum_type_name == null) {
                                if (var_is_long_long) {
                                    type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update" + unbounded_add + att_add + "_ll_with_name(\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                                } else {
                                    type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update" + unbounded_add + att_add + "_with_name(\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                                }
                            } else {
                                if (variable_name.startsWith("union_selector")) {
                                    type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update_union_selector_with_name(\"" + ovn_variable_name + "\"," + variable_name + ",nml_enum_info_for_" + enum_type_name + ");\n";
                                    union_selector_var_name = variable_name;
                                }
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update" + unbounded_add + att_add + "_enumeration_with_name(\"" + ovn_variable_name + "\"," + variable_name + ",nml_enum_info_for_" + enum_type_name + ");\n";
                            }
                        }
                    } else {
                        if (var_is_unsigned) {
                            if (var_is_long_long) {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update_unsigned_ll(" + variable_name + ");\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update_unsigned(" + variable_name + ");\n";
                            }
                        } else {
                            if (var_is_long_long) {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update_ll(" + variable_name + ");\n";
                            } else {
                                type_info.JavaUpdateFunction += "\t\t" + variable_name + " = nml_fc.update(" + variable_name + ");\n";
                            }
                        }
                    }
                }
            }
            if (update_with_name) {
                if (!type_info.is_union) {
                    if (null != type_info.DerivedFrom) {
                        type_info.JavaUpdateFunction
                                += "\n\t\tnml_fc.endClass(\"" + type_info.getName() + "\",\"" + type_info.UnqualifiedDerivedFrom + "\");\n";
                    } else {
                        type_info.JavaUpdateFunction
                                += "\n\t\tnml_fc.endClass(\"" + type_info.getName() + "\",null);\n";
                    }
                } else {
                    type_info.JavaUpdateFunction
                            += "\n\t\t} /* end of switch(union_selector) */\n";
                    type_info.JavaUpdateFunction
                            += "\n\t\tnml_fc.endUnion(\"" + type_info.getName() + "\");\n";
                }
            }
        } catch (Exception e) {
            ErrorPrint("Error Generating Java Update function for " + type_info.getName() + "\n"
                    + "    type_info.DerivedFrom = " + type_info.DerivedFrom + "\n"
                    + "    type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo + "\n"
                    + "    type_info.RawInfo = " + type_info.RawInfo + "\n"
                    + "    type_info.HiddenInfo = " + type_info.HiddenInfo + "\n"
                    + "    type_info.JavaUpdateFunction = " + type_info.JavaUpdateFunction + "\n"
                    + "    info_token = " + info_token);
            RingBell();
            e.printStackTrace();
        }
    }

    public void CreateC_UpdateFunction(StructureTypeInfo type_info) {
        try {
            C_Generator.CreateC_UpdateFunction(type_info, this, ModuleInfo.m_enumInfoHashTable, m_currentModule);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void CreateCppUpdateFunction(StructureTypeInfo type_info) {
        last_type_info = cur_type_info;
        cur_type_info = type_info;
        String info_token = null;
        int r_squareParamIndex = -1;
        int l_squareParamIndex = -1;
        int lastSpaceIndex = -1;
        String array_length_string = null;
        String variable_name = null;
        String cpp_type = null;
        type_info.CppUpdateFunction = "";
        String orig_info_token = null;
        //	String trimmed_orig_info_token=null;
        int num_dims = 0;
        int array_length = 1;
        boolean is_class = false;
        boolean is_enum = false;
        boolean is_posemath_class = false;
        boolean var_class_is_nml_msg = false;
        String union_selector_var_name = null;
        if (debug_on) {
            DebugPrint("CreateCppUpdateFunction: type_info.Name = " + type_info.getName());
            DebugPrint("CreateCppUpdateFunction: type_info.Info = " + type_info.RawInfo);
            DebugPrint("CreateCppUpdateFunction: type_info.Info = " + type_info.HiddenInfo);
            DebugPrint("CreateCppUpdateFunction: type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo);
        }

        try {
            StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            //DebugPrint("Creating Update Function for "+type_info.Name);

            // If this class was derived from another just eat the members of the base class.
            EnumTypeInfo union_enum_info = null;
            if (update_with_name) {
                if (!type_info.is_union) {
                    if (null != type_info.DerivedFrom) {
                        type_info.CppUpdateFunction
                                += "\n\tcms->beginClass(\"" + type_info.getName() + "\",\"" + type_info.UnqualifiedDerivedFrom + "\");\n";
                    } else {
                        type_info.CppUpdateFunction
                                += "\n\tcms->beginClass(\"" + type_info.getName() + "\",0);\n";
                    }
                } else {
                    String union_enum_name = StringFuncs.replaceFirstInString(type_info.getName(), "Union", "_UNION_ENUM");
                    union_enum_info = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(union_enum_name);
                    if (union_enum_info == null) {
                        ErrorPrint("Bad union definition " + type_info.getName() + ", no corresponding enum " + union_enum_name);
                        return;
                    }
                    type_info.CppUpdateFunction
                            += "\n\tcms->beginUnion(\"" + type_info.getName() + "\");\n";
                    type_info.CppUpdateFunction
                            += "\n\tswitch(union_selector)\n\t{\n";
                }
            }
            if (null != type_info.DerivedFrom) {
                if (debug_on) {
                    DebugPrint("CreateCppUpdateFunction: type_info.DerivedFrom = " + type_info.DerivedFrom);
                }
                if (!type_info.DerivedFrom.equals("RCS_CMD_MSG")
                        && !type_info.DerivedFrom.equals("RCS_STAT_MSG")
                        && !type_info.DerivedFrom.equals("NMLmsg")) {
                    if (update_with_name) {
                        type_info.CppUpdateFunction
                                += "\n\tcms->beginBaseClass(\"" + type_info.UnqualifiedDerivedFrom + "\");";
                    }
                    is_posemath_class = CheckForCppPosemathClass(type_info.DerivedFrom);
                    if (is_posemath_class) {
                        if (debug_on) {
                            DebugPrint("CreateCppUpdateFunction: is_posemath_class = " + is_posemath_class);
                        }
                        type_info.CppUpdateFunction += "\n\tcms->update(* ((" + type_info.DerivedFrom + ") x);\n";
                    } else {
                        var_class_is_nml_msg = IsNMLMsg(type_info.DerivedFrom);
                        if (debug_on) {
                            DebugPrint("CreateCppUpdateFunction: var_class_is_nml_msg = " + var_class_is_nml_msg);
                        }
                        if (var_class_is_nml_msg != type_info.is_nml_msg) {
                            String tmperrstring = type_info.getName() + " appears to be derived from " + type_info.DerivedFrom + " but IsNMLMsg(\"" + type_info.DerivedFrom + "\") returned " + var_class_is_nml_msg + " and type_info.is_nml_msg = " + type_info.is_nml_msg;
                            ErrorPrint(tmperrstring);
                            type_info.CppUpdateFunction += "\n\t//" + tmperrstring + "\n";
                        }
                        if (var_class_is_nml_msg || type_info.is_nml_msg) {
                            type_info.CppUpdateFunction += "\n\t" + type_info.DerivedFrom + "::update(cms);\n";
                        } else {
                            if (prefix_nonmsg_update) {
                                type_info.CppUpdateFunction += "\n\t" + type_info.UnqualifiedDerivedFrom + "_update(cms, (" + type_info.DerivedFrom + " *) x);\n";
                            } else {
                                type_info.CppUpdateFunction += "\n\tnmlupdate(cms, (" + type_info.DerivedFrom + " *) x);\n";
                            }
                        }
                    }
                    if (update_with_name) {
                        type_info.CppUpdateFunction
                                += "\tcms->endBaseClass(\"" + type_info.UnqualifiedDerivedFrom + "\");\n\n";
                    }
                } else if (type_info.DerivedFrom.equals("RCS_CMD_MSG")
                        && update_with_name) {
                    type_info.CppUpdateFunction
                            += "\tRCS_CMD_MSG::update_cmd_msg_base(cms);\n";
                } else if (type_info.DerivedFrom.equals("RCS_STAT_MSG")
                        && update_with_name) {
                    type_info.CppUpdateFunction
                            += "\n\tRCS_STAT_MSG::update_stat_msg_base(cms);\n";
                }

            }
            boolean dynamic_array = false;
            boolean unbounded_array = false;

            while (info_tokenizer.hasMoreTokens()) {
                info_token = info_tokenizer.nextToken();
                int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
                if (nml_dynamic_length_array_index >= 0) {
                    info_token = info_token.substring(nml_dynamic_length_array_index + ndla_string.length());
                    dynamic_array = true;
                } else {
                    dynamic_array = false;
                }
                int nml_unbounded_length_array_index = info_token.indexOf(unbounded_string);
                if (nml_unbounded_length_array_index >= 0) {
                    info_token = info_token.substring(nml_unbounded_length_array_index + unbounded_string.length());
                    unbounded_array = true;
                } else {
                    unbounded_array = false;
                }
                orig_info_token = info_token;
                info_token = info_token.trim();
                //trimmed_orig_info_token=info_token;
                //DebugPrint("info_token = "+info_token);
                array_length_string = null;
                variable_name = null;
                cpp_type = null;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                l_squareParamIndex = info_token.indexOf('[');
                String pre_array_string = info_token;
                if (l_squareParamIndex > 0) {
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex);
                    pre_array_string = info_token.substring(0, l_squareParamIndex);
                }
                array_length = 1;
                while (-1 != r_squareParamIndex && -1 != l_squareParamIndex) {
                    array_length_string = info_token.substring(l_squareParamIndex + 1, r_squareParamIndex);
                    array_length_string = m_currentModule.ReplaceDefinedValues(array_length_string, 0, null);
                    int this_dim_length = ModuleInfo.doArrayLengthMath(array_length_string);
                    array_length *= this_dim_length;
                    num_dims++;
                    if (debug_on) {
                        DebugPrint("this_dim_length = " + this_dim_length + ", array_length = " + array_length + ", info_token = " + info_token);
                    }
                    l_squareParamIndex = info_token.indexOf('[', l_squareParamIndex + 1);
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex + 1);
                }
                while (true) {
                    if (pre_array_string.length() < 2) {
                        break;
                    }
                    char last_char = pre_array_string.charAt(pre_array_string.length() - 1);
                    if (last_char != ' '
                            && last_char != '\t'
                            && last_char != '\r'
                            && last_char != '\n') {
                        break;
                    }
                    pre_array_string = pre_array_string.substring(pre_array_string.length() - 1);
                }
                lastSpaceIndex = pre_array_string.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- pre_array_string (" + pre_array_string + ") needs a space. *2");
                    RingBell();
                    continue;
                }

                variable_name = pre_array_string.substring(lastSpaceIndex + 1);
                l_squareParamIndex = variable_name.indexOf('[');
                if (l_squareParamIndex > 0) {
                    variable_name = variable_name.substring(0, l_squareParamIndex);
                }
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs another space. L2313--28%");
                        RingBell();
                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                    l_squareParamIndex = variable_name.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        variable_name = variable_name.substring(0, l_squareParamIndex);
                    }
                }
                if (variable_name.length() < 1) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- no variable_name.");
                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0 || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable (" + variable_name + ") appears to be a pointer or reference.");
                    type_info.CppUpdateFunction += "\n#error Can not create update for pointer or reference type (" + orig_info_token + ")\n";
                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains an illegal character.");
                    RingBell();
                    continue;
                }
                String ovn_variable_name = variable_name;
                String override_name = (String) type_info.VarnameOverridesHashTable.get(variable_name);
                if (override_name != null) {
                    ovn_variable_name = override_name;
                    if (debug_on) {
                        DebugPrint("CreateCppUpdate : variable_name=" + variable_name + ", override_name=" + override_name);
                    }
                }
                String att_add = "";
                cpp_type = info_token.substring(0, lastSpaceIndex);
                if (type_info.VarnameAttributeInfoHashTable.containsKey(variable_name)) {
                    att_add = "_attribute";
                } else if (variable_name.endsWith("_length")
                        && variable_name.length() > 7
                        && (type_info.VarnameNDLAHashTable.containsKey(variable_name.substring(0, variable_name.length() - 7)) || type_info.VarnameNDLAHashTable.containsKey(variable_name))) {
                    att_add = "_dla_length";
                }
                while (true) {
                    if (cpp_type.charAt(0) != ' '
                            && cpp_type.charAt(0) != '\t'
                            && cpp_type.charAt(0) != '\r'
                            && cpp_type.charAt(0) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(1);
                }
                int cpp_type_length = cpp_type.length();
                while (true) {
                    if (cpp_type.charAt(cpp_type_length - 1) != ' '
                            && cpp_type.charAt(cpp_type_length - 1) != '\t'
                            && cpp_type.charAt(cpp_type_length - 1) != '\r'
                            && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                    cpp_type_length = cpp_type.length();
                }
                if (cpp_type.indexOf('*') >= 0) {
                    type_info.contains_pointers = true;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") appears to be a pointer or reference.");
                    type_info.CppUpdateFunction += "\n#error Can not create update for pointer or reference type (" + cpp_type + ")\n";
                    RingBell();
                    continue;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('?') >= 0
                        || cpp_type.indexOf('-') >= 0
                        || cpp_type.indexOf('\\') >= 0
                        || cpp_type.indexOf('/') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('=') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                        || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                        || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                        || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                        || cpp_type.indexOf(',') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains an illegal character.");

                    continue;
                }
                is_class = CheckForCppClass(cpp_type);
                is_posemath_class = false;
                if (is_class) {
                    is_posemath_class = CheckForCppPosemathClass(cpp_type);
                    is_enum = false;
                } else {
                    is_enum = CheckForCppEnum(cpp_type);
                }
                if (type_info.is_union) {
                    if (!union_enum_info.reverse_hashtable.containsKey("UNION_" + StringFuncs.replaceAllInString(type_info.getName(), "Union", "") + "_SELECTED_" + variable_name)) {
                        continue;
                    }
                    type_info.CppUpdateFunction += "\tcase UNION_" + StringFuncs.replaceAllInString(type_info.getName(), "Union", "") + "_SELECTED_" + variable_name + ":\n\t";
                }
                if (!is_class && !is_posemath_class && !type_info.is_union && update_with_name) {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (null != defv) {
                        type_info.CppUpdateFunction += "\tcms->next_update_default(\"" + defv + "\");\n";
                    }
                }

                if (array_length > 1) {
                    array_length_string = String.valueOf(array_length);
                    //DebugPrint("m_currentModule = "+m_currentModule);
                    //DebugPrint("variable_name = "+variable_name);
                    //DebugPrint(variable_name+"_ARRAY_LENGTH_VARIABLE");
                    if (null != m_currentModule) {
                        //DebugPrint("m_currentModule.definedValues ="+m_currentModule.definedValues);
                        if (null != m_currentModule.definedValues) {
                            DefinedValue dv = (DefinedValue) m_currentModule.definedValues.get(variable_name + "_ARRAY_LENGTH_VARIABLE");
                            //DebugPrint("dv ="+dv);
                            if (null != dv) {
                                array_length_string = dv.value;
                            }
                        }
                    }
                    String cast_string = "";
                    if (num_dims > 1) {
                        cast_string = "(" + cpp_type + " *) ";
                    }
                    if (is_enum && num_dims == 1 && !update_with_name) {
                        cast_string = "(int*)";
                    }
                    if (is_class && !is_posemath_class) {
                        var_class_is_nml_msg = IsNMLMsg(cpp_type);
                        if (var_class_is_nml_msg) {
                            if (type_info.is_nml_msg) {
                                if (!dynamic_array) {
                                    if (num_dims <= 1) {
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                        if (update_with_name) {
                                            type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\t\t" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                                        if (update_with_name) {

                                            type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        }
                                    } else {
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                        if (update_with_name) {
                                            type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\t\t(" + cast_string + variable_name + ")[i_" + variable_name + "].update(cms);\n";
                                        if (update_with_name) {

                                            type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        }
                                    }
                                } else {

                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\tcms->beginStructDynamicArray(\"" + ovn_variable_name + "\"," + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + variable_name + "_length ;i_" + variable_name + "++)\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\t\t" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                                    if (update_with_name) {

                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        type_info.CppUpdateFunction += "\tcms->endStructDynamicArray(\"" + ovn_variable_name + "\"," + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                }
                            } else {
                                if (!dynamic_array) {
                                    if (num_dims <= 1) {
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                        if (update_with_name) {
                                            type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\t\tx->" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                                        if (update_with_name) {

                                            type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        }
                                    } else {
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                        if (update_with_name) {
                                            type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\t\t(" + cast_string + "x->" + variable_name + ")[i_" + variable_name + "].update(cms);\n";
                                        if (update_with_name) {

                                            type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        }
                                    }
                                } else {
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\tcms->beginStructDynamicArray(\"" + ovn_variable_name + "\",x->" + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < x->" + variable_name + "_length ;i_" + variable_name + "++)\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\t\tx->" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        type_info.CppUpdateFunction += "\tcms->endStructDynamicArray(\"" + ovn_variable_name + "\",x->" + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                }
                            }
                        } else {
                            if (type_info.is_nml_msg) {
                                if (!dynamic_array) {
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }

                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms," + cast_string + "&(" + variable_name + "[i_" + variable_name + "]));\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\t\tnmlupdate(cms, (" + cast_string + " " + variable_name + ") + i_" + variable_name + ");\n";
                                    }
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                    }
                                } else {
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\tcms->beginStructDynamicArray(\"" + ovn_variable_name + "\"," + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + variable_name + "_length ;i_" + variable_name + "++)\n";

                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }
                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms," + cast_string + "&(" + variable_name + "[i_" + variable_name + "]));\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\t\tnmlupdate(cms, (" + cast_string + "(" + variable_name + ")) + i_" + variable_name + ");\n";
                                    }

                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        type_info.CppUpdateFunction += "\tcms->endStructDynamicArray(\"" + ovn_variable_name + "\"," + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                }
                            } else {
                                if (!dynamic_array) {
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < " + array_length_string + " ;i_" + variable_name + "++)\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }
                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms," + cast_string + "&(x->" + variable_name + "[i_" + variable_name + "]));\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\t\tnmlupdate(cms, (" + cast_string + "(x->" + variable_name + ")) + i_" + variable_name + ");\n";
                                    }
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                    }
                                } else {
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\tcms->beginStructDynamicArray(\"" + ovn_variable_name + "\",x->" + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                    type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + " = 0;i_" + variable_name + " < x->" + variable_name + "_length ;i_" + variable_name + "++)\n";
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t{\n\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n";
                                    }
                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms," + cast_string + "&(x->" + variable_name + "[i_" + variable_name + "]));\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\t\tnmlupdate(cms, (" + cast_string + "(x->" + variable_name + ")) + i_" + variable_name + ");\n";
                                    }
                                    if (update_with_name) {
                                        type_info.CppUpdateFunction += "\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\",i_" + variable_name + ");\n\t}\n";
                                        type_info.CppUpdateFunction += "\tcms->endStructDynamicArray(\"" + ovn_variable_name + "\",x->" + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                }
                            }
                        }
                    } else {
                        if (type_info.is_nml_msg) {
                            if (!dynamic_array) {
                                if (update_with_name) {
                                    if (is_enum) {
                                        String enum_name = cpp_type;
                                        int lastspaceindex = cpp_type.lastIndexOf(' ');
                                        if (lastspaceindex > 0) {
                                            enum_name = enum_name.substring(lastspaceindex + 1);
                                        }
                                        EnumTypeInfo enum_info
                                                = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationArray(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationArray(\"" + ovn_variable_name + "\",0," + array_length_string + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < " + array_length_string + "; i_" + variable_name + "++ )\n";
                                        type_info.CppUpdateFunction += "\t{\n";
                                        type_info.CppUpdateFunction += "\t\t" + variable_name + "[i_" + variable_name + "] = (" + cpp_type + ")\n";
                                        type_info.CppUpdateFunction += "\t\t\tcms->update_enumeration_array_elem(" + variable_name + "[i_" + variable_name + "],(void *) &(" + variable_name + "[i_" + variable_name + "]),i_" + variable_name + ");\n";
                                        type_info.CppUpdateFunction += "\t}\n";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationArray(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationArray(\"" + ovn_variable_name + "\",0," + array_length_string + ");\n";
                                        }

                                    } else {
                                        type_info.CppUpdateFunction += "\tcms->update" + att_add + "_with_name(\"" + ovn_variable_name + "\"," + cast_string + variable_name + "," + array_length_string + ");\n";
                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(" + cast_string + variable_name + "," + array_length_string + ");\n";
                                }
                            } else {
                                if (update_with_name) {
                                    if (is_enum) {
                                        String enum_name = cpp_type;
                                        int lastspaceindex = cpp_type.lastIndexOf(' ');
                                        if (lastspaceindex > 0) {
                                            enum_name = enum_name.substring(lastspaceindex + 1);
                                        }
                                        EnumTypeInfo enum_info
                                                = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationDLA(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + variable_name + "_length," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationDLA(\"" + ovn_variable_name + "\",0," + variable_name + "_length," + array_length_string + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < " + variable_name + "_length && " + variable_name + "_length <= " + array_length_string + "; i_" + variable_name + "++ )\n";
                                        type_info.CppUpdateFunction += "\t{\n";
                                        type_info.CppUpdateFunction += "\t\t" + variable_name + "[i_" + variable_name + "] = (" + cpp_type + ")\n";
                                        type_info.CppUpdateFunction += "\t\t\tcms->update_enumeration_array_elem(" + variable_name + "[i_" + variable_name + "],(void *) &(" + variable_name + "[i_" + variable_name + "]),i_" + variable_name + ");\n";
                                        type_info.CppUpdateFunction += "\t}\n";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationDLA(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + variable_name + "_length," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationDLA(\"" + ovn_variable_name + "\",0," + variable_name + "_length," + array_length_string + ");\n";
                                        }
                                    } else {
                                        type_info.CppUpdateFunction += "\tcms->update_dla_with_name(\"" + ovn_variable_name + "\"," + cast_string + variable_name + "," + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(" + cast_string + variable_name + "," + variable_name + "_length);\n";
                                }
                            }
                        } else {
                            if (!dynamic_array) {
                                if (update_with_name) {
                                    if (is_enum) {
                                        String enum_name = cpp_type;
                                        int lastspaceindex = cpp_type.lastIndexOf(' ');
                                        if (lastspaceindex > 0) {
                                            enum_name = enum_name.substring(lastspaceindex + 1);
                                        }
                                        EnumTypeInfo enum_info
                                                = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationArray(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationArray(\"" + ovn_variable_name + "\",0," + array_length_string + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < " + array_length_string + "; i_" + variable_name + "++ )\n";
                                        type_info.CppUpdateFunction += "\t{\n";
                                        type_info.CppUpdateFunction += "\t\tx->" + variable_name + "[i_" + variable_name + "] = (" + cpp_type + ")\n";
                                        type_info.CppUpdateFunction += "\t\t\tcms->update_enumeration_array_elem(x->" + variable_name + "[i_" + variable_name + "],(void *) &(x->" + variable_name + "[i_" + variable_name + "]),i_" + variable_name + ");\n";
                                        type_info.CppUpdateFunction += "\t}\n";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationArray(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationArray(\"" + ovn_variable_name + "\",0," + array_length_string + ");\n";
                                        }
                                    } else {
                                        type_info.CppUpdateFunction += "\tcms->update" + att_add + "_with_name(\"" + ovn_variable_name + "\"," + cast_string + "x->" + variable_name + "," + array_length_string + ");\n";
                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(" + cast_string + "x->" + variable_name + "," + array_length_string + ");\n";
                                }
                            } else {
                                if (update_with_name) {
                                    if (is_enum) {
                                        String enum_name = cpp_type;
                                        int lastspaceindex = cpp_type.lastIndexOf(' ');
                                        if (lastspaceindex > 0) {
                                            enum_name = enum_name.substring(lastspaceindex + 1);
                                        }
                                        EnumTypeInfo enum_info
                                                = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationDLA(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct,x->" + variable_name + "_length," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->beginEnumerationDLA(\"" + ovn_variable_name + "\",0,x->" + variable_name + "_length," + array_length_string + ");\n";
                                        }
                                        type_info.CppUpdateFunction += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < x->" + variable_name + "_length && x->" + variable_name + "_length <= " + array_length_string + "; i_" + variable_name + "++ )\n";
                                        type_info.CppUpdateFunction += "\t{\n";
                                        type_info.CppUpdateFunction += "\t\tx->" + variable_name + "[i_" + variable_name + "] = (" + cpp_type + ")\n";
                                        type_info.CppUpdateFunction += "\t\t\tcms->update_enumeration_array_elem(x->" + variable_name + "[i_" + variable_name + "],(void *) &(x->" + variable_name + "[i_" + variable_name + "]),i_" + variable_name + ");\n";
                                        type_info.CppUpdateFunction += "\t}\n";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationDLA(\"" + ovn_variable_name + "\",&enum_" + enum_info.Name + "_info_struct,x->" + variable_name + "_length," + array_length_string + ");\n";
                                        } else {
                                            type_info.CppUpdateFunction += "\tcms->endEnumerationDLA(\"" + ovn_variable_name + "\",0,x->" + variable_name + "_length," + array_length_string + ");\n";
                                        }
                                    } else {
                                        type_info.CppUpdateFunction += "\tcms->update_dla_with_name(\"" + ovn_variable_name + "\"," + cast_string + "x->" + variable_name + ", x->" + variable_name + "_length," + array_length_string + ");\n";
                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(" + cast_string + "x->" + variable_name + "," + array_length_string + ");\n";
                                }
                            }
                        }
                    }
                } else if (is_class && !is_posemath_class) {
                    var_class_is_nml_msg = IsNMLMsg(cpp_type);
                    if (unbounded_array) {
                        if (type_info.is_nml_msg) {
                            type_info.CppUpdateFunction += "\tcms->beginStructUnboundedArray(\"" + ovn_variable_name + "\",((void **) &" + variable_name + ")," + variable_name + "_length," + variable_name + "_size_allocated,sizeof(" + cpp_type + "));\n";
                            type_info.CppUpdateFunction += "\tif(0 != " + variable_name + ")\n\t{\n";
                            type_info.CppUpdateFunction += "\t\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < " + variable_name + "_length && i_" + variable_name + " < " + variable_name + "_size_allocated ; i_" + variable_name + "++)\n\t\t{\n";
                            type_info.CppUpdateFunction += "\t\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\", i_" + variable_name + ");\n";
                            if (var_class_is_nml_msg) {
                                type_info.CppUpdateFunction += "\t\t\t" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                            } else {
                                if (prefix_nonmsg_update) {
                                    type_info.CppUpdateFunction += "\t\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms,&" + variable_name + "[i_" + variable_name + "]);\n";
                                } else {
                                    type_info.CppUpdateFunction += "\t\t\tnmlupdate(cms, ((" + cpp_type + "*)" + variable_name + ")+ i_" + variable_name + ");\n";
                                }
                            }
                            type_info.CppUpdateFunction += "\t\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\", i_" + variable_name + ");\n";
                            type_info.CppUpdateFunction += "\t\t}\n\t}\n";
                            type_info.CppUpdateFunction += "\tcms->endStructUnboundedArray(\"" + ovn_variable_name + "\",((void **) &" + variable_name + ")," + variable_name + "_length," + variable_name + "_size_allocated,sizeof(" + cpp_type + "));\n";
                        } else {
                            type_info.CppUpdateFunction += "\tcms->beginStructUnboundedArray(\"" + ovn_variable_name + "\",((void **) &x->" + variable_name + "),x->" + variable_name + "_length,x->" + variable_name + "_size_allocated,sizeof(" + cpp_type + "));\n";
                            type_info.CppUpdateFunction += "\tif(0 != x->" + variable_name + ")\n\t{\n";
                            type_info.CppUpdateFunction += "\t\tfor(int i_" + variable_name + "=0; i_" + variable_name + " < x->" + variable_name + "_length && i_" + variable_name + " < x->" + variable_name + "_size_allocated ; i_" + variable_name + "++)\n\t\t{\n";
                            type_info.CppUpdateFunction += "\t\t\tcms->beginStructArrayElem(\"" + ovn_variable_name + "\", i_" + variable_name + ");\n";
                            if (var_class_is_nml_msg) {
                                type_info.CppUpdateFunction += "\t\t\tx->" + variable_name + "[i_" + variable_name + "].update(cms);\n";
                            } else {
                                if (prefix_nonmsg_update) {
                                    type_info.CppUpdateFunction += "\t\t\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms,&x->" + variable_name + "[i_" + variable_name + "]);\n";
                                } else {
                                    type_info.CppUpdateFunction += "\t\t\tnmlupdate(cms,((" + cpp_type + " *)(x->" + variable_name + "))+i_" + variable_name + ");\n";
                                }
                            }
                            type_info.CppUpdateFunction += "\t\t\tcms->endStructArrayElem(\"" + ovn_variable_name + "\", i_" + variable_name + ");\n";
                            type_info.CppUpdateFunction += "\t\t}\n\t}\n";
                            type_info.CppUpdateFunction += "\tcms->endStructUnboundedArray(\"" + ovn_variable_name + "\",((void **) &x->" + variable_name + "),x->" + variable_name + "_length,x->" + variable_name + "_size_allocated,sizeof(" + cpp_type + "));\n";
                        }
                    } else {
                        if (update_with_name) {

                            if (null == union_selector_var_name) {
                                type_info.CppUpdateFunction += "\tcms->beginClassVar(\"" + ovn_variable_name + "\");\n";
                            } else {
                                type_info.CppUpdateFunction += "\tcms->beginUnionVar(\"" + ovn_variable_name + "\");\n";
                            }
                        }
                        if (var_class_is_nml_msg) {
                            if (type_info.is_nml_msg) {
                                type_info.CppUpdateFunction += "\t" + variable_name + ".update(cms);\n";
                            } else {
                                type_info.CppUpdateFunction += "\tx->" + variable_name + ".update(cms);\n";
                            }
                        } else {
                            if (type_info.is_nml_msg) {
                                if (null == union_selector_var_name) {
                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms,&" + variable_name + ");\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\tnmlupdate(cms,((" + cpp_type + " *)&" + variable_name + "));\n";
                                    }
                                } else {
                                    String uniontypename = cpp_type.substring(cpp_type.lastIndexOf(' ') + 1);
                                    type_info.CppUpdateFunction += "\t" + uniontypename + "_update_union(" + union_selector_var_name + ", cms,&" + variable_name + ");\n";
                                }
                            } else {
                                if (null == union_selector_var_name) {
                                    if (prefix_nonmsg_update) {
                                        type_info.CppUpdateFunction += "\t" + StringFuncs.replace_white_space(cpp_type) + "_update(cms,&(x->" + variable_name + "));\n";
                                    } else {
                                        type_info.CppUpdateFunction += "\tnmlupdate(cms,((" + cpp_type + " *)&(x->" + variable_name + ")));\n";
                                    }
                                } else {
                                    String uniontypename = cpp_type.substring(cpp_type.lastIndexOf(' ') + 1);
                                    type_info.CppUpdateFunction += "\t" + uniontypename + "_update_union(x->" + union_selector_var_name + ", cms,&(x->" + variable_name + "));\n";
                                }
                            }
                        }
                        if (update_with_name) {
                            if (null == union_selector_var_name) {
                                type_info.CppUpdateFunction += "\tcms->endClassVar(\"" + ovn_variable_name + "\");\n";
                            } else {
                                type_info.CppUpdateFunction += "\tcms->endUnionVar(\"" + ovn_variable_name + "\");\n";
                            }
                        }
                    }
                } else {
                    if (unbounded_array) {
                        if (type_info.is_nml_msg) {
                            if (!is_enum) {
                                type_info.CppUpdateFunction += "\tcms->update_unbounded" + att_add + "_with_name(\"" + ovn_variable_name + "\",&" + variable_name + "," + variable_name + "_length," + variable_name + "_size_allocated);\n";
                            }
                        } else {
                            if (!is_enum) {
                                type_info.CppUpdateFunction += "\tcms->update_unbounded" + att_add + "_with_name(\"" + ovn_variable_name + "\",&x->" + variable_name + ", x->" + variable_name + "_length, x->" + variable_name + "_size_allocated);\n";
                            }
                        }
                    } else {
                        if (type_info.is_nml_msg) {

                            if (is_enum) {
                                if (update_with_name) {
                                    String enum_name = cpp_type;
                                    int lastspaceindex = cpp_type.lastIndexOf(' ');
                                    if (lastspaceindex > 0) {
                                        enum_name = enum_name.substring(lastspaceindex + 1);
                                    }
                                    EnumTypeInfo enum_info = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                    if (null == enum_info) {
                                        ErrorPrint("Can't get enum info for " + enum_name);
                                        break;
                                    }
                                    if (debug_on) {
                                        DebugPrint("variable_name=" + variable_name + ",enum_name=" + enum_name + ",enum_info=" + enum_info);
                                    }
                                    if (variable_name.startsWith("union_selector")) {
                                        type_info.CppUpdateFunction += "\t" + variable_name + "= (" + cpp_type + ") cms->update_union_selector_with_name(\"";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction
                                                    += variable_name + "\", (int)" + variable_name + ",(void*)&" + variable_name + ",&enum_" + enum_info.Name + "_info_struct);\n";
                                        } else {
                                            type_info.CppUpdateFunction
                                                    += variable_name + "\", (int)" + variable_name + ",(void*)&" + variable_name + ",0);\n";
                                        }
                                        union_selector_var_name = variable_name;
                                    } else {
                                        type_info.CppUpdateFunction += "\t" + variable_name + "= (" + cpp_type + ") cms->update_enumeration_with_name(\"";
                                        if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                            type_info.CppUpdateFunction += ovn_variable_name + "\", (int)" + variable_name + ",(void*)&" + variable_name + ",&enum_" + enum_info.Name + "_info_struct);\n";
                                        } else {
                                            type_info.CppUpdateFunction += ovn_variable_name + "\", (int)" + variable_name + ",(void*)&" + variable_name + ",0);\n";
                                        }
                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update( (int *) &" + variable_name + ",1);\n";
                                }
                            } else {
                                if (update_with_name) {
                                    type_info.CppUpdateFunction += "\tcms->update" + att_add + "_with_name(\"" + ovn_variable_name + "\"," + variable_name + ");\n";
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(" + variable_name + ");\n";
                                }
                            }

                        } else {

                            if (is_enum) {
                                if (update_with_name) {
                                    String enum_name = cpp_type;
                                    int lastspaceindex = cpp_type.lastIndexOf(' ');
                                    if (lastspaceindex > 0) {
                                        enum_name = enum_name.substring(lastspaceindex + 1);
                                    }
                                    EnumTypeInfo enum_info = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                    if (enum_info != null && enum_info.Name != null) {
                                        if (variable_name.startsWith("union_selector")) {
                                            type_info.CppUpdateFunction += "\tx->" + variable_name + "= (" + cpp_type + ") cms->update_union_selector_with_name(\"";
                                            if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                                type_info.CppUpdateFunction += variable_name + "\", (int)x->" + variable_name + ",(void*)&(x->" + variable_name + "),&enum_" + enum_info.Name + "_info_struct);\n";
                                            } else {
                                                type_info.CppUpdateFunction += variable_name + "\", (int)x->" + variable_name + ",(void*)&(x->" + variable_name + "),0);\n";
                                            }
                                            union_selector_var_name = variable_name;
                                        } else {
                                            type_info.CppUpdateFunction += "\tx->" + variable_name + "= (" + cpp_type + ") cms->update_enumeration_with_name(\"";
                                            if (enum_info.generate_symbol_lookup || generate_symbol_lookups) {
                                                type_info.CppUpdateFunction += variable_name + "\", (int)x->" + variable_name + ",(void*)&(x->" + variable_name + "),&enum_" + enum_info.Name + "_info_struct);\n";
                                            } else {
                                                type_info.CppUpdateFunction += variable_name + "\", (int)x->" + variable_name + ",(void*)&(x->" + variable_name + "),0);\n";
                                            }
                                        }
                                    } else {
                                        ErrorPrint("Could not get enumeration info for (" + enum_name + ")");

                                    }
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update((int *) &(x->" + variable_name + "), 1);\n";
                                }
                            } else {
                                if (update_with_name) {
                                    type_info.CppUpdateFunction += "\tcms->update" + att_add + "_with_name(\"" + ovn_variable_name + "\",x->" + variable_name + ");\n";
                                } else {
                                    type_info.CppUpdateFunction += "\tcms->update(x->" + variable_name + ");\n";
                                }
                            }
                        }
                    }
                }
                if (type_info.is_union) {
                    type_info.CppUpdateFunction += "\tbreak;\n\n";
                }
            }
            if (update_with_name) {
                if (!type_info.is_union) {
                    if (null != type_info.DerivedFrom) {
                        type_info.CppUpdateFunction
                                += "\n\tcms->endClass(\"" + type_info.getName() + "\",\"" + type_info.UnqualifiedDerivedFrom + "\");\n";
                    } else {
                        type_info.CppUpdateFunction
                                += "\n\tcms->endClass(\"" + type_info.getName() + "\",0);\n";
                    }
                } else {
                    type_info.CppUpdateFunction
                            += "\n\t} /* end of switch(union_selector) */\n";
                    type_info.CppUpdateFunction
                            += "\n\tcms->endUnion(\"" + type_info.getName() + "\");\n";
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (debug_on) {
                e.printStackTrace(System.out);
            }
            ErrorPrint("Error Generating C++ Update function for " + type_info.getName());
            ErrorPrint("type_info.DerivedFrom = " + type_info.DerivedFrom);
            ErrorPrint("type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo);
            ErrorPrint("type_info.RawInfo = " + type_info.RawInfo);
            ErrorPrint("type_info.HiddenInfo = " + type_info.HiddenInfo);
            ErrorPrint("type_info.CppUpdateFunction = " + type_info.CppUpdateFunction);
            ErrorPrint("info_token = " + info_token);
            ErrorPrint("array_length_string = " + array_length_string);
            ErrorPrint("variable_name = " + variable_name);
            ErrorPrint("cpp_type = " + cpp_type);
            ErrorPrint("num_dims = " + num_dims);
            ErrorPrint("orig_info_token = " + orig_info_token);
            ErrorPrint("array_length = " + array_length);
            ErrorPrint("is_class = " + is_class);
        } finally {
            cur_type_info = null;
        }
    }

    public void PrintInfo(String options) {
        String selected_classes[];
        try {
            if (options.length() < 3) {
                selected_classes = RemoveDuplicates(ClassList.getSelectedItems());
                if (selected_classes.length < 1) {
                    return;
                }
                for (int classnum = 0; classnum < selected_classes.length; classnum++) {
                    StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[classnum]);
                    if (null != type_info) {
                        DebugPrint("#" + selected_classes[classnum]);
                        if (debug_on) {
                            DebugPrint(type_info.toString());
                        }
                        if (IsNMLMsg(selected_classes[classnum])) {
                            DebugPrint("0\t int type");
                            DebugPrint("1\t int size");
                            PrintInfo(type_info, "", 2, 8);
                        } else {
                            PrintInfo(type_info, "", 0, 0);
                        }
                    }
                }

            } else {
                StringTokenizer tokenizer = new StringTokenizer(options, ", \t");
                while (tokenizer.hasMoreTokens()) {
                    String token = tokenizer.nextToken();
                    StructureTypeInfo type_info
                            = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(token);
                    if (null != type_info) {
                        DebugPrint("#" + token);
                        if (debug_on) {
                            DebugPrint(type_info.toString());
                        }
                        if (IsNMLMsg(token)) {
                            DebugPrint("0\t int type");
                            DebugPrint("1\t int size");
                            PrintInfo(type_info, "", 2, 8);
                        } else {
                            PrintInfo(type_info, "", 0, 0);
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void PrintInfo(StructureTypeInfo type_info, String var_prefix, int varnum, int offset) {
        if (null == type_info) {
            return;
        }
        try {
            STI_TokenizerInterface stiti = type_info.getInfoTokenizer();
            while (stiti.hasMoreTokens()) {
                String token = stiti.nextToken();
                if (null != token) {
                    DebugPrint(varnum + "\t " + token);
                    varnum++;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void CreateCppInitializer(StructureTypeInfo type_info) {
        String info_token = null;
        int r_squareParamIndex = -1;
        int l_squareParamIndex = -1;
        int lastSpaceIndex = -1;
        String array_length_string = null;
        String variable_name = null;
        String cpp_type = null;
        int num_dims = 0;
        type_info.CppConstructor = "";
        String orig_info_token = null;
        //	String trimmed_orig_info_token=null;
        int array_length = 1;
        boolean is_class = false;
        boolean unbounded_array = false;

        try {
            StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            //DebugPrint("Creating Update Function for "+type_info.Name);

            // If this class was derived from another just eat the members of the base class.
            while (info_tokenizer.hasMoreTokens()) {
                info_token = info_tokenizer.nextToken();
                orig_info_token = info_token;
                info_token = info_token.trim();
                //trimmed_orig_info_token=info_token;
                //DebugPrint("info_token = "+info_token);
                array_length_string = null;
                variable_name = null;
                cpp_type = null;
                num_dims = 0;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                l_squareParamIndex = info_token.indexOf('[');
                if (l_squareParamIndex > 0) {
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex);
                }
                array_length = 1;
                while (0 < r_squareParamIndex && 0 < l_squareParamIndex) {
                    array_length_string = info_token.substring(l_squareParamIndex + 1, r_squareParamIndex);
                    array_length *= rcs.utils.StrToInt.convert(array_length_string);
                    if (l_squareParamIndex > 0) {
                        l_squareParamIndex = info_token.indexOf('[', l_squareParamIndex + 1);
                        r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex + 1);
                    }
                }
                lastSpaceIndex = info_token.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *3");
                    continue;
                }
                variable_name = info_token.substring(lastSpaceIndex + 1);
                l_squareParamIndex = variable_name.indexOf('[');
                if (l_squareParamIndex > 0) {
                    variable_name = variable_name.substring(0, l_squareParamIndex);
                }
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs another space. L3295--42%");
                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                    l_squareParamIndex = variable_name.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        variable_name = variable_name.substring(0, l_squareParamIndex);
                    }
                }
                if (variable_name.length() < 1) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- no variable_name.");

                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character.");
                    continue;
                }
                cpp_type = info_token.substring(0, lastSpaceIndex);
                while (true) {
                    if (cpp_type.charAt(0) != ' '
                            && cpp_type.charAt(0) != '\t'
                            && cpp_type.charAt(0) != '\r'
                            && cpp_type.charAt(0) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(1);
                }
                int cpp_type_length = cpp_type.length();
                while (true) {
                    if (cpp_type.charAt(cpp_type_length - 1) != ' '
                            && cpp_type.charAt(cpp_type_length - 1) != '\t'
                            && cpp_type.charAt(cpp_type_length - 1) != '\r'
                            && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                    cpp_type_length = cpp_type.length();
                }
                if (cpp_type.indexOf('*') >= 0) {
                    type_info.contains_pointers = true;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('?') >= 0
                        || cpp_type.indexOf('-') >= 0
                        || cpp_type.indexOf('\\') >= 0
                        || cpp_type.indexOf('/') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('=') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                        || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                        || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                        || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                        || cpp_type.indexOf(',') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains illegal character.");

                    continue;
                }
                is_class = CheckForCppClass(cpp_type);
                unbounded_array = type_info.VarnameUnboundedHashTable.containsKey(variable_name);
                if (debug_on) {
                    DebugPrint("CreateCppInitializer(" + type_info.getName() + ") : variable_name=" + variable_name + ",cpp_type=" + cpp_type + ",is_class=" + is_class + ",unbounded_array=" + unbounded_array + ",array_length=" + array_length);
                }
                if (unbounded_array) {
                    String defv = null;
                    if (cpp_type.equals("char") || cpp_type.equals(unbounded_string + " char")) {
                        defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    }
                    if (defv != null) {
                        type_info.CppConstructor += "\tSET_NML_UNBOUNDED_STRING(\tx->" + variable_name + ",\"" + defv + "\");\n";
                    } else {
                        if (0 == dla_length_init) {
                            type_info.CppConstructor += "\tx->" + variable_name + "=0;\n";
                            type_info.CppConstructor += "\tx->" + variable_name + "_length=0;\n";
                            type_info.CppConstructor += "\tx->" + variable_name + "_size_allocated=0;\n";
                        } else {
                            String varclassname = cpp_type;
                            int spcindex = varclassname.indexOf(' ');
                            if (spcindex > 0 && spcindex < varclassname.length()) {
                                varclassname = varclassname.substring(spcindex + 1);
                            }
                            if (varclassname == null
                                    || varclassname.length() < 1
                                    || varclassname.equals("class")
                                    || varclassname.equals("struct")) {
                                ErrorPrint("Bad varclassname=" + varclassname);
                                continue;
                            }
                            type_info.CppConstructor += "\tx->" + variable_name + "= new " + varclassname + "[" + dla_length_init + "];\n";
                            type_info.CppConstructor += "\tx->" + variable_name + "_length=" + dla_length_init + ";\n";
                            type_info.CppConstructor += "\tx->" + variable_name + "_size_allocated=" + dla_length_init + ";\n";
                            if (is_class) {
                                StructureTypeInfo vartype_info
                                        = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(varclassname);
                                if (!vartype_info.constructor_declared && vartype_info.have_initialize && !IsNMLMsg(varclassname) && !CheckForCppPosemathClass(varclassname)) {
                                    type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + dla_length_init + "; i_" + variable_name + "++)\n\t{\n";
                                    type_info.CppConstructor += "\t\tinitialize_" + varclassname + "( ((" + varclassname + "*)(x->" + variable_name + "))+i_" + variable_name + ");\n";
                                    type_info.CppConstructor += "\t}\n";
                                }
                            } else {
                                type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + dla_length_init + "; i_" + variable_name + "++)\n\t{\n";
                                type_info.CppConstructor += "\t\tx->" + variable_name + "[i_" + variable_name + "]=0;\n";
                                type_info.CppConstructor += "\t}\n";
                            }
                        }
                    }
                    continue;
                }
                if (array_length > 1 && !is_class) {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (cpp_type.equals("char") && defv != null) {
                        int ci = 0;
                        for (ci = 0; ci < defv.length() && ci < array_length - 1; ci++) {
                            type_info.CppConstructor += "\tx->" + variable_name + "[" + ci + "]=\'" + defv.charAt(ci) + "\';\n";
                        }
                        type_info.CppConstructor += "\tx->" + variable_name + "[" + ci + "]=0;\n";
                    } else {
                        type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + array_length + "; i_" + variable_name + "++)\n";
                        type_info.CppConstructor += "\t{\n";
                        String cast_type = cpp_type;
                        if (cpp_type.startsWith("NML_DYNAMIC_LENGTH_ARRAY ")) {
                            cast_type = cast_type.substring(25);
                        }
                        if (defv == null) {
                            if (cast_type.startsWith("enum")) {
                                String enum_name = cast_type;
                                int lastspaceindex = cast_type.lastIndexOf(' ');
                                if (lastspaceindex > 0) {
                                    enum_name = enum_name.substring(lastspaceindex + 1);
                                }
                                EnumTypeInfo enum_info
                                        = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                                String keystring = (String) enum_keys.nextElement();
                                String keytouse = keystring;
                                int ival = -1;
                                while (enum_keys.hasMoreElements()) {
                                    int ival2 = -1;
                                    Integer Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                    ival2 = Ival.intValue();
                                    if (ival == -1) {
                                        ival = ival2;
                                    }
                                    if (ival2 == 0) {
                                        keytouse = keystring;
                                        ival = 0;
                                        break;
                                    }
                                    keystring = (String) enum_keys.nextElement();
                                }
                                type_info.CppConstructor += "\t\t((" + cast_type + "*) x->" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") " + keytouse + "; /*" + ival + "*/\n";
                            } else {
                                type_info.CppConstructor += "\t\t((" + cast_type + "*) x->" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") 0;\n";
                            }
                        } else {
                            type_info.CppConstructor += "\t\t((" + cast_type + "*) x->" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") " + defv + "; /* set by default=  comment */\n";
                        }
                        type_info.CppConstructor += "\t}\n";
                    }
                    continue;
                } else if (is_class) {
                    String varclassname = cpp_type;
                    int vcn_space_index = varclassname.lastIndexOf(' ');
                    if (vcn_space_index > 0) {
                        varclassname = varclassname.substring(vcn_space_index + 1);
                    }
                    StructureTypeInfo vartype_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(varclassname);
                    if (null == vartype_info) {
                        ErrorPrint("Can't find vartype_info for {" + varclassname + "} for " + variable_name + " in " + type_info.getName() + " in ModuleInfo.m_structInfoByNameHashTable.");
                        Enumeration classenum = ModuleInfo.m_structInfoByNameHashTable.keys();
                        while (classenum.hasMoreElements()) {
                            String s = (String) classenum.nextElement();
                            if (debug_on) {
                                DebugPrint("comparing(" + s + ") with (" + varclassname + ") = " + varclassname.equals(s));
                            }
                        }
                    } else {
                        if (!vartype_info.constructor_declared && vartype_info.have_initialize && !IsNMLMsg(varclassname) && !CheckForCppPosemathClass(varclassname)) {
                            if (array_length > 1) {
                                type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + array_length + "; i_" + variable_name + "++)\n";
                                type_info.CppConstructor += "\t{\n";
                                type_info.CppConstructor += "\t\tinitialize_" + varclassname + "( ((" + varclassname + "*)(x->" + variable_name + "))+ i_" + variable_name + ");\n";
                                type_info.CppConstructor += "\t}\n";
                            } else {
                                type_info.CppConstructor += "\tinitialize_" + varclassname + "( &(x->" + variable_name + "));\n";
                            }
                        }
                    }
                    continue;
                } else {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (defv == null) {
                        if (cpp_type.startsWith("enum")) {
                            String enum_name = cpp_type;
                            int lastspaceindex = cpp_type.lastIndexOf(' ');
                            if (lastspaceindex > 0) {
                                enum_name = enum_name.substring(lastspaceindex + 1);
                            }
                            EnumTypeInfo enum_info
                                    = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                            if (enum_info == null) {
                                ErrorPrint("Could not get enum information for (" + enum_name + ")");
                            } else {
                                Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                                String keystring = null;
                                if (!enum_keys.hasMoreElements()) {
                                    ErrorPrint("Messed up enum " + enum_info.Name);
                                } else {
                                    keystring = (String) enum_keys.nextElement();
                                }
                                String keytouse = keystring;
                                int ival = -1;
                                while (enum_keys.hasMoreElements()) {
                                    int ival2 = -1;
                                    Integer Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                    ival2 = Ival.intValue();
                                    if (ival == -1) {
                                        ival = ival2;
                                    }
                                    if (ival2 == 0) {
                                        keytouse = keystring;
                                        ival = 0;
                                        break;
                                    }
                                    keystring = (String) enum_keys.nextElement();
                                }
                                type_info.CppConstructor += "\tx->" + variable_name + " = (" + cpp_type + ") " + keytouse + "; /*" + ival + "*/\n";
                            }
                        } else {
                            if (type_info.VarnameNDLAHashTable.containsKey(variable_name)) {
                                type_info.CppConstructor += "\tx->" + variable_name + " = (" + cpp_type + ") " + dla_length_init + ";\n";
                            } else {
                                type_info.CppConstructor += "\tx->" + variable_name + " = (" + cpp_type + ") 0;\n";
                            }
                        }
                    } else {
                        type_info.CppConstructor += "\tx->" + variable_name + " = (" + cpp_type + ") " + defv + "; /* set by default= comment */\n";
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("Error Generating C++ Initializer for " + type_info.getName());
            ErrorPrint("type_info.DerivedFrom = " + type_info.DerivedFrom);
            ErrorPrint("type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo);
            ErrorPrint("type_info.RawInfo = " + type_info.RawInfo);
            ErrorPrint("type_info.HiddenInfo = " + type_info.HiddenInfo);
            ErrorPrint("type_info.CppUpdateFunction = " + type_info.CppUpdateFunction);
            ErrorPrint("info_token = " + info_token);
            ErrorPrint("array_length_string = " + array_length_string);
            ErrorPrint("variable_name = " + variable_name);
            ErrorPrint("cpp_type = " + cpp_type);
            ErrorPrint("num_dims = " + num_dims);
            ErrorPrint("orig_info_token = " + orig_info_token);
            ErrorPrint("array_length = " + array_length);
            ErrorPrint("is_class = " + is_class);
        }
    }

    public void CreateCppConstructor(StructureTypeInfo type_info) {
        String info_token = null;
        int r_squareParamIndex = -1;
        int l_squareParamIndex = -1;
        int lastSpaceIndex = -1;
        String array_length_string = null;
        String variable_name = null;
        String cpp_type = null;
        int num_dims = 0;
        type_info.CppConstructor = "";
        String orig_info_token = null;
        //String trimmed_orig_info_token=null;
        int array_length = 1;
        boolean is_class = false;
        boolean unbounded_array = false;
        try {
            StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            //DebugPrint("Creating Update Function for "+type_info.Name);

            // If this class was derived from another just eat the members of the base class.
            while (info_tokenizer.hasMoreTokens()) {
                info_token = info_tokenizer.nextToken();
                orig_info_token = info_token;
                info_token = info_token.trim();
                //trimmed_orig_info_token=info_token;
                //DebugPrint("info_token = "+info_token);
                array_length_string = null;
                variable_name = null;
                cpp_type = null;
                num_dims = 0;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                l_squareParamIndex = info_token.indexOf('[');
                if (l_squareParamIndex > 0) {
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex);
                }
                array_length = 1;
                while (0 < r_squareParamIndex && 0 < l_squareParamIndex) {
                    array_length_string = info_token.substring(l_squareParamIndex + 1, r_squareParamIndex);
                    array_length *= rcs.utils.StrToInt.convert(array_length_string);
                    if (l_squareParamIndex > 0) {
                        l_squareParamIndex = info_token.indexOf('[', l_squareParamIndex + 1);
                        r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex + 1);
                    }
                }
                lastSpaceIndex = info_token.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *4");
                    continue;
                }
                variable_name = info_token.substring(lastSpaceIndex + 1);
                l_squareParamIndex = variable_name.indexOf('[');
                if (l_squareParamIndex > 0) {
                    variable_name = variable_name.substring(0, l_squareParamIndex);
                }
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs another space. L3722--48%");

                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                    l_squareParamIndex = variable_name.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        variable_name = variable_name.substring(0, l_squareParamIndex);
                    }
                }
                if (variable_name.length() < 1) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- no variable_name");

                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- variable_name (" + variable_name + ") contains illegal character.");
                    continue;
                }
                cpp_type = info_token.substring(0, lastSpaceIndex);
                while (true) {
                    if (cpp_type.charAt(0) != ' '
                            && cpp_type.charAt(0) != '\t'
                            && cpp_type.charAt(0) != '\r'
                            && cpp_type.charAt(0) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(1);
                }
                int cpp_type_length = cpp_type.length();
                while (true) {
                    if (cpp_type.charAt(cpp_type_length - 1) != ' '
                            && cpp_type.charAt(cpp_type_length - 1) != '\t'
                            && cpp_type.charAt(cpp_type_length - 1) != '\r'
                            && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                    cpp_type_length = cpp_type.length();
                }
                if (cpp_type.indexOf('*') >= 0) {
                    type_info.contains_pointers = true;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('?') >= 0
                        || cpp_type.indexOf('-') >= 0
                        || cpp_type.indexOf('\\') >= 0
                        || cpp_type.indexOf('/') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('=') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                        || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                        || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                        || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                        || cpp_type.indexOf(',') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains illegal character.");

                    continue;
                }
                is_class = CheckForCppClass(cpp_type);
                unbounded_array = type_info.VarnameUnboundedHashTable.containsKey(variable_name);
                if (debug_on) {
                    DebugPrint("CreateCppConstructor(" + type_info.getName() + ") : variable_name=" + variable_name + ",cpp_type=" + cpp_type + ",is_class=" + is_class + ",unbounded_array=" + unbounded_array + ",array_length=" + array_length);
                }
                if (unbounded_array) {
                    try {
                        String defv = null;
                        if (cpp_type.equals("char") || cpp_type.equals(unbounded_string + " char")) {
                            defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                        }
                        if (defv != null) {
                            type_info.CppConstructor += "\tSET_NML_UNBOUNDED_STRING(\t" + variable_name + ",\"" + defv + "\");\n";
                        } else {
                            if (0 == dla_length_init) {
                                type_info.CppConstructor += "\t" + variable_name + "=0;\n";
                                type_info.CppConstructor += "\t" + variable_name + "_length=0;\n";
                                type_info.CppConstructor += "\t" + variable_name + "_size_allocated=0;\n";
                            } else {
                                String varclassname = cpp_type;
                                int vcn_space_index = varclassname.lastIndexOf(' ');
                                if (vcn_space_index > 0) {
                                    varclassname = varclassname.substring(vcn_space_index + 1);
                                }
                                type_info.CppConstructor += "\t" + variable_name + "= new " + varclassname + "[" + dla_length_init + "];\n";
                                type_info.CppConstructor += "\t" + variable_name + "_length=" + dla_length_init + ";\n";
                                type_info.CppConstructor += "\t" + variable_name + "_size_allocated=" + dla_length_init + ";\n";
                                if (is_class) {
                                    StructureTypeInfo vartype_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(varclassname);
                                    if (vartype_info == null) {
                                        Enumeration classenum = ModuleInfo.m_structInfoByNameHashTable.keys();
                                        ErrorPrint("Can't find vartype_info for {" + varclassname + "} in ");
                                        while (classenum.hasMoreElements()) {
                                            String s = (String) classenum.nextElement();
                                            ErrorPrint("comparing(" + s + ") with (" + varclassname + ") = " + varclassname.equals(s));
                                        }
                                    } else {
                                        if (!vartype_info.constructor_declared && vartype_info.have_initialize
                                                && !IsNMLMsg(varclassname) && !CheckForCppPosemathClass(varclassname)) {
                                            type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + dla_length_init + "; i_" + variable_name + "++)\n\t{\n";
                                            type_info.CppConstructor += "\t\tinitialize_" + varclassname + "( ((" + varclassname + "*)(" + variable_name + "))+i_" + variable_name + ");\n";
                                            type_info.CppConstructor += "\t}\n";
                                        }
                                    }
                                } else {
                                    type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + dla_length_init + "; i_" + variable_name + "++)\n\t{\n";
                                    type_info.CppConstructor += "\t\t" + variable_name + "[i_" + variable_name + "]=0;\n";
                                    type_info.CppConstructor += "\t}\n";
                                }
                            }
                        }
                    } catch (Exception e) {
                        ErrorPrint("variable_name=" + variable_name);
                        e.printStackTrace();
                    }
                    continue;
                }
                if (array_length > 1 && !is_class) {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (cpp_type.equals("char") && defv != null) {
                        int ci = 0;
                        for (ci = 0; ci < defv.length() && ci < array_length - 1; ci++) {
                            type_info.CppConstructor += "\t" + variable_name + "[" + ci + "]=\'" + defv.charAt(ci) + "\';\n";
                        }
                        type_info.CppConstructor += "\t" + variable_name + "[" + ci + "]=0;\n";
                    } else {
                        type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + array_length + "; i_" + variable_name + "++)\n";
                        type_info.CppConstructor += "\t{\n";
                        String cast_type = cpp_type;
                        if (cpp_type.startsWith("NML_DYNAMIC_LENGTH_ARRAY ")) {
                            cast_type = cast_type.substring(25);
                        }
                        if (defv == null) {
                            if (cast_type.startsWith("enum")) {
                                String enum_name = cast_type;
                                int lastspaceindex = cast_type.lastIndexOf(' ');
                                if (lastspaceindex > 0) {
                                    enum_name = enum_name.substring(lastspaceindex + 1);
                                }
                                EnumTypeInfo enum_info
                                        = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                                Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                                String keystring = (String) enum_keys.nextElement();
                                String keytouse = keystring;
                                int ival = -1;
                                while (enum_keys.hasMoreElements()) {
                                    int ival2 = -1;
                                    Integer Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                    ival2 = Ival.intValue();
                                    if (ival == -1) {
                                        ival = ival2;
                                    }
                                    if (ival2 == 0) {
                                        keytouse = keystring;
                                        ival = 0;
                                        break;
                                    }
                                    keystring = (String) enum_keys.nextElement();
                                }
                                type_info.CppConstructor += "\t\t((" + cast_type + "*)" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") " + keytouse + "; /*" + ival + "*/\n";
                            } else {
                                type_info.CppConstructor += "\t\t((" + cast_type + "*)" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") 0;\n";
                            }
                        } else {
                            type_info.CppConstructor += "\t\t((" + cast_type + "*)" + variable_name + ")[i_" + variable_name + "]  = (" + cast_type + ") " + defv + "; /* set by default= comment */\n";
                        }
                        type_info.CppConstructor += "\t}\n";
                    }
                    continue;
                } else if (is_class) {
                    String varclassname = cpp_type;
                    int vcn_space_index = varclassname.lastIndexOf(' ');
                    if (vcn_space_index > 0) {
                        varclassname = varclassname.substring(vcn_space_index + 1);
                    }
                    StructureTypeInfo vartype_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(varclassname);
                    if (vartype_info == null) {
                        ErrorPrint("Can't find vartype_info for {" + varclassname + "} in ModuleInfo.m_structInfoByNameHashTable");
                        Enumeration classenum = ModuleInfo.m_structInfoByNameHashTable.keys();
                        while (classenum.hasMoreElements()) {
                            String s = (String) classenum.nextElement();
                            ErrorPrint("comparing(" + s + ") with (" + varclassname + ") = " + varclassname.equals(s));
                        }
                    } else {
                        if (!vartype_info.constructor_declared && vartype_info.have_initialize
                                && !IsNMLMsg(varclassname) && !CheckForCppPosemathClass(varclassname)) {
                            if (array_length > 1) {
                                type_info.CppConstructor += "\tfor(int i_" + variable_name + "=0; i_" + variable_name + "< " + array_length + "; i_" + variable_name + "++)\n";
                                type_info.CppConstructor += "\t{\n";
                                type_info.CppConstructor += "\t\tinitialize_" + varclassname + "(((" + varclassname + "*)" + variable_name + ")+i_" + variable_name + ");\n";
                                type_info.CppConstructor += "\t}\n";
                            } else {
                                type_info.CppConstructor += "\tinitialize_" + varclassname + "( &(" + variable_name + "));\n";
                            }
                        }
                    }
                    continue;
                } else {
                    String defv = (String) type_info.VarnameToDefaultsHashTable.get(variable_name);
                    if (defv == null) {
                        if (cpp_type.startsWith("enum ")) {
                            String enum_name = cpp_type;
                            int lastspaceindex = cpp_type.lastIndexOf(' ');
                            if (lastspaceindex > 0) {
                                enum_name = enum_name.substring(lastspaceindex + 1);
                            }
                            EnumTypeInfo enum_info
                                    = (EnumTypeInfo) ModuleInfo.m_enumInfoHashTable.get(enum_name);
                            if (null == enum_info) {
                                ErrorPrint("No enum information for " + enum_name);
                                break;
                            }
                            if (debug_on) {
                                DebugPrint("variable_name=" + variable_name + ",enum_name=" + enum_name + ",enum_info=" + enum_info);
                            }
                            Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                            String keystring = (String) enum_keys.nextElement();
                            String keytouse = keystring;
                            int ival = -1;
                            while (enum_keys.hasMoreElements()) {
                                int ival2 = -1;
                                Integer Ival = (Integer) enum_info.reverse_hashtable.get(keystring);
                                ival2 = Ival.intValue();
                                if (ival == -1) {
                                    ival = ival2;
                                }
                                if (ival2 == 0) {
                                    keytouse = keystring;
                                    ival = 0;
                                    break;
                                }
                                keystring = (String) enum_keys.nextElement();
                            }
                            type_info.CppConstructor += "\t" + variable_name + " = (" + cpp_type + ") " + keytouse + "; /*" + ival + "*/\n";
                        } else {
                            if (type_info.VarnameNDLAHashTable.containsKey(variable_name)) {
                                type_info.CppConstructor += "\t" + variable_name + " = (" + cpp_type + ") " + dla_length_init + ";\n";
                            } else {
                                type_info.CppConstructor += "\t" + variable_name + " = (" + cpp_type + ") 0;\n";
                            }
                        }
                    } else {
                        type_info.CppConstructor += "\t" + variable_name + " = (" + cpp_type + ") " + defv + "; /* set by default= comment */\n";
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            if (debug_on) {
                e.printStackTrace(System.out);
            }
            ErrorPrint("Error Generating C++ Constructor for " + type_info.getName());
            ErrorPrint("type_info.DerivedFrom = " + type_info.DerivedFrom);
            ErrorPrint("type_info.PreFinalPassInfo = " + type_info.PreFinalPassInfo);
            ErrorPrint("type_info.RawInfo = " + type_info.RawInfo);
            ErrorPrint("type_info.HiddenInfo = " + type_info.HiddenInfo);
            ErrorPrint("type_info.CppUpdateFunction = " + type_info.CppUpdateFunction);
            ErrorPrint("info_token = " + info_token);
            ErrorPrint("array_length_string = " + array_length_string);
            ErrorPrint("variable_name = " + variable_name);
            ErrorPrint("cpp_type = " + cpp_type);
            ErrorPrint("num_dims = " + num_dims);
            ErrorPrint("orig_info_token = " + orig_info_token);
            ErrorPrint("array_length = " + array_length);
            ErrorPrint("is_class = " + is_class);
        }
    }

    public void GenerateJavaClass(String class_name) {
        try {
            String jclassname = class_name.replace(':', '_');
            if (null != output_file_name) {
                if (output_file_name.startsWith("*")) {
                    SetOutputFile(jclassname + ".java");
                    first_java_class = true;
                }
            }
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(class_name);
            if (null == type_info.JavaDefinition) {
                CreateJavaDefinition(type_info);
            }
            if (null == type_info.JavaDefinition) {
                ErrorPrint("Can't generate java class for " + class_name + " : type_info=" + type_info);
//                debug_on = true;
//                CreateJavaDefinition(type_info);
                return;
            }

            if (IsNonUpdatebleClass(type_info)) {
                return;
            }

            if (first_java_class) {
                javaFileName = class_name + ".java";
                WriteOutput("/*\n*\tNew Java File starts here.\n*\tThis file should be named " + javaFileName + "\n");
                WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
                if (null != type_info.fromFileName) {
                    WriteOutput("*\tfrom " + type_info.fromFileName + ":" + type_info.fromLineNumber + "\n");
                }
                WriteOutput("*\twith command line arguments : " + orig_args_one_string + "\n");
//                WriteOutput("*\tRCS_VERSION=" + rcs.RCS_VERSION.version_string);
                if (null != prev_lines_of_script) {
                    WriteOutput("*\n");
                    WriteOutput("*\t.gen script :\n");
                    if (null != lines_of_script) {
                        for (int i = 0; i < lines_of_script.length; i++) {
                            String s = (String) lines_of_script[i];
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    } else {
                        for (int i = 0; i < prev_lines_of_script.size(); i++) {
                            String s = (String) prev_lines_of_script.get(i);
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    }
                    WriteOutput("*\n");
                }
                WriteOutput("*/\n\n");
                if (java_package_name != null) {
                    WriteOutput("\n// Set Package Name\npackage " + java_package_name + ";\n\n");
                }
                WriteOutput("// Import all NML and posemath interfaces\nimport rcs.nml.*;\nimport rcs.posemath.*;\n\n");
            }
            java_classes_written++;
            total_java_classes_written++;
            WriteOutput("/*\n*\tClass definition for " + class_name + "\n");
            WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
            WriteOutput("*/\n");
            if (first_java_class) {
                if (type_info.DerivedFrom != null) {
                    WriteOutput("public class " + jclassname + " extends " + type_info.DerivedFrom.replace(':', '_') + " implements Cloneable\n{\n");
                } else {
                    WriteOutput("public class " + jclassname + " implements Cloneable\n{\n");
                }
            } else {
                if (type_info.DerivedFrom != null) {
                    WriteOutput("class " + jclassname + " extends " + type_info.DerivedFrom.replace(':', '_') + "\n{\n");
                } else {
                    WriteOutput("class " + jclassname + "\n{\n");
                }
            }

            if (null != type_info.JavaDefinition) {
                WriteOutput(type_info.JavaDefinition);
            }

            if (type_info.Id > 0 && null != type_info.DerivedFrom) {
                WriteOutput("\n\n\t// Constructor \n\tpublic " + jclassname + "()\n\t{\n\t\tsuper(" + type_info.Id + ");\n" + type_info.JavaClassArrayInitializers + "\n\t}\n\n");
                WriteOutput("\n\n\t// Constructor that should be used by any classes that extend this class. \n\tprotected " + jclassname + "(int _type)\n\t{\n\t\tsuper(_type);\n" + type_info.JavaClassArrayInitializers + "\n\t}\n\n");
            } else {
                WriteOutput("\n\n\t// Constructor \n\tpublic " + jclassname + "()\n\t{\n" + type_info.JavaClassArrayInitializers + "\n\t}\n\n");
                String top_parent = type_info.DerivedFrom;
                while (null != top_parent) {
                    if (top_parent.equals("NMLmsg") || top_parent.equals("RCS_CMD_MSG") || top_parent.equals("RCS_STAT_MSG")) {
                        WriteOutput("\n\t/*\n\t* WARNING\n\t* This class was derived from NMLmsg but no ID was found for it.");
                        WriteOutput("\n\t* The CodeGenerator assumes that this class will be used as a base class for other NML message classes.");
                        WriteOutput("\n\t* If this is not correct make sure that " + class_name + "_TYPE was defined to be a unique integer greater than zero.");
                        WriteOutput("\n\t*/");
                        WriteOutput("\n\n\t// Constructor \n\tpublic " + jclassname + "(int nml_type)\n\t{\n\t\tsuper(nml_type);\n" + type_info.JavaClassArrayInitializers + "\n\t}\n\n");
                        break;
                    }
                    StructureTypeInfo parentInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(top_parent);
                    if (null == parentInfo) {
                        break;
                    }
                    top_parent = parentInfo.DerivedFrom;
                }
            }

            WriteOutput("\n\tpublic void update(NMLFormatConverter nml_fc)\n\t{\n");
            if (null == type_info.JavaUpdateFunction) {
                CreateJavaUpdateFunction(type_info);
            }
            if (null != type_info.JavaUpdateFunction) {
                WriteOutput(type_info.JavaUpdateFunction);
            }
            WriteOutput("\n\t}\n\n");

            WriteOutput("\n\tpublic " + jclassname + " clone() throws CloneNotSupportedException\n\t{\n");
            WriteOutput("\t\t" + jclassname + " cloned_object = (" + jclassname + ") super.clone();\n");
            StringTokenizer tz = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            while (tz.hasMoreTokens()) {
                String vardef = tz.nextToken();
                int sq_brace_index = vardef.indexOf('[');
                int last_space_index = vardef.lastIndexOf(' ');
                if (sq_brace_index > 0) {
                    last_space_index = vardef.lastIndexOf(' ', sq_brace_index - 1);
                }
                String varname = vardef.substring(last_space_index + 1);
                sq_brace_index = varname.indexOf('[');
                boolean is_array = false;
                int array_dim = 0;
                if (sq_brace_index > 0) {
                    is_array = true;
                    varname = varname.substring(0, sq_brace_index);
                }
                String array_string = " ";
                if (is_array) {
                    sq_brace_index = -1;
                    do {
                        sq_brace_index = varname.indexOf('[', sq_brace_index + 1);
                        array_dim++;
                        array_string += "[]";
                    } while (sq_brace_index > 0);
                } else if (vardef.indexOf(CodeGenCommon.unbounded_string) >= 0) {
                    is_array = true;
                    array_string = "[]";
                }
                String vartype = vardef.substring(0, last_space_index);
                boolean is_class = this.CheckForCppClass(vartype);
                if (is_class || is_array) {
                    WriteOutput("\t\tif(this." + varname + " != null) {\n");
                    String java_type = ConvertCppTypeToJavaType(vartype);
                    WriteOutput("\t\t\tcloned_object." + varname + " = (" + java_type + array_string + ") this." + varname + ".clone();\n");
                    if (is_class && is_array) {
                        if (array_dim != 1) {
                            WriteOutput("\t\tthrow  new CloneNotSupportedException(\"Cloning multidimensional arrays of structures not implemented for variable " + varname + "\");\n");
                        } else {
                            WriteOutput("\t\t\tfor(int i_" + varname + "=0; i_" + varname + " < this." + varname + ".length; i_" + varname + "++) {\n");
                            WriteOutput("\t\t\t\tif(this." + varname + "[i_" + varname + "] != null) {\n");
                            WriteOutput("\t\t\t\t\tcloned_object." + varname + "[i_" + varname + "] = (" + java_type + ") this." + varname + "[i_" + varname + "].clone();\n");
                            WriteOutput("\t\t\t\t}\n");
                            WriteOutput("\t\t\t}\n");
                        }
                    }
                    WriteOutput("\t\t}\n");
                }
            }
            WriteOutput("\t\treturn cloned_object;\n");
            WriteOutput("\n\t}\n\n");
            if (CodeGenCommon.add_java_getters_and_setters) {
                StringTokenizer line_tokenizer = new StringTokenizer(type_info.JavaDefinition, "\r\n");
                WriteOutput("\n\t/* Getters and Setters */\n");
                while (line_tokenizer.hasMoreTokens()) {
                    String line_token = line_tokenizer.nextToken();
                    int semicolonindex = line_token.indexOf(';');
                    if (semicolonindex > 0) {
                        line_token = line_token.substring(0, semicolonindex);
                    }
                    int eqindex = line_token.indexOf('=');
                    if (eqindex > 0) {
                        line_token = line_token.substring(0, eqindex);
                    }
                    if (line_token.length() < 2) {
                        continue;
                    }
                    String words[] = line_token.split("[ \t]+");
                    if (words.length < 2) {
                        continue;
                    }
                    boolean finalfound = false;
                    for (int i = 0; i < words.length; i++) {
                        if (words[i].equals("final")) {
                            finalfound = true;
                            break;
                        }
                    }
                    if (finalfound) {
                        continue;
                    }
                    String java_type = words[words.length - 2];
                    String variable_name = words[words.length - 1];
                    int sqbindex = variable_name.indexOf('[');
                    if (sqbindex > 0) {
                        java_type += " " + variable_name.substring(sqbindex);
                        variable_name = variable_name.substring(0, sqbindex);
                    }
                    WriteOutput("\tpublic " + java_type + " get" + variable_name + "() {\n\t\treturn (" + variable_name + ");\n\t}\n");
                    WriteOutput("\tpublic void set" + variable_name + "(" + java_type + " _" + variable_name + ")  {\n\t\tthis." + variable_name + "=_" + variable_name + ";\n\t}\n");
                    WriteOutput("\n");
                }
            }
            WriteOutput("\n}\n\n");
            first_java_class = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateJavaClasses() {
        java_classes_written = 0;
        File orig_current_dir = null;
        try {
            orig_current_dir = current_directory;
        } catch (Exception e) {
        }
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.GenerateJavaClasses() called.");
            }
            generating_code = true;
            String selected_classes[] = ClassList.getSelectedItems();
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateJavaClasses() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateJavaClasses() : selected_classes.length = " + selected_classes.length);
                }
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating Java Classes . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }
            if (null != selected_classes && selected_classes.length > 0
                    && null != java_package_name
                    && null == current_directory) {
                String java_dir_name = "";
                StringTokenizer jtokenizer = new StringTokenizer(java_package_name, ".");
                while (jtokenizer.hasMoreTokens()) {
                    java_dir_name += jtokenizer.nextToken();
                    if (jtokenizer.hasMoreTokens()) {
                        java_dir_name += File.separator;
                    }
                    current_directory = new File(java_dir_name);
                    current_directory.mkdirs();
                }
            }
            for (int i = 0; i < selected_classes.length; i++) {
                if (debug_on) {
                    DebugPrint("selected_classes[i=" + i + "]=" + selected_classes[i]);
                }
                GenerateJavaClass(selected_classes[i]);
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(i);
                    m_loadingPanel.force_repaint(500);
                }
            }
            generating_code = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            current_directory = orig_current_dir;
        } catch (Exception e) {
        }

        if (java_classes_written < 1) {
            ErrorPrint("CodeGenCommon.GenerateJavaClasses() but no classes were written.\n");
        }
        generate_java_classes_needed = false;
    }
    Vector nonUpdateableClasses = new Vector();
    // @SuppressWarnings("unchecked")

    public boolean IsNonUpdatebleClass(StructureTypeInfo type_info) {
        try {
            if (type_info.contains_pointers
                    || type_info.contains_unrecognized_type) {
                return true;
            }
            if (type_info.getName().compareTo("NML") == 0) {
                return true;
            }
            if (type_info.getName().compareTo("RCS_CMD_CHANNEL") == 0) {
                return true;
            }
            if (type_info.getName().compareTo("RCS_STAT_CHANNEL") == 0) {
                return true;
            }
            for (int i = 0; i < nonUpdateableClasses.size(); i++) {
                String badclassname = (String) nonUpdateableClasses.elementAt(i);
                if (null == badclassname) {
                    break;
                }
                if (type_info.getName().compareTo(badclassname) == 0) {
                    return true;
                }
            }
            if (null == type_info.DerivedFrom) {
                return false;
            }
            for (int i = 0; i < nonUpdateableClasses.size(); i++) {
                String badclassname = (String) nonUpdateableClasses.elementAt(i);
                if (null == badclassname) {
                    break;
                }
                if (type_info.DerivedFrom.compareTo(badclassname) == 0) {
                    nonUpdateableClasses.addElement(type_info.getName());
                    return true;
                }
            }
            if (IsNMLMsg(type_info.getName())) {
                return false;
            }
            if (type_info.DerivedFrom.compareTo("NML") == 0) {
                nonUpdateableClasses.addElement(type_info.getName());
                return true;
            }
            if (type_info.DerivedFrom.compareTo("RCS_CMD_CHANNEL") == 0) {
                nonUpdateableClasses.addElement(type_info.getName());
                return true;
            }
            if (type_info.DerivedFrom.compareTo("RCS_STAT_CHANNEL") == 0) {
                nonUpdateableClasses.addElement(type_info.getName());
                return true;
            }
            StructureTypeInfo parent_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(type_info.DerivedFrom);
            if (parent_info == null) {
                return false;
            }
            return IsNonUpdatebleClass(parent_info);

        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    public void GenerateC_UpdateFunction(String class_name) {
        try {
            C_Generator.GenerateC_UpdateFunction(class_name, this, ModuleInfo.m_structInfoByNameHashTable);
            first_cpp_function = false;
            cpp_updates_written++;
            total_cpp_updates_written++;
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("Error Generating C Update function for " + class_name);

        }
    }

    public void GenerateCppUpdateFunction(String class_name) {
        StructureTypeInfo type_info;
        try {
            if (class_name == null
                    || class_name.length() < 1
                    || class_name.equals("class")
                    || class_name.equals("struct")) {
                ErrorPrint("Bad class_name=" + class_name + " in GenerateCppUpdateFunction.\n");
                return;
            }
            GenerateCppStartOfFile();

            type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(class_name);
            if (null == type_info) {
                return;
            }
            if (IsNonUpdatebleClass(type_info)) {
                return;
            }
            cpp_updates_written++;
            total_cpp_updates_written++;
            WriteOutput("\n/*\n*\tNML/CMS Update function for " + class_name + "\n");
            if (null != type_info.fromFileName) {
                WriteOutput("*\tfrom " + type_info.fromFileName + ":" + type_info.fromLineNumber + "\n");
            }
            WriteOutput("*/\n");

            if (IsNMLMsg(class_name)) {
                WriteOutput("void " + class_name + "::update(CMS *cms)\n{\n");
            } else {
                if (type_info.is_union) {
                    WriteOutput("void " + StringFuncs.replace_white_space(class_name) + "_update_union(int union_selector,CMS *cms," + class_name + " *x)\n{\n");
                } else {
                    if (prefix_nonmsg_update) {
                        WriteOutput("void " + StringFuncs.replace_white_space(class_name) + "_update(CMS *cms," + class_name + " *x)\n{\n");
                    } else {
                        WriteOutput("void nmlupdate(CMS *cms," + class_name + " *x)\n{\n");
                    }
                }
            }
            if (null == type_info.CppUpdateFunction) {
                CreateCppUpdateFunction(type_info);
            }
            if (null != type_info.CppUpdateFunction) {
                WriteOutput(type_info.CppUpdateFunction);
            }
            WriteOutput("\n}\n\n");
            first_cpp_function = false;
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("Error Generating C++ Update function for " + class_name);

        }
    }

    public void WriteOutput(String str) {
        if (null != fos) {
            try {
                if (debug_on) {
                    Throwable t = new Throwable();
                    System.out.println(StackTracePrinter.ThrowableToShortList(t) + " ---WriteOutput--> " + str);
                }
                byte b[] = str.getBytes();
                fos.write(b, 0, str.length());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if (null != codeTextArea && display_on) {
            codeTextArea.append(str);
        }
    }

    public void GenerateCppInitializer(String class_name) {
        StructureTypeInfo type_info;
        try {
            GenerateCppStartOfFile();

            type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(class_name);
            if (null == type_info) {
                ErrorPrint("Can not find StructureTypeInfo for " + class_name);
                return;
            }

            type_info.have_initialize = true;

            if (IsNonUpdatebleClass(type_info)) {
                return;
            }

            WriteOutput("/*\n*\tInitializer for " + class_name + "\n");
            WriteOutput("*/\n");

            WriteOutput("void initialize_" + class_name + "(" + class_name + "* x)\n{\n");
            if (type_info.DerivedFrom != null) {
                WriteOutput("\tinitialize_" + type_info.DerivedFrom + "(x);\n");
            }
            if (null == type_info.CppConstructor) {
                CreateCppInitializer(type_info);
            }
            if (null != type_info.CppConstructor) {
                WriteOutput(type_info.CppConstructor);
            }
            WriteOutput("\n}\n\n");
            first_cpp_function = false;
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("Error Generating C++ Update function for " + class_name);

        }
    }

    public void GenerateCppConstructor(String class_name) {
        StructureTypeInfo type_info;
        try {
            GenerateCppStartOfFile();

            type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(class_name);
            if (null == type_info) {
                ErrorPrint("Can not find StructureTypeInfo for " + class_name);
                return;
            }

            if (IsNonUpdatebleClass(type_info)) {
                return;
            }

            WriteOutput("/*\n*\tConstructor for " + class_name + "\n");
            if (null != type_info.fromFileName) {
                WriteOutput("*\tfrom " + type_info.fromFileName + ":" + type_info.fromLineNumber + "\n");
            }
            WriteOutput("*/\n");

            if (type_info.Id > 0 && type_info.DerivedFrom != null) {
                WriteOutput(class_name + "::" + class_name + "()\n"
                        + "\t: " + type_info.DerivedFrom + "(" + class_name + "_TYPE,sizeof(" + class_name + "))\n{\n");
            } else {
                WriteOutput(class_name + "::" + class_name + "()\n{\n");
            }
            if (null == type_info.CppConstructor) {
                CreateCppConstructor(type_info);
            }
            if (null != type_info.CppConstructor) {
                WriteOutput(type_info.CppConstructor);
            }
            WriteOutput("\n}\n\n");
            first_cpp_function = false;
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("Error Generating C++ Update function for " + class_name);

        }
    }

    public void GenerateC_UpdateFunctions() {
        cpp_updates_written = 0;
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.GenerateC_UpdateFunctions() called.");
            }
            generating_code = true;
            String selected_classes[] = ClassList.getSelectedItems();
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateC_UpdateFunctions() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateC_UpdateFunctions() : selected_classes.length = " + selected_classes.length);
                }
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating C Update Functions . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(2 * selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }

            for (int i = 0; i < selected_classes.length; i++) {
                GenerateC_UpdateFunction(selected_classes[i]);
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.force_repaint(500);
                }
            }
            generate_cpp_update_functions_needed = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = false;
        if (cpp_updates_written < 1) {
            ErrorPrint("GenerateC_UpdateFunctions() called but no updates were written.\n");
        }
    }

    public void GenerateCppUpdateFunctions() {
        if (ModuleInfo.codegen_generate_format_only) {
            return;
        }
        cpp_updates_written = 0;
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.GenerateCppUpdateFunctions() called.");
            }
            generating_code = true;
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateCppUpdateFunctions() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateCppUpdateFunctions() : selected_classes.length = " + selected_classes.length);
                }
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating C++ Update Functions . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(2 * selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }

            for (int i = 0; i < selected_classes.length; i++) {
                GenerateCppUpdateFunction(selected_classes[i]);
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.force_repaint(500);
                }
            }
            generate_cpp_update_functions_needed = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = false;
        if (cpp_updates_written < 1) {
            ErrorPrint("GenerateCppUpdateFunctions() called but no updates were written.\n");
        }
    }

    public void GenerateCppConstructors() {
        try {
            if (ModuleInfo.codegen_generate_format_only) {
                return;
            }
            if (debug_on) {
                DebugPrint("CodeGenCommon.GenerateCppConstructors() called.");
            }
            generating_code = true;
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateCppConstructors() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateCppConstructors() : selected_classes.length = " + selected_classes.length);
                }
            }

            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating C++ Constructors and Destructors . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }
            for (int i = 0; i < selected_classes.length; i++) {
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(i);
                    m_loadingPanel.force_repaint(500);
                }
                StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (debug_on) {
                    DebugPrint("type_info=" + type_info);
                }
                if (type_info.constructor_declared_and_not_inlined) {
                    GenerateCppConstructor(selected_classes[i]);
                } else if (!type_info.constructor_declared && type_info.Id < 1) {
                    GenerateCppInitializer(selected_classes[i]);
                }
                if (type_info.destructor_declared_and_not_inlined) {
                    WriteOutput("// Destructor\n");
                    WriteOutput(type_info.getName());
                }
            }
            generate_cpp_constructors_needed = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = false;

    }

    public String find_common_root(String strs[]) {
        try {
            if (null == strs) {
                return null;
            }
            if (strs.length < 1) {
                return null;
            }
            String common_root = strs[0];
            for (int i = 1; i < strs.length; i++) {
                for (int j = 1; j < common_root.length() && j < strs[i].length(); j++) {

                    if (!strs[i].regionMatches(true, 0, common_root, 0, j)) {
                        common_root = common_root.substring(0, j - 1);
                        break;
                    }
                }
            }
            return common_root;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

    }

    public boolean IsRcsStatMsg(String classname) {
        try {
            if (debug_on) {
                DebugPrint(" IsRcsStatMsg(" + classname + ") called.");
            }
            if (classname.equals("RCS_STAT_MSG")) {
                return true;
            }
            if (classname.equals("RCS_STAT_MSG_V2")) {
                return true;
            }
            StructureTypeInfo var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(classname);
            //		StructureTypeInfo last_var_class_type_info = var_class_type_info;
            if (debug_on) {
                DebugPrint("var_class_type_info =" + var_class_type_info);
            }
            while (var_class_type_info != null) {
                if (null == var_class_type_info.DerivedFrom) {
                    return false;
                }
                if (debug_on) {
                    DebugPrint("var_class_type_info.DerivedFrom =" + var_class_type_info.DerivedFrom);
                }
                if (var_class_type_info.DerivedFrom.equals("RCS_CMD_MSG")
                        || var_class_type_info.DerivedFrom.equals("RCS_WM_MSG")
                        || var_class_type_info.DerivedFrom.equals("NMLmsg")
                        || var_class_type_info.DerivedFrom.equals("NML_QUERY_MSG")) {
                    return false;
                }
                if (var_class_type_info.DerivedFrom.equals("RCS_STAT_MSG")) {
                    return true;
                }
                //last_var_class_type_info = var_class_type_info;
                var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(var_class_type_info.DerivedFrom);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean IsRcsStatMsgV2(String classname) {
        try {
            if (debug_on) {
                DebugPrint(" IsRcsStatMsg(" + classname + ") called.");
            }
            if (classname.equals("RCS_STAT_MSG")) {
                return false;
            }
            if (classname.equals("RCS_STAT_MSG_V2")) {
                return true;
            }
            StructureTypeInfo var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(classname);
            //StructureTypeInfo last_var_class_type_info = var_class_type_info;
            if (debug_on) {
                DebugPrint("var_class_type_info =" + var_class_type_info);
            }
            while (var_class_type_info != null) {
                if (null == var_class_type_info.DerivedFrom) {
                    return false;
                }
                if (debug_on) {
                    DebugPrint("var_class_type_info.DerivedFrom =" + var_class_type_info.DerivedFrom);
                }
                if (var_class_type_info.DerivedFrom.equals("RCS_CMD_MSG")
                        || var_class_type_info.DerivedFrom.equals("RCS_WM_MSG")
                        || var_class_type_info.DerivedFrom.equals("NMLmsg")
                        || var_class_type_info.DerivedFrom.equals("NML_QUERY_MSG")) {
                    return false;
                }
                if (var_class_type_info.DerivedFrom.equals("RCS_STAT_MSG")) {
                    return false;
                }
                if (var_class_type_info.DerivedFrom.equals("RCS_STAT_MSG_V2")) {
                    return true;
                }
                //last_var_class_type_info = var_class_type_info;
                var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(var_class_type_info.DerivedFrom);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean IsNMLMsg(String classname) {
        boolean is_nml_msg = IsNMLMsgHidden(classname);
        if (debug_on) {
            DebugPrint("IsNmlMsg(" + classname + ") returning " + is_nml_msg);
        }
        return is_nml_msg;
    }

    private boolean IsNMLMsgHidden(String classname) {
        if (classname.startsWith("struct ")) {
            classname = classname.substring(7);
        } else if (classname.startsWith("class ")) {
            classname = classname.substring(6);
        }
        classname = classname.trim();

        try {
            if (debug_on) {
                DebugPrint(" IsNMLMsg(" + classname + ") called.");
            }
            if (classname.equals("RCS_CMD_MSG")
                    || classname.equals("RCS_STAT_MSG")
                    || classname.equals("RCS_WM_MSG")
                    || classname.equals("NMLmsg")
                    || classname.equals("NML_QUERY_MSG")) {
                return true;
            }
            int doublecolonindex = classname.lastIndexOf("::");
            if (doublecolonindex > 0) {
                String unqualified_classname = classname.substring(doublecolonindex + 2);
                if (IsNMLMsg(unqualified_classname)) {
                    return true;
                }
            }
            StructureTypeInfo var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(classname);
            if (classname.equals("RCS_STAT_MSG_V2")) {
                if (var_class_type_info != null) {
                    var_class_type_info.is_nml_msg = true;
                }
                return true;
            }
            StructureTypeInfo first_var_class_type_info = var_class_type_info;
            StructureTypeInfo last_var_class_type_info = var_class_type_info;
            if (debug_on) {
                DebugPrint("var_class_type_info =" + var_class_type_info);
            }
            while (var_class_type_info != null) {
                if (null == var_class_type_info.DerivedFrom) {
                    return var_class_type_info.is_nml_msg;
                }
                if (debug_on) {
                    DebugPrint("var_class_type_info =" + var_class_type_info);
                }
                if (var_class_type_info.Id < 1) {
                    try {
                        if (null != var_class_type_info.type_id_string) {
                            String idString = m_currentModule.ReplaceDefinedValues(var_class_type_info.type_id_string, 0, null);
                            if (Character.isDigit(idString.charAt(0)) || idString.charAt(0) == '+') {
                                Long idLong = Long.valueOf(rcs.utils.StrToLong.convert(idString));
                                long new_id = idLong.longValue();
                                if (new_id >= 1) {
                                    var_class_type_info.Id = new_id;
                                }
                            }
                        }
                    } catch (Exception e) {
                    }
                }
                if (var_class_type_info.Id > 0
                        && var_class_type_info.DerivedFrom != null) {
                    first_var_class_type_info.is_nml_msg = true;
                    last_var_class_type_info.is_nml_msg = true;
                    var_class_type_info.is_nml_msg = true;
                    return true;
                }
                if (var_class_type_info.is_nml_msg) {
                    first_var_class_type_info.is_nml_msg = true;
                    last_var_class_type_info.is_nml_msg = true;
                    return true;
                }
                if (null != var_class_type_info.DerivedFrom) {
                    if (var_class_type_info.DerivedFrom.equals("RCS_CMD_MSG")
                            || var_class_type_info.DerivedFrom.equals("RCS_STAT_MSG")
                            || var_class_type_info.DerivedFrom.equals("RCS_STAT_MSG_V2")
                            || var_class_type_info.DerivedFrom.equals("RCS_WM_MSG")
                            || var_class_type_info.DerivedFrom.equals("NMLmsg")
                            || var_class_type_info.DerivedFrom.equals("NML_QUERY_MSG")) {
                        first_var_class_type_info.is_nml_msg = true;
                        last_var_class_type_info.is_nml_msg = true;
                        var_class_type_info.is_nml_msg = true;
                        return true;
                    }
                }
                last_var_class_type_info = var_class_type_info;
                var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(var_class_type_info.DerivedFrom);
                if (null == var_class_type_info) {
                    doublecolonindex = last_var_class_type_info.DerivedFrom.lastIndexOf("::");
                    if (doublecolonindex > 0) {
                        //					String unqualified_classname=last_var_class_type_info.DerivedFrom.substring(doublecolonindex+2);
                        var_class_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(var_class_type_info.DerivedFrom);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }
    public String c_prototypes_header_string = null;

    public String GetFormatFunctionNameBase(String selected_classes[]) {
        try {
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            if (debug_on) {
                DebugPrint("GetFormatFunctionNameBase : selected_classes[0]=" + selected_classes[0]);
                DebugPrint("GetFormatFunctionNameBase : type_info=" + type_info);
            }
            if (null == type_info || null == type_info.first_module_used_in
                    || null == type_info.first_module_used_in.getFormatFunction()) {
                format_function_name_base = find_common_root(selected_classes);
                if (null != format_function_name_base) {
                    if (format_function_name_base.length() < 2) {
                        format_function_name_base = null;
                    } else {
                        WriteOutput("\n // Format function name set from common root of selected classes. (");
                        for (int i = 0; i < 4 && i < selected_classes.length; i++) {
                            WriteOutput(selected_classes[i]);
                            if (i > 0) {
                                WriteOutput(",");
                            }
                            if (i >= 2) {
                                WriteOutput("...");
                            }
                        }
                        WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                        WriteOutput("\n");
                    }
                }
                if (null == format_function_name_base && null != type_info.first_module_used_in) {
                    format_function_name_base = type_info.first_module_used_in.getName();
                    WriteOutput("\n // Format function name set based on module first selected class was first used in. class=" + selected_classes[0] + ", module=" + type_info.first_module_used_in.getName() + "\n");
                    WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                    WriteOutput("\n");
                }
            } else {
                format_function_name_base = type_info.first_module_used_in.getFormatFunction();
                if (format_function_name_base.endsWith("_")) {
                    format_function_name_base = format_function_name_base.substring(0, format_function_name_base.length() - 1);
                }
                if (format_function_name_base.toUpperCase().endsWith("FORMAT")) {
                    format_function_name_base = format_function_name_base.substring(0, format_function_name_base.length() - 6);
                }
                if (format_function_name_base.endsWith("_")) {
                    format_function_name_base = format_function_name_base.substring(0, format_function_name_base.length() - 1);
                }
            }
            if (null == format_function_name_base) {
                return null;
            }
            int fslash_index = format_function_name_base.lastIndexOf('/');
            if (fslash_index >= 0) {
                format_function_name_base = format_function_name_base.substring(fslash_index + 1);
            }
            int bslash_index = format_function_name_base.lastIndexOf('\\');
            if (bslash_index >= 0) {
                format_function_name_base = format_function_name_base.substring(bslash_index + 1);
            }
            int colon_index = format_function_name_base.lastIndexOf(':');
            if (colon_index >= 0) {
                format_function_name_base = format_function_name_base.substring(colon_index + 1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return format_function_name_base;
    }

    void Get_Ada_FormatFunction(String selected_classes[]) {
        try {
            if (null == ada_format_function_name) {
                if (null == format_function_name_base) {
                    GetFormatFunctionNameBase(selected_classes);
                }
                if (null == format_function_name_base) {
                    return;
                }
                ada_format_function_name = format_function_name_base + "_Format";
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateAdaSpec() {
        try {
            String selected_classes[] = StringFuncs.SortClassList(ClassList.getSelectedItems(), this, ModuleInfo.m_structInfoByNameHashTable);
            Ada_Generator.GenerateAdaSpec(selected_classes,
                    this, currentOutputFileName,
                    ModuleInfo.m_enumInfoHashTable);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateAdaBody() {
        try {
            String selected_classes[] = StringFuncs.SortClassList(ClassList.getSelectedItems(), this, ModuleInfo.m_structInfoByNameHashTable);
            Ada_Generator.GenerateAdaBody(selected_classes,
                    this, currentOutputFileName,
                    ModuleInfo.m_enumInfoHashTable);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateC_PrototypesHeader() {
        try {
            String selected_classes[] = StringFuncs.SortClassList(ClassList.getSelectedItems(), this, ModuleInfo.m_structInfoByNameHashTable);
            C_Generator.GenerateC_PrototypesHeader(selected_classes, this, currentOutputFileName, ModuleInfo.m_enumInfoHashTable);
            c_prototypes_header_string = currentOutputFileName;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateC_StartOfFile() {
        try {
            String selected_classes[] = ClassList.getSelectedItems();
            C_Generator.GenerateC_StartOfFile(selected_classes,
                    this,
                    currentOutputFileName,
                    ModuleInfo.m_enumInfoHashTable,
                    c_prototypes_header_string);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public void GenerateCppStartOfFile() {
        try {
            boolean is_posemath_class = false;
            boolean var_class_is_nml_msg = false;
            String info_token = "";
            String orig_info_token = info_token;
            //String trimmed_orig_info_token=info_token;
            String cpp_type = "";
            int lastSpaceIndex = -1;
            boolean is_class = false;
            Vector class_prototypes = new Vector();
            Vector class_init_prototypes = new Vector();
            boolean already_prototyped = false;

            if (debug_on) {
                // (new Exception()).printStackTrace();
                DebugPrint("first_cpp_function=" + first_cpp_function);
            }
            if (!first_java_class) {
                WriteOutput("\n/**************************************************************");
                WriteOutput("\n*  ERROR !!! --- Clear Java code before adding C++ functions! *");
                WriteOutput("\n**************************************************************/\n");
                return;
            }
            if (!first_cpp_function) {
                return;
            }
            String selected_classes[] = ClassList.getSelectedItems();
            if (selected_classes.length < 1) {
                return;
            }
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            if (debug_on) {
                DebugPrint("type_info=" + type_info);
                DebugPrint("type_info.first_module_used_in=" + type_info.first_module_used_in);
            }

            if (null != type_info.first_module_used_in) {
                if (type_info.first_module_used_in.get_created_from_header()) {
                    cppFileName = type_info.first_module_used_in.getName() + ".cc";
                } else {
                    cppFileName = type_info.first_module_used_in.getName() + "n.cc";
                }
                WriteOutput("/*\n*\tNew C++ File starts here.\n*\tThis file should be named " + cppFileName + "\n");
                WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
                WriteOutput("*\twith command line arguments : " + orig_args_one_string + "\n");
//                WriteOutput("*\tRCS_VERSION=" + rcs.RCS_VERSION.version_string);
                if (null != prev_lines_of_script) {
                    WriteOutput("*\n");
                    WriteOutput("*\t.gen script :\n");
                    if (null != lines_of_script) {
                        for (int i = 0; i < lines_of_script.length; i++) {
                            String s = (String) lines_of_script[i];
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    } else {
                        for (int i = 0; i < prev_lines_of_script.size(); i++) {
                            String s = (String) prev_lines_of_script.get(i);
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    }
                    WriteOutput("*\n");
                }
                WriteOutput("*/\n\n");
            }
            WriteOutput("// Include all NML, CMS, and RCS classes and functions\n#include \"rcs.hh\"\n\n");
            WriteOutput("// Include command and status message definitions\n");
            for (int i = 0; i < ModuleInfo.headerFiles.size(); i++) {
                String header = (String) ModuleInfo.headerFiles.elementAt(i);
                WriteOutput("#include \"" + header + "\"\n");
            }
            if (null != ModuleInfo.extraHeaderFiles) {
                for (int i = 0; i < ModuleInfo.extraHeaderFiles.size(); i++) {
                    String header = (String) ModuleInfo.extraHeaderFiles.elementAt(i);
                    WriteOutput("#include \"" + header + "\"\n");
                }
            }
            WriteOutput("\n");
            if (have_prototypes_header && prototypes_header_string != null) {
                WriteOutput("// Include externally supplied prototypes\n");
                WriteOutput("#include \"" + prototypes_header_string + "\"\n");
                WriteOutput("\n");
            } else {
                WriteOutput("// Forward Function Prototypes\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (type_info.DerivedFrom != null) {
                        is_posemath_class = CheckForCppPosemathClass(type_info.DerivedFrom);
                        var_class_is_nml_msg = IsNMLMsg(type_info.DerivedFrom);
                        if (!is_posemath_class && !var_class_is_nml_msg) {
                            already_prototyped = false;
                            for (int j = 0; j < class_prototypes.size(); j++) {
                                String prototype_class = (String) class_prototypes.elementAt(j);
                                if (prototype_class.equals(type_info.DerivedFrom)) {
                                    already_prototyped = true;
                                    break;
                                }
                            }
                            if (!already_prototyped && !IsNonUpdatebleClass(type_info)) {
                                if (prefix_nonmsg_update) {
                                    WriteOutput("void " + StringFuncs.replace_white_space(type_info.UnqualifiedDerivedFrom) + "_update(CMS *cms, " + type_info.DerivedFrom + " *x);\n");
                                } else {
                                    WriteOutput("void nmlupdate(CMS *cms, " + type_info.DerivedFrom + " *x);\n");
                                }
                                class_prototypes.addElement(type_info.DerivedFrom);
                            }
                            if (!type_info.constructor_declared) {
                                already_prototyped = false;
                                for (int j = 0; j < class_init_prototypes.size(); j++) {
                                    String prototype_class = (String) class_init_prototypes.elementAt(j);
                                    if (prototype_class.equals(type_info.DerivedFrom)) {
                                        already_prototyped = true;
                                        break;
                                    }
                                }
                                if (!already_prototyped && !IsNonUpdatebleClass(type_info)) {
                                    WriteOutput("void initialize_" + StringFuncs.replace_white_space(type_info.UnqualifiedDerivedFrom) + "(" + type_info.DerivedFrom + " *x);\n");
                                    class_init_prototypes.addElement(type_info.DerivedFrom);
                                    type_info.have_initialize = true;
                                }
                            }
                        }
                    }
                    String class_name = type_info.getName();
                    var_class_is_nml_msg = IsNMLMsg(class_name);
                    if (!var_class_is_nml_msg) {
                        already_prototyped = false;
                        for (int j = 0; j < class_prototypes.size(); j++) {
                            String prototype_class = (String) class_prototypes.elementAt(j);
                            if (prototype_class.equals(class_name)) {
                                already_prototyped = true;
                                break;
                            }
                        }
                        if (!already_prototyped) {
                            if (!type_info.is_union) {
                                if (prefix_nonmsg_update) {
                                    WriteOutput("void " + StringFuncs.replace_white_space(class_name) + "_update(CMS *cms, " + class_name + " *x);\n");
                                } else {
                                    WriteOutput("void nmlupdate(CMS *cms, " + class_name + " *x);\n");
                                }
                            } else {
                                WriteOutput("void " + StringFuncs.replace_white_space(class_name) + "_update_union(int union_selector,CMS *cms, " + class_name + " *x);\n");
                            }
                            class_prototypes.addElement(class_name);
                        }

                        if (!type_info.constructor_declared) {
                            already_prototyped = false;
                            for (int j = 0; j < class_init_prototypes.size(); j++) {
                                String prototype_class = (String) class_init_prototypes.elementAt(j);
                                if (prototype_class.equals(class_name)) {
                                    already_prototyped = true;
                                    break;
                                }
                            }
                            if (!already_prototyped) {
                                WriteOutput("void initialize_" + StringFuncs.replace_white_space(class_name) + "(" + class_name + " *x);\n");
                                class_init_prototypes.addElement(class_name);
                                type_info.have_initialize = true;
                            }

                        }
                    }
                    StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
                    while (info_tokenizer.hasMoreTokens()) {
                        info_token = info_tokenizer.nextToken();
                        int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
                        if (nml_dynamic_length_array_index >= 0) {
                            info_token = info_token.substring(nml_dynamic_length_array_index + ndla_string.length());
                        }
                        orig_info_token = info_token;
                        info_token = info_token.trim();
                        //trimmed_orig_info_token=info_token;
                        //DebugPrint("info_token = "+info_token);
                        cpp_type = null;
                        while (true) {
                            if (info_token.charAt(0) != ' '
                                    && info_token.charAt(0) != '\t'
                                    && info_token.charAt(0) != '\r'
                                    && info_token.charAt(0) != '\n') {
                                break;
                            }
                            if (info_token.length() < 2) {
                                break;
                            }
                            info_token = info_token.substring(1);
                        }
                        if (info_token.length() < 2) {
                            continue;
                        }
                        int l_squareParamIndex = info_token.indexOf('[');
                        if (l_squareParamIndex > 0) {
                            info_token = info_token.substring(0, l_squareParamIndex);
                        }
                        while (true) {
                            char last_char = info_token.charAt(info_token.length() - 1);
                            if (last_char != ' '
                                    && last_char != '\t'
                                    && last_char != '\r'
                                    && last_char != '\n') {
                                break;
                            }
                            if (info_token.length() < 2) {
                                break;
                            }
                            info_token = info_token.substring(0, info_token.length() - 1);
                        }
                        if (info_token.length() < 2) {
                            continue;
                        }
                        lastSpaceIndex = info_token.lastIndexOf(' ');
                        if (lastSpaceIndex < 0) {
                            ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                            ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *5");
                            continue;
                        }

                        cpp_type = info_token.substring(0, lastSpaceIndex);
                        while (true) {
                            if (cpp_type.charAt(0) != ' '
                                    && cpp_type.charAt(0) != '\t'
                                    && cpp_type.charAt(0) != '\r'
                                    && cpp_type.charAt(0) != '\n') {
                                break;
                            }
                            if (cpp_type.length() < 2) {
                                break;
                            }
                            cpp_type = cpp_type.substring(1);
                        }
                        int cpp_type_length = cpp_type.length();
                        while (true) {
                            if (cpp_type.charAt(cpp_type_length - 1) != ' '
                                    && cpp_type.charAt(cpp_type_length - 1) != '\t'
                                    && cpp_type.charAt(cpp_type_length - 1) != '\r'
                                    && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                                break;
                            }
                            if (cpp_type.length() < 2) {
                                break;
                            }
                            cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                            cpp_type_length = cpp_type.length();
                        }
                        if (cpp_type.indexOf('*') >= 0) {
                            type_info.contains_pointers = true;
                        }
                        if (cpp_type.indexOf('*') >= 0
                                || cpp_type.indexOf('&') >= 0) {
                            ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                            ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") appears to be a pointer or reference.");
                            type_info.CppUpdateFunction += "\n#error Can not create update for pointer or reference type (" + cpp_type + ")\n";
                            continue;
                        }
                        if (cpp_type.indexOf('*') >= 0
                                || cpp_type.indexOf('?') >= 0
                                || cpp_type.indexOf('-') >= 0
                                || cpp_type.indexOf('\\') >= 0
                                || cpp_type.indexOf('/') >= 0
                                || cpp_type.indexOf('+') >= 0
                                || cpp_type.indexOf('=') >= 0
                                || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                                || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                                || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                                || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                                || cpp_type.indexOf(',') >= 0
                                || cpp_type.indexOf('&') >= 0) {
                            ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                            ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains illegal character.");
                            continue;
                        }
                        if (cpp_type.length() < 1
                                || cpp_type.equals("class")
                                || cpp_type.equals("struct")) {
                            ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                            ErrorPrint("Bad cpp_type=" + cpp_type);
                            continue;
                        }
                        is_class = CheckForCppClass(cpp_type);
                        is_posemath_class = false;
                        if (is_class) {
                            is_posemath_class = CheckForCppPosemathClass(cpp_type);
                        }
                        if (is_class && !is_posemath_class) {
                            var_class_is_nml_msg = IsNMLMsg(cpp_type);
                            if (!var_class_is_nml_msg) {
                                already_prototyped = false;
                                for (int j = 0; j < class_prototypes.size(); j++) {
                                    String prototype_class = (String) class_prototypes.elementAt(j);
                                    if (prototype_class.equals(cpp_type)) {
                                        already_prototyped = true;
                                        break;
                                    }
                                }
                                if (!already_prototyped) {
                                    if (!type_info.is_union) {
                                        if (prefix_nonmsg_update) {
                                            WriteOutput("void " + StringFuncs.replace_white_space(cpp_type) + "_update(CMS *cms, " + cpp_type + " *x);\n");
                                        } else {
                                            WriteOutput("void nmlupdate(CMS *cms, " + cpp_type + " *x);\n");
                                        }
                                    } else {
                                        String uniontypename = cpp_type.substring(cpp_type.lastIndexOf(' ') + 1);
                                        WriteOutput("void " + uniontypename + "_update_union(int union_selector, CMS *cms, " + cpp_type + " *x);\n");
                                    }
                                    class_prototypes.addElement(cpp_type);
                                }
                                boolean already_init_prototyped = false;
                                for (int j = 0; j < class_init_prototypes.size(); j++) {
                                    String prototype_class = (String) class_init_prototypes.elementAt(j);
                                    if (prototype_class.equals(cpp_type)) {
                                        already_init_prototyped = true;
                                        break;
                                    }
                                }
                                if (!already_init_prototyped) {
                                    WriteOutput("void initialize_" + StringFuncs.replace_white_space(cpp_type) + "(" + cpp_type + " *x);\n");
                                    class_init_prototypes.addElement(cpp_type);
                                    type_info.have_initialize = true;
                                }

                            }
                        }
                    }

                    if (null != m_loadingPanel && display_on) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                        m_loadingPanel.force_repaint(500);
                    }
                }
            }
            WriteOutput("\n");
            first_cpp_function = false;

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String[] RemoveDuplicates(String list[]) {
        try {
            String temp;
            int list_length = list.length;
            for (int i = 0; i < list_length; i++) {
                while (null == list[i]) {
                    list[i] = list[list_length - 1];
                    list_length--;
                    if (debug_on) {
                        DebugPrint("RemoveDuplicates found null in list at i =" + i);
                        DebugPrint("list_length=" + list_length);
                    }

                }
                for (int j = i + 1; j < list_length; j++) {
                    int compare = list[i].compareTo(list[j]);
                    if (compare > 0) {
                        temp = list[i];
                        list[i] = list[j];
                        list[j] = temp;
                    }
                    if (compare == 0) {
                        list[j] = list[list_length - 1];
                        list_length--;
                        if (debug_on) {
                            DebugPrint("RemoveDuplicates found duplicate " + list[i] + " in list at i =" + i + " and j = " + j);
                        }
                        compare = list[i].compareTo(list[j]);
                        if (compare > 0) {
                            temp = list[i];
                            list[i] = list[j];
                            list[j] = temp;
                        }
                    }
                }
            }
            if (list_length < list.length) {
                String new_list[] = new String[list_length];
                for (int i = 0; i < list_length; i++) {
                    new_list[i] = list[i];
                }
                return new_list;
            }
            return list;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return list;
    }
    public boolean have_prototypes_header = false;
    public String prototypes_header_string = null;
    // @SuppressWarnings("unchecked")

    public void GenerateCppPrototypesHeader() {
        try {
            boolean is_posemath_class = false;
            boolean var_class_is_nml_msg = false;
            String info_token = "";
            String orig_info_token = info_token;
            //String trimmed_orig_info_token = info_token;
            String cpp_type = "";
            int lastSpaceIndex = -1;
            boolean is_class = false;
            Vector class_prototypes = new Vector();
            Vector class_init_prototypes = new Vector();
            boolean already_prototyped = false;
            String symbol_lookup_function_name = null;

            if (debug_on) {

                DebugPrint("CodeGenCommon.GenerateCppPrototypesHeader() called.");
            }
            String selected_classes[] = ClassList.getSelectedItems();
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateCppPrototypesHeader() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateCppPrototypesHeader() : selected_classes.length = " + selected_classes.length);
                }
            }

            if (selected_classes.length < 1) {
                return;
            }
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            if (debug_on) {
                DebugPrint("type_info=" + type_info);
                DebugPrint("type_info.first_module_used_in=" + type_info.first_module_used_in);
            }
            prototypes_header_string = currentOutputFileName;
            have_prototypes_header = true;
            WriteOutput("/*\n*\tNew C++ Header  File starts here.\n*\tThis file should be named " + currentOutputFileName + "\n");
            WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
            WriteOutput("*\twith command line arguments : " + orig_args_one_string + "\n");
//            WriteOutput("*\tRCS_VERSION=" + rcs.RCS_VERSION.version_string);
            if (null != prev_lines_of_script) {
                WriteOutput("*\n");
                WriteOutput("*\t.gen script :\n");
                if (null != lines_of_script) {
                    for (int i = 0; i < lines_of_script.length; i++) {
                        String s = (String) lines_of_script[i];
                        WriteOutput("*\t\t" + i + ":" + s + "\n");
                    }
                } else {
                    for (int i = 0; i < prev_lines_of_script.size(); i++) {
                        String s = (String) prev_lines_of_script.get(i);
                        WriteOutput("*\t\t" + i + ":" + s + "\n");
                    }
                }
                WriteOutput("*\n");
            }
            WriteOutput("*/\n\n");

            WriteOutput("#ifndef " + currentOutputFileName.replace('.', '_').replace('/', '_').replace('\\', '_') + "_included\n");
            WriteOutput("#define " + currentOutputFileName.replace('.', '_').replace('/', '_').replace('\\', '_') + "_included\n\n");
            WriteOutput("// Include all NML, CMS, and RCS classes and functions\n#include \"rcs.hh\"\n\n");
            WriteOutput("// Include command and status message definitions\n");
            for (int i = 0; i < ModuleInfo.headerFiles.size(); i++) {
                String header = (String) ModuleInfo.headerFiles.elementAt(i);
                WriteOutput("#include \"" + header + "\"\n");
            }
            WriteOutput("\n");
            WriteOutput("// Forward Function Prototypes\n");
            for (int i = 0; i < selected_classes.length; i++) {
                type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (type_info.DerivedFrom != null) {
                    is_posemath_class = CheckForCppPosemathClass(type_info.DerivedFrom);
                    var_class_is_nml_msg = IsNMLMsg(type_info.DerivedFrom);
                    if (!is_posemath_class && !var_class_is_nml_msg && !IsNonUpdatebleClass(type_info)) {
                        already_prototyped = false;
                        for (int j = 0; j < class_prototypes.size(); j++) {
                            String prototype_class = (String) class_prototypes.elementAt(j);
                            if (prototype_class.equals(type_info.DerivedFrom)) {
                                already_prototyped = true;
                                break;
                            }
                        }
                        if (!already_prototyped) {
                            if (prefix_nonmsg_update) {
                                WriteOutput("extern void " + StringFuncs.replace_white_space(type_info.UnqualifiedDerivedFrom) + "_update(CMS *cms, " + type_info.DerivedFrom + " *x);\n");
                            } else {
                                WriteOutput("extern void nmlupdate(CMS *cms, " + type_info.DerivedFrom + " *x);\n");
                            }
                            class_prototypes.addElement(type_info.DerivedFrom);
                        }
                        if (!type_info.constructor_declared) {
                            already_prototyped = false;
                            for (int j = 0; j < class_init_prototypes.size(); j++) {
                                String prototype_class = (String) class_init_prototypes.elementAt(j);
                                if (prototype_class.equals(type_info.DerivedFrom)) {
                                    already_prototyped = true;
                                    break;
                                }
                            }
                            if (!already_prototyped) {
                                WriteOutput("extern void initialize_" + StringFuncs.replace_white_space(type_info.UnqualifiedDerivedFrom) + "(" + type_info.DerivedFrom + " *x);\n");
                                class_init_prototypes.addElement(type_info.DerivedFrom);
                                type_info.have_initialize = true;
                            }
                        }
                    }
                }
                String class_name = type_info.getName();
                if (class_name == null
                        || class_name.length() < 1
                        || class_name.equals("class")
                        || class_name.equals("struct")) {
                    ErrorPrint("Bad class_name=" + class_name);
                    continue;
                }
                is_posemath_class = CheckForCppPosemathClass(class_name);
                var_class_is_nml_msg = IsNMLMsg(class_name);
                if (!var_class_is_nml_msg && !is_posemath_class && !IsNonUpdatebleClass(type_info)) {
                    already_prototyped = false;
                    for (int j = 0; j < class_prototypes.size(); j++) {
                        String prototype_class = (String) class_prototypes.elementAt(j);
                        if (prototype_class.equals(class_name)) {
                            already_prototyped = true;
                            break;
                        }
                    }
                    if (!already_prototyped) {
                        if (!type_info.is_union) {
                            if (prefix_nonmsg_update) {
                                WriteOutput("extern void " + StringFuncs.replace_white_space(class_name) + "_update(CMS *cms, " + class_name + " *x);\n");
                            } else {
                                WriteOutput("extern void nmlupdate(CMS *cms, " + class_name + " *x);\n");
                            }
                        } else {
                            String uniontypename = class_name.substring(class_name.lastIndexOf(' ') + 1);
                            WriteOutput("extern void " + uniontypename + "_update_union(int union_selector, CMS *cms, " + class_name + " *x);\n");
                        }
                        class_prototypes.addElement(class_name);
                    }

                    if (!type_info.constructor_declared) {
                        already_prototyped = false;
                        for (int j = 0; j < class_init_prototypes.size(); j++) {
                            String prototype_class = (String) class_init_prototypes.elementAt(j);
                            if (prototype_class.equals(class_name)) {
                                already_prototyped = true;
                                break;
                            }
                        }
                        if (!already_prototyped) {
                            WriteOutput("extern void initialize_" + StringFuncs.replace_white_space(class_name) + "(" + class_name + " *x);\n");
                            class_init_prototypes.addElement(class_name);
                            type_info.have_initialize = true;
                        }

                    }
                }
                StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
                while (info_tokenizer.hasMoreTokens()) {
                    info_token = info_tokenizer.nextToken();
                    int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
                    if (nml_dynamic_length_array_index >= 0) {
                        info_token = info_token.substring(nml_dynamic_length_array_index + ndla_string.length());
                    }
                    orig_info_token = info_token;
                    info_token = info_token.trim();
                    //trimmed_orig_info_token=info_token;
                    //DebugPrint("info_token = "+info_token);
                    cpp_type = null;
                    while (true) {
                        if (info_token.charAt(0) != ' '
                                && info_token.charAt(0) != '\t'
                                && info_token.charAt(0) != '\r'
                                && info_token.charAt(0) != '\n') {
                            break;
                        }
                        if (info_token.length() < 2) {
                            break;
                        }
                        info_token = info_token.substring(1);
                    }
                    if (info_token.length() < 2) {
                        continue;
                    }
                    int l_squareParamIndex = info_token.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        info_token = info_token.substring(0, l_squareParamIndex);
                    }
                    while (true) {
                        char last_char = info_token.charAt(info_token.length() - 1);
                        if (last_char != ' '
                                && last_char != '\t'
                                && last_char != '\r'
                                && last_char != '\n') {
                            break;
                        }
                        if (info_token.length() < 2) {
                            break;
                        }
                        info_token = info_token.substring(0, info_token.length() - 1);
                    }
                    if (info_token.length() < 2) {
                        continue;
                    }
                    lastSpaceIndex = info_token.lastIndexOf(' ');
                    if (lastSpaceIndex < 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- info_token (" + info_token + ") needs a space. *6");
                        continue;
                    }

                    cpp_type = info_token.substring(0, lastSpaceIndex);
                    while (true) {
                        if (cpp_type.charAt(0) != ' '
                                && cpp_type.charAt(0) != '\t'
                                && cpp_type.charAt(0) != '\r'
                                && cpp_type.charAt(0) != '\n') {
                            break;
                        }
                        if (cpp_type.length() < 2) {
                            break;
                        }
                        cpp_type = cpp_type.substring(1);
                    }
                    int cpp_type_length = cpp_type.length();
                    while (true) {
                        if (cpp_type.charAt(cpp_type_length - 1) != ' '
                                && cpp_type.charAt(cpp_type_length - 1) != '\t'
                                && cpp_type.charAt(cpp_type_length - 1) != '\r'
                                && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                            break;
                        }
                        if (cpp_type.length() < 2) {
                            break;
                        }
                        cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                        cpp_type_length = cpp_type.length();
                    }
                    if (cpp_type.indexOf('*') >= 0) {
                        type_info.contains_pointers = true;
                    }
                    if (cpp_type.indexOf('*') >= 0
                            || cpp_type.indexOf('&') >= 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") appears to be a pointer or reference.");

                        type_info.CppUpdateFunction += "\n#error Can not create update for pointer or reference type (" + cpp_type + ")\n";
                        continue;
                    }
                    if (cpp_type.indexOf('*') >= 0
                            || cpp_type.indexOf('?') >= 0
                            || cpp_type.indexOf('-') >= 0
                            || cpp_type.indexOf('\\') >= 0
                            || cpp_type.indexOf('/') >= 0
                            || cpp_type.indexOf('+') >= 0
                            || cpp_type.indexOf('=') >= 0
                            || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                            || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                            || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                            || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                            || cpp_type.indexOf(',') >= 0
                            || cpp_type.indexOf('&') >= 0) {
                        ErrorPrint("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        ErrorPrint("\t\t-- cpp_type (" + cpp_type + ") contains illegal character.");

                        continue;
                    }
                    is_class = CheckForCppClass(cpp_type);
                    is_posemath_class = false;
                    if (is_class) {
                        is_posemath_class = CheckForCppPosemathClass(cpp_type);
                    }
                    if (is_class && !is_posemath_class) {
                        var_class_is_nml_msg = IsNMLMsg(cpp_type);
                        if (!var_class_is_nml_msg) {
                            already_prototyped = false;
                            for (int j = 0; j < class_prototypes.size(); j++) {
                                String prototype_class = (String) class_prototypes.elementAt(j);
                                if (prototype_class.equals(cpp_type)) {
                                    already_prototyped = true;
                                    break;
                                }
                            }
                            if (!already_prototyped) {
                                StructureTypeInfo cpp_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(cpp_type);

                                if (null == cpp_type_info || !cpp_type_info.is_union) {
                                    if (prefix_nonmsg_update) {
                                        WriteOutput("extern void " + StringFuncs.replace_white_space(cpp_type) + "_update(CMS *cms, " + cpp_type + " *x);\n");
                                    } else {
                                        WriteOutput("extern void nmlupdate(CMS *cms, " + cpp_type + " *x);\n");
                                    }
                                } else {
                                    String uniontypename = cpp_type.substring(cpp_type.lastIndexOf(' ') + 1);
                                    WriteOutput("extern void " + uniontypename + "_update_union(int union_selector, CMS *cms, " + cpp_type + " *x);\n");
                                }
                                class_prototypes.addElement(cpp_type);
                            }
                        }
                    }
                }

                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.force_repaint(500);
                }
            }
            WriteOutput("\n");
            type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            if (null == format_function_name) {
                if (null == type_info
                        || null == type_info.first_module_used_in
                        || null == type_info.first_module_used_in.getFormatFunction()) {
                    format_function_name = find_common_root(selected_classes);
                    if (null != format_function_name) {
                        if (format_function_name.length() < 2) {
                            format_function_name = null;
                        } else {
                            WriteOutput("\n // Format function name set from common root of selected classes. (");
                            for (int i = 0; i < 4 && i < selected_classes.length; i++) {
                                WriteOutput(selected_classes[i]);
                                if (i > 0) {
                                    WriteOutput(",");
                                }
                                if (i >= 2) {
                                    WriteOutput("...");
                                }
                            }
                            WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                            WriteOutput("\n");
                        }
                    }
                    if (null == format_function_name && null != type_info.first_module_used_in) {
                        format_function_name = type_info.first_module_used_in.getName();
                        WriteOutput("\n // Format function name set based on module first selected class was first used in. class=" + selected_classes[0] + ", module=" + type_info.first_module_used_in.getName() + "\n");
                        WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                        WriteOutput("\n");
                    }
                    if (null == format_function_name) {
                        format_function_name = "_";
                    }
                    if (!format_function_name.toUpperCase().endsWith("FORMAT")) {
                        if (format_function_name.endsWith("_")) {
                            format_function_name += "format";
                        } else {
                            format_function_name += "_format";
                        }
                    }
                } else {
                    format_function_name = type_info.first_module_used_in.getFormatFunction();
                }
            }
            int fslash_index = format_function_name.lastIndexOf('/');
            if (fslash_index >= 0) {
                format_function_name = format_function_name.substring(fslash_index + 1);
            }
            int bslash_index = format_function_name.lastIndexOf('\\');
            if (bslash_index >= 0) {
                format_function_name = format_function_name.substring(bslash_index + 1);
            }
            int colon_index = format_function_name.lastIndexOf(':');
            if (colon_index >= 0) {
                format_function_name = format_function_name.substring(colon_index + 1);
            }

            if (null != type_info.first_module_used_in
                    && null != type_info.first_module_used_in.getSymbolLookup()) {
                symbol_lookup_function_name = type_info.first_module_used_in.getSymbolLookup();
            } else {
                if (!format_function_name.toUpperCase().endsWith("FORMAT")) {
                    symbol_lookup_function_name = format_function_name;
                } else {
                    symbol_lookup_function_name = format_function_name.substring(0, format_function_name.length() - 6);
                }
                if (symbol_lookup_function_name.endsWith("_")) {
                    symbol_lookup_function_name += "symbol_lookup";
                } else {
                    symbol_lookup_function_name += "_symbol_lookup";
                }
            }

            String idlist_name = format_function_name.substring(0, format_function_name.length() - 6);
            idlist_name += "id_list";
            String sizelist_name = format_function_name.substring(0, format_function_name.length() - 6);
            sizelist_name += "size_list";
            String namelist_name = format_function_name.substring(0, format_function_name.length() - 6);
            namelist_name += "name_list";

            if (update_with_name) {
                WriteOutput("\n");
                int longest_name_length = 0;
                int number_of_names = 0;
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                        continue;
                    }
                    number_of_names++;
                    if (selected_classes[i].length() > longest_name_length) {
                        longest_name_length = selected_classes[i].length();
                    }
                }

                WriteOutput("#ifndef MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH\n");
                WriteOutput("#define MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH " + (longest_name_length + 1) + "\n");
                WriteOutput("#endif\n");
                WriteOutput("#ifndef " + namelist_name.toUpperCase() + "_LENGTH\n");
                WriteOutput("#define " + namelist_name.toUpperCase() + "_LENGTH " + (number_of_names + 1) + "\n");
                WriteOutput("#endif\n");
                WriteOutput("\n");
                WriteOutput("\n/* This list must be in alphabetical order and the three lists must correspond. */\n");
                WriteOutput("extern const NMLTYPE " + idlist_name + "[" + namelist_name.toUpperCase() + "_LENGTH];\n");
                WriteOutput("extern const size_t " + sizelist_name + "[" + namelist_name.toUpperCase() + "_LENGTH];\n");

                if (generate_symbol_lookups) {
                    WriteOutput("extern const char " + namelist_name + "[" + namelist_name.toUpperCase() + "_LENGTH][MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH];\n");
                    WriteOutput("extern const char *" + symbol_lookup_function_name + "(long type);\n");
                    WriteOutput("\n");
                }

                try {
                    if (null != ModuleInfo.m_enumInfoHashTable) {
                        WriteOutput("\n// Enumerated Type Constants\n");
                        Enumeration enum_info_types = ModuleInfo.m_enumInfoHashTable.elements();
                        while (enum_info_types.hasMoreElements()) {
                            EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
                            if (null == enum_info) {
                                continue;
                            }
                            if (null == enum_info.reverse_hashtable) {
                                continue;
                            }
                            if (!generate_all_enum_symbol_lookups) {
                                if (enum_info.reverse_hashtable.size() < 1) {
                                    continue;
                                }
                                if (!enum_info.generate_symbol_lookup && !generate_symbol_lookups) {
                                    continue;
                                }
                            }
                            WriteOutput("\n// " + enum_info.Name + "\n");
                            int maxenumvalstringlength = 0;
                            int numenumvals = 0;

                            Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                            Vector sorted_enum_key_vector = new Vector();
                            while (enum_keys.hasMoreElements()) {
                                String key = (String) enum_keys.nextElement();
                                numenumvals++;
                                if (maxenumvalstringlength < key.length()) {
                                    maxenumvalstringlength = key.length();
                                }
                                boolean keyadded = false;
                                for (int ki = 0; ki < sorted_enum_key_vector.size(); ki++) {
                                    String compareKey = (String) sorted_enum_key_vector.elementAt(ki);
                                    if (key.compareTo(compareKey) < 0) {
                                        sorted_enum_key_vector.insertElementAt(key, ki);

                                        keyadded = true;
                                        break;
                                    }
                                }
                                if (!keyadded) {
                                    sorted_enum_key_vector.addElement(key);
                                }
                            }
                            String enumlistname
                                    = /// format_function_name.substring(0,format_function_name.length()-6)+
                                    "enum_" + enum_info.Name;
                            WriteOutput("#ifndef MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH\n");
                            WriteOutput("#define MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH " + (maxenumvalstringlength + 1) + "\n");
                            WriteOutput("#endif\n\t/* MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH */\n");
                            WriteOutput("#ifndef " + enumlistname.toUpperCase() + "_LENGTH\n");
                            WriteOutput("#define " + enumlistname.toUpperCase() + "_LENGTH " + (numenumvals + 1) + "\n");
                            WriteOutput("#endif\n\t/* " + enumlistname.toUpperCase() + "_LENGTH */\n");
                            /* WriteOutput("\nextern const char "+enumlistname+"_string_list["+enumlistname.toUpperCase()+
                             "_LENGTH][MAX_"+enumlistname.toUpperCase()+"_STRING_LENGTH];\n"); *
                             WriteOutput("\nextern const int "+enumlistname+"_int_list["+enumlistname.toUpperCase()+ "_LENGTH];\n"); */
                            WriteOutput("\nextern const char *" + format_function_name.substring(0, format_function_name.length() - 6) + enumlistname + "_symbol_lookup(long v);\n");
                        }
                    }
                    WriteOutput("\n");
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            WriteOutput("extern int " + format_function_name + "(NMLTYPE type, void *buffer, CMS *cms);\n");
            WriteOutput("\n");
            WriteOutput("#endif\n\t/* # endif " + currentOutputFileName.replace('.', '_').replace('/', '_').replace('\\', '_') + "_included */ \n");
            WriteOutput("\n");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    public boolean print_est_size_info = false;

    public void EstimateSize(StructureTypeInfo type_info) {
        try {
            if (null == type_info
                    || null == type_info.PreFinalPassInfo) {
                return;
            }
            if (debug_estimated_size) {
                DebugPrint2("EstimateSize(" + type_info + "called.\n\n");
            }

            StringTokenizer info_tokenizer = new StringTokenizer(type_info.PreFinalPassInfo, ";");
            //DebugPrint("Creating Update Function for "+type_info.Name);

            // If this class was derived from another just eat the members of the base class.
            //		EnumTypeInfo union_enum_info=null;
            if (type_info.is_union) {
                type_info.estimated_size = -2;
                return;
            }
            if (type_info.estimated_size > 0
                    || type_info.estimated_size < -1) {
                return;
            }
            if (type_info.is_nml_msg) {
                type_info.estimated_size = 16;
            } else {
                type_info.estimated_size = 0;
            }
            if (null != type_info.DerivedFrom) {
                if (debug_on || debug_estimated_size) {
                    DebugPrint("EstimateSize: type_info.DerivedFrom = " + type_info.DerivedFrom);
                }
                if (!type_info.DerivedFrom.equals("RCS_CMD_MSG")
                        && !type_info.DerivedFrom.equals("RCS_STAT_MSG")
                        && !type_info.DerivedFrom.equals("NMLmsg")) {
                    boolean var_class_is_nml_msg = IsNMLMsg(type_info.DerivedFrom);
                    if (debug_on || debug_estimated_size) {
                        DebugPrint("EstimatedSize: var_class_is_nml_msg = " + var_class_is_nml_msg);
                    }
                    if (var_class_is_nml_msg != type_info.is_nml_msg) {
                        return;
                    }
                    StructureTypeInfo derived_from_type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(type_info.DerivedFrom);
                    if (derived_from_type_info != null) {
                        EstimateSize(derived_from_type_info);
                        if (derived_from_type_info.estimated_size > 16) {
                            type_info.estimated_size += (derived_from_type_info.estimated_size - 16);
                        }
                    }
                } else if (type_info.DerivedFrom.equals("RCS_CMD_MSG")) {
                    type_info.estimated_size += 8;
                } else if (type_info.DerivedFrom.equals("RCS_STAT_MSG")) {
                    type_info.estimated_size += 112;
                }
            }
            boolean dynamic_array = false;
            boolean unbounded_array = false;
            String ndla_string = CodeGenCommonInterface.ndla_string;
            String unbounded_string = CodeGenCommonInterface.unbounded_string;
            while (info_tokenizer.hasMoreTokens()) {
                String info_token = info_tokenizer.nextToken();
                //			System.out.println("EstimateSize "+type_info.Name+"\t"+type_info.estimated_size+"\t"+info_token);
                if (debug_on || debug_estimated_size) {
                    DebugPrint2("estimated_size=" + type_info.estimated_size + " info_token=" + info_token);
                }
                int nml_dynamic_length_array_index = info_token.indexOf(ndla_string);
                if (nml_dynamic_length_array_index >= 0) {
                    info_token = info_token.substring(nml_dynamic_length_array_index + ndla_string.length());
                    dynamic_array = true;
                } else {
                    dynamic_array = false;
                }
                int nml_unbounded_length_array_index = info_token.indexOf(unbounded_string);
                if (nml_unbounded_length_array_index >= 0) {
                    info_token = info_token.substring(nml_unbounded_length_array_index + unbounded_string.length());
                    unbounded_array = true;
                } else {
                    unbounded_array = false;
                }
                String orig_info_token = info_token;
                info_token = info_token.trim();
                //			String trimmed_orig_info_token=info_token;
                //DebugPrint("info_token = "+info_token);
                if (print_est_size_info) {
                    System.out.println("\t" + type_info.estimated_size + "\t" + info_token);
                }
                String array_length_string = null;
                String variable_name = null;
                String cpp_type = null;
                int num_dims = 0;
                while (true) {
                    if (info_token.charAt(0) != ' '
                            && info_token.charAt(0) != '\t'
                            && info_token.charAt(0) != '\r'
                            && info_token.charAt(0) != '\n') {
                        break;
                    }
                    if (info_token.length() < 2) {
                        break;
                    }
                    info_token = info_token.substring(1);
                }
                if (info_token.length() < 2) {
                    continue;
                }
                int l_squareParamIndex = info_token.indexOf('[');
                int r_squareParamIndex = -1;
                String pre_array_string = info_token;
                if (l_squareParamIndex > 0) {
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex);
                    pre_array_string = info_token.substring(0, l_squareParamIndex);
                }
                int array_length = 1;
                while (-1 != r_squareParamIndex && -1 != l_squareParamIndex) {
                    array_length_string = info_token.substring(l_squareParamIndex + 1, r_squareParamIndex);
                    array_length_string = m_currentModule.ReplaceDefinedValues(array_length_string, 0, null);
                    int this_dim_length = ModuleInfo.doArrayLengthMath(array_length_string);
                    array_length *= this_dim_length;
                    num_dims++;
                    if (debug_on || debug_estimated_size) {
                        DebugPrint2("this_dim_length = " + this_dim_length + ", array_length = " + array_length + ", info_token = " + info_token);
                    }
                    l_squareParamIndex = info_token.indexOf('[', l_squareParamIndex + 1);
                    r_squareParamIndex = info_token.indexOf(']', l_squareParamIndex + 1);
                }
                while (true) {
                    if (pre_array_string.length() < 2) {
                        break;
                    }
                    char last_char = pre_array_string.charAt(pre_array_string.length() - 1);
                    if (last_char != ' '
                            && last_char != '\t'
                            && last_char != '\r'
                            && last_char != '\n') {
                        break;
                    }
                    pre_array_string = pre_array_string.substring(pre_array_string.length() - 1);
                }
                int lastSpaceIndex = pre_array_string.lastIndexOf(' ');
                if (lastSpaceIndex < 0) {
                    diagapplet.utils.DiagError.println("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    diagapplet.utils.DiagError.println("\t\t-- pre_array_string (" + pre_array_string + ") needs a space. *2");
                    RingBell();
                    continue;
                }

                variable_name = pre_array_string.substring(lastSpaceIndex + 1);
                l_squareParamIndex = variable_name.indexOf('[');
                if (l_squareParamIndex > 0) {
                    variable_name = variable_name.substring(0, l_squareParamIndex);
                }
                if (variable_name.length() < 1) {
                    lastSpaceIndex = info_token.lastIndexOf(' ', lastSpaceIndex - 1);
                    if (lastSpaceIndex < 0) {
                        diagapplet.utils.DiagError.println("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                        diagapplet.utils.DiagError.println("\t\t-- info_token (" + info_token + ") needs another space. L2313--28%");
                        RingBell();
                        continue;
                    }
                    variable_name = info_token.substring(lastSpaceIndex + 1);
                    l_squareParamIndex = variable_name.indexOf('[');
                    if (l_squareParamIndex > 0) {
                        variable_name = variable_name.substring(0, l_squareParamIndex);
                    }
                }
                if (variable_name.length() < 1) {
                    diagapplet.utils.DiagError.println("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    diagapplet.utils.DiagError.println("\t\t-- no variable_name.");
                    RingBell();
                    continue;
                }
                if (variable_name.indexOf('*') >= 0 || variable_name.indexOf('&') >= 0) {
                    continue;
                }
                if (variable_name.indexOf('*') >= 0
                        || variable_name.indexOf(' ') >= 0
                        || variable_name.indexOf('?') >= 0
                        || variable_name.indexOf('-') >= 0
                        || variable_name.indexOf('\\') >= 0
                        || variable_name.indexOf('/') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('=') >= 0
                        || variable_name.indexOf('+') >= 0
                        || variable_name.indexOf('<') >= 0 || variable_name.indexOf('>') >= 0
                        || variable_name.indexOf('[') >= 0 || variable_name.indexOf(']') >= 0
                        || variable_name.indexOf('(') >= 0 || variable_name.indexOf(')') >= 0
                        || variable_name.indexOf('{') >= 0 || variable_name.indexOf('}') >= 0
                        || variable_name.indexOf(',') >= 0
                        || variable_name.indexOf('&') >= 0) {
                    diagapplet.utils.DiagError.println("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    diagapplet.utils.DiagError.println("\t\t-- variable_name (" + variable_name + ") contains an illegal character.");
                    RingBell();
                    continue;
                }
                //String ovn_variable_name=variable_name;
                String override_name = (String) type_info.VarnameOverridesHashTable.get(variable_name);
                if (override_name != null) {
                    //ovn_variable_name=override_name;
                    if (debug_on) {
                        DebugPrint("CreateC_Update : variable_name=" + variable_name + ", override_name=" + override_name);
                    }
                }
                //String att_add="";
                cpp_type = info_token.substring(0, lastSpaceIndex);
                // if( type_info.VarnameAttributeInfoHashTable.containsKey(variable_name))
// 			    {
// 				att_add="_attribute";
// 			    }
// 			else if ( variable_name.endsWith("_length") &&
// 				  variable_name.length() > 7 &&
// 				  (type_info.VarnameNDLAHashTable.containsKey(variable_name.substring(0,variable_name.length()-7))
// 				   || type_info.VarnameNDLAHashTable.containsKey(variable_name))
// 				   )
// 			    {
// 				att_add="_dla_length";
// 			    }
                while (true) {
                    if (cpp_type.charAt(0) != ' '
                            && cpp_type.charAt(0) != '\t'
                            && cpp_type.charAt(0) != '\r'
                            && cpp_type.charAt(0) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(1);
                }
                int cpp_type_length = cpp_type.length();
                while (true) {
                    if (cpp_type.charAt(cpp_type_length - 1) != ' '
                            && cpp_type.charAt(cpp_type_length - 1) != '\t'
                            && cpp_type.charAt(cpp_type_length - 1) != '\r'
                            && cpp_type.charAt(cpp_type_length - 1) != '\n') {
                        break;
                    }
                    if (cpp_type.length() < 2) {
                        break;
                    }
                    cpp_type = cpp_type.substring(0, cpp_type_length - 1);
                    cpp_type_length = cpp_type.length();
                }
                if (cpp_type.indexOf('*') >= 0) {
                    type_info.contains_pointers = true;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    continue;
                }
                if (cpp_type.indexOf('*') >= 0
                        || cpp_type.indexOf('?') >= 0
                        || cpp_type.indexOf('-') >= 0
                        || cpp_type.indexOf('\\') >= 0
                        || cpp_type.indexOf('/') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('=') >= 0
                        || cpp_type.indexOf('+') >= 0
                        || cpp_type.indexOf('<') >= 0 || cpp_type.indexOf('>') >= 0
                        || cpp_type.indexOf('[') >= 0 || cpp_type.indexOf(']') >= 0
                        || cpp_type.indexOf('(') >= 0 || cpp_type.indexOf(')') >= 0
                        || cpp_type.indexOf('{') >= 0 || cpp_type.indexOf('}') >= 0
                        || cpp_type.indexOf(',') >= 0
                        || cpp_type.indexOf('&') >= 0) {
                    diagapplet.utils.DiagError.println("Invalid variable definition (" + orig_info_token + ") in class " + type_info.getName());
                    diagapplet.utils.DiagError.println("\t\t-- cpp_type (" + cpp_type + ") contains an illegal character.");

                    continue;
                }
                boolean is_class = CheckForCppClass(cpp_type);
                //			boolean is_posemath_class = false;
                boolean is_enum = false;
                if (is_class) {
                    //				is_posemath_class  =
                    CheckForCppPosemathClass(cpp_type);
                    is_enum = false;
                } else {
                    is_enum = CheckForCppEnum(cpp_type);
                }
                if (debug_on) {
                    DebugPrint("info_token=" + info_token + ", unbouded_array=" + unbounded_array + ",dynamic_array=" + dynamic_array + ",is_class=" + is_class + ",is_enum=" + is_enum + ",array_length=" + array_length + ",variable_name=" + variable_name);
                }
                if (type_info.is_union) {
                    diagapplet.utils.DiagError.println("unions not supported for NML C interface");
                    type_info.C_UpdateFunction = "\n#error unions not supported for NML C interface\n";
                    return;
                    /*
                     if(!union_enum_info.reverse_hashtable.containsKey("UNION_"+StringFuncs.replaceAllInString(type_info.Name,"Union","")+"_SELECTED_"+variable_name))
                     {
                     continue;
                     }
                     type_info.C_UpdateFunction += "\tcase UNION_"+StringFuncs.replaceAllInString(type_info.Name,"Union","")+"_SELECTED_"+variable_name+":\n\t";
                     */
                }
                int type_size = 8;
                if (is_enum) {
                    type_size = 4;
                } else if (cpp_type.equals("char") || cpp_type.equals("unsigned char")) {
                    type_size = 1;
                } else if (cpp_type.equals("bool")) {
                    type_size = 1;
                } else if (cpp_type.equals("short") || cpp_type.equals("unsigned short")) {
                    type_size = 2;
                } else if (cpp_type.equals("int") || cpp_type.equals("unsigned int") || cpp_type.equals("unsigned")) {
                    type_size = 8;
                } else if (cpp_type.equals("long") || cpp_type.equals("unsigned long")) {
                    type_size = 8;
                } else if (cpp_type.equals("float")) {
                    type_size = 4;
                } else if (cpp_type.equals("double")) {
                    type_size = 8;
                }
                if (array_length > 1) {
                    array_length_string = String.valueOf(array_length);
                    //DebugPrint("currentModule = "+currentModule);
                    //DebugPrint("variable_name = "+variable_name);
                    //DebugPrint(variable_name+"_ARRAY_LENGTH_VARIABLE");
                    if (null != m_currentModule) {
                        //DebugPrint("m_currentModule.definedValues ="+m_currentModule.definedValues);
                        if (null != m_currentModule.definedValues) {
                            DefinedValue dv = (DefinedValue) m_currentModule.definedValues.get(variable_name + "_ARRAY_LENGTH_VARIABLE");
                            //DebugPrint("dv ="+dv);
                            if (null != dv) {
                                array_length_string = dv.value;
                            }
                        }
                    }
                    // String cast_string = "";
// 				if(num_dims > 1)
// 				    {
// 					if(is_class)
// 					    {
// 						cast_string = "( nml_"+cpp_type+"_c_t *) ";
// 					    }
// 					else
// 					    {
// 						cast_string = "("+cpp_type+" *) ";
// 					    }
// 				    }
                    if (dynamic_array) {
                        if (type_info.estimated_size % 8 != 0) {
                            type_info.estimated_size += (8 - type_info.estimated_size % 8);
                        }
                        type_info.estimated_size += 8;

                    }
                    if (is_class) {
                        if (cpp_type.startsWith("struct ")) {
                            cpp_type = cpp_type.substring(7).trim();
                        } else if (cpp_type.startsWith("class ")) {
                            cpp_type = cpp_type.substring(6).trim();
                        }
                        StructureTypeInfo class_sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(cpp_type);
                        if (class_sti != null) {
                            EstimateSize(class_sti);
                            if (type_info.estimated_size % 8 != 0) {
                                type_info.estimated_size += (8 - type_info.estimated_size % 8);
                            }
                            int class_estimated_size = class_sti.estimated_size;
                            if (class_estimated_size % 8 != 0) {
                                class_estimated_size += (8 - class_estimated_size % 8);
                            }
                            if (class_estimated_size < 8) {
                                class_estimated_size = 8;
                            }
                            type_info.estimated_size += class_estimated_size * array_length;
                        }
                    } else {
                        if (type_size > 1 && type_info.estimated_size % type_size != 0) {
                            type_info.estimated_size += (type_size - type_info.estimated_size % type_size);
                        }
                        type_info.estimated_size += type_size * array_length;
                    }
                } else if (is_class) {
                    if (cpp_type.startsWith("struct ")) {
                        cpp_type = cpp_type.substring(7).trim();
                    } else if (cpp_type.startsWith("class ")) {
                        cpp_type = cpp_type.substring(6).trim();
                    }
                    StructureTypeInfo class_sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(cpp_type);
                    if (class_sti != null) {
                        EstimateSize(class_sti);
                        if (type_info.estimated_size % 8 != 0) {
                            type_info.estimated_size += (8 - type_info.estimated_size % 8);
                        }
                        type_info.estimated_size += class_sti.estimated_size;
                    }
                    if (unbounded_array) {
                        type_info.estimated_size += 16;
                    }
                } else if (unbounded_array) {
                    type_info.estimated_size += 16;
                } else {
                    if (type_size > 1 && type_info.estimated_size % type_size != 0) {
                        type_info.estimated_size += (type_size - type_info.estimated_size % type_size);
                    }
                    type_info.estimated_size += type_size;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void WriteToFos(FileOutputStream fos, String str) {
        try {
            byte b[] = str.getBytes();
            fos.write(b, 0, str.length());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    private boolean CheckUsedInSelectedClasses(String selected_classes[], String enumName) {
        for (int i = 0; i < selected_classes.length; i++) {
            String classname = selected_classes[i];
            StructureTypeInfo typeInfo
                    = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(classname);
            if (typeInfo != null) {
                if (typeInfo.PreFinalPassInfo.indexOf(enumName + " ") >= 0) {
                    return true;
                }
            }
        }
        return false;
    }

    public void GenerateCppEnumInfo(String selected_classes[]) {
        boolean orig_generating_code = generating_code;
        try {
            generating_code = true;
            GenerateCppStartOfFile();
            if (generate_all_enum_symbol_lookups) {
                ModuleInfo.AddDefaultTypes();
            }
            if (null == format_function_name) {
                if (null == selected_classes) {
                    selected_classes = RemoveDuplicates(ClassList.getSelectedItems());
                    if (selected_classes.length < 1 && !generate_all_enum_symbol_lookups) {
                        return;
                    }
                }
                SetFormatFunctionName(selected_classes);
            }

            if (null != ModuleInfo.m_enumInfoHashTable) {
                WriteOutput("\n// Enumerated Type Constants\n");
                Enumeration enum_info_types = ModuleInfo.m_enumInfoHashTable.elements();
                while (enum_info_types.hasMoreElements()) {
                    EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
                    if (null == enum_info) {
                        continue;
                    }
                    if (null == enum_info.reverse_hashtable) {
                        continue;
                    }
                    if (!generate_all_enum_symbol_lookups) {
                        if (!enum_info.generate_symbol_lookup && !generate_symbol_lookups) {
                            continue;
                        }
                        if (enum_info.reverse_hashtable.size() < 1) {
                            continue;
                        }
                        if (!CheckUsedInSelectedClasses(selected_classes, enum_info.Name)) {
                            continue;
                        }
                    }
                    WriteOutput("\n// " + enum_info.Name + "\n");
                    int maxenumvalstringlength = 0;
                    int numenumvals = 0;

                    Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                    Vector sorted_enum_key_vector = new Vector();
                    while (enum_keys.hasMoreElements()) {
                        String key = (String) enum_keys.nextElement();
                        numenumvals++;
                        if (maxenumvalstringlength < key.length()) {
                            maxenumvalstringlength = key.length();
                        }
                        boolean keyadded = false;
                        for (int ki = 0; ki < sorted_enum_key_vector.size(); ki++) {
                            String compareKey = (String) sorted_enum_key_vector.elementAt(ki);
                            if (key.compareTo(compareKey) < 0) {
                                sorted_enum_key_vector.insertElementAt(key, ki);

                                keyadded = true;
                                break;
                            }
                        }
                        if (!keyadded) {
                            sorted_enum_key_vector.addElement(key);
                        }
                    }
                    String enumlistname
                            = /// format_function_name.substring(0,format_function_name.length()-6)+
                            "enum_" + enum_info.Name;
                    WriteOutput("#ifndef MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH\n");
                    WriteOutput("#define MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH " + (maxenumvalstringlength + 1) + "\n");
                    WriteOutput("#endif\n");
                    WriteOutput("#ifndef " + enumlistname.toUpperCase() + "_LENGTH\n");
                    WriteOutput("#define " + enumlistname.toUpperCase() + "_LENGTH " + (numenumvals + 1) + "\n");
                    WriteOutput("#endif\n");

                    WriteOutput("\nstatic const char " + enumlistname + "_string_list[" + enumlistname.toUpperCase()
                            + "_LENGTH][MAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH]= {\n");
                    int ski = 0;
                    for (ski = 0; ski < sorted_enum_key_vector.size(); ski++) {
                        String key = (String) sorted_enum_key_vector.elementAt(ski);
                        String ovn_keyname = key;
                        if (null != enum_info.override_names_hashtable) {
                            ovn_keyname = (String) enum_info.override_names_hashtable.get(key);
                            if (null == ovn_keyname) {
                                ovn_keyname = key;
                            }
                        }
                        int val = -1;
                        try {
                            Integer Ival = (Integer) enum_info.reverse_hashtable.get(key);
                            val = Ival.intValue();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (!key.equals(ovn_keyname)) {
                            WriteOutput("\t\"" + ovn_keyname + "\", /* " + ski + "," + val + ",sym=" + key + " */\n");
                        } else {
                            WriteOutput("\t\"" + ovn_keyname + "\", /* " + ski + "," + val + " */\n");
                        }

                    }
                    WriteOutput("\t\"\"};\n");

                    WriteOutput("\nstatic const int " + enumlistname + "_int_list[" + enumlistname.toUpperCase()
                            + "_LENGTH]= {\n");
                    for (ski = 0; ski < sorted_enum_key_vector.size(); ski++) {
                        String key = (String) sorted_enum_key_vector.elementAt(ski);
                        String ovn_keyname = null;
                        if (null != enum_info.override_names_hashtable) {
                            ovn_keyname = (String) enum_info.override_names_hashtable.get(key);
                        }
                        int val = -1;
                        try {
                            Integer Ival = (Integer) enum_info.reverse_hashtable.get(key);
                            val = Ival.intValue();
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (ovn_keyname != null) {
                            WriteOutput("\t" + key + "," + " /* " + ski + "," + val + ",name=" + ovn_keyname + " */\n");
                        } else {
                            WriteOutput("\t" + key + "," + " /* " + ski + "," + val + " */\n");
                        }
                    }
                    WriteOutput("\t};\n");
                    WriteOutput("\nconst char *" + format_function_name.substring(0, format_function_name.length() - 6) + enumlistname + "_symbol_lookup(long v)\n{\n\tswitch(v)\n\t{\n");
                    Vector vv = new Vector();
                    for (ski = 0; ski < sorted_enum_key_vector.size(); ski++) {
                        String key = (String) sorted_enum_key_vector.elementAt(ski);
                        String ovn_keyname = key;
                        if (null != enum_info.override_names_hashtable) {
                            ovn_keyname = (String) enum_info.override_names_hashtable.get(key);
                            if (null == ovn_keyname) {
                                ovn_keyname = key;
                            }
                        }
                        Integer Ival = (Integer) enum_info.reverse_hashtable.get(key);
                        int ival = Ival.intValue();
                        boolean ival_found = false;
                        int vvi = 0;
                        for (vvi = 0; vvi < vv.size(); vvi++) {
                            Integer Ival2 = (Integer) vv.elementAt(vvi);
                            int ival2 = Ival2.intValue();
                            if (ival == ival2) {
                                ival_found = true;
                                break;
                            }
                        }
                        vv.addElement(Ival);
                        if (ival_found) {
                            String competingKey = (String) sorted_enum_key_vector.elementAt(vvi);
                            WriteOutput("\t\t/* " + key + " has the same value as " + competingKey + " of " + ival + " */\n");
                            continue;
                        }
                        WriteOutput("\t\tcase " + key + ':' + " return(\"" + ovn_keyname + "\"); /* " + ival + " */\n");
                    }
                    WriteOutput("\t\tdefault" + ':' + "break;\n\t}\n\treturn(\"!!UNDEFINED_SYMBOL!!\");\n}\n");
                    WriteOutput("\nstatic const struct cms_enum_info " + enumlistname + "_info_struct={\n");
                    WriteOutput("\t\"" + enum_info.Name + "\",\n\t(const char **)" + enumlistname + "_string_list,\n");
                    WriteOutput("\t" + enumlistname + "_int_list,\n\tMAX_" + enumlistname.toUpperCase() + "_STRING_LENGTH,\n");
                    WriteOutput("\t" + enumlistname.toUpperCase() + "_LENGTH,\n");
                    WriteOutput("\t(cms_symbol_lookup_function_t)" + format_function_name.substring(0, format_function_name.length() - 6) + enumlistname + "_symbol_lookup\n\t};\n");
                }
            }
            WriteOutput("\n");
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = orig_generating_code;
    }

    public void SetFormatFunctionName(String selected_classes[]) {
        try {
            if (null == format_function_name) {
                StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
                if (null == type_info.first_module_used_in || null == type_info.first_module_used_in.getFormatFunction()) {
                    format_function_name = find_common_root(selected_classes);
                    if (null != format_function_name) {
                        if (format_function_name.length() < 2) {
                            format_function_name = null;
                        } else {
                            WriteOutput("\n // Format function name set from common root of selected classes. (");
                            for (int i = 0; i < 4 && i < selected_classes.length; i++) {
                                WriteOutput(selected_classes[i]);
                                if (i > 0) {
                                    WriteOutput(",");
                                }
                                if (i >= 2) {
                                    WriteOutput("...");
                                }
                            }
                            WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                            WriteOutput("\n");
                        }
                    }
                    if (null == format_function_name && null != type_info.first_module_used_in) {
                        format_function_name = type_info.first_module_used_in.getName();
                        WriteOutput("\n // Format function name set based on module first selected class was first used in. class=" + selected_classes[0] + ", module=" + type_info.first_module_used_in.getName() + "\n");
                        WriteOutput("\n // You may want to add a function prototype to a header or an explicit set_format_function line to the CodeGen script to set this explicitly.");
                        WriteOutput("\n");
                    }
                    if (null == format_function_name) {
                        format_function_name = "_";
                    }
                    if (!format_function_name.toUpperCase().endsWith("FORMAT")) {
                        if (format_function_name.endsWith("_")) {
                            format_function_name += "format";
                        } else {
                            format_function_name += "_format";
                        }
                    }
                } else {
                    format_function_name = type_info.first_module_used_in.getFormatFunction();
                }
            }
            int fslash_index = format_function_name.lastIndexOf('/');
            if (fslash_index >= 0) {
                format_function_name = format_function_name.substring(fslash_index + 1);
            }
            int bslash_index = format_function_name.lastIndexOf('\\');
            if (bslash_index >= 0) {
                format_function_name = format_function_name.substring(bslash_index + 1);
            }
            int colon_index = format_function_name.lastIndexOf(':');
            if (colon_index >= 0) {
                format_function_name = format_function_name.substring(colon_index + 1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void GenerateCppFormatFunction() {
        String symbol_lookup_function_name = null;
        try {
            if (debug_on) {
                DebugPrint("GenerateCppFormatFunction");
            }
            generating_code = true;
            if (ModuleInfo.codegen_select_from_all_files) {
                //DebugPrint2("SelectFromAllFiles forced.");
                this.SelectFromAllFiles();
            }
            GenerateCppStartOfFile();

            if (!first_java_class) {
                WriteOutput("\n/**************************************************************");
                WriteOutput("\n*  ERROR !!! --- Clear Java code before adding C++ functions! *");
                WriteOutput("\n**************************************************************/\n");
                return;
            }
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            if (selected_classes.length < 1) {
                if (generate_all_enum_symbol_lookups) {
                    this.GenerateCppEnumInfo(null);
                }
                return;
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating C++ Format Function . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(2 * selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            SetFormatFunctionName(selected_classes);

            if (null != type_info.first_module_used_in
                    && null != type_info.first_module_used_in.getSymbolLookup()) {
                symbol_lookup_function_name = type_info.first_module_used_in.getSymbolLookup();
            } else {
                if (!format_function_name.toUpperCase().endsWith("FORMAT")) {
                    symbol_lookup_function_name = format_function_name;
                } else {
                    symbol_lookup_function_name = format_function_name.substring(0, format_function_name.length() - 6);
                }
                if (symbol_lookup_function_name.endsWith("_")) {
                    symbol_lookup_function_name += "symbol_lookup";
                } else {
                    symbol_lookup_function_name += "_symbol_lookup";
                }
            }

            String idlist_name = format_function_name.substring(0, format_function_name.length() - 6);
            idlist_name += "id_list";
            String sizelist_name = format_function_name.substring(0, format_function_name.length() - 6);
            sizelist_name += "size_list";
            String namelist_name = format_function_name.substring(0, format_function_name.length() - 6);
            namelist_name += "name_list";

            if (update_with_name) {
                WriteOutput("\n");
                int longest_name_length = 0;
                int number_of_names = 0;
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                        continue;
                    }
                    number_of_names++;
                    if (selected_classes[i].length() > longest_name_length) {
                        longest_name_length = selected_classes[i].length();
                    }
                }

                if (generate_symbol_lookups) {
                    WriteOutput("#ifndef MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH\n");
                    WriteOutput("#define MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH " + (longest_name_length + 1) + "\n");
                    WriteOutput("#endif\n");
                }
                WriteOutput("#ifndef " + namelist_name.toUpperCase() + "_LENGTH\n");
                WriteOutput("#define " + namelist_name.toUpperCase() + "_LENGTH " + (number_of_names + 1) + "\n");
                WriteOutput("#endif\n");
                WriteOutput("\n");

                if (generate_symbol_lookups) {
                    WriteOutput("\n/* This list must be in alphabetical order and the three lists must correspond. */\n");
                    WriteOutput("const char " + namelist_name + "[" + namelist_name.toUpperCase() + "_LENGTH][MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH]= {\n");
                    for (int i = 0; i < selected_classes.length; i++) {
                        StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                        if (null == typeInfo) {
                            continue;
                        }
                        if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                            continue;
                        }
                        WriteOutput("\t\"" + selected_classes[i] + "\", /* " + i + "," + typeInfo.Id + " */\n");
                    }
                    WriteOutput("\t\"\"};\n");
                }
                WriteOutput("const NMLTYPE " + idlist_name + "[" + namelist_name.toUpperCase() + "_LENGTH]= {\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                        continue;
                    }
                    WriteOutput("\t" + typeInfo.type_id_string + ", /* " + i + "," + typeInfo.Id + " */\n");
                }
                WriteOutput("\t-1};\n");
                WriteOutput("const size_t " + sizelist_name + "[" + namelist_name.toUpperCase() + "_LENGTH]= {\n");
                FileOutputStream sizes_fos = null;
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                        continue;
                    }
                    if (sizes_fos != null && create_print_sizes_file) {
                        WriteToFos(sizes_fos, "\tprintf(\"sizeof(" + selected_classes[i] + ")=%d\\n\",sizeof(" + selected_classes[i] + "));\n");
                    }
                    WriteOutput("\tsizeof(" + selected_classes[i] + "),\n");
                }

                if (sizes_fos != null && create_print_sizes_file) {
                    WriteToFos(sizes_fos, "}\n");
                    sizes_fos.close();
                }
                WriteOutput("\t0};\n");
                if (generate_symbol_lookups) {
                    WriteOutput("const char *" + symbol_lookup_function_name + "(long type);\n");
                }
                WriteOutput("\n");
                if (!no_enums) {
                    GenerateCppEnumInfo(selected_classes);
                }
            }

            WriteOutput("");
            WriteOutput("/*\n");
            try {
                int max_estimated_size = -1;
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    EstimateSize(typeInfo);
                    if (typeInfo.estimated_size > 0) {
                        if (typeInfo.estimated_size > max_estimated_size) {
                            max_estimated_size = typeInfo.estimated_size;
                        }
                        WriteOutput("Estimated_size\t" + typeInfo.getName() + "\t" + typeInfo.estimated_size + "\n");
                    }
                }
                WriteOutput("Estimated_size\tMAXIMUM\t" + max_estimated_size + "\n");

            } catch (Exception e) {
                e.printStackTrace();
            }
            WriteOutput("*/\n");
            WriteOutput("");

            WriteOutput("/*\n*\tNML/CMS Format function : " + format_function_name + "\n");
            WriteOutput("*/\n");

            WriteOutput("int " + format_function_name + "(NMLTYPE type, void *buffer, CMS *cms)\n{\n");
            if (update_with_name) {
                WriteOutput("\n");
                if (m_currentModule.definedValues.containsKey("DO_NOT_ADD_INDEXES_TO_ARRAY_NAMES")) {
                    WriteOutput("\tcms->add_array_indexes_to_name=false;\n");
                }
                if (!generate_symbol_lookups) {
                    WriteOutput("\ttype = cms->check_type_info(type,buffer,\"" + format_function_name.substring(0, format_function_name.length() - 7) + "\",\n" + "\t\t(cms_symbol_lookup_function_t) 0,\n" + "\t\t(const char **) 0,\n\t\t" + idlist_name + "," + sizelist_name + ",\n" + "\t\t" + namelist_name.toUpperCase() + "_LENGTH,\n" + "\t\t0);\n");
                } else {
                    WriteOutput("\ttype = cms->check_type_info(type,buffer,\"" + format_function_name.substring(0, format_function_name.length() - 7) + "\",\n" + "\t\t(cms_symbol_lookup_function_t) " + symbol_lookup_function_name + ",\n" + "\t\t(const char **)" + namelist_name + ",\n\t\t" + idlist_name + "," + sizelist_name + ",\n" + "\t\t" + namelist_name.toUpperCase() + "_LENGTH,\n" + "\t\t" + "MAX_" + namelist_name.substring(0, namelist_name.length() - 5).toUpperCase() + "_LENGTH);\n");
                }
                WriteOutput("\n");
                String fullheader = null;
                boolean uses_unbounded = false;
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null != typeInfo && typeInfo.Id > 0) {
                        if (null == fullheader) {
                            fullheader = new File(typeInfo.fromFileName).getAbsolutePath();
                        }
                        if (typeInfo.VarnameUnboundedHashTable != null
                                && typeInfo.VarnameUnboundedHashTable.size() > 0) {
                            uses_unbounded = true;
                            break;
                        }
                    }
                }
                if (null != fullheader && add_set_header) {
                    WriteOutput("\tcms->set_header_file(\"" + fullheader + "\");\n");
                }
                if (uses_unbounded) {
                    WriteOutput("\tcms->set_uses_unbounded(true);\n");
                }
            }
            int classes_in_switch = 0;
            for (int i = 0; i < selected_classes.length; i++) {
                StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null == typeInfo) {
                    continue;
                }
                if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                    continue;
                }
                classes_in_switch++;
            }
            if (classes_in_switch > 0) {
                WriteOutput("\tswitch(type)\n\t{\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null != m_loadingPanel && display_on) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                        m_loadingPanel.force_repaint(500);
                    }
                    if (null == typeInfo) {
                        continue;
                    }
                    if (typeInfo.Id <= 0 || typeInfo.DerivedFrom == null) {
                        continue;
                    }
                    WriteOutput("\tcase " + typeInfo.type_id_string + ":\n\t\t((" + selected_classes[i] + " *) buffer)->update(cms);\n\t\tbreak;\n");
                }
                WriteOutput("\n\tdefault" + ':' + "\n\t\treturn(0);\n");
                WriteOutput("\t}\n");
            }
            WriteOutput("\treturn 1;\n}\n");

            if (null != symbol_lookup_function_name && generate_symbol_lookups) {
                int types_in_switch = 0;
                for (int i = 0; i < selected_classes.length; i++) {
                    type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == type_info) {
                        continue;
                    }
                    if (type_info.Id <= 0) {
                        continue;
                    }
                    types_in_switch++;
                }
                if (types_in_switch > 0) {
                    WriteOutput("\n\n// NML Symbol Lookup Function\n");
                    WriteOutput("const char *" + symbol_lookup_function_name + "(long type)\n{\n");
                    WriteOutput("\tswitch(type)\n\t{\n");
                    for (int i = 0; i < selected_classes.length; i++) {
                        type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                        if (null != m_loadingPanel && display_on) {
                            m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                            m_loadingPanel.force_repaint(500);
                        }
                        if (null == type_info) {
                            continue;
                        }
                        if (type_info.Id <= 0) {
                            continue;
                        }
                        WriteOutput("\tcase " + type_info.type_id_string + ":\n\t\treturn \"" + selected_classes[i] + "\";\n");

                    }
                    WriteOutput("\tdefault" + ':' + "\n\t\treturn\"UNKNOWN\";\n\t\tbreak;\n");
                    WriteOutput("\t}\n");
                    WriteOutput("\treturn(NULL);\n}\n");
                } else {
                    WriteOutput("\n\n// NML Symbol Lookup Function\n");
                    WriteOutput("const char *" + symbol_lookup_function_name + "(long)\n{\n");
                    WriteOutput("\treturn(NULL);\n}\n");
                }
            }
            first_cpp_function = false;
            generate_cpp_format_function_needed = false;
            format_function_name = null;
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = false;
    }
    String currentOutputFileName = null;
    boolean cannot_write_to_output_file = false;

    public void SetOutputFile(String str) throws Exception {
        if (debug_on) {
            DebugPrint("CodeGenCommon.SetOutputFile(" + str + ") called.");
        }
        if (null == str) {
            return;
        }

        if (str.equals(currentOutputFileName)) {
            if (debug_on) {
                DebugPrint("CodeGenCommon.SetOutputFile(" + str + ") called when currentOutputFileName also equals " + currentOutputFileName);
            }
            return;
        }

        File outputFile = null;
        try {
            if (null != fos) {
                fos.close();
            }
            if (debug_on) {
                DebugPrint("Setting output file to " + str);
            }
        } catch (Exception e) {
            currentOutputFileName = null;
            e.printStackTrace();
            DebugPrint("current_directory = " + current_directory);
            DebugPrint("str = " + str);
        }
        if (null == current_directory) {
            outputFile = new File(str);
        } else {
//	        if(str.indexOf(File.separatorChar) > 0)
//		{
//		    String end_dir = str.substring(0,str.lastIndexOf(File.separatorChar)));
//		    if(current_directory.getAbsolutePath().endsWith(end_dir))
//		    {
//			outputFile = new File(current_directory, str);
//		    }
//		    else
//		    {
//			File newDir =  new File(current_directory,end_dir);
//			outputFile = new File(newDir,str.substring(str.lastIndexOf(File.separatorChar)+1));
//		    }
//		}
            outputFile = new File(current_directory, str);
        }
        if (null != outputFile) {
            if (!outputFile.canWrite() && outputFile.exists()) {
                cannot_write_to_output_file = true;
                throw new Exception("Can not write to " + outputFile.getPath());
            }
        }
        try {
//            System.out.println("outputFile = " + outputFile);
//            System.out.println("current_directory = " + current_directory);
            if (null == current_directory) {
                fos = new FileOutputStream(str);
            } else {
                fos = new FileOutputStream(new File(current_directory, str));
            }
            currentOutputFileName = str;
            first_cpp_function = true;
            first_java_class = true;
        } catch (Exception e) {
            currentOutputFileName = null;
            e.printStackTrace();
            DebugPrint("current_directory = " + current_directory);
            DebugPrint("str = " + str);
        }
        if (null != codeTextArea) {
            codeTextArea.setText("");
        }
    }

    public void GenerateC_FormatFunction() {
        try {
            if (!first_java_class) {
                WriteOutput("\n/**************************************************************");
                WriteOutput("\n*  ERROR !!! --- Clear Java code before adding C++ functions! *");
                WriteOutput("\n**************************************************************/\n");
                return;
            }
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            generating_code = true;
            C_Generator.GenerateC_FormatFunction(selected_classes, this, currentOutputFileName,
                    ModuleInfo.m_enumInfoHashTable, c_prototypes_header_string,
                    m_currentModule.definedValues.containsKey("DO_NOT_ADD_INDEXES_TO_ARRAY_NAMES"));
            first_cpp_function = false;
            generate_cpp_format_function_needed = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        generating_code = false;
    }

    public String get_base_name(String str) {
        String new_str = str;
        int slash_index = new_str.lastIndexOf('/');
        if (slash_index >= 0) {
            new_str = new_str.substring(slash_index + 1);
        }
        int bslash_index = new_str.lastIndexOf('\\');
        if (bslash_index >= 0) {
            new_str = new_str.substring(bslash_index + 1);
        }
        int colon_index = new_str.lastIndexOf(':');
        if (colon_index >= 0) {
            new_str = new_str.substring(colon_index + 1);
        }
        int dot_index = new_str.lastIndexOf('.');
        if (dot_index >= 0) {
            new_str = new_str.substring(0, dot_index);
        }
        return new_str;
    }

    public java.util.Hashtable get_optionsHashTable() {
        return this.optionsHashtable;
    }

    public void set_optionsHashTable(java.util.Hashtable ht) {
        this.optionsHashtable = ht;
    }

    public String RemoveStartingEndingSpace(String str) {
        try {
            while (str.length() > 0
                    && (str.startsWith(" ") || str.startsWith("\t"))) {
                str = str.substring(1);
            }
            while (str.length() > 0
                    && (str.endsWith(" ") || str.endsWith("\t"))) {
                str = str.substring(1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return str;
    }

    public boolean IsInteger(String str) {
        try {
            boolean digit_found = false;
            str = RemoveStartingEndingSpace(str);
            boolean is_hex = false;
            if (str.length() < 1) {
                return false;
            }
            if (str.startsWith("-") || str.startsWith("+")) {
                str = str.substring(1);
            }
            if (str.length() < 1) {
                return false;
            }
            String optest = "()+-*/<>^|& \t";
            String hex_test = "ABCDEFabcdef";
            for (int i = 0; i < str.length(); i++) {
                String cur_sub_string = str.substring(i);
                char cur_char = str.charAt(i);
                if (debug_on) {
                    DebugPrint("cur_sub_string = " + cur_sub_string);
                }
                if (cur_sub_string.startsWith("0x")) {
                    i += 1;
                    is_hex = true;
                    continue;
                }
                if (cur_sub_string.startsWith("int")) {
                    i += 2;
                    continue;
                }
                if (cur_sub_string.startsWith("long")) {
                    i += 3;
                    continue;
                }
                if (cur_sub_string.startsWith("short")) {
                    i += 4;
                    continue;
                }
                if (cur_sub_string.startsWith("unsigned")) {
                    i += 7;
                    continue;
                }
                if (optest.indexOf(cur_char) >= 0) {
                    continue;
                }
                if (!Character.isDigit(cur_char)) {
                    if (is_hex) {
                        if (hex_test.indexOf(cur_char) >= 0) {
                            digit_found = true;
                            continue;
                        }
                    }
                    return false;
                } else {
                    digit_found = true;
                }
            }
            return digit_found;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean IsDouble(String str) {
        boolean double_found = false;
        try {
            StringTokenizer tokenizer = new StringTokenizer(str, " \t()+-*/^");
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (token.equals("double")) {
                    continue;
                }
                if (token.equals("float")) {
                    continue;
                }
                Double.valueOf(token);
                double_found = true;
            }
        } catch (NumberFormatException ne) {
            return false;
        }
        return double_found;
    }
    // @SuppressWarnings("unchecked")

    public void GenerateJavaMessageDict() {
        File orig_current_dir = null;
        try {
            orig_current_dir = current_directory;
        } catch (Exception e) {
        }
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.GenerateJavaMessageDict() called.");
            }
            generating_code = true;
            String selected_classes[] = RemoveDuplicates(ClassList.getSelectedItems());
            if (selected_classes == null
                    || selected_classes.length < 1) {
                ErrorPrint("Trying to generated message dictionary " + output_file_name + " but there are no selected classes.");
                current_directory = orig_current_dir;
                return;
            }
            if (null != selected_classes && selected_classes.length > 0
                    && null != java_package_name
                    && null == current_directory
                    && output_file_name != null) {
                currentOutputFileName = null;
                String java_dir_name = "";
                StringTokenizer jtokenizer = new StringTokenizer(java_package_name, ".");
                while (jtokenizer.hasMoreTokens()) {
                    java_dir_name += jtokenizer.nextToken();
                    if (jtokenizer.hasMoreTokens()) {
                        java_dir_name += File.separator;
                    }
                    current_directory = new File(System.getProperty("user.dir"), java_dir_name);
                    current_directory.mkdirs();
                    output_file_name = (new File(output_file_name)).getName();
                }
            }
            if (null != output_file_name) {
                SetOutputFile(output_file_name);
            }
            if (debug_on) {
                if (null == selected_classes) {
                    DebugPrint("CodeGenCommon.GenerateJavaMessageDict() : selected_classes = null;");
                } else {
                    DebugPrint("CodeGenCommon.GenerateJavaMessageDict() : selected_classes.length = " + selected_classes.length);
                }
            }
            if (selected_classes.length < 1) {
                generating_code = false;
                return;
            }
            if (null == m_currentModule) {
                ErrorPrint("m_currentModule is null;\n");
                throw new NullPointerException();
            }
            StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[0]);
            if (null == type_info) {
                ErrorPrint("type_info is null;\n");
                throw new NullPointerException();
            }
            String messageDictClassName = m_currentModule.Name + "MsgDict";
            if (null != type_info.first_module_used_in) {
                messageDictClassName = type_info.first_module_used_in.getName() + "MsgDict";
            }
            if (first_java_class) {
                if (null != output_file_name) {
                    messageDictClassName = get_base_name(output_file_name);
                }
                javaFileName = messageDictClassName + ".java";
                WriteOutput("/*\n*\tNew Java File starts here.\n*\tThis file should be named " + javaFileName + "\n");
                WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
                WriteOutput("*\twith command line arguments : " + orig_args_one_string + "\n");
//                WriteOutput("*\tRCS_VERSION=" + rcs.RCS_VERSION.version_string);
                if (null != prev_lines_of_script) {
                    WriteOutput("*\n");
                    WriteOutput("*\t.gen script :\n");
                    if (null != lines_of_script) {
                        for (int i = 0; i < lines_of_script.length; i++) {
                            String s = (String) lines_of_script[i];
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    } else {
                        for (int i = 0; i < prev_lines_of_script.size(); i++) {
                            String s = (String) prev_lines_of_script.get(i);
                            WriteOutput("*\t\t" + i + ":" + s + "\n");
                        }
                    }
                    WriteOutput("*\n");
                }
                WriteOutput("*/\n\n");
                if (java_package_name != null) {
                    WriteOutput("\n// Set Package Name\npackage " + java_package_name + ";\n\n");
                }
                WriteOutput("// Import NML classes and interfaces\nimport rcs.nml.*;\n\n");
                if (update_with_name) {
                    WriteOutput("import java.util.Hashtable;\n");
                }
            }
            WriteOutput("/*\n*\tClass definition for " + messageDictClassName + "\n");
            WriteOutput("*\tAutomatically generated by NML CodeGen Java Applet.\n");
            WriteOutput("*/\n");
            if (first_java_class) {
                WriteOutput("public class " + messageDictClassName + " implements NMLMessageDictionary\n{\n");
            } else {
                WriteOutput("class " + messageDictClassName + " implements NMLMessageDictionary\n{\n");
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Generating Java Message Dictionary . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(5 * selected_classes.length);
                m_loadingPanel.force_repaint(500);
            }
            WriteOutput("\n\t// Define an object of every message class.\n");
            for (int i = 0; i < selected_classes.length; i++) {
                type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null == type_info) {
                    continue;
                }
                if (type_info.Id <= 0) {
                    continue;
                }
                WriteOutput("\tprivate " + selected_classes[i] + " " + selected_classes[i] + "_object = null;\n"); //new " + selected_classes[i] + "();\n");
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.force_repaint(500);
                }
            }
            WriteOutput("\n\t// ID Type Constants\n");
            Vector idConstants = new Vector();
            for (int i = 0; i < selected_classes.length; i++) {
                type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null == type_info) {
                    continue;
                }
                if (type_info.Id <= 0) {
                    continue;
                }
                String java_classname = selected_classes[i].replace(':', '_');
                if (type_info.conflicts) {
                    WriteOutput("\t// Class " + java_classname + " has conflicting ID : " + type_info.Id + "\n");
                    continue;
                }
                idConstants.add(java_classname + "_TYPE");
                WriteOutput("\tpublic static final int " + java_classname + "_TYPE  = " + type_info.Id + ";\n");
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.force_repaint(500);
                }
            }

            try {
                int max_estimated_size = -1;
                WriteOutput("\n");
                WriteOutput("\t// Sizes useful for C++ compat and preallocating byte storage. \n\t//(not used for normal NML reads/writes).\n");
                WriteOutput("\tpublic long getEstimatedSize(final int _type)\n");
                WriteOutput("\t{\n");
                WriteOutput("\t\tswitch(_type)\n");
                WriteOutput("\t\t{\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    StructureTypeInfo typeInfo = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == typeInfo) {
                        continue;
                    }
                    EstimateSize(typeInfo);
                    if (typeInfo.estimated_size > 0) {
                        if (typeInfo.estimated_size > max_estimated_size) {
                            max_estimated_size = typeInfo.estimated_size;
                        }
                        if (typeInfo.Id > 0) {
                            WriteOutput("\t\t\tcase " + selected_classes[i] + "_TYPE: /*" + type_info.Id + "*/\n");
                            WriteOutput("\t\t\t\treturn " + typeInfo.estimated_size + ";\n");
                        }
                    }
                }
                WriteOutput("\t\tdefault:\n");
                WriteOutput("\t\t\tbreak;\n");
                WriteOutput("\t\t}\n");
                WriteOutput("\t\treturn " + max_estimated_size + "; /* maximum */\n");
                WriteOutput("\t}\n");
                WriteOutput("\n");
                WriteOutput("\tpublic long getMaxEstimatedSize() {\n");
                WriteOutput("\t\treturn " + max_estimated_size + ";\n");
                WriteOutput("\t}\n");
                WriteOutput("\n");

                //WriteOutput("Estimated_size\tMAXIMUM\t" + max_estimated_size + "\n");
            } catch (Exception e) {
                e.printStackTrace();
            }
            WriteOutput("\n");

            if (update_with_name) {
                WriteOutput("\n\t//Define an NML_ENUM_INFO object for the type ID's\n");
                WriteOutput("\tNML_ENUM_INFO nml_enum_info_for_type_names =null;\n");
                WriteOutput("\n");

                WriteOutput("\n\t//Create a constructor to setup the NML_ENUM_INFO object.\n");
                WriteOutput("\tpublic " + messageDictClassName + "()\n");
                WriteOutput("\t{\n");
                WriteOutput("\t\tnml_enum_info_for_type_names= new NML_ENUM_INFO();\n");
                WriteOutput("\t\tnml_enum_info_for_type_names.name=\"" + messageDictClassName + "\";\n");
                WriteOutput("\t\tHashtable h1 = new Hashtable();\n");
                WriteOutput("\t\tHashtable h2 = new Hashtable();\n");
                WriteOutput("\t\tInteger I = null;\n");
                WriteOutput("\t\tString  S = null;\n");

                for (int i = 0; i < selected_classes.length; i++) {
                    type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == type_info) {
                        continue;
                    }
                    if (type_info.Id <= 0) {
                        continue;
                    }
                    if (type_info.conflicts) {
                        WriteOutput("\n\t\t// Conflict for type " + type_info.getName() + " with ID " + type_info.Id + "\n");
                        continue;
                    }
                    WriteOutput("\t\tI= (" + selected_classes[i] + "_TYPE);\n");
                    WriteOutput("\t\tS=\"" + selected_classes[i] + "\";\n");
                    WriteOutput("\t\th1.put(I,S); h2.put(S,I);\n");

                    if (null != m_loadingPanel && display_on) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                        m_loadingPanel.force_repaint(500);
                    }
                }
                WriteOutput("\t\tnml_enum_info_for_type_names.int_to_string_hash=h1;\n");
                WriteOutput("\t\tnml_enum_info_for_type_names.string_to_int_hash=h2;\n");
                WriteOutput("\t}\n");
                WriteOutput("\n");
            }

            try {

                if (null != m_currentModule.definedValues) {
                    WriteOutput("\n\n\t// Miscellaneous Pre-Defined Values\n");
                    Vector dvs_vector = new Vector();
                    Enumeration dvs = m_currentModule.definedValues.elements();
                    while (dvs.hasMoreElements()) {
                        boolean dv_in_enum = false;
                        DefinedValue dv = (DefinedValue) dvs.nextElement();
                        try {
                            Enumeration enum_info_types = ModuleInfo.m_enumInfoHashTable.elements();
                            while (enum_info_types.hasMoreElements()) {
                                EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
                                if (enum_info.reverse_hashtable.containsKey(dv.name)) {
                                    dv_in_enum = true;
                                    break;
                                }
                            }
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                        if (dv_in_enum) {
                            continue;
                        }
                        if (dv.name.endsWith("TYPE")) {
                            continue;
                        }
                        if (dv.name.startsWith("__")) {
                            continue;
                        }
                        if (dv.name.equals("JAVA_DIAG_APPLET")) {
                            continue;
                        }
                        if (dv.name.equals("JAVA_CODEGEN")) {
                            continue;
                        }
                        dv.value = dv.value.trim();
                        if (dv.value.length() < 1) {
                            continue;
                        }
                        int i = 0;
                        for (i = 0; i < dvs_vector.size(); i++) {
                            DefinedValue dvi = (DefinedValue) dvs_vector.elementAt(i);
                            if (dvi.name.compareTo(dv.name) > 0) {
                                dvs_vector.insertElementAt(dv, i);
                                break;
                            }
                        }
                        if (i >= dvs_vector.size()) {
                            dvs_vector.insertElementAt(dv, i);
                        }
                    }
                    for (int ii = 0; ii < dvs_vector.size(); ii++) {
                        DefinedValue dv = (DefinedValue) dvs_vector.elementAt(ii);
                        if (debug_on) {
                            DebugPrint("dv.name = " + dv.name);
                            DebugPrint("dv.value = " + dv.value);
                        }
                        if (dv.name.indexOf("(") >= 0 || dv.name.indexOf(")") >= 0 || dv.name.indexOf(",") >= 0) {
                            continue;
                        }
                        boolean surrounded_by_parens = false;
                        if (dv.value.startsWith("(") || dv.value.endsWith(")")) {
                            if (debug_on) {
                                DebugPrint("dv.value (surrounded_by_parens) = " + dv.value);
                            }
                            surrounded_by_parens = true;
                        }
                        if (IsInteger(dv.value)) {
                            WriteOutput("\tpublic static final int " + dv.name + "  = " + dv.value + ";\n");
                        } else if (IsDouble(dv.value)) {
                            WriteOutput("\tpublic static final double " + dv.name + "  = " + dv.value + ";\n");
                        } else if (!surrounded_by_parens) {
                            if (dv.value.startsWith("\"$") || dv.value.startsWith("$")) {
                                // Skip strings that are CVS or subversion keywords like $Id:
                                //  they cause too many trivial updates.
                                WriteOutput("\t\t// Skipped " + dv.name + " value starts with $ and could be CVS/subversion keyword.");
                                continue;
                            }
                            if (!dv.value.startsWith("\"") || !dv.value.endsWith("\"")) {
                                WriteOutput("\tpublic static final String " + dv.name + "  = \"" + dv.value + "\";\n");
                            } else {
                                WriteOutput("\tpublic static final String " + dv.name + "  = " + dv.value + ";\n");
                            }
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

            try {
                if (null != ModuleInfo.m_enumInfoHashTable) {
                    WriteOutput("\n\t// Enumerated Type Constants\n");
                    Enumeration enum_info_types = ModuleInfo.m_enumInfoHashTable.elements();
                    while (enum_info_types.hasMoreElements()) {
                        EnumTypeInfo enum_info = (EnumTypeInfo) enum_info_types.nextElement();
                        Vector enum_vector = new Vector();
                        if (null == enum_info) {
                            continue;
                        }
                        if (null == enum_info.reverse_hashtable) {
                            continue;
                        }
                        if (enum_info.reverse_hashtable.size() < 1) {
                            continue;
                        }
                        WriteOutput("\n\t// " + enum_info.Name + "\n");
                        Enumeration enum_keys = enum_info.reverse_hashtable.keys();
                        while (enum_keys.hasMoreElements()) {
                            String key = (String) enum_keys.nextElement();
                            boolean keyinserted = false;
                            for (int i = 0; i < enum_vector.size(); i++) {
                                String keycompare = (String) enum_vector.elementAt(i);
                                if (key.compareTo(keycompare) < 0) {
                                    enum_vector.insertElementAt(key, i);
                                    keyinserted = true;
                                    break;
                                }
                            }
                            if (!keyinserted) {
                                enum_vector.addElement(key);
                            }
                        }
                        for (int i = 0; i < enum_vector.size(); i++) {
                            String key = (String) enum_vector.elementAt(i);
                            Integer value = (Integer) enum_info.reverse_hashtable.get(key);
                            boolean key_is_type_const = false;
                            if (null != idConstants) {
                                for (int id_index = 0; id_index < idConstants.size(); id_index++) {
                                    String idConstName = (String) idConstants.get(id_index);
                                    //System.out.println("comparing idConstName="+idConstName+" and key="+key);
                                    if (null == idConstName) {
                                        break;
                                    }
                                    if (key.compareTo(idConstName) == 0) {
                                        key_is_type_const = true;
                                        break;
                                    }
                                }
                            }
                            if (key_is_type_const) {
                                WriteOutput("\t// Constant " + key + "=" + value + " already set as a message id.\n");
                                continue;
                            }
                            if (null != key && null != value && !idConstants.contains(key)) {
                                WriteOutput("\tpublic static final int " + key.replace(':', '_') + "=" + value + ";\n");
                            }
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
            WriteOutput("\n\n\t// NML Format Function\n");
            WriteOutput("\tpublic int formatMsg(NMLFormatConverter nml_fc)\n\t{\n");
            WriteOutput("\t\tint return_val=0;\n");
            if (update_with_name) {
                if (m_currentModule.definedValues.containsKey("DO_NOT_ADD_INDEXES_TO_ARRAY_NAMES")) {
                    WriteOutput("\t\tnml_fc.add_array_indexes_to_name=false;\n");
                }
                WriteOutput("\t\tnml_fc.check_type_info(nml_enum_info_for_type_names);\n\n");
            }
            if (selected_classes.length > 0) {
                WriteOutput("\t\tswitch(nml_fc.msg_type)\n\t\t{\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    final String selected_class_i = selected_classes[i];
                    type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == type_info) {
                        WriteOutput("\n\t\t// classname " + selected_class_i + " has no type information associated with it.\n");
                        continue;
                    }
                    if (type_info.Id <= 0) {
                        WriteOutput("\n\t\t// Type " + type_info.getName() + " has Id less than 1 :  " + type_info.Id + "\n");
                        continue;
                    }
                    if (type_info.conflicts) {
                        WriteOutput("\n\t\t// Conflict for type " + type_info.getName() + " with ID " + type_info.Id + "\n");
                        continue;
                    }
                    WriteOutput("\t\tcase " + selected_class_i + "_TYPE: /*" + type_info.Id + "*/\n");
                    WriteOutput("\t\t\tif(null == " + selected_class_i + "_object) { \n\t\t\t\t" + selected_class_i + "_object = new " + selected_class_i + "();\n\t\t\t}\n");
                    WriteOutput("\t\t\tnml_fc.msg_to_update  = " + selected_class_i + "_object;\n");
                    WriteOutput("\t\t\t" + selected_class_i + "_object.update(nml_fc);\n\t\t\tbreak;\n");
                    if (null != m_loadingPanel && display_on) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                        m_loadingPanel.force_repaint(500);
                    }
                }
                WriteOutput("\n\t\tdefault" + ":" + "\n\t\t\treturn_val=-1;\n" + "\t\t\tbreak;\n");
                WriteOutput("\t\t}\n");
            } else {
                WriteOutput("\t\t// WARNING no classes in CodeGen selected classes list/\n");
                WriteOutput("\t\treturn_val=-1;\n");
            }
            WriteOutput("\t\treturn(return_val);\n\t}\n");

            if (generate_symbol_lookups) {
                WriteOutput("\n\n\t// NML Symbol Lookup Function\n");
                WriteOutput("\tpublic static String SymbolLookup(int type)\n\t{\n");
                WriteOutput("\t\tswitch(type)\n\t\t{\n");
                for (int i = 0; i < selected_classes.length; i++) {
                    type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                    if (null == type_info) {
                        WriteOutput("\n\t\t// classname " + selected_classes[i] + " has no type information associated with it.\n");
                        continue;
                    }
                    if (type_info.Id <= 0) {
                        WriteOutput("\n\t\t// Type " + type_info.getName() + " has Id less than 1 :  " + type_info.Id + "\n");
                        continue;
                    }
                    if (type_info.conflicts) {
                        WriteOutput("\n\t\t// Conflict for type " + type_info.getName() + " with ID " + type_info.Id + "\n");
                        continue;
                    }
                    String jclassname = selected_classes[i].replace(':', '_');
                    WriteOutput("\t\tcase " + jclassname + "_TYPE:\n\t\t\treturn \"" + jclassname + "\";\n");
                    if (null != m_loadingPanel && display_on) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                        m_loadingPanel.force_repaint(500);
                    }
                }
                WriteOutput("\n\t\tdefault" + ":\n\t\t\tbreak;\n");
                WriteOutput("\t\t}\n\t\treturn(\"!!UNDEFINED_SYMBOL!!\");\n\t}\n");
            }
            WriteOutput("\n}\n");
            first_java_class = false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            current_directory = orig_current_dir;
        } catch (Exception e) {
        }
        generate_java_dictionary_needed = false;
        generating_code = false;
    }

    public void ClearAll() {
        if (null != codeTextArea) {
            codeTextArea.setText("");
        }
        MakeListGeneric(ClassList);
        if (null != ClassList) {
            int selected_indexes[] = ClassList.getSelectedIndexes();
            if (null != selected_indexes) {
                for (int i = 0; i < selected_indexes.length; i++) {
                    ClassList.deselect(selected_indexes[i]);
                }
            }
        }
        format_function_name = null;
        java_package_name = null;
    }

    public void AddAlphabatizedItem(diagapplet.utils.FastListPanelInterface lst, String item, boolean select_it) {
        int i = 0;
        int num_items = 0;
        int num_items_selected = 0;
        try {
            if (debug_on) {
                DebugPrint("AddAlphabatizedItem: lst=" + lst + ", item=" + item + ", select_it=" + select_it);
            }
            num_items = lst.countItems();
            num_items_selected = lst.getSelectedIndexes().length;
            if (debug_on) {
                DebugPrint("num_items=" + num_items + ", num_items_selected=" + num_items_selected);
            }
            if (num_items < 1) {
                if (select_it) {
                    lst.add(item);
                    lst.select(0);
                } else {
                    lst.add(item, -1);
                }
                if (debug_on) {
                    num_items = lst.countItems();
                    num_items_selected = lst.getSelectedIndexes().length;
                    DebugPrint("num_items=" + num_items + ", num_items_selected=" + num_items_selected);
                }
                return;
            }
            if (select_it) {
                for (i = 0; i < num_items_selected; i++) {
                    if (item.toUpperCase().compareTo(lst.getItem(i).toUpperCase()) < 0) {

                        lst.add(item, i);
                        lst.select(i);
                        if (debug_on) {
                            num_items = lst.countItems();
                            num_items_selected = lst.getSelectedIndexes().length;
                            DebugPrint("num_items=" + num_items + ", num_items_selected=" + num_items_selected);
                        }
                        return;
                    }
                }
            } else {
                for (i = num_items_selected; i < num_items; i++) {
                    if (item.toUpperCase().compareTo(lst.getItem(i).toUpperCase()) < 0) {

                        lst.add(item, i);
                        if (debug_on) {
                            num_items = lst.countItems();
                            num_items_selected = lst.getSelectedIndexes().length;
                            DebugPrint("num_items=" + num_items + ", num_items_selected=" + num_items_selected);
                        }
                        return;
                    }
                }
            }

            if (select_it) {
                lst.add(item, num_items_selected);
                lst.select(num_items_selected);
            } else {
                lst.add(item, -1);
            }

        } catch (Exception e) {
            ErrorPrint("Error adding " + item + " to List. i = " + i + ", num_items = " + num_items);
            e.printStackTrace();
        }

        try {
            if (debug_on) {
                DebugPrint("num_items=" + num_items + ", num_items_selected=" + num_items_selected);
            }
        } catch (Exception e) {
        }

    }

    public boolean is_generic(String str) {
        if (null == generic_classes) {
            return false;
        }
        boolean isgen = generic_classes.contains(str);
        if (debug_on) {
            DebugPrint("Class " + str + " is generic: " + isgen);
        }
        return isgen;
    }
    Vector selectedItems = null;
    // @SuppressWarnings("unchecked")

    public void SelectByFromFile(String filename) {
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.SelectByFromFile(" + filename + ") called.");
            }
            if (!select_from_all_files) {
                ClassList.clear();
            }
            if (selectedItems == null || !select_from_all_files) {
                selectedItems = new Vector();
            }
            int select_count = 0;
            int class_count = 0;
            int pathindex = filename.lastIndexOf(File.separator);
            if (pathindex >= 0) {
                filename = filename.substring(pathindex + 1);
            }
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("SelectByFromFile . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                m_loadingPanel.force_repaint(500);
            }
            Enumeration structByNameEnum = ModuleInfo.m_structInfoByNameHashTable.keys();
            while (structByNameEnum.hasMoreElements()) {
                String stringToAdd = ((String) structByNameEnum.nextElement());
                if (debug_on) {
                    DebugPrint("stringToAdd = " + stringToAdd);
                }
                StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(stringToAdd);
                if (debug_on) {
                    DebugPrint("sti = " + sti);
                }
                boolean select_it = false;
                if (null != sti) {
                    if (sti.contains_pointers || sti.contains_unrecognized_type || !select_from_all_files) {
                        sti.selected = false;
                    }
                    select_it = sti.selected;
                    if (null != sti.fromFileName && !select_it) {
                        String file_name_only = sti.fromFileName;
                        int sindex = file_name_only.lastIndexOf(File.separator);
                        if (sindex >= 0) {
                            file_name_only = file_name_only.substring(sindex + 1);
                        }
                        select_it = file_name_only.equals(filename);
                        if (debug_on) {
                            DebugPrint("(" + file_name_only + ").equals(" + filename + ") = " + select_it);
                        }
                    }
                    sti.selected = select_it;
                }
                if (select_it) {
                    select_count++;
                    selectedItems.addElement(stringToAdd);
                    if (debug_on) {
                        DebugPrint("select_it=true;");
                        DebugPrint("select_count=" + select_count);
                    }
                }
                class_count++;
                if (debug_on) {
                    DebugPrint("select_it = " + select_it);
                }
                AddAlphabatizedItem(ClassList, stringToAdd, select_it);
                if (debug_on) {
                    DebugPrint("AddAlphabtizedItem returned.");
                }
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                    m_loadingPanel.force_repaint(500);
                }
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }

            }
            //DebugPrint("select_count = "+select_count);
            //DebugPrint("class_count = "+class_count);
            String selected_classes[] = ClassList.getSelectedItems();
            int tries = 0;
            while (selected_classes.length != select_count && tries < 20) {
                if (debug_on) {
                    DebugPrint("select_count = " + select_count + " but selected_classes.length = " + selected_classes.length);
                    DebugPrint("selectedItems:");
                    for (int i = 0; i < select_count; i++) {
                        DebugPrint((String) selectedItems.elementAt(i));
                    }
                    DebugPrint("selected_classes:");
                    for (int i = 0; i < selected_classes.length; i++) {
                        DebugPrint(selected_classes[i]);
                    }
                }
                Thread.sleep(100);
                tries++;
            }
            if (debug_on) {
                DebugPrint("selectedItems:");
                for (int i = 0; i < select_count; i++) {
                    DebugPrint((String) selectedItems.elementAt(i));
                }
                DebugPrint("selected_classes:");
                for (int i = 0; i < selected_classes.length; i++) {
                    DebugPrint(selected_classes[i]);
                }
            }
            rcs_status_enum_needed = false;
            for (int i = 0; i < selected_classes.length; i++) {
                StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null != type_info) {
                    if (IsRcsStatMsg(type_info.getName())) {
                        rcs_status_enum_needed = true;
                        if (IsRcsStatMsgV2(type_info.getName())) {
                            rcs_admin_state_enum_needed = true;
                        }
                        if (rcs_admin_state_enum_needed) {
                            break;
                        }
                    }
                    if (type_info.infoContains("RCS_STATUS")) {
                        rcs_status_enum_needed = true;
                        if (rcs_admin_state_enum_needed) {
                            break;
                        }
                    }
                    if (type_info.infoContains("RCS_ADMIN_STATE")) {
                        rcs_admin_state_enum_needed = true;
                        if (rcs_status_enum_needed) {
                            break;
                        }
                    }
                }
            }
            if (!generate_all_enum_symbol_lookups) {
                if (debug_on) {
                    DebugPrint("rcs_status_enum_needed=" + rcs_status_enum_needed);
                }
                if (!rcs_status_enum_needed && !force_keep_status_enum) {
                    try {
                        ModuleInfo.m_enumInfoHashTable.remove("RCS_STATUS");
                    } catch (Exception e) {
                    }
                }
                if (debug_on) {
                    DebugPrint("rcs_admin_state_enum_needed=" + rcs_admin_state_enum_needed);
                }
                if (!rcs_admin_state_enum_needed && !force_keep_admin_state_enum) {
                    try {
                        ModuleInfo.m_enumInfoHashTable.remove("RCS_ADMIN_STATE");
                    } catch (Exception e) {
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public void SelectFromAllFiles() {
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.SelectFromAllFiles() called.");
            }

            ClassList.clear();
            selectedItems = new Vector();
            int select_count = 0;
            int class_count = 0;
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("SelectByFromFile . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                m_loadingPanel.force_repaint(500);
            }
            Enumeration structByNameEnum = ModuleInfo.m_structInfoByNameHashTable.keys();
            while (structByNameEnum.hasMoreElements()) {
                String stringToAdd = ((String) structByNameEnum.nextElement());
                if (debug_on) {
                    DebugPrint("stringToAdd = " + stringToAdd);
                }
                StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(stringToAdd);
                if (debug_on) {
                    DebugPrint("sti = " + sti);
                }
                boolean select_it = false;
                if (null != sti) {
                    if (null != sti.fromFileName) {
                        select_it = true;
                    }
                    sti.selected = select_it;
                }
                if (select_it) {
                    select_count++;
                    selectedItems.addElement(stringToAdd);
                    if (debug_on) {
                        DebugPrint("select_it=true;");
                        DebugPrint("select_count=" + select_count);
                    }
                }
                class_count++;
                if (debug_on) {
                    DebugPrint("select_it = " + select_it);
                }
                AddAlphabatizedItem(ClassList, stringToAdd, select_it);
                if (debug_on) {
                    DebugPrint("AddAlphabtizedItem returned.");
                }
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                    m_loadingPanel.force_repaint(500);
                }
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
            }
            //DebugPrint("select_count = "+select_count);
            //DebugPrint("class_count = "+class_count);
            String selected_classes[] = ClassList.getSelectedItems();
            int tries = 0;
            while (selected_classes.length != select_count && tries < 20) {
                if (debug_on) {
                    DebugPrint("select_count = " + select_count + " but selected_classes.length = " + selected_classes.length);
                    DebugPrint("selectedItems:");
                    for (int i = 0; i < select_count; i++) {
                        DebugPrint((String) selectedItems.elementAt(i));
                    }
                    DebugPrint("selected_classes:");
                    for (int i = 0; i < selected_classes.length; i++) {
                        DebugPrint(selected_classes[i]);
                    }
                }
                Thread.sleep(100);
                tries++;
            }
            if (debug_on) {
                DebugPrint("selectedItems:");
                for (int i = 0; i < select_count; i++) {
                    DebugPrint((String) selectedItems.elementAt(i));
                }
                DebugPrint("selected_classes:");
                for (int i = 0; i < selected_classes.length; i++) {
                    DebugPrint(selected_classes[i]);
                }
            }
            rcs_status_enum_needed = false;
            for (int i = 0; i < selected_classes.length; i++) {
                StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(selected_classes[i]);
                if (null != type_info) {
                    if (IsRcsStatMsg(type_info.getName())) {
                        rcs_status_enum_needed = true;
                        if (IsRcsStatMsgV2(type_info.getName())) {
                            rcs_admin_state_enum_needed = true;
                        }
                        if (rcs_admin_state_enum_needed) {
                            break;
                        }
                    }
                    if (type_info.infoContains("RCS_STATUS")) {
                        rcs_status_enum_needed = true;
                        if (rcs_admin_state_enum_needed) {
                            break;
                        }
                    }
                    if (type_info.infoContains("RCS_ADMIN_STATE")) {
                        rcs_admin_state_enum_needed = true;
                        if (rcs_status_enum_needed) {
                            break;
                        }
                    }
                }
            }
            if (debug_on) {
                DebugPrint("rcs_status_enum_needed=" + rcs_status_enum_needed);
            }
            if (!rcs_status_enum_needed) {
                try {
                    ModuleInfo.m_enumInfoHashTable.remove("RCS_STATUS");
                } catch (Exception e) {
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void InitializeClassList() throws Exception {
        try {
            if (null == ClassList) {
                throw new Exception("CodeGenCommon.InitializeClassList() : ClassList is null.");
            }
            ClassList.clear();
            if (null != m_loadingPanel && display_on) {
                m_loadingPanel.set_URLname("Initializing Class List . . . ");
                m_loadingPanel.set_bytes_read(0);
                m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                m_loadingPanel.force_repaint(500);
            }
            Enumeration structByNameEnum = ModuleInfo.m_structInfoByNameHashTable.keys();
            while (structByNameEnum.hasMoreElements()) {
                String stringToAdd = ((String) structByNameEnum.nextElement());
                if (debug_on) {
                    DebugPrint("stringToAdd=" + stringToAdd);
                }
                StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(stringToAdd);
                if (debug_on) {
                    DebugPrint("sti=" + sti);
                }
                if (!sti.getName().equals(stringToAdd)) {
                    DebugPrint("sti.Name=\"" + sti.getName() + "\" != stringToAdd=\"" + stringToAdd + "\"");
                    ErrorPrint("sti.Name=\"" + sti.getName() + "\" != stringToAdd=\"" + stringToAdd + "\"");
                    throw new Exception("sti.Name=\"" + sti.getName() + "\" != stringToAdd=\"" + stringToAdd + "\"");
                }
                sti.selected = !(is_generic(stringToAdd));
                sti.generic = !sti.selected;
                AddAlphabatizedItem(ClassList, stringToAdd, sti.selected);
                if (null != m_loadingPanel && display_on) {
                    m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + 1);
                    m_loadingPanel.set_content_length(ModuleInfo.m_structInfoByNameHashTable.size());
                    m_loadingPanel.force_repaint(500);
                }
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
            }
            if (display_on) {
                Thread.sleep(10);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public void ResetGenericClasses() {
        generic_classes = new Vector();
        if (UseDefaultTypes) {
            generic_classes.addElement("NMLmsg");
            generic_classes.addElement("RCS_CMD_MSG");
            generic_classes.addElement("RCS_STAT_MSG");
            generic_classes.addElement("RCS_STAT_MSG_V2");
            generic_classes.addElement("CMS_DATE");
            generic_classes.addElement("CMS_DATE_TIME");
            generic_classes.addElement("CMS_TIME");
            generic_classes.addElement("CMS_DURATION");
            generic_classes.addElement("PM_CARTESIAN");
            generic_classes.addElement("PM_CYLINDRICAL");
            generic_classes.addElement("PM_EULER_ZYX");
            generic_classes.addElement("PM_EULER_ZYZ");
            generic_classes.addElement("PM_HOMOGENEOUS");
            generic_classes.addElement("PM_XYA");
            generic_classes.addElement("PM_QUATERNION");
            generic_classes.addElement("PM_ROTATION_MATRIX");
            generic_classes.addElement("PM_ROTATION_VECTOR");
            generic_classes.addElement("PM_POSE");
            generic_classes.addElement("PM_RPY");
            generic_classes.addElement("PM_SPHERICAL");
            generic_classes.addElement("NML_ERROR");
            generic_classes.addElement("NML_DISPLAY");
            generic_classes.addElement("NML_TEXT");
        }
    }

    public void ParseOptionsInfo() {
        try {
            if (null == optionsInfo) {
                return;
            }
            if (null == optionsHashtable) {
                optionsHashtable = new Hashtable();
            }
            StringTokenizer optionsTokenizer = new StringTokenizer(optionsInfo, ";");
            while (optionsTokenizer.hasMoreTokens()) {
                String optline = optionsTokenizer.nextToken();
                int eqindex = optline.indexOf("=");
                if (eqindex <= 0) {
                    continue;
                }
                String optname = remove_leading_whitespace_static(optline.substring(0, eqindex));
                String optval = remove_leading_whitespace_static(optline.substring(eqindex + 1));
                int firstquote = optval.indexOf("\"");
                int secondquote = optval.indexOf("\"", firstquote + 1);
                if (firstquote >= 0 && secondquote > firstquote) {
                    optval = optval.substring(firstquote + 1, secondquote);
                }
                if (debug_on) {
                    DebugPrint("optionsHashtable.put(optname=" + optname + ", optval=" + optval + ");");
                }
                optionsHashtable.put(optname, optval);
                if (optname.compareTo("extra_action") == 0) {
                    if (extraActionsVector == null) {
                        extraActionsVector = new Vector();
                    }
                    extraActionsVector.add(optval);
                }
                if (optname.compareTo("extra_tab") == 0) {
                    if (extraTabsVector == null) {
                        extraTabsVector = new Vector();
                    }
                    extraTabsVector.add(optval);
                }
                if (optname.compareTo("add_search_path") == 0) {
                    rcs.utils.URL_and_FileLoader.AddToSearchPath(optval);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public void AddAuxBufferModule(String buffer_name, String header, String nml_file) {
        ModuleInfo mi = new ModuleInfo(diag_dict_creator, nml_creator);
        mi.Name = buffer_name;
        mi.no_cmd = true;
        mi.no_stat = true;
        if (null != header) {
            mi.AddAuxInputType(header);
        }
        mi.AddAuxInput(buffer_name);
        mi.NMLConfigurationFile = nml_file;
        mi.LoadPredefinedTypes();
        mi.LoadAuxTypes();
        if (null == header) {
            mi.AddAllAuxMessages();
        }
        if (!preserve_modules_hashtable) {
            if (null == m_modulesHashTable) {
                m_modulesHashTable = new Hashtable();
            }
            Hashtable ht = m_modulesHashTable;
            if (null != ht) {
                ht.put((Object) mi.Name, (Object) mi);
            }
        }
    }
    static public LoadHierarchyUpdateInterface lhui = null;

    public void LoadHierarchyNewThread(String HierarchyName,
            LoadHierarchyUpdateInterface _lhui) {
        lhui = _lhui;
        ModuleInfo.lhui = _lhui;
        m_hierarchyFile = HierarchyName;
        new Thread(new Runnable() {
            public void run() {
                try {
                    LoadHierarchy();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }).start();
    }

    private String ReplaceDefinedValues(String parseLine, String defines) {
        String parseLineOut = parseLine;
        while (true) {
            int dollarBraceIndex = parseLineOut.indexOf("${");
            // System.out.println("dollarBraceIndex = " + dollarBraceIndex);
            if (dollarBraceIndex < 0) {
                break;
            }
            int endBraceIndex = parseLineOut.indexOf('}', dollarBraceIndex);
            // System.out.println("endBraceIndex = " + endBraceIndex);
            if (endBraceIndex < 0) {
                break;
            }
            String start = parseLineOut.substring(0, dollarBraceIndex);
            // System.out.println("start = " + start);
            String end = parseLineOut.substring(endBraceIndex + 1);
            // System.out.println("end = " + end);
            String var = parseLineOut.substring(dollarBraceIndex + 2, endBraceIndex);
            // System.out.println("var = " + var);
            String value = "";
            // System.out.println("defines = " + defines);
            int defines_var_index = defines.indexOf(":" + var + "=");
            // System.out.println("defines_var_index = " + defines_var_index);
            if (defines_var_index >= 0) {
                int eq_index = defines.indexOf('=', defines_var_index + 1);
                int colon_index = defines.indexOf(':', defines_var_index + 1);
                if (colon_index > 0 && eq_index > 0 && colon_index > eq_index) {
                    value = defines.substring(eq_index + 1, colon_index);
                }
            } else {
                String var_env = diagapplet.CodeGen.StringFuncs.getenv(var);
                if (var_env != null) {
                    value = var_env;
                }
            }
            parseLineOut = start + value + end;
            // System.out.println("parseLineOut = " + parseLineOut);
        }
        return parseLineOut;
    }
    // @SuppressWarnings("unchecked")
    private boolean insideFalseIfdef = false;
    private boolean lastIfdefFalse = false;
    private int ifdef_level = 0;
    private Vector ifdef_vector = null;
    private int content_length = 0;
    private int total_lines = 0;
    private int lines_read = 0;
    private boolean insideComment = false;
    private boolean insideModule = false;
    private ModuleInfo currentModule = null;
    private String defines = "";
    private int starting_load_button_count = 0;

    private void LoadHierarchyInternal(final String hierarchyFileName) throws Exception {

        int indexQuoteBegin = -1;
        int indexQuoteEnd = -1;
        int indexCCommentBegin = -1;
        int indexCCommentEnd = -1;
        int indexCppCommentBegin = -1;
        URL_and_FileLoader loader = new URL_and_FileLoader(hierarchyFileName);

        int total_lines_this_file = 0;
        int lines_read_this_file = 0;
        try {
            while (loader.readLine() != null) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                total_lines_this_file++;
            }
        } catch (Exception exl) {
            if (!interrupt_loading) {
                exl.printStackTrace();
            }
        }
        loader.close();
        total_lines += total_lines_this_file;
        loader = new URL_and_FileLoader(hierarchyFileName);
        content_length += loader.content_length;

        int info_start_line = 0;
        int bytes_read = 0;
        if (null != lhui) {
            lhui.update_main(m_hierarchyFile, lines_read, total_lines + 3);
        }
        if (null != m_loadingPanel) {
            m_loadingPanel.set_content_length(loader.content_length);
            m_loadingPanel.set_URLname(m_hierarchyFile);
            bytes_read = 0;
            content_length = m_loadingPanel.get_content_length();
            if (debug_on) {
                DebugPrint("Parsing " + m_hierarchyFile + " (length = " + m_loadingPanel.get_content_length() + ")");
            }
        }
        while (true) {
            String parseString = loader.readLine();
            lines_read++;
            lines_read_this_file++;
            if (debug_on) {
                DebugPrint("lines_read=" + lines_read + ",loader.readLine() = " + parseString);
            }
            if (parseString == null) {
                break;
            }
            parseString = ReplaceDefinedValues(parseString, defines);
            if (null != m_hierarchyFileLoadButton) {
                if (starting_load_button_count != m_hierarchyFileLoadButton.get_count()) {
                    break;
                }
            }
            if (Thread.interrupted()) {
                interrupt_loading = true;
                throw new Exception("Thread.interrupted() returned true\n");
            }
            if (interrupt_loading) {
                throw new Exception("interrupt_loading=true\n");
            }
            if (parseString.length() < 1) {
                continue;
            }
            bytes_read += parseString.length();
            content_length = loader.content_length;
            if (null != m_loadingPanel) {
                m_loadingPanel.set_bytes_read(bytes_read);
                m_loadingPanel.updateDisplay();
            }
            while (parseString != null && parseString.length() > 0) {
                if (parseString.charAt(0) != ' ') {
                    break;
                }
                parseString = parseString.substring(1);
            }
            if (parseString.regionMatches(true, 0, "END", 0, 3)) {
                break;
            }
            if (parseString.regionMatches(false, 0, "#", 0, 1)) {
                try {
                    if (debug_on) {
                        DebugPrint2("parseString=" + parseString + ", ifdef_level=" + ifdef_level + ", ifdef_vector=" + ifdef_vector + ", insideFalseIfdef=" + insideFalseIfdef + ", defines=" + defines + ", lastIfdefFalse=" + lastIfdefFalse);
                    }
                    if (parseString.startsWith("#ifdef")) {
                        String varname = parseString.substring(6).trim();
                        lastIfdefFalse = true;
                        if (defines.indexOf(":" + varname + ":") > 0
                                || defines.indexOf(":" + varname + "=") > 0) {
                            lastIfdefFalse = false;
                        } else if (diagapplet.CodeGen.StringFuncs.getenv(varname) != null) {
                            lastIfdefFalse = false;
                        }
                        if (ifdef_vector.size() < ifdef_level + 1) {
                            ifdef_vector.setSize(ifdef_level + 1);
                        }
                        ifdef_vector.setElementAt(Boolean.valueOf(lastIfdefFalse), ifdef_level);
                        ifdef_level++;
                    } else if (parseString.startsWith("#ifndef")) {
                        String varname = parseString.substring(7).trim();
                        lastIfdefFalse = false;
                        if (defines.indexOf(":" + varname + ":") > 0
                                || defines.indexOf(":" + varname + "=") > 0) {
                            lastIfdefFalse = true;
                        } else if (diagapplet.CodeGen.StringFuncs.getenv(varname) != null) {
                            lastIfdefFalse = true;
                        }
                        if (ifdef_vector.size() < ifdef_level + 1) {
                            ifdef_vector.setSize(ifdef_level + 1);
                        }
                        ifdef_vector.setElementAt(Boolean.valueOf(lastIfdefFalse), ifdef_level);
                        ifdef_level++;
                    } else if (parseString.startsWith("#define")) {
                        if (insideFalseIfdef) {
                            if (debug_on) {
                                DebugPrint2("Skipping line " + lines_read + " (insideFalseIfdef==true) -- " + parseString);
                            }
                            continue;
                        }
                        String varname = parseString.substring(7).trim();
                        String value = "";
                        if (varname.indexOf(' ') > 0) {
                            value = varname.substring(varname.indexOf(' ')).trim();
                            varname = varname.substring(0, varname.indexOf(' ')).trim();
                        }
                        if (value.length() < 1) {
                            defines += " :" + varname + ": ";
                        } else {
                            defines += " :" + varname + "=" + value + ": ";
                        }
                    } else if (parseString.startsWith("#include")) {
                        if (insideFalseIfdef) {
                            if (debug_on) {
                                DebugPrint2("Skipping line " + lines_read + " (insideFalseIfdef==true) -- " + parseString);
                            }
                            continue;
                        }
                        String includeFile = parseString.substring(8).trim();
                        if (includeFile.startsWith("\"") && includeFile.endsWith("\"")) {
                            int len = includeFile.length();
                            if (len > 3) {
                                includeFile = includeFile.substring(1, len - 1);
                            }
                        } else if (includeFile.startsWith("<") && includeFile.endsWith(">")) {
                            int len = includeFile.length();
                            if (len > 3) {
                                includeFile = includeFile.substring(1, len - 1);
                            }
                        }
                        LoadHierarchyInternal(includeFile);
                    } else if (parseString.startsWith("#else")) {
                        Boolean lastIfdefFalseBoolean = (Boolean) ifdef_vector.get(ifdef_level - 1);
                        lastIfdefFalse = lastIfdefFalseBoolean.booleanValue();
                        lastIfdefFalse = !lastIfdefFalse;
                        lastIfdefFalseBoolean = Boolean.valueOf(lastIfdefFalse);
                        ifdef_vector.setElementAt(lastIfdefFalseBoolean, ifdef_level - 1);
                        insideFalseIfdef = lastIfdefFalse;
                    } else if (parseString.startsWith("#endif")) {
                        ifdef_level--;
                        ifdef_vector.setSize(ifdef_level);
                    }
                    insideFalseIfdef = false;
                    for (int i = 0; i < ifdef_vector.size() && !insideFalseIfdef && i < ifdef_level; i++) {
                        if (((Boolean) ifdef_vector.get(i)).booleanValue()) {
                            insideFalseIfdef = true;
                            break;
                        }
                    }
                    if (debug_on) {
                        DebugPrint2("parseString=" + parseString + ", ifdef_level=" + ifdef_level + ", ifdef_vector=" + ifdef_vector + ", insideFalseIfdef=" + insideFalseIfdef + ", defines=" + defines + ", lastIfdefFalse=" + lastIfdefFalse);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                continue;
            }
            if (parseString.regionMatches(false, 0, "\n", 0, 1)) {
                continue;
            }
            if (parseString.regionMatches(false, 0, "\r", 0, 1)) {
                continue;
            }
            if (insideFalseIfdef) {
                if (debug_on) {
                    DebugPrint2("Skipping line " + lines_read + " (insideFalseIfdef==true) -- " + parseString);
                }
                continue;
            }
            indexQuoteBegin = parseString.indexOf('"');
            indexQuoteEnd = parseString.lastIndexOf('"');
            indexCCommentBegin = parseString.indexOf("/*");
            if (indexCCommentBegin < indexQuoteEnd
                    && indexCCommentBegin > indexQuoteBegin) {
                indexCCommentBegin = -1;
            }
            while (indexCCommentBegin != -1 || insideComment) {
                String tempString;
                indexCCommentEnd = parseString.indexOf("*/");
                indexQuoteBegin = parseString.indexOf('"');
                indexQuoteEnd = parseString.lastIndexOf('"');
                if (indexCCommentEnd < indexQuoteEnd
                        && indexCCommentEnd > indexQuoteBegin) {
                    indexCCommentEnd = -1;
                }
                if (indexCCommentEnd != -1) {
                    if (indexCCommentBegin > 0) {
                        tempString = parseString.substring(0, indexCCommentBegin);
                    } else {
                        tempString = "";
                    }
                    parseString = tempString + parseString.substring(indexCCommentEnd + 2);
                    insideComment = false;
                } else {
                    if (indexCCommentBegin > 0 && !insideComment) {
                        parseString = parseString.substring(0, indexCCommentBegin);
                        insideComment = true;
                    } else {
                        parseString = "";
                        insideComment = true;
                        break;
                    }
                }
                indexQuoteBegin = parseString.indexOf('"');
                indexQuoteEnd = parseString.lastIndexOf('"');
                indexCCommentBegin = parseString.indexOf("/*");
                if (indexCCommentBegin < indexQuoteEnd
                        && indexCCommentBegin > indexQuoteBegin) {
                    indexCCommentBegin = -1;
                }
            }
            if (parseString.length() < 1) {
                continue;
            }
            indexQuoteBegin = parseString.indexOf('"');
            indexQuoteEnd = parseString.lastIndexOf('"');
            indexCppCommentBegin = parseString.indexOf("//");
            if (indexCppCommentBegin < indexQuoteEnd
                    && indexCppCommentBegin > indexQuoteBegin) {
                indexCppCommentBegin = -1;
            }
            if (indexCppCommentBegin != -1) {
                if (indexCppCommentBegin == 0) {
                    continue;
                }
                parseString = parseString.substring(0, indexCppCommentBegin);
            }
            if (insideModule && currentModule != null) {
                if (parseString.regionMatches(false, 0, "}", 0, 1)) {
                    insideModule = false;
                    if (debug_on) {
                        DebugPrint("Found module = " + currentModule.Name);
                    }
                    if (currentModule.Name.equalsIgnoreCase("options")) {
                        if (!ignore_options) {
                            optionsInfo = currentModule.Info;
                            ParseOptionsInfo();
                            GetParameters(orig_args);
                            String p = GetParameter("no_errlog", orig_args);
                            if (p != null) {
                                ModuleInfo.no_errlog = Boolean.valueOf(p).booleanValue();
                            }
                        }
                    } else if (currentModule.Info.toLowerCase().indexOf("is_server=true;") >= 0
                            || currentModule.Info.toLowerCase().indexOf("is_server=1;") >= 0) {
                        if (!preserve_modules_hashtable) {
                            if (null == serversHashtable) {
                                serversHashtable = new Hashtable();
                            }
                            ServerInfo si = (ServerInfo) serversHashtable.get(currentModule.Name);
                            if (null != si) {
                                currentModule.Info += si.Info;
                                serversHashtable.remove(currentModule.Name);
                            }
                            ServerInfo serverInfo = new ServerInfo(currentModule.Name, currentModule.Info);
                            serversHashtable.put(serverInfo.Name, serverInfo);
                            if (null != serversList) {
                                serversList.add(serverInfo.Name);
                            }
                        }
                    } else {
                        if (!m_modulesHashTable.containsKey(currentModule.Name)) {
                            if (!preserve_modules_hashtable) {
                                m_modulesHashTable.put((Object) currentModule.Name, (Object) currentModule);
                            }
                            currentModule.HTTPport = HTTPport;
                            currentModule.setHost(m_systemHost);
                            currentModule.m_loadingPanel = m_loadingPanel;
                            ModuleInfo.previous_url_loaded = m_hierarchyFile;
                            ModuleInfo.curFileName = m_hierarchyFile;
                            ModuleInfo.curFileLineNumber = info_start_line;
                            currentModule.LoadInfo();
                            if (!currentModule.no_stat || !currentModule.no_cmd) {
                                if (null != m_modulesList && !preserve_modules_hashtable) {
                                    m_modulesList.add(currentModule.Name);
                                }

                                if (null != m_modulesCountList && !preserve_modules_hashtable) {
                                    m_modulesCountList.add(currentModule.Name);
                                }
                            }
                            if (null != m_loadingPanel) {
                                m_loadingPanel.set_content_length(content_length);
                                m_loadingPanel.set_bytes_read(bytes_read);
                                m_loadingPanel.set_URLname(m_hierarchyFile);
                                m_loadingPanel.force_repaint(500);
                            }
                        }
                    }
                    // currentModule = new ModuleInfo(new DiagNMLMsgDictCreator());
                    currentModule = new ModuleInfo(diag_dict_creator, nml_creator);
                    if (null == m_currentModule) {
                        m_currentModule = currentModule;
                    }
                    continue;
                }
                currentModule.Info += parseString;
                continue;
            }

            int index = parseString.indexOf("{");
            if (index <= 0) {
                continue;
            }
            currentModule.Name = (parseString.substring(0, index));
            if (null != lhui) {
                lhui.update_main(m_hierarchyFile + " : " + currentModule.Name, lines_read, total_lines + 3);
            }
            int parseStringLength = parseString.length();
            if (index + 1 < parseStringLength) {
                currentModule.Info = parseString.substring(index + 1);
            }
            insideModule = true;
            info_start_line = lines_read;
        }
    }

    public void LoadHierarchy() throws Exception {

        insideFalseIfdef = false;
        lastIfdefFalse = false;
        ifdef_level = 0;
        ifdef_vector = new Vector();
        content_length = 0;
        total_lines = 0;
        lines_read = 0;
        insideComment = false;
        insideModule = false;
        currentModule = null;
        defines = "";
        starting_load_button_count = 0;

        reload_hierarchy_needed = false;
        is_loading_hierarchy = true;
        URL_and_FileLoader loader = null;
        GetParametersFirstTime = false;
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.LoadHierarchy() called. m_ConfigFile=" + m_ConfigFile + ", m_hierarchyFile=" + m_hierarchyFile);
            }
            ModuleInfo.m_loadedPreDefinedTypes.clear();
            generate_java_classes_needed = false;
            generate_java_dictionary_needed = false;
            generate_cpp_update_functions_needed = false;
            generate_cpp_format_function_needed = false;
            generate_cpp_constructors_needed = false;

            if (null != ModuleInfo.startingDefinedValues) {
                try {
                    Enumeration definesEnumeration = ModuleInfo.startingDefinedValues.keys();
                    while (definesEnumeration.hasMoreElements()) {
                        String def = (String) definesEnumeration.nextElement();
                        if (def.endsWith("_H") || def.endsWith("_HH") || def.endsWith("_HPP")) {
                            ModuleInfo.startingDefinedValues.remove(def);
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            if (null == m_modulesHashTable) {
                m_modulesHashTable = new Hashtable();
                m_currentModule = null;
            }
            if (null == m_hierarchyFile) {
                m_hierarchyFile = m_ConfigFile;
            }
            if (null == m_hierarchyFile) {
                return;
            }
            if (m_hierarchyFile.length() < 1) {
                return;
            }
            if (debug_on) {
                System.out.print("\r\nLoading Hierarchy (" + m_hierarchyFile + "). . .\r\n");
            }
            if (!preserve_modules_hashtable) {
                m_modulesHashTable.clear();
                if (null != serversHashtable) {
                    serversHashtable.clear();
                }
            }
            m_currentModule = null;
            // currentModule = new ModuleInfo( new DiagNMLMsgDictCreator());
            currentModule = new ModuleInfo(diag_dict_creator, nml_creator);
            if (null == m_currentModule) {
                m_currentModule = currentModule;
            }
            is_loading_hierarchy = true;
            ModuleInfo.interrupt_loading = false;
            interrupt_loading = false;
            m_currentModule = null;
            if (null != m_hierarchyFileLoadButton) {
                starting_load_button_count = m_hierarchyFileLoadButton.get_count();
                m_hierarchyFileLoadButton.setLabel("STOP");
            }

            if (!m_hierarchyFile.endsWith(".h") && !m_hierarchyFile.endsWith(".hh") && !m_hierarchyFile.endsWith(".hpp") && !m_hierarchyFile.endsWith(".hxx")) {
                if (m_hierarchyLoadedOnce) {
                    ModuleInfo.ClearStaticData();
                }
                m_hierarchyLoadedOnce = true;
                LoadHierarchyInternal(m_hierarchyFile);
                if (null != m_modulesList && !preserve_modules_hashtable) {
                    if (m_modulesList.getItemCount() > 0) {
                        m_modulesList.select(0);
                    }
                }
                if (null != m_modulesCountList && !preserve_modules_hashtable) {
                    if (m_modulesCountList.getItemCount() > 0) {
                        m_modulesCountList.select(0);
                    }
                }
                if (null != m_loadingPanel) {
                    m_loadingPanel.set_URLname(m_hierarchyFile);
                    m_loadingPanel.set_content_length(content_length);
                }
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                if (null != m_hierarchyFileLoadButton) {
                    if (starting_load_button_count == m_hierarchyFileLoadButton.get_count() && !interrupt_loading) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_content_length());
                    }
                }
                if (null != m_loadingPanel) {
                    m_loadingPanel.force_repaint(500);
                }
            } else {
                currentModule = new ModuleInfo(diag_dict_creator, nml_creator);

                // currentModule = new ModuleInfo(new DiagNMLMsgDictCreator());
                if (debug_on) {
                    DebugPrint(m_hierarchyFile + " assumed to be C++ header file.");
                }
                if (null == m_currentModule) {
                    m_currentModule = currentModule;
                }
                currentModule.created_from_header = true;
                currentModule.Name = m_hierarchyFile;
                int last_period_index = currentModule.Name.lastIndexOf('.');
                if (last_period_index > 0) {
                    currentModule.Name = currentModule.Name.substring(0, last_period_index);
                }
                int last_fslash_index = currentModule.Name.lastIndexOf('/');
                int last_bslash_index = currentModule.Name.lastIndexOf('\\');
                int last_slash_index = 0;
                if (last_fslash_index > last_bslash_index) {
                    last_slash_index = last_fslash_index;
                } else {
                    last_slash_index = last_fslash_index;
                }
                if (last_slash_index > 0) {
                    currentModule.Name = currentModule.Name.substring(last_slash_index + 1);
                }
                currentModule.Info = "cmd_types=\"" + m_hierarchyFile + "\");\nstat_types=\"" + m_hierarchyFile + "\";\n";
                currentModule.HTTPport = HTTPport;
                currentModule.setHost(m_systemHost);
                currentModule.m_loadingPanel = m_loadingPanel;
                ModuleInfo.previous_url_loaded = m_hierarchyFile;
                if (force_reload_file && m_modulesHashTable.contains(currentModule.Name) && !preserve_modules_hashtable) {
                    m_modulesHashTable.remove(currentModule.Name);
                }
                if (!m_modulesHashTable.contains(currentModule.Name) || force_reload_file) {
                    if (!preserve_modules_hashtable) {
                        m_modulesHashTable.put((Object) currentModule.Name, (Object) currentModule);
                    }
                    currentModule.LoadInfo();
                    if (!currentModule.no_cmd || !currentModule.no_stat) {
                        if (null != m_modulesList && !preserve_modules_hashtable) {
                            m_modulesList.add(currentModule.Name);
                        }
                        if (null != m_modulesCountList && !preserve_modules_hashtable) {
                            m_modulesCountList.add(currentModule.Name);
                        }
                    }
                }
            }
            is_loading_hierarchy = false;
            if (Thread.interrupted()) {
                interrupt_loading = true;
                throw new Exception("Thread.interrupted() returned true\n");
            }
            if (interrupt_loading) {
                throw new Exception("interrupt_loading=true\n");
            }
            if (null != lhui) {
                lhui.update_main(m_hierarchyFile + " : InitializeClassList()", 1, 3);
            }
            InitializeClassList();
            generate_java_classes_needed = false;
            generate_java_dictionary_needed = false;
            generate_cpp_update_functions_needed = false;
            generate_cpp_format_function_needed = false;
            generate_cpp_constructors_needed = false;
            force_reload_file = false;
            if (null != lhui) {
                lhui.update_main(m_hierarchyFile + " : Finishing ...", total_lines + 2, total_lines + 3);
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (null != loader) {
                loader.close();
                loader = null;
            }
        }
        try {
            if (null != m_hierarchyFileLoadButton) {
                m_hierarchyFileLoadButton.setLabel("LOAD");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (lhui != null) {
            lhui.done();
        }

        insideFalseIfdef = false;
        lastIfdefFalse = false;
        ifdef_level = 0;
        ifdef_vector = null;
        content_length = 0;
        defines = "";
// 	System.out.println("Finished CodeGenCommon.LoadHierarchy()");
// 	System.out.println("Free Memory :\t"+Runtime.getRuntime().freeMemory() +"\t:\t"+ (Runtime.getRuntime().freeMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().freeMemory()/1048576)+"M");
// 	System.out.println("Total Memory :\t"+Runtime.getRuntime().totalMemory() +"\t:\t"+ (Runtime.getRuntime().totalMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().totalMemory()/1048576)+"M");
// 	System.out.println("Max Memory :\t"+Runtime.getRuntime().maxMemory() +"\t:\t"+ (Runtime.getRuntime().maxMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().maxMemory()/1048576)+"M");
// 	long mem_used = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
// 	System.out.println("Used Memory :\t"+mem_used +"\t:\t"+ (mem_used/1024)+"k\t:\t"+ (mem_used/1048576)+"M");
//	lhui=null;
//	ModuleInfo.lhui = null;
    }

    public String remove_leading_whitespace(String str) {
        return remove_leading_whitespace_static(str);
    }

    static public String remove_leading_whitespace_static(String str) {
        try {
            if (null == str) {
                return null;
            }
            while (true) {
                if (str.length() < 1) {
                    break;
                }
                if (str.charAt(0) != ' '
                        && str.charAt(0) != '\t') {
                    break;
                }
                str = str.substring(1);
            }
            if (debug_on) {
                DebugPrint("remove_leading_whitespace_static returning " + str);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return str;
    }
    // @SuppressWarnings("unchecked")

    public void MakeListGeneric(FastListPanelInterface lst) {
        try {
            if (lst == null) {
                throw new Exception("CodeGenCommon.MakeListGeneric() called with lst == null.");
            }
            for (int i = 0; i < lst.countItems(); i++) {
                String temp = lst.getItem(i);
                if (!is_generic(temp) && null != generic_classes) {
                    generic_classes.addElement(temp);
                }
                StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(temp);
                if (null != sti) {
                    sti.selected = false;
                    sti.generic = true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    URL_and_FileLoader script_loader = null;
    boolean ignore_exit = false;
    boolean read_from_stdin = false;
    static boolean print_prompt = true;

    @Override
    public void RunScript(String new_script) {
        try {
            if (debug_on) {
                DebugPrint("CodeGenCommon.RunScriptFile(" + new_script + ") called.");
            }
            cannot_write_to_output_file = false;
            script_loader = null;
            script = new_script;
            ignore_exit = true;
            currentOutputFileName = null;
            if (null != fos) {
                fos.close();
                fos = null;
            }
            try {
                ModuleInfo.ClearStaticData();
            } catch (Exception e) {
                e.printStackTrace();
            }
            ResetGenericClasses();
            InitializeClassList();
            script_file_ran = false;
            while (!script_file_ran && !cannot_write_to_output_file) {
                RunScript();
            }
            if (null != fos) {
                fos.close();
                fos = null;
            }
            currentOutputFileName = null;

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    private static CodeGenCommon cgc_for_run_line_of_script = null;

    public static void RunLineOfScriptStatic(String line_of_script) throws Exception {
        try {
            if (null != cgc_for_run_line_of_script) {
                cgc_for_run_line_of_script.RunLineOfScript(line_of_script);
            }
        } catch (Exception exception) {
            System.err.println("line_of_script=" + line_of_script);
            exception.printStackTrace();
        }
    }
    private Vector prev_lines_of_script = null;

    public void RunLineOfScript(String line_of_script) throws Exception {
        try {
            if (null == prev_lines_of_script) {
                prev_lines_of_script = new Vector();
            }
            prev_lines_of_script.add(line_of_script);
            if (line_of_script.length() < 1) {
                return;
            }
            if (cgc_for_run_line_of_script == null) {
                return;
            }
            line_of_script = remove_leading_whitespace_static(line_of_script);
            if (line_of_script.length() < 1) {
                return;
            }
            if (line_of_script.startsWith("#")) {
                return;
            }
            if (line_of_script.startsWith("debug")) {
                int eq_index = line_of_script.lastIndexOf('=');
                if (eq_index > 0) {
                    line_of_script = line_of_script.substring(eq_index + 1);
                } else {
                    line_of_script = line_of_script.substring(5);
                }
                line_of_script = remove_leading_whitespace_static(line_of_script);
                if (line_of_script.startsWith("true")
                        || line_of_script.startsWith("on")) {
                    debug_on = true;
                    ModuleInfo.debug_on = true;
                    C_Generator.debug_on = debug_on;
                    Ada_Generator.debug_on = debug_on;
//                    if (null != DebugCheckbox) {
//                        DebugCheckbox.setState(true);
//                    }
                } else if (line_of_script.startsWith("false")
                        || line_of_script.startsWith("off")) {
                    debug_on = false;
                    ModuleInfo.debug_on = false;
                    C_Generator.debug_on = debug_on;
                    Ada_Generator.debug_on = debug_on;
//                    if (null != DebugCheckbox) {
//                        DebugCheckbox.setState(false);
//                    }
                } else {
                    ErrorPrint("Bad value for debug_on= " + line_of_script);
                }
                return;
            }

            if (line_of_script.startsWith("generate_symbol_lookups")) {
                int eq_index = line_of_script.lastIndexOf('=');
                if (eq_index > 0) {
                    line_of_script = line_of_script.substring(eq_index + 1);
                } else {
                    line_of_script = line_of_script.substring(23);
                }
                line_of_script = remove_leading_whitespace_static(line_of_script);
                if (line_of_script.startsWith("true")
                        || line_of_script.startsWith("on")) {
                    this.generate_symbol_lookups = true;
                } else if (line_of_script.startsWith("false")
                        || line_of_script.startsWith("off")) {
                    this.generate_symbol_lookups = false;
                } else {
                    ErrorPrint("Bad value for generate_symbol_lookups= " + line_of_script);
                }
                return;
            }

            if (line_of_script.startsWith("generate_all_enum_symbol_lookups")) {
                int eq_index = line_of_script.lastIndexOf('=');
                if (eq_index > 0) {
                    line_of_script = line_of_script.substring(eq_index + 1);
                } else {
                    line_of_script = line_of_script.substring(23);
                }
                line_of_script = remove_leading_whitespace_static(line_of_script);
                if (line_of_script.startsWith("true")
                        || line_of_script.startsWith("on")) {
                    set_generate_all_enum_symbol_lookups(true);
                } else if (line_of_script.startsWith("false")
                        || line_of_script.startsWith("off")) {
                    set_generate_all_enum_symbol_lookups(false);
                } else {
                    ErrorPrint("Bad value for generate_all_enum_symbol_lookups= " + line_of_script);
                }
                return;
            }

            if (line_of_script.startsWith("add_set_header")) {
                int eq_index = line_of_script.lastIndexOf('=');
                if (eq_index > 0) {
                    line_of_script = line_of_script.substring(eq_index + 1);
                } else {
                    line_of_script = line_of_script.substring(14);
                }
                line_of_script = remove_leading_whitespace_static(line_of_script);
                if (line_of_script.startsWith("true")
                        || line_of_script.startsWith("on")) {
                    set_add_set_header(true);
                } else if (line_of_script.startsWith("false")
                        || line_of_script.startsWith("off")) {
                    set_add_set_header(false);
                } else {
                    ErrorPrint("Bad value for generate_all_enum_symbol_lookups= " + line_of_script);
                }
                return;
            }

            if (line_of_script.startsWith("add_include_dir")) {
                line_of_script = line_of_script.substring(15);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                URL_and_FileLoader.AddToSearchPath(line_of_script);
                includePath += line_of_script + ");";
                if (debug_on) {
                    DebugPrint("includePath=" + includePath);
                }
                if (null != includePathField) {
                    includePathField.setText(includePath);
                }
                return;
            }
            if (line_of_script.startsWith("add_extra_header")) {
                line_of_script = line_of_script.substring(16);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                ModuleInfo.AddExtraHeader(line_of_script);
                if (debug_on) {
                    DebugPrint("AddExtraHeader" + line_of_script);
                }
                return;
            }
            if (line_of_script.startsWith("load")) {
                line_of_script = line_of_script.substring(4);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                m_ConfigFile = line_of_script;
                m_hierarchyFile = line_of_script;
                if (null != configFileTextField) {
                    configFileTextField.setText(line_of_script);
                }
                interrupt_loading = false;
                ModuleInfo.interrupt_loading = false;
                force_reload_file = true;
                if (null == m_currentModule) {
                    LoadHierarchy();
                } else {
                    m_currentModule.Name = m_hierarchyFile;
                    int last_period_index = currentModule.Name.lastIndexOf('.');
                    if (last_period_index > 0) {
                        m_currentModule.Name = currentModule.Name.substring(0, last_period_index);
                    }
                    int last_fslash_index = currentModule.Name.lastIndexOf('/');
                    int last_bslash_index = currentModule.Name.lastIndexOf('\\');
                    int last_slash_index = 0;
                    if (last_fslash_index > last_bslash_index) {
                        last_slash_index = last_fslash_index;
                    } else {
                        last_slash_index = last_fslash_index;
                    }
                    if (last_slash_index > 0) {
                        m_currentModule.Name = currentModule.Name.substring(last_slash_index + 1);
                    }
                    m_currentModule.LoadPredefinedTypeFile(m_ConfigFile);
                }
                return;
            }
            if (line_of_script.startsWith("select_from_file")) {
                line_of_script = line_of_script.substring(16);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                SelectByFromFile(line_of_script);
                interrupt_loading = false;
                ModuleInfo.interrupt_loading = false;
                force_reload_file = true;
                return;
            }
            if (line_of_script.startsWith("select_from_all")) {
                SelectFromAllFiles();
                interrupt_loading = false;
                ModuleInfo.interrupt_loading = false;
                force_reload_file = true;
                return;
            }
            if (line_of_script.startsWith("cd")) {
                line_of_script = line_of_script.substring(2);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                current_directory = new File(line_of_script);
                interrupt_loading = false;
                ModuleInfo.interrupt_loading = false;
                force_reload_file = true;
                return;
            }
            if (line_of_script.startsWith("set_format_function")) {
                line_of_script = line_of_script.substring(19);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                format_function_name = line_of_script;
                if (debug_on) {
                    DebugPrint("format_function_name=" + format_function_name);
                }
                if (format_function_name.equalsIgnoreCase("null")) {
                    format_function_name = null;
                }
                return;
            }
            if (line_of_script.startsWith("package")) {
                line_of_script = line_of_script.substring(7);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                java_package_name = line_of_script;
                if (debug_on) {
                    DebugPrint("java_package_name=" + java_package_name);
                }
                return;
            }
            if (line_of_script.indexOf("generate_symbol_lookups=true") >= 0) {
                CodeGenCommon.set_generate_symbol_lookups(true);
                return;
            } else if (line_of_script.indexOf("generate_symbol_lookups=false") >= 0) {
                CodeGenCommon.set_generate_symbol_lookups(false);
                return;
            }
            if (line_of_script.indexOf("generate_all_enum_symbol_lookups=true") >= 0) {
                CodeGenCommon.set_generate_all_enum_symbol_lookups(true);
                return;
            } else if (line_of_script.indexOf("generate_all_enum_symbol_lookups=false") >= 0) {
                CodeGenCommon.set_generate_all_enum_symbol_lookups(false);
                return;
            }

            if (line_of_script.indexOf("add_set_header=true") >= 0) {
                CodeGenCommon.set_add_set_header(true);
                return;
            } else if (line_of_script.indexOf("add_set_header=false") >= 0) {
                CodeGenCommon.set_add_set_header(false);
                return;
            }
            if (line_of_script.indexOf("generate_enum_symbol_lookup=true") >= 0) {
                ModuleInfo.generate_enum_symbol_lookup = true;
                return;
            } else if (line_of_script.indexOf("generate_enum_symbol_lookup=false") >= 0) {
                ModuleInfo.generate_enum_symbol_lookup = false;
                return;
            }
            if (line_of_script.startsWith("generate")) {
                output_file_name = null;
                line_of_script = line_of_script.substring(8);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                int lt_index = line_of_script.indexOf('>');
                if (lt_index > 0) {
                    output_file_name = line_of_script.substring(lt_index + 1);
                    if (null != output_file_name
                            && (line_of_script.indexOf("java") == -1
                            || line_of_script.indexOf("dict") == -1
                            || java_package_name == null
                            || java_package_name.length() < 1)) {
                        if (output_file_name.length() > 1 && output_file_name.indexOf('*') == -1 && output_file_name.indexOf('?') == -1) {
                            SetOutputFile(output_file_name);
                        }
                    }
                    line_of_script = line_of_script.substring(0, lt_index + 1);
                }
                if (line_of_script.toLowerCase().startsWith("java") || line_of_script.startsWith("Java") || line_of_script.startsWith("JAVA")) {
                    line_of_script = line_of_script.substring(4);
                    line_of_script = remove_leading_whitespace_static(line_of_script);
                    if (line_of_script.startsWith("classes")) {
                        GenerateJavaClasses();
                        return;
                    }
                    if (line_of_script.startsWith("dict")) {
                        GenerateJavaMessageDict();
                        return;
                    }
                    ErrorPrint("CodeGen: Generate type not recognized. " + line_of_script);
                } else if (line_of_script.startsWith("C++") || line_of_script.startsWith("c++")) {
                    line_of_script = line_of_script.substring(3);
                    line_of_script = remove_leading_whitespace_static(line_of_script);
                    if (line_of_script.startsWith("update")) {
                        GenerateCppUpdateFunctions();
                        return;
                    } else if (line_of_script.startsWith("format")) {
                        GenerateCppFormatFunction();
                        return;
                    } else if (line_of_script.startsWith("protos")) {
                        GenerateCppPrototypesHeader();
                        return;
                    } else if (line_of_script.startsWith("enum_info")) {
                        GenerateCppEnumInfo(null);
                        return;
                    } else if (line_of_script.startsWith("print_sizes")) {
                        GeneratePrintSizesFile();
                        return;
                    } else if (line_of_script.startsWith("constructor")) {
                        GenerateCppConstructors();
                        return;
                    }
                    ErrorPrint("CodeGen: Generate type not recognized. " + line_of_script);
                } else if (line_of_script.startsWith("C") || line_of_script.startsWith("c")) {
                    line_of_script = line_of_script.substring(1);
                    line_of_script = remove_leading_whitespace_static(line_of_script);
                    if (line_of_script.startsWith("protos")) {
                        GenerateC_PrototypesHeader();
                        return;
                    } else if (line_of_script.startsWith("format")) {
                        GenerateC_FormatFunction();
                        return;
                    } else if (line_of_script.startsWith("update")) {
                        GenerateC_UpdateFunctions();
                        return;
                    }
                } else if (line_of_script.startsWith("Ada") || line_of_script.startsWith("ada")) {
                    line_of_script = line_of_script.substring(3);
                    line_of_script = remove_leading_whitespace_static(line_of_script);
                    if (line_of_script.startsWith("spec")) {
                        GenerateAdaSpec();
                        return;
                    } else if (line_of_script.startsWith("body")) {
                        GenerateAdaBody();
                        return;
                    }
                }
                ErrorPrint("Language Type (" + line_of_script + ") not recognized.");
            } else if (line_of_script.startsWith("ignore")) {
                line_of_script = line_of_script.substring(6);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                if (line_of_script.length() > 0) {
                    if (null == ModuleInfo.ignored_header_list) {
                        ModuleInfo.ignored_header_list = new Vector();
                    }
                    ModuleInfo.ignored_header_list.addElement(line_of_script);
                }
                return;
            } else if (line_of_script.startsWith("print_struct_info")) {
                line_of_script = line_of_script.substring(17);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                try {
                    System.out.println("*** start print_struct_info " + line_of_script);
                    if (line_of_script.length() > 0) {
                        StructureTypeInfo type_info = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(line_of_script);
                        type_info.estimated_size = 0;
                        print_est_size_info = true;
                        System.out.println("");
                        EstimateSize(type_info);
                        System.out.println("");
                        print_est_size_info = false;
                        System.out.println("type_info=" + type_info);
                        STI_TokenizerInterface infoTokenizer = type_info.getInfoTokenizer();
                        System.out.println("type_info.getInfoTokenizer()=" + type_info.getInfoTokenizer());
                        int token_number = 0;
                        while (infoTokenizer.hasMoreTokens()) {
                            String token = infoTokenizer.nextToken();
                            System.out.println("\t" + token_number + "\t" + token);
                            token_number++;
                        }
                        infoTokenizer = type_info.getInfoTokenizer();
                        System.out.println("");
                        while (infoTokenizer.hasMoreTokens()) {
                            String token = infoTokenizer.nextToken();
                            System.out.print(" " + token + ";");
                        }
                        System.out.println("");
                    }
                    System.out.println("*** end print_struct_info " + line_of_script);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return;
            } else if (line_of_script.startsWith("no_enums")) {
                no_enums = true;
                return;
            } else if (line_of_script.startsWith("no_swig")) {
                no_swig = true;
                return;
            } else if (line_of_script.startsWith("no_format")) {
                no_format = true;
                return;
            }
            if (line_of_script.startsWith("exit")) {
                if (!ignore_exit) {
                    System.exit(0);
                }
                return;
            }
            if (line_of_script.startsWith("make_generic")) {
                MakeListGeneric(ClassList);
                return;
            }
            if (line_of_script.startsWith("init_list")) {
                InitializeClassList();
                return;
            }
            if (line_of_script.startsWith("clear")) {
                ClearAll();
                return;
            }
            if (line_of_script.startsWith("force_final_pass")) {
                ModuleInfo.always_perform_final_pass = true;
                return;
            }
            if (line_of_script.startsWith("disable_final_pass")) {
                ModuleInfo.always_perform_final_pass = false;
                return;
            }
            if (line_of_script.startsWith("prompt")) {
                if (read_from_stdin) {
                    print_prompt = true;
                }
                return;
            }
            if (line_of_script.startsWith("noprompt")) {
                print_prompt = false;
                return;
            }
            if (line_of_script.startsWith("print_info")) {
                line_of_script = line_of_script.substring(9);
                line_of_script = remove_leading_whitespace_static(line_of_script);
                PrintInfo(line_of_script);
                return;
            }
            if (line_of_script.indexOf("update_with_name=true") >= 0) {
                update_with_name = true;
                return;
            } else if (line_of_script.indexOf("update_with_name=false") >= 0) {
                update_with_name = false;
                return;
            }
            ErrorPrint("CodeGen script line not recognized.:  " + line_of_script);
        } catch (Exception exception) {
            System.err.println("line_of_script=" + line_of_script);
            exception.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")
    private String lines_of_script[] = null;
    private int current_script_line = 0;

    public void RunScript() throws Exception {
        if (lines_of_script == null) {
            lines_of_script = script.split("[\r\n]+");
//            System.out.println("lines_of_script.length = " + lines_of_script.length);
//            System.out.println("lines_of_script = " + lines_of_script);
            current_script_line = 0;
            if (null != m_hierarchyFileLoadButton) {
                m_hierarchyFileLoadButton.setLabel("STOP");
            }
        } else {
            current_script_line++;
            if (current_script_line >= lines_of_script.length) {
                script_file_ran = true;
                if (null != m_hierarchyFileLoadButton) {
                    m_hierarchyFileLoadButton.setLabel("LOAD");
                }
                return;
            }
        }
        String line_of_script = lines_of_script[current_script_line];
        if (cannot_write_to_output_file) {
            if (null != m_hierarchyFileLoadButton) {
                m_hierarchyFileLoadButton.setLabel("LOAD");
            }
            return;
        }
        cgc_for_run_line_of_script = this;
        //System.out.println("line_of_script = " + line_of_script);
        RunLineOfScript(line_of_script);
    }

    static public boolean get_static_debug_on() {
        return debug_on;
    }

    static public void set_static_debug_on(boolean dbg) {
        debug_on = dbg;
    }

    public boolean get_debug_on() {
        return debug_on;
    }

    public void set_debug_on(boolean dbg) {
        debug_on = dbg;
    }

    public String get_includePath() {
        return includePath;
    }

    public void append_includePath(String str) {
        includePath += str;
    }

    public void set_includePath(String str) {
        includePath = str;
    }

    public boolean get_print_prompt() {
        return print_prompt;
    }

    public void set_print_prompt(boolean pp) {
        print_prompt = pp;
    }

    public boolean get_display_on() {
        return display_on;
    }

    public void set_display_on(boolean dispon) {
        display_on = dispon;
    }

    public String get_script() {
        return script;
    }

    public void set_script(String scrf) {
        script = scrf;
    }

    public FastListPanelInterface get_ClassList() {
        return ClassList;
    }

    public void set_ClassList(FastListPanelInterface flp) {
        ClassList = flp;
    }

    public String get_cppFileName() {
        return cppFileName;
    }

    public void set_cppFileName(String str) {
        cppFileName = str;
    }

    public String get_javaFileName() {
        return javaFileName;
    }

    public void set_javaFileName(String str) {
        javaFileName = str;
    }

    public CodeGenTextAreaInterface get_codeTextArea() {
        return codeTextArea;
    }

    public void set_codeTextArea(CodeGenTextAreaInterface cta) {
        codeTextArea = cta;
    }

    public String get_m_hierarchyFile() {
        return m_hierarchyFile;
    }

    public void set_m_hierarchyFile(String str) {
        m_hierarchyFile = str;
        if (debug_on) {
            DebugPrint("set_m_hierarchyFile(" + str + ")");
        }
    }

    public String get_m_ConfigFile() {
        return m_ConfigFile;
    }

    public void set_m_ConfigFile(String str) {
        m_ConfigFile = str;
    }

    public CodeGenTextFieldInterface get_includePathField() {
        return includePathField;
    }

    public void set_includePathField(CodeGenTextFieldInterface cgtfi) {
        includePathField = cgtfi;
    }

    public CodeGenTextFieldInterface get_configFileTextField() {
        return configFileTextField;
    }

    public void set_configFileTextField(CodeGenTextFieldInterface cgtfi) {
        configFileTextField = cgtfi;
    }

    public boolean get_reload_hierarchy_needed() {
        return reload_hierarchy_needed;
    }

    public void set_reload_hierarchy_needed(boolean b) {
        reload_hierarchy_needed = b;
    }

    public boolean get_generate_java_classes_needed() {
        return generate_java_classes_needed;
    }

    public void set_generate_java_classes_needed(boolean b) {
        generate_java_classes_needed = b;
    }

    public boolean get_generate_java_dictionary_needed() {
        return generate_java_dictionary_needed;
    }

    public void set_generate_java_dictionary_needed(boolean b) {
        generate_java_dictionary_needed = b;
    }

    public boolean get_generate_cpp_update_functions_needed() {
        return generate_cpp_update_functions_needed;
    }

    public void set_generate_cpp_update_functions_needed(boolean b) {
        generate_cpp_update_functions_needed = b;
    }

    public boolean get_generate_cpp_format_function_needed() {
        return generate_cpp_format_function_needed;
    }

    public void set_generate_cpp_format_function_needed(boolean b) {
        generate_cpp_format_function_needed = b;
    }

    public boolean get_generate_cpp_constructors_needed() {
        return generate_cpp_constructors_needed;
    }

    public void set_generate_cpp_constructors_needed(boolean b) {
        generate_cpp_constructors_needed = b;
    }

    public boolean get_RunIndependantly() {
        return RunIndependantly;
    }

    public void set_RunIndependantly(boolean b) {
        RunIndependantly = b;
    }

    public boolean get_first_java_class() {
        return first_java_class;
    }

    public void set_first_java_class(boolean b) {
        first_java_class = b;
    }

    public boolean get_first_cpp_function() {
        return first_cpp_function;
    }

    public void set_first_cpp_function(boolean b) {
        first_java_class = b;
    }

    public boolean get_running() {
        return running;
    }

    public void set_running(boolean b) {
        running = b;
    }

    public boolean get_run_needed() {
        return run_needed;
    }

    public void set_run_needed(boolean b) {
        run_needed = b;
    }

    public boolean get_is_loading_hierarchy() {
        return is_loading_hierarchy;
    }

    public void set_is_loading_hierarchy(boolean b) {
        is_loading_hierarchy = b;
    }

    public boolean get_generating_code() {
        return generating_code;
    }

    public void set_generating_code(boolean b) {
        generating_code = b;
    }

    public boolean get_running_script() {
        return running_script;
    }

    public void set_running_script(boolean b) {
        running_script = b;
    }

    public boolean get_force_reload_file() {
        return force_reload_file;
    }

    public void set_force_reload_file(boolean b) {
        force_reload_file = b;
    }

    public java.util.Hashtable get_m_modulesHashTable() {
        return m_modulesHashTable;
    }

    public void set_m_modulesHashTable(java.util.Hashtable ht) {
        m_modulesHashTable = ht;
    }

    public java.util.Hashtable get_serversHashtable() {
        return serversHashtable;
    }

    public void set_serversHashtable(java.util.Hashtable ht) {
        serversHashtable = ht;
    }

    public diagapplet.utils.URLLoadInfoPanelInterface get_m_loadingPanel() {
        return m_loadingPanel;
    }

    public void set_m_loadingPanel(diagapplet.utils.URLLoadInfoPanelInterface lp) {
        m_loadingPanel = lp;
    }

    public boolean get_script_file_ran() {
        return script_file_ran;
    }

    public void set_script_file_ran(boolean b) {
        script_file_ran = b;
    }

    public boolean get_inside_diagapplet() {
        return inside_diagapplet;
    }

    public void set_inside_diagapplet(boolean b) {
        inside_diagapplet = b;
    }

    public diagapplet.utils.CountButtonInterface get_m_hierarchyFileLoadButton() {
        return m_hierarchyFileLoadButton;
    }

    public void set_m_hierarchyFileLoadButton(diagapplet.utils.CountButtonInterface cbi) {
        m_hierarchyFileLoadButton = cbi;
    }

    public String get_lastIncludePath() {
        return lastIncludePath;
    }

    public void set_lastIncludePath(String s) {
        lastIncludePath = s;
    }

    public boolean get_update_with_name() {
        return update_with_name;
    }

    public void set_update_with_name(boolean b) {
        update_with_name = b;
    }

    public boolean is_preserve_modules_hashtable() {
        return preserve_modules_hashtable;
    }

    public void set_preserve_modules_hashtable(boolean b) {
        preserve_modules_hashtable = b;
    }

    public boolean is_interrupt_loading() {
        return interrupt_loading;
    }

    public void set_interrupt_loading(boolean b) {
        interrupt_loading = b;
    }

    public void set_nml_creator(NMLConnectionCreatorInterface _nml_creator) {
        nml_creator = _nml_creator;
    }

    public NMLConnectionCreatorInterface get_nml_creator() {
        return nml_creator;
    }

    public void set_diag_dict_creator(DiagNMLMsgDictCreatorInterface _diag_dict_creator) {
        diag_dict_creator = _diag_dict_creator;
    }

    public DiagNMLMsgDictCreatorInterface get_diag_dict_creator() {
        return diag_dict_creator;
    }

    public diagapplet.utils.FastListPanelInterface get_m_modulesList() {
        return m_modulesList;
    }

    public void set_m_modulesList(diagapplet.utils.FastListPanelInterface lst) {
        m_modulesList = lst;
    }

    public diagapplet.utils.CountListInterface get_m_modulesCountList() {
        return m_modulesCountList;
    }

    public void set_m_modulesCountList(diagapplet.utils.CountListInterface lst) {
        m_modulesCountList = lst;
    }

    public diagapplet.utils.FastListPanelInterface get_serversList() {
        return serversList;
    }

    public void set_serversList(diagapplet.utils.FastListPanelInterface lst) {
        serversList = lst;
    }
}
