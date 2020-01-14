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
package diagapplet.CodeGen;

import java.util.Vector;
import java.util.Hashtable;
import java.util.StringTokenizer;

import rcs.nml.NMLConnectionInterface;
import rcs.nml.NMLConnectionCreatorInterface;
import diagapplet.utils.URLLoadInfoPanelInterface;
import java.util.ArrayList;
import java.util.Collections;

import java.util.Enumeration;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import rcs.nml.NMLMessageDictionary;
import rcs.utils.StackTracePrinter;
import rcs.utils.URL_and_FileLoader;

/**
 * Class contains references to all information taken from a Module section of a
 * diag or hierarchy file.
 *
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class ModuleInfo implements ModuleInfoInterface {

    /**
     * Object that can notify a progres bar etc of progress.
     */
    static public LoadHierarchyUpdateInterface lhui = null;
    private String lhui_name = null;
    private int lhui_part;
    private int lhui_total;

    private void lhui_update() {
        if (lhui != null && null != lhui_name) {
            lhui.update_internal(lhui_name, lhui_part, lhui_total);
        }
    }
    private rcs.utils.URL_and_FileLoader last_file_loader = null;

    /**
     * Get a string for debuggin purposes.
     *
     * @return last_loading_module_string;
     */
    static public String get_last_loading_module_string() {
        if (null != last_loading_module) {
            return last_loading_module.toString();
        }
        return null;
    }
    static private volatile ModuleInfo last_loading_module = null;
    /**
     * Variable temporarily set to true only by the diagnostics hierarchy view
     * when sending a message string that only contains the type and size
     * placeholder. Other variables in the message are set to zero or taken from
     * the last message.
     */
    public boolean sending_short_string = false;
    public boolean preset_x = false;
    public boolean preset_y = false;
    public boolean adding_aux_channel = false;
    static private final boolean double_buffer_nml = false;
    static public final String Id = "$Id: ModuleInfo.java 1845 2011-11-04 18:22:11Z shackle $";
    static public Vector ignored_header_list = null;

    static public rcs.nml.NMLFormatConvertErrCallbackInterface get_nml_format_err_callback() {
        return DiagNMLFormatConvertErrCallback.dnfcecb;
    }
    /**
     * Does this module have no associated command buffer?
     */
    public boolean no_cmd = false;
    /**
     * Dos this module have no associated status buffer?
     */
    public boolean no_stat = false;
    /**
     * A list of deleted commands used by the Design tool when remerging old
     * code.
     */
    public Vector deleted_commands = null;
    public String designLog = "";
    public boolean importing = false;
    public String moduleClassName = null;
    public String subsystem = null;
    public String baseClassName = null;
    public String baseModuleName = null;
    public Vector ModulesReadingAuxOutput = null;
    public Vector AuxInputNames = null;
    public Vector AuxOutputNames = null;
    public Vector AuxUpdateEveryCycleNames = null;
    public boolean update_next_aux_every_cycle = false;
    public Vector deletedAuxInputNames = null;
    public Vector deletedAuxOutputNames = null;
    public Vector externalIncludeDirectories = null;
    public Vector externalLibraries = null;
    public String releaseLibrary = null;
    public String releaseIncludeDirectory = null;
    static public Vector AllAuxChannels = null;
    public boolean positioned = false;
    public Hashtable previous_commands = new Hashtable();
    public String SourceCodeDirectory = null;
    public String cmd_name_pattern = null;
    public String cmd_name_exclude_pattern = null;
    public int current_command_variable_selected;
    public int current_command_to_send_variable_selected;
    public int current_status_variable_selected;
    public boolean new_status;
    public boolean new_status_drawn;
    public boolean new_command;
    public boolean new_command_drawn;
    public boolean line_to_sub_drawn = false;
    public long last_new_stat_time = 0;
    public long last_new_command_time = 0;
    public long last_new_echo_serial_time = 0;
    public int serial_number = 0;
    public int last_serial_number_displayed = 0;
    public int echo_serial_number = 0;
    public int last_echo_serial_number_displayed = 0;
    public String FormatFunction = null;
    public String SymbolLookup = null;
    public String cmdFormatFunction = null;
    public String statFormatFunction = null;
    public int last_selected_command_index = -1;
    public static boolean generate_enum_symbol_lookup = false;
    public Vector AuxAvailableMessageFilters = null;

    public java.util.Vector getAuxAvailableMessageFilters() {
        return AuxAvailableMessageFilters;
    }

    public void addAuxAvailableMessageFilter(String s) {
        if (null == AuxAvailableMessageFilters) {
            AuxAvailableMessageFilters = new Vector();
        }
        AuxAvailableMessageFilters.add(s);
    }

    public String getFormatFunction() {
        return FormatFunction;
    }

    public String getSymbolLookup() {
        return SymbolLookup;
    }

    static private void DebugPrint2(String s) {
        try {
            Throwable t = new Throwable();
            System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
            System.out.println("time=" + System.currentTimeMillis());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static private void DebugPrint(String s) {
        try {
            if (!debug_on) {
                return;
            }
            Throwable t = new Throwable();
            String cur_file_name = null;
            int cur_file_line = 0;
            if (last_loading_module != null) {
                cur_file_name = last_loading_module.curFileName;
                cur_file_line = last_loading_module.curFileLineNumber;
            }
            if (cur_file_name != null && cur_file_name.length() > 0 && cur_file_line > 0) {
                System.out.println(cur_file_name + ":" + cur_file_line + " " + s);
                return;
            }
            System.out.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    static private int num_error_prints = 0;

    static private void ErrorPrint(String s) {
        try {
            num_error_prints++;
            Throwable t = new Throwable();
            String cur_file_name = null;
            int cur_file_line = 0;
            if (last_loading_module != null) {
                cur_file_name = last_loading_module.curFileName;
                cur_file_line = last_loading_module.curFileLineNumber;
            }
            if (cur_file_name != null && cur_file_name.length() > 0 && cur_file_line > 0) {
                if (debug_on) {
                    System.out.println("ErrorPrint + " + cur_file_name + ":" + cur_file_line + " (" + StackTracePrinter.ThrowableToShortList(t) + ")  " + s);
                }
                diagapplet.utils.DiagError.println(cur_file_name + ":" + cur_file_line + " (" + StackTracePrinter.ThrowableToShortList(t) + ")  " + s);
                if (num_error_prints < 3) {
                    if (null != last_loading_module
                            && null != last_loading_module.last_file_loader) {
                        System.out.println("last_loading_module.last_file_loader = " + last_loading_module.last_file_loader);
                    }
                }
                return;
            }
            diagapplet.utils.DiagError.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (num_error_prints < 3) {
            if (null != last_loading_module
                    && null != last_loading_module.last_file_loader) {
                System.out.println("last_loading_module.last_file_loader = " + last_loading_module.last_file_loader);
            }
            if (debug_on) {
                Thread.dumpStack();
            }
        }
    }

    static private String cur_tag() {
        try {
            Throwable t = new Throwable();
            String cur_file_name = null;
            int cur_file_line = 0;
            if (last_loading_module != null) {
                cur_file_name = last_loading_module.curFileName;
                cur_file_line = last_loading_module.curFileLineNumber;
            }
            String cf = cur_file_name;
            if (cf == null) {
                cf = "";
            }
            String tag = "{" + cf + ":" + cur_file_line + "} [" + StackTracePrinter.ThrowableToShortList(t) + "]";
            return tag;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    public static boolean read_nml_configs = true;
    static private boolean default_types_added = false;
    public boolean created_from_header = false;

    public boolean get_created_from_header() {
        return created_from_header;
    }
    static public String DefaultNMLConfigurationFile = null;
    public String NMLConfigurationFile = null;
    private boolean errlog_on_this_host = false;
    public static boolean debug_on = false;
    public String host = null;
    public static boolean interrupt_loading = false;
    public String status_type_name;
    public long cmd_msg_type = 0;
    public long stat_msg_type = 0;
    public int max_cmd_var_number = 0;
    public int max_stat_var_number = 0;
    public Hashtable cmd_plotting_variables = null;
    public Hashtable stat_plotting_variables = null;
    public boolean cmd_has_been_plotted = false;
    public boolean stat_has_been_plotted = false;
    public static String previous_url_loaded = null;
    public int new_data_count = 0;
    public URLLoadInfoPanelInterface m_loadingPanel = null;
    public static boolean parse_error_to_send = false;
    public int HTTPport = 80;
    public String Name;

    public String getName() {
        return Name;
    }

    private void ChangeNMLConfigurationFile(NMLConnectionInterface nci, String new_config_file) {
        try {
            if (null != nci) {
                if (nci.get_buffer_name() == null) {
                    return;
                }
                nci.disconnect();
                nci.set_configuration_file(new_config_file);
                nci.ReadNMLConfigurationFileNoThrow();
                nci.connectNoThrow();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void SetNMLConfigFile(String new_config_file) {
        NMLConfigurationFile = new_config_file;
        DefaultNMLConfigurationFile = new_config_file;
        if (double_buffer_nml) {
            if (!no_cmd) {
                ChangeNMLConfigurationFile(m_cmd_write_Connection, new_config_file);
            }
            if (!no_stat) {
                ChangeNMLConfigurationFile(m_stat_write_Connection, new_config_file);
            }
        }
        if (!no_cmd) {
            ChangeNMLConfigurationFile(m_cmd_read_Connection, new_config_file);
        }
        if (!no_stat) {
            ChangeNMLConfigurationFile(m_stat_read_Connection, new_config_file);
        }
    }
    public Vector predefined_type_files = null;
    public Vector aux_type_files = null;
    public String baseClassCmdsTypeFile;
    public String baseClassStatsTypeFile;
    public String cmdsTypeFile;
    public String statsTypeFile;
    public String primaryStatusType;
    public Vector cmdsAvailable;
    public Vector statMsgsAvailable;
    public Vector cmdsAdded;
    public Vector cmdsBaseClass;
    public Vector auxMessages;
    public Vector children_names;
    public Vector baseClassChildrenNames;
    public ModuleInfo parent = null;
    public String Info = null;
    public String cmdData = null;
    public String statData = null;
    public static String errlogData = "";
    public String lastCmdName;
    public String lastStatName;
    public boolean is_connected = false;
    public int module_number;
    public static int num_modules = 0;
    public static int max_module_number = 0;
    public int generation = 0;
    public int max_children_in_generation = 0;
    public int x;
    public int y;
    public boolean new_cmd = false;
    public boolean new_stat = false;
    public static boolean new_errlog = false;
    public String parse_error_string = null;
    public NMLConnectionInterface m_cmd_read_Connection = null;
    public NMLConnectionInterface m_cmd_write_Connection = null;
    public NMLConnectionInterface m_stat_read_Connection = null;
    public NMLConnectionInterface m_stat_write_Connection = null;
    static public boolean no_errlog = false;
    static public boolean no_errlog_env_tested = false;
    static private NMLConnectionInterface m_errlogConnection = null;
    public Hashtable conflict_m_structInfoHashTable = new Hashtable();

    public Hashtable get_conflict_m_structInfoHashTable() {
        return conflict_m_structInfoHashTable;
    }
    public static Hashtable m_structInfoHashTable = new Hashtable();
    public static Hashtable m_cmd_structInfoHashTable = new Hashtable();
    public static Hashtable m_stat_structInfoHashTable = new Hashtable();
    public static Hashtable<String, StructureTypeInfo> m_structInfoByNameHashTable = new Hashtable<>();
    public static Hashtable m_enumInfoHashTable = new Hashtable();
    public static Hashtable cmdsAvailHashtable = new Hashtable();
    public static Hashtable m_loadedPreDefinedTypes = new Hashtable();
    public String currentDefaultValue = null;
    public String currentOverrideName = null;
    public String ndlaName = null;
    public String ndlaInfo = null;
    public String unboundedName = null;
    public String unboundedInfo = null;
    public String currentAttributeInfo = null;
    public String MainLoopName = null;
    public double cycle_time = 0.1;
    public static Vector headerFiles = new Vector();
    public static Vector extraHeaderFiles = null;

    public static void AddExtraHeader(String header) {
        if (null == extraHeaderFiles) {
            extraHeaderFiles = new Vector();
        }
        extraHeaderFiles.add(header);
    }
    public static boolean UseDefaultTypes = true;
    public boolean arrayLengthTooLong = false;
    public String common_cmd_base = null;
    public final static int RCS_CUSTOM_APPLICATION_TYPE = 1;
    public final static int RCS_CONTROLLER_APPLICATION_TYPE = 2;
    public final static int RCS_GUI_APPLICATION_TYPE = 3;
    public final static int RCS_DIAGNOSTICS_APPLICATION_TYPE = 4;
    public final static int RCS_DESIGN_APPLICATION_TYPE = 5;
    public final static int RCS_CODEGEN_APPLICATION_TYPE = 6;
    public static int application_type = RCS_CUSTOM_APPLICATION_TYPE;

    public String toString() {
        String str = super.toString() + " ModuleInfo { \n";
        str += "\tName=" + Name + ",\n";
        str += "\tcmdsTypeFile=" + cmdsTypeFile + ",\n";
        str += "\tstatsTypeFile=" + statsTypeFile + ",\n";
        str += "\tcmdsAvailable=" + cmdsAvailable + ",\n";
        str += "\tcur_file_name=" + curFileName + "\n";
        str += "\tcur_file_line=" + curFileLineNumber + "\n";
        str += "\tlast_file_loader=" + last_file_loader + "\n";
        str += "\tmodule_number=" + module_number + ", generation=" + generation + ", x=" + x + ", y=" + y + ",\n";
        str += "\tno_cmd=" + no_cmd + ", no_stat=" + no_stat + ",\n";
        str += "\tmoduleClassName=" + moduleClassName + ", subsystem=" + subsystem + ", \n";
        str += "\tbaseClassName=" + baseClassName + ", serial_number=" + serial_number + ", echo_serial_number=" + echo_serial_number + ", \n";
        str += "\tMainLoopName=" + MainLoopName + ", host=" + host + ", cycle_time=" + cycle_time + ", \n";
        if (null != parent) {
            str += "parent.Name = " + parent.Name + "\n";
        }
        //str += "\tauxMessages=" + auxMessages + "\n";
        str += "\tAuxInputNames=" + AuxInputNames + "\n";
        str += "\tAuxOutputNames=" + AuxOutputNames + "\n";
        /*
        if(m_structInfoHashTable != null)
        {
        str += "m_structInfoHashTable="+m_structInfoHashTable+"\n";
        }
        if(m_stat_structInfoHashTable != null)
        {
        str += "m_stat_structInfoHashTable="+m_stat_structInfoHashTable+"\n";
        }
        if(m_cmd_structInfoHashTable != null)
        {
        str += "m_cmd_structInfoHashTable="+m_cmd_structInfoHashTable+"\n";
        }
         */
        //str += "\tm_loadedPreDefinedTypes=" + m_loadedPreDefinedTypes + "\n";
        str += "} \n\n";
        return str;
    }
    DiagNMLMsgDictInterface aux_diag_msg_read_dict = null;
    DiagNMLMsgDictInterface cmd_diag_msg_read_dict = null;
    DiagNMLMsgDictInterface stat_diag_msg_read_dict = null;
    DiagNMLMsgDictInterface aux_diag_msg_write_dict = null;
    DiagNMLMsgDictInterface cmd_diag_msg_write_dict = null;
    DiagNMLMsgDictInterface stat_diag_msg_write_dict = null;
    DiagNMLMsgDictCreatorInterface diag_dict_creator = null;
    NMLConnectionCreatorInterface nml_creator = null;

    public NMLMessageDictionary get_cmd_msg_dict() {
        return this.cmd_diag_msg_read_dict;
    }

    public NMLMessageDictionary get_stat_msg_dict() {
        return this.stat_diag_msg_read_dict;
    }

    public DiagNMLMsgDictInterface get_aux_diag_msg_write_dict() {
        return this.aux_diag_msg_write_dict;
    }

    public DiagNMLMsgDictInterface get_aux_diag_msg_read_dict() {
        return this.aux_diag_msg_read_dict;
    }

    public NMLConnectionCreatorInterface get_nml_creator() {
        return nml_creator;
    }

    public ModuleInfo(DiagNMLMsgDictCreatorInterface diag_dict_creator_param, NMLConnectionCreatorInterface nml_creator_param) {
        diag_dict_creator = diag_dict_creator_param;
        nml_creator = nml_creator_param;
        try {
            num_modules++;
            if (num_modules > max_module_number) {
                max_module_number = num_modules;
            } else {
                max_module_number++;
            }
            module_number = max_module_number;
            cmdsAvailable = new Vector();
            statMsgsAvailable = new Vector();
            auxMessages = new Vector();
            children_names = new Vector();
            if (null != diag_dict_creator && null != nml_creator) {
                aux_diag_msg_read_dict = diag_dict_creator.create(false, false);
                stat_diag_msg_read_dict = diag_dict_creator.create(false, true);
                cmd_diag_msg_read_dict = diag_dict_creator.create(true, false);
                if (double_buffer_nml) {
                    aux_diag_msg_write_dict = diag_dict_creator.create(false, false);
                } else {
                    aux_diag_msg_write_dict = aux_diag_msg_read_dict;
                }
                if (double_buffer_nml) {
                    stat_diag_msg_write_dict = diag_dict_creator.create(false, true);
                } else {
                    stat_diag_msg_write_dict = stat_diag_msg_read_dict;
                }
                if (double_buffer_nml) {
                    cmd_diag_msg_write_dict = diag_dict_creator.create(true, false);
                } else {
                    cmd_diag_msg_write_dict = cmd_diag_msg_read_dict;
                }
                if (aux_diag_msg_read_dict != null) {
                    aux_diag_msg_read_dict.SetModuleInfoObject(this);
                }
                if (stat_diag_msg_read_dict != null) {
                    stat_diag_msg_read_dict.SetModuleInfoObject(this);
                }
                if (cmd_diag_msg_read_dict != null) {
                    cmd_diag_msg_read_dict.SetModuleInfoObject(this);
                }
                if (double_buffer_nml
                        && aux_diag_msg_write_dict != null) {
                    aux_diag_msg_write_dict.SetModuleInfoObject(this);
                }
                if (double_buffer_nml
                        && stat_diag_msg_write_dict != null) {
                    stat_diag_msg_write_dict.SetModuleInfoObject(this);
                }
                if (double_buffer_nml
                        && cmd_diag_msg_write_dict != null) {
                    cmd_diag_msg_write_dict.SetModuleInfoObject(this);
                }
                if (!no_cmd) {
                    m_cmd_read_Connection = nml_creator.NewNMLConnection();
                    m_cmd_read_Connection.SetMessageDictionary(cmd_diag_msg_read_dict);
                    m_cmd_read_Connection.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
                    if (double_buffer_nml) {
                        m_cmd_write_Connection = nml_creator.NewNMLConnection();
                        m_cmd_write_Connection.SetMessageDictionary(cmd_diag_msg_write_dict);
                        m_cmd_write_Connection.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
                    } else {
                        m_cmd_write_Connection = m_cmd_read_Connection;
                    }
                }
                if (!no_stat) {
                    m_stat_read_Connection = nml_creator.NewNMLConnection();
                    m_stat_read_Connection.SetMessageDictionary(stat_diag_msg_read_dict);
                    m_stat_read_Connection.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
                    if (double_buffer_nml) {
                        m_stat_write_Connection = nml_creator.NewNMLConnection();
                        m_stat_write_Connection.SetMessageDictionary(stat_diag_msg_write_dict);
                        m_stat_write_Connection.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
                    } else {
                        m_stat_write_Connection = m_stat_read_Connection;
                    }
                }
            }

            predefined_type_files = new Vector();
            cmd_plotting_variables = new Hashtable();
            stat_plotting_variables = new Hashtable();

            Info = "";
            Name = "";
            if (UseDefaultTypes) {
                AddDefaultTypes();
            }
            if (!no_errlog_env_tested) {
                if (!no_errlog) {
                    no_errlog = (diagapplet.CodeGen.StringFuncs.getenv("NO_ERRLOG") != null);
                }
                no_errlog_env_tested = true;
            }
            if (nml_creator != null && null == m_errlogConnection && !no_errlog) {
                m_errlogConnection = nml_creator.NewNMLConnection();
                m_errlogConnection.SetMessageDictionary(new rcs.nml.errlogMsgDict());

                m_errlogConnection.set_buffer_name("errlog");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public NMLConnectionInterface createExtraCommandChannel() {
        NMLConnectionInterface nci = null;
        try {
            if (no_cmd) {
                return null;
            }
            DiagNMLMsgDictInterface dict
                    = diag_dict_creator.create(true, false);
            dict.SetModuleInfoObject(this);
            nci = nml_creator.NewNMLConnection();
            nci.SetMessageDictionary(dict);
            nci.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
            if (host != null) {
                nci.set_host(host);
            }
            if (null != m_cmd_read_Connection) {
                nci.set_buffer_number(m_cmd_read_Connection.get_buffer_number());
                nci.set_port(m_cmd_read_Connection.get_port());
                if (null != m_cmd_read_Connection.get_configuration_file()) {
                    nci.set_configuration_file(m_cmd_read_Connection.get_configuration_file());
                }
                if (null != m_cmd_read_Connection.get_buffer_name()) {
                    nci.set_buffer_name(m_cmd_read_Connection.get_buffer_name());
                }
                if (null != m_cmd_read_Connection.get_process_name()) {
                    nci.set_process_name(m_cmd_read_Connection.get_process_name());
                }
            }
            nci.ReadNMLConfigurationFileNoThrow();
            nci.connectNoThrow();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        return nci;
    }

    public NMLConnectionInterface createExtraStatusChannel() {
        NMLConnectionInterface nci = null;
        try {
            if (no_stat) {
                return null;
            }
            DiagNMLMsgDictInterface dict
                    = diag_dict_creator.create(false, true);
            dict.SetModuleInfoObject(this);
            nci = nml_creator.NewNMLConnection();
            nci.SetMessageDictionary(dict);
            nci.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
            if (host != null) {
                nci.set_host(host);
            }
            if (null != m_stat_read_Connection) {
                nci.set_buffer_number(m_stat_read_Connection.get_buffer_number());
                nci.set_port(m_stat_read_Connection.get_port());
                if (null != m_stat_read_Connection.get_configuration_file()) {
                    nci.set_configuration_file(m_stat_read_Connection.get_configuration_file());
                }
                if (null != m_stat_read_Connection.get_buffer_name()) {
                    nci.set_buffer_name(m_stat_read_Connection.get_buffer_name());
                }
                if (null != m_stat_read_Connection.get_process_name()) {
                    nci.set_process_name(m_stat_read_Connection.get_process_name());
                }
            }
            nci.ReadNMLConfigurationFileNoThrow();
            nci.connectNoThrow();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        return nci;
    }
    // @SuppressWarnings("unchecked")

    public void AddAuxInput(String name) {
        try {
            if (debug_on) {
                DebugPrint("AddAuxInput(" + name + ")");
            }
            if (null == name) {
                return;
            }
            if (name.length() < 1) {
                return;
            }
            if (null == AuxInputNames) {
                AuxInputNames = new Vector();
            }
            AuxInputNames.addElement(name);
            if (update_next_aux_every_cycle) {
                if (null == AuxUpdateEveryCycleNames) {
                    AuxUpdateEveryCycleNames = new Vector();
                }
                AuxUpdateEveryCycleNames.addElement(name);
            }
            if (null == AllAuxChannels) {
                AllAuxChannels = new Vector();
            }
            boolean inside_channels_list = false;
            for (int i = 0; i < AllAuxChannels.size(); i++) {
                String auxFromList = (String) AllAuxChannels.elementAt(i);
                if (name.equals(auxFromList)) {
                    inside_channels_list = true;
                    break;
                }
            }
            if (!inside_channels_list) {
                AllAuxChannels.addElement(name);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public void AddAuxOutput(String name) {
        try {
            if (debug_on) {
                DebugPrint("AddAuxOutput(" + name + ")");
            }
            if (null == name) {
                return;
            }
            if (name.length() < 1) {
                return;
            }
            if (null == AuxOutputNames) {
                AuxOutputNames = new Vector();
            }
            AuxOutputNames.addElement(name);
            if (update_next_aux_every_cycle) {
                if (null == AuxUpdateEveryCycleNames) {
                    AuxUpdateEveryCycleNames = new Vector();
                }
                AuxUpdateEveryCycleNames.addElement(name);
            }
            if (null == AllAuxChannels) {
                AllAuxChannels = new Vector();
            }
            boolean inside_channels_list = false;
            for (int i = 0; i < AllAuxChannels.size(); i++) {
                String auxFromList = (String) AllAuxChannels.elementAt(i);
                if (name.equals(auxFromList)) {
                    inside_channels_list = true;
                    break;
                }
            }
            if (!inside_channels_list) {
                AllAuxChannels.addElement(name);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    static public void AddDefaultTypes() {
        if (default_types_added) {
            return;
        }
        EnumTypeInfo enumInfoToAdd = new EnumTypeInfo();
        Integer enum_value = null;
        enumInfoToAdd.Name = "RCS_STATUS";
        enumInfoToAdd.Info = "UNINITIALIZED_STATUS =-1,RCS_DONE = 1, RCS_EXEC = 2, RCS_ERROR = 3";
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(-1), "UNINITIALIZED_STATUS");
        enumInfoToAdd.reverse_hashtable.put("UNINITIALIZED_STATUS", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(1), "RCS_DONE");
        enumInfoToAdd.reverse_hashtable.put("RCS_DONE", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(2), "RCS_EXEC");
        enumInfoToAdd.reverse_hashtable.put("RCS_EXEC", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(3), "RCS_ERROR");
        enumInfoToAdd.reverse_hashtable.put("RCS_ERROR", enum_value);
        m_enumInfoHashTable.put((Object) enumInfoToAdd.Name, (Object) enumInfoToAdd);

        enumInfoToAdd = new EnumTypeInfo();
        enum_value = null;
        enumInfoToAdd.Name = "RCS_ADMIN_STATE";
        enumInfoToAdd.Info = "RCS_ADMIN_ZERO=0,ADMIN_UNINITIALIZED=1,ADMIN_INITIALIZED=2,ADMIN_SHUT_DOWN=3";
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(0), "RCS_ADMIN_ZERO");
        enumInfoToAdd.reverse_hashtable.put("RCS_ADMIN_ZERO", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(1), "ADMIN_UNINITIALIZED");
        enumInfoToAdd.reverse_hashtable.put("ADMIN_UNINITIALIZED", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(2), "ADMIN_INITIALIZED");
        enumInfoToAdd.reverse_hashtable.put("ADMIN_INITIALIZED", enum_value);
        enumInfoToAdd.hashtable.put(enum_value = Integer.valueOf(3), "ADMIN_SHUT_DOWN");
        enumInfoToAdd.reverse_hashtable.put("ADMIN_SHUT_DOWN", enum_value);
        m_enumInfoHashTable.put((Object) enumInfoToAdd.Name, (Object) enumInfoToAdd);

        StructureTypeInfo typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("NMLmsg");
        typeInfoToAdd.setInfo("");
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("NML_ERROR");
        typeInfoToAdd.setInfo("char error[256];");
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.Id = 1;
        typeInfoToAdd.DerivedFrom = "NMLmsg";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        m_structInfoHashTable.put(Long.valueOf(1), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("NML_TEXT");
        typeInfoToAdd.setInfo("char text[256];");
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.Id = 2;
        typeInfoToAdd.DerivedFrom = "NMLmsg";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoHashTable.put(Long.valueOf(2), typeInfoToAdd);
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("NML_DISPLAY");
        typeInfoToAdd.setInfo("char display[256];");
        typeInfoToAdd.Id = 3;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = "NMLmsg";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoHashTable.put(Long.valueOf(3), typeInfoToAdd);
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("NMLmsg");
        typeInfoToAdd.setInfo("");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.is_nml_msg = true;
        typeInfoToAdd.setPreFinalPassInfoToInfo();

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("RCS_CMD_MSG");
        typeInfoToAdd.setInfo("int serial_number;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = "NMLmsg";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.is_nml_msg = true;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("RCS_STAT_MSG");
        typeInfoToAdd.setInfo("long command_type;int echo_serial_number;enum RCS_STATUS status;int state;int line;int source_line;char source_file[64];");
        //typeInfoToAdd.setInfo("long command_type;int echo_serial_number;int status;int state;int lineNumber;\n");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = "NMLmsg";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.is_nml_msg = true;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("RCS_STAT_MSG_V2");
        typeInfoToAdd.setInfo(" long command_type;int echo_serial_number;enum RCS_STATUS status;int state;int line;int source_line;char source_file[64];enum RCS_ADMIN_STATE admin_state;int tt.count;double tt.last;double tt.now;double tt.start;double tt.elapsed;double tt.min;double tt.max;double tt.avg;int message_length;NML_DYNAMIC_LENGTH_ARRAY char message[80];");
        //typeInfoToAdd.setInfo("long command_type;int echo_serial_number;int status;int state;int lineNumber;\n");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.PreFinalPassInfo = "enum RCS_ADMIN_STATE admin_state;struct time_tracker tt;int message_length;NML_DYNAMIC_LENGTH_ARRAY char message[80];";
        typeInfoToAdd.DerivedFrom = "RCS_STAT_MSG";
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.setInfo(" long command_type;int echo_serial_number;enum RCS_STATUS status;int state;int line;int source_line;char source_file[64];enum RCS_ADMIN_STATE admin_state;int tt.count;double tt.last;double tt.now;double tt.start;double tt.elapsed;double tt.min;double tt.max;double tt.avg;int message_length;NML_DYNAMIC_LENGTH_ARRAY char message[80];");
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.is_nml_msg = true;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("time_tracker");
        typeInfoToAdd.setInfo("int count;double last;double now;double start;double elapsed;double min;double max;double avg;");
        //typeInfoToAdd.setInfo("long command_type;int echo_serial_number;int status;int state;int lineNumber;\n");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.is_nml_msg = false;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        /*
        StructureTypeInfo stat_type_info = typeInfoToAdd;
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.Name = "RCS_STAT_MSG_V2";
        typeInfoToAdd.setInfo("long command_type;int echo_serial_number; enum RCS_STATUS status; int state; int lineNumber; int source_line; char source_file[64]; enum RCS_ADMIN_STATE admin_state; ");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.PreFinalPassInfo = "PM_CARTESIAN x;PM_CARTESIAN y;PM_CARTESIAN z;";
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
         */
        // POSEMATH types
        // translation types
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("CMS_DATE_TIME");
        typeInfoToAdd.setInfo("long years;long months;long days;long hours;long minutes;double seconds;int timezoneoffsethours;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("CMS_DATE");
        typeInfoToAdd.setInfo("long years;long months;long days");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("CMS_TIME");
        typeInfoToAdd.setInfo("long hours;long minutes;double seconds;int timezoneoffsethours;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("CMS_DURATION");
        typeInfoToAdd.setInfo("long years;long months;long days;long hours;long minutes;double seconds;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_CARTESIAN");
        typeInfoToAdd.setInfo("double x;double y;double z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PmCartesian");
        typeInfoToAdd.setInfo("double x;double y;double z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        typeInfoToAdd.fromFileName = "posemath.h";

        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd.fromFileName = "";

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_SPHERICAL");
        typeInfoToAdd.setInfo("double theta;double phi;double r;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_CYLINDRICAL");
        typeInfoToAdd.setInfo("double theta;double r;double z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        // rotation types
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_ROTATION_VECTOR");
        typeInfoToAdd.setInfo("double s;double x;double y;double z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_ROTATION_MATRIX");
        typeInfoToAdd.setInfo("double x.x; double x.y; double x.z; double y.x; double y.y; double y.z; double z.x; double z.y; double z.z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.PreFinalPassInfo = "PM_CARTESIAN x;PM_CARTESIAN y;PM_CARTESIAN z;";
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_QUATERNION");
        typeInfoToAdd.setInfo("double s;double x;double y;double z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_EULER_ZYZ");
        typeInfoToAdd.setInfo("double z;double y;double zp;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_EULER_ZYX");
        typeInfoToAdd.setInfo("double z;double y;double x;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_RPY");
        typeInfoToAdd.setInfo("double r;double p;double y;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_XYA");
        typeInfoToAdd.setInfo("double x;double y;double a;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.setPreFinalPassInfoToInfo();
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        // pose types
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_POSE");
        typeInfoToAdd.setInfo("double tran.x;double tran.y;double tran.z;double rot.s;double rot.x;double rot.y;double rot.z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.PreFinalPassInfo = "PM_CARTESIAN tran;PM_QUATERNION rot;";
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);
        typeInfoToAdd = new StructureTypeInfo();
        typeInfoToAdd.setName("PM_HOMOGENEOUS");
        typeInfoToAdd.setInfo("double tran.x;double tran.y;double tran.z;double rot.x.x;double rot.x.y;double rot.x.z;double rot.y.x;double rot.y.y;double rot.y.z;double rot.z.x;double rot.z.y;double rot.z.z;");
        typeInfoToAdd.Id = -2;
        typeInfoToAdd.PreFinalPassInfo = "PM_CARTESIAN tran;PM_ROTATION_MATRIX rot;";
        typeInfoToAdd.DerivedFrom = null;
        typeInfoToAdd.UnqualifiedDerivedFrom = typeInfoToAdd.DerivedFrom;
        m_structInfoByNameHashTable.put(typeInfoToAdd.getName(), typeInfoToAdd);

        default_types_added = true;
    }

    void AddInternalStructure(String className, String classVarName, boolean ndla, StringBuffer sb_to_add_to) throws Exception {
        StructureTypeInfo sti = (StructureTypeInfo) m_structInfoByNameHashTable.get(className);
        if (debug_on) {
            DebugPrint("AddInternalStructure(String className=" + className + ", String classVarName=" + classVarName + ")");
            DebugPrint("sti=" + sti);
        }
        if (null == sti) {
            return;
        }
        int tokens_to_skip = 0;
        try {
            int info_tokenizer_num_tokens = infoTokenizer.countTokens();
            String top_parent = sti.DerivedFrom;
            String last_token = "";
            while (null != top_parent) {

                StructureTypeInfo parentInfo = (StructureTypeInfo) m_structInfoByNameHashTable.get(top_parent);
                if (top_parent.equals("RCS_CMD_MSG") || parentInfo.is_rcs_cmd_msg) {
                    tokens_to_skip = 1;
                    sti.is_rcs_cmd_msg = true;
                    parentInfo.is_rcs_cmd_msg = true;
                    break;
                }
                if (top_parent.equals("RCS_STAT_MSG") || parentInfo.is_rcs_stat_msg) {
                    tokens_to_skip = 7;
                    sti.is_rcs_stat_msg = true;
                    parentInfo.is_rcs_stat_msg = true;
                    break;
                }
                if (null == parentInfo) {
                    break;
                }
                top_parent = parentInfo.DerivedFrom;
            }
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        infoTokenizer = new StringTokenizer(sti.HiddenInfo, ";");
        for (int i = 0; i < tokens_to_skip; i++) {
            infoTokenizer.nextToken();
        }
        StringBuffer sb = new StringBuffer((infoTokenizer.countTokens() + 2) * 100);
        while (infoTokenizer.hasMoreTokens()) {
            if (Thread.interrupted()) {
                interrupt_loading = true;
                throw new Exception("Thread.interrupted() returned true\n");
            }
            if (interrupt_loading) {
                throw new Exception("interrupt_loading=true\n");
            }
            String infoToken = infoTokenizer.nextToken();
            String typeString;
            String varString;
            int lastSpaceIndex = infoToken.lastIndexOf(' ');
            if (lastSpaceIndex > 0) {
                varString = infoToken.substring(lastSpaceIndex + 1);
                typeString = infoToken.substring(0, lastSpaceIndex);
                if (ndla) {
                    sb.append(SplitInfoToken.ndla_string);
                    sb.append(' ');
                }
                sb.append(typeString + " " + classVarName + "." + varString + ";");
            } else {
                if (ndla) {
                    sb.append(SplitInfoToken.ndla_string);
                    sb.append(' ');
                }
                sb.append(classVarName + "." + infoToken + ";");
            }
        }
        sb_to_add_to.append(sb);
        //DebugPrint(returnString +" = AddInternalStructure("+className+","+classVarName+");");
    }
    public int writeCmdsFailedInARow = 0;
    volatile boolean sending_command = false;
    volatile public int commands_sent = 0;

    public void writeCmd(String cmdString) {
        if (no_cmd) {
            return;
        }
        if (!m_cmd_read_Connection.is_connected()) {
            m_cmd_read_Connection.connectNoThrow();
        }
        if (double_buffer_nml) {
            if (!m_cmd_write_Connection.is_connected()) {
                m_cmd_write_Connection.connectNoThrow();
            }
        } else {
            m_cmd_write_Connection = m_cmd_read_Connection;
        }
        if (double_buffer_nml
                && !m_stat_write_Connection.is_connected()) {
            m_stat_write_Connection.connectNoThrow();
        }
        if (!m_stat_read_Connection.is_connected()) {
            m_stat_read_Connection.connectNoThrow();
        }
        if (!m_cmd_write_Connection.is_connected()) {
            writeCmdsFailedInARow++;
            return;
        }
        try {
            sending_command = true;
            if (commands_sent < 1) {
                m_cmd_write_Connection.writeDataString(cmdString);
                try {
                    Thread.sleep(10);
                } catch (Exception e) {
                }
            }
            if (debug_on) {
                DebugPrint("Sending " + cmdString);
            }
            int starting_failed_count = cmd_diag_msg_write_dict.get_failed_count();
            if (m_cmd_write_Connection.writeDataString(cmdString) < 0) {
                ErrorPrint("Module " + Name + " failed to write cmdString " + cmdString);
                writeCmdsFailedInARow++;
                if (writeCmdsFailedInARow > 3) {
                    m_cmd_write_Connection.disconnect();
                    m_cmd_write_Connection.connectNoThrow();
                }
            } else {
                if (cmd_diag_msg_write_dict.get_failed_count() > starting_failed_count) {

                    ErrorPrint("Module " + Name + " failed to write cmdString " + cmdString);
                    writeCmdsFailedInARow++;
                } else {
                    writeCmdsFailedInARow = 0;
                    commands_sent++;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        sending_command = false;
    }
    public int writeStatsFailedInARow = 0;
    volatile boolean sending_status = false;
    volatile public int stats_sent = 0;

    public void writeStat(String statString) {
        if (!m_stat_write_Connection.is_connected()) {
            m_stat_write_Connection.connectNoThrow();
        }
        if (!m_stat_write_Connection.is_connected()) {
            ErrorPrint("!m_stat_write_Connection.get_connected())");
            writeStatsFailedInARow++;
            return;
        }
        try {
            sending_status = true;
            if (stats_sent < 1) {
                m_stat_write_Connection.writeDataString(statString);
                try {
                    Thread.sleep(10);
                } catch (Exception e) {
                }
            }
            if (debug_on) {
                DebugPrint("Sending " + statString);
            }
            int starting_failed_count = stat_diag_msg_write_dict.get_failed_count();
            if (m_stat_write_Connection.writeDataString(statString) < 0) {
                ErrorPrint("Module " + Name + " failed to write statString " + statString);
                writeStatsFailedInARow++;
                if (writeStatsFailedInARow > 3) {
                    m_stat_write_Connection.disconnect();
                    m_stat_write_Connection.connectNoThrow();
                }
            } else {
                if (stat_diag_msg_write_dict.get_failed_count() > starting_failed_count) {

                    ErrorPrint("Module " + Name + " failed to write statString " + statString);
                    writeStatsFailedInARow++;
                } else {
                    writeStatsFailedInARow = 0;
                    stats_sent++;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        sending_status = false;
    }

    static public boolean is_check_errlog_flag_set() {
        if (null != m_errlogConnection && !no_errlog) {
            return true;
        }
        return false;
    }

    static public String readErrlogData() {
        String temp;
        if (no_errlog || null == m_errlogConnection) {
            return null;
        }
//        if (parse_error_to_send) {
//            if (parse_error_string != null) {
//                if (parse_error_string.length() > 0) {
//                    errlogData = "1,0," + parse_error_string + ",";
//                    new_errlog = true;
//                    parse_error_string = "";
//                    return errlogData;
//                }
//            }
//        }
//        parse_error_to_send = false;
//        if (!is_connected) {
//            return null;
//        }
        try {
            temp = m_errlogConnection.read_errlog_string();
        } catch (Exception e) {
            temp = null;
        }
        if (temp != null) {
            new_errlog = true;
            errlogData = temp;
        }
        return temp;
    }

    private int get_serial_number(String str) {
        try {
            if (null == str) {
                return -1;
            }
            StringTokenizer tokenizer = new StringTokenizer(str, ",; +\t");
            int token_number = 0;
            if (null == tokenizer) {
                return -1;
            }
            while (tokenizer.hasMoreTokens() && token_number < 4) {
                String token = tokenizer.nextToken();
                token_number++;
                if (token_number == 3) {
                    try {
                        return rcs.utils.StrToInt.convert(token);
                    } catch (Exception e) {
                    }
                    return -1;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return -1;
    }

    private int get_echo_serial_number(String str) {
        try {
            if (null == str) {
                return -1;
            }
            StringTokenizer tokenizer = new StringTokenizer(str, ",; +\t");
            int token_number = 0;
            if (null == tokenizer) {
                return -1;
            }
            while (tokenizer.hasMoreTokens() && token_number < 5) {
                String token = tokenizer.nextToken();
                token_number++;
                if (token_number == 4) {
                    try {
                        //DebugPrint("get_echo_serial_number("+str+") returning "+token+".");
                        return rcs.utils.StrToInt.convert(token);
                    } catch (Exception e) {
                    }
                    return -1;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return -1;
    }
    public int updateCmdDataErrors = 0;
    public int new_cmd_count = 0;

    public String updateCmdData() {
        if (no_cmd) {
            return null;
        }
        String temp;
        int starting_failed_count = cmd_diag_msg_read_dict.get_failed_count();
        int starting_error_count = m_cmd_read_Connection.getNoThrowErrorCount();
        if (sending_command) {
            return null;
        }
        if (!m_cmd_read_Connection.is_connected()
                && m_cmd_read_Connection.get_disconnect_time() + 2000 < System.currentTimeMillis()
                && m_cmd_read_Connection.get_connect_time() + 2000 < System.currentTimeMillis()) {
            m_cmd_read_Connection.connectNoThrow();
            if (double_buffer_nml) {
                if (!m_cmd_write_Connection.is_connected()) {
                    m_cmd_write_Connection.connectNoThrow();
                }
            } else {
                m_cmd_write_Connection = m_cmd_read_Connection;
            }
        }
        if (!m_cmd_read_Connection.is_connected()) {
            updateCmdDataErrors++;
            if (m_cmd_read_Connection.get_disconnect_time() + 4000 < System.currentTimeMillis()
                    && m_cmd_read_Connection.get_connect_time() + 4000 < System.currentTimeMillis()) {
                if (!m_stat_write_Connection.is_connected()
                        || !m_stat_read_Connection.is_connected()) {
                    disconnect();
                } else {
                    m_cmd_read_Connection.disconnect();
                    if (double_buffer_nml) {
                        m_cmd_write_Connection.disconnect();
                    }
                }
            }
            return null;
        }
        try {
            temp = m_cmd_read_Connection.peekDataStringNoThrow();
        } catch (Exception e) {
            updateCmdDataErrors++;
            if (debug_on) {
                e.printStackTrace();
            }
            temp = null;
            if (!m_stat_write_Connection.is_connected()
                    || !m_stat_read_Connection.is_connected()) {
                disconnect();
            } else {
                m_cmd_read_Connection.disconnect();
                if (double_buffer_nml) {
                    m_cmd_write_Connection.disconnect();
                }
            }
        }
        if (temp != null) {
            cmd_has_been_plotted = false;
            cmdData = temp;
            last_new_command_time = System.currentTimeMillis();
            serial_number = get_serial_number(cmdData);
            if (new_cmd_count != m_cmd_read_Connection.get_last_id_read()) {
                new_cmd_count = m_cmd_read_Connection.get_last_id_read();
                new_cmd = true;
                new_data_count++;
            }
//	} else {
////	    if (last_new_command_time + 60000 < System.currentTimeMillis() &&
////		    m_cmd_read_Connection.get_disconnect_time() + 4000 < System.currentTimeMillis() &&
////		    m_cmd_read_Connection.get_connect_time() + 60000 < System.currentTimeMillis() &&
////		    new_cmd_count > 2) {
////		if (!m_stat_write_Connection.is_connected() ||
////			!m_stat_read_Connection.is_connected()) {
////		    disconnect();
////		} else {
////		    m_cmd_read_Connection.disconnect();
////		    if (double_buffer_nml) {
////			m_cmd_write_Connection.disconnect();
////		    }
////		}
////	    }
        }

        if (starting_error_count != m_cmd_read_Connection.getNoThrowErrorCount()) {
            updateCmdDataErrors++;
            if (!m_stat_write_Connection.is_connected()
                    || !m_stat_read_Connection.is_connected()) {
                disconnect();
            } else {
                m_cmd_read_Connection.disconnect();
                if (double_buffer_nml) {
                    m_cmd_write_Connection.disconnect();
                }
            }
        }
        if (cmd_diag_msg_read_dict.get_failed_count() > starting_failed_count) {
            updateCmdDataErrors++;
            lastStatName = "ERROR";
        }
        return temp;
    }
    public int updateStatDataErrors = 0;
    public int new_stat_count = 0;

    public String updateStatData() {
        if (no_stat) {
            return null;
        }
        String temp;
        int starting_failed_count = stat_diag_msg_read_dict.get_failed_count();
        int starting_error_count = m_stat_read_Connection.getNoThrowErrorCount();
        if (!m_stat_read_Connection.is_connected()
                && m_stat_read_Connection.get_disconnect_time() + 2000 < System.currentTimeMillis()
                && m_stat_read_Connection.get_connect_time() + 2000 < System.currentTimeMillis()) {
            m_stat_read_Connection.connectNoThrow();
            if (!m_stat_write_Connection.is_connected()) {
                m_stat_write_Connection.connectNoThrow();
            }
        }
        if (!m_stat_read_Connection.is_connected()) {
            if (debug_on) {
                DebugPrint("is_connected=false;");
            }
            updateStatDataErrors++;
            if (m_stat_read_Connection.get_disconnect_time() + 4000 < System.currentTimeMillis()
                    && m_stat_read_Connection.get_connect_time() + 4000 < System.currentTimeMillis()) {
                if (!m_cmd_write_Connection.is_connected()
                        || !m_cmd_read_Connection.is_connected()) {
                    disconnect();
                } else {
                    m_stat_read_Connection.disconnect();
                    if (double_buffer_nml) {
                        m_stat_write_Connection.disconnect();
                    }
                }
            }
            return null;
        }
        try {
            temp = m_stat_read_Connection.peekDataStringNoThrow();
        } catch (Exception e) {
            if (debug_on) {
                e.printStackTrace();
            }
            updateStatDataErrors++;
            temp = null;
            if (!m_cmd_write_Connection.is_connected()
                    || !m_cmd_read_Connection.is_connected()) {
                disconnect();
            } else {
                m_stat_read_Connection.disconnect();
                if (double_buffer_nml) {
                    m_stat_write_Connection.disconnect();
                }
            }
        }
        if (temp != null) {
            if (new_stat_count != m_stat_read_Connection.get_last_id_read()) {
                new_stat_count = m_stat_read_Connection.get_last_id_read();
                new_stat = true;
                new_data_count++;
            }
            stat_has_been_plotted = false;
            statData = temp;
            last_new_stat_time = System.currentTimeMillis();
            echo_serial_number = get_echo_serial_number(statData);
//	} else {
//	    if (last_new_stat_time + 30000 < System.currentTimeMillis() &&
//		    new_stat_count > 2 &&
//		    m_stat_read_Connection.get_disconnect_time() + 2000 < System.currentTimeMillis() &&
//		    m_stat_read_Connection.get_connect_time() + 60000 < System.currentTimeMillis()) {
//		if (!m_cmd_write_Connection.is_connected() ||
//			!m_cmd_read_Connection.is_connected()) {
//		    disconnect();
//		} else {
//		    m_stat_read_Connection.disconnect();
//		    if (double_buffer_nml) {
//			m_stat_write_Connection.disconnect();
//		    }
//		}
//	    }
        }

        if (starting_error_count != m_stat_read_Connection.getNoThrowErrorCount()) {
            updateStatDataErrors++;
            if (!m_cmd_write_Connection.is_connected()
                    || !m_cmd_read_Connection.is_connected()) {
                disconnect();
            } else {
                m_stat_read_Connection.disconnect();
                if (double_buffer_nml) {
                    m_stat_write_Connection.disconnect();
                }
            }
        }
        if (stat_diag_msg_read_dict.get_failed_count() > starting_failed_count) {
            updateStatDataErrors++;
        }
        return temp;
    }

    public void setHost(String new_host) {
        host = new_host;
        if (no_cmd) {
            if (double_buffer_nml
                    && m_cmd_write_Connection != null) {
                m_cmd_write_Connection.set_host(host);
            }
            if (m_cmd_read_Connection != null) {
                m_cmd_read_Connection.set_host(host);
            }
        }
        if (no_stat) {
            if (double_buffer_nml
                    && m_stat_write_Connection != null) {
                m_stat_write_Connection.set_host(host);
            }
            if (m_stat_read_Connection != null) {
                m_stat_read_Connection.set_host(host);
            }
        }
        if (!no_errlog_env_tested) {
            if (!no_errlog) {
                no_errlog = (diagapplet.CodeGen.StringFuncs.getenv("NO_ERRLOG") != null);
            }
            no_errlog_env_tested = true;
        }
        if (errlog_on_this_host && m_errlogConnection != null && !no_errlog) {
            m_errlogConnection.set_host(host);
        }
    }

    public boolean isMathString(String str) {
        try {
            StringTokenizer st = new StringTokenizer(str, " \t\r\n+-*/,()");
            int int_count = 0;
            while (st.hasMoreTokens()) {
                String tok = st.nextToken();
                int_count++;
                try {
                    Integer.valueOf(tok);
                } catch (Exception e) {
                    return false;
                }
            }
            return (int_count > 1);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }
    // @SuppressWarnings("unchecked")

    public String ReplaceDefinedValues(String inStr, int rdv_recurse_count, Hashtable usedDefines) {
        String outStr = "";
        String breakers = " \t\r\n+-/*\\,(){};";
        String word = "";
        int breakers_index = -1;
        String arrayLengthString = "";

        try {
            Hashtable newUsedDefines = null;
            rdv_recurse_count++;
            if (rdv_recurse_count > 50) {
                ErrorPrint("Macro " + inStr + " contains too many levels of internal macros to be expanded.\n");
                return "BAD_RECURSION";
            }
            if (null == inStr) {
                return null;
            }
            for (int i = 0; i < inStr.length(); i++) {
                char cur_char = inStr.charAt(i);
                breakers_index = breakers.indexOf(cur_char);
                //if(debug_on)
                // {
                //DebugPrint("ReplaceDefinedValues: inStr="+inStr+",cur_char="+cur_char+",i="+i+",breakers_index="+breakers_index+",word="+word+",outStr="+outStr);
                // }
                if (breakers_index >= 0 || i == inStr.length() - 1) {
                    if (i == inStr.length() - 1 && breakers_index == -1) {
                        word += Character.toString(cur_char);
                    }
                    if (debug_on) {
                        DebugPrint("ReplaceDefinedValues : rdv_recurse_count=" + rdv_recurse_count);
                        DebugPrint("ReplaceDefinedValues : inStr = " + inStr);
                        DebugPrint("ReplaceDefinedValues : outStr = " + outStr);
                        DebugPrint("ReplaceDefinedValues : word = " + word);
                        DebugPrint("ReplaceDefinedValues : usedDefines=" + usedDefines);
                        DebugPrint("ReplaceDefinedValues : newUsedDefines=" + newUsedDefines);
                    }
                    if (word.length() > 0) {
                        DefinedValue dv = (DefinedValue) definedValues.get(word);
                        if (null != dv && dv.value.indexOf(dv.name) >= 0) {
                            ErrorPrint("Defined value contains itself : dv.name=" + dv.name + ", dv.value=" + dv.value + ",dv.tag=" + dv.tag + ", dv=" + dv);
                            ErrorPrint("undefining " + dv.name);
                            definedValues.remove(dv.name);
                            dv = null;
                        }
                        if (null != usedDefines && null != dv && null != dv.name) {
                            if (usedDefines.get(dv.name) != null) {
                                if (debug_on) {
                                    DebugPrint(dv.name + " is already in usedDefines list.");
                                }
                                dv = null;
                            }
                        }
                        if (null != dv) {
                            if (debug_on) {
                                DebugPrint(word + " replaced with " + dv.value + " from " + dv.tag);
                            }
                            outStr += dv.value;
                            if (newUsedDefines == null) {
                                newUsedDefines = new Hashtable();
                            }
                            newUsedDefines.put(dv.name, dv);
                        } else {
                            if (debug_on
                                    && (word.charAt(0) < '0' || word.charAt(0) > '9')) {
                                DebugPrint(word + " is not defined.");
                            }
                            outStr += word;
                        }
                        word = "";
                    }
                    if (i != inStr.length() - 1 || breakers_index != -1) {
                        outStr += Character.valueOf(cur_char).toString();
                    }
                } else {
                    word += Character.valueOf(cur_char).toString();
                }
            }
            if (word.length() > 0) {
                DefinedValue dv = (DefinedValue) definedValues.get(word);
                if (null != dv) {
                    if (debug_on) {
                        DebugPrint("outStr = " + outStr);
                        DebugPrint(word + " replaced with " + dv.value);
                    }
                    outStr += dv.value;
                    arrayLengthString = dv.arrayLenSting;
                } else {
                    outStr += word;
                }
            }
            if (!inStr.equals(outStr)) {
                if (debug_on) {
                    DebugPrint(outStr + " = ReplaceDefinedValues(" + inStr + ")");
                }
                if (isMathString(outStr)) {
                    int i = doArrayLengthMath(outStr);
                    String newOutStr = String.valueOf(i);
                    if (debug_on) {
                        DebugPrint("ReplaceDefinedValues: " + i + "=doArrayLengthMath(" + outStr + ");");
                        DebugPrint(newOutStr + " = ReplaceDefinedValues(" + inStr + ")");
                    }
                    return newOutStr;
                }
                if (newUsedDefines != null && usedDefines == null) {
                    usedDefines = newUsedDefines;
                } else if (newUsedDefines != null && usedDefines != null) {
                    usedDefines.putAll(newUsedDefines);
                }
                String outStr2 = ReplaceDefinedValues(outStr, (rdv_recurse_count + 1), usedDefines);
                return outStr2;
            }
        } catch (Exception e) {
            e.printStackTrace();
            outStr = inStr;
        }
        if (arrayLengthString.length() > 0) {
            System.out.println("inStr = " + inStr);
            System.out.println("outStr = " + outStr);
            System.out.println("arrayLengthString = " + arrayLengthString);
            return outStr + arrayLengthString;
        }
        return outStr;
    }

    public static int doArrayLengthMath(String str) {
        try {
            int return_value = 0;
            int plus_index = str.indexOf("+");
            int minus_index = str.indexOf("-");
            int times_index = str.indexOf("*");
            int fslash_index = str.indexOf("/");
            if (plus_index < 0 && times_index < 0
                    && minus_index < 0 && fslash_index < 0) {
                return_value = rcs.utils.StrToInt.convert(str);
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            }
            if (plus_index > 0) {
                return_value = doArrayLengthMath(str.substring(0, plus_index)) + doArrayLengthMath(str.substring(plus_index + 1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            } else if (plus_index == 0) {
                return_value = doArrayLengthMath(str.substring(1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            }

            if (minus_index > 0) {
                return_value = doArrayLengthMath(str.substring(0, minus_index)) - doArrayLengthMath(str.substring(minus_index + 1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            } else if (minus_index == 0) {
                return_value = -1 * doArrayLengthMath(str.substring(1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            }

            if (times_index > 0) {
                return_value = doArrayLengthMath(str.substring(0, times_index)) * doArrayLengthMath(str.substring(times_index + 1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            } else if (times_index == 0) {
                return_value = doArrayLengthMath(str.substring(1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            }

            if (fslash_index > 0) {
                return_value = doArrayLengthMath(str.substring(0, fslash_index)) / doArrayLengthMath(str.substring(fslash_index + 1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            } else if (fslash_index == 0) {
                return_value = doArrayLengthMath(str.substring(1));
                if (debug_on) {
                    DebugPrint(return_value + " = doArrayLengthMath(" + str + ")");
                }
                return return_value;
            }
        } catch (Exception e) {
            ErrorPrint("str = " + str + ", e=" + e);
//            e.printStackTrace();
            return -1;
        }
        return 1;
    }
    static public boolean bad_simple_long_math = false;

    public static long doSimpleLongMath(String str) {
        try {
            long return_value = 0;
            int long_cast_index = str.indexOf("(long)");
            if (long_cast_index >= 0) {
                String new_str = str.substring(0, long_cast_index) + str.substring(long_cast_index + 6);
                return_value = doSimpleLongMath(new_str);
                if (debug_on) {
                    DebugPrint(return_value + " = doSimpleLongMath(" + str + ")");
                }
                return return_value;
            }
            int paren_start_index = str.lastIndexOf('(');
            int paren_end_index = str.indexOf(')', paren_start_index + 1);
            if (paren_start_index >= 0 && paren_end_index > paren_start_index
                    && paren_start_index < str.length() && paren_end_index < str.length()) {
                String new_str = str.substring(0, paren_start_index) + doSimpleLongMath(str.substring(paren_start_index + 1, paren_end_index)) + str.substring(paren_end_index + 1);
                //DebugPrint2("new_str="+new_str);
                return_value = doSimpleLongMath(new_str);
            } else {
                int plus_index = str.indexOf("+");
                int minus_index = str.indexOf("-");
                int times_index = str.indexOf("*");
                int fslash_index = str.indexOf("/");
                if (plus_index <= 0 && times_index < 0
                        && minus_index <= 0 && fslash_index <= 0) {
                    return_value = rcs.utils.StrToLong.convert(str);
                    if (rcs.utils.StrToLong.bad_token) {
                        bad_simple_long_math = true;
                    }
                } else if (plus_index > 0) {
                    return_value = doSimpleLongMath(str.substring(0, plus_index)) + doSimpleLongMath(str.substring(plus_index + 1));
                } else if (plus_index == 0) {
                    return_value = doSimpleLongMath(str.substring(1));
                } else if (minus_index > 0) {
                    return_value = doSimpleLongMath(str.substring(0, minus_index)) - doSimpleLongMath(str.substring(minus_index + 1));
                } else if (minus_index == 0) {
                    return_value = -1 * doSimpleLongMath(str.substring(1));
                } else if (times_index > 0) {
                    return_value = doSimpleLongMath(str.substring(0, times_index)) * doSimpleLongMath(str.substring(times_index + 1));
                } else if (times_index == 0) {
                    return_value = doSimpleLongMath(str.substring(1));
                } else if (fslash_index > 0) {
                    return_value = doSimpleLongMath(str.substring(0, fslash_index)) / doSimpleLongMath(str.substring(fslash_index + 1));
                } else if (fslash_index == 0) {
                    return_value = doSimpleLongMath(str.substring(1));
                }
            }
            if (debug_on) {
                DebugPrint(return_value + " = doSimpleLongMath(" + str + ")");
            }
            //DebugPrint2(return_value+" = doSimpleLongMath("+str+")");
            return return_value;
        } catch (Exception e) {
            bad_simple_long_math = true;
            System.err.println("str=" + str);
            e.printStackTrace();
            return 0;
        }
    }

    private String SeparateArrayElements(String varString) {
        return varString + ";";
    }

    /*
    {
    if(application_type != RCS_DIAGNOSTICS_APPLICATION_TYPE
    && !always_perform_final_pass)
    {
    return "";
    }
    int indexLSquareParen;
    int indexRSquareParen;
    int arrayLength;
    String arrayString;
    String  new_var_string;
    String preArrayString = "";
    String postArrayString = "";

    if(debug_on)
    {
    DebugPrint("SeparateArrayElements("+varString+")");
    }

    try
    {
    indexRSquareParen = varString.indexOf(']');
    if(indexRSquareParen < 0)
    {
    return varString+";";
    }
    indexLSquareParen = varString.indexOf('[');
    if(indexLSquareParen < 0)
    {
    return varString+";";
    }
    if(indexLSquareParen >= indexRSquareParen)
    {
    return varString+";";
    }
    if(-1 != varString.indexOf("char "))
    {
    return varString+";";
    }
    if(-1 != varString.indexOf("."))
    {
    return varString+";";
    }
    arrayString = varString.substring(indexLSquareParen +1, indexRSquareParen);
    arrayString = ReplaceDefinedValues(arrayString,null);
    try
    {
    arrayLength = doArrayLengthMath(arrayString);
    }
    catch(NumberFormatException e)
    {
    if(null == definedValues)
    {
    e.printStackTrace();
    return varString+";";
    }
    DefinedValue dv =  (DefinedValue) definedValues.get(arrayString);
    if(null == dv)
    {
    e.printStackTrace();
    return varString+";";
    }
    arrayLength = rcs.utils.StrToInt.convert(dv.value);
    }
    if(arrayLength < 1)
    {
    return varString;
    }
    preArrayString = varString.substring(0,indexLSquareParen+1);
    if(indexRSquareParen < varString.length())
    {
    postArrayString = varString.substring(indexRSquareParen);
    }
    if(arrayLength > 4000000)
    {
    ErrorPrint("Array "+varString+" is too long for diagnostics tool. (Truncating!)");
    arrayLengthTooLong = true;
    arrayLength = 4000;
    }
    StringBuffer sb = new StringBuffer((arrayLength+2)*(preArrayString.length()+postArrayString.length()+10));
    for(int i = 0; i < arrayLength; i++)
    {
    sb.append(preArrayString);
    sb.append(i);
    sb.append(postArrayString);
    sb.append(';');
    //new_var_string += preArrayString+i+postArrayString+";";
    }
    return sb.toString();
    }
    catch(Exception e)
    {
    e.printStackTrace();
    }
    return varString;
    }
     */
    private String SeparateCommaSeparatedVariables(String varString) throws Exception {
        int comma_index;
        int space_index;
        int tab_index;
        String typeString;
        String varNameString = null;
        String new_var_string = null;
        int new_comma_index;

        try {
            if (debug_on) {
                DebugPrint("SeparateCommaSeparatedVariables(" + varString + ") called.");
            }
            if (Thread.interrupted()) {
                interrupt_loading = true;
                throw new Exception("Thread.interrupted() returned true\n");
            }
            if (interrupt_loading) {
                throw new Exception("interrupt_loading=true\n");
            }
            comma_index = varString.indexOf(',');
            if (comma_index < 0) {
                return varString;
            }
            typeString = varString.substring(0, comma_index);
            space_index = comma_index;
            while (varNameString == null) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                space_index = typeString.lastIndexOf(' ');
                if (space_index < 0) {
                    return varString;
                }
                tab_index = typeString.lastIndexOf('\t');
                if (tab_index > space_index) {
                    space_index = tab_index;
                }
                typeString = varString.substring(0, space_index);
                varNameString = varString.substring(space_index + 1, comma_index);
                if (varNameString.length() < 1) {
                    varNameString = null;
                }
            }
            if (debug_on) {
                DebugPrint("SeparateCommaSeparatedVariables: typeString =" + typeString);
            }
            new_var_string = typeString + " " + varNameString + ";";
            while (-1 != (new_comma_index = varString.indexOf(',', comma_index + 1))) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                varNameString = varString.substring(comma_index + 1, new_comma_index);
                comma_index = new_comma_index;
                new_var_string += typeString + " " + varNameString + ";";
            }
            varNameString = varString.substring(comma_index + 1);
            new_var_string += typeString + " " + varNameString + ";";
            if (debug_on) {
                DebugPrint("SeparateCommaSeparatedVariables: returning  " + new_var_string);
            }
            return new_var_string;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        return varString;
    }
    static boolean always_perform_final_pass = false;

    private String PerformFinalPassOnInfoString(String info) throws Exception {
        if (debug_on) {
            DebugPrint("PerformFinalPassOnInfoString(" + info + ")");
        }
        if (application_type != RCS_DIAGNOSTICS_APPLICATION_TYPE && !always_perform_final_pass) {
            if (debug_on) {
                DebugPrint("Skipping PerformFinalPassOnInfoString. application_type=" + application_type + ", always_perform_final_pass=" + always_perform_final_pass);
            }
            return null;
        }
        // DebugPrint2("PerformFinalPassOnInfoString("+ info+")");
        String new_info_string = "";
        String temp_info_string = "";
        String infoToken = null;
        try {
            StringTokenizer infoTokenizer = new StringTokenizer(info, ";");
            arrayLengthTooLong = false;
            StringBuffer sb = new StringBuffer((infoTokenizer.countTokens() + 5) * 100);
            while (infoTokenizer.hasMoreTokens() && !arrayLengthTooLong) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                infoToken = infoTokenizer.nextToken();
                // DebugPrint2("infoToken="+infoToken);
                while (true) {
                    if (Thread.interrupted()) {
                        interrupt_loading = true;
                        throw new Exception("Thread.interrupted() returned true\n");
                    }
                    if (interrupt_loading) {
                        throw new Exception("interrupt_loading=true\n");
                    }
                    if (infoToken.length() < 1) {
                        break;
                    }
                    if (infoToken.charAt(0) != ' '
                            && infoToken.charAt(0) != '\t') {
                        break;
                    }
                    infoToken = infoToken.substring(1);
                }
                sb.append(SeparateArrayElements(infoToken));
                if (sb.length() > 800000) {
                    ErrorPrint("Class starting with " + sb.toString().substring(0, 20) + " is too long for diagnostics tool. (Truncating!)");
                    break;
                }
            }
            temp_info_string = sb.toString();
//            DebugPrint2("temp_info_string=" + temp_info_string);
            infoTokenizer = new StringTokenizer(temp_info_string, ";");
            sb = new StringBuffer((infoTokenizer.countTokens() + 5) * 100);
            while (infoTokenizer.hasMoreTokens()) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                infoToken = infoTokenizer.nextToken();
                // DebugPrint2("infoToken="+infoToken);
                if (debug_on) {
                    DebugPrint("PerformFinalPassOnInfoString : infoToken=" + infoToken);
                }
                int space_index = infoToken.indexOf(' ');
                if (space_index < 0) {
                    sb.append(infoToken);
                    sb.append(';');
                    continue;
                }
                String className = infoToken.substring(0, space_index);
                int last_space_index = space_index;
                boolean ndla_found = false;
                while (className.equals("class") || className.equals("struct")
                        || className.equals("NML_DYNAMIC_LENGTH_ARRAY")) {
                    if (className.equals("NML_DYNAMIC_LENGTH_ARRAY")) {
                        ndla_found = true;
                    }
                    space_index = infoToken.indexOf(' ', space_index + 1);
                    if (space_index < 0) {
                        sb.append(infoToken);
                        sb.append(';');
                        break;
                    }
                    className = infoToken.substring(last_space_index + 1, space_index);
                    last_space_index = space_index;
                }
                if (debug_on) {
                    DebugPrint("space_index=" + space_index + ", className=" + className);
                }
                if (space_index < 0) {
                    continue;
                }
                String classvarname = infoToken.substring(space_index + 1);
                if (m_structInfoByNameHashTable.containsKey(className)) {
                    // DebugPrint2("className="+className);
                    AddInternalStructure(className, classvarname, ndla_found, sb);
                    // DebugPrint2("stemp="+stemp);
                    //				 sb.append(stemp);
                    continue;
                }
                sb.append(infoToken);
                sb.append(';');
                if (sb.length() > 1000000) {
                    ErrorPrint("Class starting with " + sb.substring(0, 20) + " of length " + sb.length() + " is too long for diagnostics tool. (Truncating!)");
                    break;
                }
            }
            new_info_string = sb.toString();
            // DebugPrint2("PerformFinalPassOnInfoString("+ info+") : returning "+new_info_string);
            return new_info_string;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        return info;
    }

    private String PerformFirstPassOnInfoString(String info) throws Exception {
        String temp_info_string = "";
        String infoToken = null;
        try {
            StringTokenizer infoTokenizer = new StringTokenizer(info, ";");
            StringBuffer sb = new StringBuffer((infoTokenizer.countTokens() + 2) * 100);
            while (infoTokenizer.hasMoreTokens()) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                infoToken = infoTokenizer.nextToken();
                while (true) {
                    if (Thread.interrupted()) {
                        interrupt_loading = true;
                        throw new Exception("Thread.interrupted() returned true\n");
                    }
                    if (interrupt_loading) {
                        throw new Exception("interrupt_loading=true\n");
                    }
                    if (infoToken.length() < 1) {
                        break;
                    }
                    if (infoToken.charAt(0) != ' '
                            && infoToken.charAt(0) != '\t') {
                        break;
                    }
                    infoToken = infoToken.substring(1);
                }
                String possible_enum_name = infoToken;
                int comma_index = infoToken.indexOf(',');
                if (comma_index > 0) {
                    sb.append(SeparateCommaSeparatedVariables(infoToken));
                    continue;
                }
                int space_index = infoToken.indexOf(' ');
                if (space_index > 0) {
                    possible_enum_name = possible_enum_name.substring(0, space_index);
                }
                if (m_enumInfoHashTable.containsKey(possible_enum_name)) {
                    EnumTypeInfo ei = (EnumTypeInfo) m_enumInfoHashTable.get(possible_enum_name);
                    if (debug_on) {
                        DebugPrint("ei=" + ei);
                    }
                    if (null != ei && !ei.typedef) {
                        if (debug_on) {
                            DebugPrint("Adding \"enum \" to \"" + infoToken + "\",ei=" + ei);
                        }
                        infoToken = "enum " + infoToken;
                    }
                }
                sb.append(infoToken);
                sb.append(';');
            }
            temp_info_string = sb.toString();
            return temp_info_string;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        return info;
    }
    public static Hashtable<String, DefinedValue> startingDefinedValues = null;
    public Hashtable<String, DefinedValue> definedValues = null;
    String parseString;
    boolean typedef = false;
    boolean insideComment = false;
    boolean insideStruct = false;
    boolean insideEnum = false;
    boolean insideNameSpace = false;
    boolean lastInsideNameSpace = false;
    boolean nextBraceStartsEnum = false;
    boolean nextBraceStartsUnion = false;
    boolean nextBraceStartsNameSpace = false;
    boolean lastNextBraceStartsNameSpace = false;
    String currentNameSpace = "";
    boolean nextBraceStartsStruct = false;
    int default_enum_val = 0;
    StructureTypeInfo currentType = null;
    EnumTypeInfo current_enum = null;
    DefinedValue dv = null;
    int indexCppCommentBegin;
    int indexCCommentBegin;
    int indexCCommentEnd;
    int iflevel = 0;
    int trueIfLevel = 0;
    StringTokenizer infoTokenizer;
    String startingParseString = "";
    public static volatile String curFileName = null;
    public static volatile String curFileLineText = null;
    public static volatile int curFileLineNumber = 0;
    int lineNumber = 0;
    int paren_count = 0;
    int paren_index = 0;
    int brace_count = 0;
    int brace_index = 0;
    boolean inside_extern_c = false;
    boolean next_brace_starts_extern_c = false;
    boolean is_cmd_msg = false;
    boolean is_stat_msg = false;
    boolean paren_on_this_line = false;
    boolean brace_on_this_line = false;
    boolean included_file_ok = true;
    // @SuppressWarnings("unchecked")
    private int included_file_depth = 0;
    private static LinkedList<String> excludedIncludePatterns = null;

    private static void initExcludedIncludePatterns() {
        if (null == excludedIncludePatterns) {
            excludedIncludePatterns = new LinkedList<String>();
            excludedIncludePatterns.add("rcs.hh");
            excludedIncludePatterns.add("cmd_msg.hh");
            excludedIncludePatterns.add("stat_msg.hh");
            excludedIncludePatterns.add("cms.hh");
            excludedIncludePatterns.add("nml.hh");
            excludedIncludePatterns.add("nmlmsg.hh");
            excludedIncludePatterns.add("math.h");
            excludedIncludePatterns.add("stdio.h");
            excludedIncludePatterns.add("ctype.h");
            excludedIncludePatterns.add("float.h");
            excludedIncludePatterns.add("time.h");
            excludedIncludePatterns.add("stdlib.h");
            excludedIncludePatterns.add("string.h");
            excludedIncludePatterns.add("pthread.h");
            excludedIncludePatterns.add("semaphore.h");
            excludedIncludePatterns.add("termios.h");
            excludedIncludePatterns.add("vxWorks.h");
            excludedIncludePatterns.add("unistd.h");
            excludedIncludePatterns.add("varargs.h");
            excludedIncludePatterns.add("limits.h");
            excludedIncludePatterns.add("stdarg.h");
            excludedIncludePatterns.add("stdint.h");
            excludedIncludePatterns.add("stddef.h");
            excludedIncludePatterns.add("signal.h");
            excludedIncludePatterns.add("curses.h");
            excludedIncludePatterns.add("errno.h");
            excludedIncludePatterns.add("types.h");
            excludedIncludePatterns.add("windows.h");
            excludedIncludePatterns.add("expat.h");
            excludedIncludePatterns.add("pcap.h");
            excludedIncludePatterns.add("glib/gthread.h");
            excludedIncludePatterns.add("winsock.h");
            excludedIncludePatterns.add("timetracker.hh");
            excludedIncludePatterns.add("sys.*");
            excludedIncludePatterns.add("inet.*");
            excludedIncludePatterns.add("netinet.*");
            excludedIncludePatterns.add("arpa.*");
            excludedIncludePatterns.add("posemath.*");
            excludedIncludePatterns.add("rpc.*");
            excludedIncludePatterns.add(".*Lib.h");
        }
    }

    public static void addExcludedIncludePattern(String p) {
        System.out.println("addExcludedIncludePattern:  " + p);
        initExcludedIncludePatterns();
        excludedIncludePatterns.add(p);
    }

    public void CheckIncludedFile(String include_file) {
        try {
            initExcludedIncludePatterns();
            included_file_ok = true;
            if (debug_on) {
                DebugPrint("CheckIncludedFile(" + include_file + ")");
            }
            if (include_file == null) {
                return;
            }
            if (include_file.length() < 0) {
                return;
            }
            for (String p : excludedIncludePatterns) {
                if (include_file.matches(p)) {
                    if (debug_on) {
                        DebugPrint("CheckIncludedFile(" + include_file + ") : include file matches pattern \"" + p + "\" in excludedIncludePatterns list.");
                    }
                    return;
                }
            }
//	    if (include_file.equals("rcs.hh") ||
//		    include_file.equals("cmd_msg.hh") ||
//		    include_file.equals("stat_msg.hh") ||
//		    include_file.equals("cms.hh") ||
//		    include_file.equals("nml.hh") ||
//		    include_file.equals("nmlmsg.hh") ||
//		    include_file.equals("math.h") ||
//		    include_file.equals("stdio.h") ||
//		    include_file.equals("ctype.h") ||
//		    include_file.equals("float.h") ||
//		    include_file.equals("time.h") ||
//		    include_file.equals("stdlib.h") ||
//		    include_file.equals("string.h") ||
//		    include_file.equals("vxWorks.h") ||
//		    include_file.equals("unistd.h") ||
//		    include_file.equals("varargs.h") ||
//		    include_file.equals("limits.h") ||
//		    include_file.equals("stdarg.h") ||
//		    include_file.equals("stddef.h") ||
//		    include_file.equals("signal.h") ||
//		    include_file.equals("curses.h") ||
//		    include_file.equals("errno.h") ||
//		    include_file.equals("types.h") ||
//		    include_file.equals("windows.h") ||
//		    include_file.equals("winsock.h") ||
//		    include_file.equals("timetracker.hh") ||
//		    include_file.startsWith("sys") ||
//		    include_file.startsWith("inet") ||
//		    include_file.startsWith("netinet") ||
//		    include_file.startsWith("arpa") ||
//		    include_file.startsWith("posemath") ||
//		    include_file.startsWith("rpc") ||
//		    include_file.endsWith("Lib.h")) {
//		return;
//	    }
            if (null != ignored_header_list) {
                for (int i = 0; i < ignored_header_list.size(); i++) {
                    String ignored_header = (String) ignored_header_list.elementAt(i);
                    if (include_file.equals(ignored_header)) {
                        return;
                    }
                }
            }
            //System.err.println("m_loadedPreDefinedTypes.containsKey("+include_file+")="+m_loadedPreDefinedTypes.containsKey(include_file));
//	    if(m_loadedPreDefinedTypes != null)
//            {
//                Enumeration keys = m_loadedPreDefinedTypes.keys();
//                while(keys.hasMoreElements())
//                {
//                    System.err.println("\tm_loadedPreDefinedTypes.keys()=" +keys.nextElement());    
//                }
//            }
            if (m_loadedPreDefinedTypes.containsKey(include_file)) {
                Hashtable ht = (Hashtable) m_loadedPreDefinedTypes.get(include_file);
                if (ht != null) {
                    if (null != definedValues) {
                        definedValues.putAll(ht);
                    } else {
                        definedValues = new Hashtable(ht);
                    }
                    recheck_defined_values();
                }
                return;
            }
            if (debug_on) {
                DebugPrint("");
                DebugPrint("Notice: Loading " + include_file + " . . . ");
            }
            int origLineNumber = lineNumber;
            String origFileLineText = curFileLineText;
            String orig_file = current_file;
            lineNumber = 0;
//	    curFileLineNumber=lineNumber;
//	    curFileName=include_file;
            included_file_depth++;
            try {
                LoadPredefinedTypeFile(include_file);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                included_file_depth--;
            }
            current_file = orig_file;
            lineNumber = origLineNumber;
            curFileLineNumber = lineNumber;
            curFileLineText = origFileLineText;
            last_loading_module = this;
            curFileName = orig_file;
            currentType = new StructureTypeInfo();
            currentType.first_module_used_in = this;
            currentType.fromFileName = orig_file;
            currentType.fromLineNumber = lineNumber;
            currentType.fromLineText = curFileLineText;
            if (debug_on) {
                DebugPrint("");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private boolean HandlePreProcessing() throws Exception {
        try {
            if (null == definedValues) {
                definedValues = new Hashtable();
            }
            if (!definedValues.containsKey("JAVA_DIAG_APPLET")) {
                AddDefaultDefinedValues();
            }
            if (null == parseString) {
                return false;
            }
            if (parseString.length() < 1) {
                return false;
            }
            while (parseString.length() > 2
                    && parseString.charAt(0) == '#'
                    && (parseString.charAt(1) == '\t' || parseString.charAt(1) == ' ')) {
                parseString = "#" + parseString.substring(2);
            }
            if (parseString.startsWith("#ifdef ")) {
                if (parseString.length() > 7) {
                    String temp = parseString.substring(7);
                    if (null == temp) {
                        return false;
                    }
                    StringTokenizer tempTokenizer = new StringTokenizer(temp, " \t\n\r\b");
                    if (tempTokenizer.hasMoreTokens()) {
                        String name = tempTokenizer.nextToken();
                        if (name != null) {
                            if (definedValues.containsKey(name) && trueIfLevel == iflevel) {
                                trueIfLevel++;
                            }
                            iflevel++;
                        }
                    }
                }
                return false;
            }
            if (parseString.startsWith("#ifndef ")) {
                if (parseString.length() > 8) {
                    String temp = parseString.substring(8);
                    if (null == temp) {
                        return false;
                    }
                    StringTokenizer tempTokenizer = new StringTokenizer(temp, " \t\n\r\b");
                    if (tempTokenizer.hasMoreTokens()) {
                        String name = tempTokenizer.nextToken();
                        if (name != null) {
                            if (!definedValues.containsKey(name) && trueIfLevel == iflevel) {
                                trueIfLevel++;
                            }
                            iflevel++;
                        }
                    }
                }
                return false;
            }
            if (parseString.startsWith("#endif")) {
                if (iflevel == trueIfLevel) {
                    trueIfLevel--;
                }
                iflevel--;
                return false;
            }
            if (parseString.startsWith("#else")) {
                if (iflevel == trueIfLevel + 1) {
                    trueIfLevel++;
                } else if (iflevel == trueIfLevel) {
                    trueIfLevel--;
                }
                return false;
            }
            if (parseString.startsWith("#if")) {
                if (definedValues.containsKey("JAVA_DIAG_APPLET_FORCE_TRUE") && trueIfLevel == iflevel) {
                    trueIfLevel++;
                }
                iflevel++;
                return false;
            }

            if (iflevel != trueIfLevel) {
                return false;
            }
            while (true) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                if (parseString.charAt(0) != ' ') {
                    break;
                }
                parseString = parseString.substring(1);
                if (parseString.length() < 1) {
                    break;
                }
            }
            if (parseString.regionMatches(true, 0, "END", 0, 3)) {
                return false;
            }
            if (parseString.regionMatches(true, 0, "#error", 0, 3)) {
                ErrorPrint(parseString);
                return false;
            }
            if (parseString.regionMatches(false, 0, "\n", 0, 1)) {
                return false;
            }
            if (parseString.regionMatches(false, 0, "\r", 0, 1)) {
                return false;
            }
            if (!insideComment && (parseString.startsWith("#include")
                    || parseString.startsWith("# include"))) {
                int q1 = parseString.indexOf('"');
                if (q1 > 0) {
                    int q2 = parseString.indexOf('"', q1 + 1);
                    if (q2 > q1) {
                        String include_file = parseString.substring(q1 + 1, q2);
                        CheckIncludedFile(include_file);
                    }
                } else {
                    q1 = parseString.indexOf('<');
                    if (q1 > 0) {
                        int q2 = parseString.indexOf('>', q1 + 1);
                        if (q2 > q1) {
                            String include_file = parseString.substring(q1 + 1, q2);
                            CheckIncludedFile(include_file);
                        }
                    }
                }
                if (!included_file_ok) {
                    ErrorPrint(current_file + ":" + lineNumber + ": ERROR bad include.");
                }
                included_file_ok = true;
                return false;
            }
            return true;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
            return false;
        }
    }

    private boolean CheckForFormatFunction() throws Exception {
        try {
            if (parseString.toUpperCase().indexOf("FORMAT") > 0 || parseString.toUpperCase().indexOf("FMT") > 0) {
                if (debug_on) {
                    DebugPrint("possible format function on this line = " + parseString);
                }
                int parenindex = parseString.indexOf('(');
                if (!insideEnum && !insideStruct && !insideComment
                        && parenindex > 0
                        && parseString.indexOf("CMS") > 0) {
                    parseString = parseString.substring(0, parenindex);
                    if (debug_on) {
                        DebugPrint("parseString = " + parseString);
                    }
                    StringTokenizer tkz = new StringTokenizer(parseString, " \t");
                    String tk = null;
                    while (tkz.hasMoreTokens()) {
                        tk = tkz.nextToken();
                    }
                    FormatFunction = tk;
                    if (debug_on) {
                        DebugPrint("FormatFunction = " + FormatFunction);
                    }
                    return false;
                }
            }
            return true;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        return false;
    }

    private boolean CheckForSymbolLookup() throws Exception {
        try {
            if (insideEnum) {
                return true;
            }
            if (insideStruct) {
                return true;
            }
            if (insideComment) {
                return true;
            }
            if (parseString.indexOf("symbol_lookup") > 0) {
                if (debug_on) {
                    DebugPrint("possible symbol lookup on this line = " + parseString);
                }
                int parenindex = parseString.indexOf('(');
                if (parenindex <= 0) {
                    return true;
                }
                if (parseString.indexOf("long") >= 0 || parseString.indexOf("NMLTYPE") >= 0) {
                    parseString = parseString.substring(0, parenindex);
                    if (debug_on) {
                        DebugPrint("parseString = " + parseString);
                    }
                    StringTokenizer tkz = new StringTokenizer(parseString, " \t");
                    String tk = null;
                    boolean char_found = false;
                    while (tkz.hasMoreTokens()) {
                        tk = tkz.nextToken();
                        if (tk.indexOf("char") >= 0 && tkz.hasMoreTokens()) {
                            char_found = true;
                        }
                    }
                    tk = tk.trim();
                    while (tk.length() > 0 && tk.startsWith("*")) {
                        tk = tk.substring(1);
                    }
                    if (tk.endsWith("symbol_lookup")
                            && char_found) {
                        if (SymbolLookup == null
                                && tk.indexOf("_enum_") < 0
                                && included_file_depth == 0) {
                            SymbolLookup = tk;
                        }
                        CodeGenCommon.set_generate_symbol_lookups(true);
                        return false;
                    }
                    if (debug_on) {
                        DebugPrint("SymbolLookup = " + SymbolLookup);
                    }
                    return true;
                } else if (parseString.indexOf("(int") >= 0 && parseString.indexOf("_enum_") < 0) {
                    ErrorPrint(parseString + " appears to be a symbol lookup with int argument rather than long.");
                }
            }
            return true;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
        return true;
    }

    private boolean HandleComments() throws Exception {
        try {
            indexCCommentBegin = parseString.indexOf("/*");
            if (indexCCommentBegin > 0 && parseString.charAt(indexCCommentBegin - 1) == '/') {
                indexCCommentBegin = parseString.indexOf("/*", indexCCommentBegin + 1);
            }
            while (indexCCommentBegin != -1 || insideComment) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                String tempString;
                indexCCommentEnd = parseString.indexOf("*/");
                indexCCommentBegin = parseString.indexOf("/*");
                if (indexCCommentBegin > 0 && parseString.charAt(indexCCommentBegin - 1) == '/') {
                    indexCCommentBegin = parseString.indexOf("/*", indexCCommentBegin + 1);
                }
                if (indexCCommentEnd != -1) {
                    if (indexCCommentBegin > 0 && indexCCommentBegin < indexCCommentEnd) {
                        tempString = parseString.substring(0, indexCCommentBegin);
                    } else {
                        tempString = "";
                    }
                    String commentString = "";
                    if (indexCCommentBegin >= 0 && indexCCommentBegin < indexCCommentEnd) {
                        commentString = parseString.substring(indexCCommentBegin, indexCCommentEnd + 2);
                    } else {
                        commentString = parseString.substring(0, indexCCommentEnd + 2);
                    }
                    if (commentString.indexOf("generate_symbol_lookups=true") >= 0) {
                        CodeGenCommon.set_generate_symbol_lookups(true);
                    } else if (commentString.indexOf("generate_symbol_lookups=false") >= 0) {
                        CodeGenCommon.set_generate_symbol_lookups(false);
                    }
                    if (commentString.indexOf("generate_all_enum_symbol_lookups=true") >= 0) {
                        CodeGenCommon.set_generate_all_enum_symbol_lookups(true);
                    } else if (commentString.indexOf("generate_all_enum_symbol_lookups=false") >= 0) {
                        CodeGenCommon.set_generate_all_enum_symbol_lookups(false);
                    }
                    if (commentString.indexOf("add_set_header=true") >= 0) {
                        CodeGenCommon.set_add_set_header(true);
                    } else if (commentString.indexOf("add_set_header=false") >= 0) {
                        CodeGenCommon.set_add_set_header(false);
                    }
                    if (commentString.indexOf("generate_enum_symbol_lookup=true") >= 0) {
                        generate_enum_symbol_lookup = true;
                    } else if (commentString.indexOf("generate_enum_symbol_lookup=false") >= 0) {
                        generate_enum_symbol_lookup = false;
                    }
                    if (commentString.indexOf("CodeGenCommand=") >= 0) {
                        CodeGenCommon.RunLineOfScriptStatic(commentString.substring(commentString.indexOf("CodeGenCommand=")));
                    }
                    int defindex = commentString.indexOf("default=");
                    if (defindex > 0 && lineNumber > 0) {
                        String defvalue = commentString.substring(defindex + 8);
                        int dvendindex = defvalue.indexOf(' ');
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        dvendindex = defvalue.indexOf('\t');
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        dvendindex = defvalue.indexOf(',');
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        dvendindex = defvalue.indexOf('\r');
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        dvendindex = defvalue.indexOf('\n');
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        dvendindex = defvalue.indexOf("*/");
                        if (dvendindex > 0) {
                            defvalue = defvalue.substring(0, dvendindex);
                        }
                        currentDefaultValue = defvalue;
                        if (debug_on) {
                            DebugPrint("currentDefaultValue=" + currentDefaultValue);
                        }
                    }
                    int attributeindex = commentString.indexOf("attribute");
                    if (attributeindex > 0 && lineNumber > 0) {
                        currentAttributeInfo = commentString;
                        if (debug_on) {
                            DebugPrint("currentAttributeInfo=" + currentAttributeInfo);
                        }
                    }
                    int nameindex = commentString.indexOf("name=");
                    if (nameindex > 0 && lineNumber > 0) {
                        String overridename = commentString.substring(nameindex + 5);
                        int onendindex = overridename.indexOf(' ');
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        onendindex = overridename.indexOf('\t');
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        onendindex = overridename.indexOf(',');
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        onendindex = overridename.indexOf('\r');
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        onendindex = overridename.indexOf('\n');
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        onendindex = overridename.indexOf("*/");
                        if (onendindex > 0) {
                            overridename = overridename.substring(0, onendindex);
                        }
                        currentOverrideName = overridename;
                        if (debug_on) {
                            DebugPrint("currentOverrideName=" + currentOverrideName);
                        }
                    }
                    if (debug_on && commentString.length() > 1) {
                        DebugPrint("Remove C Comment " + commentString);
                    }
                    parseString = tempString + parseString.substring(indexCCommentEnd + 2);
                    if (debug_on) {
                        DebugPrint("parseString = " + parseString);
                    }
                    insideComment = false;
                } else {
                    if (indexCCommentBegin > 0 && !insideComment) {
                        if (debug_on) {
                            DebugPrint("Remove C Comment " + parseString.substring(indexCCommentBegin));
                        }
                        parseString = parseString.substring(0, indexCCommentBegin);
                        if (debug_on) {
                            DebugPrint("parseString = " + parseString);
                        }
                    } else {
                        parseString = "";
                    }
                    insideComment = true;
                    break;
                }
                indexCCommentBegin = parseString.indexOf("/*");
            }
            if (parseString.length() < 1) {
                return false;
            }
            indexCppCommentBegin = parseString.indexOf("//");
            if (indexCppCommentBegin != -1) {
                String commentString = parseString.substring(indexCppCommentBegin);

                if (commentString.indexOf("generate_all_enum_symbol_lookups=true") >= 0) {
                    CodeGenCommon.set_generate_all_enum_symbol_lookups(true);
                } else if (commentString.indexOf("generate_all_enum_symbol_lookups=false") >= 0) {
                    CodeGenCommon.set_generate_all_enum_symbol_lookups(false);
                }

                if (commentString.indexOf("add_set_header=true") >= 0) {
                    CodeGenCommon.set_add_set_header(true);
                } else if (commentString.indexOf("add_set_header=false") >= 0) {
                    CodeGenCommon.set_add_set_header(false);
                }

                if (commentString.indexOf("generate_symbol_lookups=true") >= 0) {
                    CodeGenCommon.set_generate_symbol_lookups(true);
                } else if (commentString.indexOf("generate_symbol_lookups=false") >= 0) {
                    CodeGenCommon.set_generate_symbol_lookups(false);
                }
                if (commentString.indexOf("generate_enum_symbol_lookup=true") >= 0) {
                    generate_enum_symbol_lookup = true;
                } else if (commentString.indexOf("generate_enum_symbol_lookup=false") >= 0) {
                    generate_enum_symbol_lookup = false;
                }
                if (commentString.indexOf("CodeGenCommand=") >= 0) {
                    CodeGenCommon.RunLineOfScriptStatic(commentString.substring(commentString.indexOf("CodeGenCommand=")));
                }
                if (indexCppCommentBegin == 0) {
                    return false;
                }
                int defindex = commentString.indexOf("default=");
                if (defindex > 0 && lineNumber > 0) {
                    String defvalue = commentString.substring(defindex + 8);
                    int dvendindex = defvalue.indexOf(' ');
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    dvendindex = defvalue.indexOf('\t');
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    dvendindex = defvalue.indexOf(',');
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    dvendindex = defvalue.indexOf('\r');
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    dvendindex = defvalue.indexOf('\n');
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    dvendindex = defvalue.indexOf("*/");
                    if (dvendindex > 0) {
                        defvalue = defvalue.substring(0, dvendindex);
                    }
                    currentDefaultValue = defvalue;
                    if (debug_on) {
                        DebugPrint2("currentDefaultValue=" + currentDefaultValue);
                    }
                }
                int attributeindex = commentString.indexOf("attribute");
                if (attributeindex > 0 && lineNumber > 0) {
                    currentAttributeInfo = commentString;
                    if (debug_on) {
                        DebugPrint("currentAttributeInfo=" + currentAttributeInfo);
                    }
                }
                int nameindex = commentString.indexOf("name=");
                if (nameindex > 0 && lineNumber > 0) {
                    String overridename = commentString.substring(nameindex + 5);
                    int onendindex = overridename.indexOf(' ');
                    if (onendindex > 0) {
                        overridename = overridename.substring(0, onendindex);
                    }
                    onendindex = overridename.indexOf('\t');
                    if (onendindex > 0) {
                        overridename = overridename.substring(0, onendindex);
                    }
                    onendindex = overridename.indexOf('\r');
                    if (onendindex > 0) {
                        overridename = overridename.substring(0, onendindex);
                    }
                    onendindex = overridename.indexOf('\n');
                    if (onendindex > 0) {
                        overridename = overridename.substring(0, onendindex);
                    }
                    onendindex = overridename.indexOf("*/");
                    if (onendindex > 0) {
                        overridename = overridename.substring(0, onendindex);
                    }
                    currentOverrideName = overridename;
                    if (debug_on) {
                        DebugPrint("currentOverrideName=" + currentOverrideName);
                    }
                }
                if (debug_on) {
                    DebugPrint("Remove C++ Comment " + parseString.substring(indexCppCommentBegin));
                }
                parseString = parseString.substring(0, indexCppCommentBegin);
                if (debug_on) {
                    DebugPrint("parseString = " + parseString);
                }
            }
            int att_start_index = parseString.indexOf("__attribute__((");
            if (att_start_index >= 0) {
                int att_end_index = parseString.indexOf("))", att_start_index);
                if (att_end_index > att_start_index) {
                    if (att_end_index < parseString.length() - 3) {
                        parseString = parseString.substring(0, att_start_index) + parseString.substring(att_end_index + 2);
                    } else {
                        parseString = parseString.substring(0, att_start_index);
                    }
                }
            }
            return true;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
            return false;
        }
    }

    public String RemoveStartingEndingSpace(String str) {
        try {
            while (str.length() > 0
                    && (str.startsWith(" ") || str.startsWith("\t"))) {
                str = str.substring(1);
            }
            while (str.length() > 0
                    && (str.endsWith(" ") || str.endsWith("\t"))) {
                str = str.substring(0, str.length() - 1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return str;
    }
    public static boolean codegen_select_from_all_files = false;
    public static boolean codegen_generate_format_only = false;
    // @SuppressWarnings("unchecked")

    private boolean HandleDefines() throws Exception {
        try {
            if (parseString.startsWith("#define") && parseString.length() > 7) {
                String temp = parseString.substring(7);
                if (null == temp) {
                    return false;
                }
                StringTokenizer tempTokenizer = new StringTokenizer(temp, " \t\n\r\b");
                if (tempTokenizer.hasMoreTokens()) {
                    dv = new DefinedValue();
                    dv.name = tempTokenizer.nextToken();
//		    DebugPrint2("dv.name="+dv.name);
                    if (dv.name.compareTo("CODEGEN_SELECT_FROM_ALL_FILES") == 0) {
                        codegen_select_from_all_files = true;
//			DebugPrint2("codegen_select_from_all_files="+codegen_select_from_all_files);
                        return false;
                    }
                    if (dv.name.compareTo("CODEGEN_GENERATE_FORMAT_ONLY") == 0) {
                        codegen_generate_format_only = true;
                        return false;
                    }

                    if (tempTokenizer.hasMoreTokens()) {
                        dv.value = tempTokenizer.nextToken();
                        while (tempTokenizer.hasMoreTokens()) {
                            if (Thread.interrupted()) {
                                interrupt_loading = true;
                                throw new Exception("Thread.interrupted() returned true\n");
                            }
                            if (interrupt_loading) {
                                throw new Exception("interrupt_loading=true\n");
                            }
                            dv.value += " " + tempTokenizer.nextToken();
                        }
                    } else {
                        dv.value = "";
                    }
                    if (dv.name.compareTo("CODEGEN_COMMAND") == 0) {
                        CodeGenCommon.RunLineOfScriptStatic(dv.value);
                        return false;
                    }
                    dv.value = ReplaceDefinedValues(dv.value, 0, null);
                    dv.value = RemoveStartingEndingSpace(dv.value);
                    if (!definedValues.containsKey(dv.name)) {
                        dv.tag = cur_tag();
                        definedValues.put(dv.name, dv);
                    }
                    if (debug_on) {
                        DebugPrint("Adding defined value " + dv.name + " = " + dv.value);
                    }
                }
                return false;
            }
            if (parseString.startsWith("#undef") && parseString.length() > 7) {
                if (parseString.length() > 7) {
                    String temp = parseString.substring(7);
                    if (null == temp) {
                        return false;
                    }
                    StringTokenizer tempTokenizer = new StringTokenizer(temp, " \t\n\r\b");
                    if (tempTokenizer.hasMoreTokens()) {
                        String name = tempTokenizer.nextToken();
                        if (null != name) {
                            definedValues.remove(name);
                        }
                    }
                }
                return false;
            }
            return true;
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
            return false;
        }
    }
    static public int anonymous_enum_number = 0;
    boolean structenum_on_this_line = false;

    String DefaultTypeIdString(String name) {
        if (definedValues.containsKey(name + "_TYPE")) {
            return name + "_TYPE";
        } else if (definedValues.containsKey(name + "Type")) {
            return name + "Type";
        }
        return name.toUpperCase() + "_TYPE";
    }

    String RemoveUnnecessarySpace(String inStr) {
        StringTokenizer st = new StringTokenizer(inStr, " \t");
        char lastChar = ';';
        String otherbreakers = " ;:()+-/*,\'";
        String outStr = "";
        if (inStr.indexOf('[') < 0 && inStr.indexOf(']') < 0) {
            otherbreakers = " ;:()+-/,\'";
        }
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            if (otherbreakers.indexOf(lastChar) < 0
                    && otherbreakers.indexOf(token.charAt(0)) < 0) {
                outStr += " " + token;
            } else {
                outStr += token;
            }
            lastChar = token.charAt(token.length() - 1);
        }
        return outStr;
    }
    String current_file = "";
    static private Vector cmdLineDefaultValues = null;

    public static void AddDCmdOption(String arg) {
        if (arg.startsWith("-D")) {
            arg = arg.substring(2);
        }
        int eq_index = arg.indexOf('=');
        if (eq_index > 0) {
            String name = arg.substring(0, eq_index);
            String value = arg.substring(eq_index + 1);
            AddCmdLineDefaultValue(name, value);
        }
    }

    public static void AddCmdLineDefaultValue(final String name, final String value) {
        DefinedValue dv = new DefinedValue();
        dv.tag = "CmdLine";
        dv.name = name;
        dv.value = value;
        if (cmdLineDefaultValues == null) {
            cmdLineDefaultValues = new Vector();
        }
        cmdLineDefaultValues.add(dv);
    }

    private void AddDefaultDefinedValues() {
        if (null == definedValues) {
            definedValues = new Hashtable();
        }
        if (null != cmdLineDefaultValues) {
            for (int i = 0; i < cmdLineDefaultValues.size(); i++) {
                DefinedValue dv = (DefinedValue) cmdLineDefaultValues.elementAt(i);
                definedValues.put(dv.name, dv);
            }
        }
        DefinedValue dv = new DefinedValue();
        dv.tag = cur_tag();
        dv.name = "JAVA_DIAG_APPLET";
        dv.value = "1";
        definedValues.put(dv.name, dv);

        dv.tag = cur_tag();
        dv.name = "RCS_HAVE_STAT_MSG_V2_SYMBOL_LOOKUP";
        dv.value = "1";
        definedValues.put(dv.name, dv);

        dv.tag = cur_tag();
        dv.name = "JAVA_CODEGEN";
        dv.value = "1";
        definedValues.put(dv.name, dv);

        dv = new DefinedValue();
        dv.tag = cur_tag();
        dv.name = "NMLTYPE";
        dv.value = "long";
        definedValues.put(dv.name, dv);

        dv = new DefinedValue();
        dv.tag = cur_tag();
        dv.name = "__CPLUSPLUS__";
        dv.value = "1";
        definedValues.put(dv.name, dv);

        if (diagapplet.CodeGen.StringFuncs.getenv("TIMETRACKER_IN_RCS") != null) {
            dv = new DefinedValue();
            dv.tag = cur_tag();
            dv.name = "TIMETRACKER_IN_RCS";
            dv.value = "1";
            definedValues.put(dv.name, dv);
        }

        dv = new DefinedValue();
        dv.tag = cur_tag();
        dv.name = "__cplusplus";
        dv.value = "1";
        definedValues.put(dv.name, dv);

        dv = new DefinedValue();
        dv.tag = cur_tag();
    }

    private void AddCppQualifiedType(StructureTypeInfo currentType) {
        if (currentType.CppQualifiedName.length() < 0) {
            ErrorPrint("Attempt to add empty type name.");
        } else if (currentType.CppQualifiedName.equals("extern")) {
            ErrorPrint("Attempt to add extern type name.");
        } else if (m_structInfoByNameHashTable.containsKey(currentType.CppQualifiedName)) {
            StructureTypeInfo prevSti = (StructureTypeInfo) m_structInfoByNameHashTable.get(currentType.CppQualifiedName);
            ErrorPrint("Attempt to add type already on the hashtable. (" + currentType.CppQualifiedName + " from " + prevSti.fromFileName + ":" + prevSti.fromLineNumber + ")");
        } else {
            m_structInfoByNameHashTable.put(currentType.CppQualifiedName, currentType);
        }
    }
    // @SuppressWarnings("unchecked")

    private void CheckIfShouldAddAvailableCommand(boolean is_cmd_stream) throws Exception {
        if (currentType.Id <= 0) {
            return;
        }
        if (!is_cmd_msg) {
            return;
        }
        if (!(is_cmd_stream
                || (currentType.fromFileName != null && cmdsTypeFile != null
                && (currentType.fromFileName.equals(cmdsTypeFile) || currentType.fromFileName.equals(baseClassCmdsTypeFile))))) {
            return;
        }
        if (null != cmd_name_pattern && !currentType.getName().matches(cmd_name_pattern)) {
            return;
        }
        if (null != cmd_name_exclude_pattern && currentType.getName().matches(cmd_name_exclude_pattern)) {
            return;
        }
        cmdsAvailable.addElement(currentType.getName() + "=" + currentType.Id);
        try {
            if (cmdsAvailable.size() < 2) {
                common_cmd_base = null;
            } else if (cmdsAvailable.size() == 2) {
                String s1 = (String) cmdsAvailable.elementAt(0);
                String s2 = (String) cmdsAvailable.elementAt(1);
                String c1 = s1;
                String c2 = s2;
                if (s1.indexOf('=') > 0) {
                    c1 = s1.substring(0, s1.indexOf('='));
                }
                if (s2.indexOf('=') > 0) {
                    c1 = s1.substring(0, s1.indexOf('='));
                }
                for (int i = 1; i < c1.length() && i < c2.length(); i++) {
                    if (debug_on) {
                        DebugPrint("c1=" + c1 + ", c2=" + c2 + ", common_cmd_base=" + common_cmd_base);
                    }
                    if (c1.substring(0, i).equals(c2.substring(0, i))) {
                        common_cmd_base = c1.substring(0, i);
                    } else {
                        break;
                    }
                }
                if (null != common_cmd_base
                        && common_cmd_base.length() < 2) {
                    common_cmd_base = null;
                }
            } else if (null != common_cmd_base) {
                String c2 = currentType.getName();
                if (c2 == null
                        || c2.length() < 2) {
                    common_cmd_base = null;
                } else {
                    if (c2.length() < common_cmd_base.length()) {
                        common_cmd_base = common_cmd_base.substring(0, c2.length() - 1);
                    }
                    for (int i = common_cmd_base.length(); i > 0; i--) {
                        common_cmd_base = common_cmd_base.substring(0, i);
                        if (debug_on) {
                            DebugPrint("common_cmd_base=" + common_cmd_base + ", c2=" + c2);
                        }
                        if (c2.substring(0, i).equals(common_cmd_base)) {
                            break;
                        } else {
                            if (i < 2) {
                                common_cmd_base = null;
                                break;
                            }
                        }
                    }
                    if (null != common_cmd_base
                            && common_cmd_base.length() < 2) {
                        common_cmd_base = null;
                    }
                }
            }
        } catch (Exception e) {
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        }
    }

    private void ParseInfoStream(URL_and_FileLoader file_loader, boolean is_cmd_stream) throws Exception {
        parseString = "";
        typedef = false;
        last_file_loader = file_loader;
        if (debug_on) {
            new Throwable().printStackTrace(System.out);
            DebugPrint("currentThread=" + Thread.currentThread());
            DebugPrint("currentThread.getName()=" + Thread.currentThread().getName());
            DebugPrint("typedef=" + typedef);
            DebugPrint("is_cmd_stream=" + is_cmd_stream);
            DebugPrint("file_loader=" + file_loader);
            DebugPrint("file_loader.name=" + file_loader.name);
        }
        this.lhui_name = file_loader.name;
        this.lhui_total = file_loader.content_length;
        this.lhui_part = curFileLineNumber;
        this.lhui_update();
        insideComment = false;
        insideStruct = false;
        insideEnum = false;
        nextBraceStartsUnion = false;
        nextBraceStartsEnum = false;
        nextBraceStartsNameSpace = false;
        nextBraceStartsStruct = false;
        default_enum_val = 0;
        currentType = null;
        current_enum = null;
        dv = null;
        indexCppCommentBegin = -1;
        indexCCommentBegin = -1;
        indexCCommentEnd = -1;
        iflevel = 0;
        trueIfLevel = 0;
        infoTokenizer = null;
        startingParseString = "";
        lineNumber = 0;
        curFileLineNumber = lineNumber;
        curFileLineText = "";
        paren_count = 0;
        paren_index = 0;
        brace_count = 0;
        brace_index = 0;
        is_cmd_msg = false;
        is_stat_msg = false;
        paren_on_this_line = false;
        brace_on_this_line = false;
        structenum_on_this_line = false;

        try {
            if (debug_on) {
                DebugPrint("ModuleInfo.ParseInfoStream(" + file_loader + ", " + is_cmd_stream + ")");
            }
            currentType = new StructureTypeInfo();
            currentType.first_module_used_in = this;
            currentType.fromFileName = file_loader.name;
            currentType.fromLineNumber = lineNumber;
            current_file = file_loader.name;
            last_loading_module = this;
            curFileName = current_file;
            current_enum = new EnumTypeInfo();
            current_enum.generate_symbol_lookup = generate_enum_symbol_lookup;
            dv = new DefinedValue();
            if (null != startingDefinedValues && null == definedValues) {
                try {
                    definedValues = new Hashtable<>(startingDefinedValues);
                    recheck_defined_values();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            if (null == definedValues) {
                definedValues = new Hashtable();
            }
            if (!definedValues.containsKey("JAVA_DIAG_APPLET")) {
                AddDefaultDefinedValues();
            }
            boolean first_line_read = false;
            currentOverrideName = null;
            currentDefaultValue = null;
            int main_parse_loop_count = 0;
            String tempInfoString = "";
            String infoToken;

            MAIN_PARSE_LOOP:
            while (true) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                try {
                    if (debug_on) {
                        main_parse_loop_count++;
                        DebugPrint("main_parse_loop_count=" + main_parse_loop_count);
                    }
                    if (typedef
                            && !this.nextBraceStartsStruct
                            && brace_count < 1
                            && currentType != null
                            && currentType.getName() != null
                            && currentType.getName().length() > 1) {
                        insideStruct = false;
                        if (debug_on) {
                            DebugPrint("insideStruct=" + insideStruct);
                        }
                        if (insideNameSpace) {
                            currentType.inside_namespace = true;
                            currentType.NameSpace = new String(currentNameSpace);
                            currentType.CppQualifiedName = currentType.NameSpace + "::" + currentType.getName();
                        } else {
                            currentType.CppQualifiedName = currentType.getName();
                        }
                        if (null != currentType.RawInfo
                                && (null == currentType.PreFinalPassInfo || currentType.PreFinalPassInfo.length() < 1)) {
                            String info_with_unneccesary_space_removed = RemoveUnnecessarySpace(currentType.RawInfo);
                            if (debug_on) {
                                DebugPrint("info_with_unneccesary_space_removed=" + info_with_unneccesary_space_removed);
                            }
                            currentType.HiddenInfo = new String(info_with_unneccesary_space_removed);
                            infoTokenizer = new StringTokenizer(currentType.HiddenInfo, " \t\r\n");
                            tempInfoString = "";
                            while (infoTokenizer.hasMoreTokens()) {
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                                infoToken = infoTokenizer.nextToken();
                                if (null == infoToken) {
                                    break;
                                }
                                if (debug_on) {
                                    DebugPrint("infoToken = " + infoToken);
                                }
                                if (infoToken.length() < 1) {
                                    continue;
                                }

                                if (definedValues.containsKey(infoToken)) {
                                    dv = (DefinedValue) definedValues.get(infoToken);
                                    if (dv != null) {
                                        if (dv.value.length() > 0) {
                                            if (debug_on) {
                                                DebugPrint("infoToken = " + infoToken + " replaced by DefinedValue =" + dv.value);
                                            }
                                            infoToken = dv.value;
                                        }
                                        currentType.usedDefinedValues.add(dv);
                                    }
                                }
                                int lSquareParamIndex = infoToken.indexOf('[');
                                int rSquareParamIndex = infoToken.indexOf(']');
                                while (lSquareParamIndex >= 0 && rSquareParamIndex > lSquareParamIndex) {
                                    String checkString = infoToken.substring(lSquareParamIndex + 1, rSquareParamIndex);
                                    checkString = ReplaceDefinedValues(checkString, 0, null);
                                    int array_length = doArrayLengthMath(checkString);
                                    if (array_length < 1) {
                                        ErrorPrint(file_loader.name + ":" + lineNumber + ": Bad array length(" + array_length + ") checkString=\"" + checkString + "\",infoToken=\"" + infoToken + "\"");
                                        array_length = 1;
                                    }
                                    String array_length_string = Integer.toString(array_length);
                                    infoToken = infoToken.substring(0, lSquareParamIndex) + "[" + array_length_string + infoToken.substring(rSquareParamIndex);
                                    lSquareParamIndex = infoToken.indexOf('[', lSquareParamIndex + array_length_string.length() + 2);
                                    if (lSquareParamIndex > 0) {
                                        rSquareParamIndex = infoToken.indexOf(']', lSquareParamIndex + 1);
                                    }
                                }
                                if (infoToken.length() > 0) {
                                    if (tempInfoString.length() > 1 && !infoToken.startsWith(";") && !infoToken.startsWith("[")) {
                                        tempInfoString += " ";
                                    }
                                    tempInfoString += infoToken;
                                }
                            }
                            if (debug_on) {
                                DebugPrint(" First pass data (DefinedValues substituted). = " + tempInfoString);
                            }
                            currentType.StepTwoInfo = tempInfoString;
                            currentType.PreFinalPassInfo = PerformFirstPassOnInfoString(tempInfoString);
                            if (debug_on) {
                                DebugPrint(" Second pass data (Extra White space eliminated and Enums identified.) = " + currentType.PreFinalPassInfo);
                            }
                            currentType.HiddenInfo = PerformFinalPassOnInfoString(currentType.PreFinalPassInfo);
                            if (debug_on) {
                                DebugPrint(" Final parsed info data (Arrays and Internal Structures Expanded)= " + currentType.HiddenInfo);
                            }
                        }
                        if (debug_on) {
                            DebugPrint("m_structInfoByNameHashTable.put(currentType.CppQualifiedName=" + currentType.CppQualifiedName + ",currentType=" + currentType + ");");
                            new Throwable().printStackTrace(System.out);
                        }
                        this.AddCppQualifiedType(currentType);
                        is_cmd_msg = false;
                        is_stat_msg = false;
                        currentType = new StructureTypeInfo();
                        currentType.fromFileName = file_loader.name;
                        currentType.fromLineNumber = file_loader.lines_read;
                        currentType.first_module_used_in = this;
                        currentType.DerivedFrom = null;
                        currentType.UnqualifiedDerivedFrom = null;
                        typedef = false;
                        if (debug_on) {
                            DebugPrint("typedef=" + typedef);
                        }
                        insideStruct = false;
                        continue MAIN_PARSE_LOOP;
                    }
                    structenum_on_this_line = false;
                    paren_on_this_line = false;
                    brace_on_this_line = false;
                    parseString = file_loader.readLine();
                    lineNumber++;
                    curFileLineNumber = lineNumber;
                    curFileLineText = parseString;
                    if (debug_on) {
                        DebugPrint("parseString = file_loader.readLine() = " + parseString);
                    }
                    if (Thread.interrupted()) {
                        interrupt_loading = true;
                        throw new Exception("Thread.interrupted() returned true\n");
                    }
                    if (interrupt_loading) {
                        throw new Exception("interrupt_loading=true\n");
                    }
                    if (parseString == null) {
                        if (!first_line_read) {
                            ErrorPrint("Can not read " + file_loader.name);
                            included_file_ok = false;
                            return;
                        }
                        break;
                    }
                    included_file_ok = true;
                    first_line_read = true;
                    if (null != m_loadingPanel) {
                        m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + parseString.length());
                        m_loadingPanel.updateDisplay();
                    }
                    this.lhui_name = file_loader.name;
                    this.lhui_total = file_loader.content_length;
                    this.lhui_part = curFileLineNumber;
                    this.lhui_update();
                    ndlaName = null;
                    ndlaInfo = null;
                    unboundedName = null;
                    unboundedInfo = null;
                    currentAttributeInfo = null;
                    startingParseString = parseString;
                    if (!HandlePreProcessing()) {
                        if (debug_on) {
                            if (iflevel != trueIfLevel) {
                                DebugPrint("***" + lineNumber + "*** " + parseString);
                            } else {
                                DebugPrint("---" + lineNumber + "--- " + parseString);
                            }
                        }
                        continue;
                    }
                    if (debug_on) {
                        if (iflevel != trueIfLevel) {
                            DebugPrint("***" + lineNumber + "*** " + parseString);
                        } else {
                            DebugPrint("---" + lineNumber + "--- " + parseString);
                        }
                    }
                    if (!HandleComments()) {
                        continue;
                    }

                    String combined_line = parseString;
                    paren_index = -1;
                    while (-1 != (paren_index = parseString.indexOf('(', paren_index + 1))) {
                        paren_on_this_line = true;
                        if (Thread.interrupted()) {
                            interrupt_loading = true;
                            throw new Exception("Thread.interrupted() returned true\n");
                        }
                        if (interrupt_loading) {
                            throw new Exception("interrupt_loading=true\n");
                        }
                        paren_count++;
                        if (debug_on) {
                            DebugPrint("paren_count = " + paren_count);
                        }
                    }
                    paren_index = -1;
                    while (-1 != (paren_index = parseString.indexOf(')', paren_index + 1))) {
                        paren_on_this_line = true;
                        if (Thread.interrupted()) {
                            interrupt_loading = true;
                            throw new Exception("Thread.interrupted() returned true\n");
                        }
                        if (interrupt_loading) {
                            throw new Exception("interrupt_loading=true\n");
                        }
                        paren_count--;
                        if (debug_on) {
                            DebugPrint("paren_count = " + paren_count);
                        }
                    }

                    while (paren_count != 0) {
                        parseString = file_loader.readLine();
                        lineNumber++;
                        curFileLineNumber = lineNumber;
                        curFileLineText = parseString;
                        if (debug_on) {
                            DebugPrint("parseString = file_loader.readLine() = " + parseString);
                        }
                        if (Thread.interrupted()) {
                            interrupt_loading = true;
                            throw new Exception("Thread.interrupted() returned true\n");
                        }
                        if (interrupt_loading) {
                            throw new Exception("interrupt_loading=true\n");
                        }
                        if (parseString == null) {
                            break;
                        }
                        if (null != m_loadingPanel) {
                            m_loadingPanel.set_bytes_read(m_loadingPanel.get_bytes_read() + parseString.length());
                            m_loadingPanel.updateDisplay();
                        }

                        this.lhui_name = file_loader.name;
                        this.lhui_total = file_loader.content_length;
                        this.lhui_part = curFileLineNumber;
                        this.lhui_update();
                        if (!HandlePreProcessing()) {
                            if (debug_on) {
                                if (iflevel != trueIfLevel) {
                                    DebugPrint("***" + lineNumber + "*** " + parseString);
                                } else {
                                    DebugPrint("---" + lineNumber + "--- " + parseString);
                                }
                            }
                            continue;
                        }
                        if (debug_on) {
                            if (iflevel != trueIfLevel) {
                                DebugPrint("***" + lineNumber + "*** " + parseString);
                            } else {
                                DebugPrint("---" + lineNumber + "--- " + parseString);
                            }
                        }
                        if (!HandleComments()) {
                            continue;
                        }
                        paren_index = -1;
                        combined_line += parseString;
                        if (debug_on) {
                            DebugPrint("combined_line=" + combined_line);
                        }
                        while (-1 != (paren_index = parseString.indexOf('(', paren_index + 1))) {
                            paren_on_this_line = true;
                            if (Thread.interrupted()) {
                                interrupt_loading = true;
                                throw new Exception("Thread.interrupted() returned true\n");
                            }
                            if (interrupt_loading) {
                                throw new Exception("interrupt_loading=true\n");
                            }
                            paren_count++;
                            if (debug_on) {
                                DebugPrint("paren_count = " + paren_count);
                            }
                        }
                        paren_index = -1;
                        while (-1 != (paren_index = parseString.indexOf(')', paren_index + 1))) {
                            paren_on_this_line = true;
                            if (Thread.interrupted()) {
                                interrupt_loading = true;
                                throw new Exception("Thread.interrupted() returned true\n");
                            }
                            if (interrupt_loading) {
                                throw new Exception("interrupt_loading=true\n");
                            }
                            paren_count--;
                            if (debug_on) {
                                DebugPrint("paren_count = " + paren_count);
                            }
                        }
                    }
                    parseString = combined_line;
                    if (!inside_extern_c) {
                        try {

                            StringTokenizer externc_check_tokenizer = new StringTokenizer(combined_line, " \t\r\n");
                            String tok1 = null;
                            String tok2 = null;
                            String tok3 = null;
                            if (externc_check_tokenizer.hasMoreTokens()) {
                                tok1 = externc_check_tokenizer.nextToken();
                            }
                            if (externc_check_tokenizer.hasMoreTokens()) {
                                tok2 = externc_check_tokenizer.nextToken();
                            }
                            if (externc_check_tokenizer.hasMoreTokens()) {
                                tok3 = externc_check_tokenizer.nextToken();
                            }
                            if (debug_on
                                    && (combined_line.indexOf("\"C\"") >= 0)) {
                                DebugPrint("StringTokenizer externc_check_tokenizer = new StringTokenizer(combined_line=" + combined_line + ",\" \t\r\n\");");
                                DebugPrint("tok1=\"" + tok1 + "\" \t\r\n\");");
                                DebugPrint("tok2=\"" + tok2 + "\" \t\r\n\");");
                                DebugPrint("tok3=\"" + tok3 + "\" \t\r\n\");");
                            }
                            if (null != tok1 && tok1.compareTo("extern") == 0
                                    && null != tok2 && tok2.compareTo("\"C\"") == 0) {
                                //DebugPrint2("combined_line="+combined_line);
                                if (null != tok3
                                        && tok3.startsWith("{")) {

                                    // if(combined_line.matches("[ \t\r\n]*extern[ \t\r\n]*\"C\"[ \t\r\n]*\\{.*")) {}
                                    inside_extern_c = true;
                                    next_brace_starts_extern_c = false;
                                    if (debug_on) {
                                        DebugPrint("inside_extern_c = " + inside_extern_c);
                                    }
                                    continue;
                                } else {
                                    // if(combined_line.matches("[ \t\r\n]*extern[ \t\r\n]*\"C\"[ \t\r\n]*\\{.*")) {}
                                    if (!combined_line.trim().endsWith(";")) {
                                        next_brace_starts_extern_c = true;
                                    }
                                    if (debug_on) {
                                        DebugPrint("inside_extern_c = " + inside_extern_c);
                                    }
                                    continue;
                                }
                            }
                        } catch (Exception e) {
                            if (interrupt_loading) {
                                throw e;
                            } else {
                                e.printStackTrace();
                            }
                        }
                    }
                    startingParseString = parseString;
                    if (!CheckForFormatFunction()) {
                        continue;
                    }
                    if (!CheckForSymbolLookup()) {
                        continue;
                    }
                    boolean const_found = false;
                    StringTokenizer word_tokenizer = new StringTokenizer(parseString, "\r\n\t \b;,(){}:");
                    while (word_tokenizer.hasMoreTokens()) {
                        String word = word_tokenizer.nextToken();
                        if (word.equals("const")) {
                            int eqindex = parseString.indexOf('=');
                            int scolonindex = parseString.indexOf(';');
                            if (eqindex > 0 && scolonindex > eqindex + 1 && brace_count == 0
                                    && paren_count == 0 && eqindex < parseString.length() - 1
                                    && parseString.charAt(eqindex - 1) != '<'
                                    && parseString.charAt(eqindex - 1) != '>'
                                    && parseString.charAt(eqindex - 1) != '-'
                                    && parseString.charAt(eqindex - 1) != '+'
                                    && parseString.charAt(eqindex - 1) != '*'
                                    && parseString.charAt(eqindex - 1) != '/'
                                    && parseString.charAt(eqindex - 1) != '!'
                                    && parseString.charAt(eqindex + 1) != '=') {
                                String preEqString = parseString.substring(0, eqindex);
                                StringTokenizer preEqTokenizer
                                        = new StringTokenizer(preEqString, "\r\n \t()=");
                                String last_token = null;
                                while (preEqTokenizer.hasMoreTokens()) {
                                    last_token = preEqTokenizer.nextToken();
                                }
                                String afterEqString = parseString.substring(eqindex + 1, scolonindex);
                                if (last_token != null
                                        && parseString.indexOf('{') < 0
                                        && parseString.indexOf('}') < 0
                                        && parseString.indexOf('(') < 0
                                        && parseString.indexOf(')') < 0
                                        && parseString.indexOf('*') < 0
                                        && parseString.indexOf('&') < 0
                                        && (Character.isLetter(last_token.charAt(0)) || last_token.charAt(0) == '_')) {
                                    DefinedValue dv = new DefinedValue();
                                    dv.name = last_token;
                                    dv.value = afterEqString;
                                    dv.tag = cur_tag();
                                    if (!definedValues.containsKey(dv.name)) {
                                        definedValues.put(dv.name, dv);
                                    }
                                }
                            }
                            const_found = true;
                            break;
                        }
                    }
                    if (const_found) {
                        continue;
                    }
                    if (parseString.indexOf("DECLARE_NML_DYNAMIC_LENGTH_ARRAY") >= 0) {
                        String array_type = null;
                        String array_name = null;
                        String array_size = null;
                        int paren1index = parseString.indexOf('(');
                        int paren2index = parseString.indexOf(')', paren1index);
                        if (paren1index > 0 && paren2index > paren1index + 1) {
                            String args = parseString.substring(paren1index + 1, paren2index);
                            StringTokenizer argTokenizer
                                    = new StringTokenizer(args, ",");
                            if (argTokenizer.hasMoreTokens()) {
                                array_type = argTokenizer.nextToken();
                            }
                            if (argTokenizer.hasMoreTokens()) {
                                array_name = RemoveStartingEndingSpace(argTokenizer.nextToken());
                            }
                            if (argTokenizer.hasMoreTokens()) {
                                array_size = ReplaceDefinedValues(argTokenizer.nextToken(), 0, null);
                                while (array_size.length() > 1 && array_size.charAt(0) == ' ') {
                                    array_size = array_size.substring(1);
                                }
                            }
                            if (null != array_type && null != array_name
                                    && null != array_size) {
                                parseString = "int " + array_name + "_length; NML_DYNAMIC_LENGTH_ARRAY " + array_type + " " + array_name + "[" + array_size + "];";
                                ndlaName = array_name;
                                ndlaInfo = parseString;
                            }
                        }
                        paren_on_this_line = false;
                    }
                    if (parseString.indexOf("DECLARE_NML_UNBOUNDED_ARRAY") >= 0) {
                        if (!definedValues.containsKey("NO_NML_UNBOUNDED")) {
                            String array_type = null;
                            String array_name = null;
                            int paren1index = parseString.indexOf('(');
                            int paren2index = parseString.indexOf(')', paren1index);
                            if (paren1index > 0 && paren2index > paren1index + 1) {
                                String args = parseString.substring(paren1index + 1, paren2index);
                                StringTokenizer argTokenizer
                                        = new StringTokenizer(args, ",");
                                if (argTokenizer.hasMoreTokens()) {
                                    array_type = argTokenizer.nextToken();
                                }
                                if (argTokenizer.hasMoreTokens()) {
                                    array_name = RemoveStartingEndingSpace(argTokenizer.nextToken());
                                }
                                if (null != array_type && null != array_name) {
                                    if (application_type == ModuleInfo.RCS_DIAGNOSTICS_APPLICATION_TYPE) {
                                        parseString = "int " + array_name + "_length; NML_DYNAMIC_LENGTH_ARRAY " + array_type + " " + array_name + "[2147483648];";
                                        ndlaName = array_name;
                                        ndlaInfo = parseString;
                                    } else {
                                        parseString = "NML_UNBOUNDED_LENGTH_ARRAY " + array_type + " " + array_name + ";";
                                        unboundedName = array_name;
                                        unboundedInfo = parseString;
                                    }
                                }
                            }
                            paren_on_this_line = false;
                        }
                    }
                    //if(parseString.regionMatches(false,0,"extern",0,6))
                    //  {
                    //    if(parseString.indexOf('{') > 0  && parseString.indexOf('}') < 0)
                    //      {
                    //        brace_count++;
                    //      }
                    //    continue;
                    // }
                    if (parseString.regionMatches(false, 0, "typedef", 0, 7)) {
                        if (currentType == null
                                || (currentType.getName() != null && currentType.getName().length() > 0)) {
                            currentType = new StructureTypeInfo();
                            currentType.fromFileName = file_loader.name;
                            currentType.fromLineNumber = file_loader.lines_read;
                            currentType.first_module_used_in = this;
                        }
                        if (parseString.indexOf('{') < 0
                                && parseString.indexOf('}') < 0
                                && parseString.indexOf(':') < 0
                                && parseString.indexOf(',') < 0
                                && parseString.indexOf('[') < 0
                                && parseString.indexOf(']') < 0
                                && parseString.indexOf('(') < 0
                                && parseString.indexOf(')') < 0
                                && parseString.indexOf("enum") < 0
                                && parseString.indexOf("class") < 0
                                && parseString.indexOf("struct") < 0
                                && parseString.indexOf("union") < 0
                                && parseString.indexOf(';') > 0) {
                            // Simple typedef -- treat like #define
                            String value = parseString.substring(8);
                            value = value.substring(0, value.indexOf(';'));
                            StringTokenizer typedefTokenizer = new StringTokenizer(value, " \t\r\n\b");
                            value = typedefTokenizer.nextToken();
                            String last_token = null;
                            while (typedefTokenizer.hasMoreTokens()) {
                                last_token = typedefTokenizer.nextToken();
                                if (typedefTokenizer.hasMoreTokens()) {
                                    value += " " + last_token;
                                }
                            }
                            dv = new DefinedValue();
                            dv.name = last_token;
                            dv.value = value;
                            if (null != dv.value && null != dv.name) {
                                if (dv.value.length() > 1 && dv.name.length() > 1) {
                                    dv.tag = cur_tag();
                                    definedValues.put(dv.name, dv);
                                }
                            }
                        }
                        typedef = true;
                        if (debug_on) {
                            DebugPrint("typedef=" + typedef);
                        }
                        parseString = parseString.substring(8);
                        if (debug_on) {
                            DebugPrint("parseString = " + parseString);
                        }
                    }
                    if (typedef && !insideStruct && brace_count == 0 && parseString.indexOf(';') > 0 && currentType != null && currentType.getName() != null
                            && currentType.getName().length() > 0) {
                        typedef = false;
                    } else if (typedef && !insideStruct && brace_count == 0 && parseString.indexOf(';') > 0) {
                        String pre_semicolon_string = parseString.substring(0, parseString.indexOf(';'));
                        StringTokenizer spc_tokenizer = new StringTokenizer(pre_semicolon_string, " \t");
                        String last_token = spc_tokenizer.nextToken();
                        String type_def_body = "";
                        while (last_token != null & spc_tokenizer.hasMoreTokens()) {
                            if (last_token.equals("struct") || last_token.equals("enum") || last_token.equals("class")) {
                                last_token = null;
                                type_def_body = null;
                                break;
                            }
                            type_def_body += last_token;
                            last_token = spc_tokenizer.nextToken();
                            if (spc_tokenizer.hasMoreTokens()) {
                                type_def_body += " ";
                            }
                        }
                        if (debug_on) {
                            DebugPrint("type_def_body=\"" + type_def_body + "\", type_def_name=\"" + last_token + "\"");
                        }
                        if (null != last_token
                                && null != type_def_body
                                && last_token.length() > 0
                                && type_def_body.length() > 0) {
                            if (last_token.indexOf('(') >= 0
                                    || last_token.indexOf(')') >= 0
                                    || last_token.indexOf('*') >= 0
                                    || last_token.indexOf('[') >= 0
                                    || last_token.indexOf(']') >= 0
                                    || last_token.indexOf(',') >= 0
                                    || type_def_body.indexOf('(') >= 0
                                    || type_def_body.indexOf(')') >= 0
                                    || type_def_body.indexOf('*') >= 0
                                    || type_def_body.indexOf('[') >= 0
                                    || type_def_body.indexOf(',') >= 0
                                    || type_def_body.indexOf(']') >= 0) {
                                int lti = last_token.indexOf('[');
                                if (lti > 0
                                        && last_token.charAt(last_token.length() - 1) == ']'
                                        && type_def_body.indexOf('(') < 0
                                        && type_def_body.indexOf(')') < 0
                                        && type_def_body.indexOf('*') < 0
                                        && type_def_body.indexOf('[') < 0
                                        && type_def_body.indexOf(',') < 0
                                        && type_def_body.indexOf(']') < 0) {
                                    DefinedValue typedef_dv = new DefinedValue();
                                    typedef_dv.value = type_def_body;
                                    typedef_dv.name = last_token.substring(0, lti);
                                    typedef_dv.tag = cur_tag();
                                    typedef_dv.arrayLenSting = last_token.substring(lti);
                                    definedValues.put(typedef_dv.name, typedef_dv);
                                    typedef = false;
                                } else {
                                    ErrorPrint("typedef too complicated : CodeGen doesn't do pointer or array typedefs,\n"
                                            + "    parseString=\"" + parseString + "\"\n"
                                            + "    last_token=\"" + last_token + "\"\n"
                                            + "    type_def_body=\"" + type_def_body + "\"\n");
                                    typedef = false;
                                    if (debug_on) {
                                        DebugPrint("typedef=" + typedef);
                                    }
                                    continue MAIN_PARSE_LOOP;
                                }
                            }

                            DefinedValue typedef_dv = new DefinedValue();
                            typedef_dv.value = type_def_body;
                            typedef_dv.name = last_token;
                            typedef_dv.tag = cur_tag();
                            definedValues.put(typedef_dv.name, typedef_dv);
                            typedef = false;
                            if (debug_on) {
                                DebugPrint("Adding defined value " + typedef_dv.name + " = " + typedef_dv.value + ", typedef=false");
                            }
                            continue MAIN_PARSE_LOOP;
                        }
                    }
                    if (debug_on) {
                        DebugPrint("typedef=" + typedef + ",insideStruct=" + insideStruct + ",brace_count=" + brace_count + ",parseString=" + parseString + ",currentType.Name=" + currentType.getName());
                    }

                    if (!HandleDefines()) {
                        continue;
                    }

                    if (parseString.regionMatches(false, 0, "#", 0, 1)) {
                        continue;
                    }
                    if (parseString.startsWith("enum") && brace_count < 1 && !insideStruct) {
                        default_enum_val = 0;
                        structenum_on_this_line = true;
                        if (!typedef) {
                            String enum_name = parseString.substring(5);
                            while (true) {
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                                if (enum_name.length() < 1) {
                                    break;
                                }
                                if (enum_name.charAt(0) != ' ' && enum_name.charAt(0) != '\t') {
                                    break;
                                }
                                enum_name = enum_name.substring(1);
                            }
                            int index = enum_name.indexOf('{');
                            if (index >= 0) {
                                enum_name = enum_name.substring(0, index);
                            }
                            while (true) {
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                                int enum_name_length = enum_name.length();
                                if (enum_name_length < 1) {
                                    break;
                                }
                                if (enum_name.charAt(enum_name_length - 1) != ' ' && enum_name.charAt(enum_name_length - 1) != '\t') {
                                    break;
                                }
                                enum_name = enum_name.substring(0, enum_name_length - 1);
                            }
                            if (debug_on) {
                                DebugPrint("current_enum.Name=" + current_enum.Name);
                                DebugPrint("current_enum.Info=" + current_enum.Info);
                            }
                            current_enum.Name = enum_name;
                            current_enum.Info = "";
                            if (debug_on) {
                                DebugPrint("current_enum.Name=" + current_enum.Name);
                                DebugPrint("current_enum.Info=" + current_enum.Info);
                            }
                        }
                        nextBraceStartsEnum = true;
                        nextBraceStartsUnion = false;
                        nextBraceStartsStruct = false;
                        if (debug_on) {
                            DebugPrint("nextBraceStartsEnum = " + nextBraceStartsEnum);
                            DebugPrint("nextBraceStartsStruct = " + nextBraceStartsStruct);
                            DebugPrint("current_enum.Name=" + current_enum.Name);
                        }
                    }
                    if (parseString.startsWith("struct") && brace_count < 1 && !insideStruct) {
                        nextBraceStartsEnum = false;
                        nextBraceStartsUnion = false;
                        nextBraceStartsStruct = true;
                        structenum_on_this_line = true;
                        parseString = parseString.substring(6);
                        if (debug_on) {
                            DebugPrint("nextBraceStartsEnum = " + nextBraceStartsEnum);
                            DebugPrint("nextBraceStartsStruct = " + nextBraceStartsStruct);
                            DebugPrint("parseString = " + parseString);
                        }
                    }
                    if (parseString.startsWith("class") && brace_count < 1 && !insideStruct) {
                        nextBraceStartsEnum = false;
                        nextBraceStartsUnion = false;
                        nextBraceStartsStruct = true;
                        structenum_on_this_line = true;
                        parseString = parseString.substring(5);
                        if (debug_on) {

                            DebugPrint("nextBraceStartsEnum = " + nextBraceStartsEnum);
                            DebugPrint("nextBraceStartsStruct = " + nextBraceStartsStruct);
                            DebugPrint("parseString = " + parseString);
                        }
                    }
                    if (parseString.startsWith("namespace") && !insideNameSpace) {
                        nextBraceStartsNameSpace = true;
                        nextBraceStartsEnum = false;
                        nextBraceStartsUnion = false;
                        nextBraceStartsStruct = false;
                        structenum_on_this_line = false;
                        currentNameSpace = "";
                        parseString = parseString.substring(9);
                        if (debug_on) {

                            DebugPrint("nextBraceStartsNameSpace = " + nextBraceStartsNameSpace);
                            DebugPrint("nextBraceStartsEnum = " + nextBraceStartsEnum);
                            DebugPrint("nextBraceStartsStruct = " + nextBraceStartsStruct);
                            DebugPrint("parseString = " + parseString);
                        }
                    }
                    if (parseString.startsWith("union")) {
                        nextBraceStartsEnum = false;
                        nextBraceStartsStruct = true;
                        structenum_on_this_line = true;
                        nextBraceStartsUnion = true;
                        parseString = parseString.substring(5);
                        if (debug_on) {

                            DebugPrint("nextBraceStartsEnum = " + nextBraceStartsEnum);
                            DebugPrint("nextBraceStartsStruct = " + nextBraceStartsStruct);
                            DebugPrint("parseString = " + parseString);
                        }
                    }
                    if (nextBraceStartsNameSpace && !insideNameSpace) {
                        brace_index = parseString.indexOf('{');
                        if (debug_on && brace_index >= 0) {
                            DebugPrint("brace_index=" + brace_index);
                        }
                        if (currentNameSpace.length() < 1) {
                            String nsstring = parseString;
                            if (debug_on) {
                                DebugPrint("nsstring=" + nsstring);
                            }
                            if (brace_index > 0) {
                                nsstring = nsstring.substring(0, brace_index);
                            }
                            StringTokenizer nsTokenizer = new StringTokenizer(parseString, " \t\r\n\b{}");
                            while (nsTokenizer.hasMoreTokens() && currentNameSpace.length() < 1) {
                                currentNameSpace = nsTokenizer.nextToken();
                                if (debug_on) {
                                    DebugPrint("currentNameSpace=" + currentNameSpace);
                                }
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                            }
                        }
                        if (brace_index >= 0) {
                            insideNameSpace = true;
                            nextBraceStartsNameSpace = false;
                            parseString = parseString.substring(brace_index + 1);
                        }

                        if (debug_on) {
                            DebugPrint("insideNameSpace=" + insideNameSpace);
                            DebugPrint("nextBraceStartsNameSpace=" + nextBraceStartsNameSpace);
                            DebugPrint("parseString=" + parseString);
                            DebugPrint("brace_index=" + brace_index);
                            DebugPrint("brace_count=" + brace_count);
                            DebugPrint("currentNameSpace=" + currentNameSpace);
                        }

                    }
                    brace_index = -1;
                    while (-1 != (brace_index = parseString.indexOf('{', brace_index + 1))) {
                        if (debug_on && brace_index >= 0) {
                            DebugPrint("brace_index=" + brace_index);
                            DebugPrint("brace_count=" + brace_count);
                        }
                        brace_on_this_line = true;
                        if (Thread.interrupted()) {
                            interrupt_loading = true;
                            throw new Exception("Thread.interrupted() returned true\n");
                        }
                        if (interrupt_loading) {
                            throw new Exception("interrupt_loading=true\n");
                        }
                        brace_count++;
                        if (debug_on) {
                            DebugPrint("brace_count = " + brace_count);
                        }
                    }
                    if (!brace_on_this_line
                            && next_brace_starts_extern_c
                            && parseString.indexOf(';') > 0) {
                        next_brace_starts_extern_c = false;
                    }
                    if (brace_count == 1
                            && next_brace_starts_extern_c) {
                        next_brace_starts_extern_c = false;
                        inside_extern_c = true;
                        brace_count = 0;
                        continue;
                    }
                    if (brace_count > 0
                            && nextBraceStartsNameSpace) {
                        nextBraceStartsNameSpace = false;
                        insideNameSpace = true;
                        continue;
                    }
                    brace_index = -1;
                    while (-1 != (brace_index = parseString.indexOf('}', brace_index + 1))) {
                        brace_on_this_line = true;
                        if (Thread.interrupted()) {
                            interrupt_loading = true;
                            throw new Exception("Thread.interrupted() returned true\n");
                        }
                        if (interrupt_loading) {
                            throw new Exception("interrupt_loading=true\n");
                        }
                        brace_count--;
                        if (brace_count == -1 && insideNameSpace) {
                            brace_count = 0;
                            currentNameSpace = "";
                            insideNameSpace = false;
                            if (debug_on) {
                                DebugPrint("insideNameSpace=" + insideNameSpace);
                                DebugPrint("nextBraceStartsNameSpace=" + nextBraceStartsNameSpace);
                                DebugPrint("parseString=" + parseString);
                                DebugPrint("brace_index=" + brace_index);
                                DebugPrint("brace_count=" + brace_count);
                                DebugPrint("currentNameSpace=" + currentNameSpace);
                            }
                        }
                        if (debug_on) {
                            if (insideNameSpace || lastInsideNameSpace || nextBraceStartsNameSpace || lastNextBraceStartsNameSpace) {
                                DebugPrint("insideNameSpace=" + insideNameSpace);
                                DebugPrint("nextBraceStartsNameSpace=" + nextBraceStartsNameSpace);
                                DebugPrint("currentNameSpace=" + currentNameSpace);
                            }
                            lastInsideNameSpace = insideNameSpace;
                            lastNextBraceStartsNameSpace = nextBraceStartsNameSpace;
                            DebugPrint("brace_count = " + brace_count);
                        }
                        if (brace_count < 0) {
                            if (inside_extern_c) {
                                inside_extern_c = false;
                                if (debug_on) {
                                    DebugPrint("inside_extern_c = " + inside_extern_c);
                                }
                            } else {
                                ErrorPrint(file_loader.name + ":" + lineNumber + ":  WARNING negative brace count(" + brace_count + ").");
                                if (debug_on) {
                                    DebugPrint("brace_count set to zero to avoid negative brace_count.");
                                }
                            }
                            brace_count = 0;
                        }
                    }

                    if (debug_on && (brace_on_this_line || structenum_on_this_line)) {
                        DebugPrint("brace_on_this_line=" + brace_on_this_line);
                        DebugPrint("structenum_on_this_line=" + structenum_on_this_line);

                    }

                    if (!insideEnum && !insideStruct && !typedef
                            && (brace_count < 1
                            || (brace_on_this_line && structenum_on_this_line))) {
                        String pre_brace_line = parseString;
                        if (brace_on_this_line) {
                            int brace_index = parseString.indexOf('{');
                            if (brace_index >= 0) {
                                pre_brace_line = parseString.substring(0, brace_index);
                            }
                        }
                        if (pre_brace_line.length() < 1) {
                            continue;
                        }
                        StringTokenizer nameTokenizer = null;
                        if (!insideEnum && !nextBraceStartsEnum) {
                            if (debug_on) {
                                DebugPrint(" StringTokenizer nameTokenizer = new StringTokenizer(pre_brace_line=" + pre_brace_line + ",\" ,{};:=()\t\r\n\b\");");
                            }
                            nameTokenizer = new StringTokenizer(pre_brace_line, " ,{};:=()\t\r\n\b");
                            if (!nameTokenizer.hasMoreTokens()) {
                                continue;
                            }
                            currentType.setName(nameTokenizer.nextToken());
                            if (debug_on) {
                                DebugPrint("currentType.Name=" + currentType.getName());
                                new Throwable().printStackTrace(System.out);
                            }
                            if (typedef) {
                                continue MAIN_PARSE_LOOP;
                            }
                            if (insideNameSpace) {
                                currentType.inside_namespace = true;
                                currentType.NameSpace = new String(currentNameSpace);
                                currentType.CppQualifiedName = currentType.NameSpace + "::" + currentType.getName();
                            } else {
                                currentType.CppQualifiedName = currentType.getName();
                            }
                            currentType.type_id_string = DefaultTypeIdString(currentType.getName());
                            if (debug_on) {
                                DebugPrint("currentType.Id = " + currentType.Id);
                            }
                            if (null == currentType.getName()) {
                                continue;
                            }
                            if (debug_on) {
                                DebugPrint("currentType.Name = " + currentType.getName());
                            }
                            while (currentType.getName().equals("RCS_EXPORT") || currentType.getName().equals("EXPORT")
                                    || currentType.getName().equals("static") || currentType.getName().equals("__attribute__")
                                    || currentType.getName().equals("__export") || currentType.getName().equals("_export")
                                    || currentType.getName().equals("__declspec") || currentType.getName().equals("dllimport")
                                    || currentType.getName().equals("dllexport") || currentType.getName().equals("public")
                                    || currentType.getName().equals("extern")) {
                                currentType.setName(null);
                                if (nameTokenizer == null || !nameTokenizer.hasMoreTokens()) {
                                    continue MAIN_PARSE_LOOP;
                                }
                                currentType.setName(nameTokenizer.nextToken());
                                if (debug_on) {
                                    DebugPrint("currentType.Name=" + currentType.getName());
                                    new Throwable().printStackTrace();
                                }
                                currentType.type_id_string = DefaultTypeIdString(currentType.getName());
                                if (debug_on) {
                                    DebugPrint("currentType.Name = " + currentType.getName());
                                }
                                if (null == currentType.getName()) {
                                    continue MAIN_PARSE_LOOP;
                                }
                            }
                            if (currentType.getName().length() < 1) {
                                continue;
                            }
                            currentType.type_id_string = DefaultTypeIdString(currentType.getName());
                            int index1 = pre_brace_line.indexOf(":");
                            if (index1 > 0) {
                                String parentsString = pre_brace_line.substring(index1 + 1);
                                if (null != parentsString) {
                                    StringTokenizer parentTokenizer = new StringTokenizer(parentsString, " ,\t\r\n\b");
                                    while (parentTokenizer.hasMoreTokens()) {
                                        if (Thread.interrupted()) {
                                            interrupt_loading = true;
                                            throw new Exception("Thread.interrupted() returned true\n");
                                        }
                                        if (interrupt_loading) {
                                            throw new Exception("interrupt_loading=true\n");
                                        }
                                        String parentname = parentTokenizer.nextToken();
                                        if (parentname.equals("public") || parentname.equals("protected") || parentname.equals("private")) {
                                            continue;
                                        }
                                        String top_parent = parentname;
                                        while (top_parent != null) {
                                            if (debug_on) {
                                                DebugPrint("top_parent=" + top_parent);
                                            }
                                            if (0 == top_parent.compareTo("RCS_CMD_MSG")) {
                                                is_cmd_msg = true;
                                                break;
                                            }
                                            if (0 == top_parent.compareTo("NMLmsg")) {
                                                if (currentType.getName().endsWith("_WM")
                                                        || currentType.getName().endsWith("_STAT")) {
                                                    is_cmd_msg = false;
                                                    is_stat_msg = true;
                                                } else {
                                                    is_cmd_msg = true;
                                                    is_stat_msg = false;
                                                }
                                                break;
                                            }
                                            if (0 == top_parent.compareTo("RCS_STAT_MSG")) {
                                                is_stat_msg = true;
                                                break;
                                            }
                                            StructureTypeInfo pi = (StructureTypeInfo) m_structInfoByNameHashTable.get(top_parent);
                                            if (null == pi) {
                                                break;
                                            }
                                            top_parent = pi.DerivedFrom;
                                        }
                                        if (debug_on) {
                                            DebugPrint("is_cmd_msg=" + is_cmd_msg);
                                        }
                                        currentType.DerivedFrom = parentname;
                                        currentType.UnqualifiedDerivedFrom = currentType.DerivedFrom;
                                        int doublecolonindex = currentType.UnqualifiedDerivedFrom.lastIndexOf("::");
                                        if (doublecolonindex > 0) {
                                            currentType.UnqualifiedDerivedFrom
                                                    = currentType.UnqualifiedDerivedFrom.substring(doublecolonindex + 2);
                                        }
                                        if (debug_on) {
                                            DebugPrint(currentType.getName() + " derived from " + currentType.DerivedFrom);
                                        }
                                    }
                                }

                            } else if (current_enum.Name == null || current_enum.Name.length() < 1) {
                                if (debug_on) {
                                    DebugPrint(" StringTokenizer nameTokenizer = new StringTokenizer(pre_brace_line=" + pre_brace_line + ",\" ,{};:=()\t\r\n\b\");");
                                }
                                nameTokenizer = new StringTokenizer(pre_brace_line, " ,{};:=()\t\r\n\b");
                                if (!nameTokenizer.hasMoreTokens()) {
                                    continue;
                                }
                                String t = nameTokenizer.nextToken();
                                while ((t == null || t.length() < 1 || t.equals("enum") || t.equals("typedef")) && nameTokenizer.hasMoreTokens()) {
                                    t = nameTokenizer.nextToken();
                                }
                                if (t != null && t.length() > 0 && !t.equals("enum") && !t.equals("typedef")) {
                                    current_enum.Name = t;
                                    if (debug_on) {
                                        DebugPrint("current_enum.Name=" + current_enum.Name);
                                        new Throwable().printStackTrace(System.out);
                                    }
                                }
                            }
                        }
                        dv = (DefinedValue) definedValues.get(currentType.getName() + "_TYPE");
                        if (debug_on) {
                            DebugPrint("dv =" + dv + " (DefinedValue) definedValues.get(" + currentType.getName() + "_TYPE)");
                        }
                        if (null != dv) {
                            if (null != dv.value) {
                                String dvr = ReplaceDefinedValues(dv.value, 0, null);
                                if (dvr != null && !dvr.equals(dvr) && dvr.length() > 1) {
                                    dv.value = dvr;
                                    definedValues.put(dv.name, dv);
                                }
                                StringTokenizer idTokenizer = new StringTokenizer(dv.value, " (),:;\t\r.*\n\\");
                                Long idLong = Long.valueOf(0);
                                while (idTokenizer.hasMoreTokens()) {
                                    if (Thread.interrupted()) {
                                        interrupt_loading = true;
                                        throw new Exception("Thread.interrupted() returned true\n");
                                    }
                                    if (interrupt_loading) {
                                        throw new Exception("interrupt_loading=true\n");
                                    }
                                    String idString = idTokenizer.nextToken();
                                    if (idString.compareTo("int") == 0
                                            || idString.compareTo("long") == 0
                                            || idString.compareTo("NMLTYPE") == 0
                                            || idString.compareTo("unsigned") == 0) {
                                        continue;
                                    }
                                    if (debug_on) {
                                        DebugPrint("dv=" + dv + ",idString=" + idString);
                                    }
                                    try {
                                        idLong = Long.valueOf(doSimpleLongMath(idString));
                                        if ((currentType.Id = idLong.intValue()) > 0) {
                                            break;
                                        }
                                    } catch (NumberFormatException e) {
                                        if (interrupt_loading) {
                                            throw e;
                                        }
                                        continue;
                                    }
                                }
                            }
                        }
                        if (debug_on) {
                            DebugPrint("currentType.Name = " + currentType.getName());
                            DebugPrint("currentType.Id = " + currentType.Id);
                            DebugPrint("is_cmd_stream = " + is_cmd_stream);
                            DebugPrint("is_cmd_msg = " + is_cmd_msg);
                            DebugPrint("is_stat_msg = " + is_stat_msg);
                        }
                        if (currentType.Id > 0) {
                            if (is_stat_msg) {
                                statData = currentType.Id + ",0,unknown,";
                            }
                            if (adding_aux_channel && !currentType.on_aux_msg_list) {
                                if (debug_on) {
                                    DebugPrint("auxMessages.addElement(currentType.Name=" + currentType.getName() + "=currentType.Id=" + currentType.Id + ");");
                                }
                                //Thread.dumpStack();
                                auxMessages.addElement(currentType.getName() + "=" + currentType.Id);
                                currentType.on_aux_msg_list = true;
                            }
                        }
                    }

                    int brace_begin_index = parseString.indexOf("{");
                    int brace_end_index = 0;
                    if (brace_begin_index > 0) {
                        brace_end_index = parseString.indexOf("}", brace_begin_index);
                    } else {
                        brace_end_index = parseString.indexOf("}");
                    }
                    if (brace_begin_index >= 0 && debug_on) {
                        DebugPrint("brace_begin_index = " + brace_begin_index);
                    }
                    if (brace_end_index >= 0 && debug_on) {
                        DebugPrint("brace_end_index = " + brace_end_index);
                    }
                    if (nextBraceStartsStruct && debug_on) {
                        DebugPrint("nextBraceStartsStruct=" + nextBraceStartsStruct);
                    }
                    if (nextBraceStartsEnum && debug_on) {
                        DebugPrint("nextBraceStartsEnum=" + nextBraceStartsEnum);
                    }

                    if (nextBraceStartsStruct || nextBraceStartsEnum) {
                        if (brace_begin_index >= 0) {
                            if (debug_on) {
                                DebugPrint("brace_begin_index=" + brace_begin_index);
                            }
                            if (brace_end_index < 0) {
                                if (nextBraceStartsEnum) {
                                    nextBraceStartsEnum = false;
                                    nextBraceStartsStruct = false;
                                    insideEnum = true;
                                    current_enum.Info = parseString.substring(brace_begin_index + 1) + "\n";
                                    if (debug_on) {
                                        DebugPrint("insideEnum=" + insideEnum);
                                        DebugPrint("parseString =" + parseString);
                                        DebugPrint("brace_begin_index =" + brace_begin_index);
                                        DebugPrint("current_enum.Info = " + current_enum.Info);
                                    }
                                    continue;
                                } else if (nextBraceStartsStruct) {
                                    nextBraceStartsEnum = false;
                                    nextBraceStartsStruct = false;
                                    insideStruct = true;
                                    currentType.RawInfo = parseString.substring(brace_begin_index + 1) + "\n";
                                    currentType.is_union = nextBraceStartsUnion;
                                    nextBraceStartsUnion = false;
                                    if (debug_on) {
                                        DebugPrint("insideStruct=" + insideStruct);
                                        DebugPrint("currentType.RawInfo = " + currentType.RawInfo);
                                    }
                                }
                            } else {
                                if (debug_on) {
                                    DebugPrint("brace_end_index=" + brace_end_index);
                                }
                                if (nextBraceStartsEnum) {
                                    nextBraceStartsEnum = false;
                                    nextBraceStartsStruct = false;
                                    insideEnum = true;
                                    current_enum.typedef = typedef;
                                    current_enum.Info = parseString.substring(brace_begin_index + 1, brace_end_index) + "\n";
                                    if (debug_on) {
                                        DebugPrint("insideEnum=" + insideEnum);
                                        DebugPrint("parseString =" + parseString);
                                        DebugPrint("brace_begin_index =" + brace_begin_index);
                                        DebugPrint("brace_end_index =" + brace_end_index);
                                        DebugPrint("current_enum.Info = " + current_enum.Info);
                                        DebugPrint("typedef=" + typedef);
                                    }
                                    if (typedef) {
                                        int semicolon_index = parseString.indexOf(';', brace_end_index);
                                        if (debug_on) {
                                            DebugPrint("semicolon_index=" + semicolon_index);
                                        }
                                        if (semicolon_index > brace_end_index) {
                                            String name_search_string = parseString.substring(brace_end_index + 1, semicolon_index);
                                            if (debug_on) {
                                                DebugPrint("name_search_string=" + name_search_string);
                                            }
                                            StringTokenizer name_search_string_tokenizer = new StringTokenizer(name_search_string, " \t\r\n;{},");
                                            while (name_search_string_tokenizer.hasMoreTokens()) {
                                                String t = name_search_string_tokenizer.nextToken();
                                                if (t != null && t.length() > 0 && t.charAt(0) != '*') {
                                                    current_enum.Name = t;
                                                    if (debug_on) {
                                                        DebugPrint("current_enum.Name=" + current_enum.Name);
                                                    }
                                                    break;
                                                }
                                            }
                                        }
                                    }
                                } else {
                                    nextBraceStartsEnum = false;
                                    nextBraceStartsStruct = false;
                                    insideStruct = true;
                                    currentType.RawInfo = parseString.substring(brace_begin_index + 1, brace_end_index) + "\n";
                                    if (debug_on) {
                                        DebugPrint("insideStruct=" + insideStruct);
                                        DebugPrint("currentType.RawInfo = " + currentType.RawInfo);
                                    }
                                }
                            }
                        }
                    }
                    if (insideEnum) {
                        if (debug_on) {
                            DebugPrint("insideEnum=" + insideEnum + ",brace_count=" + brace_count + ",brace_end_index=" + brace_end_index + ",typedef=" + typedef);
                        }
                        if (brace_count < 1
                                && ((brace_end_index >= 0 && !typedef) || (typedef && parseString.indexOf(';') >= 0))) {
                            insideEnum = false;
                            if (debug_on) {
                                DebugPrint("insideEnum=" + insideEnum);
                            }
                            if (typedef) {
                                StringTokenizer temptokenizerb = new StringTokenizer(parseString.substring(brace_end_index + 1), ":;{}, \t\r\n");
                                if (debug_on) {
                                    DebugPrint("StringTokenizer temptokenizerb = new StringTokenizer(parseString.substring(brace_end_index+1)=" + parseString.substring(brace_end_index + 1) + ",\":;{}, \t\r\n\");");
                                }
                                while (temptokenizerb.hasMoreTokens()
                                        && (current_enum.Name == null
                                        || current_enum.Name.length() < 1
                                        || current_enum.Name.indexOf('*') >= 0)) {
                                    current_enum.Name = temptokenizerb.nextToken();
                                    if (debug_on) {
                                        DebugPrint("current_enum.Name=" + current_enum.Name);
                                        new Throwable().printStackTrace(System.out);
                                    }
                                }
                            }
                            if (null == current_enum.Info) {
                                ErrorPrint("No Info for enum " + current_enum.Name);
                                current_enum = new EnumTypeInfo();
                                current_enum.generate_symbol_lookup = generate_enum_symbol_lookup;
                                continue;
                            }
                            if (debug_on) {
                                DebugPrint("Adding enum " + current_enum.Name);
                            }
                            String file_base = file_loader.name;
                            file_base = file_base.replace('.', '_');
                            file_base = file_base.replace(':', '_');
                            file_base = file_base.replace('.', '_');
                            file_base = file_base.replace('/', '_');
                            file_base = file_base.replace('\\', '_');
                            file_base = file_base + "_" + file_loader.lines_read;
                            if (current_enum.Name == null) {
                                current_enum.Name = "anonymous_enum_" + file_base;
                                anonymous_enum_number++;
                            }
                            int name_brace_begin_index = current_enum.Name.lastIndexOf("{");
                            if (name_brace_begin_index >= 0) {
                                current_enum.Name = current_enum.Name.substring(name_brace_begin_index + 1);
                            }
                            int name_brace_end_index = current_enum.Name.indexOf("}");
                            if (name_brace_end_index >= 0) {
                                current_enum.Name = current_enum.Name.substring(0, name_brace_end_index);
                            }
                            if (current_enum.Name.length() < 1) {
                                current_enum.Name = "anonymous_enum_" + file_base;
                                anonymous_enum_number++;
                            }
                            int info_brace_begin_index = current_enum.Info.lastIndexOf("{");
                            if (info_brace_begin_index >= 0) {
                                current_enum.Info = current_enum.Info.substring(info_brace_begin_index + 1);
                            }
                            int info_brace_end_index = current_enum.Info.indexOf("}");
                            if (info_brace_end_index >= 0) {
                                current_enum.Info = current_enum.Info.substring(0, info_brace_end_index);
                            }
                            current_enum.insideNameSpace = insideNameSpace;
                            if (insideNameSpace) {
                                current_enum.NameSpace = new String(currentNameSpace);
                                current_enum.CppQualifiedName = current_enum.NameSpace + "::" + current_enum.Name;
                            } else {
                                current_enum.CppQualifiedName = current_enum.Name;
                            }
                            //DebugPrint(current_enum.Name);
                            infoTokenizer = new StringTokenizer(current_enum.Info, " \t\r\n");
                            tempInfoString = "";
                            while (infoTokenizer.hasMoreTokens()) {
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                                infoToken = infoTokenizer.nextToken();
                                if (null == infoToken) {
                                    break;
                                }
                                if (infoToken.length() < 1) {
                                    continue;
                                }
                                tempInfoString += infoToken;
                            }
                            current_enum.Info = tempInfoString;
                            if (debug_on) {
                                DebugPrint("current_enum.Info = " + current_enum.Info);
                            }
                            infoTokenizer = new StringTokenizer(current_enum.Info, ",");

                            while (infoTokenizer.hasMoreTokens()) {
                                if (Thread.interrupted()) {
                                    interrupt_loading = true;
                                    throw new Exception("Thread.interrupted() returned true\n");
                                }
                                if (interrupt_loading) {
                                    throw new Exception("interrupt_loading=true\n");
                                }
                                infoToken = infoTokenizer.nextToken();
                                if (null == infoToken) {
                                    break;
                                }
                                if (infoToken.length() < 1) {
                                    continue;
                                }
                                Integer enum_value = Integer.valueOf(default_enum_val);
                                String enum_string = infoToken;
                                int eqIndex = infoToken.indexOf('=');
                                if (eqIndex != -1) {
                                    enum_string = infoToken.substring(0, eqIndex);
                                    String enum_v_temp = ReplaceDefinedValues(infoToken.substring(eqIndex + 1), 0, null);
                                    enum_value = Integer.valueOf(rcs.utils.StrToInt.convert(enum_v_temp));
                                }
                                default_enum_val = enum_value.intValue() + 1;
                                current_enum.insideNameSpace = insideNameSpace;
                                if (insideNameSpace) {
                                    enum_string = currentNameSpace + "::" + enum_string;
                                }
                                current_enum.hashtable.put(enum_value, enum_string);
                                current_enum.reverse_hashtable.put(enum_string, enum_value);
                                if (debug_on) {
                                    DebugPrint(current_enum.Name + ":" + enum_string + "=" + enum_value);
                                }
                                DefinedValue dv_enum_value = new DefinedValue();
                                dv_enum_value.name = enum_string;
                                dv_enum_value.value = enum_value.toString();
                                if (!definedValues.containsKey(dv_enum_value.name)) {
                                    dv_enum_value.tag = cur_tag();
                                    definedValues.put(dv_enum_value.name, dv_enum_value);
                                }
                                if (debug_on) {
                                    DebugPrint("Adding defined value " + dv_enum_value.name + " = " + dv_enum_value.value);
                                }

                            }
                            current_enum.typedef = typedef;
                            if (current_enum.CppQualifiedName.indexOf(' ') >= 0
                                    || current_enum.CppQualifiedName.indexOf(',') >= 0) {
                                ErrorPrint("Bad enum name=" + current_enum.CppQualifiedName + " : " + current_enum);
                            } else {
                                if (debug_on) {
                                    DebugPrint("m_enumInfoHashTable.put((Object) current_enum.CppQualifiedName=" + current_enum.CppQualifiedName + ",(Object) current_enum=" + current_enum);
                                }
                                current_enum.generate_symbol_lookup = generate_enum_symbol_lookup;
                                m_enumInfoHashTable.put((Object) current_enum.CppQualifiedName, (Object) current_enum);
                            }
                            current_enum = new EnumTypeInfo();
                            currentDefaultValue = null;
                            currentOverrideName = null;
                            typedef = false;
                            if (debug_on) {
                                DebugPrint("typedef=" + typedef);
                            }
                            continue;
                        }
                        if (debug_on) {
                            DebugPrint("currentOverrideName=" + currentOverrideName);
                        }
                        if (null != currentOverrideName) {
                            StringTokenizer st = new StringTokenizer(parseString, " \t\r\n,=/*");
                            if (st.hasMoreTokens()) {
                                String tok = st.nextToken();
                                if (null != tok) {
                                    if (null == current_enum.override_names_hashtable) {
                                        current_enum.override_names_hashtable = new Hashtable();
                                    }
                                    current_enum.override_names_hashtable.put(tok, currentOverrideName);
                                    DebugPrint("currentOverrideName=" + currentOverrideName + ", enum_string=" + tok);
                                    currentOverrideName = null;
                                }
                            }
                        }
                        if (debug_on) {
                            DebugPrint("current_enum.Info(" + current_enum.Info + ") += parseString(" + parseString + ")");
                        }
                        current_enum.Info += parseString;
                        continue;
                    } else if (insideStruct) {
                        if (debug_on) {
                            DebugPrint("insideStruct");
                        }
                        if ((brace_end_index >= 0 || typedef) && brace_count < 1) {
                            if (debug_on) {
                                DebugPrint("insideStruct=" + insideStruct);
                            }
                            if (typedef) {
                                if (debug_on) {
                                    DebugPrint("StringTokenizer temptokenizerb = new StringTokenizer(parseString=" + parseString + ",\":;{}, \t\r\n\");");
                                }
                                parseString = ReplaceDefinedValues(parseString, 0, null);
                                StringTokenizer temptokenizerb = new StringTokenizer(parseString, ":;{}, \t\r\n");
                                while (temptokenizerb.hasMoreTokens()
                                        && (currentType.getName() == null
                                        || currentType.getName().length() < 1
                                        || currentType.getName().indexOf('*') >= 0)) {
                                    currentType.setName(temptokenizerb.nextToken());
                                    if (currentType.getName().compareTo("struct") == 0) {
                                        currentType.setName(null);
                                        continue;
                                    }
                                    if (debug_on) {
                                        DebugPrint("currentType.Name=" + currentType.getName());
                                    }
                                    if (insideNameSpace) {
                                        currentType.inside_namespace = true;
                                        currentType.NameSpace = new String(currentNameSpace);
                                        currentType.CppQualifiedName = currentType.NameSpace + "::" + currentType.getName();
                                    } else {
                                        currentType.CppQualifiedName = currentType.getName();
                                    }
                                    if (debug_on) {
                                        DebugPrint("currentType.Name = " + currentType.getName());
                                    }
                                }
                                if (currentType.getName() != null
                                        && currentType.getName().length() > 0) {
                                    continue MAIN_PARSE_LOOP;
                                }
                            } else {
                                insideStruct = false;
                                if (debug_on) {
                                    DebugPrint("insideStruct=" + insideStruct);
                                }
                            }
                            if (debug_on) {
                                DebugPrint("ModuleInfo.ParseInfoString: Preparing info for class " + currentType.getName());
                                DebugPrint(" Original  currentType.RawInfo = " + currentType.RawInfo);
                                DebugPrint(" Original  currentType.HiddenInfo = " + currentType.HiddenInfo);
                            }
                            Long key = Long.valueOf(currentType.Id);
                            if (currentType.Id > 10) {
                                if (m_structInfoHashTable.containsKey(key)) {
                                    try {
                                        StructureTypeInfo conflictType = (StructureTypeInfo) m_structInfoHashTable.get(key);
                                        if (0 != conflictType.getName().compareTo(currentType.getName())) {
                                            ErrorPrint("NMLTYPE " + currentType.Id + " reused.");
                                            ErrorPrint("currentType= " + currentType.getName() + " conflicts with conflictType= " + conflictType.getName());
                                            parse_error_string += "\nNMLTYPE " + currentType.Id + " reused.\n" + currentType.getName() + " conflicts with " + conflictType.getName();
                                            parse_error_to_send = true;
                                            ErrorPrint("currentType.type_id_string=" + currentType.type_id_string + ", conflictType.type_id_string=" + conflictType.type_id_string);
                                            conflictType.conflicts = true;
                                            currentType.conflicts = true;
                                            if (null != conflictType.first_module_used_in
                                                    && this != conflictType.first_module_used_in) {
                                                if (null == conflictType.first_module_used_in.get_conflict_m_structInfoHashTable().get(key)) {
                                                    conflictType.first_module_used_in.get_conflict_m_structInfoHashTable().put(key, conflictType);
                                                }
                                                conflict_m_structInfoHashTable.put(key, currentType);
                                            }
                                        }
                                    } catch (Exception e) {
                                        if (interrupt_loading) {
                                            throw e;
                                        } else {
                                            e.printStackTrace();
                                        }
                                    }
                                }
                            }
                            m_structInfoHashTable.put((Long.valueOf(currentType.Id)), currentType);
                            if (currentType.Id > 0) {
                                if (is_stat_msg) {
                                    statData = currentType.Id + ",0,unknown,";
                                }
                                if (adding_aux_channel && !currentType.on_aux_msg_list) {
                                    if (debug_on) {
                                        DebugPrint("auxMessages.addElement(currentType.Name=" + currentType.getName() + "=currentType.Id=" + currentType.Id + ");");
                                    }
                                    //Thread.dumpStack();
                                    auxMessages.addElement(currentType.getName() + "=" + currentType.Id);
                                    currentType.on_aux_msg_list = true;
                                }
                            }

                            if (null != currentType.RawInfo) {
                                String info_with_unneccesary_space_removed = RemoveUnnecessarySpace(currentType.RawInfo);
                                if (debug_on) {
                                    DebugPrint("info_with_unneccesary_space_removed=" + info_with_unneccesary_space_removed);
                                }
                                currentType.HiddenInfo = new String(info_with_unneccesary_space_removed);
                                infoTokenizer = new StringTokenizer(currentType.HiddenInfo, " \t\r\n");
                                tempInfoString = "";
                                while (infoTokenizer.hasMoreTokens()) {
                                    if (Thread.interrupted()) {
                                        interrupt_loading = true;
                                        throw new Exception("Thread.interrupted() returned true\n");
                                    }
                                    if (interrupt_loading) {
                                        throw new Exception("interrupt_loading=true\n");
                                    }
                                    infoToken = infoTokenizer.nextToken();
                                    if (null == infoToken) {
                                        break;
                                    }
                                    if (debug_on) {
                                        DebugPrint("infoToken = " + infoToken);
                                    }
                                    if (infoToken.length() < 1) {
                                        continue;
                                    }

                                    if (definedValues.containsKey(infoToken)) {
                                        dv = (DefinedValue) definedValues.get(infoToken);
                                        if (dv != null) {
                                            if (dv.value.length() > 0) {
                                                if (debug_on) {
                                                    DebugPrint("infoToken = (" + infoToken + ") replaced by DefinedValue = (" + dv.value + ")");
                                                    DebugPrint("dv=" + dv);
                                                }
                                                infoToken = dv.value;
                                            }
                                        }
                                    }
                                    int lSquareParamIndex = infoToken.indexOf('[');
                                    int rSquareParamIndex = infoToken.indexOf(']');
                                    while (lSquareParamIndex >= 0 && rSquareParamIndex > lSquareParamIndex) {
                                        String checkString = infoToken.substring(lSquareParamIndex + 1, rSquareParamIndex);
                                        checkString = ReplaceDefinedValues(checkString, 0, null);
                                        int array_length = doArrayLengthMath(checkString);
                                        if (array_length < 1) {
                                            ErrorPrint(file_loader.name + ":" + lineNumber + ": Bad array length(" + array_length + ")");
                                            array_length = 1;
                                        }
                                        String array_length_string = Integer.toString(array_length);
                                        infoToken = infoToken.substring(0, lSquareParamIndex) + "[" + array_length_string + infoToken.substring(rSquareParamIndex);
                                        lSquareParamIndex = infoToken.indexOf('[', lSquareParamIndex + array_length_string.length() + 2);
                                        if (lSquareParamIndex > 0) {
                                            rSquareParamIndex = infoToken.indexOf(']', lSquareParamIndex + 1);
                                        }
                                    }
                                    if (infoToken.length() > 0) {
                                        if (tempInfoString.length() > 1 && !infoToken.startsWith(";") && !infoToken.startsWith("[")) {
                                            tempInfoString += " ";
                                        }
                                        tempInfoString += infoToken;
                                    }
                                }
                                if (debug_on) {
                                    DebugPrint(" First pass data (DefinedValues substituted). = " + tempInfoString);
                                }
                                currentType.StepTwoInfo = tempInfoString;
                                currentType.PreFinalPassInfo = PerformFirstPassOnInfoString(tempInfoString);
                                if (debug_on) {
                                    DebugPrint(" Second pass data (Extra White space eliminated and Enums identified.) = " + currentType.PreFinalPassInfo);
                                }
                                currentType.HiddenInfo = PerformFinalPassOnInfoString(currentType.PreFinalPassInfo);
                                if (debug_on) {
                                    DebugPrint(" Final parsed info data (Arrays and Internal Structures Expanded)= " + currentType.HiddenInfo);
                                }
                                if (null != currentType.DerivedFrom && null != currentType.DerivedFrom) {
                                    StructureTypeInfo parentInfo = (StructureTypeInfo) m_structInfoByNameHashTable.get(currentType.DerivedFrom);
                                    if (debug_on) {
                                        DebugPrint(" parentInfo = " + parentInfo);
                                    }
                                    if (null != parentInfo
                                            && null != parentInfo.HiddenInfo
                                            && parentInfo.HiddenInfo.compareTo("null") != 0) {
                                        if (debug_on) {
                                            DebugPrint(" parentInfo.RawInfo = " + parentInfo.RawInfo);
                                            DebugPrint(" parentInfo.HiddenInfo = " + parentInfo.HiddenInfo);
                                        }
                                        if (null != currentType.HiddenInfo
                                                && currentType.HiddenInfo.compareTo("null") != 0) {
                                            currentType.HiddenInfo = parentInfo.HiddenInfo + currentType.HiddenInfo;
                                        } else {
                                            currentType.HiddenInfo = parentInfo.HiddenInfo;
                                        }
                                    }
                                }
                                if (currentType.CppQualifiedName.length() > 0) {
                                    if (m_structInfoByNameHashTable.containsKey(currentType.CppQualifiedName)) {
                                        m_structInfoByNameHashTable.remove(currentType.CppQualifiedName);
                                    }
                                    String type_id_string_rv = ReplaceDefinedValues(currentType.type_id_string, 0, null);
                                    if (debug_on) {
                                        DebugPrint("currentType.Name = " + currentType.getName());
                                        DebugPrint("currentType.Id = " + currentType.Id);
                                        DebugPrint("currentType.type_id_string = " + currentType.type_id_string);
                                        DebugPrint("type_id_string_rv = type_id_string_rv");
                                    }
                                    if (type_id_string_rv != null && type_id_string_rv.length() > 1
                                            && !type_id_string_rv.equals(currentType.type_id_string)) {
                                        try {
                                            Long idLong = Long.valueOf(doSimpleLongMath(type_id_string_rv));
                                            long new_id = idLong.longValue();
                                            if (new_id != currentType.Id) {
                                                if (debug_on) {
                                                    DebugPrint("new_id=" + new_id);
                                                }
                                                if (new_id > 0) {
                                                    currentType.Id = new_id;
                                                    DefinedValue dv = (DefinedValue) definedValues.get(currentType.type_id_string);
                                                    if (null != dv && !type_id_string_rv.equals(dv.value)) {
                                                        dv.value = idLong.toString();
                                                        definedValues.put(dv.name, dv);
                                                    }

                                                }
                                            }
                                        } catch (Exception e) {
                                            if (interrupt_loading) {
                                                throw e;
                                            }
                                        }
                                    }
                                    if (null == cmdsAvailable) {
                                        common_cmd_base = null;
                                    }
                                    CheckIfShouldAddAvailableCommand(is_cmd_stream);

                                    if (currentType.Id > 0 && is_stat_msg
                                            && (statsTypeFile == null || statsTypeFile.length() < 1
                                            || currentType.fromFileName.equals(statsTypeFile)
                                            || currentType.fromFileName.equals(baseClassStatsTypeFile)
                                            || statsTypeFile.compareTo(cmdsTypeFile) == 0)) {
                                        statMsgsAvailable.addElement(currentType.getName() + "=" + currentType.Id);
                                        if (primaryStatusType == null) {
                                            primaryStatusType = currentType.getName();
                                        }
                                    }
                                    if (debug_on) {
                                        DebugPrint("currentType.Name = " + currentType.getName());
                                        DebugPrint("currentType.Id = " + currentType.Id);
                                        DebugPrint("is_cmd_stream = " + is_cmd_stream);
                                        DebugPrint("is_cmd_msg = " + is_cmd_msg);
                                        DebugPrint("is_stat_msg = " + is_stat_msg);
                                        DebugPrint("m_structInfoByNameHashTable.put(" + currentType.CppQualifiedName + "," + currentType + ");");
                                        new Throwable().printStackTrace(System.out);
                                        ;
                                    }
                                    this.AddCppQualifiedType(currentType);
                                    currentType = new StructureTypeInfo();
                                    currentType.fromFileName = file_loader.name;
                                    currentType.fromLineNumber = file_loader.lines_read;
                                    currentType.first_module_used_in = this;
                                    is_cmd_msg = false;
                                    is_stat_msg = false;
                                    continue MAIN_PARSE_LOOP;
                                }
                            }
                            if (debug_on) {
                                DebugPrint(" currentType.RawInfo = " + currentType.RawInfo);
                                DebugPrint(" currentType.HiddenInfo = " + currentType.HiddenInfo);
                            }
                            currentDefaultValue = null;
                            currentOverrideName = null;
                            continue;
                        }
                        int tilde_index = parseString.indexOf('~');

                        if (tilde_index >= 0) {
                            if (null != currentType) {
                                if (null != currentType.getName()) {
                                    if (parseString.indexOf(currentType.getName()) > 0) {
                                        currentType.destructor_declared = true;
                                        if (parseString.indexOf(';') > 0
                                                && parseString.indexOf('{') < 0) {
                                            currentType.destructor_declared_and_not_inlined = true;
                                            if (debug_on) {
                                                DebugPrint("Destructor for " + currentType.getName() + " declared but not inlined.");
                                            }
                                        }
                                    }
                                }
                            }
                            continue;
                        }
                        if (null != currentType) {
                            if (null != currentType.getName() && !typedef) {
                                if (currentType.getName().length() > 0) {
                                    if (parseString.indexOf(currentType.getName()) >= 0) {
                                        if (debug_on) {
                                            DebugPrint(" Possible constructor :" + parseString);
                                        }
                                        if (parseString.indexOf('(') > 0
                                                && parseString.indexOf(')') > 0) {
                                            while (brace_count > 1 || parseString.indexOf(';') < 0) {
                                                String line_to_add = file_loader.readLine();
                                                lineNumber++;
                                                curFileLineNumber = lineNumber;
                                                curFileLineText = parseString;
                                                String origParseString = parseString;
                                                parseString = line_to_add;
                                                DebugPrint("line_to_add=file_loader.readLine() =" + line_to_add);
                                                if (Thread.interrupted()) {
                                                    interrupt_loading = true;
                                                    throw new Exception("Thread.interrupted() returned true\n");
                                                }
                                                if (interrupt_loading) {
                                                    throw new Exception("interrupt_loading=true\n");
                                                }
                                                if (null == line_to_add || interrupt_loading) {
                                                    break;
                                                }
                                                if (!HandlePreProcessing()) {
                                                    if (debug_on) {
                                                        if (iflevel != trueIfLevel) {
                                                            DebugPrint("***" + lineNumber + "*** " + parseString);
                                                        } else {
                                                            DebugPrint("---" + lineNumber + "--- " + parseString);
                                                        }
                                                    }
                                                    parseString = origParseString;
                                                    continue;
                                                }
                                                if (debug_on) {
                                                    if (iflevel != trueIfLevel) {
                                                        DebugPrint("***" + lineNumber + "*** " + parseString);
                                                    } else {
                                                        DebugPrint("---" + lineNumber + "--- " + parseString);
                                                    }
                                                }
                                                if (!HandleComments()) {
                                                    parseString = origParseString;
                                                    continue;
                                                }
                                                brace_index = -1;
                                                while (-1 != (brace_index = parseString.indexOf('{', brace_index + 1))) {
                                                    brace_on_this_line = true;
                                                    if (Thread.interrupted()) {
                                                        interrupt_loading = true;
                                                        throw new Exception("Thread.interrupted() returned true\n");
                                                    }
                                                    if (interrupt_loading) {
                                                        throw new Exception("interrupt_loading=true\n");
                                                    }
                                                    brace_count++;
                                                    if (debug_on) {
                                                        DebugPrint("brace_count = " + brace_count);
                                                    }
                                                }
                                                brace_index = -1;
                                                while (-1 != (brace_index = parseString.indexOf('}', brace_index + 1))) {
                                                    brace_on_this_line = true;
                                                    if (Thread.interrupted()) {
                                                        interrupt_loading = true;
                                                        throw new Exception("Thread.interrupted() returned true\n");
                                                    }
                                                    if (interrupt_loading) {
                                                        throw new Exception("interrupt_loading=true\n");
                                                    }
                                                    brace_count--;
                                                }
                                                parseString = origParseString + line_to_add;
                                            }
                                            currentType.constructor_declared = true;
                                            if (parseString.indexOf('{') < 0
                                                    && parseString.indexOf(':') < 0) {
                                                currentType.constructor_declared_and_not_inlined = true;
                                                if (debug_on) {
                                                    DebugPrint("Constructor for " + currentType.getName() + " declared but not inlined.");
                                                }
                                                continue;
                                            } else {
                                                int comma_index = parseString.indexOf(',');
                                                int colon_index = parseString.indexOf(':');
                                                int first_paren_after_colon_index = parseString.indexOf('(', colon_index);
                                                if (first_paren_after_colon_index > 0 && comma_index > first_paren_after_colon_index) {
                                                    String type_id_string = parseString.substring(first_paren_after_colon_index + 1, comma_index);
                                                    //boolean orig_debug_3 = debug_on;
                                                    if (debug_on) {
                                                        DebugPrint("type_id_string =" + type_id_string);
                                                        //debug_on=true;
                                                    }
                                                    long type_id = -1;
                                                    try {
                                                        if (currentType.DerivedFrom != null || currentType.is_nml_msg) {
                                                            bad_simple_long_math = false;
                                                            String idString = ReplaceDefinedValues(type_id_string, 0, null);
                                                            Long idLong = Long.valueOf(doSimpleLongMath(idString));
                                                            type_id = idLong.longValue();
                                                            if (rcs.utils.StrToLong.bad_token || bad_simple_long_math) {
                                                                ErrorPrint("Bad type_id_string for " + currentType.getName() + "  : " + type_id_string + " --> " + idString + " --> " + type_id);
                                                            }
                                                        }
                                                    } catch (Exception e) {
                                                        if (interrupt_loading) {
                                                            throw e;
                                                        }
                                                    }
                                                    //debug_on=orig_debug_3;
                                                    if (type_id > 0) {
                                                        currentType.Id = type_id;
                                                        currentType.type_id_string = type_id_string;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if (paren_on_this_line) {
                            if (debug_on) {
                                DebugPrint("paren_on_this_line");
                            }
                            continue;
                        }
                        if (brace_on_this_line) {
                            if (debug_on) {
                                DebugPrint("brace_on_this_line");
                            }
                            continue;
                        }
                        if (paren_count > 0) {
                            continue;
                        }
                        if (paren_count < 0) {
                            ErrorPrint("paren_count = " + paren_count);
                            paren_count = 0;
                        }
                        if (brace_count > 1) {
                            continue;
                        }
                        if (brace_count < 0) {
                            if (inside_extern_c) {
                                inside_extern_c = false;
                                if (debug_on) {
                                    DebugPrint("inside_extern_c = " + inside_extern_c);
                                }
                            } else {
                                ErrorPrint("brace_count = " + brace_count);
                            }
                            brace_count = 0;
                        }
                        if (-1 != parseString.indexOf(':')) {
                            continue;
                        }
                        if (-1 != parseString.indexOf('&')) {
                            continue;
                        }
                        if (debug_on) {
                            DebugPrint("Adding " + parseString + " to currentType.Info");
                        }
                        if (currentDefaultValue != null) {
                            String var = parseString;
                            int var_semicolon_index = var.indexOf(';');
                            if (var_semicolon_index > 0) {
                                var = var.substring(0, var_semicolon_index);
                                int var_space_index = var.lastIndexOf(' ');
                                if (var_space_index > 0) {
                                    var = var.substring(var_space_index + 1);
                                }
                                int var_squareparen_index = var.indexOf('[');
                                if (var_squareparen_index > 0) {
                                    var = var.substring(0, var_squareparen_index);
                                }
                                if (debug_on) {
                                    DebugPrint("currentDefaultValue=" + currentDefaultValue + ", var =" + var);
                                }

                                currentType.VarnameToDefaultsHashTable.put(var, currentDefaultValue);
                                currentDefaultValue = null;
                            }
                        }
                        if (ndlaInfo != null) {
                            String var = parseString;
                            int var_semicolon_index = var.indexOf(';');
                            if (var_semicolon_index > 0) {
                                var = var.substring(0, var_semicolon_index);
                                int var_space_index = var.lastIndexOf(' ');
                                if (var_space_index > 0) {
                                    var = var.substring(var_space_index + 1);
                                }
                                int var_squareparen_index = var.indexOf('[');
                                if (var_squareparen_index > 0) {
                                    var = var.substring(0, var_squareparen_index);
                                }
                                if (debug_on) {
                                    DebugPrint("ndlaInfo=" + ndlaInfo + ",ndlaName=" + ndlaName + ", var =" + var);
                                }
                                currentType.VarnameNDLAHashTable.put(var, ndlaInfo);
                            }
                        }
                        if (unboundedInfo != null) {
                            String var = parseString;
                            int var_semicolon_index = var.indexOf(';');
                            if (var_semicolon_index > 0) {
                                var = var.substring(0, var_semicolon_index);
                                int var_space_index = var.lastIndexOf(' ');
                                if (var_space_index > 0) {
                                    var = var.substring(var_space_index + 1);
                                }
                                int var_squareparen_index = var.indexOf('[');
                                if (var_squareparen_index > 0) {
                                    var = var.substring(0, var_squareparen_index);
                                }
                                if (debug_on) {
                                    DebugPrint("unboundedInfo=" + unboundedInfo + ",unboundedName=" + unboundedName + ", var =" + var);
                                }

                                currentType.VarnameUnboundedHashTable.put(var, unboundedInfo);
                            }
                        }
                        if (currentOverrideName != null) {
                            String var = parseString;
                            int var_semicolon_index = var.indexOf(';');
                            if (var_semicolon_index > 0) {
                                var = var.substring(0, var_semicolon_index);
                                int var_space_index = var.lastIndexOf(' ');
                                if (var_space_index > 0) {
                                    var = var.substring(var_space_index + 1);
                                }
                                int var_squareparen_index = var.indexOf('[');
                                if (var_squareparen_index > 0) {
                                    var = var.substring(0, var_squareparen_index);
                                }
                                if (debug_on) {
                                    DebugPrint("currentOverrideName=" + currentOverrideName + ", var =" + var);
                                }

                                currentType.VarnameOverridesHashTable.put(var, currentOverrideName);
                                currentOverrideName = null;
                            }
                        }
                        if (currentAttributeInfo != null) {
                            String var = parseString;
                            int var_semicolon_index = var.indexOf(';');
                            if (var_semicolon_index > 0) {
                                var = var.substring(0, var_semicolon_index);
                                int var_space_index = var.lastIndexOf(' ');
                                if (var_space_index > 0) {
                                    var = var.substring(var_space_index + 1);
                                }
                                int var_squareparen_index = var.indexOf('[');
                                if (var_squareparen_index > 0) {
                                    var = var.substring(0, var_squareparen_index);
                                }
                                if (debug_on) {
                                    DebugPrint("currentAttributeInfo=" + currentAttributeInfo + ", var =" + var);
                                }
                                currentType.VarnameAttributeInfoHashTable.put(var, currentAttributeInfo);
                            }
                        }
                        if (null != currentType) {
                            currentType.RawInfo += parseString + "\n";
                        }
                        continue;
                    }

                } catch (Exception e) {
                    ErrorPrint("startingParseString=" + startingParseString);
                    ErrorPrint("parseString=" + parseString);
//                    debug_on = true;
//                    String s2 = ReplaceDefinedValues(parseString, 0, null);
//                    ErrorPrint("s2 = " + s2);
                    ErrorPrint("line = " + lineNumber);
                    final Set<String> definedValueskeySet = definedValues.keySet();
                    List<String> definedValuesKeyList = new ArrayList<>(definedValueskeySet);
                    Collections.sort(definedValuesKeyList);
                    ErrorPrint("definedValuesKeyList=" + definedValuesKeyList);
                    if (interrupt_loading) {
                        throw e;
                    } else {
                        e.printStackTrace();
                    }
                }
            }
            if (null != m_loadingPanel) {
                m_loadingPanel.set_bytes_read(m_loadingPanel.get_content_length());
                m_loadingPanel.updateDisplay();
            }
            this.lhui_name = file_loader.name;
            this.lhui_total = file_loader.content_length;
            this.lhui_part = curFileLineNumber;
            this.lhui_update();
        } catch (Exception e) {
            ErrorPrint("line=" + lineNumber);
            ErrorPrint("parseString=" + parseString);
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
        } finally {
            if (brace_count != 0) {
                ErrorPrint("End of " + file_loader.name + " reached with non matching braces. brace_count=" + brace_count);
                System.err.println("currentType=" + currentType);
            }
            if (paren_count != 0) {
                ErrorPrint("End of " + file_loader.name + " reached with non matching parenthases. paren_count=" + paren_count);
                System.err.println("currentType=" + currentType);
            }
            if (null != file_loader) {
                file_loader.close();
            }
        }
//	curFileName=null;
//	curFileLineNumber=0;
    }

    static public void ClearStaticData() {
        try {
            //NMLConnection.ClearStaticData();
            interrupt_loading = false;
            if (null != m_errlogConnection) {
                m_errlogConnection.disconnect();
                m_errlogConnection = null;
            }
            m_structInfoHashTable.clear();
            m_structInfoByNameHashTable.clear();
            if (!CodeGenCommon.get_generate_all_enum_symbol_lookups()) {
                m_enumInfoHashTable.clear();
            }
            m_loadedPreDefinedTypes.clear();
            headerFiles.removeAllElements();
            default_types_added = false;
            startingDefinedValues = null;
            if (UseDefaultTypes) {
                AddDefaultTypes();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    private void recheck_defined_values() {
        try {
            if (!definedValues.containsKey("JAVA_DIAG_APPLET")) {
                DefinedValue dv = new DefinedValue();
                dv.name = "JAVA_DIAG_APPLET";
                dv.value = "1";
                definedValues.put(dv.name, dv);
            }
            if (!definedValues.containsKey("NMLTYPE")) {
                DefinedValue dv = new DefinedValue();
                dv.name = "NMLTYPE";
                dv.value = "long";
                definedValues.put(dv.name, dv);
            }
            if (!definedValues.containsKey("__CPLUSPLUS__")) {
                DefinedValue dv = new DefinedValue();
                dv.name = "__CPLUSPLUS__";
                dv.value = "1";
                definedValues.put(dv.name, dv);
            }
            if (!definedValues.containsKey("__cplusplus")) {
                DefinedValue dv = new DefinedValue();
                dv.name = "__cplusplus";
                dv.value = "1";
                definedValues.put(dv.name, dv);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public int LoadPredefinedTypeFile(String type_file_string) {
        String file_to_load = null;
        URL_and_FileLoader typeFileLoader = null;
        try {
            if (debug_on) {
                DebugPrint("LoadPredefinedTypeFile(String type_file_string=" + type_file_string + ")");
            }
            if (m_loadedPreDefinedTypes.containsKey(type_file_string)) {
                Hashtable ht = (Hashtable) m_loadedPreDefinedTypes.get(type_file_string);
                if (ht != null) {
                    if (null != definedValues) {
                        definedValues.putAll(ht);
                    } else {
                        definedValues = new Hashtable(ht);
                    }
                    recheck_defined_values();
                }
                return 0;
            }
            if (type_file_string.startsWith("http://") || type_file_string.startsWith("ftp:")) {
                file_to_load = type_file_string;
            } else {
                if (previous_url_loaded == null) {
                    file_to_load = "http://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
                }
                if (type_file_string.charAt(0) == '/') {
                    if (previous_url_loaded.startsWith("http:")) {
                        file_to_load = "http://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
                    } else if (previous_url_loaded.startsWith("ftp:")) {
                        file_to_load = "ftp://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
                    } else {
                        file_to_load = type_file_string;
                    }
                } else {
                    int last_slash = 0;
                    if (previous_url_loaded != null) {
                        last_slash = previous_url_loaded.lastIndexOf('/');
                    }
                    if (last_slash > 0) {
                        file_to_load = previous_url_loaded.substring(0, last_slash + 1) + type_file_string;
                    } else {
                        file_to_load = type_file_string;
                    }
                }
            }
            if (null == file_to_load) {
                return -1;
            }
            typeFileLoader = new URL_and_FileLoader(file_to_load);
            if (!typeFileLoader.TryNameSucceeded) {
                typeFileLoader = new URL_and_FileLoader(type_file_string);
            }
            last_file_loader = typeFileLoader;
            curFileLineNumber = 0;
            this.lhui_name = curFileName = last_file_loader.name;
            this.lhui_total = last_file_loader.content_length;
            this.lhui_part = curFileLineNumber;
            this.lhui_update();
            if (!typeFileLoader.TryNameSucceeded) {
                if (debug_on) {
                    Thread.dumpStack();
                }
                ErrorPrint("Can't open file \"" + type_file_string + "\".");
                ErrorPrint("previous_url_loaded=" + previous_url_loaded);
                ErrorPrint("URL_and_FileLoader.SearchPath = " + URL_and_FileLoader.get_SearchPath());
                AddHeader(file_to_load);
                typeFileLoader = null;
            } else {
                if (null != m_loadingPanel) {
                    m_loadingPanel.set_URLname(file_to_load);
                    m_loadingPanel.set_content_length(typeFileLoader.content_length);
                    m_loadingPanel.set_bytes_read(0);
                    m_loadingPanel.updateDisplay();
                    if (debug_on) {
                        DebugPrint("Loading " + m_loadingPanel.get_URLname() + " . . .");
                    }
                }
                AddHeader(file_to_load);
                ParseInfoStream(typeFileLoader, false);
                //DebugPrint("definedValues="+definedValues);
                m_loadedPreDefinedTypes.put(type_file_string, definedValues);
                if (file_to_load.compareTo(type_file_string) != 0) {
                    m_loadedPreDefinedTypes.put(file_to_load, definedValues);
                }
                if (typeFileLoader.name.compareTo(type_file_string) != 0
                        && typeFileLoader.name.compareTo(file_to_load) != 0) {
                    m_loadedPreDefinedTypes.put(typeFileLoader.name, definedValues);
                }
            }
            return 0;
        } catch (Exception e) {
            ErrorPrint("type_file_string =" + type_file_string);
            ErrorPrint("file_to_load =" + file_to_load);
            ErrorPrint("typeFileLoader =" + typeFileLoader);
            e.printStackTrace();
        }
        return -1;
    }
    private static boolean nml_hh_dir_checked = false;
    // @SuppressWarnings("unchecked")

    public int LoadAuxTypeFile(String type_file_string) {
        String file_to_load = null;
        URL_and_FileLoader typeFileLoader = null;
        try {
            if (debug_on) {
                DebugPrint("LoadAuxTypeFile(String type_file_string=" + type_file_string + ")");
            }
            if (m_loadedPreDefinedTypes.containsKey(type_file_string)) {
                Hashtable ht = (Hashtable) m_loadedPreDefinedTypes.get(type_file_string);
                if (ht != null) {
                    if (null != definedValues) {
                        definedValues.putAll(ht);
                    } else {
                        definedValues = new Hashtable(ht);
                    }
                    recheck_defined_values();
                }
                return 0;
            }
            if (type_file_string.startsWith("http://") || type_file_string.startsWith("ftp:")) {
                file_to_load = type_file_string;
            } else {
                if (m_cmd_write_Connection != null) {
//                    if (previous_url_loaded == null) {
//                        file_to_load = "http://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
//                    }
                    if (type_file_string.charAt(0) == '/') {
                        if (previous_url_loaded != null && previous_url_loaded.startsWith("http:")) {
                            file_to_load = "http://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
                        } else if (previous_url_loaded != null && previous_url_loaded.startsWith("ftp:")) {
                            file_to_load = "ftp://" + m_cmd_write_Connection.get_host() + ":" + HTTPport + type_file_string;
                        } else {
                            file_to_load = type_file_string;
                        }
                    } else {
                        int last_slash = 0;
                        if (null != previous_url_loaded) {
                            last_slash = previous_url_loaded.lastIndexOf('/');
                        }
                        if (last_slash > 0) {
                            file_to_load = previous_url_loaded.substring(0, last_slash + 1) + type_file_string;
                        } else {
                            file_to_load = type_file_string;
                        }
                    }
                } else {
                    file_to_load = type_file_string;
                }
            }
            if (null == file_to_load) {
                return -1;
            }
            if (!nml_hh_dir_checked) {
                nml_hh_dir_checked = true;
                String nml_hh_dir = StringFuncs.getenv("NML_HH_DIR");
                if (nml_hh_dir != null) {
                    URL_and_FileLoader.AddToSearchPath(nml_hh_dir);
                }
            }
            typeFileLoader = new URL_and_FileLoader(file_to_load);
            last_file_loader = typeFileLoader;
            curFileLineNumber = 0;
            this.lhui_name = curFileName = last_file_loader.name;
            this.lhui_total = last_file_loader.content_length;
            this.lhui_part = curFileLineNumber;
            this.lhui_update();
            if (!typeFileLoader.TryNameSucceeded) {
                Thread.dumpStack();
                ErrorPrint("Can't open file \"" + type_file_string + "\".");
                AddHeader(file_to_load);
                typeFileLoader = null;
            } else {
                if (null != m_loadingPanel) {
                    m_loadingPanel.set_URLname(file_to_load);
                    m_loadingPanel.set_content_length(typeFileLoader.content_length);
                    m_loadingPanel.set_bytes_read(0);
                    m_loadingPanel.updateDisplay();
                    if (debug_on) {
                        DebugPrint("Loading " + m_loadingPanel.get_URLname() + " . . .");
                    }
                }
                AddHeader(file_to_load);
                Hashtable orig_defined_values = definedValues;
                //definedValues = null;
                adding_aux_channel = true;
                ParseInfoStream(typeFileLoader, false);
                adding_aux_channel = false;
                m_loadedPreDefinedTypes.put(type_file_string, definedValues);
                definedValues = new Hashtable();
                if (null != orig_defined_values) {
                    definedValues.putAll(orig_defined_values);
                }
                recheck_defined_values();
            }
            return 0;
        } catch (Exception e) {
            ErrorPrint("type_file_string =" + type_file_string);
            ErrorPrint("file_to_load =" + file_to_load);
            ErrorPrint("typeFileLoader =" + typeFileLoader);
            e.printStackTrace();
        }
        adding_aux_channel = false;
        return -1;
    }

    public int LoadPredefinedTypes() {
        int i;
        String type_file_string = null;
        if (debug_on) {
            DebugPrint("");
            DebugPrint("LoadPredefinedTypes()");
            DebugPrint("predefined_type_files=" + predefined_type_files);
        }
        if (null == predefined_type_files) {
            return 0;
        }
        int num_type_files = predefined_type_files.size();
        if (0 == num_type_files) {
            return 0;
        }
        for (i = 0; i < num_type_files; i++) {
            try {
                type_file_string = (String) predefined_type_files.elementAt(i);
                LoadPredefinedTypeFile(type_file_string);
            } catch (Exception e) {
                ErrorPrint("Error parsing predefined type file" + type_file_string);
                e.printStackTrace();
            }
        }
        return 0;
    }

    public int LoadAuxTypes() {
        int i;
        String type_file_string = null;
        if (debug_on) {
            DebugPrint("");
            DebugPrint("LoadAuxTypes()");
            DebugPrint("aux_type_files=" + aux_type_files);
        }
        if (null == aux_type_files) {
            return 0;
        }
        int num_type_files = aux_type_files.size();
        if (0 == num_type_files) {
            return 0;
        }
        for (i = 0; i < num_type_files; i++) {
            try {
                type_file_string = (String) aux_type_files.elementAt(i);
                String fileNameOrig = curFileName;
                int fileLineNumberOrig = curFileLineNumber;
                String fileLineTextOrig = curFileLineText;
                LoadAuxTypeFile(type_file_string);
                curFileName = fileNameOrig;
                curFileLineNumber = fileLineNumberOrig;
                curFileLineText = fileLineTextOrig;
            } catch (Exception e) {
                ErrorPrint("Error parsing predefined type file" + type_file_string);
                e.printStackTrace();
            }
        }
        this.AddAllAuxMessagesFromHeader();
        return 0;
    }
    // @SuppressWarnings("unchecked")

    public void AddHeader(String header) {
        if (null == header) {
            return;
        }
        if (header.length() < 1) {
            return;
        }
        if (!header.endsWith(".h")
                && !header.endsWith(".H")
                && !header.endsWith(".hh")
                && !header.endsWith(".HH")
                && !header.endsWith(".hpp")
                && !header.endsWith(".HPP")) {
            return;
        }
        int fslash_index = header.lastIndexOf('/');
        if (fslash_index >= 0) {
            header = header.substring(fslash_index + 1);
        }
        int bslash_index = header.lastIndexOf('\\');
        if (bslash_index >= 0) {
            header = header.substring(bslash_index + 1);
        }
        int colon_index = header.lastIndexOf(':');
        if (colon_index > 0) {
            header = header.substring(colon_index + 1);
        }
        for (int i = 0; i < headerFiles.size(); i++) {
            String stored_header = (String) headerFiles.elementAt(i);
            if (stored_header.equals(header)) {
                return;
            }
        }
        headerFiles.addElement(header);
    }
    // @SuppressWarnings("unchecked")

    public void AddAuxInputType(String valueString) {
        try {
            if (application_type == RCS_DIAGNOSTICS_APPLICATION_TYPE || always_perform_final_pass) {
                //predefined_type_files.addElement(valueString);
                if (null == aux_type_files) {
                    aux_type_files = new Vector();
                }
                aux_type_files.addElement(valueString);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    // @SuppressWarnings("unchecked")

    public int LoadInfo(String new_cmdsTypeFile, String new_statsTypeFile) {
        URL_and_FileLoader cmdsTypeFileLoader = null;
        URL_and_FileLoader statsTypeFileLoader = null;
        cmdsTypeFile = new_cmdsTypeFile;
        statsTypeFile = new_statsTypeFile;
        String file_to_load = null;
        try {
            if (debug_on) {
                /*
                DebugPrint(Thread.currentThread());
                new Throwable().printStackTrace(System.out);; */
                DebugPrint("ModuleInfo.LoadInfo(" + new_cmdsTypeFile + ", " + new_statsTypeFile + ") : Name=" + Name);
            }
            if (null != cmdsTypeFile) {
                file_to_load = cmdsTypeFile;
                m_cmd_structInfoHashTable = null;
                cmdsTypeFileLoader = new URL_and_FileLoader(file_to_load);
                last_file_loader = cmdsTypeFileLoader;
                curFileLineNumber = 0;
                curFileLineText = "";
                this.lhui_name = curFileName = last_file_loader.name;
                this.lhui_total = last_file_loader.content_length;
                this.lhui_part = curFileLineNumber;
                this.lhui_update();
                if (!cmdsTypeFileLoader.TryNameSucceeded) {
                    ErrorPrint("Can't open file \"" + file_to_load + "\".");
                    AddHeader(file_to_load);
                    cmdsTypeFileLoader = null;
                } else {
                    if (null != m_loadingPanel) {
                        m_loadingPanel.set_URLname(file_to_load);
                        m_loadingPanel.set_content_length(cmdsTypeFileLoader.content_length);
                        m_loadingPanel.set_bytes_read(0);
                        m_loadingPanel.updateDisplay();
                        if (debug_on) {
                            DebugPrint("Loading " + m_loadingPanel.get_URLname() + " . . .");
                        }
                    }
                    this.lhui_name = file_to_load;
                    this.lhui_total = cmdsTypeFileLoader.content_length;
                    this.lhui_part = 0;
                    this.lhui_update();
                    AddHeader(cmdsTypeFile);
                    ParseInfoStream(cmdsTypeFileLoader, true);
                    m_loadedPreDefinedTypes.put(cmdsTypeFile, definedValues);
                    m_cmd_structInfoHashTable = new Hashtable(m_structInfoHashTable);
                    cmdFormatFunction = FormatFunction;
                }
            }
        } catch (Exception e) {
            diagapplet.utils.DiagError.println("\r\nCan't load cmd info for module=" + Name + " cmdsTypeFile =" + cmdsTypeFile + " statsTypeFile=" + statsTypeFile + "\r\n");
            e.printStackTrace();
        }

        try {
            if (null != statsTypeFile) {
                m_stat_structInfoHashTable = null;
                if (statsTypeFile.equals(cmdsTypeFile)) {
                    return 0;
                }
                file_to_load = statsTypeFile;
                statsTypeFileLoader = new URL_and_FileLoader(file_to_load);
                last_file_loader = statsTypeFileLoader;
                curFileLineNumber = 0;
                curFileLineText = "";
                this.lhui_name = curFileName = last_file_loader.name;
                this.lhui_total = last_file_loader.content_length;
                this.lhui_part = curFileLineNumber;
                this.lhui_update();
                if (!statsTypeFileLoader.TryNameSucceeded) {
                    ErrorPrint("Can't open file \"" + file_to_load + "\".");
                    AddHeader(file_to_load);
                    statsTypeFileLoader = null;
                } else {
                    if (null != m_loadingPanel) {
                        m_loadingPanel.set_URLname(file_to_load);
                        m_loadingPanel.set_content_length(statsTypeFileLoader.content_length);
                        m_loadingPanel.set_bytes_read(0);
                        m_loadingPanel.updateDisplay();
                        if (debug_on) {
                            DebugPrint("Loading " + m_loadingPanel.get_URLname() + " . . .");
                        }
                    }
                    this.lhui_name = file_to_load;
                    this.lhui_total = statsTypeFileLoader.content_length;
                    this.lhui_part = 0;
                    this.lhui_update();
                    AddHeader(statsTypeFile);
                    ParseInfoStream(statsTypeFileLoader, false);
                    m_loadedPreDefinedTypes.put(statsTypeFile, definedValues);
                    statFormatFunction = FormatFunction;
                    m_stat_structInfoHashTable = new Hashtable(m_structInfoHashTable);
                }
            }
        } catch (Exception e) {
            diagapplet.utils.DiagError.println("\r\nCan't load stats info for module=" + Name + " cmdsTypeFile =" + cmdsTypeFile + " statsTypeFile=" + statsTypeFile + "\r\n");
            e.printStackTrace();
        }
        return 0;
    }
    // @SuppressWarnings("unchecked")

    public int LoadInfo() throws Exception {
        String varString;
        int indexQuoteBegin;
        int indexQuoteEnd;
        try {
            if (debug_on) {
                /*DebugPrint(Thread.currentThread());
                new Throwable().printStackTrace(System.out);; */
                DebugPrint("ModuleInfo.Name = " + Name);
                DebugPrint("ModuleInfo.LoadInfo() : Info = ");
                DebugPrint(Info);
            }
            StringTokenizer tokenizer = new StringTokenizer(Info, ";");
            while (tokenizer.hasMoreTokens()) {
                if (Thread.interrupted()) {
                    interrupt_loading = true;
                    throw new Exception("Thread.interrupted() returned true\n");
                }
                if (interrupt_loading) {
                    throw new Exception("interrupt_loading=true\n");
                }
                if (interrupt_loading) {
                    if (debug_on) {
                        new Throwable().printStackTrace(System.out);
                        ;
                        DebugPrint("ModuleInfo.LoadInfo() : interrupt_loading = " + interrupt_loading);
                    }
                    return -1;
                }
                varString = tokenizer.nextToken();
                if (varString == null) {
                    break;
                }
                if (varString.length() < 1) {
                    continue;
                }
                while (true) {
                    if (Thread.interrupted()) {
                        interrupt_loading = true;
                        throw new Exception("Thread.interrupted() returned true\n");
                    }
                    if (interrupt_loading) {
                        throw new Exception("interrupt_loading=true\n");
                    }
                    if (interrupt_loading) {
                        if (debug_on) {
                            new Throwable().printStackTrace(System.out);
                            ;
                            DebugPrint("ModuleInfo.LoadInfo() : interrupt_loading = " + interrupt_loading);
                        }
                        return -1;
                    }
                    if (varString.charAt(0) != ' '
                            && varString.charAt(0) != '\t'
                            && varString.charAt(0) != '\r'
                            && varString.charAt(0) != '\n') {
                        break;
                    }
                    varString = varString.substring(1);
                    if (varString.length() < 1) {
                        break;
                    }
                }
                int index = varString.indexOf('=');
                if (index < 0) {
                    continue;
                }
                String nameString = varString.substring(0, index);
                String valueString = varString.substring(index + 1);
                if (debug_on) {
                    DebugPrint("nameString = " + nameString);
                    DebugPrint("valueString = " + valueString);
                }
                if (valueString.equalsIgnoreCase("null") || valueString.equalsIgnoreCase("\"null\"")) {
                    continue;
                }
                while (true) {
                    if (Thread.interrupted()) {
                        interrupt_loading = true;
                        throw new Exception("Thread.interrupted() returned true\n");
                    }
                    if (interrupt_loading) {
                        throw new Exception("interrupt_loading=true\n");
                    }
                    if (null == valueString) {
                        break;
                    }
                    if (valueString.length() < 1) {
                        break;
                    }
                    if (valueString.charAt(0) != ' '
                            && valueString.charAt(0) != '\t'
                            && valueString.charAt(0) != '\r'
                            && valueString.charAt(0) != '\n') {
                        break;
                    }
                    valueString = valueString.substring(1);
                }
                if (nameString.equalsIgnoreCase("external_include_dir")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (null == externalIncludeDirectories) {
                        externalIncludeDirectories = new Vector();
                    }
                    externalIncludeDirectories.addElement(valueString);
                } else if (nameString.equalsIgnoreCase("external_library")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (null == externalLibraries) {
                        externalLibraries = new Vector();
                    }
                    externalLibraries.addElement(valueString);
                } else if (nameString.equalsIgnoreCase("base_cmd")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (null == cmdsBaseClass) {
                        cmdsBaseClass = new Vector();
                    }
                    cmdsBaseClass.addElement(valueString);
                    if (null == cmdsAvailable) {
                        cmdsAvailable = new Vector();
                    }
                    boolean base_cmd_in_cmds_available = false;
                    for (int i = 0; i < cmdsAvailable.size(); i++) {
                        String cmd = (String) cmdsAvailable.elementAt(i);
                        if (cmd.startsWith(valueString)) {
                            base_cmd_in_cmds_available = true;
                            break;
                        }
                    }
                    if (!base_cmd_in_cmds_available) {
                        cmdsAvailable.addElement(valueString);
                    }

                } else if (nameString.equalsIgnoreCase("release_library")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    releaseLibrary = valueString;
                } else if (nameString.equalsIgnoreCase("release_include_dir")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    releaseIncludeDirectory = valueString;
                } else if (nameString.equalsIgnoreCase("subsystem")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    subsystem = valueString;
                } else if (nameString.equalsIgnoreCase("child")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (importing) {
                        if (null == baseClassChildrenNames) {
                            baseClassChildrenNames = new Vector();
                        }
                        baseClassChildrenNames.addElement(valueString);
                        if (null != subsystem) {
                            valueString = subsystem + "_" + valueString;
                        }
                    }
                    if (IsAncestor(valueString)) {
                        ErrorPrint("child name can not equal ancestor parent: Name=" + Name + ", valueString=" + valueString);
                    } else {
                        children_names.addElement(valueString);
                        if (debug_on) {
                            DebugPrint("adding child " + valueString + " to " + Name);
                            DebugPrint("children_names=" + children_names);
                            DebugPrint("children_names.size()=" + children_names.size());
                            DebugPrint("children_names.elementAt(children_names.size()-1)=" + children_names.elementAt(children_names.size() - 1));
                        }
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("base_child")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (null == baseClassChildrenNames) {
                        baseClassChildrenNames = new Vector();
                    }
                    baseClassChildrenNames.addElement(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("host")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    setHost(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("class_name")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        moduleClassName = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("base_class_name")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        baseClassName = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("base_name")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        baseModuleName = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("errlog_port")) {
                    if (!no_errlog) {
                        m_errlogConnection.set_port(rcs.utils.StrToInt.convert(valueString));
                        errlog_on_this_host = true;
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("errlog_buffer_number")) {
                    if (!no_errlog) {
                        m_errlogConnection.set_buffer_number(rcs.utils.StrToInt.convert(valueString));
                        errlog_on_this_host = true;
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("errlog_buffer_name")) {
                    if (!no_errlog) {
                        m_errlogConnection.set_buffer_name(valueString);
                        errlog_on_this_host = true;
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("no_cmd")) {
                    no_cmd = true;
                } else if (nameString.equalsIgnoreCase("no_stat")) {
                    no_stat = true;
                } else if (nameString.equalsIgnoreCase("no_errlog")) {
                    no_errlog = true;
                } else if (nameString.equalsIgnoreCase("cmd_port") && !no_cmd) {
                    if (double_buffer_nml) {
                        m_cmd_write_Connection.set_port(rcs.utils.StrToInt.convert(valueString));
                    }
                    m_cmd_read_Connection.set_port(rcs.utils.StrToInt.convert(valueString));
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_buffer_number") && !no_cmd) {
                    if (double_buffer_nml) {
                        m_cmd_write_Connection.set_buffer_number(rcs.utils.StrToInt.convert(valueString));
                    }
                    m_cmd_read_Connection.set_buffer_number(rcs.utils.StrToInt.convert(valueString));
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_buffer_name") && !no_cmd) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (double_buffer_nml) {
                        m_cmd_write_Connection.set_buffer_name(valueString);
                    }
                    m_cmd_read_Connection.set_buffer_name(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_configuration_file") && !no_cmd) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (valueString.startsWith("getenv(")) {
                        int pindex = valueString.indexOf(')');
                        if (pindex > 7) {
                            valueString = valueString.substring(7, pindex);
                            valueString = diagapplet.CodeGen.StringFuncs.getenv(valueString);
                        }
                    }
                    if (double_buffer_nml) {
                        m_cmd_write_Connection.set_configuration_file(valueString);
                    }
                    m_cmd_read_Connection.set_configuration_file(valueString);
                    if (null == NMLConfigurationFile) {
                        NMLConfigurationFile = valueString;
                    }
                    DefaultNMLConfigurationFile = valueString;
                    if (debug_on) {
                        DebugPrint(varString);
                        DebugPrint("ModuleInfo.NMLConfiguration = " + NMLConfigurationFile);
                    }

                    continue;
                } else if (nameString.equalsIgnoreCase("SourceCodeDirectory")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    SourceCodeDirectory = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_name_pattern")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    cmd_name_pattern = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_name_exclude_pattern")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    cmd_name_exclude_pattern = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("preset_x")) {
                    x = rcs.utils.StrToInt.convert(valueString);
                    preset_x = true;
                } else if (nameString.equalsIgnoreCase("preset_y")) {
                    y = rcs.utils.StrToInt.convert(valueString);
                    preset_y = true;
                } else if (nameString.equalsIgnoreCase("nml_configuration_file")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (valueString.startsWith("getenv(")) {
                        int pindex = valueString.indexOf(')');
                        if (pindex > 7) {
                            valueString = valueString.substring(7, pindex);
                            valueString = diagapplet.CodeGen.StringFuncs.getenv(valueString);
                        }
                    }
                    if (!no_cmd) {
                        if (null != m_cmd_read_Connection) {
                            m_cmd_read_Connection.set_configuration_file(valueString);
                        }
                        if (double_buffer_nml
                                && null != m_cmd_write_Connection) {
                            m_cmd_write_Connection.set_configuration_file(valueString);
                        }
                    }
                    if (!no_stat) {
                        if (null != m_stat_read_Connection) {
                            m_stat_read_Connection.set_configuration_file(valueString);
                        }
                        if (double_buffer_nml
                                && null != m_stat_write_Connection) {
                            m_stat_write_Connection.set_configuration_file(valueString);
                        }
                    }
                    if (!no_errlog && null != m_errlogConnection) {
                        m_errlogConnection.set_configuration_file(valueString);
                    }
                    NMLConfigurationFile = valueString;
                    DefaultNMLConfigurationFile = valueString;
                    if (debug_on) {
                        DebugPrint(varString);
                        DebugPrint("ModuleInfo.NMLConfiguration = " + NMLConfigurationFile);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("cmd_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    cmdsTypeFile = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("base_cmd_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    baseClassCmdsTypeFile = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("stat_port") && !no_stat) {
                    m_stat_read_Connection.set_port(rcs.utils.StrToInt.convert(valueString));
                    if (double_buffer_nml) {
                        m_stat_write_Connection.set_port(rcs.utils.StrToInt.convert(valueString));
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("stat_buffer_number") && !no_stat) {
                    m_stat_read_Connection.set_buffer_number(rcs.utils.StrToInt.convert(valueString));
                    if (double_buffer_nml) {
                        m_stat_write_Connection.set_buffer_number(rcs.utils.StrToInt.convert(valueString));
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("module_number")) {
                    module_number = rcs.utils.StrToInt.convert(valueString);
                    if (module_number > max_module_number) {
                        max_module_number = module_number;
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("stat_buffer_name") && !no_stat) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    m_stat_read_Connection.set_buffer_name(valueString);
                    if (double_buffer_nml) {
                        m_stat_write_Connection.set_buffer_name(valueString);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("stat_configuration_file") && !no_stat) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (valueString.startsWith("getenv(")) {
                        int pindex = valueString.indexOf(')');
                        if (pindex > 7) {
                            valueString = valueString.substring(7, pindex);
                            valueString = diagapplet.CodeGen.StringFuncs.getenv(valueString);
                        }
                    }
                    m_stat_read_Connection.set_configuration_file(valueString);
                    if (double_buffer_nml) {
                        m_stat_write_Connection.set_configuration_file(valueString);
                    }
                    DefaultNMLConfigurationFile = valueString;
                    if (null == NMLConfigurationFile) {
                        NMLConfigurationFile = valueString;
                        if (debug_on) {
                            DebugPrint(varString);
                            DebugPrint("ModuleInfo.NMLConfiguration = " + NMLConfigurationFile);
                        }
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("stat_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    statsTypeFile = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("base_stat_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    baseClassStatsTypeFile = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("pre_defined_types")) {

                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    boolean already_added = false;
                    for (int i = 0; i < predefined_type_files.size(); i++) {
                        String str = (String) predefined_type_files.elementAt(i);
                        if (str.equals(valueString)) {
                            already_added = true;
                            break;
                        }
                    }
                    if (!already_added) {
                        predefined_type_files.addElement(valueString);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("pre-defined_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    predefined_type_files.addElement(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("predefined_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    predefined_type_files.addElement(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("MainLoopName")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    MainLoopName = valueString;
                    continue;
                } else if (nameString.equalsIgnoreCase("cycle_time")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    try {
                        cycle_time = Double.valueOf(valueString).doubleValue();
                    } catch (Exception e) {
                        if (interrupt_loading) {
                            throw e;
                        } else {
                            e.printStackTrace();
                        }
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("update_next_aux_every_cycle")) {
                    if (valueString.equalsIgnoreCase("TRUE")
                            || valueString.startsWith("1")) {
                        update_next_aux_every_cycle = true;
                    }
                } else if (nameString.equalsIgnoreCase("aux_input")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    String inputs[] = valueString.split("[, \t]+");
                    for (String input : inputs) {
                        AddAuxInput(input);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("aux_available_message_filter")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    addAuxAvailableMessageFilter(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("aux_input_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    AddAuxInputType(valueString);
                    continue;
                } else if (nameString.equalsIgnoreCase("aux_output")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    String outputs[] = valueString.split("[, \t]+");
                    for (String output : outputs) {
                        AddAuxOutput(output);
                    }
                    continue;
                } else if (nameString.equalsIgnoreCase("aux_output_types")) {
                    indexQuoteBegin = valueString.indexOf('"');
                    indexQuoteEnd = valueString.lastIndexOf('"');
                    if (indexQuoteBegin > -1 && indexQuoteEnd > -1 && indexQuoteEnd > indexQuoteBegin) {
                        valueString = valueString.substring(indexQuoteBegin + 1, indexQuoteEnd);
                    }
                    if (application_type == RCS_DIAGNOSTICS_APPLICATION_TYPE
                            || always_perform_final_pass) {
                        //predefined_type_files.addElement(valueString);
                        if (null == aux_type_files) {
                            aux_type_files = new Vector();
                        }
                        aux_type_files.addElement(valueString);
                    }
                    continue;
                } else {
                    ErrorPrint(nameString + " not recognized.\n");
                    continue;
                }
            }
            if (debug_on) {
                DebugPrint("previous_url_loaded = " + previous_url_loaded);
            }
            if (previous_url_loaded != null && previous_url_loaded.startsWith("http:")) {
                int last_slash_index = previous_url_loaded.lastIndexOf("/");
                if (last_slash_index > 0) {
                    URL_and_FileLoader.current_directory = previous_url_loaded.substring(0, last_slash_index + 1);
                }
            }
            if (debug_on) {
                DebugPrint("URL_and_FileLoader.current_directory = " + URL_and_FileLoader.current_directory);
            }
            if (null == moduleClassName && Name != null) {
                moduleClassName = Name.toUpperCase() + "_MODULE";
            }
            try {
                if (!no_cmd) {
                    if (m_cmd_read_Connection != null) {
                        if (m_cmd_read_Connection.get_buffer_name() == null) {
                            m_cmd_read_Connection.set_buffer_name(Name + "_cmd");
                        }
                        if (null == m_cmd_read_Connection.get_configuration_file()
                                && null != DefaultNMLConfigurationFile) {
                            m_cmd_read_Connection.set_configuration_file(DefaultNMLConfigurationFile);
                        }
                        if (null != m_cmd_read_Connection.get_configuration_file()) {
                            if (m_cmd_read_Connection.get_process_name() == null) {
                                m_cmd_read_Connection.set_process_name("jdiag");
                            }
                            try {
                                if (read_nml_configs) {
                                    m_cmd_read_Connection.ReadNMLConfigurationFileNoThrow();
                                }
                                host = m_cmd_read_Connection.get_host();
                            } catch (Exception e) {
                                if (interrupt_loading) {
                                    throw e;
                                } else {
                                    e.printStackTrace();
                                }
                            }
                        } else {
                            diagapplet.utils.DiagError.println("Module : " + Name + " has null cmd configuration file.");
                        }
                    }
                    if (!double_buffer_nml) {
                        m_cmd_write_Connection = m_cmd_read_Connection;
                    } else if (m_cmd_write_Connection != null) {
                        if (null == m_cmd_write_Connection.get_configuration_file()
                                && null != DefaultNMLConfigurationFile) {
                            m_cmd_write_Connection.set_configuration_file(DefaultNMLConfigurationFile);
                        }
                        if (null != m_cmd_write_Connection.get_configuration_file()) {
                            if (m_cmd_write_Connection.get_buffer_name() == null) {
                                m_cmd_write_Connection.set_buffer_name(Name + "_cmd");
                            }
                            if (m_cmd_write_Connection.get_process_name() == null) {
                                m_cmd_write_Connection.set_process_name("jdiag");
                            }
                            try {
                                if (read_nml_configs) {
                                    m_cmd_write_Connection.ReadNMLConfigurationFileNoThrow();
                                }
                                host = m_cmd_write_Connection.get_host();
                            } catch (Exception e) {
                                if (interrupt_loading) {
                                    throw e;
                                } else {
                                    e.printStackTrace();
                                }
                            }
                        }
                    }
                }
                if (!no_stat) {
                    if (m_stat_read_Connection != null) {
                        if (m_stat_read_Connection.get_buffer_name() == null) {
                            m_stat_read_Connection.set_buffer_name(Name + "_sts");
                        }
                        if (null == m_stat_read_Connection.get_configuration_file()
                                && null != DefaultNMLConfigurationFile) {
                            m_stat_read_Connection.set_configuration_file(DefaultNMLConfigurationFile);
                        }
                        if (null != m_stat_read_Connection.get_configuration_file()) {

                            if (m_stat_read_Connection.get_process_name() == null) {
                                m_stat_read_Connection.set_process_name("jdiag");
                            }
                            try {
                                if (read_nml_configs) {
                                    m_stat_read_Connection.ReadNMLConfigurationFileNoThrow();
                                }
                                host = m_stat_read_Connection.get_host();
                            } catch (Exception e) {
                                if (interrupt_loading) {
                                    throw e;
                                } else {
                                    e.printStackTrace();
                                }
                            }
                        } else {
                            diagapplet.utils.DiagError.println("Module : " + Name + " has null stat configuration file.");
                        }
                    }
                    if (!double_buffer_nml) {
                        m_stat_write_Connection = m_stat_read_Connection;
                    } else if (m_stat_write_Connection != null) {
                        if (null == m_stat_write_Connection.get_configuration_file()
                                && null != DefaultNMLConfigurationFile) {
                            m_stat_write_Connection.set_configuration_file(DefaultNMLConfigurationFile);
                        }
                        if (null != m_stat_write_Connection.get_configuration_file()) {
                            if (m_stat_write_Connection.get_buffer_name() == null) {
                                m_stat_write_Connection.set_buffer_name(Name + "_sts");
                            }
                            if (m_stat_write_Connection.get_process_name() == null) {
                                m_stat_write_Connection.set_process_name("jdiag");
                            }
                            try {
                                if (read_nml_configs) {
                                    m_stat_write_Connection.ReadNMLConfigurationFileNoThrow();
                                }
                                host = m_stat_write_Connection.get_host();
                            } catch (Exception e) {
                                if (interrupt_loading) {
                                    throw e;
                                } else {
                                    e.printStackTrace();
                                }
                            }
                        }
                    }
                }
                if (!no_errlog && m_errlogConnection != null) {

                    if (null != m_errlogConnection.get_configuration_file()) {
                        if (m_errlogConnection.get_process_name() == null) {
                            m_errlogConnection.set_process_name("jdiag");
                        }
                        try {
                            if (read_nml_configs) {
                                m_errlogConnection.ReadNMLConfigurationFileNoThrow();
                            }
                        } catch (Exception e) {
                            if (interrupt_loading) {
                                throw e;
                            } else {
                                e.printStackTrace();
                            }
                        }
                    }
                }
            } catch (Exception e) {
                if (interrupt_loading) {
                    throw e;
                } else {
                    e.printStackTrace();
                }
            }
            LoadPredefinedTypes();
            LoadAuxTypes();
            int retval = LoadInfo(cmdsTypeFile, statsTypeFile);

            return retval;
        } catch (Exception e) {
            diagapplet.utils.DiagError.println("\r\nCan't load info for module=" + Name + "\r\n");
            if (interrupt_loading) {
                throw e;
            } else {
                e.printStackTrace();
            }
            return -1;
        }
    }

    public int connect() {
        try {
            new_data_count = 0;
            try {
                if (!no_cmd) {
                    if (null != m_cmd_read_Connection) {
                        if (null == m_cmd_read_Connection.get_host()) {
                            m_cmd_read_Connection.set_host(host);
                        } else if (m_cmd_read_Connection.get_host().length() < 1) {
                            m_cmd_read_Connection.set_host(host);
                        }
                        if (m_cmd_read_Connection.connect() < 0) {
                            return -1;
                        }
                        if (!m_cmd_read_Connection.verify_bufname()) {
                            ErrorPrint("Verify bufname returned false for command for module " + Name + ", "
                                    + m_cmd_read_Connection.get_buffer_name() + "!=" + m_cmd_read_Connection.get_returned_buffer_name());
                            ErrorPrint("Check port numbers and buffer numbers in NML configuration file.");
                            return -1;
                        }
                    }
                    if (!double_buffer_nml) {
                        m_cmd_write_Connection = m_cmd_read_Connection;
                    } else if (null != m_cmd_write_Connection) {
                        if (null == m_cmd_write_Connection.get_host()) {
                            m_cmd_write_Connection.set_host(host);
                        } else if (m_cmd_write_Connection.get_host().length() < 1) {
                            m_cmd_write_Connection.set_host(host);
                        }
                        if (m_cmd_write_Connection.connect() < 0) {
                            return -1;
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
                return -1;
            }
            try {
                if (!no_stat) {
                    if (null != m_stat_read_Connection) {
                        if (null == m_stat_read_Connection.get_host()) {
                            m_stat_read_Connection.set_host(host);
                        } else if (m_stat_read_Connection.get_host().length() < 1) {
                            m_stat_read_Connection.set_host(host);
                        }
                        if (m_stat_read_Connection.connect() < 0) {
                            return -1;
                        }
                        if (!m_stat_read_Connection.verify_bufname()) {
                            diagapplet.utils.DiagError.println("Verify bufname returned false for command for module " + Name + ", "
                                    + m_stat_read_Connection.get_buffer_name() + "!=" + m_stat_read_Connection.get_returned_buffer_name());
                            diagapplet.utils.DiagError.println("Check port numbers and buffer numbers in NML configuration file.");
                            return -1;
                        }
                    }
                    if (!double_buffer_nml) {
                        m_stat_write_Connection = m_stat_read_Connection;
                    } else if (m_stat_write_Connection != null) {
                        if (null == m_stat_write_Connection.get_host()) {
                            m_stat_write_Connection.set_host(host);
                        } else if (m_stat_write_Connection.get_host().length() < 1) {
                            m_stat_write_Connection.set_host(host);
                        }
                        if (m_stat_write_Connection.connect() < 0) {
                            return -1;
                        }

                    }
                }
                is_connected = true;
            } catch (Exception e) {
                e.printStackTrace();
                return -1;
            }

            try {
                if (!no_errlog && null != m_errlogConnection) {
                    if (!m_errlogConnection.is_connected()) {
                        if (null == m_errlogConnection.get_host()) {
                            m_errlogConnection.set_host(host);
                        } else if (m_errlogConnection.get_host().length() < 1) {
                            m_errlogConnection.set_host(host);
                        }
                        m_errlogConnection.connectNoThrow();
                    }
                }
                is_connected = true;
            } catch (Exception e) {
                e.printStackTrace();
                return -1;
            }
        } catch (Throwable e) {
            e.printStackTrace();
            return -1;
        }
        return 0;
    }

//    public boolean login(String name, String passwd) {
//        try {
//            if (!is_connected) {
//                if (connect() < 0) {
//                    return false;
//                }
//            }
//            if (!no_cmd) {
//                if (!m_cmd_read_Connection.loginNoThrow(name, passwd)) {
//                    return false;
//                }
//                if (double_buffer_nml
//                        && !m_cmd_write_Connection.loginNoThrow(name, passwd)) {
//                    return false;
//                }
//            }
//            if (!no_stat) {
//                if (double_buffer_nml
//                        && !m_stat_write_Connection.loginNoThrow(name, passwd)) {
//                    return false;
//                }
//                if (!m_stat_read_Connection.loginNoThrow(name, passwd)) {
//                    return false;
//                }
//            }
//            if (!no_errlog && null != m_errlogConnection) {
//                m_errlogConnection.loginNoThrow(name, passwd);
//            }
//            return true;
//        } catch (Throwable e) {
//            if (null != e.getMessage()) {
//                diagapplet.utils.DiagError.println("\r\n" + e.getMessage() + "\r\n");
//            }
//            e.printStackTrace();
//            return false;
//        }
//    }
    public int disconnect() {
        new_data_count = 0;
        try {
            if (null != m_cmd_read_Connection) {
                m_cmd_read_Connection.disconnect();
            }
            if (double_buffer_nml
                    && null != m_cmd_write_Connection) {
                m_cmd_write_Connection.disconnect();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (null != m_stat_read_Connection) {
                m_stat_read_Connection.disconnect();
            }
            if (double_buffer_nml
                    && null != m_stat_write_Connection) {
                m_stat_write_Connection.disconnect();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (!no_errlog && null != m_errlogConnection) {
                m_errlogConnection.disconnect();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        is_connected = false;
        return 0;
    }

    public void AddAllAuxMessagesFromHeader() {
        try {
            if (null == ModuleInfo.m_structInfoHashTable) {
                return;
            }
            Enumeration elements_enum = ModuleInfo.m_structInfoHashTable.elements();
            while (elements_enum.hasMoreElements()) {
                StructureTypeInfo sti = (StructureTypeInfo) elements_enum.nextElement();
                boolean in_aux_messages = false;
                for (int i = 0; i < auxMessages.size(); i++) {
                    String s = (String) auxMessages.get(i);
                    if (s.startsWith(sti.getName() + "=")) {
                        in_aux_messages = true;
                        break;
                    }
                }
                if (sti.fromFileName == null) {
                    ErrorPrint("sti.fromFile == null");
                }
                boolean in_header = false;
                if (null != aux_type_files && null != sti && null != sti.fromFileName) {
                    for (int i = 0; i < this.aux_type_files.size(); i++) {
                        String h = (String) aux_type_files.elementAt(i);
                        if (sti.fromFileName.equals(h)) {
                            in_header = true;
                            break;
                        }
                    }
                }
                if (!in_aux_messages && in_header && sti.Id > 0) {
                    auxMessages.add(sti.getName() + "=" + sti.Id);
                    sti.on_aux_msg_list = true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void AddAllAuxMessages() {
        try {
            if (null == ModuleInfo.m_structInfoHashTable) {
                return;
            }
            Enumeration elements_enum = ModuleInfo.m_structInfoHashTable.elements();
            while (elements_enum.hasMoreElements()) {
                StructureTypeInfo sti = (StructureTypeInfo) elements_enum.nextElement();
                boolean in_aux_messages = false;
                for (int i = 0; i < auxMessages.size(); i++) {
                    String s = (String) auxMessages.get(i);
                    if (s.startsWith(sti.getName() + "=")) {
                        in_aux_messages = true;
                        break;
                    }
                }
                if (!in_aux_messages && sti.Id > 0) {
                    auxMessages.add(sti.getName() + "=" + sti.Id);
                    sti.on_aux_msg_list = true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public Vector getAuxMessages() {
        return auxMessages;
    }

    public boolean IsAncestor(String str) {
        try {
            if (Name.equals(str)) {
                return true;
            }
            ModuleInfo p = parent;
            while (p != null && p.Name != null && p != this && !p.Name.equals(Name)) {
                if (p.Name.equals(str)) {
                    return true;
                }
                p = p.parent;
            }
            return false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }
}
