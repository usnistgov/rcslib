/*
 * The NIST RCS (Real-time Control Systems) 
 * library is domain software, however it is preferred
 * that the following disclaimers be attached.
 *
 * Software Copywrite/Warranty Disclaimer
 *
 * This software was developed at the National Institute of Standards and 
 * Technology by employees of the Federal Government in the course of their 
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the 
 * domain. NIST Real-Time Control System software is an experimental *
 * system. NIST assumes no responsibility whatsoever for its use by other
 * parties, and makes no guarantees, expressed or implied, about its
 * quality, reliability, or any other characteristic. We would appreciate
 * acknowledgement if the software is used. This software can be 
 * redistributed and/or modified freely provided that any derivative works
 * bear some notice that they are derived from it, and any modified
 * versions bear some notice that they have been modified.
 *
 * To change this template, choose Tools | Templates
 * 
 */
package diagapplet.CodeGen;

import java.util.Vector;
import rcs.nml.NMLConnectionCreatorInterface;

/**
 * This includes protected extentions of the CodeGenCommonInterface not intented to
 * be accessed outside the diagapplet directory.
 * @author Will Shackleford <shackle@nist.gov>
 */
interface CodeGenCommonInterface2 extends CodeGenCommonInterface {

    CodeGenTextFieldInterface get_includePathField();

    void set_includePathField(CodeGenTextFieldInterface cgtfi);

    CodeGenTextFieldInterface get_configFileTextField();

    void set_configFileTextField(CodeGenTextFieldInterface cgtfi);

    CodeGenTextAreaInterface get_codeTextArea();

    void set_codeTextArea(CodeGenTextAreaInterface cta);

    boolean is_preserve_modules_hashtable();

    void set_preserve_modules_hashtable(boolean b);

    boolean get_update_with_name();

    void set_update_with_name(boolean b);

    diagapplet.utils.CountButtonInterface get_m_hierarchyFileLoadButton();

    void set_m_hierarchyFileLoadButton(diagapplet.utils.CountButtonInterface cbi);

    String get_lastIncludePath();

    void set_lastIncludePath(String s);

    boolean get_inside_diagapplet();

    void set_inside_diagapplet(boolean b);

    diagapplet.utils.URLLoadInfoPanelInterface get_m_loadingPanel();

    void set_m_loadingPanel(diagapplet.utils.URLLoadInfoPanelInterface lp);

    /**
     * Sets the modulesHashTable property. 
     * This is a HashTable matching module names (Strings) to ModuleInfo objects.
     * The use of a generic hashtable is to maintain Java 1.4 compatibility.
     * @param ht new hashtable
     */
    void set_m_modulesHashTable(java.util.Hashtable ht);

    void set_optionsHashTable(java.util.Hashtable ht);

    java.util.Hashtable get_serversHashtable();

    void set_serversHashtable(java.util.Hashtable ht);

    boolean get_running();

    void set_running(boolean b);

    boolean get_run_needed();

    void set_run_needed(boolean b);

    boolean get_is_loading_hierarchy();

    void set_is_loading_hierarchy(boolean b);

    boolean get_generating_code();

    void set_generating_code(boolean b);

    boolean get_running_script();

    void set_running_script(boolean b);

    boolean get_force_reload_file();

    void set_force_reload_file(boolean b);

    boolean get_first_java_class();

    void set_first_java_class(boolean b);

    boolean get_first_cpp_function();

    void set_first_cpp_function(boolean b);

    boolean get_RunIndependantly();

    void set_RunIndependantly(boolean b);

    boolean get_generate_java_classes_needed();

    void set_generate_java_classes_needed(boolean b);

    boolean get_generate_java_dictionary_needed();

    void set_generate_java_dictionary_needed(boolean b);

    boolean get_generate_cpp_update_functions_needed();

    void set_generate_cpp_update_functions_needed(boolean b);

    boolean get_generate_cpp_format_function_needed();

    void set_generate_cpp_format_function_needed(boolean b);

    boolean get_generate_cpp_constructors_needed();

    void set_generate_cpp_constructors_needed(boolean b);

    boolean get_reload_hierarchy_needed();

    void set_reload_hierarchy_needed(boolean b);

    String get_m_ConfigFile();

    void set_m_ConfigFile(String str);

    String get_cppFileName();

    void set_cppFileName(String str);

    String get_javaFileName();

    void set_javaFileName(String str);

    String get_script();

    void set_script(String scrf);

    boolean get_script_file_ran();

    void set_script_file_ran(boolean b);

    boolean get_display_on();

    void set_display_on(boolean dispon);

    String get_includePath();

    void append_includePath(String str);

    void set_includePath(String str);

    boolean get_debug_on();

    void set_debug_on(boolean dbg);

    boolean get_print_prompt();

    void set_print_prompt(boolean pp);

    void setVisible(boolean visible);

    diagapplet.utils.CountListInterface get_m_modulesCountList();

    void set_m_modulesCountList(diagapplet.utils.CountListInterface lst);

    diagapplet.utils.FastListPanelInterface get_serversList();

    void set_serversList(diagapplet.utils.FastListPanelInterface lst);

    void LoadHierarchyNewThread(String HierarchyName,
            diagapplet.CodeGen.LoadHierarchyUpdateInterface _lhui);

    int get_error_count();
    
    public void GetParameters(String args[]);
    
    public String createScript(String args[]);

    public boolean CheckForCppEnum(String cpp_type);

    public boolean CheckForCppClass(String cpp_type);

    public boolean CheckForCppPosemathClass(String cpp_type);

    public String GetEnumTypeName(String vardef);

    public boolean VarIsUnsigned(String vardef);

    public String GetCppVarDef(String var, StructureTypeInfo type_info);

    public String ConvertCppTypeToJavaType(String cpp_type);

    public boolean CheckForJavaStatic(String java_type);
    public String last_java_classname = null;

    public boolean CheckForJavaClass(String java_type);
    public void CreateJavaDefinition(StructureTypeInfo type_info);

    public void CreateJavaUpdateFunction(StructureTypeInfo type_info);

    public void CreateCppUpdateFunction(StructureTypeInfo type_info);

    public void CreateC_UpdateFunction(StructureTypeInfo type_info);

    public void PrintInfo(String options);

    public void PrintInfo(StructureTypeInfo type_info, String var_prefix, int varnum, int offset);

    public void CreateCppInitializer(StructureTypeInfo type_info);

    public void CreateCppConstructor(StructureTypeInfo type_info);

    public void GenerateJavaClass(String class_name);

    public void GenerateJavaClasses();

    public boolean IsNonUpdatebleClass(StructureTypeInfo type_info);

    public void GenerateCppUpdateFunction(String class_name);

    public void WriteOutput(String str);

    public void GenerateCppInitializer(String class_name);

    public void GenerateCppConstructor(String class_name);

    public void GenerateCppUpdateFunctions();

    public void GenerateCppConstructors();

    public String find_common_root(String strs[]);

    public boolean IsNMLMsg(String classname);

    public void GenerateCppStartOfFile();

    public void GenerateC_StartOfFile();

    public String[] RemoveDuplicates(String list[]);

    public void GenerateCppPrototypesHeader();

    public void GenerateCppFormatFunction();

    public void SetOutputFile(String str) throws Exception;

    public String get_base_name(String str);

    public String RemoveStartingEndingSpace(String str);

    public boolean IsInteger(String str);

    public boolean IsDouble(String str);

    public void GenerateJavaMessageDict();

    public void ClearAll();

    public void AddAlphabatizedItem(diagapplet.utils.FastListPanelInterface lst, String item, boolean select_it);

    public boolean is_generic(String str);

    public void SelectByFromFile(String filename);

    public void InitializeClassList() throws Exception;

    public void ResetGenericClasses();

    public void RingBell();

    public void ParseOptionsInfo();

    public String remove_leading_whitespace(String str);

    public void MakeListGeneric(diagapplet.utils.FastListPanelInterface lst);

    public void RunScript(String new_script_file);

    public void RunScript() throws Exception;

    public String GetFormatFunctionNameBase(String selected_classes[]);


}
