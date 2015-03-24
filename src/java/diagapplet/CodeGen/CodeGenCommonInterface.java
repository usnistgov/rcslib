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

import java.util.Vector;
import rcs.nml.NMLConnectionCreatorInterface;

/**
 * Provides an interface for the Diagnostics tool and Design Tools to CodeGenCommon
 * without a circular dependency that directly using the class would cause.
 * @author Will Shackleford 
 */
public interface CodeGenCommonInterface {

    /**
     * Return the value of the interrupt_loading property.
     * It is generally set from one thread to end a long CodeGen function in another
     * thread safely and needs to be checked from wihin each loop.
     * @return interrupt_loading property.
     */
    public boolean is_interrupt_loading();

    
    /**
     * Set the value of the interrupt_loading property.
     * This should cause any function likely to take a long time in another thread
     * to return an error quickly.
     * @param b  new value of interupt_loading property.
     */
    public void set_interrupt_loading(boolean b);
   
    /**
     * Returns the modulesHashTable property. 
     * This is a HashTable matching module names (Strings) to ModuleInfo objects.
     * The use of a generic hashtable rather than {@code Hashtable<String,ModuleInfo> } is to maintain Java 1.4 compatibility. 
     * @return m_modulesHashTable current modules hash table
     */
    public java.util.Hashtable get_m_modulesHashTable();

    /**
     * Returns the optionsHashTable property.
     * This is a Hashtable macthing String option names to String values.
     * Options can be set with command line parameters, int the options section of 
     * a config file or .diag file or with environment variables.
     * The use of a generic hashtable rather than {@code Hashtable<String,String> } is to maintain Java 1.4 compatibility.
     * @return the hashtable.
     */
    public java.util.Hashtable get_optionsHashTable();
    
    /**
     * Get the hierarchyFile property. A string containing the name/URL of the last loaded hiearchyFile
     * optionally including the path.
     * @return hierarchy file name.
     */
    public String get_m_hierarchyFile();

    /**
     * Set the hierarchyFile property.
     * @param str new hierarchy file name
     */
    public void set_m_hierarchyFile(String str);

    /**
     * Get the ClassList property. Depending on whether this is run from a graphical tool or not
     * the ClassList may simply wrap a Vector of strings of the class names or a Graphical widget
     * for displaying selecting them.
     * @return ClassList
     */
    public diagapplet.utils.FastListPanelInterface get_ClassList();

    /**
     * Set the ClassList property. Depending on whether this is run from a graphical tool or not
     * the ClassList may simply wrap a Vector of strings of the class names or a Graphical widget
     * for displaying selecting them.
     * @param flp new fast list panel
     */
    public void set_ClassList(diagapplet.utils.FastListPanelInterface flp);
    
    /**
     * Get the ModulesList property. Depending on whether this is run from a graphical tool or not
     * the ModulesList may simply wrap a Vector of strings of the module names or a Graphical widget
     * for displaying selecting them.
     * @return modulesList
     */
    public diagapplet.utils.FastListPanelInterface get_m_modulesList();

    /**
     * Set the ModulesList property. Depending on whether this is run from a graphical tool or not
     * the ModulesList may simply wrap a Vector of strings of the module names or a Graphical widget
     * for displaying selecting them.
     * @param lst new fast list panel
     */
    public void set_m_modulesList(diagapplet.utils.FastListPanelInterface lst);
    
    /**
     * Load the hierarchy parsing the file set with set_m_hierarchyFile().
     * @throws java.lang.Exception when LoadHierachy fails
     */
    public void LoadHierarchy() throws Exception;

    /**
     * Set a diag_dict_creator property.
     * Only used by diagnostics tool.
     * This object create DiagNMLMsgDict objects indirectly in a way to avoid 
     * circular dependancy.
     * @param _diag_dict_creator new dictionary object
     */
    public void set_diag_dict_creator(DiagNMLMsgDictCreatorInterface _diag_dict_creator);

    /**
     * Get the diag_dict_creator property.
     * Only used by diagnostics tool.
     * @return diag_dict_creator
     */
    public DiagNMLMsgDictCreatorInterface get_diag_dict_creator();
    
    /**
     * Set the nml_creator property.
     * Used only by the diagnostics tools.
     * This object create NMLConnecton objects indirectly in a way to avoid 
     * circular dependancy.
     * @param _nml_creator new nml creator object
     */
    public void set_nml_creator(NMLConnectionCreatorInterface _nml_creator);

    /**
     * Get the nml_creator property.
     * @return nml_creator.
     */
    public NMLConnectionCreatorInterface get_nml_creator();


    /**
     * Return the value of the matching strName if one exist(), optionally
     * searching command line args given in args.
     * Parameters can also be found in environment variables or the options section
     * of the diag file.
     * @param strName parameter name
     * @param args arguments
     * @return value
     */
    public String GetParameter(String strName, String args[]);
    
    /**
     * Adds Information related to an auxilliary buffer.
     * Only used by diagnostics tool.
     * @param buffer_name buffer name
     * @param header header file name
     * @param nml_file nml file name
     */
    public void AddAuxBufferModule(String buffer_name, String header, String nml_file);

    /**
     * Returns an extrActionsVector(). 
     * It is a vector of strings read from the config file.
     * The diagnostics tool adds an menu item at run time for each item in the vector.
     * @return extraActionsVector()
     */
    public Vector get_extraActionsVector();

    /**
     * Returns an extrTabsVector(). 
     * It is a vector of strings read from the config file.
     * The diagnostics tool adds a tab at run time for each item in the vector.
     * @return extraTabsVector()
     */
    public Vector get_extraTabsVector();
    
    // Constant option/ command line argument names.
    public static final String PARAM_HierarchyFile = "HierarchyFile";
    public static final String PARAM_ConfigFile = "ConfigFile";
    public static final String PARAM_HeaderFile = "HeaderFile";
    public static final String PARAM_HFile = "HFile";
    public static final String PARAM_HHFile = "HHFile";
    public static final String PARAM_ScriptFile = "script";
    public static final String PARAM_DebugOn = "debug_on";
    public static final String PARAM_UpdateWithName = "update_with_name";
    public static final String PARAM_CreatePrintSizesFile = "create_print_sizes_file";
    public static final String PARAM_SelectFromAllFiles = "select_from_all_files";
    public static final String PARAM_SizesFileName = "sizes_filename";
    public static final String PARAM_GenerateSymbolLookups = "generate_symbol_lookups";
    public static final String PARAM_GenerateAllEnumSymbolLookups = "generate_all_enum_symbol_lookups";
    public static final String PARAM_AddSetHeader = "add_set_header";
    public static final String PARAM_GenerateForC = "generate_for_c";
    public static final String PARAM_GenerateForAllLangs = "generate_for_all_langs";
    public static final String PARAM_GenerateForCPP = "generate_for_cpp";
    public static final String PARAM_GenerateForJava = "generate_for_java";
    public static final String PARAM_GenerateForAda = "generate_for_ada";
    public static final String PARAM_JavaPackageName = "java_package_name";
    public static final String PARAM_DlaLengthInit = "dla_length_init";
    public static final String PARAM_DisplayOn = "display_on";
    public static final String PARAM_UseDefaultTypes = "UseDefaultTypes";
    public static final String PARAM_NoErrlog = "no_errlog";
    public static final String PARAM_PrintScript = "print_script";
    public static final String ndla_string = "NML_DYNAMIC_LENGTH_ARRAY";
    public static final String ndla_len_string = "length_for_nml_dynamic_length_array";
    public static final String unbounded_string = "NML_UNBOUNDED_LENGTH_ARRAY";

}
