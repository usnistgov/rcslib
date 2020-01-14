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

package rcsdesign;

import java.util.*;
import java.io.*;
import diagapplet.CodeGen.*;
import diagapplet.HierarchyPanel;

 class rcsDesignWriter extends rcsDesignWriterInfo
{
    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613922L;

    static public boolean debug_on = false;
    static final double CURRENT_TEMPLATE_VERSION = 1.1;

    public boolean merging_backup = false;
    public Merger merger = null;
    public boolean auto_backup = true;


    public String forced_cpps = null;
    public String forced_hpps = null;
    public String forced_objs = null;
    public String forced_bins = null;
    public String forced_libs = null;

    boolean put_merger_in_nml_mode = false;
    boolean makefile_mode = false;
    protected boolean CanNotOverwriteExistingFile(File file, boolean allow_merge, boolean cpp_mode) throws rcsDesignUserAbortException
    {
	if(debug_on)
	    {
		System.out.println("rcsDesignWriter.CanNotOverwriteExistingFile("+file+", "+allow_merge+", "+cpp_mode+")");
	    }
	merging_backup = false;
	merger = new Merger();
	merger.template_version = CURRENT_TEMPLATE_VERSION;
	merger.cpp_mode = cpp_mode;
	merger.nml_mode = put_merger_in_nml_mode;
	merger.Makefile_mode = makefile_mode;
	makefile_mode = false;
	if(file.exists())
	    {
		if(debug_on)
		    {
			System.out.println("file.exists() returned true");
		    }
		if(!file.canWrite() && gui.autoCheckOutCheckbox.getState())
		    {
			gui.CheckOutFile(file);
		    }
		if(!file.canWrite())
		    {
			gui.Alert("Can not write over "+file);
			return true;
		    }
		if(gui.overwriteNeverCheckbox.getState())
		    {
			if(debug_on)
			    {
				System.out.println("gui.overwriteNeverCheckbox.getState() returned true");
			    }
			return true;
		    }
		if(gui.overwritePromptCheckbox.getState())
		    {
			QueryDialog replaceFileQd = gui.Query(file+" already exists.\nReplace It?");
			while(!replaceFileQd.done)
			    {
				if(debug_on)
				    {
					System.out.println("Waiting for User");
				    }
				try
				    {
					Thread.sleep(500);
				    }
				catch(Exception e)
				    {
				    }
			    }
			if(QueryDialog.cancel)
			    {
				if(debug_on)
				    {
					System.out.println("QueryDialog.cancel");
				    }
				quitFillDirectories = true;
				throw new rcsDesignUserAbortException();
			    }
			if(replaceFileQd.yes_to_all)
			    {
				gui.overwriteAlwaysCheckbox.setState(true);
			    }
			if(replaceFileQd.no_to_all)
			    {
				gui.overwriteNeverCheckbox.setState(true);
			    }
			if(!replaceFileQd.ok)
			    {
				if(debug_on)
				    {
					System.out.println("!replaceFileQd.ok");
				    }
				return true;
			    }
		    }
		if(gui.useMergerCheckbox.getState() && allow_merge)
		    {
			merger.GetBackupData(file);
			merging_backup = true;
		    }
		if(auto_backup)
		    {
			BackupFile(file);
		    }
	    }
	if(debug_on)
	    {
		System.out.println("rcsDesignWriter.CanNotOverwriteExistingFile returning false.\n");
	    }

	return false;
    }


    protected File BackupFile(File file_to_backup)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsDesignWriter.BackupFile("+file_to_backup+")");
		    }

		if(null != gui.makeBackupsCheckbox)
		    {
			if(!gui.makeBackupsCheckbox.getState())
			    {
				return file_to_backup;
			    }
		    }
		String parentdir = file_to_backup.getParent();
		File parentDirFile = new File(parentdir);
		if(!parentDirFile.canWrite())
		    {
			return null;
		    }
		for(int i = 0; i < 10; i++)
		    {
			String newname = file_to_backup.getName()+".~"+i+"~";
			File newnameFile = new File(parentDirFile, newname);
			String delete = file_to_backup.getName()+".~"+(i+1)+"~";
			File deleteFile = new File(parentDirFile, delete);
			if(!newnameFile.exists())
			    {
				if(debug_on)
				    {
					System.out.println("Renaming "+file_to_backup+" to "+newnameFile+" as a backup.");
				    }
				file_to_backup.renameTo(newnameFile);
				if(deleteFile.exists())
				    {
					if(debug_on)
					    {
						System.out.println("Deleting old backup file "+deleteFile);
					    }
					deleteFile.delete();
				    }
				return newnameFile;
			    }
		    }
		File newnameFile = new File(parentDirFile,file_to_backup.getName()+"~0~");
		System.out.println("Renaming "+file_to_backup+" to "+newnameFile+" as a backup.");
		file_to_backup.renameTo(newnameFile);
		String delete = file_to_backup.getName()+".~1~";
		File deleteFile = new File(parentDirFile, delete);
		if(deleteFile.exists())
		    {
			System.out.println("Deleting old backup file "+deleteFile);
			deleteFile.delete();
		    }
		return newnameFile;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }


    public void UpdateFile(FileTypeInfo fti) throws rcsDesignUserAbortException
    {
	try
	    {
		put_merger_in_nml_mode = false;
		makefile_mode = false;
		if(debug_on)
		    {
			System.out.println("UpdateFile: name = "+fti.name+", type = "+fti.type);
		    }

		QueryDialog.cancel = false;
		auto_backup = true;

		File parentDir = new File(fti.file.getParent());
		if(!singleDir)
		    {
			parentDir.mkdirs();
		    }
		if(null != fti)
		    {
			switch(fti.type)
			    {
			    case FileTypeInfo.MAIN_DESIGN_LOG:
				WriteMainDesignLog(parentDir);
				break;

			    case FileTypeInfo.NML_MODULE_DESIGN_LOG:
				WriteModuleDesignLog(parentDir, fti.module_name);
				break;

			    case FileTypeInfo.NML_MODULE_CPP:
				WriteModuleCpp(parentDir, fti.module_name,fti.name);
				break;

			    case FileTypeInfo.NML_MODULE_HEADER:
				WriteModuleHeader(parentDir, fti.module_name);
				break;

			    case FileTypeInfo.NML_MODULE_MESSAGE_HEADER:
				WriteNMLModuleMessageHeader(parentDir,fti.module_name,fti.name);
				break;

			    case FileTypeInfo.NML_MODULE_MESSAGE_CPP:
				String script_file = fti.file.toString();
				script_file = script_file.substring(0, script_file.length()-cpp_ext.length())+".gen";
				gui.codeGenerationApplet.set_preserve_modules_hashtable(true);
				if(fti.file.exists() && !fti.file.canWrite() && gui.autoCheckOutCheckbox.getState())
				    {
					gui.CheckOutFile(fti.file);
				    }
				gui.codeGenerationApplet.RunScriptFile(script_file);
				gui.codeGenerationApplet.set_preserve_modules_hashtable(false);
				break;


			    case FileTypeInfo.NML_MODULE_MESSAGE_CODEGEN_SCRIPT:
				WriteModuleCodeGenScript(parentDir,fti.module_name,fti.name);
				break;

			    case FileTypeInfo.NML_MISC_MESSAGE_CODEGEN_SCRIPT:
				WriteMiscCodeGenScript(parentDir,fti.name);
				break;

			    case FileTypeInfo.NML_AUX_MESSAGE_HEADER:
				WriteNMLAuxMessageHeader(parentDir,fti.aux);
				break;

			    case FileTypeInfo.NML_AUX_MESSAGE_CPP:
				script_file = fti.file.toString();
				script_file = script_file.substring(0, script_file.length()-cpp_ext.length())+".gen";
				gui.codeGenerationApplet.set_preserve_modules_hashtable(true);
				gui.codeGenerationApplet.RunScriptFile(script_file);
				gui.codeGenerationApplet.set_preserve_modules_hashtable(false);
				break;


			    case FileTypeInfo.NML_AUX_MESSAGE_CODEGEN_SCRIPT:
				WriteAuxCodeGenScript(parentDir,fti.aux,fti.name);
				break;

			    case FileTypeInfo.MODULE_MAKEFILE:
				forced_cpps = fti.module_name+cpp_ext;
				forced_objs = fti.module_name+obj_ext;
				forced_hpps = fti.module_name+hpp_ext;
				WriteUtilMakefile( parentDir);
				forced_cpps = null;
				forced_objs = null;
				forced_hpps = null;
				break;

			    case FileTypeInfo.INTF_MAKEFILE:
				WriteIntfMakefile( parentDir);
				break;

			    case FileTypeInfo.INCLUDE_MAKEFILE:
				WriteAppIncMakefile( parentDir);
				break;

			    case FileTypeInfo.TOP_MAKEFILE:
				WriteTopLevelMakefile( parentDir);
				break;

			    case FileTypeInfo.MODULE_CONFIGURATION:
				WriteConfiguration( parentDir);
				break;

			    case FileTypeInfo.NML_CONFIGURATION:
				WriteNMLLocalFile( parentDir);
				break;

			    case FileTypeInfo.SCRIPT:
				WriteRunAllScript( parentDir);
				break;

			    case FileTypeInfo.MAIN_MAKEFILE:
				WriteMainMakefile( parentDir);
				break;

			    case FileTypeInfo.UTIL_MAKEFILE:
				WriteUtilMakefile( parentDir);
				break;

			    case FileTypeInfo.SERVER_CPP:
				WriteServerCpp( parentDir, fti.server_name);
				break;

			    case FileTypeInfo.MAIN_LOOP_CPP:
				WriteMainCpp( parentDir, fti.loop_name);
				break;

			    case FileTypeInfo.EC_SCRIPT:
				WriteEcScript( parentDir);
				break;

			    case FileTypeInfo.SINGLE_DIR_GNUMAKEFILE:
				WriteSingleDirGnuMakefile( parentDir, fti.name);
				break;

			    }
			if(gui.screenChoice.getSelectedItem().toUpperCase().indexOf("FILE") >= 0)
			    {
				gui.ReadFileIntoTextArea(fti.file.toString(), FileTypeInfo.typeToString(fti.type), fti);
			    }
			fti.generated = true;
			fti.up_to_date = true;
			fti.in_use = false;
			merger = null;
		    }
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteMainDesignLog(File dirFile) throws rcsDesignUserAbortException
    {
	try
	    {
		byte orig_file_buffer[] = null;
		if(null == designLog)
		    {
			return;
		    }
		if(designLog.length() < 1)
		    {
			return;
		    }
		File designLogFile = new File(dirFile,"design.log");
		if(designLogFile.exists())
		    {
			if(!designLogFile.canRead())
			    {
				System.err.println("Can't read design log.");
				return;
			    }
			if(!designLogFile.canWrite())
			    {
				gui.CheckOutFile(designLogFile);
			    }
			if(!designLogFile.canWrite())
			    {
				System.err.println("Can't write to  design log.");
				return;
			    }
			FileInputStream fis = new FileInputStream(designLogFile);
			orig_file_buffer = new byte[(int) designLogFile.length()];
			if(fis.read(orig_file_buffer) < 0)
			    {
				System.err.println("Can't read design log.");
				return;
			    }
			fis.close();
			fis = null;
		    }
		FileOutputStream fos = new FileOutputStream(designLogFile);
		if(null != orig_file_buffer)
		    {
			fos.write(orig_file_buffer);
		    }
		byte new_log_buffer[] = designLog.getBytes();
		fos.write(new_log_buffer);
		fos.flush();
		fos.close();
		fos = null;
		designLog = "";
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteModuleDesignLog(File dirFile, String moduleName) throws rcsDesignUserAbortException
    {
	try
	    {
		ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(moduleName);
		if(null == modInfo)
		    {
			return;
		    }
		byte orig_file_buffer[] = null;
		if(null == modInfo.designLog)
		    {
			return;
		    }
		if(modInfo.designLog.length() < 1)
		    {
			return;
		    }
		File designLogFile = new File(dirFile,"design.log");
		if(designLogFile.exists())
		    {
			if(!designLogFile.canRead())
			    {
				System.err.println("Can't read design log.");
				return;
			    }
			if(!designLogFile.canWrite())
			    {
				gui.CheckOutFile(designLogFile);
			    }
			if(!designLogFile.canWrite())
			    {
				System.err.println("Can't write to  design log.");
				return;
			    }
			FileInputStream fis = new FileInputStream(designLogFile);
			orig_file_buffer = new byte[(int) designLogFile.length()];
			if(fis.read(orig_file_buffer) < 0)
			    {
				System.err.println("Can't read design log.");
				return;
			    }
			fis.close();
			fis = null;
		    }
		FileOutputStream fos = new FileOutputStream(designLogFile);
		if(null != orig_file_buffer)
		    {
			fos.write(orig_file_buffer);
		    }
		byte new_log_buffer[] = modInfo.designLog.getBytes();
		fos.write(new_log_buffer);
		fos.flush();
		fos.close();
		fos = null;
		modInfo.designLog = "";
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteConfiguration(File dirFile) throws rcsDesignUserAbortException
    {
	try
	    {
		File configFile = new File(dirFile,AppName+".cfg");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(configFile,false,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating config File for "+AppName);
		    }
		FileOutputStream fos = new FileOutputStream(configFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("");
		pos.println("// This configuration file is used by the RCS Design and Diagnostics tools.");
		pos.println("// The format is similar to C++.");
		pos.println("// Sections are delimited with curly braces \"{}\".");
		pos.println("// Most sections correspond to modules or servers but the options section is unique..");
		pos.println("// Inividual variables are delimited with semicolons \";\".");
		pos.println("// Comments can be started with double forward // and continue to the end of the line");
	
		pos.println("");
		pos.println("options{");
		pos.println("");
		pos.println("\t// This set of options controls how scripts and makefiles and source code are generated from within the Design Tool.");
		pos.println("\t// They have no effect on the diagnostics tool.");
		pos.println("\tAppDir=\""+AppDir+"\"; \t// Directories where final installs should go.");
		pos.println("\tAppName=\""+AppName+"\"; \t//Name for the entire application.");
		pos.println("\tCppExt=\""+cpp_ext+"\"; \t//File extension for C++ source files.");
		pos.println("\tHppExt=\""+hpp_ext+"\"; \t//File extension for C++ header files.");
		pos.println("\tObjExt=\""+obj_ext+"\"; \t//File extension for object files.");
		pos.println("\tRcsLibDir=\""+RcsLibDir+"\"; \t//Directory where the RCS Library was installed.");
		pos.println("\tTerminalCommand=\""+TerminalCommand+"\"; // Command to start a new graphical terminal.");
		pos.println("\tMakeCommand=\""+MakeCommand+"\"; \t//Command to build app.");
		pos.println("\tSingleDir="+singleDir+"; \t// Keep all source files in the same directory allowing for simple Makefiles instead of a number of subdirectories.");

		if(java_setup_file_name.length() > 0)
		    {
			pos.println("\tJavaSetupFileName=\""+java_setup_file_name+"\"; \t// Command to run before running java.");
		    }
		else if(java_setup.length() > 0)
		    {
			pos.println("\tJavaSetup=\""+java_setup+"\"; \t// Command to run before running java.");
		    }
		pos.println("");
		pos.println("\t//This set of options controls the Hierarchy Panel display in both tools.\n");
		pos.println("\t// Show subordinates of the same parent from right to left\n");
		pos.println("\t// according to module_number rather than name.");
		pos.println("\tListModulesByNumber="+gui.list_modules_by_number+";");
		pos.println("\t// Set the width in pixels of the rectangle used for each module.");
		pos.println("\tMODULE_WIDTH="+HierarchyPanel.MODULE_WIDTH+";");
		pos.println("\t// Set the hieght in pixels of the rectangle used for each module.");
		pos.println("\tMODULE_HEIGHT="+HierarchyPanel.MODULE_HEIGHT+";");
		pos.println("\t// Set the horizontal distance between rectangles used for each module.");
		pos.println("\tMODULE_X_SPACING="+HierarchyPanel.MODULE_X_SPACING+";");
		pos.println("\t// Set the vertical distance between rectangles used for each module.");
		pos.println("\tMODULE_Y_SPACING="+HierarchyPanel.MODULE_Y_SPACING+";");
		pos.println("\t// Set the minumum width in pixels of the Hierarchy Panel");
		pos.println("\tHierarchyMinWidth=800;");
		pos.println("\t// Set the minimum height in pixels of the Hierarchy Panel");
		pos.println("\tHierarchyMinHeight=500;");

		pos.println("");
		pos.println("\t// The remaining options only effect the Diagnostics tool.\n");
		pos.println("\t// Set the width in chararacters of the lists for displaying messages.\n");
		pos.println("\tListWidth=30;");
		pos.println("\t// Set number of lines displayed in the lists for displaying messages.\n");
		pos.println("\tListHeight=10;");
		pos.println("\tSet the delay in seconds before getting new data from the controller and displaying it.");
		pos.println("\trefreshTime=0.5;");
		    
		pos.println("}");
		pos.println("");
		pos.println("// List of servers");
		if(debug_on)
		    {
			System.out.println("gui.serverList = "+gui.serverList);
		    }
		if(null != gui.serverList)
		    {
			if(debug_on)
			    {
				System.out.println("gui.serverList.getItemCount() = "+gui.serverList.getItemCount());
			    }
			if(gui.serverList.getItemCount() > 1)
			    {
				for(int i = 0; i < gui.serverList.getItemCount(); i++)
				    {
					String server_name = gui.serverList.getItem(i);
					if(debug_on)
					    {
						System.out.println("server_name = "+server_name);
					    }
					ServerInfo si = (ServerInfo) serversHashtable.get(server_name);
					if(debug_on)
					    {
						System.out.println("si = "+si);
					    }
					pos.println("");// blank lineNumber
					pos.println(si.Name+"{");
					pos.println("\tis_server=true; // indicates that this is a server and not a control module.");
					if(si.Host != null)
					    {
						if(si.Host.length() > 0)
						    {
							pos.println("// The host that this server will run on.");
							pos.println("// This parameter is used only by the Design tool when generating new NML files.");
							pos.println("// The diagnostics tool will get this info from the NML file.");
							pos.println("\thost=\""+si.Host+"\";");
						    }
					    }
					pos.println("\t// List of buffers this server provides access to.");
					for(int j = 0; j < si.bufferNames.size(); j++)
					    {
						pos.println("\tbuf=\""+si.bufferNames.elementAt(j)+"\";");
					    }
					pos.println("}");
					pos.println("");// blank lineNumber
				    }
			    }
		    }
		pos.println("");

		for(int i = 0; i < gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			if(null == modName)
			    {
				break;
			    }
			ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
			if(null == modInfo)
			    {
				continue;
			    }
			pos.println(modName+"{");
			if(null != modInfo.subsystem)
			    {
				pos.println("\tsubsystem=\""+modInfo.subsystem+"\";");
			    }
			if(null != modInfo.children_names)
			    {
				for(int j = 0; j < modInfo.children_names.size(); j++)
				    {
					String childname = (String) modInfo.children_names.elementAt(j);
					if(null == childname)
					    {
						continue;
					    }
					pos.println("\tchild=\""+childname+"\";");
				    }
			    }
			if(null != modInfo.cmdsTypeFile)
			    {
				pos.println("\tcmd_types=\""+modInfo.cmdsTypeFile+"\";");
			    }
			if(null != modInfo.statsTypeFile)
			    {
				pos.println("\tstat_types=\""+modInfo.statsTypeFile+"\";");
			    }
			if(null != modInfo.SourceCodeDirectory)
			    {
				pos.println("\tSourceCodeDirectory=\""+modInfo.SourceCodeDirectory+"\";");
			    }
			if(null != modInfo.m_stat_read_Connection)
			    {
				if(null != modInfo.m_stat_read_Connection.get_buffer_name())
				    {
					pos.println("\tstat_buffer_name=\""+modInfo.m_stat_read_Connection.get_buffer_name()+"\";");
				    }
			    }
			if(null != modInfo.m_cmd_read_Connection)
			    {
				if(null != modInfo.m_cmd_read_Connection.get_buffer_name())
				    {
					pos.println("\tcmd_buffer_name=\""+modInfo.m_cmd_read_Connection.get_buffer_name()+"\";");
				    }
			    }
			if(null != modInfo.predefined_type_files)
			    {
				String last_types_file = null;
				for(int pdi = 0; pdi < modInfo.predefined_type_files.size(); pdi++)
				    {
					String types_file = (String) modInfo.predefined_type_files.elementAt(pdi);
					if(null != types_file)
					    {
						if(types_file.equals(last_types_file))
						    {
							continue;
						    }
						pos.println("\tpre_defined_types=\""+types_file+"\";");
						last_types_file = types_file;
					    }
				    }
			    }
			if(null != modInfo.NMLConfigurationFile)
			    {
				pos.println("\tnml_configuration_file=\""+modInfo.NMLConfigurationFile+"\";");
			    }
			else
			    {
				pos.println("\tnml_configuration_file=\""+AppName+".nml\";");
			    }

			if(null != modInfo.moduleClassName)
			    {
				pos.println("\tclass_name=\""+modInfo.moduleClassName+"\";");
			    }

			if(null != modInfo.MainLoopName)
			    {
				pos.println("\tMainLoopName=\""+modInfo.MainLoopName+"\";");
				pos.println("\tcycle_time="+modInfo.cycle_time+";");
				if(null != modInfo.host)
				    {
					if(modInfo.host.length() > 0)
					    {
						pos.println("\thost="+modInfo.host+";");
					    }
				    }
			    }
			if(null != modInfo.AuxOutputNames)
			    {
				for(int j = 0; j < modInfo.AuxOutputNames.size(); j++)
				    {
					String aux = (String) modInfo.AuxOutputNames.elementAt(j);
					BufferInfo bi = (BufferInfo) buffersHashtable.get(aux);
					if(null == bi)
					    {
						continue;
					    }
					ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(modInfo.Name);
					if(null == ci)
					    {
						continue;
					    }
					if(ci.updateEveryCycle)
					    {
						pos.println("\tupdate_next_aux_every_cycle=true;");
					    }
					else
					    {
						pos.println("\tupdate_next_aux_every_cycle=false;");
					    }
					pos.println("\taux_output=\""+aux+"\";");
					if(!singleDir)
					    {
						pos.println("\taux_output_types=\"src"+File.separator+"intf"+File.separator+aux+"n"+hpp_ext+"\";");
					    }
					else
					    {
						pos.println("\taux_output_types=\""+aux+"n"+hpp_ext+"\";");
					    }

				    }
			    }
			if(null != modInfo.AuxInputNames)
			    {
				for(int j = 0; j < modInfo.AuxInputNames.size(); j++)
				    {
					String aux = (String) modInfo.AuxInputNames.elementAt(j);
					BufferInfo bi = (BufferInfo) buffersHashtable.get(aux);
					if(null == bi)
					    {
						continue;
					    }
					ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(modInfo.Name);
					if(null == ci)
					    {
						continue;
					    }
					if(ci.updateEveryCycle)
					    {
						pos.println("\tupdate_next_aux_every_cycle=true;");
					    }
					else
					    {
						pos.println("\tupdate_next_aux_every_cycle=false;");
					    }
					pos.println("\taux_input=\""+aux+"\";");
					if(!singleDir)
					    {
						pos.println("\taux_input_types=\"src"+File.separator+"intf"+File.separator+aux+"n"+hpp_ext+"\";");
					    }
					else
					    {
						pos.println("\taux_input_types=\""+aux+"n"+hpp_ext+"\";");
					    }
				    }
			    }
			if(null != modInfo.externalIncludeDirectories)
			    {
				for(int j = 0; j < modInfo.externalIncludeDirectories.size(); j++)
				    {
					String extInclude = (String) modInfo.externalIncludeDirectories.elementAt(j);
					pos.println("\texternal_include_dir=\""+extInclude+"\";");
				    }
			    }
			if(null !=gui.includesList)
			    {
				for(int j = 0; j < gui.includesList.getItemCount(); j++)
				    {
					String extInclude = gui.includesList.getItem(j);
					pos.println("\texternal_include_dir=\""+extInclude+"\";");
				    }
			    }
			if(null != modInfo.externalLibraries)
			    {
				for(int j = 0; j < modInfo.externalLibraries.size(); j++)
				    {
					String extLib = (String) modInfo.externalLibraries.elementAt(j);
					pos.println("\texternal_library=\""+extLib+"\";");
				    }
			    }
			if(null != gui.libsList)
			    {
				for(int j = 0; j < gui.libsList.getItemCount(); j++)
				    {
					String extLib = gui.libsList.getItem(j);
					pos.println("\texternal_library=\""+extLib+"\";");
				    }
			    }
			if(null != modInfo.releaseLibrary)
			    {
				pos.println("\trelease_library=\""+modInfo.releaseLibrary+"\";");
			    }
			if(null != modInfo.releaseIncludeDirectory)
			    {
				pos.println("\trelease_include_dir=\""+modInfo.releaseIncludeDirectory+"\";");
			    }
			if(null != modInfo.baseClassName)
			    {
				pos.println("\tbase_class_name=\""+modInfo.baseClassName+"\";");
			    }
			if(null != modInfo.baseModuleName)
			    {
				pos.println("\tbase_name=\""+modInfo.baseModuleName+"\";");
			    }
			if(null != modInfo.baseClassChildrenNames)
			    {
				for(int j = 0; j < modInfo.baseClassChildrenNames.size(); j++)
				    {
					String bchild = (String) modInfo.baseClassChildrenNames.elementAt(j);
					pos.println("\tbase_child=\""+bchild+"\";");
				    }
			    }
			if(null != modInfo.cmdsBaseClass)
			    {
				for(int j = 0; j < modInfo.cmdsBaseClass.size(); j++)
				    {
					String bcmd = (String) modInfo.cmdsBaseClass.elementAt(j);
					pos.println("\tbase_cmd=\""+bcmd+"\";");
				    }
			    }
			if(null != modInfo.baseClassCmdsTypeFile)
			    {
				pos.println("\tbase_cmd_types=\""+modInfo.baseClassCmdsTypeFile+"\";");
			    }
			if(null != modInfo.baseClassStatsTypeFile)
			    {
				pos.println("\tbase_stat_types=\""+modInfo.baseClassStatsTypeFile+"\";");
			    }
			if(modInfo.module_number > 0)
			    {
				pos.println("\tmodule_number="+modInfo.module_number+";");
			    }
			pos.println("}");
			pos.println("");// blank lineNumber
		    }
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteRunAllScript(File dirFile) throws rcsDesignUserAbortException
    {
	try
	    {
		File scriptFile = null;
		if(mswinDevPlat)
		    {
			scriptFile = new File(dirFile,AppName+".bat");
		    }
		else
		    {
			scriptFile = new File(dirFile,"run."+AppName);
		    }


		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(scriptFile, false,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating script File for "+AppName);
		    }
		FileOutputStream fos = new FileOutputStream(scriptFile);
		PrintWriter pos = new PrintWriter(fos);
		File userDirFile = new File(getUserDir());
		if(!mswinDevPlat)
		    {
			String term_start = TerminalCommand;
			int de_index = term_start.indexOf(" -e");
			if(de_index > 0 && term_start.startsWith("xterm"))
			    {
				term_start=term_start.substring(0,de_index);
			    }
			pos.println("#! /bin/sh");
			pos.println("");// blank lineNumber
			pos.println("# CD to the User Directory, just in-case we're not already there.");
			pos.println("cd "+userDirFile.getAbsolutePath());
			pos.println("");// blank lineNumber
			pos.println("if test \"x${HOST}\" = \"x\" ;  then");
			pos.println("\techo Setting HOST to `hostname`");
			pos.println("\tHOST=`hostname`;");
			pos.println("\texport HOST;");
			pos.println("fi");
			pos.println("");// blank lineNumber
                        
                        pos.println("if test \"x${xterm_cmd}\" = \"x\" ; then");
                        pos.println("\txterm_cmd=\""+term_start+"\";");
                        pos.println("fi");
			pos.println("");// blank lineNumber
                        
                        
//			if(term_start.startsWith("xterm"))
//			    {
//				pos.println("");// blank lineNumber
//				pos.println("if test '!' -x /usr/X11R6/bin/xterm -a '!' -x /usr/bin/xterm -a '!' -x /usr/X/bin/xterm ;  then");
//				pos.println("\tNO_XTERM=1;");
//				pos.println("\texport NO_XTERM;");
//				pos.println("fi");
//				pos.println("");// blank lineNumber
//			    }
//			else
//			    {
//				pos.println("");// blank lineNumber
//				pos.println("if test '!' -x "+term_start+" ;  then");
//				pos.println("\tNO_XTERM=1;");
//				pos.println("\texport NO_XTERM;");
//				pos.println("fi");
//				pos.println("");// blank lineNumber
//			    }
			pos.println("");// blank lineNumber
			pos.println("pwd");
			pos.println("");// blank lineNumber
			if(RcsLibDir != null && RcsLibDir.length() > 1 )
			    {
				pos.println("if test \"x${RCSLIB_MAIN_DIR}\" = \"x\" ;  then");
				pos.println("\tRCSLIB_MAIN_DIR="+RcsLibDir);
				pos.println("\texport RCSLIB_MAIN_DIR;");
				pos.println("fi");
			    }
			pos.println("");// blank lineNumber
			pos.println("echo RCSLIB_MAIN_DIR=${RCSLIB_MAIN_DIR}");
			pos.println("");// blank lineNumber
			pos.println("LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${RCSLIB_MAIN_DIR}/lib;");
			pos.println("export LD_LIBRARY_PATH");
			if(!singleDir)
			    {
				pos.println("if test \"x${PLAT}\" = \"x\" ; then");
				pos.println("\tPLAT=`uname -sr | sed \"y/ /_/\" | sed \"y/./_/\" | sed s/-rtl.\\*\\$// | sed \"y/-/_/\" | gawk '{ printf ( \"%s\",tolower ( $1 ) ) }'`");
				pos.println("\texport PLAT;");
				pos.println("\tif test ! -d ./plat/${PLAT} ; then");
				pos.println("\t\techo \"Can not determine plat automatically,\"");
				pos.println("\t\techo \"Please set the environment variable PLAT.\"");
				pos.println("\t\texit -1");
				pos.println("\tfi");
				pos.println("\techo Setting PLAT to ${PLAT}");
				pos.println("fi");
				pos.println("");// blank lineNumber
			    }
			pos.println("");// blank lineNumber
			pos.println("LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${RCSLIB_MAIN_DIR}:${RCSLIB_MAIN_DIR}/plat/${PLAT}/lib;");
			pos.println("export LD_LIBRARY_PATH");
			pos.println("");// blank lineNumber
			pos.println("echo LD_LIBRARY_PATH=${LD_LIBRARY_PATH}");
			pos.println("");// blank lineNumber

			if(useJavaInScripts)
			    {
				if(java_setup.length() > 1)
				    {
					pos.println("# Java Setup");
					StringTokenizer java_setup_tokenizer = new StringTokenizer(java_setup, "\n\r");
					while(java_setup_tokenizer.hasMoreTokens())
					    {
						String java_setup_line = java_setup_tokenizer.nextToken();
						pos.println(java_setup_line);
					    }
				    }
			    }
			pos.println("");// blank lineNumber
			pos.println("# Store current host name in NML configuration file");
			pos.println("if test \"x${HOST}\" != \"x\" ;  then");
			pos.println("\tif test -f "+AppName+".nml.local ; then");
			pos.println("\t\t\\rm -f "+AppName+".nml");
			pos.println("\t\tcat "+AppName+".nml.local | sed s/localhost/${HOST}/ >"+AppName+".nml");
			pos.println("\tfi");
			pos.println("fi");
			pos.println("");// blank lineNumber
			Enumeration svrkey = serversHashtable.keys();
			while(svrkey.hasMoreElements())
			    {
				pos.println("");// blank lineNumber
				String svrname = (String) svrkey.nextElement();
				ServerInfo si = (ServerInfo) serversHashtable.get(svrname);
				if(null == si)
				    {
					continue;
				    }
				if(si.bufferNames == null || si.bufferNames.size() < 1)
				    {
					continue;
				    }
				pos.println("# "+svrname);
				boolean use_host_query = false;
				if(null != si.Host)
				    {
					if(!si.Host.equals("localhost") && si.Host.length() > 1)
					    {
						use_host_query = true;
					    }
				    }
				pos.println("\\rm -f "+svrname+".log");
				if(use_host_query)
				    {
					pos.println("if test  `hostname` = "+si.Host+" ; then");
				    }
				pos.println("\techo Starting "+svrname+" . . .");
				if(singleDir)
				    {
					pos.println("\tif test ! -x ./"+svrname+" ; then");
					pos.println("\t\techo Can not execute ./"+svrname);
					pos.println("\tfi");
				    }
				else
				    {
					pos.println("\tif test ! -x ./plat/$PLAT/bin/"+svrname+" ; then");
					pos.println("\t\techo Can not execute ./plat/$PLAT/bin/"+svrname);
					pos.println("\tfi");
				    }
			
				if(term_start.startsWith("xterm"))
				    {
					if(singleDir)
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t\t${xterm_cmd} -lf "+svrname+".log -title \""+svrname.toUpperCase()+"\" -iconic -e ./"+svrname+" &");
						pos.println("\telse");
						pos.println("\t\t./"+svrname+" &");
						pos.println("\tfi");			
					    }
					else
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t\t${xterm_cmd} -lf "+svrname+".log -title \""+svrname.toUpperCase()+"\" -iconic -e ./plat/$PLAT/bin/"+svrname+" &");
						pos.println("\telse");
						pos.println("\t\t./plat/$PLAT/bin/"+svrname+" &");
						pos.println("\tfi");			
					    }
				    }
				else
				    {
					if(singleDir)
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 

						pos.println("\t${xterm_cmd} ./"+svrname+" &");
						pos.println("\telse");
						pos.println("\t\t./"+svrname+" &");
						pos.println("\tfi");			
					    }
					else
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t${xterm_cmd} ./plat/$PLAT/bin/"+svrname+" &");
						pos.println("\telse");
						pos.println("\t\t./plat/$PLAT/bin/"+svrname+" &");
						pos.println("\tfi");			
					    }
				    }

				if(use_host_query)
				    {
					pos.println("else");
					pos.println("\techo Make sure the program "+svrname+" has been started on the host "+si.Host+".");
					pos.println("\techo Press enter when ready.");
					pos.println("\tread ready");
					pos.println("fi");
				    }
				pos.println("");// blank lineNumber
			    }
			pos.println("\tsleep 2");
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name = gui.mainLoopList.getItem(i);
				rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) mainloopsHashtable.get(loop_name);
				if(mli == null)
				    {
					continue;
				    }
				if(mli.getModules() == null || mli.getModules().size() < 1)
				    {
					continue;
				    }
				boolean use_host_query = false;
				if(null != mli.host)
				    {
					if(!mli.host.equals("localhost") && mli.host.length() > 1)
					    {
						use_host_query = true;
					    }
				    }
				pos.println("");// blank lineNumber
				pos.println("# "+loop_name+"main");
				pos.println("\\rm -f "+loop_name+"main.log");
				if(use_host_query)
				    {
					pos.println("if test `hostname` = "+mli.host+" ; then");
				    }
				pos.println("\techo Starting "+loop_name+"main  . . .");
				if(singleDir)
				    {
					pos.println("\tif test ! -x ./"+loop_name+"main ; then");
					pos.println("\t\techo Can not execute ./"+loop_name+"main");
					pos.println("\tfi");
				    }
				else
				    {
					pos.println("\tif test ! -x ./plat/$PLAT/bin/"+loop_name+"main ; then");
					pos.println("\t\techo Can not execute ./plat/$PLAT/bin/"+loop_name+"main");
					pos.println("\tfi");
				    }
				if(term_start.startsWith("xterm"))
				    {
					if(singleDir)
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t\t${xterm_cmd}  -lf "+loop_name+"main.log -title \""+loop_name.toUpperCase()+"MAIN\" -iconic -e ./"+loop_name+"main &");
						pos.println("\telse");
						pos.println("\t\t ./"+loop_name+"main &");
						pos.println("\tfi");	
					    }
					else
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t${xterm_cmd}  -lf "+loop_name+"main.log -title \""+loop_name.toUpperCase()+"MAIN\" -iconic -e ./plat/$PLAT/bin/"+loop_name+"main &");
						pos.println("\telse");
						pos.println("\t\t./plat/$PLAT/bin/"+loop_name+"main &");
						pos.println("\tfi");	
					    }
				    }
				else
				    {
					if(singleDir)
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t\t${xterm_cmd} ./"+loop_name+"main &");
						pos.println("\telse");
						pos.println("\t\t ./"+loop_name+"main &");
						pos.println("\tfi");	
					    }
					else
					    {
						pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
						pos.println("\t${xterm_cmd} ./plat/$PLAT/bin/"+loop_name+"main &");
						pos.println("\telse");
						pos.println("\t\t./plat/$PLAT/bin/"+loop_name+"main &");
						pos.println("\tfi");	
					    }
				    }			
				if(use_host_query)
				    {
					pos.println("else");
					pos.println("\techo Make sure the program "+loop_name+"main has been started on the host "+mli.host+".");
					pos.println("\techo Press enter when ready.");
					pos.println("\tread ready");
					pos.println("fi");
				    }
			    }
			pos.println("\tsleep 2");
			pos.println("");// blank lineNumber
			if(useJavaInScripts)
			    {
				pos.println("echo \"Run Diagnostics Tool? '(y/n)'\"");
				pos.println("read diag_confirm");
				pos.println("");// blank lineNumber
				pos.println("if test \"x${diag_confirm}\" = \"xy\" ;  then");
				pos.println("");// blank lineNumber
				if( null != gui.diagCommandField)
				    {
					gui.diag_cmd = gui.diagCommandField.getText();
				    }
				if(null == gui.diag_cmd || gui.diag_cmd.length() < 2)
				    {
					gui.diag_cmd=java_cmd_prefix+" -jar "+RcsLibDir+File.separator+"plat"+File.separator+"java"+File.separator+"lib"+File.separator+"diag_NB.jar";
				    }
                                pos.println("\tif test \"x${diag_cmd}\" = \"x\" ; then");
                                pos.println("\t\tdiag_cmd=\""+gui.diag_cmd+"\";");
                                pos.println("\tfi");
                                pos.println("");// blank lineNumber
				pos.println("\techo Starting Diagnostics Tool . . .");
				pos.println("\tif test \"x${USE_XTERM}\" = \"x1\" ; then"); 
				if(term_start.startsWith("xterm"))
				    {
					pos.println("\t\tif test \"x${ec}\" = \"x\" -a -x ./.ec ; then");
					pos.println("\t\t\tec=./.ec;");
					pos.println("\t\tfi");

					pos.println("\t\techo ${xterm_cmd} -lf diagapplet.log -title Diagnostics_IO_Term -iconic -e ${ec} ${diag_cmd} "+AppName+".cfg");
					pos.println("\t\t${xterm_cmd}   -lf diagapplet.log -title Diagnostics_IO_Term -iconic -e ${ec} ${diag_cmd} "+AppName+".cfg &");
				    }
				else
				    {
					pos.println("\t\techo ${xterm_cmd} ${diag_cmd} "+AppName+".cfg");
					pos.println("\t\t${xterm_cmd} ${diag_cmd} "+AppName+".cfg &");
				    }
				pos.println("\telse");
				pos.println("\t\techo ${diag_cmd} "+AppName+".cfg");
				pos.println("\t\t${diag_cmd}  "+AppName+".cfg &");
				pos.println("\tfi");
		
				pos.println("\tsleep 2");
				pos.println("");// blank lineNumber
				pos.println("fi");
			    }
			pos.println("");// blank lineNumber
			pos.println("echo \"Shutdown?\"");
			pos.println("echo \"Enter \"\\\"y\\\"\" when you are ready to shutdown the controllers and servers,\"");
			pos.println("echo \"or \"\\\"n\\\"\" to exit the script with everything running.\"");
			pos.println("read shutdown_confirm");
			pos.println("while test \"x${shutdown_confirm}\" != \"xn\" -a \"x${shutdown_confirm}\" != \"xy\" ; do ");
			pos.println("\techo \"Shutdown? '(y/n)'\"");
			pos.println("\tread shutdown_confirm");
			pos.println("done");
			pos.println("");// blank lineNumber
			pos.println("if test \"x${shutdown_confirm}\" = \"xy\" ; then");
			pos.println("");// blank lineNumber
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name = gui.mainLoopList.getItem(i);
				String loopmain_name = loop_name+"main";
				if(loopmain_name.length() > 8)
				    {
					loopmain_name = loopmain_name.substring(0,8);
				    }
				pos.println("");// blank lineNumber
				pos.println("\t"+loop_name+"main_pid=`ps -ea | grep "+loopmain_name+" | grep -v run. | awk '{print $1}' `");
				pos.println("\techo Killing "+loop_name+"main, pid = $"+loop_name+"main_pid");
				pos.println("\tkill -INT $"+loop_name+"main_pid");
			    }
			pos.println("\tsleep 2");
			svrkey = serversHashtable.keys();
			while(svrkey.hasMoreElements())
			    {
				String svrname = (String) svrkey.nextElement();
				String short_svrname = svrname;
				if(svrname.length() > 8)
				    {
					short_svrname = svrname.substring(0,8);
				    }
				pos.println("\t"+svrname+"_pid=`ps -ea | grep "+short_svrname+" | grep -v run. | awk '{print $1}' `");
				pos.println("\techo Killing "+svrname+", pid = $"+svrname+"_pid");
				pos.println("\tkill -INT $"+svrname+"_pid");
			    }


			pos.println("");// blank lineNumber
			pos.println("\tsleep 2");
			pos.println("");// blank lineNumber
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name = gui.mainLoopList.getItem(i);
				String loopmain_name = loop_name+"main";
				if(loopmain_name.length() > 8)
				    {
					loopmain_name = loopmain_name.substring(0,8);
				    }
				pos.println("");// blank lineNumber
				pos.println("\t"+loop_name+"main_pid=`ps -ea | grep "+loopmain_name+" | grep -v run. | awk '{print $1}' `");
				pos.println("\techo Killing "+loop_name+"main, pid = $"+loop_name+"main_pid");
				pos.println("\tkill -KILL $"+loop_name+"main_pid");
			    }
			pos.println("\tsleep 2");
			svrkey = serversHashtable.keys();
			while(svrkey.hasMoreElements())
			    {
				String svrname = (String) svrkey.nextElement();
				String short_svrname = svrname;
				if(svrname.length() > 8)
				    {
					short_svrname = svrname.substring(0,8);
				    }
				pos.println("\t"+svrname+"_pid=`ps -ea | grep "+short_svrname+" | grep -v run. | awk '{print $1}' `");
				pos.println("\techo Killing "+svrname+", pid = $"+svrname+"_pid");
				pos.println("\tkill -KILL $"+svrname+"_pid");
			    }

			pos.println("");// blank lineNumber
			pos.println("fi");
			pos.println("");// blank lineNumber
		    }
		else
		    {
			pos.println("echo off");
			pos.println("REM Batch File for running all the modules/servers in "+AppName);
			pos.println("");// blank lineNumber
			pos.println("REM CD to the User Directory, just in-case we're not already there.");
			pos.println("cd "+userDirFile.getAbsolutePath());
			pos.println("");// blank lineNumber
			if(useJavaInScripts)
			    {
				if(java_setup.length() > 1)
				    {
					pos.println("REM Java Setup");
					StringTokenizer java_setup_tokenizer = new StringTokenizer(java_setup, "\n\r");
					while(java_setup_tokenizer.hasMoreTokens())
					    {
						String java_setup_line = java_setup_tokenizer.nextToken();
						pos.println(java_setup_line);
					    }
				    }
			    }
			pos.println("REM Make sure that rcs32msc.dll is on the PATH.");
			pos.println("set path=%path%;"+RcsLibDir+"\\PLAT\\WIN32MSC\\BIN;");
			pos.println("");// blank lineNumber
			pos.println("IF NOT EXIST \""+AppName+".nml\" COPY \""+AppName+".nml.local\" \""+AppName+".nml\"");
			pos.println("");// blank lineNumber
			Enumeration svrkey = serversHashtable.keys();
			while(svrkey.hasMoreElements())
			    {
				String svrname = (String) svrkey.nextElement();
				pos.println("echo Starting "+svrname);
				pos.println("start /MIN PLAT\\WIN32MSC\\BIN\\"+svrname+".exe");
				pos.println("pause");
			    }
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name = gui.mainLoopList.getItem(i);
				pos.println("");// blank lineNumber
				pos.println("echo Starting "+loop_name+"main");
				pos.println("start /MIN PLAT\\WIN32MSC\\BIN\\"+loop_name+"main.exe");
				pos.println("pause");
			    }
			pos.println("");// blank lineNumber
			if(useJavaInScripts)
			    {
				pos.println("REM Start Java Diagnostics Application");
				if( null != gui.diagCommandField)
				    {
					gui.diag_cmd = gui.diagCommandField.getText();
				    }
				pos.println(gui.diag_cmd +" HierarchyFile="+AppName+".cfg");
			    }


		    }
		pos.close();
		fos.close();
		pos = null;
		fos = null;
		if(!mswinDevPlat)
		    {
			try 
			    {
				System.out.println("Executing chmod a+rx "+scriptFile);
				Runtime.getRuntime().exec("chmod a+rx "+scriptFile);
			    }
			catch(Throwable t)
			    {
				t.printStackTrace();
			    }
		    }
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteEcScript(File dirFile) throws rcsDesignUserAbortException
    {
	try
	    {
		File scriptFile = null;
		if(mswinDevPlat)
		    {
			return;
		    }
		else
		    {
			scriptFile = new File(dirFile,".ec");
		    }
	

		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(scriptFile, false,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating ec script File for "+AppName);
		    }
		FileOutputStream fos = new FileOutputStream(scriptFile);
		PrintWriter pos = new PrintWriter(fos);
		File userDirFile = new File(getUserDir());
		pos.println("$*");
		pos.println("${SHELL}");
		pos.close();
		fos.close();
		pos = null;
		fos = null;
		System.out.println("Executing chmod a+rx "+scriptFile);
		Runtime.getRuntime().exec("chmod a+rx "+scriptFile);
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    String nml_attach_string  = "";
    int bufsize = 2048;

    String encodingType="xdr";

    void WriteNMLBufferLine(PrintWriter pos, String buffer_name)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsDesignWriter.WriteNMLBufferLine("+pos+", "+buffer_name+") :");
		    }
		BufferInfo bi = (BufferInfo) buffersHashtable.get(buffer_name);
		int buf_tcpport = tcpport;
		if(debug_on)
		    {
			System.out.println("BufferInfo bi = (BufferInfo) buffersHashtable.get("+buffer_name+"); ="+bi);
			System.out.println("int buf_tcpport = tcpport; ="+buf_tcpport);
		    }
		String buffer_host = "localhost";
		if(null != bi)
		    {
			if(null != bi.si)
			    {
				if(null != bi.si.Host)
				    {
					if(bi.si.Host.length() > 1)
					    {
						buffer_host = bi.si.Host;
					    }
				    }
				buf_tcpport += bi.si.id;
			    }
		    }
		String bufname_pad = "";
		for(int i = buffer_name.length(); i < 16; i++)
		    {
			bufname_pad += " ";
		    }
		String bufhost_pad = "";
		for(int i = buffer_host.length(); i < 16; i++)
		    {
			bufhost_pad += " ";
		    }
		merger.WriteLine("B "+buffer_name+bufname_pad+" \tSHMEM \t"+buffer_host+bufhost_pad+" \t"+bufsize+"\t0    \t* \t"+buffer_number+"        \t*     \t"+shmemkey+" TCP="+buf_tcpport+" "+encodingType+" "+nml_attach_string);
		shmemkey++;
		buffer_number++;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    int buffer_number = 1;
    int max_procs = 20;
    int max_modname_length = 0;
    long rpc_number = 0x20000000;
    int shmemkey = 1000;
    int tcpport = 2000;
    int process_cnum = 0;
    String current_processname = null;
    String current_processhost = "localhost";
    int is_server = 0;
    int is_master = 0;

    public void PrintMainLoopsHashtable()
    {
	if(null == mainloopsHashtable)
	    {
		System.out.println("rcsDesignWriter.mainloopsHashtable == null");
		return;
	    }
	System.out.println("* mainloopsHashtable");
	Enumeration keys = mainloopsHashtable.keys();
	while(keys.hasMoreElements())
	    {
		String main_loop_name = (String) keys.nextElement();
		rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) mainloopsHashtable.get(main_loop_name);
		System.out.println(main_loop_name+" : "+mli.toString());
	    }
    }

    public void PrintServersHashtable()
    {
	Enumeration keys = serversHashtable.keys();
	System.out.println("* serversHashtable");
	while(keys.hasMoreElements())
	    {
		String key = (String) keys.nextElement();
		ServerInfo si_temp = (ServerInfo) serversHashtable.get(key);
		System.out.println(key+":"+si_temp);
	    }
    }

    public void PrintBuffersHashtable()
    {
	Enumeration keys = buffersHashtable.keys();
	System.out.println("* buffersHashtable");
	while(keys.hasMoreElements())
	    {
		String key = (String) keys.nextElement();
		BufferInfo bi_temp = (BufferInfo) buffersHashtable.get(key);
		System.out.println(key+":"+bi_temp);
	    }
    }

    public void PrintModulesHashtable()
    {
	if(null == modulesHashtable)
	    {
		System.out.println("rcsDesignWriter.modulesHashtable == null");
		return;
	    }
	System.out.println("* modulesHashtable");
	Enumeration keys = modulesHashtable.keys();
	while(keys.hasMoreElements())
	    {
		String mod_name = (String) keys.nextElement();
		ModuleInfo mi = (ModuleInfo) modulesHashtable.get(mod_name);
		System.out.println(mod_name+" : "+mi.toString());
	    }
    }

    void StartProcess(PrintWriter pos, String process_name)
    {
	try
	    {
		process_cnum++;
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("# "+process_name+"("+process_cnum+")");
		current_processname = process_name;
		current_processhost = "localhost";
		rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) mainloopsHashtable.get(process_name);
		if(null != mli)
		    {
			if(null != mli.host)
			    {
				if(mli.host.length() > 1)
				    {
					current_processhost = mli.host;
				    }
			    }
		    }
		else
		    {
			ModuleInfo mi = (ModuleInfo) modulesHashtable.get(process_name);
			if(null != mi)
			    {
				if(null != mi.MainLoopName)
				    {
					if(debug_on)
					    {
						System.out.println("mi.MainLoopName = "+mi.MainLoopName);
					    }
					mli = (rcsdesignMainLoopInfo) mainloopsHashtable.get(mi.MainLoopName);
					if(null != mli)
					    {
						if(null != mli.host)
						    {
							if(mli.host.length() > 1)
							    {
								current_processhost = mli.host;
							    }
						    }
					    }
				    }
			    }
		    }
		ServerInfo si = (ServerInfo) serversHashtable.get(process_name);
		if(null != si)
		    {
			if(null != si.Host)
			    {
				if(si.Host.length() > 1)
				    {
					current_processhost = si.Host;
				    }
			    }
		    }
		if(debug_on)
		    {
			System.out.println("current_processhost = "+current_processhost);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteNMLProcessLine(PrintWriter pos, String buffer_name)
    {
	try
	    {
		if(mswinDevPlat)
		    {
			encodingType = "disp";
		    }
		BufferInfo bi = (BufferInfo) buffersHashtable.get(buffer_name);
		int buf_tcpport = tcpport;
		String buffer_host = "localhost";
		if(null != bi)
		    {
			if(null != bi.si)
			    {
				if(null != bi.si.Host)
				    {
					if(bi.si.Host.length() > 1)
					    {
						buffer_host = bi.si.Host;
					    }
				    }
				buf_tcpport += bi.si.id;
			    }
		    }
		String bufname_pad = "";
		for(int i = buffer_name.length(); i < 16; i++)
		    {
			bufname_pad += " ";
		    }
		String procname_pad = "";
		for(int i = current_processname.length(); i < 16; i++)
		    {
			procname_pad += " ";
		    }
		String prochost_pad = "";
		for(int i = current_processhost.length(); i < 16; i++)
		    {
			prochost_pad += " ";
		    }
		if(current_processhost.equals(buffer_host))
		    {
			String wfm_string;
			if(is_master != 0)
			    {
				wfm_string="";
			    }
			else
			    {
				wfm_string=" \twaitformaster";
			    }
			merger.WriteLine("P "+current_processname+procname_pad+" \t"+buffer_name+bufname_pad+" \tLOCAL  \t"+current_processhost+prochost_pad+" \tRW   \t"+is_server+"         \t0.1     \t"+is_master+"    \t"+process_cnum+wfm_string);
		    }
		else
		    {
			merger.WriteLine("P "+current_processname+procname_pad+" \t"+buffer_name+bufname_pad+" \tREMOTE \t"+current_processhost+prochost_pad+" \tRW   \t0         \t10.0    \t0     \t"+process_cnum);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteNMLLocalFile(File dirFile) throws rcsDesignUserAbortException
    {
	try
	    {
		File nmlFile = new File(dirFile,AppName+".nml.local");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		put_merger_in_nml_mode = true;
		if(CanNotOverwriteExistingFile(nmlFile, true,false))
		    {
			put_merger_in_nml_mode = false;
			return;
		    }
		put_merger_in_nml_mode = false;
		if(debug_on)
		    {
			System.out.println("Creating NML File for "+AppName);
		    }
		FileOutputStream fos = new FileOutputStream(nmlFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);
		merger.WriteLine("# NML Configuration file for the "+AppName+" application");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("# Buffers");
		max_procs = gui.modulesList.getItemCount() + 3;
		max_modname_length = 0;
		buffer_number = 1;
		merger.WriteLine("# Name             \tType  \tHost             \tsize \tneut? \tRPC#           \tbuffer# \tMP \t. . .");
		max_procs += (4 - max_procs%4);
		int ticks =(int) ( System.currentTimeMillis() % 1000);
		rpc_number = 0x20000000 + ((long) (Math.random() *      1.0e7))+ticks;
		shmemkey = ((int) (Math.random() *98000))+ticks+1000;
		shmemkey += (20 - (shmemkey %20));
		tcpport = ((int) (Math.random() *7000))+ticks+2000;
		bufsize = 2048;
		nml_attach_string  = "";
		for(int i = 0; i < gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			String pad = "";
			for(int k = modName.length(); k < max_modname_length; k++)
			    {
				pad += " ";
			    }
			WriteNMLBufferLine(null, modName+"_cmd");
			WriteNMLBufferLine(null, modName+"_sts");
		    }
		String errlogpad = "";
		for(int ek = 2; ek < max_modname_length; ek++)
		    {
			errlogpad += " ";
		    }
		nml_attach_string  = " queue";
		bufsize = 8192;
		WriteNMLBufferLine(null, "errlog");
		shmemkey++;
		shmemkey += 20 - (shmemkey%10);
		nml_attach_string  = "";
		bufsize = 2048;
		int bufnum = gui.modulesList.getItemCount()*2+1;
		bufnum++;
		bufnum += 20 - (bufnum%10);
		if(null != auxChannelsVector)
		    {
			for(int i = 0; i < auxChannelsVector.size(); i++)
			    {
				String auxName = (String) auxChannelsVector.elementAt(i);
				WriteNMLBufferLine(null,auxName);
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("# Processes");
		merger.WriteLine("# Name             \tBuffer           \tType    \tHost             \tOps \tserver? \ttimeout \tmaster? \tcnum");
		process_cnum = -1;
		current_processname = null;
		current_processhost = "localhost";
		for(int i = 0; i < gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			String pad = "";
			for(int k = modName.length(); k < max_modname_length; k++)
			    {
				pad += " ";
			    }
			is_master=0;
			is_server=0;
			StartProcess(null, modName);
			WriteNMLProcessLine(null, modName+"_cmd");
			WriteNMLProcessLine(null, modName+"_sts");
			ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
			if(null != modInfo)
			    {
				if(null != modInfo.children_names)
				    {
					for(int j = 0; j < modInfo.children_names.size(); j++)
					    {
						String childname = (String) modInfo.children_names.elementAt(j);
						String childpad = "";
						for(int ck = childname.length(); ck < max_modname_length; ck++)
						    {
							childpad += " ";
						    }
						WriteNMLProcessLine(null,childname+"_cmd");
						WriteNMLProcessLine(null,childname+"_sts");
					    }
				    }
				if(null != modInfo.AuxInputNames)
				    {
					for(int j = 0; j < modInfo.AuxInputNames.size(); j++)
					    {
						String auxName = (String) modInfo.AuxInputNames.elementAt(j);
						String auxpad = "";
						for(int k = auxName.length(); k < max_modname_length+4; k++)
						    {
							auxpad += " ";
						    }
						merger.WriteLine("P "+modName+pad+" \t"+auxName+auxpad+" \tLOCAL \tlocalhost \tR   \t0         \t0.1     \t0     \t"+i);
					    }
				    }
				if(null != modInfo.AuxOutputNames)
				    {
					for(int j = 0; j < modInfo.AuxOutputNames.size(); j++)
					    {
						String auxName = (String) modInfo.AuxOutputNames.elementAt(j);
						String auxpad = "";
						for(int k = auxName.length(); k < max_modname_length+4; k++)
						    {
							auxpad += " ";
						    }
						WriteNMLProcessLine(null, auxName);
					    }
				    }
			    }
			WriteNMLProcessLine(null, "errlog");
		    }

		merger.WriteLine("");// blank lineNumber
		for(int svrindex = 0; svrindex < gui.serverList.getItemCount(); svrindex++)
		    {
			String svrname = gui.serverList.getItem(svrindex);
			StartProcess(null,svrname);
			is_server = 1;
			is_master = 1;
			ServerInfo si = (ServerInfo) serversHashtable.get(svrname);
			if(null != si)
			    {
				if(null != si.bufferNames)
				    {
					// Remove duplicates
					for(int j = 0; j < si.bufferNames.size(); j++)
					    {
						String bufnamej = (String) si.bufferNames.elementAt(j);
						for(int k = j+1; k < si.bufferNames.size(); k++)
						    {
							String bufnamek = (String) si.bufferNames.elementAt(k);
							if(bufnamek.equals(bufnamej))
							    {
								if(debug_on)
								    {
									System.out.println("rcsDesignWriter.WriteNMLLocalFile Removing redundant element "+bufnamek+"  from si.bufferNames at "+k);
								    }
								si.bufferNames.removeElementAt(k);
								k--;
							    }
						    }
					    }
					for(int j = 0; j < si.bufferNames.size(); j++)
					    {
						String bufname = (String) si.bufferNames.elementAt(j);
						WriteNMLProcessLine(null, bufname);
					    }
				    }
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("");// blank lineNumber
		merger.Finish();
		pos.close();
		pos = null;
		fos.close();
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteTopLevelMakefile(File dirFile)  throws rcsDesignUserAbortException
    {
	try
	    {
		File makeFile = new File(dirFile,"Makefile");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(makeFile, false,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Top-Level Makefile for "+dirFile);
		    }
		FileOutputStream fos = new FileOutputStream(makeFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# Top-Level Makefile automatically generated by rcsdesign on");
		pos.println("# "+new Date());
		pos.println("# This makefile should call for makes in each of the");
		pos.println("# src subdirctories in this application");
		pos.println("");// blank lineNumber
		pos.println("SRCS = ");
		pos.println("");// blank lineNumber
		pos.println("HEADERS = ");
		pos.println("");// blank lineNumber
		pos.println("OBJS = ");
		pos.println("");// blank lineNumber
		pos.println("LIBS = ");
		pos.println("");// blank lineNumber
		pos.println("BINS = ");
		pos.println("");// blank lineNumber
		pos.println("# Include Application Specific Definions and the RCS generic Makefile");
		if(mswinDevPlat)
		    {
			pos.println("!INCLUDE <Makefile.inc>");
		    }
		else
		    {
			pos.println("include Makefile.inc");
		    }
		pos.println("");// blank lineNumber
		File srcdirFile = new File(dirFile,"src");
		if(!srcdirFile.exists())
		    {
			pos.close();
			fos.close();
			pos = null;
			fos = null;
			return;
		    }
		pos.println("# Rules to go down into subdirectories");
		pos.println("all clean depend sources headers:");
		File intfDirFile = new File(srcdirFile,"intf");
		if(intfDirFile.exists() && intfDirFile.isDirectory())
		    {
			if(mswinDevPlat)
			    {
				pos.println("\tcd src\\intf");
				pos.println("\t$(MAKE) $@");
				pos.println("\tcd ..\\..");
			    }
			else
			    {
				pos.println("\t( cd src"+File.separator+"intf; $(MAKE) $@;)");
			    }
		    }
		File utilDirFile = new File(srcdirFile,"util");
		if(utilDirFile.exists() && utilDirFile.isDirectory())
		    {
			if(mswinDevPlat)
			    {
				pos.println("\tcd src\\util");
				pos.println("\t$(MAKE) $@");
				pos.println("\tcd ..\\..");
			    }
			else
			    {
				pos.println("\t( cd src"+File.separator+"util; $(MAKE) $@;)");
			    }
		    }
		for(int i = 0; i < gui.modulesList.getItemCount(); i++)
		    {
			String moddir = gui.modulesList.getItem(i);
			File modDirFile = new File(srcdirFile,moddir);
			if(debug_on)
			    {
				System.out.println("modDirFile = "+modDirFile);
			    }
			if(modDirFile.exists() && modDirFile.isDirectory())
			    {
				if(mswinDevPlat)
				    {
					pos.println("\tcd src\\"+moddir);
					pos.println("\t$(MAKE) $@");
					pos.println("\tcd ..\\..");
				    }
				else
				    {
					pos.println("\t( cd src"+File.separator+moddir+"; $(MAKE) $@;)");
				    }
			    }
		    }
		File mainDirFile = new File(srcdirFile,"main");
		if(mainDirFile.exists() && mainDirFile.isDirectory())
		    {
			if(mswinDevPlat)
			    {
				pos.println("\tcd src\\main");
				pos.println("\t$(MAKE) $@");
				pos.println("\tcd ..\\..");
			    }
			else
			    {
				pos.println("\t( cd src"+File.separator+"main; $(MAKE) $@;)");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteAppIncMakefile(File dirFile)  throws rcsDesignUserAbortException
    {
	try
	    {
		File makeFile = new File(dirFile,"Makefile.inc");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(makeFile, false,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Application Include Makefile for "+dirFile);
		    }
		FileOutputStream fos = new FileOutputStream(makeFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# Makefile automatically generated by rcsdesign on");
		pos.println("# "+new Date()+" for "+dirFile);
		pos.println("# This makefile should define the APPDIR any other");
		pos.println("# variables used thoughout the application such as");
		pos.println("# LOCAL_CFLAGS, or RCSLIB_MAIN_DIR and then include the");
		pos.println("# RCS generic makefile definitions.");
		pos.println("");//blank lineNumber
		if(null != AppDir)
		    {
			pos.println("# Set the Release Directory for this Application");
			pos.println("APPDIR = "+AppDir);
			pos.println("");//blank lineNumber
		    }
		File UserDirFile = new File(getUserDir());
		if(null != getUserDir())
		    {
			pos.println("# Set the Development Directory for this user");
			pos.println("USER_DIR = "+UserDirFile.getAbsolutePath());
			pos.println("");//blank lineNumber
		    }
		pos.println("");// blank lineNumber
		if(null != RcsLibDir)
		    {
			if(RcsLibDir.length() > 0)
			    {
				pos.println("# Set the main RCS Library Directory");
				pos.println("RCSLIB_MAIN_DIR="+RcsLibDir);
			    }
		    }
		pos.println("");// blank lineNumber
		if(gui.libsList.getItemCount() > 0)
		    {
			pos.println("EXTRA_LIBS = \\");
			for(int i = 0; i < gui.libsList.getItemCount(); i++)
			    {
				if(i < gui.libsList.getItemCount() -1)
				    {
					pos.println("\t"+gui.libsList.getItem(i)+" \\");
				    }
				else
				    {
					pos.println("\t"+gui.libsList.getItem(i));
				    }
			    }
			pos.println("");// blank lineNumber
		    }

		if(gui.includesList.getItemCount() > 0)
		    {
			pos.println("EXTRA_INCLUDES = \\");
			for(int i = 0; i < gui.includesList.getItemCount(); i++)
			    {
				if(i < gui.includesList.getItemCount() -1)
				    {
					pos.println("\t-I"+gui.includesList.getItem(i)+" \\");
				    }
				else
				    {
					pos.println("\t-I"+gui.includesList.getItem(i));
				    }
			    }
			pos.println("");// blank lineNumber
			pos.println("LOCAL_CFLAGS = $(EXTRA_INCLUDES)");

		    }
		pos.println("# Include the generic RCS makefile definitions");
		if(mswinDevPlat)
		    {
			pos.println("!INCLUDE <$(RCSLIB_MAIN_DIR)\\etc\\nmakegen.def>");
		    }
		else
		    {
			pos.println("include $(RCSLIB_MAIN_DIR)/etc/generic.def");
		    }
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteUtilMakefile(File dirFile)  throws rcsDesignUserAbortException
    {
	try
	    {
		File makeFile = new File(dirFile,"Makefile");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(makeFile, false, false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Makefile for "+dirFile);
		    }
		FileOutputStream fos = new FileOutputStream(makeFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# Makefile automatically generated by rcsdesign on");
		pos.println("# "+new Date()+" for "+dirFile);
		pos.println("# This makefile should define the sources, headers and objects");
		pos.println("# stored or created with this directory and then include");
		pos.println("# the application include Makefile.");
		pos.println("");// blank lineNumber
		pos.println("SRCS =  \\");
		String cpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+cpp_ext));
		for(int i = 0; i < cpps.length; i++)
		    {
			if(cpps[i].indexOf(',') >= 0 || cpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+cpps[i]+" \\");
		    }
		String cs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.c"));
		for(int i = 0; i < cs.length; i++)
		    {
			if(cs[i].indexOf(',') >= 0 || cs[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+cs[i]+" \\");
		    }
		if(forced_cpps != null)
		    {
			StringTokenizer forced_cpps_tokenizer = new StringTokenizer(forced_cpps, " \t\r\n\b,;:");
			while(forced_cpps_tokenizer.hasMoreTokens())
			    {
				String forced_cpp = forced_cpps_tokenizer.nextToken();
				boolean forced_cpp_found = false;
				for(int i = 0; i < cpps.length; i++)
				    {
					if(forced_cpp.equals(cpps[i]))
					    {
						forced_cpp_found = true;
						break;
					    }
				    }
				if(forced_cpp_found)
				    {
					continue;
				    }
				for(int i = 0; i < cs.length; i++)
				    {
					if(forced_cpp.equals(cs[i]))
					    {
						forced_cpp_found = true;
						break;
					    }
				    }
				if(forced_cpp_found)
				    {
					continue;
				    }
				pos.println("\t"+forced_cpp+" \\");
			    }
		    }

		pos.println("");// blank lineNumber
		pos.println("HEADERS = \\");
		String hpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+hpp_ext));
		for(int i = 0; i < hpps.length; i++)
		    {
			if(hpps[i].indexOf(',') >= 0 || hpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+hpps[i]+" \\");
		    }
		String hs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.h"));
		for(int i = 0; i < hs.length; i++)
		    {
			if(hs[i].indexOf(',') >= 0 || hs[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+hs[i]+" \\");
		    }
		if(forced_hpps != null)
		    {
			StringTokenizer forced_hpps_tokenizer = new StringTokenizer(forced_hpps, " \t\r\n\b,;:");
			while(forced_hpps_tokenizer.hasMoreTokens())
			    {
				String forced_hpp = forced_hpps_tokenizer.nextToken();
				boolean forced_hpp_found = false;
				for(int i = 0; i < hpps.length; i++)
				    {
					if(forced_hpp.equals(hpps[i]))
					    {
						forced_hpp_found = true;
						break;
					    }
				    }
				if(forced_hpp_found)
				    {
					continue;
				    }
				for(int i = 0; i < hs.length; i++)
				    {
					if(forced_hpp.equals(hs[i]))
					    {
						forced_hpp_found = true;
						break;
					    }
				    }
				if(forced_hpp_found)
				    {
					continue;
				    }
				pos.println("\t"+forced_hpp+" \\");
			    }
		    }

		pos.println("");// blank lineNumber
		pos.println("OBJS = \\");
		String cppso[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+cpp_ext));
		for(int i = 0; i < cppso.length; i++)
		    {
			if(cppso[i].indexOf(',') >= 0 || cppso[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+cppso[i].substring(0,cppso[i].length()-cpp_ext.length())+obj_ext+" \\");
		    }
		String cso[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.c"));
		for(int i = 0; i < cso.length; i++)
		    {
			if(cso[i].indexOf(',') >= 0 || cso[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+cso[i].substring(0,cso[i].length()-2)+obj_ext+" \\");
		    }
		if(forced_objs != null)
		    {
			StringTokenizer forced_objs_tokenizer = new StringTokenizer(forced_objs, " \t\r\n\b,;:");
			while(forced_objs_tokenizer.hasMoreTokens())
			    {
				String forced_obj = forced_objs_tokenizer.nextToken();
				if(forced_obj.length() < 2)
				    {
					continue;
				    }
				boolean forced_obj_found = false;
				for(int i = 0; i < cppso.length; i++)
				    {
					if(cppso[i].length() <= cpp_ext.length())
					    {
						continue;
					    }
					String cppso_object_file = cppso[i].substring(0,cppso[i].length()-cpp_ext.length())+obj_ext;
					if(forced_obj.equals(cppso_object_file))
					    {
						forced_obj_found = true;
						break;
					    }
				    }
				if(forced_obj_found)
				    {
					continue;
				    }
				for(int i = 0; i < cso.length; i++)
				    {
					if(cso[i].length() <= 2)
					    {
						continue;
					    }
					String cso_object_file = cso[i].substring(0,cso[i].length()-2)+obj_ext;
					if(forced_obj.equals(cso_object_file))
					    {
						forced_obj_found = true;
						break;
					    }
				    }
				if(forced_obj_found)
				    {
					continue;
				    }
				pos.println("\t"+forced_obj+" \\");
			    }
		    }

		pos.println("");// blank lineNumber
		pos.println("LIBS = ");
		pos.println("");// blank lineNumber
		pos.println("BINS = ");
		pos.println("");// blank lineNumber
		pos.println("# Include Application Specific Definitions and the RCS generic Makefile");
		if(mswinDevPlat)
		    {
			pos.println("!INCLUDE <..\\..\\Makefile.inc>");
		    }
		else
		    {
			pos.println("include ../../Makefile.inc");
		    }
		pos.println("");// blank lineNumber
		if(mswinDevPlat)
		    {
			pos.println("DEVP_OBJS = \\");
			for(int i = 0; i < cpps.length; i++)
			    {
				pos.println("\t$(DEVP_LIB_DIR)\\"+cppso[i].substring(0,cppso[i].length()-cpp_ext.length())+obj_ext+" \\");
			    }
			for(int i = 0; i < cs.length; i++)
			    {
				pos.println("\t$(DEVP_LIB_DIR)\\"+cso[i].substring(0,cso[i].length()-1)+obj_ext+" \\");
			    }
			pos.println("");// blank lineNumber
			pos.println("all: $(DEVP_OBJS)");
			pos.println("");// blank lineNumber

		    }

		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    void WriteServerCpp(File dirFile, String serverName)  throws rcsDesignUserAbortException
    {
	try
	    {
		File serverFile = new File(dirFile,serverName+cpp_ext);
		ServerInfo si = (ServerInfo) serversHashtable.get(serverName);
		if(null == si)
		    {
			gui.Alert("No information on server "+serverName);
			return;
		    }
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(serverFile, false,true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating NML Server C++ File for "+AppName);
		    }
		FileOutputStream fos = new FileOutputStream(serverFile);
		PrintWriter pos = new PrintWriter(fos);
		Date createDate = new Date();
		pos.println("/*");
		pos.println("\t"+serverName+cpp_ext);
		pos.println("");// blank lineNumber
		pos.println("\tThis C++ file provides a main routin to start an NML server for this application");
		pos.println("\tIt connects to all of the NML channels used by this application.");
		pos.println("\tIf they are all valid it will call run_nml_servers(), otherwise it will exit immediately.");
		pos.println("");
		pos.println("\tMODIFICATIONS:");
		pos.println("\t"+createDate+"\tCreated.");
		pos.println("");// blank lineNumber
		pos.println("*/");
		pos.println("");// blank lineNumber
		pos.println("// Include Files");
		pos.println("#include <stdlib.h> // exit()");
		pos.println("#include \"rcs.hh\" \t// Common RCS definitions");
		for(int i = 0; i< gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			boolean mod_header_needed = false;
			for(int j = 0; j < si.bufferNames.size(); j++)
			    {
				String bufname = (String) si.bufferNames.elementAt(j);
				if(bufname.indexOf(modName) >= 0 || modName.indexOf(bufname) >= 0)
				    {
					mod_header_needed = true;
					break;
				    }
			    }
			if(!mod_header_needed)
			    {
				continue;
			    }
			ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
			if(modInfo == null || null == modInfo.cmdsTypeFile)
			    {
				pos.println("#include \""+modName+"n"+hpp_ext+"\" \t// "+modName+"Format");
			    }
			else
			    {
				String cmd_header = modInfo.cmdsTypeFile;
				int slash_index = cmd_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					cmd_header = cmd_header.substring(slash_index+1);
				    }
				pos.println("#include \""+cmd_header+"\" \t// NML Commands and Status definitions for "+modName);
				if(modInfo != null && null != modInfo.cmdsTypeFile)
				    {
					String stat_header = modInfo.statsTypeFile;
					slash_index = stat_header.lastIndexOf(File.separator);
					if(slash_index > 0)
					    {
						stat_header = stat_header.substring(slash_index+1);
					    }
					if(!stat_header.equals(cmd_header))
					    {
						pos.println("#include \""+stat_header+"\" \t// NML Status definitions for "+modName);
					    }
				    }
			    }
		    }
		if(null != auxChannelsVector)
		    {
			for(int i = 0; i < auxChannelsVector.size(); i++)
			    {
				boolean aux_header_needed = false;
				String auxName = (String) auxChannelsVector.elementAt(i);
				for(int j = 0; j < si.bufferNames.size(); j++)
				    {
					String bufname = (String) si.bufferNames.elementAt(j);
					if(bufname.indexOf(auxName) >= 0 || auxName.indexOf(bufname) >= 0)
					    {
						aux_header_needed = true;
						break;
					    }
				    }
				if(!aux_header_needed)
				    {
					continue;
				    }
				pos.println("#include \""+auxName+"n"+hpp_ext+"\" \t// "+auxName+"Format");
			    }
		    }

		pos.println("");// blank lineNumber
		pos.println("// NML Channel Pointers");
		for(int i = 0; i< gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			boolean mod_needed = false;
			for(int j = 0; j < si.bufferNames.size(); j++)
			    {
				String bufname = (String) si.bufferNames.elementAt(j);
				if(bufname.indexOf(modName) >= 0 || modName.indexOf(bufname) >= 0)
				    {
					mod_needed = true;
					break;
				    }
			    }
			if(!mod_needed)
			    {
				continue;
			    }
			pos.println("static RCS_CMD_CHANNEL *"+modName+"_cmd = NULL;");
			pos.println("static RCS_STAT_CHANNEL *"+modName+"_stat = NULL;");
		    }
		if(null != auxChannelsVector)
		    {
			for(int i = 0; i < auxChannelsVector.size(); i++)
			    {
				String auxName = (String) auxChannelsVector.elementAt(i);
				boolean aux_needed = false;
				for(int j = 0; j < si.bufferNames.size(); j++)
				    {
					String bufname = (String) si.bufferNames.elementAt(j);
					if(bufname.indexOf(auxName) >= 0 || auxName.indexOf(bufname) >= 0)
					    {
						aux_needed = true;
						break;
					    }
				    }
				if(!aux_needed)
				    {
					continue;
				    }pos.println("static NML *"+auxName+"= NULL;");
			    }
		    }
		boolean errlog_needed = false;
		for(int j = 0; j < si.bufferNames.size(); j++)
		    {
			String bufname = (String) si.bufferNames.elementAt(j);
			if(bufname.equals("errlog"))
			    {
				errlog_needed = true;
				break;
			    }
		    }
		if(errlog_needed)
		    {
			pos.println("static NML *errlog = NULL;");
		    }

		pos.println("");// blank lineNumber
		pos.println("static int InitNML()");
		pos.println("{");
		pos.println("");// blank lineNumber
		for(int i = 0; i< gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			boolean mod_needed = false;
			boolean mod_stat_needed = false;
			boolean mod_cmd_needed = false;
			for(int j = 0; j < si.bufferNames.size(); j++)
			    {
				String bufname = (String) si.bufferNames.elementAt(j);
				if(bufname.indexOf(modName) >= 0 || modName.indexOf(bufname) >= 0)
				    {
					mod_needed = true;
				    }
				else
				    {
					continue;
				    }
				if(bufname.equals(modName+"_cmd"))
				    {
					mod_cmd_needed = true;
				    }
				else if(bufname.equals(modName+"_sts"))
				    {
					mod_stat_needed = true;
				    }
				if(mod_stat_needed && mod_cmd_needed)
				    {
					break;
				    }
			    }
			if(!mod_needed)
			    {
				continue;
			    }
			pos.println("");// blank lineNumber
			ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
			String format_function = modName+"Format";
			if(mod_cmd_needed)
			    {
				pos.println("\t// "+modName);
				format_function = modName+"Format";
				if(modInfo != null  && modInfo.cmdFormatFunction != null)
				    {
					format_function = modInfo.cmdFormatFunction;
				    }
				pos.println("\t"+modName+"_cmd = new RCS_CMD_CHANNEL("+format_function+", \""+modName+"_cmd\", \""+serverName+"\", \""+AppName+".nml\");");
				pos.println("\tif(NULL == "+modName+"_cmd)");
				pos.println("\t\treturn -1;");
				pos.println("\tif(!"+modName+"_cmd->valid())");
				pos.println("\t\treturn -1;");
			    }
			if(mod_stat_needed)
			    {
				pos.println("");// blank lineNumber
				format_function = modName+"Format";
				if(modInfo != null  && modInfo.statFormatFunction != null)
				    {
					format_function = modInfo.statFormatFunction;
				    }
				pos.println("\t"+modName+"_stat = new RCS_STAT_CHANNEL("+format_function+", \""+modName+"_sts\", \""+serverName+"\", \""+AppName+".nml\");");
				pos.println("\tif(NULL == "+modName+"_stat)");
				pos.println("\t\treturn -1;");
				pos.println("\tif(!"+modName+"_stat->valid())");
				pos.println("\t\treturn -1;");
				pos.println("");// blank lineNumber
			    }
		    }
		if(null != auxChannelsVector)
		    {
			for(int i = 0; i < auxChannelsVector.size(); i++)
			    {
				String auxName = (String) auxChannelsVector.elementAt(i);
				boolean aux_needed = false;
				for(int j = 0; j < si.bufferNames.size(); j++)
				    {
					String bufname = (String) si.bufferNames.elementAt(j);
					if(bufname.equals(auxName))
					    {
						aux_needed = true;
						break;
					    }
				    }
				if(!aux_needed)
				    {
					continue;
				    }
				pos.println("");// blank lineNumber
				pos.println("\t"+auxName+" = new NML("+auxName+"Format, \""+auxName+"\", \""+serverName+"\", \""+AppName+".nml\");");
				pos.println("\tif(NULL == "+auxName+")");
				pos.println("\t\treturn -1;");
				pos.println("\tif(!"+auxName+"->valid())");
				pos.println("\t\treturn -1;");
				pos.println("");// blank lineNumber
			    }
		    }

		if(errlog_needed)
		    {
			pos.println("\terrlog = new NML(nmlErrorFormat, \"errlog\", \""+serverName+"\", \""+AppName+".nml\");");
			pos.println("\tif(NULL == errlog)");
			pos.println("\t\treturn -1;");
			pos.println("\tif(!errlog->valid())");
			pos.println("\t\treturn -1;");
			pos.println("");// blank lineNumber
		    }
		pos.println("\treturn 0;");
		pos.println("}");
		pos.println("");// blank lineNumber
		pos.println("static void DeleteNML()");
		pos.println("{");
		for(int i = 0; i< gui.modulesList.getItemCount(); i++)
		    {
			String modName = gui.modulesList.getItem(i);
			boolean mod_needed = false;
			boolean mod_stat_needed = false;
			boolean mod_cmd_needed = false;
			for(int j = 0; j < si.bufferNames.size(); j++)
			    {
				String bufname = (String) si.bufferNames.elementAt(j);
				if(bufname.indexOf(modName) >= 0 || modName.indexOf(bufname) >= 0)
				    {
					mod_needed = true;
				    }
				else
				    {
					continue;
				    }
				if(bufname.equals(modName+"_cmd"))
				    {
					mod_cmd_needed = true;
				    }
				else if(bufname.equals(modName+"_sts"))
				    {
					mod_stat_needed = true;
				    }
				if(mod_stat_needed && mod_cmd_needed)
				    {
					break;
				    }
			    }
			if(!mod_needed)
			    {
				continue;
			    }
			pos.println("");// blank lineNumber
			if(mod_cmd_needed)
			    {
				pos.println("\t// "+modName);
				pos.println("\tif(NULL != "+modName+"_cmd)");
				pos.println("\t{");
				pos.println("\t\tdelete "+modName+"_cmd;");
				pos.println("\t\t"+modName+"_cmd = NULL;");
				pos.println("\t}");
			    }
			if(mod_stat_needed)
			    {
				pos.println("");// blank lineNumber
				pos.println("\tif(NULL != "+modName+"_stat)");
				pos.println("\t{");
				pos.println("\t\tdelete "+modName+"_stat;");
				pos.println("\t\t"+modName+"_stat = NULL;");
				pos.println("\t}");
				pos.println("");// blank lineNumber
			    }
		    }
		if(null != auxChannelsVector)
		    {
			for(int i = 0; i < auxChannelsVector.size(); i++)
			    {
				String auxName = (String) auxChannelsVector.elementAt(i);
				boolean aux_needed = false;
				for(int j = 0; j < si.bufferNames.size(); j++)
				    {
					String bufname = (String) si.bufferNames.elementAt(j);
					if(bufname.equals(auxName))
					    {
						aux_needed = true;
						break;
					    }
				    }
				if(!aux_needed)
				    {
					continue;
				    }
				pos.println("");// blank lineNumber
				pos.println("\tif(NULL != "+auxName+")");
				pos.println("\t{");
				pos.println("\t\tdelete "+auxName+";");
				pos.println("\t\t"+auxName+" = NULL;");
				pos.println("\t}");
				pos.println("");// blank lineNumber
			    }
		    }
		if(errlog_needed)
		    {
			pos.println("\tif(NULL != errlog)");
			pos.println("\t{");
			pos.println("\t\tdelete errlog;");
			pos.println("\t\terrlog = NULL;");
			pos.println("\t}");
		    }
		pos.println("");// blank lineNumber
		pos.println("}");
		pos.println("");// blank lineNumber
		pos.println("// Main ");
		if(!mswinDevPlat)
		    {
			pos.println("#ifdef VXWORKS");
			pos.println("extern \"C\" int "+serverName+"_run();");
			pos.println("");
			pos.println("int "+serverName+"_run()");
			pos.println("#else");
		    }
		pos.println("int main(int argc, char **argv)");
		if(!mswinDevPlat)
		    {
			pos.println("#endif");
		    }
		pos.println("{");
		pos.println("");// blank lineNumber
		pos.println("\tset_rcs_print_destination(RCS_PRINT_TO_STDOUT);");
		pos.println("");// blank lineNumber
		pos.println("\tif(InitNML() < 0)");
		pos.println("\t{");
		pos.println("\t\tDeleteNML();");
		pos.println("\t\treturn(-1);");
		pos.println("\t}");
		pos.println("");// blank lineNumber
		pos.println("\trun_nml_servers();");
		pos.println("");// blank lineNumber
		pos.println("}");
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void WriteMainMakefile(File dirFile)  throws rcsDesignUserAbortException
    {
	try
	    {
		File makeFile = new File(dirFile,"Makefile");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		makefile_mode = true;
		if(CanNotOverwriteExistingFile(makeFile,true, false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Makefile for "+dirFile);
		    }
		FileOutputStream fos = new FileOutputStream(makeFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);
		merger.WriteLine("# Makefile automatically generated by rcsdesign on");
		merger.WriteLine("# "+new Date()+" for "+dirFile);
		merger.WriteLine("# This makefile should define the sources, headers and objects");
		merger.WriteLine("# stored or created with this directory and then include");
		merger.WriteLine("# the application include Makefile.");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("SRCS =  \\");
		String cpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+cpp_ext));
		for(int i = 0; i < cpps.length; i++)
		    {
			if(cpps[i].indexOf(',') >= 0 || cpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			merger.WriteLine("\t"+cpps[i]+" \\");
		    }
		String cs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.c"));
		for(int i = 0; i < cs.length; i++)
		    {
			if(cs[i].indexOf(',') >= 0 || cs[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			merger.WriteLine("\t"+cs[i]+" \\");
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("HEADERS = \\");
		String hpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+hpp_ext));
		for(int i = 0; i < hpps.length; i++)
		    {
			if(hpps[i].indexOf(',') >= 0 || hpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			merger.WriteLine("\t"+hpps[i]+" \\");
		    }
		if(!hpp_ext.equals(".h"))
		    {
			String hs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.h"));
			for(int i = 0; i < hs.length; i++)
			    {
				if(hs[i].indexOf(',') >= 0 || hs[i].indexOf('~') >= 0)
				    {
					continue;
				    }
				merger.WriteLine("\t"+hs[i]+" \\");
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("OBJS = \\");
		String cppso[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+cpp_ext));
		for(int i = 0; i < cppso.length; i++)
		    {
			if(cppso[i].indexOf(',') >= 0 || cppso[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			merger.WriteLine("\t"+cppso[i].substring(0,cppso[i].length()-cpp_ext.length())+obj_ext+" \\");
		    }
		String cso[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.c"));
		for(int i = 0; i < cso.length; i++)
		    {
			if(cso[i].indexOf(',') >= 0 || cso[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			merger.WriteLine("\t"+cso[i].substring(0,cso[i].length()-2)+obj_ext+" \\");
		    }
		merger.WriteLine("");// blank lineNumber
		if(mswinDevPlat)
		    {
			merger.WriteLine("LIBS = "+AppName+".lib");
		    }
		else
		    {
			merger.WriteLine("ifeq (vxworks,$(findstring vxworks,$(PLAT)))");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("LIBS =");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("else");
			merger.WriteLine("LIBS = lib"+AppName+".a");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("endif");
		    }
		merger.WriteLine("");// blank lineNumber
		if(!mswinDevPlat)
		    {
			merger.WriteLine("ifeq (vxworks,$(findstring vxworks,$(PLAT)))");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("BINS =");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("else");
			merger.WriteLine("");// blank lineNumber
		    }
		merger.WriteLine("BINS = \\");
		Enumeration svrkey = serversHashtable.keys();
		while(svrkey.hasMoreElements())
		    {
			String svrname = (String) svrkey.nextElement();
			merger.WriteLine("\t"+svrname+" \\");
		    }
		if(gui.modulesList.getItemCount() > 0)
		    {
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name =       gui.mainLoopList.getItem(i);
				merger.WriteLine("\t"+loop_name+"main \\");
			    }
		    }
		if(!mswinDevPlat)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("endif");
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("# Include Application Specific Definitions and the RCS generic Makefile");
		if(mswinDevPlat)
		    {
			merger.WriteLine("!INCLUDE <..\\..\\Makefile.inc>");
		    }
		else
		    {
			merger.WriteLine("include ../../Makefile.inc");
		    }
		merger.WriteLine("");// blank lineNumber
		if(mswinDevPlat)
		    {
			merger.WriteLine("DEVP_OBJS = \\");
			for(int i = 0; i < cppso.length; i++)
			    {
				if(cppso[i].indexOf(',') >= 0 || cppso[i].indexOf('~') >= 0)
				    {
					continue;
				    }
				merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+cppso[i].substring(0,cppso[i].length()-cpp_ext.length())+obj_ext+" \\");
			    }
			for(int i = 0; i < cso.length; i++)
			    {
				if(cso[i].indexOf(',') >= 0 || cso[i].indexOf('~') >= 0)
				    {
					continue;
				    }
				merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+cso[i].substring(0,cso[i].length()-2)+obj_ext+" \\");
			    }
			merger.WriteLine("");// blank lineNumber
			String loopnames = "";
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loopname = gui.mainLoopList.getItem(i);
				loopnames += "$(DEVP_BIN_DIR)\\"+loopname+"main.exe ";
			    }
			merger.WriteLine("all: $(DEVP_OBJS) $(DEVP_LIB_DIR)\\"+AppName+".lib $(DEVP_BIN_DIR)\\"+AppName+"svr.exe "+loopnames);
		    }
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("# Rules for specific libraries");
		merger.WriteLine("");// blank lineNumber

		HashSet lib_hs = new HashSet();
		if(mswinDevPlat)
		    {
			merger.WriteLine("# "+AppName+".lib");
			merger.WriteLine("$(DEVP_LIB_DIR)\\"+AppName+".lib: \\");
		    }
		else
		    {
			merger.WriteLine("# "+AppName+".a");
			merger.WriteLine("$(DEVP_LIB_DIR)/lib"+AppName+".a: \\");
		    }
		if(null != auxChannelsVector)
		    {
			for(int j = 0; j < auxChannelsVector.size(); j++)
			    {
				String aux = (String) auxChannelsVector.elementAt(j);
				String obj = aux+"n"+obj_ext;
				if(lib_hs.contains(obj))
				    {
					continue;
				    }
				else
				    {					
					lib_hs.add(obj);
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+obj+" \\");
				    }
				else
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)/"+obj+" \\");
				    }
			    }
		    }
		if(gui.modulesList.getItemCount() > 0)
		    {
			for(int i = 0; i < gui.modulesList.getItemCount(); i++)
			    {
				String modName =         gui.modulesList.getItem(i);
				String obj = modName+obj_ext;
				if(lib_hs.contains(obj))
				    {
					continue;
				    }
				else
				    {
					lib_hs.add(obj);
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+obj+" \\");
				    }
				else
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)/"+obj+" \\");
				    }
		
			    }
			for(int i = 0; i < gui.modulesList.getItemCount(); i++)
			    {
				String modName =         gui.modulesList.getItem(i);
				ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
				String cmd_obj = modName+"n"+obj_ext;
				if(null != modInfo && null != modInfo.cmdsTypeFile)
				    {
					String cmd_header = modInfo.cmdsTypeFile;
					int slash_index = cmd_header.lastIndexOf(File.separator);
					if(slash_index >= 0)
					    {
						cmd_header = cmd_header.substring(slash_index+1);
					    }
					String cmd_base = cmd_header;
					int dot_index = cmd_base.lastIndexOf('.');
					if(dot_index > 0 )
					    {
						cmd_base = cmd_base.substring(0,dot_index);
					    }
					//System.out.println("modName="+modName+",cmd_base="+cmd_base);
					if(!cmd_base.equals(modName+"n"))
					    {
						cmd_obj = cmd_base+obj_ext;
					    }
				    }
				String obj = cmd_obj;
				if(lib_hs.contains(obj))
				    {
					cmd_obj=null;
				    }
				else
				    {
					lib_hs.add(obj);
				    }
				if(cmd_obj != null)
				    {
					if(mswinDevPlat)
					    {
						merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+obj+" \\");
					    }
					else
					    {
						merger.WriteLine("\t$(DEVP_LIB_DIR)/"+obj+" \\");
					    }
				    }
				String stat_obj = modName+"n"+obj_ext;
				if(null != modInfo && null != modInfo.statsTypeFile)
				    {
					String stat_header = modInfo.statsTypeFile;
					int slash_index = stat_header.lastIndexOf(File.separator);
					if(slash_index >= 0)
					    {
						stat_header = stat_header.substring(slash_index+1);
					    }
					String stat_base = stat_header;
					int dot_index = stat_base.lastIndexOf('.');
					if(dot_index > 0 )
					    {
						stat_base = stat_base.substring(0,dot_index);
					    }
					if(!stat_base.equals(modName+"n"))
					    {
						stat_obj = stat_base+obj_ext;
					    }
				    }
				obj = stat_obj;
				if(lib_hs.contains(obj))
				    {
					stat_obj=null;
				    }
				else
				    {
					lib_hs.add(obj);
				    }
				if(stat_obj != null)
				    {
					if(mswinDevPlat)
					    {
						merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+obj+" \\");
					    }
					else
					    {
						merger.WriteLine("\t$(DEVP_LIB_DIR)/"+obj+" \\");
					    }
				    }

			    }
			try 
			    {
				File up1dirFile = dirFile.getParentFile();
				File intfDirFile= new File(up1dirFile,"intf");
				for(int i =0; i < ModuleInfo.headerFiles.size(); i++)
				    {
					String header = (String) ModuleInfo.headerFiles.elementAt(i);
					File hFile = new File(intfDirFile,header);
					if(hFile.exists())
					    {
						String hbase = header;
						int dindex = hbase.lastIndexOf('.');
						if(dindex > 0)
						    {
							hbase = hbase.substring(0,dindex);
						    }
						String obj = hbase + obj_ext;
						if(lib_hs.contains(obj))
						    {
							continue;
						    }
						else
						    {
							lib_hs.add(obj);
						    }
						if(mswinDevPlat)
						    {
							merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+obj+" \\");
						    }
						else
						    {
							merger.WriteLine("\t$(DEVP_LIB_DIR)/"+obj+" \\");
						    }
					    }
				    }
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
			merger.WriteLine("\t");
		    }
		if(!mswinDevPlat)
		    {
			merger.WriteLine("\t$(COMPILER_SETUP); \\");
		    }
		if(mswinDevPlat)
		    {
			merger.WriteLine("\t$(AR) /OUT:$@ $**");
		    }
		else
		    {
			merger.WriteLine("\t$(AR) cr $@ $^");
			merger.WriteLine("\t$(RANLIB) $@");
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("# Rules for specific binaries");
		merger.WriteLine("");// blank lineNumber
		svrkey = serversHashtable.keys();
		while(svrkey.hasMoreElements())
		    {
			String svrname = (String) svrkey.nextElement();
			if(gui.modulesList.getItemCount() > 0)
			    {
				if(mswinDevPlat)
				    {
					merger.WriteLine("# "+svrname+".exe");
					merger.WriteLine("$(DEVP_BIN_DIR)\\"+svrname+".exe: $(DEVP_LIB_DIR)\\"+svrname+obj_ext+" \\");
				    }
				else
				    {
					merger.WriteLine("# "+svrname);
					merger.WriteLine("$(DEVP_BIN_DIR)/"+svrname+": $(DEVP_LIB_DIR)/"+svrname+obj_ext+" \\");
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+AppName+".lib");
				    }
				else
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)/lib"+AppName+".a");
				    }
				if(!mswinDevPlat)
				    {
					merger.WriteLine("\t$(COMPILER_SETUP); \\");
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(CPLUSPLUS) /Fe$@ $(CPLUSPLUSLINK) $(CPLUSPLUSFLAGS) $** \\");
					merger.WriteLine("\t$(EXTRA_LIBS) \\");
					merger.WriteLine("\t$(RCS_LIBRARY)");
				    }
				else
				    {
					merger.WriteLine("\t$(CPLUSPLUS) $(CPLUSPLUSFLAGS) $^ \\");
					merger.WriteLine("\t$(RCS_LIBRARY) \\");
					merger.WriteLine("\t$(CPLUSPLUSLINK) $(EXTRA_LIBS) \\");
					merger.WriteLine("\t-o $@");
				    }
				merger.WriteLine("");// blank lineNumber
				if(mswinDevPlat)
				    {
					merger.WriteLine(svrname+": $(DEVP_BIN_DIR)\\"+svrname+".exe");
				    }
				else
				    {
					merger.WriteLine(svrname+": $(DEVP_BIN_DIR)/"+svrname);
				    }
				merger.WriteLine("");// blank lineNumber
				merger.WriteLine(".PHONY: "+svrname);
				merger.WriteLine("");// blank lineNumber
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		if(gui.modulesList.getItemCount() > 0)
		    {
			for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
			    {
				String loop_name =       gui.mainLoopList.getItem(i);
				rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) mainloopsHashtable.get(loop_name);
				if(null == loopInfo)
				    {
					continue;
				    }
				if(null == loopInfo.getModules())
				    {
					continue;
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("# "+loop_name+"main.exe");
					merger.WriteLine("$(DEVP_BIN_DIR)\\"+loop_name+"main.exe: $(DEVP_LIB_DIR)\\"+loop_name+"main"+obj_ext+" \\");
				    }
				else
				    {
					merger.WriteLine("# "+loop_name+"main");
					merger.WriteLine("$(DEVP_BIN_DIR)/"+loop_name+"main: $(DEVP_LIB_DIR)/"+loop_name+"main"+obj_ext+" \\");
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)\\"+AppName+".lib");
				    }
				else
				    {
					merger.WriteLine("\t$(DEVP_LIB_DIR)/lib"+AppName+".a");
				    }
				if(!mswinDevPlat)
				    {
					merger.WriteLine("\t$(COMPILER_SETUP); \\");
				    }
				if(mswinDevPlat)
				    {
					merger.WriteLine("\t$(CPLUSPLUS) /Fe$@ $(CPLUSPLUSLINK) $(CPLUSPLUSFLAGS)  $** \\");
					merger.WriteLine("\t$(EXTRA_LIBS) \\");
					merger.WriteLine("\t$(RCS_LIBRARY)");
				    }
				else
				    {
					merger.WriteLine("\t$(CPLUSPLUS)  $(CPLUSPLUSFLAGS) $^ \\");
					merger.WriteLine("\t$(RCS_LIBRARY) \\");
					merger.WriteLine("\t$(CPLUSPLUSLINK) $(EXTRA_LIBS) \\");
					merger.WriteLine("\t-o $@");
				    }
				merger.WriteLine("");// blank lineNumber
				if(mswinDevPlat)
				    {
					merger.WriteLine(loop_name+"main: $(DEVP_BIN_DIR)\\"+loop_name+"main.exe");
				    }
				else
				    {
					merger.WriteLine(loop_name+"main: $(DEVP_BIN_DIR)/"+loop_name+"main");
				    }
				merger.WriteLine("");// blank lineNumber
				merger.WriteLine(".PHONY: "+loop_name+"main");
				merger.WriteLine("");// blank lineNumber
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.Finish();
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteSingleDirGnuMakefile(File dirFile, String file_name)  throws rcsDesignUserAbortException
    {
	try
	    {
		File makeFile = new File(dirFile,file_name);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		makefile_mode = true;
		if(CanNotOverwriteExistingFile(makeFile,true, false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Makefile for "+dirFile);
		    }
		FileOutputStream fos = new FileOutputStream(makeFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);
		merger.WriteLine("# Makefile automatically generated by rcsdesign on");
		merger.WriteLine("# "+new Date()+" for "+dirFile);
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("# Set the main RCS Library Directory");
		merger.WriteLine("ifndef RCSLIB_DIR");
		merger.WriteLine("RCSLIB_DIR="+RcsLibDir);
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("CPPFLAGS:=$(CPPFLAGS) -I$(RCSLIB_DIR)/include -I$(RCSLIB_DIR)/plat/$(PLAT)/include -Isrc/intf -Isrc/utils");

		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("LIBS:=$(LIBS) -L$(RCSLIB_DIR)/lib -L$(RCSLIB_DIR)/plat/$(PLAT)/lib -lrcs ");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("ifndef JAVA");
		merger.WriteLine("JAVA=java");
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("ifndef RANLIB");
		merger.WriteLine("RANLIB=ranlib");
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("ifndef AR");
		merger.WriteLine("AR=ar");
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("ifndef CXX");
		merger.WriteLine("CXX=g++");
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("ifndef NML_CODEGEN");
		merger.WriteLine("NML_CODEGEN=$(JAVA) -jar $(RCSLIB_DIR)/plat/java/lib/CodeGenCmdLine.jar ");
		merger.WriteLine("endif");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("APPNAME="+AppName);
		String svrsLine="SVRS = ";
		Enumeration svrkey = serversHashtable.keys();
		while(svrkey.hasMoreElements())
		    {
			String svrname = (String) svrkey.nextElement();
			if(svrname.endsWith("svr"))
			    {
				svrname = svrname.substring(0,svrname.length()-3);
			    }
			svrsLine += " "+svrname+" ";
		    }
		merger.WriteLine(svrsLine);
		String loopsLine="LOOPS = ";
		for(int i = 0; i < gui.mainLoopList.getItemCount(); i++)
		    {
			String loop_name = gui.mainLoopList.getItem(i);
			if(loop_name.endsWith("main"))
			    {
				loop_name = loop_name.substring(0,loop_name.length()-4);
			    }
			loopsLine += " "+loop_name+" ";
		    }
		merger.WriteLine(loopsLine);

		String modulesLine="MODULES = ";
		String nmlsLine="NMLS = ";
		for(int k = 0; k < gui.modulesList.getItemCount(); k++)
		    {
			String module_name = gui.modulesList.getItem(k);
			modulesLine += " " +module_name+" ";
			nmlsLine += " " +module_name+" ";
			ModuleInfo mi = (ModuleInfo) modulesHashtable.get(module_name);
			if(null != mi)
			    {
				if(null != mi.AuxInputNames)
				    {
					for(int l = 0; l < mi.AuxInputNames.size(); l++)
					    {
						String aux_name =(String) mi.AuxInputNames.elementAt(l);
						String aux_name_plus_space = " "+aux_name+" ";
						if(nmlsLine.indexOf(aux_name_plus_space) < 0)
						    {
							nmlsLine += aux_name_plus_space;
						    }
					    }
				    }
				if(null != mi.AuxOutputNames)
				    {
					for(int m = 0; m < mi.AuxOutputNames.size(); m++)
					    {
						String aux_name = (String) mi.AuxOutputNames.elementAt(m);
						String aux_name_plus_space = " "+aux_name+" ";
						if(nmlsLine.indexOf(aux_name_plus_space) < 0)
						    {
							nmlsLine += aux_name_plus_space;
						    }
					    }
				    }
			    }
		    }
		merger.WriteLine(modulesLine);
		merger.WriteLine(nmlsLine);

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("SERVERS_LIST=$(SVRS:%=%svr)");
		merger.WriteLine("MAINS_LIST=$(LOOPS:%=%main)");
		merger.WriteLine("MODULES_O_LIST=$(MODULES:%=%_module.o)");
		merger.WriteLine("NML_O_LIST=$(NMLS:%=%n_n.o)");
		merger.WriteLine("NML_A=lib$(APPNAME)_nml.a");
		merger.WriteLine("MODULE_A=lib$(APPNAME)_module.a");

		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("all: $(SERVERS_LIST) $(MAINS_LIST)");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine(".PHONY: all");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("%n_n.cc: %n.hh");
		merger.WriteLine("\t$(NML_CODEGEN) $^");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("$(NML_A): $(NML_O_LIST)");
		merger.WriteLine("\t$(AR) cr $@ $^ ");
		merger.WriteLine("\t$(RANLIB) $@");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("$(MODULE_A): $(MODULES_O_LIST)");
		merger.WriteLine("\t$(AR) cr $@ $^ ");
		merger.WriteLine("\t$(RANLIB) $@");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("%svr: %svr.o $(NML_A)");
		merger.WriteLine("\t$(CXX) $^ $(LIBS) $(CXXFLAGS) -o $@");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("%main: %main.o $(MODULE_A) $(NML_A)");
		merger.WriteLine("\t$(CXX) $^ $(LIBS) $(CXXFLAGS) -o $@");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("clean:");
		merger.WriteLine("\t-\\rm -f *n_n.cc *.o lib*.a lib*.so lib*.la *.lib *.ddll *.obj *.exe *main *svr");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine(".PHONY: clean");

		merger.WriteLine("");// blank lineNumber
		merger.Finish();
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void WriteIntfMakefile(File dirFile)  throws rcsDesignUserAbortException
    {
	FileOutputStream fos = null;
	PrintWriter pos = null;
	try
	    {
		File makeFile = new File(dirFile,"Makefile");
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(makeFile, false, false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Makefile for "+dirFile);
		    }
		fos = new FileOutputStream(makeFile);
		pos = new PrintWriter(fos);
		pos.println("# Makefile automatically generated by rcsdesign on");
		pos.println("# "+new Date()+" for "+dirFile);
		pos.println("# This makefile should define the sources, headers and objects");
		pos.println("# stored or created with this directory and then include");
		pos.println("# the application include Makefile.");
		pos.println("");// blank lineNumber
		pos.println("SRCS =  \\");
		/*
		  String hns[] = dirFile.list(new rcs.utils.SimpleFileFilter("*n"+hpp_ext));
		  for(int i = 0; i < hns.length; i++)
		  {
		  if(hns[i].indexOf(',') >= 0 || hns[i].indexOf('~') >= 0)
		  {
		  continue;
		  }
		  pos.println("\t"+hns[i].substring(0,hns[i].length()-hpp_ext.length())+cpp_ext+" \\");
		  }
		*/
		String cpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+cpp_ext));
		cpploop:  for(int i = 0; i < cpps.length; i++)
		    {
			if(cpps[i].indexOf(',') >= 0 || cpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			/*
			  for(int j = 0; j < hns.length; j++)
			  {
			  if(cpps[i].equals(hns[j].substring(0,hns[j].length()-hpp_ext.length())+cpp_ext))
			  {
			  continue cpploop;
			  }
			  }
			*/
			pos.println("\t"+cpps[i]+" \\");
		    }
		String cs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.c"));
		for(int i = 0; i < cs.length; i++)
		    {
			if(cs[i].indexOf(',') >= 0 || cs[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+cs[i]+" \\");
		    }
		forced_cpps = null;
		if(null != gui.modulesList)
		    {
			forced_cpps = "";
			for(int k = 0; k < gui.modulesList.getItemCount(); k++)
			    {
				String module_name = gui.modulesList.getItem(k);
				ModuleInfo mi = (ModuleInfo) modulesHashtable.get(module_name);
				if(null == mi || null == mi.cmdsTypeFile || null == mi.statsTypeFile)
				    {
					forced_cpps += " "+module_name+"n"+cpp_ext+", ";
					continue;
				    }
				String fname = mi.cmdsTypeFile;
				int sep_index = fname.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fname = fname.substring(sep_index+1);
				    }
				int dot_index = fname.lastIndexOf('.');
				if(dot_index > 0)
				    {
					fname = fname.substring(0,dot_index)+cpp_ext;
				    }
				forced_cpps += " "+fname+", ";
				fname = mi.statsTypeFile;
				sep_index = fname.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fname = fname.substring(sep_index+1);
				    }
				dot_index = fname.lastIndexOf('.');
				if(dot_index > 0)
				    {
					fname = fname.substring(0,dot_index)+cpp_ext;
				    }
				forced_cpps += " "+fname+", ";
			    }
		    }
		for(int i =0; i < ModuleInfo.headerFiles.size(); i++)
		    {
			String header = (String) ModuleInfo.headerFiles.elementAt(i);
			File hFile = new File(dirFile,header);
			if(hFile.exists())
			    {
				String hbase = header;
				int dindex = hbase.lastIndexOf('.');
				if(dindex > 0)
				    {
					hbase = hbase.substring(0,dindex);
				    }
				forced_cpps += " " +hbase+cpp_ext+", ";
			    }
		    }
		if(forced_cpps != null)
		    {
			StringTokenizer forced_cpps_tokenizer = new StringTokenizer(forced_cpps, " \t\r\n\b,;:");
			while(forced_cpps_tokenizer.hasMoreTokens())
			    {
				String forced_cpp = forced_cpps_tokenizer.nextToken();
				boolean forced_cpp_found = false;
				for(int i = 0; i < cpps.length; i++)
				    {
					if(forced_cpp.equals(cpps[i]))
					    {
						forced_cpp_found = true;
						break;
					    }
				    }
				if(forced_cpp_found)
				    {
					continue;
				    }
				for(int i = 0; i < cs.length; i++)
				    {
					if(forced_cpp.equals(cs[i]))
					    {
						forced_cpp_found = true;
						break;
					    }
				    }
				if(forced_cpp_found)
				    {
					continue;
				    }
				/*
				  for(int i = 0; i < hns.length; i++)
				  {
				  String hns_cpp_file = hns[i].substring(0,hns[i].length() - hpp_ext.length()) + cpp_ext;
				  if(forced_cpp.equals(hns_cpp_file))
				  {
				  forced_cpp_found = true;
				  break;
				  }
				  }
				*/
				if(forced_cpp_found)
				    {
					continue;
				    }
				pos.println("\t"+forced_cpp+" \\");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.println("HEADERS = \\");
		String hpps[] = dirFile.list(new rcs.utils.SimpleFileFilter("*"+hpp_ext));
		for(int i = 0; i < hpps.length; i++)
		    {
			if(hpps[i].indexOf(',') >= 0 || hpps[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+hpps[i]+" \\");
		    }
		String hs[] = dirFile.list(new rcs.utils.SimpleFileFilter("*.h"));
		for(int i = 0; i < hs.length; i++)
		    {
			if(hs[i].indexOf(',') >= 0 || hs[i].indexOf('~') >= 0)
			    {
				continue;
			    }
			pos.println("\t"+hs[i]+" \\");
		    }

		forced_hpps = null;
		if(null != gui.modulesList)
		    {
			forced_hpps = "";
			for(int k = 0; k < gui.modulesList.getItemCount(); k++)
			    {
				String module_name = gui.modulesList.getItem(k);
				ModuleInfo mi = (ModuleInfo) modulesHashtable.get(module_name);
				if(null == mi || null == mi.cmdsTypeFile || null == mi.statsTypeFile)
				    {
					forced_hpps += " "+module_name+"n"+hpp_ext+", ";
					continue;
				    }
				String fname = mi.cmdsTypeFile;
				int sep_index = fname.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fname = fname.substring(sep_index+1);
				    }
				forced_hpps += " "+fname+", ";
				fname = mi.statsTypeFile;
				sep_index = fname.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fname = fname.substring(sep_index+1);
				    }
				forced_hpps += " "+fname+", ";
			    }
		    }
		if(forced_hpps != null)
		    {
			StringTokenizer forced_hpps_tokenizer = new StringTokenizer(forced_hpps, " \t\r\n\b,;:");
			while(forced_hpps_tokenizer.hasMoreTokens())
			    {
				String forced_hpp = forced_hpps_tokenizer.nextToken();
				boolean forced_hpp_found = false;
				for(int i = 0; i < hpps.length; i++)
				    {
					if(forced_hpp.equals(hpps[i]))
					    {
						forced_hpp_found = true;
						break;
					    }
				    }
				if(forced_hpp_found)
				    {
					continue;
				    }
				for(int i = 0; i < hs.length; i++)
				    {
					if(forced_hpp.equals(hs[i]))
					    {
						forced_hpp_found = true;
						break;
					    }
				    }
				if(forced_hpp_found)
				    {
					continue;
				    }
				pos.println("\t"+forced_hpp+" \\");
			    }
		    }
		forced_hpps = null;

		pos.println("");// blank lineNumber
		pos.println("OBJS = \\");
		HashSet obj_hs = new HashSet();
		StringTokenizer obj_tokenizer = new StringTokenizer(forced_cpps," ,;");
      
		while(obj_tokenizer.hasMoreTokens())
		    {
			String obj_base = obj_tokenizer.nextToken();
			int pindex = obj_base.lastIndexOf('.');
			if(pindex > 0)
			    {
				obj_base=obj_base.substring(0,pindex);
			    }
			if(obj_hs.contains(obj_base))
			    {
				continue;
			    }
			else
			    {
				obj_hs.add(obj_base);
			    }
			pos.println("\t"+obj_base+obj_ext+" \\");
		    }

		pos.println("");// blank lineNumber
		pos.println("LIBS = ");
		pos.println("");// blank lineNumber
		pos.println("BINS = ");
		pos.println("");// blank lineNumber
		pos.println("# Include Application Specific Definitions and the RCS generic Makefile");
		if(mswinDevPlat)
		    {
			pos.println("!INCLUDE <..\\..\\Makefile.inc>");
		    }
		else
		    {
			pos.println("include ../../Makefile.inc");
		    }

		if(mswinDevPlat)
		    {
			pos.println("");// blank lineNumber
			pos.println("DEVP_OBJS = \\");
			obj_hs = new HashSet();
			obj_tokenizer = new 
			    StringTokenizer(forced_cpps," ,;");
			while(obj_tokenizer.hasMoreTokens())
			    {
				String obj_base = obj_tokenizer.nextToken();
				int pindex = obj_base.lastIndexOf('.');
				if(pindex > 0)
				    {
					obj_base=obj_base.substring(0,pindex);
				    }
				if(obj_hs.contains(obj_base))
				    {
					continue;
				    }
				else
				    {
					obj_hs.add(obj_base);
				    }
				pos.println("\t $(DEVP_LIB_DIR)\\"+obj_base+obj_ext+" \\");
			    }
			pos.println("");// blank lineNumber
			pos.println("all:  $(DEVP_OBJS)");
			pos.println("");// blank lineNumber
		    }
		pos.println("");// blank lineNumber
		pos.println("# Rules for specific targets");
		pos.println("");// blank lineNumber


		HashSet cpp_hs=new HashSet();
		if(debug_on)
		    {
			System.out.println("forced_cpps="+forced_cpps);
		    }
		if(forced_cpps != null)
		    {
			StringTokenizer forced_cpps_tokenizer = new StringTokenizer(forced_cpps, " \t\r\n\b,;:");
			while(forced_cpps_tokenizer.hasMoreTokens())
			    {
				String forced_cpp = forced_cpps_tokenizer.nextToken();
				if(cpp_hs.contains(forced_cpp))
				    {
					continue;
				    }
				else
				    {
					cpp_hs.add(forced_cpp);
				    }
				String forced_hpp = forced_cpp.substring(0, forced_cpp.length() - cpp_ext.length())+hpp_ext;
				String forced_gen = forced_cpp.substring(0, forced_cpp.length() - cpp_ext.length())+".gen";
				pos.println(forced_cpp+" : "+forced_hpp);
				if(mswinDevPlat)
				    {
					pos.println("\t$(NML_CODEGEN) script="+forced_gen+" display_on=false");
				    }
				else
				    {
					pos.println("\t$(NML_CODEGEN) script="+forced_gen+" display_on=false");
				    }
				pos.println("");
			    }
		    }
		forced_cpps = null;
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		try
		    {

			if(null != pos)
			    {
				pos.close();
				pos = null;
			    }
			if(null != fos)
			    {
				fos.close();
				fos = null;
			    }
			e.printStackTrace();
		    }
		catch(Exception e2)
		    {
			e2.printStackTrace();
		    }
	    }
    }


    static final String months[] = { "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};

    void WriteModuleCodeGenScript(File dirFile, String modName, String filename)  throws rcsDesignUserAbortException
    {
	try
	    {
		if(null == modulesHashtable)
		    {
			return;
		    }
		ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
		if(null == modInfo)
		    {
			return;
		    }
		File moduleHeaderFile = new File(dirFile,filename);
		String filenamebase = filename;
		int dot_index = filenamebase.indexOf('.');
		if(dot_index > 0)
		    {
			filenamebase = filenamebase.substring(0,dot_index);
		    }
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(moduleHeaderFile, true,true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating CodeGen Script file for "+modName);
		    }
		FileOutputStream fos = new FileOutputStream(moduleHeaderFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# CodeGen Script file to create "+filenamebase+cpp_ext+" from "+filenamebase+hpp_ext);
		if(null != modInfo.predefined_type_files)
		    {
			if(modInfo.predefined_type_files.size() > 0)
			    {
				int loaded_files = 0;
				pos.println("# Pre-defined Types");
				for(int i = 0; i < modInfo.predefined_type_files.size(); i++)
				    {
					String type_file = (String) modInfo.predefined_type_files.elementAt(i);
					if(type_file.equals(modInfo.baseClassCmdsTypeFile))
					    {
						continue;
					    }
					if(type_file.equals(modInfo.baseClassStatsTypeFile))
					    {
						continue;
					    }
					if(type_file.length() < 1)
					    {
						continue;
					    }
					if(type_file.endsWith("null"))
					    {
						continue;
					    }
					pos.println("load "+type_file);
					loaded_files++;
				    }
				if(loaded_files > 0)
				    {
					pos.println("clear");
				    }
			    }
		    }
		if(null != modInfo.baseClassCmdsTypeFile)
		    {
			if(!modInfo.baseClassCmdsTypeFile.endsWith("null") && modInfo.baseClassCmdsTypeFile.length() > 0)
			    {
				pos.println("load "+modInfo.baseClassCmdsTypeFile);
			    }
		    }
		if(null != modInfo.baseClassStatsTypeFile)
		    {
			if(!modInfo.baseClassStatsTypeFile.equals(modInfo.baseClassCmdsTypeFile))
			    {
				if(!modInfo.baseClassStatsTypeFile.endsWith("null") && modInfo.baseClassStatsTypeFile.length() > 0)
				    {
					pos.println("load "+modInfo.baseClassStatsTypeFile);
				    }
			    }
		    }
		pos.println("load "+filenamebase+hpp_ext);
		pos.println("clear");
		if(null != modInfo.baseClassCmdsTypeFile)
		    {
			pos.println("select_from_file "+modInfo.baseClassCmdsTypeFile);
		    }
		if(null != modInfo.baseClassStatsTypeFile)
		    {
			if(!modInfo.baseClassStatsTypeFile.equals(modInfo.baseClassCmdsTypeFile))
			    {
				pos.println("select_from_file "+modInfo.baseClassStatsTypeFile);
			    }
		    }
		pos.println("select_from_file "+filenamebase+hpp_ext);
		if(modInfo.cmdFormatFunction != null && 
		   modInfo.cmdsTypeFile.indexOf(filenamebase) >= 0 &&
		   modInfo.statsTypeFile.indexOf(filenamebase) < 0)
		    {
			pos.println("set_format_function "+modInfo.cmdFormatFunction+"");
		    }
		else if(modInfo.statFormatFunction != null && 
			modInfo.statsTypeFile.indexOf(filenamebase) >= 0 &&
			modInfo.cmdsTypeFile.indexOf(filenamebase) < 0)
		    {
			pos.println("set_format_function "+modInfo.statFormatFunction+"");
		    }
		else if(!filenamebase.equals(modInfo.Name+"n"))
		    {
			pos.println("set_format_function "+filenamebase+"Format");
		    }
		else
		    {
			pos.println("set_format_function "+modInfo.Name+"Format");
		    }		
		pos.println("generate C++ format >"+filenamebase+cpp_ext);
		pos.println("clear");
		pos.println("select_from_file "+filenamebase+hpp_ext);
		pos.println("generate C++ update >"+filenamebase+cpp_ext);
		pos.println("generate C++ constructor >"+filenamebase+cpp_ext);
		pos.println("exit");
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteAuxCodeGenScript(File dirFile, String aux, String filename)  throws rcsDesignUserAbortException
    {
	try
	    {
		if(null == modulesHashtable)
		    {
			return;
		    }

		File auxCodeGenFile = new File(dirFile,filename);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(auxCodeGenFile, true,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating CodeGen Script file for "+aux);
		    }
		FileOutputStream fos = new FileOutputStream(auxCodeGenFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# CodeGen Script file to create "+aux+"n"+cpp_ext+" from "+aux+"n"+hpp_ext);
		pos.println("load "+aux+"n"+hpp_ext);
		pos.println("generate C++ format >"+aux+"n"+cpp_ext);
		pos.println("generate C++ update >"+aux+"n"+cpp_ext);
		pos.println("generate C++ constructor >"+aux+"n"+cpp_ext);
		pos.println("exit");
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void WriteMiscCodeGenScript(File dirFile, String filename)  throws rcsDesignUserAbortException
    {
	try
	    {
		String filename_base = filename;
		int dot_index = filename.lastIndexOf('.');
		if(dot_index > 0)
		    {
			filename_base = filename_base.substring(0,dot_index);
		    }
		String cpp_file = filename_base+cpp_ext;
		String hpp_file = filename_base+hpp_ext;
		File miscCodeGenFile = new File(dirFile,filename);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(miscCodeGenFile, true,false))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating CodeGen Script file for "+cpp_file);
		    }
		FileOutputStream fos = new FileOutputStream(miscCodeGenFile);
		PrintWriter pos = new PrintWriter(fos);
		pos.println("# CodeGen Script file to create "+cpp_file+" from "+hpp_file);
		pos.println("load "+hpp_file);
		pos.println("clear");
		pos.println("select_from_file "+hpp_file);
		pos.println("generate C++ update >"+cpp_file);
		pos.println("generate C++ constructor >"+cpp_file);
		pos.println("exit");
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    String deletedCommands = "";

    void RemoveDeletedCommandsFromNMLModuleHeader(File moduleHeaderFile, ModuleInfo modInfo)
    {
	try
	    {
		deletedCommands = "";
		if(debug_on)
		    {
			System.out.println("RemoveDeletedCommandsFromNMLModuleHeader ("+moduleHeaderFile+", "+modInfo+")");
		    }
		if(moduleHeaderFile.exists())
		    {
			if(!moduleHeaderFile.canWrite())
			    {
				gui.Alert("Can not write over "+moduleHeaderFile);
				return;
			    }
			if(!moduleHeaderFile.canRead())
			    {
				gui.Alert("Can not read  "+moduleHeaderFile);
				return;
			    }
		    }
		else
		    {
			return;
		    }
		if(null == modInfo.deleted_commands)
		    {
			return;
		    }
		StringBuffer nondeleted_lines = new StringBuffer((int) moduleHeaderFile.length());
		FileInputStream fis = new FileInputStream(moduleHeaderFile);
		InputStreamReader isr = new InputStreamReader(fis);
		BufferedReader br = new BufferedReader(isr);
		boolean inside_deleted_command = false;
		boolean inside_class = false;
		int brace_count = 0;
		boolean delCmdsAdded = false;
		int line_count = 0;
		while(true)
		    {
			String line = br.readLine();
			line_count++;
			if(line == null)
			    {
				break;
			    }
			if(brace_count == 0)
			    {
				if(!inside_deleted_command)
				    {
					StringTokenizer tokenizer = new StringTokenizer(line,"\t \r\n");
					while(tokenizer.hasMoreTokens())
					    {
						String token = tokenizer.nextToken();
						if(debug_on)
						    {
							System.out.println("token = "+token);
						    }
						if(null == token)
						    {
							break;
						    }
						if(token.equals("//"))
						    {
							break;
						    }
						if(token.equals("struct") || token.equals("class") )
						    {
							inside_class = true;
							if(debug_on)
							    {
								System.out.println("inside_class");
							    }
							continue;
						    }
						if(inside_class)
						    {
							for(int i = 0; i < modInfo.deleted_commands.size(); i++)
							    {
								String delCmd = (String) modInfo.deleted_commands.elementAt(i);
								if(!delCmdsAdded)
								    {
									deletedCommands += delCmd+", ";
								    }
								if(debug_on)
								    {
									System.out.println("delCmd = "+delCmd);
								    }
								if(token.equals(delCmd))
								    {
									inside_deleted_command = true;
									if(debug_on)
									    {
										System.out.println("inside_deleted_command");
									    }
									break;
								    }
							    }
							delCmdsAdded = true;
						    }
						if(inside_deleted_command)
						    {
							break;
						    }
					    }
				    }
			    }
			if(!inside_deleted_command)
			    {
				nondeleted_lines.append(line);
				nondeleted_lines.append("\n");
			    }
			int bindex = line.indexOf("{");
			while(bindex >= 0)
			    {
				brace_count++;
				bindex = line.indexOf("{", bindex+1);
			    }
			bindex = line.indexOf("}");
			while(bindex >= 0)
			    {
				brace_count--;
				if(brace_count == 0)
				    {
					inside_deleted_command = false;
					inside_class = false;
				    }
				bindex = line.indexOf("}", bindex+1);
			    }
		    }
		br.close();
		br = null;
		isr.close();
		isr = null;
		fis.close();
		fis = null;
		BackupFile(moduleHeaderFile);
		FileOutputStream fos = new FileOutputStream(moduleHeaderFile);
		String nondeleted_lines_str = nondeleted_lines.toString();
		byte buf[] = nondeleted_lines_str.getBytes();
		fos.write(buf);
		fos.close();
		fos = null;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    @SuppressWarnings("unchecked")
    void WriteNMLModuleMessageHeader(File dirFile, String modName, String filename)  throws rcsDesignUserAbortException
    {
	try
	    {
		Vector cmdsVector = null;
		auto_backup = false;
		String deletedItems = "";
		if(null == modulesHashtable)
		    {
			return;
		    }
		ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
		if(null == modInfo)
		    {
			return;
		    }
		File moduleHeaderFile = new File(dirFile,filename);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(moduleHeaderFile, false, true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println(" modInfo.baseModuleName = "+ modInfo.baseModuleName);
		    }
		if(null == modInfo.baseModuleName)
		    {
			cmdsVector = modInfo.cmdsAvailable;
		    }
		else
		    {
			modInfo.cmdsAdded = new Vector();
			for(int i = 0; i < modInfo.cmdsAvailable.size(); i++)
			    {
				String cmd = (String) modInfo.cmdsAvailable.elementAt(i);
				if(debug_on)
				    {
					System.out.println("cmd = "+ cmd);
				    }
				int eqIndex = cmd.indexOf('=');
				if(eqIndex > 0)
				    {
					cmd = cmd.substring(0,eqIndex);
				    }
				boolean is_base_cmd = false;
				for(int j = 0; j  < modInfo.cmdsBaseClass.size(); j++)
				    {
					String base_cmd = (String) modInfo.cmdsBaseClass.elementAt(j);
					if(debug_on)
					    {
						System.out.println("base_cmd = "+ base_cmd);
					    }
					eqIndex = base_cmd.indexOf('=');
					if(eqIndex > 0)
					    {
						base_cmd = base_cmd.substring(0,eqIndex);
					    }
					if(base_cmd.equals(cmd))
					    {
						is_base_cmd = true;
						break;
					    }
				    }
				if(is_base_cmd)
				    {
					continue;
				    }
				modInfo.cmdsAdded.addElement(cmd);
			    }
			cmdsVector = modInfo.cmdsAdded;
		    }
		if(moduleHeaderFile.exists() && gui.useMergerCheckbox.getState())
		    {
			if(modInfo.deleted_commands != null)
			    {
				RemoveDeletedCommandsFromNMLModuleHeader(moduleHeaderFile, modInfo);
				deletedItems += deletedCommands;
			    }
			merger = new Merger();
			merger.template_version = CURRENT_TEMPLATE_VERSION;
			merger.cpp_mode = true;
			merger.GetBackupData(moduleHeaderFile);
			merging_backup = true;
			BackupFile(moduleHeaderFile);
		    }
		auto_backup = true;
		if(debug_on)
		    {
			System.out.println("Creating NML Interface Header for "+modName);
		    }
		FileOutputStream fos = new FileOutputStream(moduleHeaderFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);
		Date createDate = new Date();
		String modNameUpper = modName.toUpperCase();
		merger.WriteLine("/*");
		merger.WriteLine("\t"+modName+"n"+hpp_ext);
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\tThis C++ header file defines the NML Messages used for command and status by "+modInfo.moduleClassName+"");
		if(merger.template_version > 0.0 || !merging_backup)
		    {
			merger.WriteLine("\tTemplate Version "+merger.template_version);
		    }
		merger.WriteLine("");
		merger.WriteLine("\tMODIFICATIONS:");
		if(merging_backup)
		    {
			if(deletedItems.length() > 0)
			    {
				merger.WriteLine("\t"+createDate+"\tDeleted "+deletedItems);
			    }
			merger.WriteLine("\t"+createDate+"\tModified by rcsdesign.");
		    }
		else
		    {
			merger.WriteLine("\t"+createDate+"\tCreated by rcsdesign.");
		    }

		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("*/");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Prevent Multiple Inclusion");
		merger.WriteLine("#ifndef "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("#define "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Include Files");
		Vector previous_includes = new Vector();
		merger.WriteLine("#include \"rcs.hh\" \t// Common RCS definitions");
		previous_includes.addElement("rcs.hh");
		merger.WriteLine("");
		if(modInfo.baseClassCmdsTypeFile != null)
		    {
			merger.WriteLine("// Base class header files");
			String types_file = modInfo.baseClassCmdsTypeFile;
			int fslash_index = types_file.lastIndexOf(File.separator);
			if(fslash_index >= 0)
			    {
				types_file = types_file.substring(fslash_index+1);
			    }
			int bslash_index = types_file.lastIndexOf("\\");
			if(bslash_index >= 0)
			    {
				types_file = types_file.substring(bslash_index+1);
			    }
			merger.WriteLine("#include \""+types_file+"\" \t// Base class Command Messages");
			previous_includes.addElement(types_file);
		    }
		if(modInfo.baseClassStatsTypeFile != null)
		    {
			String types_file = modInfo.baseClassStatsTypeFile;
			int fslash_index = types_file.lastIndexOf(File.separator);
			if(fslash_index >= 0)
			    {
				types_file = types_file.substring(fslash_index+1);
			    }
			int bslash_index = types_file.lastIndexOf("\\");
			if(bslash_index >= 0)
			    {
				types_file = types_file.substring(bslash_index+1);
			    }
			if(types_file.length() > 0 && !types_file.equals("null"))
			    {
				if(!modInfo.baseClassStatsTypeFile.equals(modInfo.baseClassCmdsTypeFile))
				    {
					merger.WriteLine("#include \""+types_file+"\" \t// Base class Status Messages");
					previous_includes.addElement(types_file);
				    }
			    }
		    }
		if(null != modInfo.predefined_type_files)
		    {
			if(merger.template_version >= 1.1)
			    {
				merger.WriteLine("// Predefined type files");
				merger.DisableMerge("//");
			    }
			for(int i = 0; i < modInfo.predefined_type_files.size(); i++)
			    {
				String types_file = (String) modInfo.predefined_type_files.elementAt(i);
				if(null == types_file)
				    {
					break;
				    }

				int fslash_index = types_file.lastIndexOf(File.separator);
				if(fslash_index >= 0)
				    {
					types_file = types_file.substring(fslash_index+1);
				    }
				int bslash_index = types_file.lastIndexOf("\\");
				if(bslash_index >= 0)
				    {
					types_file = types_file.substring(bslash_index+1);
				    }
				boolean previously_included = false;
				for(int j= 0; j < previous_includes.size(); j++)
				    {
					String str = (String) previous_includes.elementAt(j);
					if(types_file.equals(str))
					    {
						previously_included = true;
						break;
					    }
				    }
				if(previously_included)
				    {
					continue;
				    }
				if(!types_file.equals(modInfo.baseClassStatsTypeFile) && !types_file.equals(modInfo.baseClassCmdsTypeFile))
				    {
					merger.WriteLine("#include \""+types_file+"\" \t// Pre-defined types file");
					previous_includes.addElement(types_file);
				    }
			    }
			if(merger.template_version >= 1.1)
			    {
				merger.EnableMerge("//");
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Define the integer type ids.");
		merger.DisableMerge("// ");
		merger.WriteLine("#define "+modNameUpper+"_STATUS_TYPE "+(modInfo.module_number*1000));
		if(null != cmdsVector)
		    {
			for(int i = 0; i< cmdsVector.size(); i++)
			    {
				String cmdName =  (String) cmdsVector.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				merger.WriteLine("#define "+cmdName+"_TYPE "+(modInfo.module_number*1000+i+1));
			    }
		    }
		merger.EnableMerge("// ");


		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Define the NML Message Classes");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Status Class");
		if(!merger.ClassFoundInExistingFile(modNameUpper+"_STATUS"))
		    {
			if(null == modInfo.baseModuleName)
			    {
				merger.WriteLine("class "+modNameUpper+"_STATUS : public RCS_STAT_MSG");
			    }
			else
			    {
				merger.WriteLine("class "+modNameUpper+"_STATUS : public "+modInfo.baseModuleName.toUpperCase()+"_STATUS");
			    }
			merger.WriteLine("{");
			merger.WriteLine("public:");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// Normal Constructor");
			merger.WriteLine("\t"+modNameUpper+"_STATUS();");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// Constructor used by derived classes");
			if(null == modInfo.baseModuleName)
			    {
				merger.WriteLine("\t"+modNameUpper+"_STATUS(NMLTYPE t, size_t s) :  RCS_STAT_MSG(t,s) {};");
			    }
			else
			    {
				merger.WriteLine("\t"+modNameUpper+"_STATUS(NMLTYPE t, size_t s) :  "+modInfo.baseModuleName.toUpperCase()+"_STATUS(t,s) {};");
			    }
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// CMS Update Function");
			merger.WriteLine("\tvoid update(CMS *);");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// Place custom variables here.");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("};");
		    }
		else
		    {
			merger.input_subsection_number++;
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Command Classes");
		merger.WriteLine("");   // blank lineNumber
		if(null != cmdsVector)
		    {
			for(int i = 0; i< cmdsVector.size(); i++)
			    {
				String cmdName =  (String) cmdsVector.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				merger.WriteLine("");
				if(!merger.ClassFoundInExistingFile(cmdName))
				    {
					merger.WriteLine("class "+cmdName+" : public RCS_CMD_MSG");
					merger.WriteLine("{");
					merger.WriteLine("public:");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("\t//Constructor");
					merger.WriteLine("\t"+cmdName+"();");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("\t// CMS Update Function");
					merger.WriteLine("\tvoid update(CMS *);");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("\t// Place custom variables here.");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("};");
					merger.WriteLine("");// blank lineNumber
				    }
				else
				    {
					merger.input_subsection_number++;
				    }
			    }

		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Declare NML format function");
		merger.WriteLine("extern int "+modName+"Format(NMLTYPE, void *, CMS *);");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("#endif \t// "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void WriteNMLAuxMessageHeader(File dirFile, String auxName)  throws rcsDesignUserAbortException
    {
	try
	    {
		if(null == modulesHashtable)
		    {
			return;
		    }
		BufferInfo bi = (BufferInfo) buffersHashtable.get(auxName);
		if(null == bi)
		    {
			return;
		    }
		File auxNMLHeaderFile = new File(dirFile,auxName+"n"+hpp_ext);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(auxNMLHeaderFile, true, true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating NML Interface Header for "+auxName);
		    }
		FileOutputStream fos = new FileOutputStream(auxNMLHeaderFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);
		Date createDate = new Date();
		String auxNameUpper = auxName.toUpperCase();
		merger.WriteLine("/*");
		merger.WriteLine("\t"+auxName+"n"+hpp_ext);
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\tThis C++ header file defines the NML Messages for "+auxNameUpper);
		if(merger.template_version > 0.0 || !merging_backup)
		    {
			merger.WriteLine("\tTemplate Version "+merger.template_version);
		    }
		merger.WriteLine("");
		merger.WriteLine("\tMODIFICATIONS:");
		if(merging_backup)
		    {
			merger.WriteLine("\t"+createDate+"\tModified by rcsdesign.");
		    }
		else
		    {
			merger.WriteLine("\t"+createDate+"\tCreated by rcsdesign.");
		    }

		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("*/");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Prevent Multiple Inclusion");
		merger.WriteLine("#ifndef "+auxNMLHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("#define "+auxNMLHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Include Files");
		merger.WriteLine("#include \"rcs.hh\" \t// Common RCS definitions");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Trying to merge the type ids often results in redefinn the ID twice..");
		merger.DisableMerge("//");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Define the integer type ids.");
		int aux_base = gui.modulesList.getItemCount()*2;
		aux_base += (100 - (aux_base%100));
		merger.WriteLine("#define "+auxNameUpper+"_MSG_TYPE "+((bi.id+aux_base)*1000));
		merger.WriteLine("");// blank lineNumber
		merger.EnableMerge("//");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("// Define the NML Message Classes");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("class "+auxNameUpper+"_MSG : public NMLmsg");
		merger.WriteLine("{");
		merger.WriteLine("public:");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t//Constructor");
		merger.WriteLine("\t"+auxNameUpper+"_MSG();");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// CMS Update Function");
		merger.WriteLine("\tvoid update(CMS *);");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Place custom variables here.");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("};");
		merger.WriteLine("");// blank lineNumber


		merger.WriteLine("// Declare NML format function");
		merger.WriteLine("extern int "+auxName+"Format(NMLTYPE, void *, CMS *);");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("#endif \t// "+auxNMLHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void WriteModuleHeader(File dirFile, String modName)  throws rcsDesignUserAbortException
    {
	try
	    {
		if(null == modulesHashtable)
		    {
			return;
		    }
		ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
		Vector cmdsVector = null;
		File backupfile = null;
		if(null == modInfo)
		    {
			return;
		    }
		File moduleHeaderFile = new File(dirFile,modName+hpp_ext);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(null == modInfo.baseModuleName)
		    {
			cmdsVector = modInfo.cmdsAvailable;
		    }
		else
		    {

			modInfo.cmdsAdded = new Vector();
			for(int i = 0; i < modInfo.cmdsAvailable.size(); i++)
			    {
				String cmd = (String) modInfo.cmdsAvailable.elementAt(i);
				int eqIndex = cmd.indexOf('=');
				if(eqIndex > 0)
				    {
					cmd = cmd.substring(0,eqIndex);
				    }
				boolean is_base_cmd = false;
				for(int j = 0; j  < modInfo.cmdsBaseClass.size(); j++)
				    {
					String base_cmd = (String) modInfo.cmdsBaseClass.elementAt(j);
					eqIndex = base_cmd.indexOf('=');
					if(eqIndex > 0)
					    {
						base_cmd = base_cmd.substring(0,eqIndex);
					    }
					if(base_cmd.equals(cmd))
					    {
						is_base_cmd = true;
						break;
					    }
				    }
				if(is_base_cmd)
				    {
					continue;
				    }
				modInfo.cmdsAdded.addElement(cmd);
			    }
			cmdsVector = modInfo.cmdsAdded;
		    }
		if(CanNotOverwriteExistingFile(moduleHeaderFile, true, true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Module Header for "+modName);
		    }
		FileOutputStream fos = new FileOutputStream(moduleHeaderFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);

		if(merging_backup)
		    {
			if(modInfo.deletedAuxInputNames != null)
			    {
				if(null == modInfo.AuxInputNames)
				    {
					merger.DeleteLine("// auxiliary Input NML Message Files");
				    }
				else if(modInfo.AuxInputNames.size() == 0)
				    {
					merger.DeleteLine("// auxiliary Input NML Message Files");
				    }
				for(int i = 0; i < modInfo.deletedAuxInputNames.size(); i++)
				    {
					String auxInput = (String)  modInfo.deletedAuxInputNames.elementAt(i);
					merger.DeleteLine("#include \""+auxInput+"n"+hpp_ext+"\"\t// NML Messages for "+auxInput);
				    }
			    }
			if(modInfo.deletedAuxOutputNames != null)
			    {
				if(null == modInfo.AuxOutputNames)
				    {
					merger.DeleteLine("// auxiliary Output NML Message Files");
				    }
				else if(modInfo.AuxOutputNames.size() == 0)
				    {
					merger.DeleteLine("// auxiliary Output NML Message Files");
				    }
				for(int i = 0; i < modInfo.deletedAuxOutputNames.size(); i++)
				    {
					String auxOutput = (String)  modInfo.deletedAuxOutputNames.elementAt(i);
					merger.DeleteLine("#include \""+auxOutput+"n"+hpp_ext+"\"\t// NML Messages for "+auxOutput);
				    }
			    }
		    }

		Date createDate = new Date();
		String modNameUpper = modName.toUpperCase();
		merger.WriteLine("/*");
		merger.WriteLine("\t"+modName+hpp_ext);
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\tThis C++ header file defines the class "+modInfo.moduleClassName+"");
		merger.WriteLine("\tIt was generated with the RCS-Design tool.");
		if(merger.template_version > 0.0 || !merging_backup)
		    {
			merger.WriteLine("\tTemplate Version "+merger.template_version);
		    }
		merger.WriteLine("");
		merger.WriteLine("\tMODIFICATIONS:");
		if(merging_backup)
		    {
			merger.WriteLine("\t"+createDate+"\tModified by RCS-Design tool.");
		    }
		else
		    {
			merger.WriteLine("\t"+createDate+"\tCreated by RCS-Design tool.");
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("*/");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Prevent Multiple Inclusion");
		merger.WriteLine("#ifndef "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("#define "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Include Files");
		merger.WriteLine("#include \"rcs.hh\" \t// Common RCS definitions");
		merger.WriteLine("#include \"nml_mod.hh\" \t// NML_MODULE definitions");
		merger.WriteLine("");// blank lineNumber
		if(merger.template_version >= 1.1)
		    {
			merger.DisableMerge("//");
		    }
		if(null != modInfo.baseModuleName)
		    {
			merger.WriteLine("#include \""+modInfo.baseModuleName+hpp_ext+"\" \t// "+modInfo.baseClassName+" definitions");
		    }

		String cmd_header = modInfo.cmdsTypeFile;
		int slash_index = cmd_header.lastIndexOf(File.separator);
		if(slash_index > 0)
		    {
			cmd_header = cmd_header.substring(slash_index+1);
		    }
		merger.WriteLine("#include \""+cmd_header+"\" \t// NML Commands and Status definitions for "+modName);
		String stat_header = modInfo.statsTypeFile;
		slash_index = stat_header.lastIndexOf(File.separator);
		if(slash_index > 0)
		    {
			stat_header = stat_header.substring(slash_index+1);
		    }
		if(!stat_header.equals(cmd_header))
		    {
			merger.WriteLine("#include \""+stat_header+"\" \t// NML Status definitions for "+modName);
		    }
		// merger.WriteLine("#include \""+modName+"n"+hpp_ext+"\" \t// NML Commands and Status definitions for "+modName);
		if(null != modInfo.children_names)
		    {
			for(int i = 0; i< modInfo.children_names.size(); i++)
			    {
				String child =  (String) modInfo.children_names.elementAt(i);
				ModuleInfo childModInfo = (ModuleInfo) modulesHashtable.get(child);
				if(null == childModInfo)
				    {
					merger.WriteLine("#include \""+child+"n"+hpp_ext+"\" \t// NML Commands and Status definitions for "+child);
					continue;
				    }
				String child_stat_header = childModInfo.statsTypeFile;
				slash_index = child_stat_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					child_stat_header = child_stat_header.substring(slash_index+1);
				    }
				merger.WriteLine("#include \""+child_stat_header+"\" \t// NML Status definitions for "+child);
				//merger.WriteLine("#include \""+((String) modInfo.children_names.elementAt(i))+"n"+hpp_ext+"\" \t// NML Commands and Status definitions for "+((String) modInfo.children_names.elementAt(i)));
			    }
		    }
		if(null != modInfo.AuxInputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			if(modInfo.AuxInputNames.size() > 0)
			    {
				merger.WriteLine("// auxiliary Input NML Message Files");
			    }
			for(int i = 0; i < modInfo.AuxInputNames.size(); i++)
			    {
				String auxInput = (String) modInfo.AuxInputNames.elementAt(i);
				merger.WriteLine("#include \""+auxInput+"n"+hpp_ext+"\"\t// NML Messages for "+auxInput);
			    }
		    }
		if(null != modInfo.AuxOutputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			if(modInfo.AuxOutputNames.size() > 0)
			    {
				merger.WriteLine("// auxiliary Output NML Message Files");
			    }
			for(int i = 0; i < modInfo.AuxOutputNames.size(); i++)
			    {
				String auxOutput = (String) modInfo.AuxOutputNames.elementAt(i);
				merger.WriteLine("#include \""+auxOutput+"n"+hpp_ext+"\"\t// NML Messages for "+auxOutput);
			    }
		    }
		merger.EnableMerge("//");
		merger.WriteLine("");// blank lineNumber
		if(modInfo.baseClassName == null)
		    {
			merger.WriteLine("class "+modInfo.moduleClassName+": public NML_MODULE");
		    }
		else
		    {
			merger.WriteLine("class "+modInfo.moduleClassName+": public "+modInfo.baseClassName);
		    }
		merger.WriteLine("{");
		merger.DisableMerge("\t//");
		merger.WriteLine("public:");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t"+modInfo.moduleClassName+"(int _is_base_class = 0); // Constructor");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Overloaded Virtual Functions");
		merger.WriteLine("\tvirtual void PRE_PROCESS();");
		merger.WriteLine("\tvirtual void DECISION_PROCESS();");
		merger.WriteLine("\tvirtual void POST_PROCESS();");
		merger.WriteLine("\tvirtual void INITIALIZE_NML();");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Command Functions");
		if(null != cmdsVector)
		    {
			for(int i = 0; i< cmdsVector.size(); i++)
			    {
				String cmdName = (String) cmdsVector.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				String function_name = cmdName;
				if(function_name.startsWith(modName))
				    {
					function_name = function_name.substring(modName.length());
				    }
				else if(function_name.startsWith(modNameUpper))
				    {
					function_name = function_name.substring(modNameUpper.length());
				    }
				else
				    {
					if(function_name.startsWith("_"))
					    {
						function_name = function_name.substring(1);
					    }
					function_name = "DO_"+function_name;
				    }
				if(function_name.startsWith("_"))
				    {
					function_name = function_name.substring(1);
				    }
				merger.WriteLine("\tvirtual void "+function_name+"("+cmdName+" *);");
			    }
		    }
		if(null != modInfo.cmdsBaseClass)
		    {
			for(int i = 0; i  < modInfo.cmdsBaseClass.size(); i++)
			    {
				String cmdName = (String) modInfo.cmdsBaseClass.elementAt(i);
				String baseModNameUpper = modInfo.baseModuleName.toUpperCase();
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				String function_name = cmdName;
				if(function_name.startsWith(modInfo.baseModuleName))
				    {
					function_name = function_name.substring(modInfo.baseModuleName.length());
				    }
				if(function_name.startsWith(baseModNameUpper))
				    {
					function_name = function_name.substring(baseModNameUpper.length());
				    }
				if(function_name.startsWith("_"))
				    {
					function_name = function_name.substring(1);
				    }
				merger.WriteLine("\tvirtual void "+function_name+"("+cmdName+" *);");
			    }
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Convenience Variables");
		if(modInfo.primaryStatusType == null)
		    {
			merger.WriteLine("\t"+modNameUpper+"_STATUS *"+modName+"_status;");
		    }
		else
		    {
			merger.WriteLine("\t"+modInfo.primaryStatusType+" *"+modName+"_status;");
		    }		
		if(null != modInfo.children_names)
		    {
			for(int i = 0; i< modInfo.children_names.size(); i++)
			    {
				String child = (String) modInfo.children_names.elementAt(i);
				ModuleInfo childModInfo = (ModuleInfo) modulesHashtable.get(child);
				/*
				  boolean is_base_class_child = false;
				  String base_child = child;
				  if(modInfo.subsystem != null && modInfo.baseModuleName != null && modInfo.baseClassChildrenNames != null)
				  {
				  if(child.startsWith(modInfo.subsystem))
				  {
				  base_child = base_child.substring(modInfo.subsystem.length()+1);
				  for(int j = 0; j < modInfo.baseClassChildrenNames.size(); j++)
				  {
				  String str = (String) modInfo.baseClassChildrenNames.elementAt(j);
				  if(str.equals(base_child))
				  {
				  is_base_class_child = true;
				  break;
				  }
				  }
				  }
				  }
				*/
				merger.WriteLine("\tint "+child+"_sub_num;");
				if(null == childModInfo || null == childModInfo.primaryStatusType) 
				    {
					merger.WriteLine("\t"+child.toUpperCase()+"_STATUS *"+child+"_status;");
				    }
				else
				    {
					merger.WriteLine("\t"+childModInfo.primaryStatusType+" *"+child+"_status;");
				    }
			    }
		    }
		if(null != modInfo.AuxInputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("// auxiliary Input NML Channels");
			for(int i = 0; i < modInfo.AuxInputNames.size(); i++)
			    {
				String auxInput = (String) modInfo.AuxInputNames.elementAt(i);
				merger.WriteLine("\tNML *"+auxInput.toUpperCase()+"_CHANNEL;\t// NML Channel for "+auxInput);
				merger.WriteLine("\t"+auxInput.toUpperCase()+"_MSG *"+auxInput+"_data;\t// NML Data for "+auxInput);
			    }
		    }
		if(null != modInfo.AuxOutputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("// auxiliary Output NML Channels");
			for(int i = 0; i < modInfo.AuxOutputNames.size(); i++)
			    {
				String auxOutput = (String) modInfo.AuxOutputNames.elementAt(i);
				merger.WriteLine("\tNML *"+auxOutput.toUpperCase()+"_CHANNEL;\t// NML Channel for "+auxOutput);
				merger.WriteLine("\t"+auxOutput.toUpperCase()+"_MSG "+auxOutput+"_data;\t//NML Data for "+auxOutput);
			    }
		    }

		merger.EnableMerge("\t//");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("private:");
		merger.WriteLine("\t// Add custom variables and functions here.");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("};");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("#endif \t// "+moduleHeaderFile.getName().toUpperCase().replace('.','_'));
		merger.WriteLine("");// blank lineNumber
		merger.Finish();
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void WriteMainCpp(File dirFile, String loop_name)  throws rcsDesignUserAbortException
    {
	try
	    {
		rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) mainloopsHashtable.get(loop_name);
		if(null == loopInfo)
		    {
			System.err.println("WriteMainCpp can't find loopInfo for "+loop_name);
			return;
		    }
		if(null == loopInfo.getModules())
		    {
			System.err.println("WriteMainCpp  "+loop_name+" does not contain any modules.");
			return;
		    }
		File mainCppFile = new File(dirFile,loop_name+"main"+cpp_ext);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		if(CanNotOverwriteExistingFile(mainCppFile, false, true))
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Creating Main C++ for "+loop_name);
		    }
		FileOutputStream fos = new FileOutputStream(mainCppFile);
		PrintWriter pos = new PrintWriter(fos);
		Date createDate = new Date();
		pos.println("/*");
		pos.println("\t"+loop_name+"main.cc");
		pos.println("");// blank lineNumber
		pos.println("\tThis file provides the C++ main function which");
		pos.println("\tcreates and runs the following control modules:");
		pos.println("");// blank lineNumber
		// Eliminate duplicate modules
		String new_modules[] = new String[loopInfo.getModules().size()+1];
		int unique_modules = 0;
		for(int i = 0; i <  loopInfo.getModules().size(); i++)
		    {
			boolean duplicate = false;
			if(loopInfo.getModules().elementAt(i) == null)
			    {
				continue;
			    }
			if(((String) loopInfo.getModules().elementAt(i)).length() < 1)
			    {
				continue;
			    }
			for(int j = 0; j < i; j++)
			    {
				if(((String) loopInfo.getModules().elementAt(j)) == null)
				    {
					continue;
				    }
				if(((String) loopInfo.getModules().elementAt(j)).length() < 1)
				    {
					continue;
				    }
				if(((String) loopInfo.getModules().elementAt(i)).equals(((String) loopInfo.getModules().elementAt(j))))
				    {
					duplicate = true;
					break;
				    }
			    }
			if(!duplicate)
			    {
				new_modules[unique_modules] = (String) loopInfo.getModules().elementAt(i);
				unique_modules++;
			    }
		    }
		if(unique_modules < loopInfo.getModules().size())
		    {
			String unique_modules_list[] = new String[unique_modules+1];
			for(int i = 0; i <  unique_modules; i++)
			    {
				unique_modules_list[i] = new_modules[i];
			    }
			loopInfo.clearModules();
			for(int k = 0; k < unique_modules_list.length; k++)
			    {
				loopInfo.getModules().addElement(unique_modules_list[k]);
			    }
		    }
		for(int i = 0; i < loopInfo.getModules().size(); i++)
		    {
			String modName = ((String) loopInfo.getModules().elementAt(i));
			if(null != modName)
			    {
				ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
				pos.println("\t\t"+modInfo.moduleClassName+"");
			    }
		    }
		pos.println("");
		pos.println("\tMODIFICATIONS:");
		pos.println("\t"+createDate+"\tCreated.");
		pos.println("");// blank lineNumber
		pos.println("*/");
		pos.println("");// blank lineNumber
		pos.println("// Include Files");
		pos.println("#include <stdlib.h>\t// exit()");
		pos.println("#include <signal.h>\t// SIGINT, signal()");
		pos.println("#include \"rcs.hh\" \t// Common RCS definitions");
		pos.println("#include \"nml_mod.hh\" \t// NML_MODULE definitions");
		for(int i = 0; i < loopInfo.getModules().size(); i++)
		    {
			String modName = ((String) loopInfo.getModules().elementAt(i));
			if(null != modName)
			    {
				ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
				pos.println("#include \""+modName+hpp_ext+"\"\t// definition of "+modInfo.moduleClassName+"");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.println("// flag signifying main loop is to terminate");
		pos.println("int "+loop_name+"_done = 0;");
		pos.println("");// blank lineNumber
		pos.println("//signal handler for ^C");
		pos.println("extern \"C\" void "+loop_name+"_quit(int sig);");
		pos.println("void "+loop_name+"_quit(int sig)");
		pos.println("{");
		pos.println("\t"+loop_name+"_done = 1;");
		pos.println("}");
		pos.println("");// blank lineNumber
		pos.println("// main loop, running "+modulesHashtable.size()+" controller(s)");
		if(!mswinDevPlat)
		    {
			pos.println("#ifdef VXWORKS");
			pos.println("extern \"C\" int "+loop_name+"_run();");
			pos.println("");
			pos.println("int "+loop_name+"_run()");
			pos.println("#else");
		    }
		pos.println("int main(int argc, char **argv)");
		if(!mswinDevPlat)
		    {
			pos.println("#endif");
		    }
		pos.println("{");
		pos.println("");// blank lineNumber
		pos.println("\tset_rcs_print_destination(RCS_PRINT_TO_STDOUT);");
		pos.println("");// blank lineNumber
		pos.println("\tRCS_TIMER *timer = new RCS_TIMER("+loopInfo.cycle_time+");");
		for(int i = 0; i < loopInfo.getModules().size(); i++)
		    {
			String modName = ((String) loopInfo.getModules().elementAt(i));
			if(null != modName)
			    {
				ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
				pos.println("\t"+modInfo.moduleClassName+" *"+modName+" = new "+modInfo.moduleClassName+"();");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.println("\t// set the SIGINT handler");
		pos.println("\tsignal(SIGINT, "+loop_name+"_quit);");
		pos.println("");// blank lineNumber
		pos.println("\t// enter main loop");
		pos.println("\twhile(!"+loop_name+"_done)");
		pos.println("\t{");
		for(int i = 0; i < loopInfo.getModules().size(); i++)
		    {
			String modName = ((String) loopInfo.getModules().elementAt(i));
			if(null != modName)
			    {
				pos.println("\t\t"+modName+"->controller();");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.println("\t\ttimer->wait();");
		pos.println("\t}");
		pos.println("");// blank lineNumber
		pos.println("\t// Delete Modules");
		for(int i = 0; i < loopInfo.getModules().size(); i++)
		    {
			String modName = ((String) loopInfo.getModules().elementAt(i));
			if(null != modName)
			    {
				pos.println("\tdelete "+modName+";");
			    }
		    }
		pos.println("");// blank lineNumber
		pos.println("\t// Delete Timer");
		pos.println("\tdelete timer;");
		pos.println("}");
		pos.println("");// blank lineNumber
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    void RemoveDeletedCommandsFromModuleCpp(File moduleCppFile, ModuleInfo modInfo)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("RemoveDeletedCommandsFromNMLModuleHeader ("+moduleCppFile+", "+modInfo+")");
		    }
		if(moduleCppFile.exists())
		    {
			if(!moduleCppFile.canWrite())
			    {
				gui.Alert("Can not write over "+moduleCppFile);
				return;
			    }
			if(!moduleCppFile.canRead())
			    {
				gui.Alert("Can not read  "+moduleCppFile);
				return;
			    }
		    }
		else
		    {
			return;
		    }
		if(null == modInfo.deleted_commands)
		    {
			return;
		    }
		StringBuffer nondeleted_lines = new StringBuffer((int) moduleCppFile.length());
		FileInputStream fis = new FileInputStream(moduleCppFile);
		InputStreamReader isr = new InputStreamReader(fis);
		BufferedReader br = new BufferedReader(isr);
		String comment = null;
		boolean inside_comment = false;
		boolean inside_deleted_command = false;
		boolean blank_line_needed = false;
		int brace_count = 0;
		String modNameUpper = modInfo.Name.toUpperCase();
		while(true)
		    {
			String line = br.readLine();
			if(line == null)
			    {
				break;
			    }
			if(brace_count == 0)
			    {
				if(!inside_deleted_command)
				    {
					StringTokenizer tokenizer = new StringTokenizer(line,"\t \r\n(){};,");
					while(tokenizer.hasMoreTokens())
					    {
						String token = tokenizer.nextToken();
						if(debug_on)
						    {
							System.out.println("token = "+token);
						    }
						if(null == token)
						    {
							break;
						    }
						if(token.equals("//"))
						    {
							break;
						    }
						for(int i = 0; i < modInfo.deleted_commands.size(); i++)
						    {
							String delCmd = (String) modInfo.deleted_commands.elementAt(i);
							if(delCmd.startsWith(modNameUpper))
							    {
								delCmd = modNameUpper+"_MODULE::"+delCmd.substring(modNameUpper.length()+1);
							    }
							if(debug_on)
							    {
								System.out.println("delCmd = "+delCmd);
							    }
							if(token.equals(delCmd))
							    {
								inside_deleted_command = true;
								comment = null;
								System.out.println("inside_deleted_command");

								break;
							    }
						    }
						if(inside_deleted_command)
						    {
							break;
						    }
					    }
				    }
			    }

			int cbindex = line.indexOf("/*");
			if(cbindex >= 0)
			    {
				int ceindex = line.indexOf("*/",cbindex);
				if( ceindex < 0)
				    {
					inside_comment = true;
					comment = line.substring(cbindex) +"\n";
					line = line.substring(0,cbindex);
					System.out.println("comment="+comment);
					System.out.println("line="+line);
					System.out.println("inside_comment="+inside_comment);
					if(!inside_deleted_command  && line.length() >0 )
					    {
						nondeleted_lines.append(line);
					    }
					continue;
				    }
			    }
			if(inside_comment)
			    {
				int ceindex = line.indexOf("*/");
				if( ceindex >= 0)
				    {
					inside_comment = false;
				    }
				comment += line +"\n";
				System.out.println("comment="+comment);
				System.out.println("line="+line);
				System.out.println("inside_comment="+inside_comment);
				continue;
			    }
			if(line.length() == 0)
			    {
				blank_line_needed = true;
				continue;
			    }
			if(!inside_deleted_command && !inside_comment && line.length() >0 )
			    {
				if(blank_line_needed)
				    {
					nondeleted_lines.append("\n");
					blank_line_needed = false;
				    }
				if(null != comment)
				    {
					System.out.println("Adding comment: comment="+comment);
					System.out.println("line=("+line+")");
					nondeleted_lines.append(comment);
					comment = null;
				    }
				nondeleted_lines.append(line);
				nondeleted_lines.append("\n");
			    }
			int bindex = line.indexOf("{");
			while(bindex >= 0)
			    {
				brace_count++;
				bindex = line.indexOf("{", bindex+1);
			    }
			bindex = line.indexOf("}");
			while(bindex >= 0)
			    {
				brace_count--;
				if(brace_count == 0)
				    {
					inside_deleted_command = false;
				    }
				bindex = line.indexOf("}", bindex+1);
			    }
		    }
		br.close();
		br = null;
		isr.close();
		isr = null;
		fis.close();
		fis = null;
		BackupFile(moduleCppFile);
		FileOutputStream fos = new FileOutputStream(moduleCppFile);
		String nondeleted_lines_str = nondeleted_lines.toString();
		byte buf[] = nondeleted_lines_str.getBytes();
		fos.write(buf);
		fos.close();
		fos = null;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void WriteModuleCpp(File dirFile, String modName, String file_name)  throws rcsDesignUserAbortException
    {
	try
	    {
		if(null == modulesHashtable)
		    {
			return;
		    }
		ModuleInfo modInfo = (ModuleInfo) modulesHashtable.get(modName);
		if(null == modInfo)
		    {
			return;
		    }
		Vector cmdsVector = null;
		if(null == modInfo.baseModuleName)
		    {
			cmdsVector = modInfo.cmdsAvailable;
		    }
		else
		    {
			modInfo.cmdsAdded = new Vector();
			for(int i = 0; i < modInfo.cmdsAvailable.size(); i++)
			    {
				String cmd = (String) modInfo.cmdsAvailable.elementAt(i);
				int eqIndex = cmd.indexOf('=');
				if(eqIndex > 0)
				    {
					cmd = cmd.substring(0,eqIndex);
				    }
				boolean is_base_cmd = false;
				for(int j = 0; j  < modInfo.cmdsBaseClass.size(); j++)
				    {
					String base_cmd = (String) modInfo.cmdsBaseClass.elementAt(j);
					eqIndex = base_cmd.indexOf('=');
					if(eqIndex > 0)
					    {
						base_cmd = base_cmd.substring(0,eqIndex);
					    }
					if(base_cmd.equals(cmd))
					    {
						is_base_cmd = true;
						break;
					    }
				    }
				if(is_base_cmd)
				    {
					continue;
				    }
				modInfo.cmdsAdded.addElement(cmd);
			    }
			cmdsVector = modInfo.cmdsAdded;
		    }
		File moduleCppFile = new File(dirFile,file_name);
		if(!dirFile.canWrite())
		    {
			gui.Alert("Can not write into "+dirFile);
			return;
		    }
		auto_backup = false;
		if(CanNotOverwriteExistingFile(moduleCppFile, false, true))
		    {
			return;
		    }
		if(moduleCppFile.exists())
		    {
			if(gui.useMergerCheckbox.getState())
			    {
				if(modInfo.deleted_commands != null)
				    {
					RemoveDeletedCommandsFromModuleCpp(moduleCppFile, modInfo);
				    }
				merger.cpp_mode = true;
				merger.GetBackupData(moduleCppFile);
				merging_backup = true;
			    }
			BackupFile(moduleCppFile);
		    }
		auto_backup = true;
		String deletedItems = "";
		if(debug_on)
		    {
			System.out.println("Creating Module C++ for "+modName);
		    }
		if(merging_backup)
		    {
			if(modInfo.deletedAuxInputNames != null)
			    {
				if(null == modInfo.AuxInputNames)
				    {
					merger.DeleteLine("// auxiliary Input NML Message Files");
				    }
				else if(modInfo.AuxInputNames.size() == 0)
				    {
					merger.DeleteLine("// auxiliary Input NML Message Files");
				    }
				for(int i = 0; i < modInfo.deletedAuxInputNames.size(); i++)
				    {
					String auxInput = (String)  modInfo.deletedAuxInputNames.elementAt(i);
					deletedItems += auxInput+", ";
					merger.DeleteLine("#include \""+auxInput+"n"+hpp_ext+"\"\t// NML Messages for "+auxInput);
					merger.DeleteLine("\t// Read new data from "+auxInput);
					merger.DeleteLine("\t"+auxInput.toUpperCase()+"_CHANNEL->read();");
				    }
			    }
			if(modInfo.deletedAuxOutputNames != null)
			    {
				if(null == modInfo.AuxOutputNames)
				    {
					merger.DeleteLine("// auxiliary Output NML Message Files");
				    }
				else if(modInfo.AuxOutputNames.size() == 0)
				    {
					merger.DeleteLine("// auxiliary Output NML Message Files");
				    }
				for(int i = 0; i < modInfo.deletedAuxOutputNames.size(); i++)
				    {
					String auxOutput = (String)  modInfo.deletedAuxOutputNames.elementAt(i);
					deletedItems += auxOutput+", ";
					merger.DeleteLine("#include \""+auxOutput+"n"+hpp_ext+"\"\t// NML Messages for "+auxOutput);
					merger.DeleteLine("\t// Write data to "+auxOutput);
					merger.DeleteLine("\t"+auxOutput.toUpperCase()+"_CHANNEL->write(&"+auxOutput+"_data);");
				    }
			    }
		    }
		FileOutputStream fos = new FileOutputStream(moduleCppFile);
		PrintWriter pos = new PrintWriter(fos);
		merger.SetPrintWriter(pos);

		Date createDate = new Date();
		String modNameUpper = modName.toUpperCase();
		merger.WriteLine("/*");
		merger.WriteLine("\t"+modName+cpp_ext);
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\tThis C++ file defines member functions for the class "+modInfo.moduleClassName+"");
		merger.WriteLine("\tIt was generated with rcsdesign");
		merger.WriteLine("\t\twith template version "+merger.template_version);
		merger.WriteLine("");
		merger.WriteLine("\tMODIFICATIONS:");
		if(merging_backup)
		    {
			if(deletedItems.length() > 0)
			    {
				merger.WriteLine("\t"+createDate+"\tDeleted "+deletedItems);
			    }
			merger.WriteLine("\t"+createDate+"\tModified with RCS-Design tool.");
		    }
		else
		    {
			merger.WriteLine("\t"+createDate+"\tCreated with RCS-Design tool.");
		    }
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("*/");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Include Files");
		merger.WriteLine("#include \"rcs.hh\" \t// Common RCS definitions");
		merger.WriteLine("#include \"nml_mod.hh\" \t// NML_MODULE definitions");
		merger.WriteLine("#include \""+modName+hpp_ext+"\" \t// "+modInfo.moduleClassName+" definitions");
		merger.WriteLine("");
		if(merger.template_version >= 1.1)
		    {
			merger.DisableMerge("//");
		    }
		String cmd_header = modInfo.cmdsTypeFile;
		int slash_index = cmd_header.lastIndexOf(File.separator);
		if(slash_index > 0)
		    {
			cmd_header = cmd_header.substring(slash_index+1);
		    }
		merger.WriteLine("#include \""+cmd_header+"\" \t// NML Commands and Status definitions for "+modName);
		String stat_header = modInfo.statsTypeFile;
		slash_index = stat_header.lastIndexOf(File.separator);
		if(slash_index > 0)
		    {
			stat_header = stat_header.substring(slash_index+1);
		    }
		if(!stat_header.equals(cmd_header))
		    {
			merger.WriteLine("#include \""+stat_header+"\" \t// NML Status definitions for "+modName);
		    }
		if(null != modInfo.children_names)
		    {
			for(int i = 0; i< modInfo.children_names.size(); i++)
			    {
				String child =  (String) modInfo.children_names.elementAt(i);
				ModuleInfo childModInfo = (ModuleInfo) modulesHashtable.get(child);
				if(null == childModInfo)
				    {
					merger.WriteLine("#include \""+child+"n"+hpp_ext+"\" \t// NML Commands and Status definitions for "+child);
					continue;
				    }
				String child_cmd_header = childModInfo.cmdsTypeFile;
				slash_index = child_cmd_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					child_cmd_header = child_cmd_header.substring(slash_index+1);
				    }
				merger.WriteLine("#include \""+child_cmd_header+"\" \t// NML Commands and Status definitions for "+child);
				String child_stat_header = childModInfo.statsTypeFile;
				slash_index = child_stat_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					child_stat_header = child_stat_header.substring(slash_index+1);
				    }
				if(!child_stat_header.equals(child_cmd_header))
				    {
					merger.WriteLine("#include \""+child_stat_header+"\" \t// NML Status definitions for "+child);
				    }		  
			    }
		    }
		if(null != modInfo.AuxInputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			if( modInfo.AuxInputNames.size() > 0)
			    {
				merger.WriteLine("// auxiliary Input NML Message Files");
			    }
			for(int i = 0; i < modInfo.AuxInputNames.size(); i++)
			    {
				String auxInput = (String) modInfo.AuxInputNames.elementAt(i);
				merger.WriteLine("#include \""+auxInput+"n"+hpp_ext+"\"\t// NML Messages for "+auxInput);
			    }
		    }
		if(null != modInfo.AuxOutputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			if( modInfo.AuxOutputNames.size() > 0)
			    {
				merger.WriteLine("// auxiliary Output NML Message Files");
			    }
			for(int i = 0; i < modInfo.AuxOutputNames.size(); i++)
			    {
				String auxOutput = (String) modInfo.AuxOutputNames.elementAt(i);
				merger.WriteLine("#include \""+auxOutput+"n"+hpp_ext+"\"\t// NML Messages for "+auxOutput);
			    }
		    }
		if(merger.template_version >= 1.1)
		    {
			merger.EnableMerge("//");
		    }

		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("// Constructor");
		if(modInfo.baseClassName != null)
		    {
			merger.WriteLine(modInfo.moduleClassName+"::"+modInfo.moduleClassName+"(int _is_base_class) : "+modInfo.baseClassName+"(1)");
		    }
		else
		    {
			merger.WriteLine(modInfo.moduleClassName+"::"+modInfo.moduleClassName+"(int _is_base_class)");
		    }
		merger.WriteLine("{");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Initialize the NML channels if this module is not being used as the base class for another module.");
		merger.WriteLine("\tif(!_is_base_class)");
		merger.WriteLine("\t{");
		merger.WriteLine("\t\tINITIALIZE_NML();");
		merger.WriteLine("\t}");
		merger.WriteLine("\t// Add additional code to initialize the module here.");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("}");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("\t// Overloaded Virtual Functions");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("/*");
		merger.WriteLine("INITIALIZE_NML");
		merger.WriteLine("");// blank lineNumber
		if(!merging_backup)
		    {
			merger.WriteLine("The INITIALIZE_NML function is ussually called only once from within the");
			merger.WriteLine("constructor. It should not be called if a derived class will also call it.\n");
		    }
		merger.WriteLine("*/");
		merger.WriteLine("void "+modInfo.moduleClassName+"::INITIALIZE_NML()");
		merger.WriteLine("{");
		merger.DisableMerge("\t//");
		merger.WriteLine("\tsetErrorLogChannel(new NML(nmlErrorFormat, \"errlog\", \""+modName+"\", \""+AppName+".nml\"));");

		String cmd_base = cmd_header;
		int dot_index = cmd_base.lastIndexOf('.');
		if(dot_index > 0)
		    {
			cmd_base = cmd_base.substring(0,dot_index);
		    }
		//System.out.println("cmd_base="+cmd_base+", cmd_header="+cmd_header);
		String format_function = modName+"n";
		if(modInfo.cmdFormatFunction != null)
		    {
			format_function = modInfo.cmdFormatFunction;
		    }
		else if(!cmd_base.equals(modName+"n"))
		    {
			format_function = cmd_base+"Format";
		    }
		else
		    {
			format_function = modName+"Format";
		    }
		merger.WriteLine("\tsetCmdChannel(new RCS_CMD_CHANNEL("+format_function+", \""+modName+"_cmd\", \""+modName+"\", \""+AppName+".nml\"));");
		if(null == modInfo.primaryStatusType)
		    {
			merger.WriteLine("\t"+modName+"_status = new "+modNameUpper+"_STATUS();");
		    }
		else
		    {
			merger.WriteLine("\t"+modName+"_status = new "+modInfo.primaryStatusType+"();");
		    }
		if(modInfo.baseModuleName != null)
		    {
			merger.WriteLine("\t"+modInfo.baseModuleName+"_status = "+modName+"_status;");
		    }
		String stat_base = stat_header;
		dot_index = stat_base.lastIndexOf('.');
		if(dot_index > 0)
		    {
			stat_base = stat_base.substring(0,dot_index);
		    }
		if(modInfo.statFormatFunction != null)
		    {
			format_function = modInfo.statFormatFunction;
		    }
		else if(!stat_base.equals(modName+"n"))
		    {
			format_function = stat_base+"Format";
		    }
		else
		    {
			format_function = modName+"Format";
		    }
		merger.WriteLine("\tsetStatChannel(new RCS_STAT_CHANNEL("+format_function+", \""+modName+"_sts\", \""+modName+"\", \""+AppName+".nml\"), "+modName+"_status);");
		merger.WriteLine("");// blank lineNumber
		if(null != modInfo.children_names)
		    {
			for(int i = 0; i< modInfo.children_names.size(); i++)
			    {

				String child = (String) modInfo.children_names.elementAt(i);
				boolean is_base_class_child = false;
				String base_child = child;
				if(modInfo.subsystem != null && modInfo.baseModuleName != null && modInfo.baseClassChildrenNames != null)
				    {
					if(child.startsWith(modInfo.subsystem))
					    {
						base_child = base_child.substring(modInfo.subsystem.length()+1);
						for(int j = 0; j < modInfo.baseClassChildrenNames.size(); j++)
						    {
							String str = (String) modInfo.baseClassChildrenNames.elementAt(j);
							if(str.equals(base_child))
							    {
								is_base_class_child = true;
								break;
							    }
						    }
					    }
				    }
				merger.WriteLine("\t"+child+"_sub_num = ");
				merger.WriteLine("\t\taddSubordinate(");
				ModuleInfo childModInfo = (ModuleInfo) modulesHashtable.get(child);
				String child_cmd_header = childModInfo.cmdsTypeFile;
				slash_index = child_cmd_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					child_cmd_header = child_cmd_header.substring(slash_index+1);
				    }
				String child_cmd_base = child_cmd_header;
				dot_index = child_cmd_base.lastIndexOf('.');
				if(dot_index > 0)
				    {
					child_cmd_base = child_cmd_base.substring(0,dot_index);
				    }
				if(childModInfo.cmdFormatFunction != null)
				    {
					format_function = childModInfo.cmdFormatFunction;
				    }
				else if(!child_cmd_base.equals(child+"n"))
				    {
					format_function = child_cmd_base+"Format";
				    }
				else
				    {
					format_function = child+"Format";
				    }
				merger.WriteLine("\t\t\tnew RCS_CMD_CHANNEL("+format_function+", \""+child+"_cmd\", \""+modName+"\", \""+AppName+".nml\"),");
				String child_stat_header = childModInfo.statsTypeFile;
				slash_index = child_stat_header.lastIndexOf(File.separator);
				if(slash_index > 0)
				    {
					child_stat_header = child_stat_header.substring(slash_index+1);
				    }
				String child_stat_base = child_stat_header;
				dot_index = child_stat_base.lastIndexOf('.');
				if(dot_index > 0)
				    {
					child_stat_base = child_stat_base.substring(0,dot_index);
				    }
				if(childModInfo.statFormatFunction != null)
				    {
					format_function = childModInfo.statFormatFunction;
				    }
				else if(!child_stat_base.equals(child+"n"))
				    {
					format_function = child_stat_base+"Format";
				    }
				else
				    {
					format_function = child + "Format";
				    }
				merger.WriteLine("\t\t\tnew  RCS_STAT_CHANNEL("+format_function+", \""+child+"_sts\", \""+modName+"\", \""+AppName+".nml\"));");		
				if(childModInfo.primaryStatusType == null)
				    {
					merger.WriteLine("\t"+child+"_status = ("+child.toUpperCase()+"_STATUS *)  statusInData["+child+"_sub_num];");
				    }
				else
				    {
					merger.WriteLine("\t"+child+"_status = ("+childModInfo.primaryStatusType+" *)  statusInData["+child+"_sub_num];");
				    }			
				if(is_base_class_child)
				    {
					merger.WriteLine("\t"+base_child+"_status = "+child+"_status;");
					merger.WriteLine("\t"+base_child+"_sub_num = "+child+"_sub_num;");
				    }
				merger.WriteLine("");// blank lineNumber
			    }
		    }
		if(null != modInfo.AuxInputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// auxiliary Input NML Channels");
			for(int i = 0; i < modInfo.AuxInputNames.size(); i++)
			    {
				String auxInput = (String) modInfo.AuxInputNames.elementAt(i);
				merger.WriteLine("\t//"+auxInput);
				merger.WriteLine("\t"+auxInput.toUpperCase()+"_CHANNEL = new NML("+auxInput+"Format, \""+auxInput+"\", \""+modName+"\", \""+AppName+".nml\");");
				merger.WriteLine("\t"+auxInput+"_data = ("+auxInput.toUpperCase()+"_MSG *) "+auxInput.toUpperCase()+"_CHANNEL->get_address();");
			    }
		    }
		if(null != modInfo.AuxOutputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// auxiliary Output NML Channels");
			for(int i = 0; i < modInfo.AuxOutputNames.size(); i++)
			    {
				String auxOutput = (String) modInfo.AuxOutputNames.elementAt(i);
				merger.WriteLine("\t//"+auxOutput);
				merger.WriteLine("\t"+auxOutput.toUpperCase()+"_CHANNEL = new NML("+auxOutput+"Format, \""+auxOutput+"\", \""+modName+"\", \""+AppName+".nml\");");
			    }
		    }
		merger.EnableMerge("\t//");
		merger.WriteLine("}");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("/*");
		merger.WriteLine("PRE_PROCESS");
		merger.WriteLine("");// blank lineNumber
		if(!merging_backup)
		    {
			merger.WriteLine(" The PRE_PROCESS function is called every cycle after the command and");
			merger.WriteLine(" subordinates status have been read but before DECISION_PROCESS is called.");
			merger.WriteLine(" It is intended to be used for tasks such as sensory processing that should");
			merger.WriteLine(" be performed every cycle regardless of the current command or state.");
		    }
		merger.WriteLine("*/");
		merger.WriteLine("void "+modInfo.moduleClassName+"::PRE_PROCESS()");
		merger.WriteLine("{");
		if(modInfo.baseClassName != null)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// Call Overloaded  base class PRE_PROCESS function.");
			merger.WriteLine("\t"+modInfo.baseClassName+"::PRE_PROCESS();");
			merger.WriteLine("");// blank lineNumber
		    }
		if(null != modInfo.AuxInputNames)
		    {
			merger.WriteLine("\t// auxiliary Input NML Channels");
			for(int i = 0; i < modInfo.AuxInputNames.size(); i++)
			    {
				String auxInput = (String) modInfo.AuxInputNames.elementAt(i);
				BufferInfo bi = (BufferInfo) buffersHashtable.get(auxInput);
				if(null == bi)
				    {
					continue;
				    }
				ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(modName);
				if(null == ci)
				    {
					continue;
				    }
				if(ci.updateEveryCycle)
				    {
					merger.WriteLine("\t// Read new data from "+auxInput);
					merger.WriteLine("\t"+auxInput.toUpperCase()+"_CHANNEL->read();");
				    }
			    }

		    }
		merger.WriteLine("\t// Pre-Processing Code");
		merger.WriteLine("}");
		merger.WriteLine("");// blank lineNumber

	
		boolean have_init=false;
		boolean have_halt=false;

		if(null != cmdsVector)
		    {
			for(int i = 0; i< cmdsVector.size(); i++)
			    {
				String cmdName = (String) cmdsVector.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				if(cmdName.equals(modNameUpper+"_INIT"))
				    {
					have_init=true;
				    }
				if(cmdName.equals(modNameUpper+"_HALT"))
				    {
					have_halt=true;
				    }
				if(have_init && have_halt)
				    {
					break;
				    }
			    }
		    }

		merger.WriteLine("/*");
		merger.WriteLine("DECISION_PROCESS");
		merger.WriteLine("");// blank lineNumber
		if(!merging_backup)
		    {
			merger.WriteLine(" The DECISION_PROCESS function is called every cycle as long as there is a non-zero command.");
			merger.WriteLine(" It is expected to call a command function based on commandInData->type.");
		    }
		merger.WriteLine("*/");
		merger.WriteLine("void "+modInfo.moduleClassName+"::DECISION_PROCESS()");
		merger.WriteLine("{");
		merger.WriteLine("\tswitch(commandInData->type)");
		merger.WriteLine("\t{");
		merger.DisableMerge("\t//");
		if(have_init)
		    {
			merger.WriteLine("\tcase "+modNameUpper+"_INIT_TYPE:");
			merger.WriteLine("\t\tINIT(("+modNameUpper+"_INIT *)commandInData);");
			merger.WriteLine("\t\tbreak;");
		    }
		merger.WriteLine(""); // blank lineNumber
		if(have_halt)
		    {
			merger.WriteLine("\tcase "+modNameUpper+"_HALT_TYPE:");
			merger.WriteLine("\t\tHALT(("+modNameUpper+"_HALT *)commandInData);");
			merger.WriteLine("\t\tbreak;");
		    }
		merger.WriteLine("");// blank lineNumber
		if(null != modInfo.cmdsAvailable)
		    {
			for(int i = 0; i< modInfo.cmdsAvailable.size(); i++)
			    {
				String cmdName = (String) modInfo.cmdsAvailable.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				if(cmdName.equals(modNameUpper+"_INIT"))
				    {
					continue;
				    }
				if(cmdName.equals(modNameUpper+"_HALT"))
				    {
					continue;
				    }
				String function_name = cmdName;
				if(function_name.startsWith(modName))
				    {
					function_name = function_name.substring(modName.length());
				    }
				else if(function_name.startsWith(modNameUpper))
				    {
					function_name = function_name.substring(modNameUpper.length());
				    }
				else
				    {
					if(function_name.startsWith("_"))
					    {
						function_name = function_name.substring(1);
					    }
					function_name = "DO_"+function_name;
				    }

				if(null != modInfo.baseModuleName)
				    {
					if(function_name.startsWith(modInfo.baseModuleName))
					    {
						function_name = function_name.substring(modInfo.baseModuleName.length());
					    }
					else if(function_name.startsWith(modInfo.baseModuleName.toUpperCase()))
					    {
						function_name = function_name.substring(modInfo.baseModuleName.toUpperCase().length());
					    }
				    }
				if(function_name.startsWith("_"))
				    {
					function_name = function_name.substring(1);
				    }
				if(null != modInfo && 
				   null != modInfo.definedValues && 
				   null != cmdName &&
				   modInfo.definedValues.contains(cmdName+"_TYPE"))
				    {
					merger.WriteLine("\tcase "+cmdName+"_TYPE:");
				    }
				else
				    {
					StructureTypeInfo sti = (StructureTypeInfo) ModuleInfo.m_structInfoByNameHashTable.get(cmdName);
					if(null == sti)
					    {
						continue;
					    }
					merger.WriteLine("\tcase "+sti.type_id_string+":");
				    }
				merger.WriteLine("\t\t"+function_name+"(("+cmdName+" *)commandInData);");
				merger.WriteLine("\t\tbreak;");
				merger.WriteLine("");// blank lineNumber
			    }
		    }
		merger.EnableMerge("\t//");
		merger.WriteLine("");// blank lineNumber

		merger.WriteLine("\tdefault:");
		merger.WriteLine("\t\tlogError(\"The command %d is not recognized.\",commandInData->type);");
		merger.WriteLine("\t\tbreak;");
		merger.WriteLine("\t}");
		merger.WriteLine("}");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("/*");
		merger.WriteLine("POST_PROCESS");
		merger.WriteLine("");// blank lineNumber
		if(!merging_backup)
		    {
			merger.WriteLine(" The POST_PROCESS function is called every cycle after DECISION_PROCESS is called");
			merger.WriteLine(" but before the status and the subordinates commands  have been written.");
			merger.WriteLine(" It is intended to be used for tasks such as output filters that should");
			merger.WriteLine(" be performed every cycle regardless of the current command or state.");
		    }
		merger.WriteLine("*/");
		merger.WriteLine("void "+modInfo.moduleClassName+"::POST_PROCESS()");
		merger.WriteLine("{");
		merger.WriteLine("\t// Post-Processing Code");
		if(null != modInfo.AuxOutputNames)
		    {
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t// auxiliary Output NML Channels");
			for(int i = 0; i < modInfo.AuxOutputNames.size(); i++)
			    {
				String auxOutput = (String) modInfo.AuxOutputNames.elementAt(i);
				BufferInfo bi = (BufferInfo) buffersHashtable.get(auxOutput);
				if(null == bi)
				    {
					continue;
				    }
				ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(modName);
				if(null == ci)
				    {
					continue;
				    }
				if(ci.updateEveryCycle)
				    {
					merger.WriteLine("\t// Write data to "+auxOutput);
					merger.WriteLine("\t"+auxOutput.toUpperCase()+"_CHANNEL->write(&"+auxOutput+"_data);");
				    }
			    }
		    }

		if(modInfo.baseClassName != null)
		    {
			merger.WriteLine("\t// Call Overloaded  base class POST_PROCESS function.");
			merger.WriteLine("\t"+modInfo.baseClassName+"::POST_PROCESS();");
			merger.WriteLine("");// blank lineNumber
		    }

		merger.WriteLine("}");
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("");// blank lineNumber
		merger.WriteLine("\t// Command Functions");

		merger.WriteLine("");// blank lineNumber


		if(have_init)
		    {
			merger.WriteLine("/*");
			merger.WriteLine("INIT");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("Parameter(s):");
			merger.WriteLine(modNameUpper+"_INIT *cmd_in -- NML Message sent from superior.");
			merger.WriteLine("");// blank lineNumber
			if(!merging_backup)
			    {
				merger.WriteLine(" Most Modules will have an INIT command.");
				merger.WriteLine(" The INIT function is expected to initialize any variables that may be");
				merger.WriteLine(" in an uninitialized or unknown state, send INIT commands to its subordinates,");
				merger.WriteLine(" wait for the subordinates to be DONE and then inform its superior that it is done.");
				merger.WriteLine(" The state tables should use the STATE_MATCH macro so the diagnostics tool can ");
				merger.WriteLine(" highlight the current line in the state table.");
			    }
			merger.WriteLine("*/");
			merger.WriteLine("void "+modInfo.moduleClassName+"::INIT("+modNameUpper+"_INIT *cmd_in)");
			merger.WriteLine("{");
			merger.DisableMerge("//");
			if(null != modInfo.children_names)
			    {
				for(int i = 0; i< modInfo.children_names.size(); i++)
				    {
					merger.WriteLine("\t"+((String) modInfo.children_names.elementAt(i)).toUpperCase()+"_INIT "+ ((String) modInfo.children_names.elementAt(i))+"InitMsg;");
				    }
			    }
			merger.EnableMerge("//");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\tif(STATE_MATCH(NEW_COMMAND))");
			merger.WriteLine("\t{");
			merger.WriteLine("\t\t// Send an INIT command to all subordinates.");
			merger.DisableMerge("\t\t//");
			if(null != modInfo.children_names)
			    {
				for(int i = 0; i< modInfo.children_names.size(); i++)
				    {
					merger.WriteLine("\t\tsendCommand(&"+((String) modInfo.children_names.elementAt(i))+"InitMsg, "+((String) modInfo.children_names.elementAt(i))+"_sub_num);");
				    }
			    }
			merger.EnableMerge("\t\t//");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t\tstateNext(S1);");
			merger.WriteLine("\t\t// Reinitialize variables here.");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\t}");
			merger.WriteLine("\t// Wait for all subordinates to report done.");
			merger.DisableMerge("\t//");
			merger.WriteLine("\telse if(STATE_MATCH(S1,");
			if(null != modInfo.children_names)
			    {
				if(modInfo.children_names.size() > 0)
				    {
					for(int i = 0; i< modInfo.children_names.size() ; i++)
					    {
						merger.WriteLine("\t\t"+((String) modInfo.children_names.elementAt(i))+"_status->status == RCS_DONE &&");
					    }
				    }
			    }
			merger.WriteLine("\t\t1))");
			merger.EnableMerge("\t//");
			merger.WriteLine("\t{");
			merger.WriteLine("\t\tstatus = RCS_DONE;");
			merger.WriteLine("\t\tstateNext(S2);");
			merger.WriteLine("\t}");
			merger.WriteLine("\telse if(STATE_MATCH(S2))");
			merger.WriteLine("\t{");
			merger.WriteLine("\t\t// Idle State");
			merger.WriteLine("\t}");
			merger.WriteLine("}");
		    }

		merger.WriteLine("");// blank lineNumber

		if(have_halt)
		    {
			merger.WriteLine("/*");
			merger.WriteLine("HALT");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("Parameter(s):");
			merger.WriteLine(modNameUpper+"_HALT *cmd_in -- NML Message sent from superior.");
			merger.WriteLine("");// blank lineNumber
			if(!merging_backup)
			    {
				merger.WriteLine(" Most Modules will have an HALT command.");
				merger.WriteLine(" The HALT function is expected to stop any motion or command execution,");
				merger.WriteLine(" send HALT commands to its subordinates,");
				merger.WriteLine(" wait for the subordinates to be DONE and then inform its superior that it is done.");
				merger.WriteLine(" The state tables should use the STATE_MATCH macro so the diagnostics tool can ");
				merger.WriteLine(" highlight the current line in the state table.");
			    }
			merger.WriteLine("*/");
			merger.WriteLine("void "+modInfo.moduleClassName+"::HALT("+modNameUpper+"_HALT *cmd_in)");
			merger.WriteLine("{");
			merger.DisableMerge("\t//");
			if(null != modInfo.children_names)
			    {
				for(int i = 0; i< modInfo.children_names.size(); i++)
				    {
					merger.WriteLine("\t"+((String) modInfo.children_names.elementAt(i)).toUpperCase()+"_HALT "+ ((String) modInfo.children_names.elementAt(i))+"HaltMsg;");
				    }
			    }
			merger.EnableMerge("\t//");
			merger.WriteLine("");// blank lineNumber
			merger.WriteLine("\tif(STATE_MATCH(NEW_COMMAND))");
			merger.WriteLine("\t{");
			merger.WriteLine("\t\t//Send a HALT command to all subordinates.");
			merger.DisableMerge("\t\t//");
			if(null != modInfo.children_names)
			    {
				for(int i = 0; i< modInfo.children_names.size(); i++)
				    {
					merger.WriteLine("\t\tsendCommand(&"+((String) modInfo.children_names.elementAt(i))+"HaltMsg, "+((String) modInfo.children_names.elementAt(i))+"_sub_num);");
				    }
			    }
			merger.EnableMerge("\t\t//");
			merger.WriteLine("\t\tstateNext(S1);");
			merger.WriteLine("\t}");
			merger.WriteLine("\t// Wait for all subordinates to report done.");
			merger.DisableMerge("\t//");
			merger.WriteLine("\telse if(STATE_MATCH(S1,");
			if(null != modInfo.children_names)
			    {
				if(modInfo.children_names.size() > 0)
				    {
					for(int i = 0; i< modInfo.children_names.size() ; i++)
					    {
						merger.WriteLine("\t\t"+((String) modInfo.children_names.elementAt(i))+"_status->status == RCS_DONE &&");
					    }
				    }
			    }
			merger.WriteLine("\t\t1))");
			merger.EnableMerge("\t//");

			merger.WriteLine("\t{");
			merger.WriteLine("\t\tstatus = RCS_DONE;");
			merger.WriteLine("\t\tstateNext(S2);");
			merger.WriteLine("\t}");
			merger.WriteLine("\telse if(STATE_MATCH(S2))");
			merger.WriteLine("\t{");
			merger.WriteLine("\t\t// Idle State");
			merger.WriteLine("\t}");
			merger.WriteLine("}");
		    }
	
		if(null != cmdsVector)
		    {
			for(int i = 0; i< cmdsVector.size(); i++)
			    {
				String cmdName = (String) cmdsVector.elementAt(i);
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				if(cmdName.equals(modNameUpper+"_INIT"))
				    {
					continue;
				    }
				if(cmdName.equals(modNameUpper+"_HALT"))
				    {
					continue;
				    }
				String function_name = cmdName;
				if(function_name.startsWith(modName))
				    {
					function_name = function_name.substring(modName.length());
				    }
				else if(function_name.startsWith(modNameUpper))
				    {
					function_name = function_name.substring(modNameUpper.length());
				    }
				else
				    {
					if(function_name.startsWith("_"))
					    {
						function_name = function_name.substring(1);
					    }
					function_name = "DO_"+function_name;
				    }
				if(function_name.startsWith("_"))
				    {
					function_name = function_name.substring(1);
				    }
				if(!merger.FunctionFoundInExistingFile(modInfo.moduleClassName+"::"+function_name))
				    {
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("/*");
					merger.WriteLine(function_name);
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("Parameter(s):");
					merger.WriteLine(cmdName+" *cmd_in -- NML Message sent from superior.");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("*/");
					merger.WriteLine("void "+modInfo.moduleClassName+"::"+function_name+"("+cmdName+" *cmd_in)");
					merger.WriteLine("{");
					merger.WriteLine("\t// Put state table for "+cmdName+" here.");
					merger.WriteLine("}");
				    }
				else
				    {
					merger.input_subsection_number++;
				    }
			    }
		    }
		if(null != modInfo.cmdsBaseClass)
		    {
			debug_on=true;
			Merger.debug_on=true;
			for(int i = 0; i  < modInfo.cmdsBaseClass.size(); i++)
			    {
				String cmdName = (String) modInfo.cmdsBaseClass.elementAt(i);
				String baseModNameUpper = modInfo.baseModuleName.toUpperCase();
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				String function_name = cmdName;
				if(function_name.startsWith(modInfo.baseModuleName))
				    {
					function_name = function_name.substring(modInfo.baseModuleName.length());
				    }
				if(function_name.startsWith(baseModNameUpper))
				    {
					function_name = function_name.substring(baseModNameUpper.length());
				    }
				if(function_name.startsWith("_"))
				    {
					function_name = function_name.substring(1);
				    }
				if(!merger.FunctionFoundInExistingFile(modInfo.moduleClassName+"::"+function_name))
				    {
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("/*");
					merger.WriteLine(function_name);
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("Parameter(s):");
					merger.WriteLine(cmdName+" *cmd_in -- NML Message sent from superior.");
					merger.WriteLine("");// blank lineNumber
					merger.WriteLine("*/");
					merger.WriteLine("void "+modInfo.moduleClassName+"::"+function_name+"("+cmdName+" *cmd_in)");
					merger.WriteLine("{");
					if(null != modInfo.baseClassName)
					    {
						merger.WriteLine("\t// To customize how this command is handled, delete the call to the base class function and put state table for "+cmdName+" here.");
						if(!merging_backup)
						    {
							merger.WriteLine("\t"+modInfo.baseClassName+"::"+function_name+"(cmd_in);");
						    }
					    }
					merger.WriteLine("}");
				    }
			    }
			debug_on=false;
			Merger.debug_on=false;
		    }


		merger.WriteLine("");// blank lineNumber
		merger.Finish();
		pos.close();
		fos.close();
		pos = null;
		fos = null;
	    }
	catch(rcsDesignUserAbortException rduae)
	    {
		throw rduae;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


}
