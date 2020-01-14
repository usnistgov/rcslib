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


//******************************************************************************// rcsdesign.java:      Applet
//
//*****************************************************************************
package rcsdesign;

import diagapplet.CodeGen.BufferInfo;
import diagapplet.CodeGen.ChannelInfo;
import diagapplet.CodeGen.CodeGen;
import diagapplet.CodeGen.DiagNMLMsgDictCreator;
import diagapplet.CodeGen.ModuleInfo;
import diagapplet.CodeGen.ServerInfo;
import diagapplet.HierarchyPanel;
import diagapplet.utils.CountButton;
import diagapplet.utils.ModifiedFileDialog;
import diagapplet.utils.StandAloneApplet;
import diagapplet.utils.URLLoadInfoPanel;
import java.awt.AWTEvent;
import java.awt.Button;
import java.awt.CardLayout;
import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Choice;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Label;
import java.awt.Panel;
import java.awt.PrintJob;
import java.awt.Rectangle;
import java.awt.Scrollbar;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;
import rcs.utils.SimpleFileFilter;
import rcs.utils.URL_and_FileLoader;



/**
 * Main class for RCS Design tool.
 * @author Will Shackleford
 */
public class rcsDesign extends rcsDesignGui     implements Runnable, ActionListener, ItemListener, TextListener, AdjustmentListener
{
    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613925L;

    // Graphical Controls
    boolean update_all_files=false;
    FlowLayout lm = null;
    Panel innerPanel = null;
    GridBagLayout innerLayout = null;
    CountButton goButton = null;
    Button printButton = null;
    Button makeButton = null;
    Button runButton = null;
    Button importButton = null;
    Button openButton = null;
    Button newButton = null;
    Button diagButton = null;
    Button helpButton = null;
    URLLoadInfoPanel loadPanel = null;
    Panel choicePanel = null;
    CardLayout choicePanelLayout = null;
    Panel optionsPanel = null;
    FlowLayout optionsPanelLayout = null;
    Panel optionsInnerPanel = null;
    GridBagLayout optionsInnerLayout = null;
    String currentViewString = "Hierarchy";
    String prevViewString = "Files";
    String lastFileSelected = null;
    Checkbox debugCheckbox = null;
    Button printHashtablesButton = null;
    boolean reading_config_file=false;
    boolean creating_new_controller=false;

    Panel loopsPanel = null;
    FlowLayout loopsLayout = null;
    Panel loopsInnerPanel = null;
    GridBagLayout loopsInnerLayout = null;

    Panel hierarchyOuterPanel = null;
    FlowLayout hierarchyOuterLayout = null;
    Panel hierarchyInnerPanel = null;
    GridBagLayout hierarchyInnerLayout = null;
    Label modulesLabel = null;
    Button delModuleButton = null;
    Choice CmdOrSubChoice = null;
    Panel CmdOrSubPanel = null;
    CardLayout CmdOrSubLayout = null;
    Panel cmdPanel = null;
    FlowLayout cmdLayout =null;
    Panel cmdInnerPanel = null;
    GridBagLayout cmdInnerLayout = null;
    Panel subPanel = null;
    FlowLayout subLayout = null;
    Label cmdLabel = null;
    java.awt.List cmdList = null;
    Label addcmdLabel = null;
    TextField addcmdField = null;
    Button delcmdButton = null;
    Label subordinatesLabel = null;
    java.awt.List subordinatesList = null;
    Panel auxPanel = null;
    FlowLayout auxLayout = null;
    Panel auxInnerPanel = null;
    GridBagLayout auxInnerLayout = null;
    Label auxLabel = null;
    java.awt.List auxList = null;
    Button delauxButton = null;
    Label addauxLabel = null;
    TextField addauxField = null;
    CheckboxGroup auxTypeGroup = null;
    Checkbox auxInputCheckbox = null;
    Checkbox auxOutputCheckbox = null;
    Checkbox auxUpdateCheckbox = null;

    HierarchyPanel         hierarchyPanel = null;
    Scrollbar hierarchyHorzScrollbar = null;
    Scrollbar hierarchyVertScrollbar = null;
    Panel codegenOuterPanel = null;
    FlowLayout codegenOuterLayout = null;
    Panel codegenInnerPanel = null;
    GridBagLayout codegenInnerLayout = null;
    Panel filesPanel = null;
    FlowLayout filesLayout = null;
    Panel filesInnerPanel = null;
    GridBagLayout filesInnerLayout = null;
    Button saveFileButton = null;
    Button updateFileButton = null;
    Button updateAllFilesButton = null;
    boolean shutting_down=false;



    // Miscellaneous Variables
    rcsDesignWriter writer = new rcsDesignWriter();
    boolean hierarchy_modified = true;
    boolean read_everything_needed = true;
    boolean file_list_needs_to_be_modified = false;
    static int rcsdesign_count = 0;
    String browser_cmd = "mozilla ";
    String default_plat="linux";
    String java_plat="java";

    static final boolean VERSION_CONTROL_STUFF_DISABLED=true;
    static final boolean SHORT_OPTIONS_SCREEN=true;

    static  final int MIN_WIDTH_DEFAULT = 700;
    static  final int MIN_HEIGHT_DEFAULT = 500;
    static  final int MAX_WIDTH_DEFAULT=900;
    static  final int MAX_HEIGHT_DEFAULT=800;

    static  final int FONT_SIZE_DEFAULT=10;
    static  final String FONT_NAME_DEFAULT="Monospaced";

    int font_size = FONT_SIZE_DEFAULT;
    String font_name = FONT_NAME_DEFAULT;

    static int min_width = MIN_WIDTH_DEFAULT;
    static int min_height = MIN_HEIGHT_DEFAULT;
    static int max_width = MAX_WIDTH_DEFAULT;
    static int max_height = MAX_HEIGHT_DEFAULT;

    static public final int MAX_FILE_TEXT_WIDTH_DEFAULT= 100;
    static protected final int MAX_FILE_TEXT_LENGTH_DEFAULT= 50;

    int max_file_text_length = MAX_FILE_TEXT_LENGTH_DEFAULT;
    int max_file_text_width = MAX_FILE_TEXT_WIDTH_DEFAULT;

    static protected final int MIN_FILE_TEXT_PANEL_PIXEL_WIDTH_DEFAULT = 150;
    static protected final int MIN_FILE_TEXT_PANEL_PIXEL_HEIGHT_DEFAULT = 150;
    static protected final int MAX_FILE_TEXT_PANEL_PIXEL_WIDTH_DEFAULT=1400;
    static protected final int MAX_FILE_TEXT_PANEL_PIXEL_HEIGHT_DEFAULT=1400;

    int min_file_text_panel_pixel_width = MIN_FILE_TEXT_PANEL_PIXEL_WIDTH_DEFAULT;
    int min_file_text_panel_pixel_height = MIN_FILE_TEXT_PANEL_PIXEL_HEIGHT_DEFAULT;
    int max_file_text_panel_pixel_height = MAX_FILE_TEXT_PANEL_PIXEL_HEIGHT_DEFAULT;
    int max_file_text_panel_pixel_width = MAX_FILE_TEXT_PANEL_PIXEL_WIDTH_DEFAULT;

    static protected final int MIN_HIERARCHY_PANEL_WIDTH_DEFAULT = 150;
    static protected final int MIN_HIERARCHY_PANEL_HEIGHT_DEFAULT = 150;
    static protected final int MAX_HIERARCHY_PANEL_HEIGHT_DEFAULT=500;
    static protected final int MAX_HIERARCHY_PANEL_WIDTH_DEFAULT=600;

    int min_hierarchy_panel_width = MIN_HIERARCHY_PANEL_WIDTH_DEFAULT;
    int min_hierarchy_panel_height = MIN_HIERARCHY_PANEL_HEIGHT_DEFAULT;
    int max_hierarchy_panel_height = MAX_HIERARCHY_PANEL_HEIGHT_DEFAULT;
    int max_hierarchy_panel_width = MAX_HIERARCHY_PANEL_WIDTH_DEFAULT;

    static protected final int MIN_FILE_PANEL_WIDTH_DEFAULT = 200;
    static protected final int MIN_FILE_PANEL_HEIGHT_DEFAULT = 200;
    static protected final int MAX_FILE_PANEL_HEIGHT_DEFAULT=1400;
    static protected final int MAX_FILE_PANEL_WIDTH_DEFAULT=1400;

    int min_file_panel_width = MIN_FILE_PANEL_WIDTH_DEFAULT;
    int min_file_panel_height = MIN_FILE_PANEL_HEIGHT_DEFAULT;
    int max_file_panel_height = MAX_FILE_PANEL_HEIGHT_DEFAULT;
    int max_file_panel_width = MAX_FILE_PANEL_WIDTH_DEFAULT;

    // PARAMETER SUPPORT:
    //            Parameters allow an HTML author to pass information to the applet;
    // the HTML author specifies them using the <PARAM> tag within the <APPLET>
    // tag.  The following variables are used to store the values of the
    // parameters.
    //--------------------------------------------------------------------------

    // Members for applet parameters
    // <type>       <MemberVar>    = <Default Value>
    //--------------------------------------------------------------------------
    private boolean m_useColor = true;
    private String ImportDir = null;
    private boolean useMerger = true;
    private boolean makeBackups = true;
    private boolean singleDir = true;
    private static final String default_rcs_version_control_directory = "RCS";
    private static final String default_rcs_version_control_checkout_command = "co -l";
    private static final String default_rcs_version_control_checkin_command = "echo . | ci -u";
    private static final String default_sccs_version_control_directory = "SCCS";
    private static final String default_sccs_version_control_checkout_command = "sccs edit";
    private static final String default_sccs_version_control_checkin_command = "sccs delget -y";


    private boolean param_set_version_control_directory = false;
    private boolean param_set_version_control_checkout_command = false;
    private boolean param_set_version_control_checkin_command = false;
    private String execute_in_dir_command = "exec_in_dir $dir $command";
    private static boolean config_file_read_started=false;

    // Parameter names.  To change a name of a parameter, you need only make
    // a single change.  Simply modify the value of the parameter string below.
    //--------------------------------------------------------------------------
    private final String PARAM_ConfigFile = "ConfigFile";
    private final String PARAM_UserDir = "UserDir";
    private final String PARAM_RcsLibDir = "RcsLibDir";
    private final String PARAM_AppDir = "AppDir";
    private final String PARAM_AppName = "AppName";
    private final String PARAM_FinalScript = "FinalScript";
    private final String PARAM_useColor = "useColor";
    private final String PARAM_CppExt = "CppExt";
    private final String PARAM_HppExt = "HppExt";
    private final String PARAM_ObjExt = "ObjExt";
    private final String PARAM_MakeCommand = "MakeCommand";
    private final String PARAM_TerminalCommand = "TerminalCommand";
    private final String PARAM_DevPlat = "DevPlat";
    private final String PARAM_DiagCmd = "DiagCmd";
    private final String PARAM_BrowserCmd = "BrowserCmd";
    private final String PARAM_JavaCmdPrefix = "JavaCmdPrefix";
    private final String PARAM_JavaClasspathSeparator = "JavaClasspathSeparator";
    private final String PARAM_UseJavaInScripts= "UseJavaInScripts";
    private final String PARAM_JavaSetup = "JavaSetup";
    private final String PARAM_JavaSetupFileName = "JavaSetupFileName";
    private final String PARAM_ImportDir = "ImportDir";
    private final String PARAM_UseMerger = "UseMerger";
    private final String PARAM_MakeBackups = "MakeBackups";
    private final String PARAM_SingleDir = "SingleDir";
    private final String PARAM_PLAT = "PLAT";
    private final String PARAM_VCT = "VCT";
    private final String PARAM_CHECKOUT_COMMAND = "CheckOutCommand";
    private final String PARAM_CHECKIN_COMMAND = "CheckInCommand";
    private final String PARAM_SYMLINK_COMMAND = "SymbolicLinkCommand";
    private final String PARAM_VC_DIRECTORY = "VCDirectory";
    private final String PARAM_EXEC_IN_DIR_COMMAND = "ExecInDirCommand";
    private final String PARAM_AUTO_CHECKIN = "AutoCheckin";
    private final String PARAM_AUTO_CHECKOUT = "AutoCheckout";
    private final String PARAM_LIST_MODULES_BY_NUMBER = "ListModulesByNumber";
    private final String PARAM_MAX_HEIGHT = "MaxHeight";
    private final String PARAM_MAX_WIDTH = "MaxWidth";
    private final String PARAM_MIN_HEIGHT = "MinHeight";
    private final String PARAM_MIN_WIDTH = "MinWidth";

    private final String PARAM_FONT_SIZE = "FontSize";
    private final String PARAM_FONT_NAME = "FontName";

    private final String PARAM_MAX_FILE_TEXT_LENGTH = "MaxFileTextLength";
    private final String PARAM_MAX_FILE_TEXT_WIDTH = "MaxFileTextWidth";

    private final String PARAM_MAX_HIERARCHY_PANEL_HEIGHT = "HierarchyPanelMaxHeight";
    private final String PARAM_MAX_HIERARCHY_PANEL_WIDTH = "HierarchyPanelMaxWidth";
    private final String PARAM_MIN_HIERARCHY_PANEL_HEIGHT = "HierarchyPanelMinHeight";
    private final String PARAM_MIN_HIERARCHY_PANEL_WIDTH = "HierarchyPanelMinWidth";

    private final String PARAM_MAX_FILE_TEXT_PANEL_PIXEL_HEIGHT = "FileTextPanelPixelMaxHeight";
    private final String PARAM_MAX_FILE_TEXT_PANEL_PIXEL_WIDTH = "FileTextPanelPixelMaxWidth";
    private final String PARAM_MIN_FILE_TEXT_PANEL_PIXEL_HEIGHT = "FileTextPanelPixelMinHeight";
    private final String PARAM_MIN_FILE_TEXT_PANEL_PIXEL_WIDTH = "FileTextPanelPixelMinWidth";

    String currentTime()
    {
	Date d = new Date();
	DateFormat df = DateFormat.getTimeInstance(DateFormat.SHORT);
	return df.format(d);
    }

    // STANDALONE APPLICATION SUPPORT
    //    The GetParameter() method is a replacement for the getParameter() method
    // defined by Applet. This method returns the value of the specified parameter;
    // unlike the original getParameter() method, this method works when the applet
    // is run as a standalone application, as well as when run within an HTML page.
    // This method is called by GetParameters().
    //---------------------------------------------------------------------------
    String GetParameter(String strName, String args[])
    {
	try
	    {
//		if (args == null)
//		    {
//			String retval = null;
//			// Running within an HTML page, so call original getParameter().
//			//-------------------------------------------------------------------
//			try
//			    {
//				retval = getParameter(strName);
//			    }
//			catch(Exception e)
//			    {
//				e.printStackTrace();
//			    }
//			if(null != retval)
//			    {
//				if(true)
//				    {
//					System.out.println("Setting parameter "+strName+" to "+retval);
//				    }
//			    }
//			else
//			    {
//				if(null != codeGenerationApplet)
//				    {
//					retval = codeGenerationApplet.GetParameter(strName,null);
//				    }
//			    }
//			return retval;
//		    }

		// Running as standalone application, so parameter values are obtained from
		// the command lineNumber. The user specifies them as follows:
		//
		//      JView rcsdesign param1=<val> param2=<"val with spaces"> ...
		//-----------------------------------------------------------------------
		int    i;
		String strArg   = strName + "=";
		String strValue = null;

		for (i = 0; i < args.length; i++)
		    {
			if(null == args[i])
			    {
				continue;
			    }
			if(args[i].length() <= strArg.length())
			    {
				continue;
			    }
			if (strArg.equalsIgnoreCase(args[i].substring(0, strArg.length())))
			    {
				// Found matching parameter on command lineNumber, so extract its value.
				// If in double quotes, remove the quotes.
				//---------------------------------------------------------------
				strValue= args[i].substring(strArg.length());
				if (strValue.startsWith("\""))
				    {
					strValue = strValue.substring(1);
					if (strValue.endsWith("\""))
					    strValue = strValue.substring(0, strValue.length() - 1);
				    }
			    }
		    }
		if(null != strValue)
		    {
			if(true)
			    {
				System.out.println("Setting parameter "+strName+" to "+strValue);
			    }
		    }
		else
		    {
			if(null != codeGenerationApplet)
			    {
				strValue = codeGenerationApplet.GetParameter(strName,null);
			    }
		    }
		return strValue;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return null;
    }

    protected void monitored_repaint()
    {
	try
	    {
		if(debug_on)
		    {
			Thread.dumpStack();
		    }
		repaint();
	    }
	catch(Exception e)
	    {
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
    //---------------------------------------------------------------------------
    void GetParameters(String args[])
    {
	// Query values of all Parameters
	//--------------------------------------------------------------
	String param;

	if(debug_on)
	    {
		System.out.println("GetParameters called.");
	    }

	if(!reading_config_file && !creating_new_controller)
	    {
		// UserDir: Development directory for this user
		//--------------------------------------------------------------
		param = GetParameter(PARAM_UserDir, args);
		//System.out.println("param GetParameter(PARAM_UserDir, args); ="+param);
		if (param != null)
		    {
			writer.setUserDir(param);
			if(File.separator.equals("\\"))
			    {
				writer.setUserDir(writer.getUserDir().replace('/','\\'));
			    }
			else if(File.separator.equals("/"))
			    {
				writer.setUserDir(writer.getUserDir().replace('\\','/'));
			    }
		    }
		else
		    {
			try
			    {
				//Properties props = System.getProperties();
				//props.list(System.out);
				writer.setUserDir(System.getProperty("user.dir"));
				if(debug_on)
				    {
					System.out.println("writer.UserDir = System.getProperty(\"user.dir\") ="+writer.getUserDir());
				    }
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }
		if(null == writer.getUserDir())
		    {
			writer.setUserDir("");
		    }
		if(debug_on)
		    {
			System.out.println("writer.UserDir="+writer.getUserDir());
		    }


		// AppName: Name (ussually a 2 to 4 letter abbreviation) for this application
		//--------------------------------------------------------------
		param = GetParameter(PARAM_AppName, args);
		if (param != null)
		    {
			writer.AppName = param;
			File userDirFile = new File(writer.getUserDir());
			if(!userDirFile.getName().equals(writer.AppName))
			    {
				File newUserDirFile = new File(userDirFile,writer.AppName);
				writer.setUserDir(newUserDirFile.toString());
			    }
			File AppDirFile = new File(writer.AppDir);
			if(!AppDirFile.getName().equals(writer.AppName))
			    {
				File newAppDirFile = new File(AppDirFile,writer.AppName);
				writer.AppDir = newAppDirFile.toString();
			    }
			if(null != appnameField)
			    {
				appnameField.setText(writer.AppName);
			    }
			if(null != appdirField)
			    {
				appdirField.setText(writer.AppDir);
			    }
			if(null != userdirField)
			    {
				userdirField.setText(writer.getUserDir());
			    }
		    }
		else
		    {
			try
			    {
				writer.AppName = new File(writer.getUserDir()).getName();
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }
		if(null == writer.AppName)
		    {
			writer.AppName = "";
		    }

		// ConfigFile: Configuration File For the Project to Extend
		//--------------------------------------------------------------
		param = GetParameter(PARAM_ConfigFile, args);
		if (param != null)
		    {
			writer.ConfigFile = param;
		    }
		else
		    {
			writer.ConfigFile = writer.AppName + ".cfg";
		    }

		if(null != main_window)
		    {
			if(main_window.isVisible())
			    {
				if(null != writer && null != writer.ConfigFile)
				    {
					main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+") -- "+writer.ConfigFile);
				    }
				else
				    {
					main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+")");
				    }
			    }
		    }

		// AppDir: Release directory for this application
		//--------------------------------------------------------------
		param = GetParameter(PARAM_AppDir, args);
		if (param != null)
		    {
			writer.AppDir = param;
			if(null != appdirField)
			    {
				appdirField.setText(writer.AppDir);
			    }
		    }
		else
		    {
			if(null != writer.getUserDir())
			    {
				writer.AppDir = writer.getUserDir();
				if(null != appdirField)
				    {
					appdirField.setText(writer.AppDir);
				    }
			    }
		    }
	    }

	// RcsLibDir: RCS Library Directory
	//--------------------------------------------------------------
	param = GetParameter(PARAM_RcsLibDir, args);
	if (param != null)
	    {
		writer.RcsLibDir = param;
		if(null != rcslibdirField)
		    {
			rcslibdirField.setText(writer.RcsLibDir);
		    }
	    }
	else
	    {
		boolean have_cfg_file =false;
		try
		    {
			if(writer.ConfigFile == null && writer.getUserDir() != null && !writer.getUserDir().equals(""))
			    {
				writer.ConfigFile = (new File(writer.getUserDir())).getName() + ".cfg";
			    }
			if(writer.ConfigFile != null && writer.getUserDir() != null && !writer.getUserDir().equals("") &&
			   (new File(writer.getUserDir()+File.separator + writer.ConfigFile)).exists())
			    {
				have_cfg_file=true;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		try
		    {
			if(!have_cfg_file)
			    {
				if(writer.RcsLibDir != null)
				    {
					File f = new File(writer.RcsLibDir);
					if(!f.exists() || !f.isDirectory())
					    {
						writer.RcsLibDir=null;
					    }
				    }
				param = System.getProperty("user.dir")+File.separator+"rrcslib";
				File f1 = new File(param);
				File f0 = null;
				if(debug_on)
				    {
					System.out.println("Looking for the RCS library directory in "+f1);
				    }
				if(f1.exists() && f1.isDirectory())
				    {
					writer.RcsLibDir = f1.getPath();
					if(null != rcslibdirField)
					    {
						rcslibdirField.setText(writer.RcsLibDir);
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					param = System.getProperty("user.dir")+File.separator+"rcslib-"+rcs.RCS_VERSION.version_string;
					f1 = new File(param);
					f0 = null;
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f0 = new File(System.getProperty("user.dir"));
					f1 = new File(f0.getParent() + File.separator+"rcslib");
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f0 = new File(System.getProperty("user.dir"));
					f1 = new File(f0.getParent() + File.separator+"rcslib-"+rcs.RCS_VERSION.version_string);
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File(System.getProperty("user.home") + File.separator+"rcslib");
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }

				if(writer.RcsLibDir == null)
				    {
					f1 = new File(System.getProperty("user.home") + File.separator+"rcslib-"+rcs.RCS_VERSION.version_string);
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File("/usr/local/rcslib");
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File("/usr/local/rcslib-" + rcs.RCS_VERSION.version_string);
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File("/mxproj/rcslib");
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File("C:\\RCSLIB");
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
				if(writer.RcsLibDir == null)
				    {
					f1 = new File("C:\\rcslib-"+ rcs.RCS_VERSION.version_string);
					if(debug_on)
					    {
						System.out.println("Looking for the RCS library directory in "+f1);
					    }
					if(f1.exists() && f1.isDirectory())
					    {
						writer.RcsLibDir = f1.getPath();
						if(null != rcslibdirField)
						    {
							rcslibdirField.setText(writer.RcsLibDir);
						    }
					    }
				    }
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		if(null == writer.RcsLibDir)
		    {
			writer.RcsLibDir = "/usr/local/rcslib";
		    }
	    }
	System.out.println("writer.RcsLibDir="+writer.RcsLibDir);

	// UseColor: Should the hierarchy panel display color.
	//--------------------------------------------------------------
	param = GetParameter(PARAM_useColor, args);
	if (param != null)
	    {
		try
		    {
			m_useColor = Boolean.valueOf(param).booleanValue();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
	    }

	// MakeCommand: Command to run to build application
	param = GetParameter(PARAM_DevPlat, args);
	if(null != param)
	    {
		writer.DevPlat = param;
		if(writer.DevPlat.toUpperCase().equals("MSWINDOWS"))
		    {
			writer.mswinDevPlat = true;
			writer.cpp_ext = ".cpp";
			writer.hpp_ext = ".hpp";
			writer.obj_ext = ".obj";
			writer.MakeCommand = "NMAKE ";
			writer.TerminalCommand = "cmd /c start";
			writer.java_cmd_prefix = "java";
			default_version_control_type="NONE";
		    }
		else if(writer.DevPlat.toUpperCase().equals("UNIX"))
		    {
			writer.mswinDevPlat = false;
			writer.MakeCommand = "make ";
			writer.TerminalCommand = "xterm -sb -sl 1000 -e ./.ec ";
			writer.cpp_ext = ".cc";
			writer.hpp_ext = ".hh";
			writer.obj_ext = ".o";
			writer.java_cmd_prefix = "java";
			default_version_control_type="NONE";
		    }
		if(debug_on)
		    {
			System.out.println("mswinDevPlat="+writer.mswinDevPlat);
			System.out.println("DevPlat="+writer.DevPlat);
		    }
	    }



	// CppExt: Extension for C++ Source files
	param = GetParameter(PARAM_CppExt, args);
	if (param != null)
	    {
		writer.cpp_ext = param;
		if(null != cppExtField)
		    {
			cppExtField.setText(writer.cpp_ext);
		    }
	    }

	// HppExt: Extension for C++ Header files
	param = GetParameter(PARAM_HppExt, args);
	if (param != null)
	    {
		writer.hpp_ext = param;
		if(null != hppExtField)
		    {
			hppExtField.setText(writer.hpp_ext);
		    }
	    }


	// ObjExt: Extension for C++ Header files
	param = GetParameter(PARAM_ObjExt, args);
	if (param != null)
	    {
		writer.obj_ext = param;
		if(null != objExtField)
		    {
			objExtField.setText(writer.obj_ext);
		    }
	    }

	// RunStart: String to prepend to Make targets to run such as "gnumake" or "NMAKE"
	param = GetParameter(PARAM_MakeCommand, args);
	if (param != null)
	    {
		writer.MakeCommand = param;
		if(null != makeCommandField)
		    {
			makeCommandField.setText(writer.MakeCommand);
		    }
	    }

	// RunStart: String to prepend to commands to run such as "run "
	param = GetParameter(PARAM_TerminalCommand, args);
	if (param != null)
	    {
		writer.TerminalCommand = param;
		if(null != terminalCommandField)
		    {
			terminalCommandField.setText(writer.TerminalCommand);
		    }
	    }

	// DiagCmd: String to prepend to start the RCS Diagnostics Tool
	param = GetParameter(PARAM_DiagCmd, args);
	if (param != null)
	    {
		diag_cmd = param;
		if(null != diagCommandField)
		    {
			diagCommandField.setText(diag_cmd);
		    }
	    }

	// BrowserCmd: String to prepend to start the HTML Web-Browser
	param = GetParameter(PARAM_BrowserCmd, args);
	if (param != null)
	    {
		browser_cmd = param;
		if(null != browserCommandField)
		    {
			browserCommandField.setText(browser_cmd);
		    }
	    }

	// JavaCmdPrefixt: String to prepend to java commands "
	param = GetParameter(PARAM_JavaCmdPrefix, args);
	if (param != null)
	    {
		writer.java_cmd_prefix = param;
		if(null != javaCommandField)
		    {
			javaCommandField.setText(writer.java_cmd_prefix);
		    }
	    }

	// UseJavaInScripts to enable/disable the use of java in generated scripts and makefiles.
	param = GetParameter(PARAM_UseJavaInScripts, args);
	if(param != null)
	    {
		try
		    {
			writer.useJavaInScripts = Boolean.valueOf(param).booleanValue();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
	    }

	// Extra commands needed to set LD_LIBRARY_PATH, CLASSPATH, PATH etc before java can be run.
	param = GetParameter(PARAM_JavaSetup, args);
	if(param != null)
	    writer.java_setup = param;


	// Extra commands needed to set LD_LIBRARY_PATH, CLASSPATH, PATH etc before java can be run.
	param = GetParameter(PARAM_JavaSetupFileName, args);
	if(param != null)
	    {
		writer.java_setup_file_name = param;
		String java_setup_orig = writer.java_setup;
		try
		    {
			URL_and_FileLoader loader = new URL_and_FileLoader(param);
			String setup_line = null;
			while(null != (setup_line = loader.readLine()))
			    {
				writer.java_setup += setup_line+"\n";
			    }
		    }
		catch(Exception e)
		    {
			writer.java_setup = java_setup_orig;
			e.printStackTrace();
		    }
	    }

	param = GetParameter(PARAM_ImportDir, args);
	if(param != null)
	    ImportDir = param;

	param = GetParameter(PARAM_UseMerger, args);
	if(param != null)
	    {
		useMerger = Boolean.valueOf(param).booleanValue();
		if(null != useMergerCheckbox)
		    {
			useMergerCheckbox.setState(useMerger);
		    }
	    }


	param = GetParameter(PARAM_MakeBackups, args);
	if(param != null)
	    {
		makeBackups = Boolean.valueOf(param).booleanValue();
		if(null != makeBackupsCheckbox)
		    {
			makeBackupsCheckbox.setState(useMerger);
		    }
	    }


	param = GetParameter(PARAM_SingleDir, args);
	if(param != null)
	    {
		singleDir = Boolean.valueOf(param).booleanValue();
		if(null != singleDirCheckbox)
		    {
			singleDirCheckbox.setState(useMerger);
		    }
	    }

	param = GetParameter(PARAM_PLAT, args);
	if(null != param)
	    {
		default_plat = param;
	    }

	param = GetParameter(PARAM_VCT, args);
	if(null != param)
	    {
		default_version_control_type = param;
	    }

	param = GetParameter(PARAM_VC_DIRECTORY, args);
	if(null != param)
	    {
		param_set_version_control_directory = true;
		default_version_control_directory = param;
	    }

	param = GetParameter(PARAM_CHECKOUT_COMMAND, args);
	if(null != param)
	    {
		param_set_version_control_checkout_command = true;
		default_version_control_checkout_command = param;
	    }

	param = GetParameter(PARAM_CHECKIN_COMMAND, args);
	if(null != param)
	    {
		param_set_version_control_checkin_command = true;
		default_version_control_checkin_command = param;
	    }

	param = GetParameter(PARAM_SYMLINK_COMMAND, args);
	if(null != param)
	    {
		default_version_control_symlink_command = param;
	    }


	param = GetParameter(PARAM_EXEC_IN_DIR_COMMAND, args);
	if(null != param)
	    {
		execute_in_dir_command  = param;
	    }

	param = GetParameter(PARAM_AUTO_CHECKIN, args);
	if(null != param)
	    {
		auto_checkin  = Boolean.valueOf(param).booleanValue();
	    }

	param = GetParameter(PARAM_AUTO_CHECKOUT, args);
	if(null != param)
	    {
		auto_checkout  = Boolean.valueOf(param).booleanValue();
	    }

	param = GetParameter(PARAM_LIST_MODULES_BY_NUMBER, args);
	if(null != param)
	    {
		list_modules_by_number = Boolean.valueOf(param).booleanValue();
	    }

	param = GetParameter("MODULE_WIDTH", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_WIDTH = Integer.valueOf(param).intValue();
	    }

	param = GetParameter("MODULE_HEIGHT", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_HEIGHT = Integer.valueOf(param).intValue();
	    }

	param = GetParameter("MODULE_XOFFSET", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_XOFFSET = Integer.valueOf(param).intValue();
	    }

	param = GetParameter("MODULE_YOFFSET", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_YOFFSET = Integer.valueOf(param).intValue();
	    }

	param = GetParameter("MODULE_X_SPACING", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_X_SPACING = Integer.valueOf(param).intValue();
	    }

	param = GetParameter("MODULE_Y_SPACING", args);
	if(null != param)
	    {
		HierarchyPanel.MODULE_Y_SPACING = Integer.valueOf(param).intValue();
	    }


	param = GetParameter("MIN_WIDTH", args);
	if(null != param)
	    {
		min_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter("MIN_HEIGHT", args);
	if(null != param)
	    {
		min_height = Integer.valueOf(param).intValue();
	    }
	param = GetParameter("MAX_WIDTH", args);
	if(null != param)
	    {
		max_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter("MAX_HEIGHT", args);
	if(null != param)
	    {
		max_height = Integer.valueOf(param).intValue();
	    }





	param = GetParameter(PARAM_MAX_WIDTH, args);
	if(null != param)
	    {
		max_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MAX_HEIGHT, args);
	if(null != param)
	    {
		max_height = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_WIDTH, args);
	if(null != param)
	    {
		min_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_HEIGHT,  args);
	if(null != param)
	    {
		min_height = Integer.valueOf(param).intValue();
	    }


	param = GetParameter(PARAM_MAX_HIERARCHY_PANEL_WIDTH, args);
	if(null != param)
	    {
		max_hierarchy_panel_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MAX_HIERARCHY_PANEL_HEIGHT, args);
	if(null != param)
	    {
		max_hierarchy_panel_height = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_HIERARCHY_PANEL_WIDTH, args);
	if(null != param)
	    {
		min_hierarchy_panel_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_HIERARCHY_PANEL_HEIGHT,  args);
	if(null != param)
	    {
		min_hierarchy_panel_height = Integer.valueOf(param).intValue();
	    }



	param = GetParameter(PARAM_MAX_FILE_TEXT_PANEL_PIXEL_WIDTH, args);
	if(null != param)
	    {
		max_file_text_panel_pixel_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MAX_FILE_TEXT_PANEL_PIXEL_HEIGHT, args);
	if(null != param)
	    {
		max_file_text_panel_pixel_height = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_FILE_TEXT_PANEL_PIXEL_WIDTH, args);
	if(null != param)
	    {
		min_file_text_panel_pixel_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MIN_FILE_TEXT_PANEL_PIXEL_HEIGHT,  args);
	if(null != param)
	    {
		min_file_text_panel_pixel_height = Integer.valueOf(param).intValue();
	    }



	param = GetParameter(PARAM_MAX_FILE_TEXT_WIDTH, args);
	if(null != param)
	    {
		max_file_text_width = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_MAX_FILE_TEXT_LENGTH, args);
	if(null != param)
	    {
		max_file_text_length = Integer.valueOf(param).intValue();
	    }


	param = GetParameter(PARAM_FONT_SIZE, args);
	if(null != param)
	    {
		font_size = Integer.valueOf(param).intValue();
	    }
	param = GetParameter(PARAM_FONT_NAME, args);
	if(null != param)
	    {
		font_name = param;
	    }

	param = GetParameter("JAVA_PLAT",args);
	if(param != null)
	    {
		java_plat = param;
		if(null != writer)
		    {
			writer.java_plat = param;
		    }
	    }
    }

    @SuppressWarnings("unchecked")
    protected void AddToBufferList(String name)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsDesign.AddToBufferList("+name+")");
			writer.PrintBuffersHashtable();
		    }
		if(null == name || null == bufsInServerList || null == writer)
		    {
			return;
		    }
		if(null == writer.buffersHashtable)
		    {
			return;
		    }
		if(!writer.buffersHashtable.containsKey(name))
		    {
			BufferInfo bi = new BufferInfo();
			bi.Name = name;
			writer.buffersHashtable.put(name, bi);
		    }
		boolean bufsListContainsItem = false;
		for(int i = 0; i < bufsInServerList.getItemCount(); i++)
		    {
			String tempbuf = bufsInServerList.getItem(i);
			if(tempbuf.equals(name))
			    {
				bufsListContainsItem = true;
				break;
			    }
		    }
		if(!bufsListContainsItem)
		    {
			bufsInServerList.add(name);
		    }
		boolean server_found = false;
		for(int i = 0; i < serverList.getItemCount(); i++)
		    {
			String svrName = serverList.getItem(i);
			if(debug_on)
			    {
				System.out.println(" String svrName = serverList.getItem("+i+"); = "+svrName);
			    }
			ServerInfo si = (ServerInfo) writer.serversHashtable.get(svrName);
			if(debug_on)
			    {
				System.out.println("ServerInfo si = writer.serversHashtable.get("+svrName+"); = "+si);
			    }
			if(null == si)
			    {
				continue;
			    }
			if(null == si.bufferNames)
			    {
				continue;
			    }
			for(int j = 0; j < si.bufferNames.size(); j++)
			    {
				String bufname =  (String) si.bufferNames.elementAt(j);
				if(debug_on)
				    {
					System.out.println("String bufname = si.bufferNames.elementAt("+j+"); = "+bufname);
				    }
				if(bufname.equals(name))
				    {
					server_found = true;
					break;
				    }
			    }
			if(server_found)
			    {
				break;
			    }
		    }
		if(debug_on)
		    {
			System.out.println("server_found ="+server_found);
		    }
		if(!server_found)
		    {
			for(int i = 0; i < serverList.getItemCount(); i++)
			    {
				String svrName = serverList.getItem(i);
				if(debug_on)
				    {
					System.out.println(" String svrName = serverList.getItem("+i+"); = "+svrName);
				    }
				if(svrName.startsWith(writer.AppName))
				    {
					ServerInfo si = (ServerInfo) writer.serversHashtable.get(svrName);
					if(debug_on)
					    {
						System.out.println("ServerInfo si = writer.serversHashtable.get("+svrName+"); = "+si);
					    }
					if(null != si)
					    {
						if(null != si.bufferNames)
						    {
							si.bufferNames.addElement(name);
						    }
					    }
				    }
			    }
		    }

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    protected void CreateBufferList()
    {
	if(debug_on)
	    {
		System.out.println("");
		System.out.println("rcsDesign.CreateBufferList:");
		System.out.println("bufsInServerList ="+bufsInServerList);
	    }
	if(null == bufsInServerList)
	    {
		return;
	    }
	try
	    {
		bufsInServerList.removeAll();
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			String modName = modulesList.getItem(i);
			AddToBufferList(modName+"_cmd");
			AddToBufferList(modName+"_sts");
		    }
		AddToBufferList("errlog");
		if(null != writer.auxChannelsVector)
		    {
			for(int i = 0; i < writer.auxChannelsVector.size(); i++)
			    {
				String auxName = (String) writer.auxChannelsVector.elementAt(i);
				AddToBufferList(auxName);
			    }
		    }
		String selected_server = serverList.getSelectedItem();
		if(debug_on)
		    {
			System.out.println("String selected_server = serverList.getSelectedItem(); ="+selected_server);
		    }
		if(null != selected_server)
		    {
			writer.curServer = (ServerInfo) writer.serversHashtable.get(selected_server);
			if(debug_on)
			    {
				System.out.println("writer.curServer = (ServerInfo) writer.serversHashtable.get("+selected_server+"); ="+writer.curServer);
			    }
			int buf_indexes[] = bufsInServerList.getSelectedIndexes();
			if(debug_on)
			    {
				System.out.println(" int buf_indexes[] = bufsInServerList.getSelectedIndexes(); = "+buf_indexes);
			    }
			for(int i = 0; i < buf_indexes.length; i++)
			    {
				bufsInServerList.deselect(buf_indexes[i]);
				if(debug_on)
				    {
					System.out.println("bufsInServerList.deselect(buf_indexes["+i+"]="+buf_indexes[i]+");");
				    }
			    }
			if(null != writer.curServer)
			    {
				ServerInfo si = writer.curServer;
				if(debug_on)
				    {
					System.out.println("ServerInfo si = writer.curServer; = "+si);
				    }
				for(int i = 0; i < si.bufferNames.size(); i++)
				    {
					String bufname = (String) si.bufferNames.elementAt(i);
					if(debug_on)
					    {
						System.out.println(" String bufname = (String) si.bufferNames.elementAt("+i+"); = "+bufname);
					    }
					for(int j = 0; j < bufsInServerList.getItemCount(); j++)
					    {
						String temp = bufsInServerList.getItem(j);
						if(debug_on)
						    {
							System.out.println("String temp = bufsInServerList.getItem("+j+"); ="+temp);
						    }
						if(temp.equals(bufname))
						    {
							BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(bufname);
							if(debug_on)
							    {
								System.out.println("BufferInfo bi = (BufferInfo) writer.buffersHashtable.get("+bufname+"); ="+bi);
							    }
							if(null != bi)
							    {
								bi.si = si;
							    }
							bufsInServerList.select(j);
							if(debug_on)
							    {
								System.out.println("bufsInServerList.select("+j+");");
							    }
							break;
						    }
					    }
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void CheckOutFile(File f)
    {
        try
            {
                if(null == writer)
                    {
                        return;
                    }
                if(writer.mswinDevPlat)
                    {
                        return;
                    }
                if(null == default_version_control_type)
                    {
                        return;
                    }
                if(default_version_control_type.equalsIgnoreCase("NONE"))
                    {
                        return;
                    }
                String cmd = checkOutCommandField.getText() +" "+f.getName();
                String dir = f.getParent();
                File dirFile = new File(dir);
                File csFile = new File(dirFile,fileCheckinDirectoryField.getText());
                if(!csFile.exists())
                    {
                        return;
                    }
                ExecuteInDirectory(dir,cmd);
            }
        catch(Exception e)
            {
                e.printStackTrace();
            }
    }

    public void CheckInFile(File f)
    {
        try
            {
                if(null == writer)
                    {
                        return;
                    }
                if(writer.mswinDevPlat)
                    {
                        return;
                    }
                if(null == default_version_control_type)
                    {
                        return;
                    }
                if(default_version_control_type.equalsIgnoreCase("NONE"))
                    {
                        return;
                    }
                if(default_version_control_type.equals("SCCS"))
                    {
                        File pfile = new File(f.getParent() + File.separator+"SCCS"+File.separator+"p."+f.getName());
                        File sfile = new File(f.getParent() + File.separator+"SCCS"+File.separator+"p."+f.getName());
                        if(!pfile.exists() && ! sfile.exists())
                            {
                                String cmd = "sccs create "+f.getName();
                                String dir = f.getParent();
                                ExecuteInDirectory(dir, cmd);
                                return;
                            }
                    }
                String cmd = checkInCommandField.getText() +" "+f.getName();
                String dir = f.getParent();
                File dirFile = new File(dir);
                File csFile = new File(dirFile,fileCheckinDirectoryField.getText());
                if(!csFile.exists())
                    {
                        return;
                    }
                ExecuteInDirectory(dir,cmd);
            }
        catch(Exception e)
            {
                e.printStackTrace();
            }
    }

    protected void ExecuteInDirectory(String directory, String command)
    {
	String cmd = null;
	try
	    {
		directory = " "+directory+" ";
		command = " "+command+ " ";
		cmd = execute_in_dir_command;
		cmd = ReplaceVar(cmd, "$dir",directory);
		cmd = ReplaceVar(cmd, "$(DIR)",directory);
		cmd = ReplaceVar(cmd, "%DIR%",directory);
		cmd = ReplaceVar(cmd, "$command",command);
		cmd = ReplaceVar(cmd, "$(COMMAND)",command);
		cmd = ReplaceVar(cmd, "%COMMAND%",command);
		Runtime rt = Runtime.getRuntime();
		if(debug_on)
		    {
			System.out.println("Exectuting cmd="+cmd);
		    }
		Process p = rt.exec(cmd);
		p.waitFor();
	    }
	catch(Exception e)
	    {
		System.err.println("directory="+directory+", command="+command+", cmd = "+cmd);
		e.printStackTrace();
	    }
    }
    static protected String orig_args[];

    static protected Frame main_window = null;

    // STANDALONE APPLICATION SUPPORT
    //    The main() method acts as the applet's entry point when it is run
    // as a standalone application. It is ignored if the applet is run from
    // within an HTML page.
    //--------------------------------------------------------------------------
    public static void main(String args[])
    {
	for(int i = 0; i < args.length; i++)
	    {
		if((args[i].toUpperCase().indexOf("DEBUG=TRUE") >= 0) ||
		   (args[i].toUpperCase().indexOf("DEBUG_ON=TRUE") >= 0) ||
		   (args[i].toUpperCase().indexOf("DEBUGON=TRUE") >= 0))
		    {
			debug_on=true;
			Merger.debug_on = true;
			rcsdesignFrame.debug_on = true;
			StandAloneApplet.debug_on = true;
//			CodeGen.debug_on=true;
			ModuleInfo.debug_on=true;
			HierarchyPanel.debug_on = true;
			QueryDialog.debug_on = true;
			AlertDialog.debug_on=true;
			URLLoadInfoPanel.debug_on=true;
			URL_and_FileLoader.debug_on=true;
			break;
		    }
	    }

	int frame_width = 775;
	int frame_height = 575;

	orig_args = args;
	// Create Toplevel Window to contain applet rcsdesign
	//----------------------------------------------------------------------
//	if(debug_on)
//	    {
//		diagapplet.CodeGen.CodeGen.debug_on=true;
//	    }
	if(debug_on)
	    {
		System.out.println("Create new rcsdesignFrame object.");
	    }
	rcsdesignFrame frame = new rcsdesignFrame("RCS Design("+rcs.RCS_VERSION.version_string+")");
	boolean orig_diagapplet_utils_URLLoadInfoPanel_ignore_repaint_requests
	    = diagapplet.utils.URLLoadInfoPanel.ignore_repaint_requests;
	diagapplet.utils.URLLoadInfoPanel.ignore_repaint_requests=true;
	main_window = frame;
	if(debug_on)
	    {
		System.out.println("Create new rcsDesign object.");
	    }
	rcsDesign applet_rcsdesign = new rcsDesign();
	frame.add("Center",applet_rcsdesign);
	frame.innerApplet = applet_rcsdesign;
	applet_rcsdesign.m_fStandAlone = true;
	//	applet_rcsdesign.setSize(min_width, min_height);
	applet_rcsdesign.GetParameters(args);
	applet_rcsdesign.init();
	frame.pack();
	rcsdesignFrame.force_it=true;
	frame.resizeInnerApplet();
	applet_rcsdesign.start();
	frame.pack();
	int old_read_config_count = read_config_count;
	config_file_read_started=false;
	applet_rcsdesign.StartConfigFileRead();
	frame.pack();
	if(config_file_read_started)
	    {
		for(int tries=0; tries < 500; tries++)
		    {
			if(read_config_count != old_read_config_count)
			    {
				break;
			    }
			try
			    {
				Thread.sleep(500);
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
			switch(tries%4)
			    {
			    case 0:
				System.out.print("Please wait  -  ");
				break;

			    case 1:
				System.out.print("Please wait  \\  ");
				break;

			    case 2:
				System.out.print("Please wait  |  ");
				break;

			    case 3:
				System.out.print("Please wait  /  ");
				break;

			    }
			if(null != applet_rcsdesign.loadPanel)
			    {
				if(applet_rcsdesign.loadPanel.bytes_read > 0 && applet_rcsdesign.loadPanel.content_length > 0)
				    {
					System.out.print(" loading "+applet_rcsdesign.loadPanel.URLname+"  ("+applet_rcsdesign.loadPanel.bytes_read+"/"+applet_rcsdesign.loadPanel.content_length+ ") "+ 
							 (100*applet_rcsdesign.loadPanel.bytes_read/applet_rcsdesign.loadPanel.content_length) + "%");
				    }
			    }
			System.out.print("                                        \r");
		    }
		System.out.println("");
	    }
	diagapplet.utils.URLLoadInfoPanel.ignore_repaint_requests=
	    orig_diagapplet_utils_URLLoadInfoPanel_ignore_repaint_requests;
	frame.pack();
	frame.setVisible(true);
    }

    protected void UpdateWriter()
    {
	try
	    {
		writer.AppName = appnameField.getText();
		writer.AppDir = appdirField.getText();
		writer.RcsLibDir = rcslibdirField.getText();
		if(writer.getUserDir() == null || writer.getUserDir().equals("") || !userdirField.getText().equals(""))
		    {
			writer.setUserDir(userdirField.getText());
		    }
		writer.TerminalCommand = terminalCommandField.getText();
		writer.MakeCommand = makeCommandField.getText();
		if(null != javaSetupArea)
		    {
			writer.java_setup = javaSetupArea.getText();
		    }
		writer.cpp_ext = cppExtField.getText();
		writer.hpp_ext = hppExtField.getText();
		writer.obj_ext = objExtField.getText();
		writer.useJavaInScripts = useJavaInScriptsCheckbox.getState();
		writer.mswinDevPlat = !unixCheckbox.getState();
		writer.useMerger = useMergerCheckbox.getState();
		writer.makeBackups = makeBackupsCheckbox.getState();
		writer.singleDir = singleDirCheckbox.getState();
		UpdateMainLoopInfo();
		for(int i = 0; i < mainLoopList.getItemCount(); i++)
		    {
			String loopname = mainLoopList.getItem(i);
			rcsdesignMainLoopInfo mli =  (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(loopname);
			if(null != mli)
			    {
				if(null != mli.getModules())
				    {
					for(int j = 0; j < mli.getModules().size(); j++)
					    {
						String modname = (String) mli.getModules().elementAt(j);
						ModuleInfo mi = (ModuleInfo) writer.modulesHashtable.get(modname);
						if(null != mi)
						    {
							mi.MainLoopName = loopname;
							if(null != mli.host)
							    {
								mi.host = mli.host;
							    }
							mi.cycle_time = mli.cycle_time;
						    }
					    }
				    }
			    }
		    }
		for(int i = 0; i < serverList.getItemCount(); i++)
		    {
			String svrname = serverList.getItem(i);
			ServerInfo si =  (ServerInfo) writer.serversHashtable.get(svrname);
			if(null != si)
			    {
				if(null != si.bufferNames)
				    {
					for(int j = 0; j < si.bufferNames.size(); j++)
					    {
						String bufname = (String) si.bufferNames.elementAt(j);
						BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(bufname);
						if(null == bi)
						    {
							AddToBufferList(bufname);
							bi = (BufferInfo) writer.buffersHashtable.get(bufname);
						    }
						bi.si = si;
					    }
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    static
    {
	ModuleInfo.application_type = ModuleInfo.RCS_DESIGN_APPLICATION_TYPE;
    }

    // rcsdesign Class Constructor
    //--------------------------------------------------------------------------
    public rcsDesign()
    {
	System.out.print(getAppletInfo());
	ModuleInfo.application_type = ModuleInfo.RCS_DESIGN_APPLICATION_TYPE;
	writer.gui = this;
	rcsdesign_count++;
	if(File.separator.equals("\\"))
	    {
		default_plat = "win32msc";
		writer.mswinDevPlat = true;
		writer.cpp_ext = ".cpp";
		writer.hpp_ext = ".hpp";
		writer.obj_ext = ".obj";
		writer.TerminalCommand = "cmd /c start";
		writer.MakeCommand = "NMAKE ";
		writer.java_cmd_prefix = "java -classpath %CLASSPATH% ";
		writer.RcsLibDir = "\\rcslib";
	    }
	else
	    {
		default_plat = "linux";
		writer.mswinDevPlat = false;
		writer.cpp_ext = ".cc";
		writer.hpp_ext = ".hh";
		writer.obj_ext = ".o";
		writer.TerminalCommand = "xterm -sb -sl 1000 -e ./.ec ";
		writer.MakeCommand = "make ";
		writer.RcsLibDir = "/usr/local/rcslib";

	    }
	getDefaultPlat();
    }

    protected String getDefaultPlat()
    {
	try
	    {
		String os_name = System.getProperty("os.name");
		if(debug_on)
		    {
			System.out.println("os_name = "+os_name);
		    }
		if(null != os_name)
		    {
			if(os_name.toUpperCase().indexOf("SUN") >= 0)
			    {
				default_plat = "sunos5";
			    }
			else if(os_name.toUpperCase().indexOf("LINUX") >= 0)
			    {
				default_plat = "linux";
			    }
			else if(os_name.toUpperCase().indexOf("IRIX") >= 0 || os_name.toUpperCase().indexOf("SGI") >= 0 )
			    {
				default_plat = "irix6";
			    }
			else if(os_name.toUpperCase().indexOf("WIN") >= 0)
			    {
				default_plat = "win32msc";
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return default_plat;
    }

    void setup_version_control_info()
    {
	try
	    {
		if(default_version_control_type != null)
		    {
			if(default_version_control_type.equals("RCS"))
			    {
				if(!param_set_version_control_checkout_command)
				    {
					default_version_control_checkout_command = default_rcs_version_control_checkout_command;
				    }
				if(!param_set_version_control_checkin_command)
				    {
					default_version_control_checkin_command = default_rcs_version_control_checkin_command;
				    }
				if(!param_set_version_control_directory)
				    {
					default_version_control_directory = default_rcs_version_control_directory;
				    }
			    }
			else if(default_version_control_type.equals("SCCS"))
			    {
				if(!param_set_version_control_checkout_command)
				    {
					default_version_control_checkout_command = default_sccs_version_control_checkout_command;
				    }
				if(!param_set_version_control_checkin_command)
				    {
					default_version_control_checkin_command = default_sccs_version_control_checkin_command;
				    }
				if(!param_set_version_control_directory)
				    {
					default_version_control_directory = default_sccs_version_control_directory;
				    }
			    }
			else
			    {
				if(!param_set_version_control_checkout_command)
				    {
					default_version_control_checkout_command = "";
				    }
				if(!param_set_version_control_checkin_command)
				    {
					default_version_control_checkin_command = "";
				    }
				if(!param_set_version_control_directory)
				    {
					default_version_control_directory = "";
				    }
				auto_checkin = false;
				auto_checkout = false;
			    }
		    }
		if(default_version_control_type != null && fileCheckinTypeChoice != null)
		    {
			fileCheckinTypeChoice.select(default_version_control_type);
		    }
		if(default_version_control_directory != null && fileCheckinDirectoryField != null)
		    {
			fileCheckinDirectoryField.setText(default_version_control_directory);
		    }
		if(default_version_control_checkout_command != null && checkOutCommandField != null)
		    {
			checkOutCommandField.setText(default_version_control_checkout_command);
		    }
		if(default_version_control_checkin_command != null && checkInCommandField != null)
		    {
			checkInCommandField.setText(default_version_control_checkin_command);
		    }
		if(default_version_control_symlink_command != null && symLinkCommandField != null)
		    {
			symLinkCommandField.setText(default_version_control_symlink_command);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    // APPLET INFO SUPPORT:
    //            The getAppletInfo() method returns a string describing the applet's
    // author, copyright date, or miscellaneous information.
    //--------------------------------------------------------------------------
    public String getAppletInfo()
    {
	return  "******************************************************************** \r\n"+
	    "* RCS-Design \r\n" +
	    "* \r\n" +
	    "* Graphical Tool for arranging RCS applications and \r\n"+
	    "* generating code.\r\n" +
	    "* \r\n" +
	    "* $Id: rcsDesign.java 2118 2013-05-23 14:11:54Z shackle $ \r\n"+
	    "* \r\n" +
	    "* "+rcs.RCS_VERSION.info_string+" \r\n"+
	    "* \r\n" +
	    "* This software was produced by the National Institute of \r\n"+
	    "* Standards and Technology(NIST), an agency of the U.S. government,\r\n"+
	    "* and by statute is not subject to copyright in the United\r\n" +
	    "* States. Recipients of this software assume all responsibility \r\n"+
	    "* associated with its operation, modification, maintenance, and\r\n"+
	    "* subsequent redistribution. \r\n" +
	    "* \r\n" +
	    "* Please contact Will Shackleford \r\n"+
	    "* ( email: shackle@cme.nist.gov, phone: (301) 975-4286) \r\n"+
	    "* if you have any questions, comments or problems related \r\n"+
	    "* to this software. \r\n"+
	    "******************************************************************** \r\n"+
	    "\r\n";
    }

    // PARAMETER SUPPORT
    //            The getParameterInfo() method returns an array of strings describing
    // the parameters understood by this applet.
    //
    // rcsdesign Parameter Information:
    //  { "Name", "Type", "Description" },
    //--------------------------------------------------------------------------
    public String[][] getParameterInfo()
    {
	String[][] info =
	    {
		{ PARAM_ConfigFile, "String", "Configuration File For the Project to Extend" },
		{ PARAM_UserDir, "String", "Development directory for this user" },
		{ PARAM_AppDir, "String", " Release directory for this application" },
		{ PARAM_AppName, "String", "Name (ussually a 2 to 4 letter abbreviation) for this application" },
		{ PARAM_RcsLibDir, "String", "RCS Library Directory" },
		{ PARAM_useColor, "boolean", "Should the hierarchy panel display color?" },
		{ PARAM_FinalScript, "String", "Script to run at the end of code generation." },
		{ PARAM_CppExt, "String", "C++ Source File Extension" },
		{ PARAM_HppExt, "String", "C++ Header File Extension" },
		{ PARAM_ObjExt, "String", "Object File Extension" },
		{ PARAM_MakeCommand, "String", "Command to run to build application." },
		{ PARAM_DevPlat, "String", "Development Platform (UNIX of MSWINDOWS)" },
		{ PARAM_UseJavaInScripts, "boolean", "Enable/Disable the use of java in generated scripts." },
		{ PARAM_JavaSetup, "String", "Extra commands needed to set LD_LIBRARY_PATH, path, CLASSPATH ect before java can be executed." },
		{ PARAM_ImportDir, "String", "Directory to look for controllers to import." },
		{ PARAM_UseMerger, "boolean", "Use the merger to merge changes with previous files." },
		{ PARAM_MakeBackups, "boolean", "Make backups of old files before making changes." },
		{ PARAM_SingleDir, "boolean", "Use a single directory instead of multiple subdirectories allowing simpler makefiles and scripts." },
		{ PARAM_PLAT, "String", "Platform to build application for. (ex. sunos5, vxworks5.3, win32msc . . .)" },
		{ PARAM_VCT, "String", "Version Control Type (ex. RCS, SCCS,  . . .)" },
		{ PARAM_VC_DIRECTORY, "String", "Sub-directory used to store version control info." },
		{ PARAM_CHECKOUT_COMMAND, "String", "Command used to check a file out of the version control system" },
		{ PARAM_CHECKIN_COMMAND, "String", "Command used to check a file in to the version control system" },
		{ PARAM_AUTO_CHECKIN, "boolean", "Should the tool automatically checkin files to the version control system after modifying the file." },
		{ PARAM_AUTO_CHECKOUT, "boolean", "Should the tool automatically check out files it needs to modify if they are checked in to a version control system." },
	    };
	return info;
    }

    private void CheckForColor()
    {
	try
	    {
		java.awt.Toolkit tk =   java.awt.Toolkit.getDefaultToolkit();
		if(tk == null)
		    {
			return;
		    }
		java.awt.image.ColorModel cm = tk.getColorModel();
		if(cm == null)
		    {
			return;
		    }
		if(cm.getPixelSize() < 2)
		    {
			m_useColor = false;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void SubComponentSetFont(Component c)
    {
	try
	    {
		c.setFont(preferredFont);
	    }
	catch(Exception e)
	    {
	    }
    }

    boolean first_init = true;

    Font preferredFont = null;

    // The init() method is called by the AWT when an applet is first loaded or
    // reloaded.  Override this method to perform whatever initialization your
    // applet needs, such as initializing data structures, loading images or
    // fonts, creating frame windows, setting the layout manager, or adding UI
    // components.
    //--------------------------------------------------------------------------
    @SuppressWarnings("unchecked")
    public void init()
    {
	if(debug_on)
	    {
		Thread.dumpStack();
	    }

	try
	    {
		inside_init = true;
		initialized = false;
		last_size = getSize();
		if(debug_on)
		    {
			System.out.println("Initializing RCS-Design (size = "+last_size.width+"x"+last_size.height+")");
		    }
		ModuleInfo.read_nml_configs = false;
		if (!m_fStandAlone)
		    GetParameters(null);


		// If you use a ResourceWizard-generated "control creator" class to
		// arrange controls in your applet, you may want to call its
		// CreateControls() method from within this method. Remove the following
		// call to resize() before adding the call to CreateControls();
		// CreateControls() does its own resizing.
		//----------------------------------------------------------------------
		int text_length = 40;
		int text_width = 80;
		int tries = 0;
		Dimension d = getSize();
		if(resizing)
		    {
			while(tries < 20 &&
			      (d.width != resize_width || d.height != resize_height))
			    {
				Thread.sleep(50);
				d = getSize();
				setSize(resize_width, resize_height);
				if(debug_on)
				    {
					System.out.println("d = size(); = "+d);
				    }
				tries++;
			    }
		    }
		else
		    {
			while(tries < 20 &&
			      (d.width < min_width || d.height < min_height))
			    {
				Thread.sleep(50);
				d = getSize();
				if(debug_on)
				    {
					System.out.println("d = size(); = "+d);
				    }
				tries++;
			    }
			if(tries >= 20)
			    {
				tries = 0;
				while(tries < 20 &&
				      (d.width < min_width || d.height < min_height))
				    {
					Thread.sleep(50);
					d = getSize();
					if(debug_on)
					    {
						System.out.println("d = size(); = "+d);
					    }
					setSize(min_width, min_height);
					tries++;
				    }
			    }
		    }

		if(d.width > max_width)
		    {
			d.width = max_width;
		    }
		if(d.height > max_height)
		    {
			d.height = max_height;
		    }
		if(tries >= 20)
		    {
			System.err.println("Timed out waiting for valid size. d="+d.width+"x"+d.height);
		    }

		try
		    {
			if(null == preferredFont)
			    {
				preferredFont = new Font(font_name,Font.PLAIN,font_size);
			    }
			setFont(preferredFont);
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }

		try
		    {
			Font f = getFont();
			int font_height = 8;
			int font_width = 8;
			if(null != f)
			    {
				FontMetrics fm = getFontMetrics(f);
				if(null != fm)
				    {
					font_height = fm.getHeight();
					if(font_height < 8)
					    {
						font_height = 8;
					    }
					font_width = fm.stringWidth("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")/62;
					if(font_width < 3)
					    {
						font_width = 3;
					    }

				    }
				else
				    {
					System.err.println("Can't getFontMetrics()\n");
				    }
			    }
			if(debug_on)
			    {
				System.out.println("font_height = "+font_height);
				System.out.println("font_width = "+font_width);
			    }
			if(d.height > 0)
			    {
				text_length = (d.height - 60)/font_height;
			    }
			if(text_length > 60)
			    {
				text_length = 60;
			    }
			if(d.width > 0)
			    {
				text_width = (d.width - 4)/font_width;
			    }
			if(text_width > 200)
			    {
				text_width = 200;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }

		if(debug_on)
		    {
			System.out.println("text_length = "+text_length);
			System.out.println("text_width = "+text_width);
			System.out.println("Checking for color.");
		    }

		CheckForColor();

		if(debug_on)
		    {
			System.out.println("Creating FlowLayout");
		    }

		lm = new FlowLayout(FlowLayout.LEFT);
		setLayout(lm);
		innerPanel = new Panel();
		innerLayout = new GridBagLayout();
		innerPanel.setLayout(innerLayout);
		GridBagConstraints c = new GridBagConstraints();
		c.anchor = GridBagConstraints.NORTHWEST;

		if(debug_on)
		    {
			System.out.println("Adding screenChoice drop down list.");
		    }

		screenChoice = new Choice();
		screenChoice.add("Hierarchy");
		screenChoice.add("Options");
		screenChoice.add("Loops/Servers");
		if(!SHORT_OPTIONS_SCREEN)
		    {
			screenChoice.add("NML Code Generation");
		    }
		screenChoice.add("Files");
		innerPanel.add(screenChoice);


		c.gridx = 0;
		c.gridy = 0;
		c.insets = new Insets(2,2,2,2);
		c.ipadx = 2;
		c.ipady = 2;
		c.fill = GridBagConstraints.NONE;
		c.gridwidth = 1;
		c.gridheight = 1;
		innerLayout.setConstraints(screenChoice, c);
		screenChoice.addItemListener(this);

		if(debug_on)
		    {
			System.out.println("Adding top row buttons.");
		    }

		goButton = new CountButton("Create Source");
		SubComponentSetFont(goButton);
		innerPanel.add(goButton);
		c.gridx = 1;
		c.gridy = 0;
		innerLayout.setConstraints(goButton,c);
		goButton.addActionListener(this);

		printButton = new CountButton("Print");
		innerPanel.add(printButton);
		c.gridx = 2;
		c.gridy = 0;
		innerLayout.setConstraints(printButton,c);
		printButton.addActionListener(this);

		makeButton = new Button("Make");
		innerPanel.add(makeButton);
		c.gridx = 3;
		c.gridy = 0;
		innerLayout.setConstraints(makeButton,c);
		makeButton.addActionListener(this);

		runButton = new Button("Run");
		innerPanel.add(runButton);
		c.gridx = 4;
		c.gridy = 0;
		innerLayout.setConstraints(runButton,c);
		runButton.addActionListener(this);

		importButton = new Button("Import");
		innerPanel.add(importButton);
		c.gridx = 5;
		c.gridy = 0;
		innerLayout.setConstraints(importButton,c);
		importButton.addActionListener(this);


		openButton = new Button("Open");
		innerPanel.add(openButton);
		c.gridx = 6;
		c.gridy = 0;
		innerLayout.setConstraints(openButton,c);
		openButton.addActionListener(this);

		newButton = new Button("New");
		innerPanel.add(newButton);
		c.gridx = 7;
		c.gridy = 0;
		innerLayout.setConstraints(newButton,c);
		newButton.addActionListener(this);

		diagButton = new Button("Diag");
		innerPanel.add(diagButton);
		c.gridx = 8;
		c.gridy = 0;
		innerLayout.setConstraints(diagButton,c);
		diagButton.addActionListener(this);

		helpButton = new Button("Help");
		innerPanel.add(helpButton);
		c.gridx = 9;
		c.gridy = 0;
		innerLayout.setConstraints(helpButton,c);
		helpButton.addActionListener(this);

		if(debug_on)
		    {
			System.out.println("Adding URLLoadInfoPanel.");
		    }
		loadPanel = new URLLoadInfoPanel();
		innerPanel.add(loadPanel);
		c.gridx = 0;
		c.gridy = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridwidth = 20;
		innerLayout.setConstraints(loadPanel,c);
		c.gridwidth=1;

		if(debug_on)
		    {
			System.out.println("Adding choicePanel.");
		    }

		choicePanel = new Panel();
		choicePanelLayout = new CardLayout();
		choicePanel.setLayout(choicePanelLayout);


		if(debug_on)
		    {
			System.out.println("Adding hierarchy panel.");
		    }


		hierarchyOuterPanel = new Panel();
		hierarchyOuterLayout = new FlowLayout(FlowLayout.LEFT);
		hierarchyOuterPanel.setLayout(hierarchyOuterLayout);


		hierarchyInnerPanel = new Panel();
		hierarchyInnerLayout = new GridBagLayout();
		hierarchyInnerPanel.setLayout(hierarchyInnerLayout);



		modulesLabel = new Label("Modules");
		hierarchyInnerPanel.add(modulesLabel);
		c.gridx = 0;
		c.gridy = 0;
		c.fill = GridBagConstraints.HORIZONTAL;
		hierarchyInnerLayout.setConstraints(modulesLabel,c);



		if(null == modulesList)
		    {
			if(!list_modules_by_number)
			    {
				modulesList = new AlphabetizedList(8,false);
			    }
			else
			    {
				modulesList = new diagapplet.utils.CountList(8,false);
			    }
		    }
		hierarchyInnerPanel.add(modulesList);
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(modulesList,c);
		modulesList.addItemListener(this);


		addModuleLabel = new Label("Add Module");
		hierarchyInnerPanel.add(addModuleLabel);
		c.gridx = 0;
		c.gridy = 2;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(addModuleLabel,c);



		addModuleField = new TextField("",20);
		hierarchyInnerPanel.add(addModuleField);
		c.gridx = 0;
		c.gridy = 3;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(addModuleField,c);
		addModuleField.addActionListener(this);

		delModuleButton = new Button("Delete Module");
		hierarchyInnerPanel.add(delModuleButton);
		c.gridx = 0;
		c.gridy = 4;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(delModuleButton,c);
		delModuleButton.addActionListener(this);


		CmdOrSubChoice = new Choice();
		CmdOrSubChoice.add("Subordinates");
		CmdOrSubChoice.add("Commands");
		CmdOrSubChoice.add("Aux. Channels");
		hierarchyInnerPanel.add(CmdOrSubChoice);
		c.gridx = 0;
		c.gridy = 5;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		hierarchyInnerLayout.setConstraints(CmdOrSubChoice,c);
		CmdOrSubChoice.addItemListener(this);


		CmdOrSubPanel = new Panel();
		CmdOrSubLayout = new CardLayout();
		CmdOrSubPanel.setLayout(CmdOrSubLayout);

		subPanel = new Panel();
		subLayout = new FlowLayout(FlowLayout.LEFT);
		subPanel.setLayout(subLayout);

		if(null == subordinatesList)
		    {
			subordinatesList = new AlphabetizedList(10,true);
		    }
		subPanel.add(subordinatesList);
		subordinatesList.addItemListener(this);

		CmdOrSubPanel.add("Subordinates",subPanel);

		cmdPanel = new Panel();
		cmdLayout = new FlowLayout(FlowLayout.LEFT);
		cmdPanel.setLayout(cmdLayout);

		cmdInnerPanel = new Panel();
		cmdInnerLayout = new GridBagLayout();
		cmdInnerPanel.setLayout(cmdInnerLayout);


		if(null == cmdList)
		    {
			cmdList = new AlphabetizedList(7,false);
		    }
		cmdInnerPanel.add(cmdList);
		c.gridx = 0;
		c.gridy = 0;
		c.gridheight = 1;
		cmdInnerLayout.setConstraints(cmdList,c);
		cmdList.addItemListener(this);


		addcmdLabel = new Label("Add Command");
		cmdInnerPanel.add(addcmdLabel);
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		cmdInnerLayout.setConstraints(addcmdLabel,c);


		addcmdField = new TextField("",20);
		cmdInnerPanel.add(addcmdField);
		c.gridx = 0;
		c.gridy = 2;
		c.gridheight = 1;
		cmdInnerLayout.setConstraints(addcmdField,c);
		addcmdField.addActionListener(this);

		delcmdButton = new Button("Delete Command");
		cmdInnerPanel.add(delcmdButton);
		c.gridx = 0;
		c.gridy = 3;
		c.gridheight = 1;
		cmdInnerLayout.setConstraints(delcmdButton,c);
		delcmdButton.addActionListener(this);

		cmdPanel.add(cmdInnerPanel);
		CmdOrSubPanel.add("Commands",cmdPanel);


		auxPanel = new Panel();
		auxLayout = new FlowLayout(FlowLayout.LEFT);
		auxPanel.setLayout(auxLayout);

		auxInnerPanel = new Panel();
		auxInnerLayout = new GridBagLayout();
		auxInnerPanel.setLayout(auxInnerLayout);

		if(null == auxList)
		    {
			auxList = new AlphabetizedList(4,false);
		    }
		auxInnerPanel.add(auxList);
		c.gridx = 0;
		c.gridy = 0;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(auxList,c);
		auxList.addItemListener(this);

		addauxLabel = new Label("Add Channel");
		auxInnerPanel.add(addauxLabel);
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(addauxLabel,c);

		addauxField = new TextField("",20);
		auxInnerPanel.add(addauxField);
		c.gridx = 0;
		c.gridy = 2;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(addauxField,c);
		addauxField.addActionListener(this);

		delauxButton = new Button("Delete Channel");
		auxInnerPanel.add(delauxButton);
		c.gridx = 0;
		c.gridy = 3;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(delauxButton,c);
		delauxButton.addActionListener(this);

		//auxTypeGroup = new CheckboxGroup();
		auxInputCheckbox = new Checkbox("Input",null, true);
		auxInnerPanel.add(auxInputCheckbox);
		c.gridx = 0;
		c.gridy = 4;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(auxInputCheckbox,c);
		auxInputCheckbox.addItemListener(this);

		/*
		  auxOutputCheckbox = new Checkbox("Output",auxTypeGroup, false);
		  auxInnerPanel.add(auxOutputCheckbox);
		  c.gridx = 0;
		  c.gridy = 5;
		  c.gridheight = 1;
		  auxInnerLayout.setConstraints(auxOutputCheckbox,c);
		*/
		auxUpdateCheckbox = new Checkbox("Update Every Cycle", null,false);
		auxInnerPanel.add(auxUpdateCheckbox);
		c.gridx = 0;
		c.gridy = 6;
		c.gridheight = 1;
		auxInnerLayout.setConstraints(auxUpdateCheckbox,c);
		auxUpdateCheckbox.addItemListener(this);


		auxPanel.add(auxInnerPanel);
		CmdOrSubPanel.add("Aux. Channels",auxPanel);


		hierarchyInnerPanel.add(CmdOrSubPanel);
		c.gridx = 0;
		c.gridy = 6;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(CmdOrSubPanel,c);
		c.gridheight = 1;


		int dwidth  = d.width;
		if(dwidth > max_width)
		    {
			dwidth = max_width;
		    }
		if(dwidth < min_width)
		    {
			dwidth = min_width;
		    }
		int dheight  = d.height;
		if(dheight > max_height)
		    {
			dheight = max_height;
		    }
		if(dheight < min_height)
		    {
			dheight = min_height;
		    }
		int hierarchy_panel_width = dwidth - 220;
		if(hierarchy_panel_width > max_hierarchy_panel_width)
		    {
			hierarchy_panel_width = max_hierarchy_panel_width;
		    }
		if(hierarchy_panel_width < min_hierarchy_panel_width)
		    {
			hierarchy_panel_width = min_hierarchy_panel_width;
		    }
		int hierarchy_panel_height =  dheight - 250;
		if(hierarchy_panel_height > max_hierarchy_panel_height)
		    {
			hierarchy_panel_height = max_hierarchy_panel_height;
		    }
		if(hierarchy_panel_height < min_hierarchy_panel_height)
		    {
			hierarchy_panel_height = min_hierarchy_panel_height;
		    }
		if(debug_on)
		    {
			System.out.println("HierarchyPanel("+hierarchy_panel_width+", "+hierarchy_panel_height+")" );
		    }
		hierarchyPanel = new HierarchyPanel(hierarchy_panel_width,hierarchy_panel_height);
		SubComponentSetFont(hierarchyPanel);
		hierarchyPanel.setCountList(modulesList);
		if(m_useColor)
		    {
			hierarchyPanel.setBackground(Color.blue);
		    }
		else
		    {
			hierarchyPanel.setBackground(Color.white);
		    }
		hierarchyInnerPanel.add(hierarchyPanel);
		c.gridx = 1;
		c.gridy = 0;
		c.fill = GridBagConstraints.BOTH;
		c.gridwidth=1;
		c.gridheight = 7;
		hierarchyInnerLayout.setConstraints(hierarchyPanel, c);
		hierarchyPanel.parentFrame = parentFrame;
		hierarchyPanel.design_mode = true;

		hierarchyHorzScrollbar = new Scrollbar(Scrollbar.HORIZONTAL);
		hierarchyPanel.horzScrollbar = hierarchyHorzScrollbar;
		hierarchyInnerPanel.add(hierarchyHorzScrollbar);
		c.gridx = 1;
		c.gridy = 7;
		c.fill =  GridBagConstraints.HORIZONTAL;
		c.gridheight = 1;
		hierarchyInnerLayout.setConstraints(hierarchyHorzScrollbar, c);
		hierarchyHorzScrollbar.addAdjustmentListener(this);

		hierarchyVertScrollbar = new Scrollbar(Scrollbar.VERTICAL);
		hierarchyPanel.vertScrollbar = hierarchyVertScrollbar;
		hierarchyInnerPanel.add(hierarchyVertScrollbar);
		hierarchyVertScrollbar.addAdjustmentListener(this);
		
		c.gridx = 2;
		c.gridy = 0;
		c.fill =  GridBagConstraints.VERTICAL;
		c.gridheight = 7;
		hierarchyInnerLayout.setConstraints(hierarchyVertScrollbar, c);
		c.gridheight = 1;

		hierarchyPanel.horzScrollbar = hierarchyHorzScrollbar;
		hierarchyPanel.vertScrollbar = hierarchyVertScrollbar;
		hierarchyOuterPanel.add(hierarchyInnerPanel);
		choicePanel.add("Hierarchy",hierarchyOuterPanel);

                if(debug_on)
		    {
			System.out.println("Adding codegen panel.");
		    }

		if(!SHORT_OPTIONS_SCREEN)
		    {
			codegenOuterPanel = new Panel();
			codegenOuterLayout = new FlowLayout(FlowLayout.LEFT);
			codegenOuterPanel.setLayout(codegenOuterLayout);
		    }

		boolean new_codeGen = false;
		if(null == codeGenerationApplet)
		    {
			codeGenerationApplet = new CodeGen();
			new_codeGen = true;
		    }
		codeGenerationApplet.set_ClassList(new diagapplet.utils.FakeFastListPanel());
		codeGenerationApplet.set_m_fStandAlone(m_fStandAlone);
		codeGenerationApplet.set_inside_diagapplet(true);
		codeGenerationApplet.set_m_modulesHashTable(writer.modulesHashtable);
		codeGenerationApplet.set_serversHashtable(writer.serversHashtable);
		codeGenerationApplet.set_m_loadingPanel(loadPanel);
		codeGenerationApplet.set_m_modulesCountList(modulesList);
		codeGenerationApplet.set_m_hierarchyFile(writer.ConfigFile);
		codeGenerationApplet.set_m_ConfigFile(writer.ConfigFile);
		if(new_codeGen)
		    {
			codeGenerationApplet.init();
		    }


		if(!SHORT_OPTIONS_SCREEN)
		    {
			codegenOuterPanel.add(codeGenerationApplet);
			choicePanel.add("NML Code Generation", codegenOuterPanel);
		    }
		if(debug_on)
		    {
			System.out.println("Adding options panel.");
		    }

		optionsPanel = new Panel();
		optionsPanelLayout = new FlowLayout(FlowLayout.LEFT);
		optionsPanel.setLayout(optionsPanelLayout);

		optionsInnerPanel = new Panel();
		optionsInnerLayout = new GridBagLayout();
		optionsInnerPanel.setLayout(optionsInnerLayout);



		appdirLabel = new Label("Application Directory");
		optionsInnerPanel.add(appdirLabel);
		c.gridx = 0;
		c.gridy = 0;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(appdirLabel,c);


		appdirField = new TextField(writer.AppDir,20);
		optionsInnerPanel.add(appdirField);
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(appdirField,c);
		appdirField.addActionListener(this);


		userdirLabel = new Label("User Directory");
		optionsInnerPanel.add(userdirLabel);
		c.gridx = 0;
		c.gridy = 2;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(userdirLabel,c);

		userdirField = new TextField(writer.getUserDir(),20);
		optionsInnerPanel.add(userdirField);
		c.gridx = 0;
		c.gridy = 3;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(userdirField,c);
		userdirField.addActionListener(this);

		File userdirFile = new File(writer.getUserDir());
		try
		    {
			default_version_control_type="NONE";
			if(userdirFile.exists())
			    {
				File rcsDir = new File(userdirFile, "RCS");
				if(rcsDir.exists())
				    {
					default_version_control_type = "RCS";
				    }
				File sccsDir = new File(userdirFile, "SCCS");
				if(sccsDir.exists())
				    {
					default_version_control_type = "SCCS";
				    }
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }


		rcslibdirLabel = new Label("RCS Library Directory");
		optionsInnerPanel.add(rcslibdirLabel);
		c.gridx = 0;
		c.gridy = 4;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(rcslibdirLabel,c);


		rcslibdirField = new TextField(writer.RcsLibDir,20);
		optionsInnerPanel.add(rcslibdirField);
		c.gridx = 0;
		c.gridy = 5;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(rcslibdirField,c);
		rcslibdirField.addActionListener(this);

		appnameLabel = new Label("Application Name");
		optionsInnerPanel.add(appnameLabel);
		c.gridx = 0;
		c.gridy = 6;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(appnameLabel,c);

		appnameField = new TextField(writer.AppName,20);
		optionsInnerPanel.add(appnameField);
		c.gridx = 0;
		c.gridy = 7;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(appnameField,c);
		appnameField.addActionListener(this);

		cppExtLabel = new Label("C++ Souce File Extension");
		optionsInnerPanel.add(cppExtLabel);
		c.gridx = 0;
		c.gridy = 8;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(cppExtLabel,c);

		cppExtField = new TextField(writer.cpp_ext,20);
		optionsInnerPanel.add(cppExtField);
		c.gridx = 0;
		c.gridy = 9;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(cppExtField,c);
		cppExtField.addActionListener(this);

		hppExtLabel = new Label("C++ Header File Extension");
		optionsInnerPanel.add(hppExtLabel);
		c.gridx = 0;
		c.gridy = 10;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(hppExtLabel,c);

		hppExtField = new TextField(writer.hpp_ext,20);
		optionsInnerPanel.add(hppExtField);
		c.gridx = 0;
		c.gridy = 11;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(hppExtField,c);
		hppExtField.addActionListener(this);

		objExtLabel = new Label("C++ Object File Extension");
		optionsInnerPanel.add(objExtLabel);
		c.gridx = 0;
		c.gridy = 12;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(objExtLabel,c);


		objExtField = new TextField(writer.obj_ext,20);
		optionsInnerPanel.add(objExtField);
		c.gridx = 0;
		c.gridy = 13;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(objExtField,c);
		objExtField.addActionListener(this);

		makeCommandLabel = new Label("Make Command");
		optionsInnerPanel.add(makeCommandLabel);
		c.gridx = 1;
		c.gridy = 12;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(makeCommandLabel,c);

		makeCommandField = new TextField(writer.MakeCommand,20);
		optionsInnerPanel.add(makeCommandField);
		c.gridx = 1;
		c.gridy = 13;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(makeCommandField,c);

		terminalCommandLabel = new Label("Terminal Command");
		optionsInnerPanel.add(terminalCommandLabel);
		c.gridx = 0;
		c.gridy = 14;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(terminalCommandLabel,c);

		runCommandLabel = new Label("Run Command");
		optionsInnerPanel.add(runCommandLabel);
		c.gridx = 1;
		c.gridy = 14;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(runCommandLabel,c);

		File userDirFile = new File(writer.getUserDir());
		File runCmdFile = null;
		try
		    {
			if(userDirFile.exists())
			    {
				if(writer.mswinDevPlat)
				    {
					runCmdFile = new File(userDirFile, writer.AppName+".bat");
				    }
				else
				    {
					runCmdFile = new File(userDirFile, "run."+writer.AppName);
				    }
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		terminalCommandField = new TextField(writer.TerminalCommand,20);
		optionsInnerPanel.add(terminalCommandField);
		c.gridx = 0;
		c.gridy = 15;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(terminalCommandField,c);
		terminalCommandField.addActionListener(this);

		String run_cmd_end = "";
		if(null != runCmdFile)
		    {
			run_cmd_end = runCmdFile.toString();
		    }

		runCommandField = new TextField(run_cmd_end,20);
		optionsInnerPanel.add(runCommandField);
		c.gridx = 1;
		c.gridy = 15;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(runCommandField,c);
		runCommandField.addActionListener(this);

		if(!SHORT_OPTIONS_SCREEN)
		    {
			diagCommandLabel = new Label("Diag Command");
			optionsInnerPanel.add(diagCommandLabel);
			c.gridx = 1;
			c.gridy = 16;
			c.gridheight = 1;
			optionsInnerLayout.setConstraints(diagCommandLabel,c);
		
			diagCommandField = new TextField(diag_cmd,20);
			optionsInnerPanel.add(diagCommandField);
			c.gridx = 1;
			c.gridy = 17;
			c.gridheight = 1;
			optionsInnerLayout.setConstraints(diagCommandField,c);
			diagCommandField.addActionListener(this);

			browserCommandLabel = new Label("Browser Command");
			optionsInnerPanel.add(browserCommandLabel);
			c.gridx = 1;
			c.gridy = 18;
			c.gridheight = 1;
			optionsInnerLayout.setConstraints(browserCommandLabel,c);
		
			browserCommandField = new TextField(browser_cmd,20);
			optionsInnerPanel.add(browserCommandField);
			c.gridx = 1;
			c.gridy = 19;
			c.gridheight = 1;
			optionsInnerLayout.setConstraints(browserCommandField,c);
			browserCommandField.addActionListener(this);
		    }

		String java_cmd_string = writer.java_cmd_prefix;
		if(writer.mswinDevPlat)
		    {
			int classpath_index = java_cmd_string.toUpperCase().indexOf("%CLASSPATH%");
			if(classpath_index > 0)
			    {
				java_cmd_string = java_cmd_string.substring(0,classpath_index)+writer.RcsLibDir+"\\PLAT\\"+java_plat+"\\LIB"+File.pathSeparator+java_cmd_string.substring(classpath_index);
			    }
		    }
		else
		    {
			int classpath_index = java_cmd_string.indexOf("$(CLASSPATH)");
			if(classpath_index > 0)
			    {
				java_cmd_string = java_cmd_string.substring(0,classpath_index)+writer.RcsLibDir+"/plat/"+java_plat+"/lib"+File.pathSeparator+java_cmd_string.substring(classpath_index);
			    }
			else
			    {
				classpath_index = java_cmd_string.indexOf("$CLASSPATH");
				if(classpath_index > 0)
				    {
					java_cmd_string = java_cmd_string.substring(0,classpath_index)+writer.RcsLibDir+"/plat/"+java_plat+"/lib"+File.pathSeparator+java_cmd_string.substring(classpath_index);
				    }
			    }
		    }

		javaCommandLabel = new Label("Java Command");
		optionsInnerPanel.add(javaCommandLabel);
		c.gridx = 2;
		c.gridy = 12;
		c.gridwidth = 2;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(javaCommandLabel,c);

		javaCommandField = new TextField(java_cmd_string,20);
		optionsInnerPanel.add(javaCommandField);
		c.gridx = 2;
		c.gridy = 13;
		c.gridwidth = 2;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(javaCommandField,c);
		javaCommandField.addActionListener(this);

		useJavaInScriptsCheckbox = new Checkbox("Use Java In Scripts", null, writer.useJavaInScripts);
		optionsInnerPanel.add(useJavaInScriptsCheckbox);
		c.gridx = 2;
		c.gridy = 14;
		c.gridwidth = 2;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(useJavaInScriptsCheckbox,c);
		useJavaInScriptsCheckbox.addItemListener(this);

		if(!SHORT_OPTIONS_SCREEN)
		    {
			javaSetupLabel = new Label("Java Setup");
			optionsInnerPanel.add(javaSetupLabel);
			c.gridx = 2;
			c.gridy = 15;
			c.gridwidth = 2;
			c.gridheight = 1;
			optionsInnerLayout.setConstraints(javaSetupLabel,c);
		

			javaSetupArea = new TextArea(writer.java_setup,3, 40);
			optionsInnerPanel.add(javaSetupArea);
			c.gridx = 2;
			c.gridy = 16;
			c.gridwidth = 2;
			c.gridheight = 3;
			optionsInnerLayout.setConstraints(javaSetupArea,c);
		    }

		c.gridwidth = 1;

		addPlatLabel = new Label("Add Platform");
		optionsInnerPanel.add(addPlatLabel);
		c.gridx = 1;
		c.gridy = 0;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(addPlatLabel,c);


		addPlatField = new TextField("",20);
		optionsInnerPanel.add(addPlatField);
		c.gridx = 1;
		c.gridy = 2;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(addPlatField,c);
		addPlatField.addActionListener(this);

		delPlatButton = new Button("Delete Plat");
		optionsInnerPanel.add(delPlatButton);
		c.gridx = 1;
		c.gridy = 3;
		c.gridheight = 1;
		optionsInnerLayout.setConstraints(delPlatButton,c);
		delPlatButton.addActionListener(this);


		platLabel = new Label("Platforms");
		optionsInnerPanel.add(platLabel);
		c.gridx = 1;
		c.gridy = 5;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(platLabel,c);

		if(null == platList)
		    {
			platList = new AlphabetizedList(10,false);
			platList.add("linux");
			platList.add("vxworks5.1");
			platList.add("vxworks5.3");
			platList.add("win32msc");
			platList.add("sunos5");
			platList.add("irix6");
			platList.add("lynxosPC23");
		    }
		optionsInnerPanel.add(platList);
		c.gridx = 1;
		c.gridy = 6;
		c.gridheight = 5;
		optionsInnerLayout.setConstraints(platList,c);
		if(null != default_plat)
		    {
			boolean plat_found = false;
			for(int plati = 0; plati < platList.getItemCount(); plati++)
			    {
				String plat_temp = platList.getItem(plati);
				if(plat_temp.equals(default_plat))
				    {
					plat_found = true;
					platList.select(plati);
					break;
				    }
			    }
			if(!plat_found)
			    {
				platList.add(default_plat);
				for(int plati = 0; plati < platList.getItemCount(); plati++)
				    {
					String plat_temp = platList.getItem(plati);
					if(plat_temp.equals(default_plat))
					    {
						plat_found = true;
						platList.select(plati);
						break;
					    }
				    }
			    }
		    }
		platList.addItemListener(this);

		overwriteLabel = new Label("Replace Existing Files");
		optionsInnerPanel.add(overwriteLabel);
		c.gridx = 2;
		c.gridy = 0;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(overwriteLabel,c);

		overwriteGroup = new CheckboxGroup();

		overwriteAlwaysCheckbox = new Checkbox("Always",overwriteGroup, false);
		optionsInnerPanel.add(overwriteAlwaysCheckbox);
		c.gridx = 2;
		c.gridy = 1;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(overwriteAlwaysCheckbox,c);
		overwriteAlwaysCheckbox.addItemListener(this);

		overwritePromptCheckbox = new Checkbox("Prompt First",overwriteGroup, true);
		optionsInnerPanel.add(overwritePromptCheckbox);
		c.gridx = 2;
		c.gridy = 2;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(overwritePromptCheckbox,c);
		overwritePromptCheckbox.addItemListener(this);

		overwriteNeverCheckbox = new Checkbox("Never",overwriteGroup, false);
		optionsInnerPanel.add(overwriteNeverCheckbox);
		c.gridx = 2;
		c.gridy = 3;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(overwriteNeverCheckbox,c);
		overwriteNeverCheckbox.addItemListener(this);

		useMergerCheckbox = new Checkbox("Use Merger",null, useMerger);
		optionsInnerPanel.add(useMergerCheckbox);
		c.gridx = 2;
		c.gridy = 4;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(useMergerCheckbox,c);
		useMergerCheckbox.addItemListener(this);

		makeBackupsCheckbox = new Checkbox("Make Backups",null, makeBackups);
		optionsInnerPanel.add(makeBackupsCheckbox);
		c.gridx = 2;
		c.gridy = 5;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(makeBackupsCheckbox,c);
		makeBackupsCheckbox.addItemListener(this);

		removeBackupsButton  = new Button("Remove Backups");
		optionsInnerPanel.add(removeBackupsButton);
		c.gridx = 2;
		c.gridy = 6;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(removeBackupsButton,c);
		removeBackupsButton.addActionListener(this);

		devPlatTypeLabel = new Label("Development Platform");
		optionsInnerPanel.add(devPlatTypeLabel);
		c.gridx = 2;
		c.gridy = 7;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(devPlatTypeLabel,c);

		devPlatTypeGroup = new CheckboxGroup();
		unixCheckbox = new Checkbox("UNIX",devPlatTypeGroup,!writer.mswinDevPlat);
		optionsInnerPanel.add(unixCheckbox);
		c.gridx = 2;
		c.gridy = 8;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(unixCheckbox,c);
		unixCheckbox.addItemListener(this);

		mswinCheckbox = new Checkbox("MS Windows 95/NT", devPlatTypeGroup, writer.mswinDevPlat);
		optionsInnerPanel.add(mswinCheckbox);
		c.gridx = 2;
		c.gridy = 9;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(mswinCheckbox,c);
		mswinCheckbox.addItemListener(this);

		singleDirCheckbox = new Checkbox("SingleDir",null, singleDir);
		optionsInnerPanel.add(singleDirCheckbox);
		c.gridx = 2;
		c.gridy = 10;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(singleDirCheckbox,c);
		singleDirCheckbox.addItemListener(this);


		libsLabel = new Label("Libraries");
		optionsInnerPanel.add(libsLabel);
		c.gridx = 3;
		c.gridy = 0;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(libsLabel,c);

		if(null == libsList)
		    {
			libsList = new AlphabetizedList(5, false);
			for(int i = 0; i < 10; i++)
			    {
				libsList.add("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
			    }
		    }
		optionsInnerPanel.add(libsList);
		c.gridx = 3;
		c.gridy = 1;
		c.gridheight = 3;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(libsList,c);
		libsList.addItemListener(this);

		addLibLabel = new Label("Add Library");
		optionsInnerPanel.add(addLibLabel);
		c.gridx = 3;
		c.gridy = 4;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(addLibLabel,c);


		addLibField = new TextField("",20);
		optionsInnerPanel.add(addLibField);
		c.gridx = 3;
		c.gridy = 5;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(addLibField,c);
		addLibField.addActionListener(this);

		delLibButton = new Button("Delete Library");
		optionsInnerPanel.add(delLibButton);
		c.gridx = 3;
		c.gridy = 6;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(delLibButton,c);
		delLibButton.addActionListener(this);

		includesLabel = new Label("Include Directories");
		optionsInnerPanel.add(includesLabel);
		c.gridx = 3;
		c.gridy = 7;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(includesLabel,c);

		if(null == includesList)
		    {
			includesList = new AlphabetizedList(5, false);
			for(int i = 0; i < 10; i++)
			    {
				includesList.add("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
			    }
		    }
		optionsInnerPanel.add(includesList);
		c.gridx = 3;
		c.gridy = 8;
		c.gridheight = 3;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(includesList,c);
		includesList.addItemListener(this);

		addIncludeLabel = new Label("Add Include Directory");
		optionsInnerPanel.add(addIncludeLabel);
		c.gridx = 3;
		c.gridy = 11;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(addIncludeLabel,c);


		addIncludeField = new TextField("",20);
		optionsInnerPanel.add(addIncludeField);
		c.gridx = 3;
		c.gridy = 12;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(addIncludeField,c);
		addIncludeField.addActionListener(this);

		delIncludeButton = new Button("Delete Include Directory");
		optionsInnerPanel.add(delIncludeButton);
		c.gridx = 3;
		c.gridy = 13;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(delIncludeButton,c);
		delIncludeButton.addActionListener(this);

		debugCheckbox = new Checkbox("Debug",null,debug_on);
		optionsInnerPanel.add(debugCheckbox);
		c.gridx = 3;
		c.gridy = 14;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(debugCheckbox,c);
		debugCheckbox.addItemListener(this);

		printHashtablesButton = new Button("Print Hashtables");
		optionsInnerPanel.add(printHashtablesButton);
		c.gridx = 3;
		c.gridy = 15;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		optionsInnerLayout.setConstraints(printHashtablesButton,c);
		printHashtablesButton.addActionListener(this);

		if(default_version_control_type != null)
		    {
			if(default_version_control_type.equals("RCS"))
			    {
				if(null == default_version_control_checkout_command)
				    {
					default_version_control_checkout_command = default_rcs_version_control_checkout_command;
				    }
				if(null == default_version_control_checkin_command)
				    {
					default_version_control_checkin_command = default_rcs_version_control_checkin_command;
				    }
				if(null == default_version_control_directory)
				    {
					default_version_control_directory = default_rcs_version_control_directory;
				    }
			    }
			else if(default_version_control_type.equals("SCCS"))
			    {
				if(null == default_version_control_checkout_command)
				    {
					default_version_control_checkout_command = default_sccs_version_control_checkout_command;
				    }
				if(null == default_version_control_checkin_command)
				    {
					default_version_control_checkin_command = default_sccs_version_control_checkin_command;
				    }
				if(null == default_version_control_directory)
				    {
					default_version_control_directory = default_sccs_version_control_directory;
				    }
			    }
		    }

		if(!VERSION_CONTROL_STUFF_DISABLED)
		    {
			fileCheckinTypeLabel = new Label("File Version Control Type (disabled)");
			optionsInnerPanel.add(fileCheckinTypeLabel);
			c.gridx = 3;
			c.gridy = 12;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(fileCheckinTypeLabel,c);

			fileCheckinTypeChoice = new Choice();
			fileCheckinTypeChoice.add("RCS");
			fileCheckinTypeChoice.add("SCCS");
			fileCheckinTypeChoice.add("NONE");
			optionsInnerPanel.add(fileCheckinTypeChoice);
			c.gridx = 3;
			c.gridy = 13;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(fileCheckinTypeChoice,c);
			if(default_version_control_type == null)
			    {
				default_version_control_type = "NONE";
			    }
			if(default_version_control_type != null)
			    {
				fileCheckinTypeChoice.select(default_version_control_type);
				if(default_version_control_type.toUpperCase().equals("NONE"))
				    {
					auto_checkin = false;
					auto_checkout = false;
				    }
				fileCheckinTypeChoice.addItemListener(this);
			    }

			fileCheckinDirectoryLabel = new Label("Version Control Directory");
			optionsInnerPanel.add(fileCheckinDirectoryLabel);
			c.gridx = 3;
			c.gridy = 14;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(fileCheckinDirectoryLabel,c);
		 
			fileCheckinDirectoryField = new TextField();
			optionsInnerPanel.add(fileCheckinDirectoryField);
			c.gridx = 3;
			c.gridy = 15;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(fileCheckinDirectoryField,c);
			if(default_version_control_directory != null)
			    {
				fileCheckinDirectoryField.setText(default_version_control_directory);
			    }
			fileCheckinDirectoryField.addActionListener(this);
		 
			checkOutCommandLabel = new Label("Check-out Command");
			optionsInnerPanel.add(checkOutCommandLabel);
			c.gridx = 3;
			c.gridy = 16;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(checkOutCommandLabel,c);
		 
			checkOutCommandField = new TextField();
			optionsInnerPanel.add(checkOutCommandField);
			c.gridx = 3;
			c.gridy = 17;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(checkOutCommandField,c);
			if(default_version_control_checkout_command != null)
			    {
				checkOutCommandField.setText(default_version_control_checkout_command);
			    }
			checkOutCommandField.addActionListener(this);
		 
			checkInCommandLabel = new Label("Check-In Command");
			optionsInnerPanel.add(checkInCommandLabel);
			c.gridx = 3;
			c.gridy = 18;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(checkInCommandLabel,c);
		 
			checkInCommandField = new TextField();
			optionsInnerPanel.add(checkInCommandField);
			c.gridx = 3;
			c.gridy = 19;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(checkInCommandField,c);
			if(default_version_control_checkin_command != null)
			    {
				checkInCommandField.setText(default_version_control_checkin_command);
			    }
			checkInCommandField.addActionListener(this);
		 
			symLinkCommandLabel = new Label("Symbolic Link Command");
			optionsInnerPanel.add(symLinkCommandLabel);
			c.gridx = 3;
			c.gridy = 20;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(symLinkCommandLabel,c);
		 
			symLinkCommandField = new TextField();
			optionsInnerPanel.add(symLinkCommandField);
			c.gridx = 3;
			c.gridy = 21;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(symLinkCommandField,c);
			if(default_version_control_symlink_command != null)
			    {
				symLinkCommandField.setText(default_version_control_symlink_command);
			    }
			symLinkCommandField.addActionListener(this);

			checkInEverythingButton = new Button("Check-in All Files");
			optionsInnerPanel.add(checkInEverythingButton);
			c.gridx = 3;
			c.gridy = 22;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(checkInEverythingButton,c);
			c.gridwidth = 1;
			checkInEverythingButton.addActionListener(this);
		 
			autoCheckOutCheckbox = new Checkbox("Automatically check files out");
			optionsInnerPanel.add(autoCheckOutCheckbox);
			c.gridx = 3;
			c.gridy = 23;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(autoCheckOutCheckbox,c);
			autoCheckOutCheckbox.setState(auto_checkout);
			c.gridwidth = 1;
			autoCheckOutCheckbox.addItemListener(this);

			autoCheckInCheckbox = new Checkbox("Automatically check files in.");
			optionsInnerPanel.add(autoCheckInCheckbox);
			c.gridx = 3;
			c.gridy = 24;
			c.gridheight = 1;
			c.gridwidth = 2;
			c.fill = GridBagConstraints.HORIZONTAL;
			optionsInnerLayout.setConstraints(autoCheckInCheckbox,c);
			autoCheckInCheckbox.setState(auto_checkin);
			c.gridwidth = 1;
			autoCheckInCheckbox.addItemListener(this);
		    }
		optionsPanel.add(optionsInnerPanel);
		choicePanel.add("Options", optionsPanel);

		if(debug_on)
		    {
			System.out.println("Adding loops panel.");
		    }


		loopsPanel = new Panel();
		loopsLayout = new FlowLayout(FlowLayout.LEFT);
		loopsPanel.setLayout(loopsLayout);

		loopsInnerPanel = new Panel();
		loopsInnerLayout = new GridBagLayout();
		loopsInnerPanel.setLayout(loopsInnerLayout);

		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;

		addmainLoopLabel = new Label("Add Main Loop");
		loopsInnerPanel.add(addmainLoopLabel);
		c.gridx = 0;
		c.gridy = 0;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(addmainLoopLabel,c);


		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		addmainLoopField = new TextField("",20);
		loopsInnerPanel.add(addmainLoopField);
		c.gridx = 0;
		c.gridy = 1;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(addmainLoopField,c);
		addmainLoopField.addActionListener(this);

		delmainLoopButton = new Button("Delete Main Loop");
		loopsInnerPanel.add(delmainLoopButton);
		c.gridx = 0;
		c.gridy = 2;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(delmainLoopButton,c);
		delmainLoopButton.addActionListener(this);

		cycletimeLabel = new Label("Cycle Time");
		loopsInnerPanel.add(cycletimeLabel);
		c.gridx = 0;
		c.gridy = 3;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(cycletimeLabel,c);

		cycletimeField = new TextField("0.1",20);
		loopsInnerPanel.add(cycletimeField);
		c.gridx = 0;
		c.gridy = 4;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(cycletimeField,c);
		cycletimeField.addActionListener(this);

		mainLoopHostLabel = new Label("Host");
		loopsInnerPanel.add(mainLoopHostLabel);
		c.gridx = 0;
		c.gridy = 5;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(mainLoopHostLabel,c);

		mainLoopHostField = new TextField("localhost",20);
		loopsInnerPanel.add(mainLoopHostField);
		c.gridx = 0;
		c.gridy = 6;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(mainLoopHostField,c);
		mainLoopHostField.addActionListener(this);

		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;

		mainLoopLabel = new Label("Main Loops");
		loopsInnerPanel.add(mainLoopLabel);
		c.gridx = 1;
		c.gridy = 0;
		c.fill = GridBagConstraints.HORIZONTAL;
		loopsInnerLayout.setConstraints(mainLoopLabel,c);

		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		if(null == mainLoopList)
		    {
			mainLoopList = new AlphabetizedList(10,false);
			mainLoopList.add(writer.AppName);
			mainLoopList.select(0);
		    }
		loopsInnerPanel.add(mainLoopList);
		c.gridx = 1;
		c.gridy = 1;
		c.gridheight = 9;
		loopsInnerLayout.setConstraints(mainLoopList,c);
		if(null == writer.mainloopsHashtable)
		    {
			writer.mainloopsHashtable = new Hashtable();
		    }
		if(null != writer.mainloopsHashtable)
		    {
			if(writer.AppName.length() > 0)
			    {
				rcsdesignMainLoopInfo tempMli = new rcsdesignMainLoopInfo(writer.AppName);
				writer.mainloopsHashtable.put(writer.AppName, tempMli);
				writer.curmainLoop = (rcsdesignMainLoopInfo)  writer.mainloopsHashtable.get(writer.AppName);
			    }
		    }
		mainLoopList.addItemListener(this);



		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;


		modsInLoopLabel = new Label("Modules In Loop");
		loopsInnerPanel.add(modsInLoopLabel);
		c.gridx = 2;
		c.gridy = 0;
		c.gridheight = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		loopsInnerLayout.setConstraints(modsInLoopLabel,c);


		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		if(null == modsInLoopList)
		    {
			modsInLoopList = new AlphabetizedList(10,true);
		    }
		loopsInnerPanel.add(modsInLoopList);
		c.gridx = 2;
		c.gridy = 1;
		c.gridheight = 9;
		loopsInnerLayout.setConstraints(modsInLoopList,c);
		c.gridheight = 1;
		modsInLoopList.addItemListener(this);

		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;

		addServerLabel = new Label("Add NML Server");
		loopsInnerPanel.add(addServerLabel);
		c.gridx = 0;
		c.gridy = 10;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(addServerLabel,c);


		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		addServerField = new TextField();
		loopsInnerPanel.add(addServerField);
		c.gridx = 0;
		c.gridy = 11;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(addServerField,c);
		addServerField.addActionListener(this);

		delServerButton = new Button("Delete Server");
		loopsInnerPanel.add(delServerButton);
		c.gridx = 0;
		c.gridy = 12;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(delServerButton,c);
		delServerButton.addActionListener(this);

		serverHostLabel = new Label("Host");
		loopsInnerPanel.add(serverHostLabel);
		c.gridx = 0;
		c.gridy = 14;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(serverHostLabel,c);

		serverHostField = new TextField("localhost",20);
		loopsInnerPanel.add(serverHostField);
		c.gridx = 0;
		c.gridy = 15;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(serverHostField,c);
		serverHostField.addActionListener(this);

		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;

		serverLabel = new Label("NML Servers");
		loopsInnerPanel.add(serverLabel);
		c.gridx = 1;
		c.gridy = 10;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(serverLabel,c);

		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		if(null == serverList)
		    {
			serverList = new AlphabetizedList(10,false);
			if(null != writer && null != codeGenerationApplet)
			    {
				serverList.add(writer.AppName+"svr");
				ServerInfo si = new ServerInfo(writer.AppName+"svr","is_server=true;\nhost=localhost;\n");
				codeGenerationApplet.set_serversHashtable(writer.serversHashtable);
				// System.out.println("Adding "+si);
				writer.serversHashtable.put(si.Name,si);
				serverList.select(0);
			    }
		    }
		loopsInnerPanel.add(serverList);
		c.gridx = 1;
		c.gridy = 11;
		c.gridheight = 9;
		loopsInnerLayout.setConstraints(serverList,c);
		serverList.addItemListener(this);


		c.insets = new Insets(8,8,1,8);
		c.ipadx = 5;
		c.ipady = 2;

		bufsInServerLabel = new Label("Buffers for server");
		loopsInnerPanel.add(bufsInServerLabel);
		c.gridx = 2;
		c.gridy = 10;
		c.gridheight = 1;
		loopsInnerLayout.setConstraints(bufsInServerLabel,c);

		c.insets = new Insets(0,8,0,8);
		c.ipadx = 5;
		c.ipady = 1;

		if(null == bufsInServerList)
		    {
			bufsInServerList = new AlphabetizedList(10,true);
		    }
		loopsInnerPanel.add(bufsInServerList);
		c.gridx = 2;
		c.gridy = 11;
		c.gridheight = 9;
		loopsInnerLayout.setConstraints(bufsInServerList,c);
		bufsInServerList.addItemListener(this);

		c.gridheight = 1;
		c.insets = new Insets(2,2,2,2);
		c.ipadx = 2;
		c.ipady = 2;


		loopsPanel.add(loopsInnerPanel);
		choicePanel.add("Loops/Servers", loopsPanel);


		if(debug_on)
		    {
			System.out.println("Adding files panel.");
		    }


		filesPanel = new Panel();
		filesLayout = new FlowLayout(FlowLayout.LEFT);
		filesPanel.setLayout(filesLayout);
		filesInnerPanel = new Panel();
		filesInnerLayout = new GridBagLayout();
		filesInnerPanel.setLayout(filesInnerLayout);


		saveFileButton = new Button("Save");
		c.gridx = 0;
		c.gridy = 0;
		c.gridheight = 1;
		c.fill = GridBagConstraints.NONE;
		c.insets = new Insets(0,0,0,0);
		c.ipadx = 0;
		filesInnerPanel.add(saveFileButton);
		filesInnerLayout.setConstraints(saveFileButton,c);
		saveFileButton.addActionListener(this);

		updateFileButton = new Button("Update");
		c.gridx = 1;
		c.gridy = 0;
		c.gridheight = 1;
		filesInnerPanel.add(updateFileButton);
		filesInnerLayout.setConstraints(updateFileButton,c);
		updateFileButton.addActionListener(this);

		updateAllFilesButton = new Button("Update All");
		c.gridx = 2;
		c.gridy = 0;
		c.gridheight = 1;
		filesInnerPanel.add(updateAllFilesButton);
		filesInnerLayout.setConstraints(updateAllFilesButton,c);
		updateAllFilesButton.addActionListener(this);

		fileCheckOutCheckbox = new Checkbox("Checked-Out");
		c.gridx = 3;
		c.gridy = 0;
		c.gridheight = 1;
		c.fill = GridBagConstraints.BOTH;
		filesInnerPanel.add(fileCheckOutCheckbox);
		filesInnerLayout.setConstraints(fileCheckOutCheckbox,c);
		fileCheckOutCheckbox.addItemListener(this);


		filesListLabel = new Label("Application Files");
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth=3;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(8,8,8,8);
		c.ipadx = 8;
		c.ipady = 1;
		filesInnerPanel.add(filesListLabel);
		filesInnerLayout.setConstraints(filesListLabel,c);

		fileTextLabel = new Label("No File Selected.");
		c.gridx = 3;
		c.gridy = 1;
		c.gridwidth=3;
		c.fill = GridBagConstraints.BOTH;
		filesInnerPanel.add(fileTextLabel);
		filesInnerLayout.setConstraints(fileTextLabel,c);


		if(null == filesList)
		    {
			int file_list_length = text_length - 15;
			if(file_list_length < 10)
			    {
				file_list_length = 10;
			    }
			if(debug_on)
			    {
				System.out.println("file_list_length = "+file_list_length);
			    }
			filesList = new AlphabetizedList(file_list_length,false);
			for(int i = 0; i < file_list_length+15; i++)
			    {
				filesList.add("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
			    }
		    }
		c.fill = GridBagConstraints.BOTH;
		c.gridx = 0;
		c.gridy = 2;
		c.fill = GridBagConstraints.BOTH;
		c.gridheight = 5;
		c.gridwidth=3;
		filesInnerPanel.add(filesList);
		filesInnerLayout.setConstraints(filesList,c);
		filesList.addItemListener(this);

		int file_text_panel_pixel_width = dwidth/2;
		if(file_text_panel_pixel_width > max_file_text_panel_pixel_width)
		    {
			file_text_panel_pixel_width =  max_file_text_panel_pixel_width;
		    }
		if(file_text_panel_pixel_width < min_file_text_panel_pixel_width)
		    {
			file_text_panel_pixel_width =  min_file_text_panel_pixel_width;
		    }
		int file_text_panel_pixel_height = dheight/5*4-80;
		if(file_text_panel_pixel_height > max_file_text_panel_pixel_height)
		    {
			file_text_panel_pixel_height =  max_file_text_panel_pixel_height;
		    }
		if(file_text_panel_pixel_height < min_file_text_panel_pixel_height)
		    {
			file_text_panel_pixel_height =  min_file_text_panel_pixel_height;
		    }
		fileTextArea = new AutoSizedTextArea("", file_text_panel_pixel_width, file_text_panel_pixel_height, max_file_text_width, max_file_text_length);
		c.gridx = 3;
		c.gridy = 2;
		c.gridheight = 5;
		c.gridwidth=3;
		c.fill = GridBagConstraints.BOTH;
		filesInnerPanel.add(fileTextArea);
		filesInnerLayout.setConstraints(fileTextArea,c);
		fileTextArea.addTextListener(this);




		fileAsterixLabel = new Label("(*) Means the file may need to be updated and/or saved.");
		c.gridx = 0;
		c.gridy = 7;
		c.gridheight = 1;
		c.gridwidth = 6;
		c.fill = GridBagConstraints.BOTH;
		filesInnerPanel.add(fileAsterixLabel);
		filesInnerLayout.setConstraints(fileAsterixLabel,c);
		c.gridwidth = 1;

		filesPanel.add(filesInnerPanel);
		choicePanel.add("Files", filesPanel);


		innerPanel.add(choicePanel);
		c.gridx = 0;
		c.gridy = 2;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridwidth = 20;
		innerLayout.setConstraints(choicePanel, c);
		if(debug_on)
		    {
			System.out.println("choicePanelLayout.show(choicePanel,"+currentViewString+")");
		    }
		choicePanelLayout.show(choicePanel,currentViewString);


		add(innerPanel);
		if(debug_on)
		    {
			System.out.println("rcsDesign.init(): setVisible(true)");
		    }
		setVisible(true);

		if(modulesList != null && !first_init)
		    {
			int selected_module_index = modulesList.getSelectedIndex();
			if(selected_module_index >=  0)
			    {
				String selected_module_name = modulesList.getSelectedItem();
				if(null == selected_module_name || null == writer.modulesHashtable)
				    {
					modulesList.deselect(selected_module_index);
					writer.curModule = null;
					inside_init=false;
					initialized=true;
					return;
				    }
				writer.curModule = (ModuleInfo) writer.modulesHashtable.get(selected_module_name);
				if(null == writer.curModule)
				    {
					modulesList.deselect(selected_module_index);
					writer.curModule = null;
					inside_init=false;
					initialized=true;
					return;
				    }
				if(!selected_module_name.equals(writer.curModule.Name))
				    {
					Alert("Current Module Invalid "+selected_module_name+" != "+writer.curModule.Name);
					modulesList.deselect(selected_module_index);
					writer.curModule = null;
					inside_init=false;
					initialized=true;
					return;
				    }
			    }
		    }
		read_everything_needed = true;
		if(debug_on)
		    {
			System.out.println("rcsDesign.init(): CreateBufferList()");
		    }
		CreateBufferList();


		if(first_init)
		    {
			File configfileFile = new File(writer.ConfigFile);

			if(!userdirFile.exists() || !userdirFile.isDirectory() || ! configfileFile.exists())
			    {
				try
				    {
					Thread.sleep(100);
				    }
				catch(Exception e)
				    {
					e.printStackTrace();
				    }
				DisableAllControls();
				//Alert("No RCS Application found in the current directory.\nAfter the RCS-Design tool is up and running, \nclick on the New Button to create a new application\nor the Open Button to open an existing one.\n");
			    }
			else
			    {
				if(!m_fStandAlone)
				    {
					if(debug_on)
					    {
						System.out.println("rcsDesign.init(): StartLongFunction(READ_CONFIGURATION);");
					    }
					StartLongFunction(READ_CONFIGURATION);
				    }
			    }
		    }
		first_init = false;

		initialized = true;
		inside_init = false;
		if(debug_on)
		    {
			System.out.println("rcsDesign.init(): validate();");
		    }
		validate();
		if(debug_on)
		    {
			System.out.println("rcsDesign.init(): repaint();");
		    }
		repaint();
		if(debug_on)
		    {
			System.out.println("RCS-Design Initialized");
		    }
		setVisible(true);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }

    }

    void StartConfigFileRead()
    {
	try
	    {
		config_file_read_started=false;
		if(null == writer)
		    {
			return;
		    }
		if(null == writer.getUserDir())
		    {
			return;
		    }
		if(writer.getUserDir().length() < 1)
		    {
			return;
		    }
		File userDirFile = new File(writer.getUserDir());
		if(null == userDirFile)
		    {
			return;
		    }
		if(!userDirFile.exists())
		    {
			return;
		    }
		if(!userDirFile.isDirectory())
		    {
			return;
		    }
		if(null == writer.ConfigFile)
		    {
			return;
		    }
		if(writer.ConfigFile.length() < 1)
		    {
			return;
		    }
		File configfileFile = new File(writer.ConfigFile);
		if(null == configfileFile)
		    {
			return;
		    }
		config_file_read_started=true;
		StartLongFunction(READ_CONFIGURATION);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void PrintHashtables()
    {
	try
	    {
		System.out.println("");
		System.out.println("*********************************************************************");
		System.out.println("*  RCS-Design Hashtables   ---   " + currentTime());
		// Thread.dumpStack();
		writer.PrintServersHashtable();
		writer.PrintBuffersHashtable();
		writer.PrintMainLoopsHashtable();
		writer.PrintModulesHashtable();
		System.out.println("*********************************************************************");
		System.out.println("");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void DisableAllControls()
    {
	if(null != screenChoice)
	    {
		screenChoice.setEnabled(false);
	    }
	if(null != addModuleField)
	    {
		addModuleField.setEditable(false);
		addModuleField.setEnabled(false);
	    }
	if(null != goButton)
	    {
		goButton.setEnabled(false);
	    }
	if(null != printButton)
	    {
		printButton.setEnabled(false);
	    }
	if(null != importButton)
	    {
		importButton.setEnabled(false);
	    }
	if(null != CmdOrSubChoice)
	    {
		CmdOrSubChoice.setEnabled(false);
	    }
	if(null != delModuleButton)
	    {
		delModuleButton.setEnabled(false);
	    }
	if(null != modulesList)
	    {
		modulesList.setEnabled(false);
	    }
	if(null != makeButton)
	    {
		makeButton.setEnabled(false);
	    }
	if(null != runButton)
	    {
		runButton.setEnabled(false);
	    }
	if(null != diagButton)
	    {
		diagButton.setEnabled(false);
	    }
	if(null != cmdList)
	    {
		cmdList.setEnabled(false);
	    }
	if(null != addModuleLabel)
	    {
		addModuleLabel.setEnabled(false);
	    }
	if(null != modulesLabel)
	    {
		modulesLabel.setEnabled(false);
	    }
	monitored_repaint();
    }

    void EnableAllControls()
    {
	if(null != screenChoice)
	    {
		screenChoice.setEnabled(true);
	    }
	if(null != addModuleField)
	    {
		addModuleField.setEditable(true);
		addModuleField.setEnabled(true);
	    }
	if(null != goButton)
	    {
		goButton.setEnabled(true);
	    }
	if(null != printButton)
	    {
		printButton.setEnabled(true);
	    }
	if(null != importButton)
	    {
		importButton.setEnabled(true);
	    }
	if(null != CmdOrSubChoice)
	    {
		CmdOrSubChoice.setEnabled(true);
	    }
	if(null != delModuleButton)
	    {
		delModuleButton.setEnabled(true);
	    }
	if(null != modulesList)
	    {
		modulesList.setEnabled(true);
	    }
	if(null != makeButton)
	    {
		makeButton.setEnabled(true);
	    }
	if(null != runButton)
	    {
		runButton.setEnabled(true);
	    }
	if(null != helpButton)
	    {
		helpButton.setEnabled(true);
	    }
	if(null != diagButton)
	    {
		diagButton.setEnabled(true);
	    }
	if(null != cmdList)
	    {
		cmdList.setEnabled(true);
	    }
	if(null != addModuleLabel)
	    {
		addModuleLabel.setEnabled(true);
	    }
	if(null != modulesLabel)
	    {
		modulesLabel.setEnabled(true);
	    }
	if(null != loadPanel)
	    {
		loadPanel.setVisible(true);
		loadPanel.setEnabled(false);
	    }
	if(null != auxInputCheckbox)
	    {
		auxInputCheckbox.setEnabled(true);
	    }
	if(null != auxUpdateCheckbox)
	    {
		auxUpdateCheckbox.setEnabled(true);
	    }
	monitored_repaint();
    }

    String rcslibwarndir = "";

    private void RcsLibDirCheck()
    {

	try
	    {
		GetParameters(orig_args);
		writer.RcsLibDir = rcslibdirField.getText();
		if(writer.RcsLibDir.equals(rcslibwarndir))
		    {
			return;
		    }
		rcslibwarndir = writer.RcsLibDir;
		File rcslibdirFile  = new File(writer.RcsLibDir);
		if(!rcslibdirFile.exists())
		    {
			Alert("The Real-time Control Systems Library\n can not be found at "+writer.RcsLibDir+"\n\n Either install the library \nor modify the setting on the options screen.");
		    }

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    static protected int read_config_count = 0;

    @SuppressWarnings("unchecked")
    protected void ReadEverything()
    {
	try
	    {
		reading_config_file=true;
		HierarchyPanel.updating_hierarchy = true;
		File configfileFile = new File(writer.ConfigFile);
		File userdirFile = new File(writer.getUserDir());
		ModuleInfo.ClearStaticData();
		modulesList.removeAll();
		cmdList.removeAll();
		subordinatesList.removeAll();
		auxList.removeAll();
		libsList.removeAll();
		includesList.removeAll();
		mainLoopList.removeAll();
		mainLoopList.add(writer.AppName);
		modsInLoopList.removeAll();
		codeGenerationApplet.set_m_modulesHashTable(writer.modulesHashtable);
		codeGenerationApplet.set_serversHashtable(writer.serversHashtable);
		if(userdirFile.exists() && userdirFile.isDirectory() && ! configfileFile.exists())
		    {
			File completeFile = new File(userdirFile, configfileFile.getName());
			if(completeFile.exists())
			    {
				configfileFile = completeFile;
			    }
			default_version_control_type="NONE";
		    }
		if(!configfileFile.exists())
		    {
			InitializeLists();
			default_version_control_type="NONE";
			writer.ConfigFile="";
			//writer.setUserDir("");
			read_config_count++;
			HierarchyPanel.updating_hierarchy = false;
			reading_config_file=false;
			return;
		    }
		System.out.println("");
		System.out.println("RCS-Design: Reading configuration file "+configfileFile);        
		writer.ConfigFile = configfileFile.getName();
		try
		    {
			if(null != configfileFile.getParentFile())
			    {
				writer.setUserDir(configfileFile.getParentFile().getPath());
			    }
			else
			    {
				writer.setUserDir(System.getProperty("user.dir"));
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		codeGenerationApplet.set_m_hierarchyFile(writer.ConfigFile);
		codeGenerationApplet.set_m_ConfigFile(writer.ConfigFile);
		boolean orig_cga_debug = codeGenerationApplet.debug_on;
		if(debug_on)
		    {
			System.out.println("****************************");
			CodeGen.debug_on =true;
		    }
		codeGenerationApplet.LoadHierarchy();
		System.out.println("****************************");
		CodeGen.debug_on =orig_cga_debug;
		GetParameters(orig_args);
		setup_version_control_info();
		if(null != rcslibdirField)
		    {
			rcslibdirField.setText(writer.RcsLibDir);
		    }
		RcsLibDirCheck();
		InitializeLists();
		if(list_modules_by_number)
		    {
			SortModulesList();
		    }
		if(debug_on)
		    {
			PrintHashtables();
		    }
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Application loaded at "+currentTime()+". ";
			loadPanel.bytes_read = 1;
			loadPanel.content_length = 1;
			loadPanel.repaint();
		    }
		if(null != hierarchyPanel)
		    {
			hierarchyPanel.repaint();
		    }
		reading_config_file=false;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	read_config_count++;
	HierarchyPanel.updating_hierarchy = false;
    }

    @SuppressWarnings("unchecked")
    protected void InitializeLists()
    {
	boolean orig_updating_hierarchy = HierarchyPanel.updating_hierarchy;
	try
	    {
		HierarchyPanel.updating_hierarchy = true;
		if(debug_on)
		    {
			System.out.println("");
			System.out.println("rcsDesign.InitializeLists():");
		    }
		subordinatesList.removeAll();
		modsInLoopList.removeAll();
		Enumeration modsEnum = writer.modulesHashtable.keys();
		while(modsEnum.hasMoreElements())
		    {
			String modName = (String) modsEnum.nextElement();
			if(debug_on)
			    {
				System.out.println("modName="+modName);
			    }
			subordinatesList.add(modName);
			modsInLoopList.add(modName);
			ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
			if(debug_on)
			    {
				System.out.println("ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get("+modName+") ="+modInfo);
			    }
			if(null != modInfo)
			    {
				if(null == modInfo.cmdsAvailable)
				    {
					modInfo.cmdsAvailable = new Vector();
				    }
				if(null != modInfo.cmdsAvailable)
				    {
					boolean init_found = false;
					boolean halt_found = false;
					for(int i = 0; i < modInfo.cmdsAvailable.size(); i++)
					    {
						String cmdName = (String) modInfo.cmdsAvailable.elementAt(i);
						if(debug_on)
						    {
							System.out.println("String cmdName = (String) modInfo.cmdsAvailable.elementAt("+i+") = "+cmdName);
						    }
						if(cmdName.indexOf("INIT") >= 0  && !init_found)
						    {
							init_found = true;
							if(null != modInfo.cmdsBaseClass)
							    {
								for(int j = 0; j  < modInfo.cmdsBaseClass.size(); j++)
								    {
									String base_cmd = (String) modInfo.cmdsBaseClass.elementAt(j);
									if(base_cmd.equals(cmdName))
									    {
										init_found = false;
										break;
									    }
								    }
							    }
						    }
						if(cmdName.indexOf("HALT") >= 0 && !halt_found)
						    {
							halt_found = true;
							if(null != modInfo.cmdsBaseClass)
							    {
								for(int j = 0; j  < modInfo.cmdsBaseClass.size(); j++)
								    {
									String base_cmd = (String) modInfo.cmdsBaseClass.elementAt(j);
									if(base_cmd.equals(cmdName))
									    {
										halt_found = false;
										break;
									    }
								    }
							    }
						    }
						if(init_found && halt_found)
						    {
							break;
						    }
					    }
				    }
				if(null != modInfo.externalIncludeDirectories)
				    {
					for(int i = -0; i < modInfo.externalIncludeDirectories.size(); i++)
					    {
						String dir = (String) modInfo.externalIncludeDirectories.elementAt(i);
						if(debug_on)
						    {
							System.out.println("String dir = (String) modInfo.externalIncludeDirectories.elementAt("+i+") ="+dir);
						    }
						includesList.add(dir);
					    }
				    }
				if(null != modInfo.externalLibraries)
				    {
					for(int i = -0; i < modInfo.externalLibraries.size(); i++)
					    {
						String lib = (String) modInfo.externalLibraries.elementAt(i);
						if(debug_on)
						    {
							System.out.println("String lib = (String) modInfo.externalLibraries.elementAt("+i+") ="+lib);
						    }
						libsList.add(lib);
					    }
				    }
				writer.AppDir = appdirField.getText();
				if(writer.getUserDir() == null || writer.getUserDir().equals("") || !userdirField.getText().equals(""))
				    {
					writer.setUserDir(userdirField.getText());
				    }
				if(debug_on)
				    {
					System.out.println("writer.AppDir = appdirField.getText(); = "+writer.AppDir);
					System.out.println("writer.UserDir = userdirField.getText(); = "+writer.getUserDir());
				    }
				if(null != writer.getUserDir())
				    {
					if(null == writer.AppDir)
					    {
						writer.AppDir = writer.getUserDir();
						appdirField.setText(writer.AppDir);
					    }
					if(writer.AppDir.length() < 1)
					    {
						writer.AppDir = writer.getUserDir();
						appdirField.setText(writer.AppDir);
					    }
				    }
				if(null == writer.AppDir)
				    {
					writer.AppDir = ".";
				    }
				if(writer.AppDir.length() < 1)
				    {
					writer.AppDir = ".";
				    }
				if(modInfo.releaseLibrary == null)
				    {
					if(writer.mswinDevPlat)
					    {
						modInfo.releaseLibrary  =  writer.AppDir+ File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+appnameField.getText()+".lib";
					    }
					else
					    {
						modInfo.releaseLibrary  = writer.AppDir + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+"lib"+appnameField.getText()+".a";
					    }
				    }
				if(modInfo.releaseIncludeDirectory == null)
				    {
					modInfo.releaseIncludeDirectory  = writer.AppDir + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"include";
				    }

				if(debug_on)
				    {
					System.out.println("modInfo.MainLoopName = "+modInfo.MainLoopName);
				    }
				if(modInfo.MainLoopName == null)
				    {
					if(debug_on)
					    {
						System.out.println("writer.AppName = "+writer.AppName);
					    }
					modInfo.MainLoopName = writer.AppName;
				    }
				if(modInfo.MainLoopName != null)
				    {
					boolean loop_found = false;
					for(int i = 0; i < mainLoopList.getItemCount(); i++)
					    {
						String mainloopname = mainLoopList.getItem(i);
						if(debug_on)
						    {
							System.out.println("mainloopname= "+mainloopname);
						    }
						if(mainloopname.equals(modInfo.MainLoopName))
						    {
							rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(mainloopname);
							if(debug_on)
							    {
								System.out.println("rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get("+mainloopname+"); ="+loopInfo);
							    }
							if(null == loopInfo)
							    {
								loopInfo = new rcsdesignMainLoopInfo(mainloopname);
								if(debug_on)
								    {
									System.out.println("loopInfo = new rcsdesignMainLoopInfo("+mainloopname+"); ="+loopInfo);
								    }
								writer.mainloopsHashtable.put(mainloopname, loopInfo);
							    }
							loopInfo.addModule(modName);
							loopInfo.cycle_time = modInfo.cycle_time;
							loopInfo.host = modInfo.host;
							loop_found = true;
							break;
						    }
					    }
					if(debug_on)
					    {
						System.out.println("loop_found="+loop_found);
					    }
					if(!loop_found)
					    {
						mainLoopList.add(modInfo.MainLoopName);
						rcsdesignMainLoopInfo loopInfo = new rcsdesignMainLoopInfo(modInfo.MainLoopName);
						loopInfo.cycle_time = modInfo.cycle_time;
						loopInfo.host = modInfo.host;
						loopInfo.addModule(modName);
						writer.mainloopsHashtable.put(modInfo.MainLoopName, loopInfo);
					    }
				    }
			    }
		    }
		int mod_index = modulesList.getSelectedIndex();
		if(debug_on)
		    {
			System.out.println("int mod_index = modulesList.getSelectedIndex(); = "+mod_index);
		    }
		if(mod_index >= 0)
		    {
			String modName = modulesList.getItem(mod_index);
			if(debug_on)
			    {
				System.out.println("String modName = modulesList.getItem("+mod_index+"); = "+modName);
			    }
			if(null != modName)
			    {
				ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
				if(debug_on)
				    {
					System.out.println("ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get("+modName+"); ="+modInfo);
				    }
				if(null != modInfo)
				    {
					for(int i = 0; i < modInfo.children_names.size(); i++)
					    {
						for(int j = 0; j < subordinatesList.getItemCount(); j++)
						    {
							if(subordinatesList.getItem(j).equals(((String) modInfo.children_names.elementAt(i))))
							    {
								subordinatesList.select(j);
								if(debug_on)
								    {
									System.out.println("subordinatesList.select(j="+j+")");
								    }
							    }
						    }
					    }
					cmdList.removeAll();
					storeCmdsLoop:                        for(int i = 0; i < modInfo.cmdsAvailable.size(); i++)
					    {
						String cmdName = (String) modInfo.cmdsAvailable.elementAt(i);
						int eqIndex = cmdName.indexOf('=');
						if(eqIndex > 0)
						    {
							cmdName = cmdName.substring(0,eqIndex);
						    }
						for(int j = 0; j < cmdList.getItemCount(); j++)
						    {
							String cmdFromList = cmdList.getItem(j);
							if(cmdFromList.equals(cmdName))
							    {
								continue storeCmdsLoop;
							    }
						    }
						cmdList.add(cmdName);
					    }
				    }
				writer.curModule = modInfo;
				if(debug_on && null != writer.curModule)
				    {
					System.out.println("Selected module = "+writer.curModule.Name);
				    }
			    }
			int loop_index = mainLoopList.getSelectedIndex();
			if(debug_on)
			    {
				System.out.println("int loop_index = mainLoopList.getSelectedIndex(); = "+loop_index);
			    }
			if(loop_index < 0)
			    {
				for(int i = 0; i < mainLoopList.getItemCount(); i++)
				    {
					String tempLoopName = mainLoopList.getItem(i);
					if(debug_on)
					    {
						System.out.println("tempLoopName = mainLoopList.getItem("+i+"); ="+tempLoopName);
					    }
					if(tempLoopName.startsWith(writer.AppName))
					    {
						mainLoopList.select(i);
						int tries = 0;
						while(!mainLoopList.isIndexSelected(i) && tries < 20)
						    {
							Thread.sleep(10);
							mainLoopList.select(i);
							tries++;
						    }
						break;
					    }
				    }
				loop_index = mainLoopList.getSelectedIndex();
				if(debug_on)
				    {
					System.out.println("int loop_index = mainLoopList.getSelectedIndex(); = "+loop_index);
				    }
			    }
			if(loop_index < 0)
			    {
				mainLoopList.select(0);
				loop_index = mainLoopList.getSelectedIndex();
				if(debug_on)
				    {
					System.out.println("int loop_index = mainLoopList.getSelectedIndex(); = "+loop_index);
				    }
			    }

			if(loop_index >= 0)
			    {
				String loopName = mainLoopList.getItem(loop_index);
				if(debug_on)
				    {
					System.out.println("String loopName = mainLoopList.getItem(loop_index="+loop_index+"); ="+loopName);
				    }
				if(loopName != null)
				    {
					rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(loopName);
					if(debug_on)
					    {
						System.out.println("rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get("+loopName+"); ="+loopInfo);
					    }
					if(null != loopInfo)
					    {
						try
						    {
							cycletimeField.setText(String.valueOf(loopInfo.cycle_time));
							if(loopInfo.host == null)
							    {
								loopInfo.host = "localhost";
							    }
							mainLoopHostField.setText(loopInfo.host);
						    }
						catch(Exception e)
						    {
							e.printStackTrace();
						    }
						if(null != loopInfo.getModules())
						    {
							for(int i = 0; i < loopInfo.getModules().size(); i++)
							    {
								String modInLoopName = (String) loopInfo.getModules().elementAt(i);
								if(debug_on)
								    {
									System.out.println("String modInLoopName = (String) loopInfo.getModules().elementAt("+i+"); ="+modInLoopName);
								    }
								for(int j = 0; j < modsInLoopList.getItemCount(); j++)
								    {
									if(debug_on)
									    {
										System.out.println("modsInLoopList.getItem("+j+") = "+modsInLoopList.getItem(j));
									    }
									if(modsInLoopList.getItem(j).equals(modInLoopName))
									    {
										modsInLoopList.select(j);
										if(debug_on)
										    {
											System.out.println("modsInLoopList.select("+j+");");
										    }
										break;
									    }
								    }
							    }
						    }
					    }
				    }
			    }
		    }
		if(modulesList.getItemCount() > 0)
		    {
			EliminateDuplicatesFromLists();
			hierarchyPanel.removeAll();
			FindAllParents();
		    }
		screenChoice.select("Hierarchy");
		choicePanelLayout.show(choicePanel,"Hierarchy");
		CreateFileList(writer.getUserDir(), true);
		SelectTopModule();
		if(null == writer.auxChannelsVector)
		    {
			writer.auxChannelsVector = ModuleInfo.AllAuxChannels;
		    }
		if(null != writer.auxChannelsVector)
		    {
			for(int i = 0; i < writer.auxChannelsVector.size(); i++)
			    {
				String auxName = (String) writer.auxChannelsVector.elementAt(i);
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(auxName);
				if(null == bi)
				    {
					bi = new BufferInfo();
					bi.Name = auxName;
					writer.buffersHashtable.put(auxName, bi);
				    }
			    }
		    }
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			String modName = modulesList.getItem(i);
			if(null == modName)
			    {
				continue;
			    }
			ModuleInfo mi = (ModuleInfo) writer.modulesHashtable.get(modName);
			if(null == mi)
			    {
				continue;
			    }
			if(null != mi.AuxInputNames)
			    {
				for(int j = 0; j < mi.AuxInputNames.size(); j++)
				    {
					String auxInput = (String) mi.AuxInputNames.elementAt(j);
					BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(auxInput);
					if(null == bi)
					    {
						continue;
					    }
					if(null == bi.readerNames)
					    {
						bi.readerNames = new ArrayList<String>();
					    }
					boolean in_readers_list = false;
					for(int k = 0; k < bi.readerNames.size(); k++)
					    {
						String reader = bi.readerNames.get(k);
						if(auxInput.equals(reader))
						    {
							in_readers_list = true;
							break;
						    }
					    }
					if(!in_readers_list)
					    {
						bi.readerNames.add(modName);
					    }
					if(!bi.channelsHashtable.contains(modName))
					    {
						ChannelInfo ci = new ChannelInfo();
						ci.updateEveryCycle = false;
						ci.Name = auxInput;
						if(null != mi.AuxUpdateEveryCycleNames)
						    {
							for(int auecn_i = 0; auecn_i < mi.AuxUpdateEveryCycleNames.size(); auecn_i++)
							    {
								String auecn_name = (String) mi.AuxUpdateEveryCycleNames.elementAt(auecn_i);
								if(auecn_name.equals(ci.Name))
								    {
									ci.updateEveryCycle = true;
									break;
								    }
							    }
						    }
						ci.ModuleName = modName;
						ci.isWriter = false;
						bi.channelsHashtable.put(modName, ci);
					    }
				    }
			    }
			if(null != mi.AuxOutputNames)
			    {
				for(int j = 0; j < mi.AuxOutputNames.size(); j++)
				    {
					String auxOutput = (String) mi.AuxOutputNames.elementAt(j);
					BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(auxOutput);
					if(null == bi)
					    {
						continue;
					    }
					if(null == bi.writerNames)
					    {
						bi.writerNames = new ArrayList<String>();
					    }
					boolean in_writers_list = false;

					for(int k = 0; k < bi.writerNames.size(); k++)
					    {
						String writer = bi.writerNames.get(k);
						if(auxOutput.equals(writer))
						    {
							in_writers_list = true;
							break;
						    }
					    }
					if(!in_writers_list)
					    {
						bi.writerNames.add(modName);
					    }
					if(!bi.channelsHashtable.contains(modName))
					    {
						ChannelInfo ci = new ChannelInfo();
						ci.Name = auxOutput;
						if(null != mi.AuxUpdateEveryCycleNames)
						    {
							for(int auecn_i = 0; auecn_i < mi.AuxUpdateEveryCycleNames.size(); auecn_i++)
							    {
								String auecn_name = (String) mi.AuxUpdateEveryCycleNames.elementAt(auecn_i);
								if(auecn_name.equals(ci.Name))
								    {
									ci.updateEveryCycle = true;
									break;
								    }
							    }
						    }
						ci.ModuleName = modName;
						ci.isWriter = true;
						bi.channelsHashtable.put(modName, ci);
					    }
				    }
			    }
		    }

		if(!writer.serversHashtable.containsKey(writer.AppName+"svr"))
		    {
			ServerInfo si = new ServerInfo(writer.AppName+"svr","is_server=true;\nhost=localhost;\n");
			writer.curServer = si;
			if(debug_on)
			    {
				System.out.println("Adding "+si);
			    }
			writer.serversHashtable.put(si.Name,si);
			CreateBufferList();
			for(int i = 0; i < bufsInServerList.getItemCount(); i++)
			    {
				String bufname = bufsInServerList.getItem(i);
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(bufname);
				if(null != bi)
				    {
					if(null == bi.si)
					    {
						si.bufferNames.addElement(bufname);
						bufsInServerList.select(i);
						bi.si = si;
					    }
					else if(bi.si.Name.equals(writer.AppName+"svr"))
					    {
						si.bufferNames.addElement(bufname);
						bufsInServerList.select(i);
					    }
				    }
			    }
		    }
		else
		    {
			CreateBufferList();
		    }
		Enumeration serverKeys = writer.serversHashtable.keys();
		while(serverKeys.hasMoreElements())
		    {
			String server_name = (String) serverKeys.nextElement();
			boolean add_server = true;
			for(int i = 0; i < serverList.getItemCount(); i++)
			    {
				String temp_server_name = serverList.getItem(i);
				if(temp_server_name.equals(server_name))
				    {
					add_server = false;
					break;
				    }
			    }
			if(add_server)
			    {
				serverList.add(server_name);
			    }
		    }
		if(debug_on)
		    {
			System.out.println("RCS-Design: Previous configuration read.");
		    }
		UpdateWriter();
		serverList.select(0);
		writer.curServer = null;
		UpdateServerInfo();
		writer.curmainLoop = null;
		mainLoopList.select(0);
		UpdateMainLoopInfo();
		CreateFileList(userdirField.getText(),false);
		if(debug_on)
		    {
			System.out.println("End of InitializeLists()");
			System.out.println("");
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	read_everything_needed = false;
	HierarchyPanel.updating_hierarchy = orig_updating_hierarchy;
    }

    void EliminateDuplicatesFromLists()
    {
	EliminateDuplicatesFromList(modulesList);
	EliminateDuplicatesFromList(mainLoopList);
	EliminateDuplicatesFromList(subordinatesList);
	EliminateDuplicatesFromList(modsInLoopList);
	EliminateDuplicatesFromList(libsList);
	EliminateDuplicatesFromList(includesList);
    }

    @SuppressWarnings("unchecked")
    void EliminateDuplicatesFromList(java.awt.List lst)
    {
	try
	    {
		Vector v = new Vector();
		boolean any_repeats = false;
		for(int i = 0; i < lst.getItemCount(); i++)
		    {
			boolean repeated = false;
			String stri = lst.getItem(i);
			for(int j = 0; j < i; j++)
			    {
				String strj = lst.getItem(j);
				if(stri.equals(strj))
				    {
					repeated = true;
					any_repeats = true;
					break;
				    }
			    }
			if(!repeated)
			    {
				ListElement le = new  ListElement();
				le.selected = lst.isIndexSelected(i);
				le.item = stri;
				v.addElement(le);
			    }
		    }
		if(any_repeats)
		    {
			lst.removeAll();
			for(int i = 0; i < v.size(); i++)
			    {
				ListElement le = (ListElement) v.elementAt(i);
				lst.add(le.item);
				if(le.selected)
				    {
					lst.select(i);
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    boolean GeneratingEverything = false;

    void SelectTopModule()
    {
	try
	    {
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			String modName = modulesList.getItem(i);
			ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
			if(null == modInfo)
			    {
				continue;
			    }
			if(modInfo.parent == null && modInfo.generation == 0)
			    {
				cmdList.removeAll();
				writer.curModule=modInfo;
				LoadListsFromCurModule();
				modulesList.select(i);
				ItemEvent select_event = new ItemEvent(modulesList, ItemEvent.ITEM_STATE_CHANGED, i,  ItemEvent.SELECTED);
				modulesList.dispatchEvent(select_event);
				break;
			    }
		    }
		String selectedModuleName = modulesList.getSelectedItem();
		if(null != selectedModuleName)
		    {
			ModuleInfo module = (ModuleInfo) writer.modulesHashtable.get(selectedModuleName);
			if(null != module)
			    {
				Rectangle rect = hierarchyPanel.getBounds();
				hierarchyPanel.scroll_x = module.x - hierarchyPanel.getSize().width/2+rect.x;
				hierarchyHorzScrollbar.setValue(hierarchyPanel.scroll_x);
				hierarchyPanel.scroll_y = module.y +HierarchyPanel.MODULE_YOFFSET - HierarchyPanel.MODULE_HEIGHT+rect.y;
				if(null != module.parent)
				    {
					hierarchyPanel.scroll_y += HierarchyPanel.MODULE_YOFFSET - HierarchyPanel.MODULE_HEIGHT;
				    }
				hierarchyVertScrollbar.setValue(hierarchyPanel.scroll_y);
				hierarchyPanel.repaint();
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    private void FindAllParents()
    {
	try
	    {
		hierarchyPanel.setCountList(modulesList);
		hierarchyPanel.vertScrollbar = hierarchyVertScrollbar;
		hierarchyPanel.horzScrollbar = hierarchyHorzScrollbar;
		hierarchyPanel.FindAllParents(
					      writer.modulesHashtable, 
					      loadPanel);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    protected void ImportController()
    {

	String filename=null;

	Frame tmpFrame = new Frame();
	ModifiedFileDialog file_dialog = new ModifiedFileDialog(tmpFrame, "Import Controller", ModifiedFileDialog.LOAD);
	if(null != ImportDir)
	    {
		file_dialog.setDirectory(ImportDir);
	    }
	else
	    {
		file_dialog.setDirectory(writer.getUserDir());
	    }
	//file_dialog.setFilenameFilter(new SimpleFileFilter("*.cfg"));
	file_dialog.setVisible(true);
	String dir = file_dialog.getDirectory();
	filename = file_dialog.getDirectory() + file_dialog.getFile();
	if(null == filename)
	    {
		System.out.println("No config file provided -- aborting import.");
		return;
	    }
	if(filename.length() <= 0)
	    {
		System.out.println("No config file provided -- aborting import.");
		return;
	    }
	QueryStringDialog qsd = new QueryStringDialog(tmpFrame,"Import Subsystem","Please enter the name for a subsystem\n to import this controller as.", "");
	while(!qsd.done)
	    {
		try
		    {
			Thread.sleep(100);
		    }
		catch(InterruptedException ie)
		    {
			break;
		    }
	    }
	String subsystem = qsd.queryString;
	if(subsystem == null)
	    {
		System.out.println("No subsystem provided -- aborting import.");
		return;
	    }
	if(subsystem.length() <= 0)
	    {
		System.out.println("No subsystem provided -- aborting import.");
		return;
	    }
	ImportController(dir, filename, subsystem);
	RcsLibDirCheck();

    }


    @SuppressWarnings("unchecked")
    protected void OpenController()
    {
	try
	    {
		HierarchyPanel.updating_hierarchy = true;
		if(null != loadPanel)
		    {
			loadPanel.setVisible(true);
			loadPanel.setEnabled(false);
		    }
		String filename=null;

		Frame tmpFrame = new Frame();
		String user_dir=".";
		try
		    {
			user_dir= System.getProperty("user.dir");
			if(!user_dir.endsWith(File.separator))
			    {
				user_dir += File.separator;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		if(debug_on)
		    {
			System.out.println("OpenController : user_dir="+user_dir);
		    }
		ModifiedFileDialog file_dialog = new ModifiedFileDialog(tmpFrame, "Open Controller", ModifiedFileDialog.LOAD,user_dir,"*.cfg");
		file_dialog.setDirectory(writer.getUserDir());
		//file_dialog.setFilenameFilter(new SimpleFileFilter("*.cfg"));
		file_dialog.setVisible(true);
		if(null == file_dialog.getFile())
		    {
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		filename = file_dialog.getDirectory() + file_dialog.getFile();
		if(filename.length() < 1)
		    {
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		if(filename.endsWith(File.separator))
		    {
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }

		if(!filename.endsWith(".cfg"))
		    {
			Alert("RCS-Design configuration file to open must end with .cfg\nfilename="+filename);
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		writer.ConfigFile = filename;
		if(null != main_window)
		    {
			if(null != writer && null != writer.ConfigFile)
			    {
				main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+") -- "+writer.ConfigFile);
			    }
			else
			    {
				main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+")");
			    }
		    }
		writer.setUserDir(file_dialog.getDirectory());
		userdirField.setText(writer.getUserDir());
		File userdirFile = new File(writer.getUserDir());
		String appname = writer.ConfigFile;
		int pathindex = appname.lastIndexOf(File.separator);
		if(pathindex > 0)
		    {
			appname = appname.substring(pathindex+1);
		    }
		if(appname.endsWith(".cfg"))
		    {
			appname = appname.substring(0, appname.length() -4);
		    }
		appnameField.setText(appname);
		writer.AppName = appname;
		appdirField.setText("");
		modulesList.removeAll();
		cmdList.removeAll();
		subordinatesList.removeAll();
		auxList.removeAll();
		libsList.removeAll();
		includesList.removeAll();
		mainLoopList.removeAll();
		mainLoopList.add(writer.AppName);
		mainLoopList.select(0);
		modsInLoopList.removeAll();
		writer.mainloopsHashtable.clear();
		rcsdesignMainLoopInfo tempMli = new rcsdesignMainLoopInfo(writer.AppName);
		writer.mainloopsHashtable.put(writer.AppName, tempMli );
		hierarchyPanel.removeAll();
		read_everything_needed=true;
		if(debug_on)
		    {
			System.out.println("rcsDesign.OpenController(): StartLongFunction(READ_CONFIGURATION);");
			System.out.println("read_everything_needed="+read_everything_needed);
		    }
		StartLongFunction(READ_CONFIGURATION);
		EnableAllControls();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	HierarchyPanel.updating_hierarchy = false;
    }

    @SuppressWarnings("unchecked")
   protected void NewController()
    {

	try
	    {
		creating_new_controller=true;
		HierarchyPanel.updating_hierarchy = true;

		String filename=null;

		Frame tmpFrame = new Frame();
		writer.ConfigFile=null;
		writer.setUserDir(null);
		writer.AppName=null;
		QueryStringDialog qsd = new QueryStringDialog(tmpFrame,"RCS-Design","Please enter the name for the new controller.","");
		while(!qsd.done)
		    {
			try
			    {
				Thread.sleep(100);
			    }
			catch(InterruptedException ie)
			    {
				break;
			    }
		    }
		String appname = qsd.queryString;
		if(appname == null)
		    {
			Alert("No application name entered.");
			HierarchyPanel.updating_hierarchy = false;
			creating_new_controller=false;
			return;
		    }
		if(appname.length() < 1)
		    {
			Alert("No application name entered.");
			HierarchyPanel.updating_hierarchy = false;
			creating_new_controller=false;
			return;
		    }
		writer.AppName = appname;
		qsd = new QueryStringDialog(tmpFrame,"RCS-Design","Please enter the Developer's directory for the new controller.",
					    URL_and_FileLoader.current_directory+File.separator+writer.AppName);
		while(!qsd.done)
		    {
			try
			    {
				Thread.sleep(100);
			    }
			catch(InterruptedException ie)
			    {
				break;
			    }
		    }
		writer.setUserDir(qsd.queryString);
		if(writer.getUserDir() == null)
		    {
			Alert("No directory entered.");
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		if(writer.getUserDir().length() < 1)
		    {
			Alert("No directory  entered.");
			HierarchyPanel.updating_hierarchy = false;
			creating_new_controller=false;
			return;
		    }
		if(!writer.getUserDir().endsWith(appname) && !writer.getUserDir().endsWith(appname+File.separator))
		    {
			if(!writer.getUserDir().endsWith(File.separator))
			    {
				writer.setUserDir(writer.getUserDir() + File.separator);
			    }
			writer.setUserDir(writer.getUserDir() + appname);
		    }
		File userDirFile = new File(writer.getUserDir());
		writer.ConfigFile = new File(userDirFile, appname+".cfg").getPath();
		writer.setUserDir(userDirFile.getPath());
		userDirFile.mkdirs();
		String default_release_directory = writer.RcsLibDir+File.separator+"controllers";
		qsd = new QueryStringDialog(tmpFrame,"RCS-Design","Please enter the Application Release directory for the new controller.",
					    default_release_directory+File.separator+writer.AppName);
		while(!qsd.done)
		    {
			try
			    {
				Thread.sleep(100);
			    }
			catch(InterruptedException ie)
			    {
				break;
			    }
		    }
		writer.AppDir = qsd.queryString;
		File appDirFile = new File(writer.AppDir);
		appDirFile.mkdirs();
		appdirField.setText(writer.AppDir);
		writer.ConfigFile = new File(userDirFile, appname+".cfg").getPath();
		appnameField.setText(appname);
		writer.AppName = appname;
		userdirField.setText(writer.getUserDir());
		if(null != main_window)
		    {
			if(null != writer && null != writer.ConfigFile)
			    {
				main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+") -- "+writer.ConfigFile);
			    }
			else
			    {
				main_window.setTitle("RCS Design("+rcs.RCS_VERSION.version_string+")");
			    }
		    }
		codeGenerationApplet.set_m_modulesHashTable(writer.modulesHashtable);
		codeGenerationApplet.set_serversHashtable(writer.serversHashtable);
		modulesList.removeAll();
		cmdList.removeAll();
		subordinatesList.removeAll();
		auxList.removeAll();
		libsList.removeAll();
		includesList.removeAll();
		mainLoopList.removeAll();
		mainLoopList.add(writer.AppName);
		mainLoopList.select(0);
		serverList.removeAll();
		serverList.add(writer.AppName+"svr");
		serverList.select(0);
		writer.serversHashtable.clear();
		String serverInfoString = "is_server=true;\nhost=localhost;\n";
		ServerInfo si = new ServerInfo(writer.AppName+"svr", serverInfoString);
		writer.serversHashtable.put(writer.AppName+"svr",si);
		modsInLoopList.removeAll();
		hierarchyPanel.removeAll();
		writer.mainloopsHashtable.clear();
		rcsdesignMainLoopInfo tempMli = new rcsdesignMainLoopInfo(writer.AppName);
		writer.mainloopsHashtable.put(writer.AppName, tempMli );
		File runCmdFile = null;
		try
		    {
			if(userDirFile.exists())
			    {
				if(writer.mswinDevPlat)
				    {
					runCmdFile = new File(userDirFile, writer.AppName+".bat");
				    }
				else
				    {
					runCmdFile = new File(userDirFile, "run."+writer.AppName);
				    }
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }

		String run_cmd_end = "";
		if(null != runCmdFile)
		    {
			run_cmd_end = runCmdFile.toString();
		    }
		runCommandField.setText(run_cmd_end);
		screenChoice.select("Hierarchy");
		choicePanelLayout.show(choicePanel,"Hierarchy");
		hierarchyPanel.monitored_repaint();
		monitored_repaint();
		RcsLibDirCheck();
		if(debug_on)
		    {
			System.out.println("appname = "+appname);
			System.out.println("userDirFile = "+userDirFile);
			System.out.println("writer.UserDir = "+writer.getUserDir());
			System.out.println("writer.ConfigFile = "+writer.ConfigFile);
		    }
		EnableAllControls();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	HierarchyPanel.updating_hierarchy = false;
	creating_new_controller=false;
    }

    @SuppressWarnings("unchecked")
    protected void ImportController(String importdir, String filename, String subsystem)
    {
	HierarchyPanel.updating_hierarchy = true;
	String parseString;
	boolean insideComment = false;
	boolean insideModule = false;
	URL_and_FileLoader loader = null;
	ModuleInfo currentModule;
	int starting_load_button_count = 0;
	int indexCppCommentBegin;
	int indexCCommentBegin;
	int indexCCommentEnd;
	int indexQuoteBegin;
	int indexQuoteEnd;
	int content_length;
	int bytes_read;
	String top_imported_module = null;
	Vector importedModules = new Vector();
	String libname= "lib"+importdir+".a";
	System.out.println("Importing "+filename+" from "+importdir+" as the "+subsystem+" subsystem.");
	if(importdir.length() > 1 && importdir.endsWith(File.separator))
	    {
		importdir = importdir.substring(0, importdir.length() -1);
	    }

	try
	    {
		if(null == writer.modulesHashtable)
		    {
			currentModule = null;
		    }
		File configFile = new File(filename);
		String configFileParent = configFile.getParent();
		libname  = filename;
		int pathindex = libname.lastIndexOf(File.separator);
		if(pathindex >= 0)
		    {
			libname = libname.substring(pathindex+1);
		    }
		if(libname.endsWith(".cfg"))
		    {
			libname = libname.substring(0, libname.length() - 4);
		    }
		if(writer.mswinDevPlat)
		    {
			libname = libname+".lib";
		    }
		else
		    {
			libname = "lib"+libname+".a";
		    }
		if(null != configFileParent)
		    {

			includesList.add(configFileParent+File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"include");
			libsList.add(configFileParent+File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+libname);
		    }
		File dirFile = new File(importdir);
		writer.setUserDir(userdirField.getText());
		File userDirFile = new File(writer.getUserDir());
		String ini_files[] = dirFile.list(new SimpleFileFilter("*.ini"));
		for(int i = 0; i < ini_files.length; i++)
		    {
			try
			    {
				File iniFile = new File(dirFile, ini_files[i]);
				File iniCopy = new File(userDirFile, ini_files[i]);
				if(iniFile.exists() && !iniCopy.exists())
				    {
					FileInputStream fis = new FileInputStream(iniFile);
					byte bytes[] = new byte[(int) iniFile.length()];
					fis.read(bytes);
					FileOutputStream fos = new FileOutputStream(iniCopy);
					fos.write(bytes);
					fis.close();
					fos.close();
				    }
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
				continue;
			    }
		    }

		if(null == filename)
		    {
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		if(filename.length() < 1)
		    {
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Importing Controller "+filename);
		    }
		currentModule = null;
		try
		    {
			currentModule = new ModuleInfo( new DiagNMLMsgDictCreator(), rcs.nml.NMLConnection.Creator );
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
			HierarchyPanel.updating_hierarchy = false;
			return;
		    }
		loader = new URL_and_FileLoader(filename);
		loadPanel.content_length = loader.content_length;
		bytes_read = 0;
		content_length = loadPanel.content_length;
		if(debug_on)
		    {
			System.out.println("Parsing "+ filename+" (length = "+loadPanel.content_length+")");
		    }
		while(true)
		    {
			parseString = loader.readLine();
			if(debug_on)
			    {
				System.out.println(parseString);
			    }
			if(parseString == null)
			    {
				break;
			    }
			loadPanel.bytes_read += parseString.length();
			bytes_read = loadPanel.bytes_read;
			loadPanel.updateDisplay();
			if(parseString.length() < 1)
			    {
				continue;
			    }
			while(true)
			    {
				if(parseString.charAt(0) != ' ')
				    {
					break;
				    }
				parseString = parseString.substring(1);
			    }
			if(parseString.regionMatches(true,0,"END",0,3))
			    {
				break;
			    }
			if(parseString.regionMatches(false,0,"#",0,1))
			    {
				continue;
			    }
			if(parseString.regionMatches(false,0,"\n",0,1))
			    {
				continue;
			    }
			if(parseString.regionMatches(false,0,"\r",0,1))
			    {
				continue;
			    }
			indexQuoteBegin = parseString.indexOf('"');
			indexQuoteEnd = parseString.lastIndexOf('"');
			indexCCommentBegin = parseString.indexOf("/*");
			if(indexCCommentBegin < indexQuoteEnd &&
			   indexCCommentBegin > indexQuoteBegin)
			    {
				indexCCommentBegin = -1;
			    }
			while(indexCCommentBegin != -1 || insideComment)
			    {
				String tempString;
				indexCCommentEnd = parseString.indexOf("*/");
				indexQuoteBegin = parseString.indexOf('"');
				indexQuoteEnd = parseString.lastIndexOf('"');
				if(indexCCommentEnd < indexQuoteEnd &&
				   indexCCommentEnd > indexQuoteBegin)
				    {
					indexCCommentEnd = -1;
				    }
				if(indexCCommentEnd != -1)
				    {
					if(indexCCommentBegin > 0)
					    {
						tempString = parseString.substring(0,indexCCommentBegin);
					    }
					else
					    {
						tempString = "";
					    }
					parseString = tempString + parseString.substring(indexCCommentEnd+2);
					insideComment = false;
				    }
				else
				    {
					if(indexCCommentBegin > 0 && !insideComment)
					    {
						parseString = parseString.substring(0,indexCCommentBegin);
						insideComment = true;
					    }
					else
					    {
						parseString = "";
						insideComment = true;
						break;
					    }
				    }
				indexQuoteBegin = parseString.indexOf('"');
				indexQuoteEnd = parseString.lastIndexOf('"');
				indexCCommentBegin = parseString.indexOf("/*");
				if(indexCCommentBegin < indexQuoteEnd &&
				   indexCCommentBegin > indexQuoteBegin)
				    {
					indexCCommentBegin = -1;
				    }
			    }
			if(parseString.length() < 1)
			    {
				continue;
			    }
			indexQuoteBegin = parseString.indexOf('"');
			indexQuoteEnd = parseString.lastIndexOf('"');
			indexCppCommentBegin = parseString.indexOf("//");
			if(indexCppCommentBegin < indexQuoteEnd &&
			   indexCppCommentBegin > indexQuoteBegin)
			    {
				indexCppCommentBegin = -1;
			    }
			if(indexCppCommentBegin != -1)
			    {
				if(indexCppCommentBegin == 0)
				    {
					continue;
				    }
				parseString = parseString.substring(0,indexCppCommentBegin);
			    }
			if(insideModule && currentModule != null)
			    {
				if(parseString.regionMatches(false,0,"}",0,1))
				    {
					insideModule = false;
					if(debug_on)
					    {
						System.out.println("Found module = "+currentModule.Name);
					    }
					if(currentModule.Name.equalsIgnoreCase("options"))
					    {
						currentModule = new ModuleInfo( new DiagNMLMsgDictCreator(), rcs.nml.NMLConnection.Creator );
						continue;
					    }
					if(!writer.modulesHashtable.containsKey(currentModule.Name))
					    {
						currentModule.m_loadingPanel = loadPanel;
						ModuleInfo.previous_url_loaded = filename;
						currentModule.subsystem = subsystem;
						currentModule.importing = true;
						currentModule.LoadInfo();
						currentModule.baseModuleName = currentModule.Name;
						currentModule.baseClassName = currentModule.moduleClassName;
						System.out.println(currentModule.Name +" derived from "+currentModule.baseClassName);
						if(null != currentModule.cmdsTypeFile)
						    {
							currentModule.baseClassCmdsTypeFile = importdir + File.separator + currentModule.cmdsTypeFile;
						    }
						if(null != currentModule.statsTypeFile)
						    {
							currentModule.baseClassStatsTypeFile = importdir + File.separator + currentModule.statsTypeFile;
						    }
						currentModule.Name = subsystem+"_"+currentModule.Name;
						currentModule.moduleClassName = currentModule.Name.toUpperCase()+"_MODULE";
						if(null == currentModule.predefined_type_files)
						    {
							currentModule.predefined_type_files = new Vector();
						    }
						currentModule.predefined_type_files.addElement(importdir+File.separator+currentModule.cmdsTypeFile);
						if(!currentModule.cmdsTypeFile.equals(currentModule.statsTypeFile))
						    {
							currentModule.predefined_type_files.addElement(importdir+File.separator+currentModule.cmdsTypeFile);
						    }
						if(singleDir)
						    {
							currentModule.cmdsTypeFile = currentModule.Name+"n"+writer.hpp_ext;
							currentModule.statsTypeFile =currentModule.Name+"n"+writer.hpp_ext;
						    }
						else
						    {
							currentModule.cmdsTypeFile = "src"+File.separator+"intf"+File.separator+currentModule.Name+"n"+writer.hpp_ext;
							currentModule.statsTypeFile = "src"+File.separator+"intf"+File.separator+currentModule.Name+"n"+writer.hpp_ext;
							currentModule.SourceCodeDirectory = "src"+File.separator+currentModule.Name;
						    }
						currentModule.cmdsBaseClass = new Vector();
						if(null != currentModule.m_stat_read_Connection)
						    {
							if(null != currentModule.m_stat_read_Connection.get_buffer_name())
							    {
								currentModule.m_stat_read_Connection.set_buffer_name(currentModule.Name+"_sts");
							    }
						    }
						if(null != currentModule.m_cmd_read_Connection)
						    {
							if(null != currentModule.m_cmd_read_Connection.get_buffer_name())
							    {
								currentModule.m_cmd_read_Connection.set_buffer_name(currentModule.Name+"_cmd");
							    }
						    }
						if(null != currentModule.MainLoopName)
						    {
							currentModule.MainLoopName=subsystem+currentModule.MainLoopName;
						    }
						if(debug_on)
						    {
							System.out.println("currentModule.cmdsAvailable.size() ="+currentModule.cmdsAvailable.size());
						    }
						for(int i = 0; i < currentModule.cmdsAvailable.size(); i++)
						    {
							String cmd = (String) currentModule.cmdsAvailable.elementAt(i);
							System.out.println("cmd = "+cmd);
							currentModule.cmdsBaseClass.addElement(cmd);
						    }
						if(null == top_imported_module)
						    {
							top_imported_module = currentModule.Name;
						    }
						for(int i = 0; i < currentModule.children_names.size(); i++)
						    {
							String child = (String) currentModule.children_names.elementAt(i);
							if(child.equals(top_imported_module))
							    {
								top_imported_module = currentModule.Name;
							    }
						    }

						currentModule.NMLConfigurationFile = appnameField.getText()+".nml";
						String relInc = currentModule.releaseIncludeDirectory;
						if(null == relInc)
						    {
							relInc = importdir+File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"include";
						    }
						if(relInc.startsWith("."))
						    {
							relInc = importdir+relInc.substring(1);
						    }
						includesList.add(relInc);
						if(null == currentModule.externalIncludeDirectories)
						    {
							currentModule.externalIncludeDirectories = new Vector();
						    }
						currentModule.externalIncludeDirectories.addElement(relInc);

						String relLib = currentModule.releaseLibrary;
						if(null == relLib)
						    {
							relLib = importdir+File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+libname;
						    }
						if(relLib.startsWith("."))
						    {
							relLib = importdir+relLib.substring(1);
						    }
						libsList.add(relLib);
						if(null == currentModule.externalLibraries)
						    {
							currentModule.externalLibraries = new Vector();
						    }
						currentModule.externalLibraries.addElement(relLib);
						currentModule.releaseIncludeDirectory  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"include";
						if(writer.mswinDevPlat)
						    {
							currentModule.releaseLibrary  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+appnameField.getText()+".lib";
						    }
						else
						    {
							currentModule.releaseLibrary  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+"lib"+appnameField.getText()+".a";
						    }
						writer.modulesHashtable.put((Object)currentModule.Name,(Object) currentModule);
						if(null != modulesList)
						    {
							modulesList.add(currentModule.Name);
						    }
						if(null == currentModule.predefined_type_files)
						    {
							currentModule.predefined_type_files = new Vector();
						    }

						loadPanel.content_length = content_length;
						loadPanel.bytes_read = bytes_read;
						loadPanel.URLname = filename;
						loadPanel.repaint(500);
					    }
					importedModules.addElement(currentModule);
					currentModule = new ModuleInfo( new DiagNMLMsgDictCreator(), rcs.nml.NMLConnection.Creator );
					continue;
				    }
				currentModule.Info += parseString;
				continue;
			    }

			int index = parseString.indexOf("{");
			if(index <= 0)
			    {
				continue;
			    }
			currentModule.Name = (parseString.substring(0,index));
			int parseStringLength = parseString.length();
			if(index+1 < parseStringLength)
			    {
				currentModule.Info = parseString.substring(index+1);
			    }
			insideModule = true;
		    }
		loadPanel.URLname = filename;
		loadPanel.content_length = content_length;
		loadPanel.repaint(500);
		String orig_cur_module_name = modulesList.getSelectedItem();
		if(null != orig_cur_module_name)
		    {
			writer.curModule = (ModuleInfo) writer.modulesHashtable.get(orig_cur_module_name);
			if(null != writer.curModule)
			    {
				writer.curModule.children_names.addElement(top_imported_module);
				for(int i = 0; i < subordinatesList.getItemCount(); i++)
				    {
					if(subordinatesList.getItem(i).equals(top_imported_module))
					    {
						subordinatesList.select(i);
					    }
				    }
			    }

		    }
		int max_imported_module_number = 0;
		for(int i = 0; i < importedModules.size(); i++)
		    {
			ModuleInfo mi = (ModuleInfo) importedModules.elementAt(i);
			if(mi.module_number > max_imported_module_number)
			    {
				max_imported_module_number = mi.module_number;
			    }
			ModuleInfo.max_module_number++;
			mi.module_number = ModuleInfo.max_module_number;
		    }
		Enumeration modulesEnumeration = writer.modulesHashtable.elements();
		while(modulesEnumeration.hasMoreElements())
		    {
			ModuleInfo mi = (ModuleInfo) modulesEnumeration.nextElement();
			if(mi.module_number <= max_imported_module_number)
			    {
				ModuleInfo.max_module_number++;
				mi.module_number = ModuleInfo.max_module_number;
			    }
		    }
		codeGenerationApplet.InitializeClassList();
		if(list_modules_by_number)
		    {
			SortModulesList();
		    }
		InitializeLists();
		HierarchyPanel.updating_hierarchy = false;
	    }catch(Exception e) {
	    System.err.println("Error: "+ e.toString());
	    e.printStackTrace();
	    HierarchyPanel.updating_hierarchy = false;
	}


    }

    @SuppressWarnings("unchecked")
   void SortModulesList()
    {
	try
	    {
		AlphabetizedList al = (AlphabetizedList) modulesList;
		if(null != al)
		    {
			al.disable_alphabetizing = true;
		    }
	    }
	catch(ClassCastException cce)
	    {
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	try
	    {
		AlphabetizedList al = (AlphabetizedList) subordinatesList;
		if(null != al)
		    {
			al.disable_alphabetizing = true;
		    }
	    }
	catch(ClassCastException cce)
	    {
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	try
	    {
		Vector mod1List = new Vector();
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			mod1List.addElement(modulesList.getItem(i));
		    }
		modulesList.removeAll();
		subordinatesList.removeAll();
		for(int i = 0; i < mod1List.size(); i++)
		    {
			String modi_name = (String) mod1List.elementAt(i);
			ModuleInfo modi_mi = (ModuleInfo) writer.modulesHashtable.get(modi_name);
			int modi_number = modi_mi.module_number;
			for(int j = i+1; j < mod1List.size(); j++)
			    {
				String modj_name = (String) mod1List.elementAt(j);
				ModuleInfo modj_mi = (ModuleInfo) writer.modulesHashtable.get(modj_name);
				int modj_number = modj_mi.module_number;
				if(modj_number < modi_number)
				    {
					mod1List.setElementAt(modj_name, i);
					mod1List.setElementAt(modi_name, j);
					modi_number = modj_number;
					modi_name = modj_name;
				    }
			    }
		    }
		for(int i = 0; i < mod1List.size(); i++)
		    {
			modulesList.add((String) mod1List.elementAt(i));
			subordinatesList.add((String) mod1List.elementAt(i));
		    }
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			String modName = modulesList.getItem(i);
			ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
			if(null == modInfo)
			    {
				continue;
			    }
			if(modInfo.parent == null && modInfo.generation == 0)
			    {
				modulesList.select(i);
				writer.curModule = null;
				ItemEvent select_event = new ItemEvent(modulesList, ItemEvent.ITEM_STATE_CHANGED, i, ItemEvent.SELECTED);
				modulesList.dispatchEvent(select_event);
				break;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    @SuppressWarnings("unchecked")
    void BuildHierarchyDisplay()
    {
	try
	    {
		HierarchyPanel.updating_hierarchy = true;
		if(writer.curModule != null)
		    {
			String subnames[] = subordinatesList.getSelectedItems();
			writer.curModule.children_names.removeAllElements();
			for(int i = 0; i < subnames.length; i++)
			    {
				if(subnames[i] != null)
				    {
					writer.curModule.children_names.addElement(subnames[i]);
				    }
			    }

			if(cmdList.getItemCount() > 0)
			    {
				writer.curModule.cmdsAvailable.removeAllElements();
				for(int i = 0; i < cmdList.getItemCount(); i++)
				    {
					writer.curModule.cmdsAvailable.addElement( cmdList.getItem(i));
				    }
			    }
		    }
		hierarchyPanel.removeAll();
		FindAllParents();
		String selectedModuleName = modulesList.getSelectedItem();
		if(null != selectedModuleName)
		    {
			ModuleInfo module = (ModuleInfo) writer.modulesHashtable.get(selectedModuleName);
			if(null != module)
			    {
				Rectangle rect = hierarchyPanel.getBounds();
				hierarchyPanel.scroll_x = module.x - hierarchyPanel.getSize().width/2+rect.x;
				hierarchyHorzScrollbar.setValue(hierarchyPanel.scroll_x);
				hierarchyPanel.scroll_y = module.y +HierarchyPanel.MODULE_YOFFSET - HierarchyPanel.MODULE_HEIGHT+rect.y;
				if(null != module.parent)
				    {
					hierarchyPanel.scroll_y += HierarchyPanel.MODULE_YOFFSET - HierarchyPanel.MODULE_HEIGHT;
				    }
				hierarchyVertScrollbar.setValue(hierarchyPanel.scroll_y);
			    }
		    }
		hierarchyPanel.UpdateDisplay(true);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	HierarchyPanel.updating_hierarchy = false;
    }


    @SuppressWarnings("unchecked")
    void DeleteModuleFiles(String mod_to_delete)
    {
	try
	    {
		if(mod_to_delete == null)
		    {
			return;
		    }
		if(mod_to_delete.length() < 1)
		    {
			return;
		    }
		File moduleDirFile = null;
		if(singleDir)
		    {
			moduleDirFile = new File(writer.getUserDir());

		    }
		else
		    {
			moduleDirFile = new File(writer.getUserDir() + File.separator + "src"+File.separator+mod_to_delete);
		    }
		File deletedDirFile = new File(writer.getUserDir() + File.separator + "trash");
		if(!deletedDirFile.exists())
		    {
			deletedDirFile.mkdirs();
		    }
		if(moduleDirFile.exists() && moduleDirFile.isDirectory())
		{
		    String filenames[] = moduleDirFile.list();
		    for(int i = 0; i < filenames.length; i++)
			{
			    File fileToDelete = new File(moduleDirFile, filenames[i]);
			    File deletedFile = new File(deletedDirFile, filenames[i]);
			    fileToDelete.renameTo(deletedFile);
			}
		    moduleDirFile.delete();
		}
		File intfDirFile = null;
		if(singleDir)
		    {
			intfDirFile =new File(writer.getUserDir()); 
		    }
		else
		    {
			intfDirFile = new File(writer.getUserDir() + File.separator + "src"+File.separator+"intf");
		    }
		if(intfDirFile.exists() && intfDirFile.isDirectory())
		    {
			File headerFile = new File(intfDirFile, mod_to_delete+"n"+writer.hpp_ext);
			if(headerFile.exists())
			    {
				File deletedHeaderFile = new File(deletedDirFile, mod_to_delete+"n"+writer.hpp_ext);
				headerFile.renameTo(deletedHeaderFile);
			    }
			File cppFile = new File(intfDirFile, mod_to_delete+"n"+writer.cpp_ext);
			if(cppFile.exists())
			    {
				File deletedCppFile = new File(deletedDirFile, mod_to_delete+"n"+writer.cpp_ext);
				cppFile.renameTo(deletedCppFile);
			    }
			File genFile = new File(intfDirFile, mod_to_delete+"n.gen");
			if(genFile.exists())
			    {
				File deletedGenFile = new File(deletedDirFile, mod_to_delete+"n.gen");
				genFile.renameTo(deletedGenFile);
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }

    }

    void RunMakeBatchFile(String envp[])
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("RunMakeBatchFile . . .");
		    }
		writer.setUserDir(userdirField.getText());
		if(debug_on)
		    {
			System.out.println("writer.UserDir = "+writer.getUserDir());
		    }
		File userdirFile = new File(writer.getUserDir());
		writer.MakeCommand = makeCommandField.getText();
		String command = writer.MakeCommand;
		writer.TerminalCommand = terminalCommandField.getText();
		String terminal_launch_command = writer.TerminalCommand;
		if(terminal_launch_command.length() > 0 &&
		   !terminal_launch_command.endsWith(" "))
		    {
			terminal_launch_command += " ";
		    }
		command = ReplaceVars(terminal_launch_command +command);
		if(debug_on)
		    {
			System.out.println("command = "+command);
			System.out.println("Creating new MakeFileRunner . . .");
		    }
		MakeFileRunner runner  = new MakeFileRunner(userdirFile, command, envp);
		if(debug_on)
		    {
			System.out.println("Creating new Thread . . .");
		    }
		Thread runner_thread = new Thread(runner);
		if(debug_on)
		    {
			System.out.println("Starting new Thread . . .");
		    }
		runner_thread.start();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    protected String getFilesListItem(int i)
    {
	String str = null;
	try
	    {
		if(i < 0)
		    {
			return null;
		    }
		str = filesList.getItem(i);
		if(null == str)
		    {
			return null;
		    }
		if(str.endsWith(" (*)"))
		    {
			return str.substring(0, str.length()-4);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return str;
    }

    protected void RemoveBackups()
    {
	try
	    {
//	    if( JOptionPane.OK_OPTION != JOptionPane.showConfirmDialog(this,"Remove all backup files?","",JOptionPane.YES_NO_OPTION))
//	    {
//		return;
//	    }
//	    
//		QueryDialog qd = Query("Remove all backup files?");
//		qd.show();
//		while(!qd.done)
//		    {
//			Thread.sleep(500);
//		    }
//		if(!qd.ok)
//		    {F
//			return;
//		    }
		writer.setUserDir(userdirField.getText());
		File userDirFile = new File(writer.getUserDir());
		if(!userDirFile.exists() || ! userDirFile.canWrite())
		    {
			System.out.println("Can not remove backups from "+writer.getUserDir());
			return;
		    }
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String relFile = getFilesListItem(i);
			for(int j = 0; j < 10; j++)
			    {
				String backupFileName = relFile+".~"+j+"~";
				File backupFile = new File(userDirFile, backupFileName);
				System.out.println("Checking "+backupFile.getPath());
				if(!backupFile.exists() || !backupFile.canWrite())
				    {
					continue;
				    }
				System.out.println("Deleting "+backupFile.getPath());
				backupFile.delete();
			    }
			for(int j = 0; j < 10; j++)
			    {
				String backupFileName = relFile+"~"+j+"~";
				File backupFile = new File(userDirFile, backupFileName);
				System.out.println("Checking "+backupFile.getPath());
				if(!backupFile.exists() || !backupFile.canWrite())
				    {
					continue;
				    }
				System.out.println("Deleting "+backupFile.getPath());
				backupFile.delete();
			    }
		    }
		File trashFile = new File(writer.getUserDir()+File.separator+"trash");
		if(trashFile.exists())
		    {
			if(trashFile.isDirectory())
			    {
				String filenames[] = trashFile.list();
				for(int i = 0; i < filenames.length; i++)
				    {
					File fileToDelete = new File(trashFile, filenames[i]);
					fileToDelete.delete();
				    }
			    }
			trashFile.delete();
		    }
		Alert("All Backup Files Deleted!");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    protected void CheckInAllFiles()
    {
	try
	    {
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Checking in files . . . ";
			loadPanel.bytes_read = 0;
			loadPanel.content_length = filesList.getItemCount();
			if(debug_on)
			    {
				System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
			    }
			loadPanel.repaint();
		    }
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String file = getFilesListItem(i);
			if(debug_on)
			    {
				System.out.println("CheckInAllFiles:" +file + " selected");
			    }
			if(null == file)
			    {
				continue;
			    }
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(file);
			if(null == fti)
			    {
				System.err.println("No entry in file types hashtable for "+file);
				continue;
			    }
			if(fti.in_use)
			    {
				if(debug_on)
				    {
					Thread.dumpStack();
					System.out.println("File in use "+fti);
				    }
				continue;
			    }
			if(fti.file.exists() && fti.file.canWrite())
			    {
				CheckInFile(fti.file);
			    }
			if(null != loadPanel)
			    {
				loadPanel.URLname = "Checking in files . . . ";
				loadPanel.bytes_read = i;
				loadPanel.content_length = filesList.getItemCount();
				if(debug_on)
				    {
					System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
				    }
				loadPanel.repaint();
			    }
			try
			    {
				Thread.sleep(20);
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Checking in files . . . ";
			loadPanel.bytes_read = filesList.getItemCount();
			loadPanel.content_length = filesList.getItemCount();
			if(debug_on)
			    {
				System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
			    }
			loadPanel.repaint();
		    }
		Alert("All files Checked In!!!");
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    boolean modulesInMainLoopChanged = false;

    void UpdateMainLoopInfo()
    {
	try
	    {
		String MainLoopName = mainLoopList.getSelectedItem();
		if(debug_on)
		    {
			System.out.println("rcsDesign.UpdateMainLoopInfo:");
			System.out.println("writer.curmainLoop = "+writer.curmainLoop);
		    }
		if(null != writer.curmainLoop)
		    {
			if(null != writer.curmainLoop.Name)
			    {
				if(!writer.curmainLoop.Name.equals(MainLoopName) || modulesInMainLoopChanged)
				    {
					writer.curmainLoop.host = mainLoopHostField.getText();
					try
					    {
						writer.curmainLoop.cycle_time = Double.valueOf(cycletimeField.getText()).doubleValue();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
					String modules[] = modsInLoopList.getSelectedItems();
					writer.curmainLoop.clearModules();
					for(int i = 0;  i < modules.length; i++)
					    {
						ModuleInfo mi = (ModuleInfo) writer.buffersHashtable.get(modules[i]);
						if(null != mi)
						    {
							mi.MainLoopName = MainLoopName;
							mi.host = writer.curmainLoop.host;
							mi.cycle_time = writer.curmainLoop.cycle_time;
						    }
						writer.curmainLoop.addModule(modules[i]);
						for(int j = 0; j < mainLoopList.getItemCount(); j++)
						    {
							String temploop = mainLoopList.getItem(j);
							if(temploop.equals(writer.curmainLoop.Name))
							    {
								continue;
							    }
							rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(temploop);
							if(null == mli)
							    {
								continue;
							    }
							if(mli == writer.curmainLoop)
							    {
								continue;
							    }
							if(mli.Name.equals(writer.curmainLoop.Name))
							    {
								continue;
							    }
							if(mli.getModules() == null)
							    {
								continue;
							    }
							mli.getModules().removeElement(modules[i]);
						    }
					    }
				    }
			    }
		    }
		if(debug_on)
		    {
			System.out.println(MainLoopName+" selected.");
		    }
		if(null == MainLoopName)
		    {
			mainLoopHostField.setText("localhost");
			return;
		    }
		if(null == writer.mainloopsHashtable)
		    {
			return;
		    }
		rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(MainLoopName);
		if(null != mli)
		    {
			writer.curmainLoop = mli;
			mainLoopHostField.setText(mli.host);
			int module_indexes[] = modsInLoopList.getSelectedIndexes();
			for(int i = 0; i < module_indexes.length; i++)
			    {
				modsInLoopList.deselect(module_indexes[i]);
			    }
			if(mli.getModules() == null)
			    {
				Alert("Loop "+MainLoopName+" has no modules.");
				return;
			    }
			if(mli.getModules().size() < 0)
			    {
				Alert("Loop "+MainLoopName+" has no modules.");
				return;
			    }
			for(int i = 0; i < mli.getModules().size(); i++)
			    {
				String modname = (String) mli.getModules().elementAt(i);
				for(int j = 0; j < modsInLoopList.getItemCount(); j++)
				    {
					String temp = modsInLoopList.getItem(j);
					if(temp.equals(modname))
					    {
						modsInLoopList.select(j);
						break;
					    }
				    }
			    }
		    }
		modulesInMainLoopChanged = false;
	    }
        catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    boolean bufsInServerChanged = false;

    @SuppressWarnings("unchecked")
    void UpdateServerInfo()
    {
	try
	    {
		String serverName = serverList.getSelectedItem();
		if(debug_on)
		    {
			System.out.println("");
			System.out.println("rcsDesign.UpdateServerInfo:");
			Thread.dumpStack();
			System.out.println("writer.curServer = "+writer.curServer);
			System.out.println("serverName = "+serverName);
			System.out.println("bufsInServerChanged ="+bufsInServerChanged);
		    }
		if(null != writer.curServer)
		    {
			if(!writer.curServer.Name.equals(serverName) || bufsInServerChanged)
			    {
				writer.curServer.Host = serverHostField.getText();
				writer.curServer.bufferNames = new Vector();
				String bufs[] = bufsInServerList.getSelectedItems();
				for(int i = 0;  i < bufs.length; i++)
				    {

					BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(bufs[i]);
					if(debug_on)
					    {
						System.out.println("bufs["+i+"] ="+bufs[i]);
						System.out.println("bi ="+bi);
					    }
					if(null != bi)
					    {
						bi.si = writer.curServer;
					    }
					writer.curServer.bufferNames.addElement(bufs[i]);
					for(int j = 0; j < serverList.getItemCount(); j++)
					    {
						String tempsvr = serverList.getItem(j);
						if(tempsvr.equals(writer.curServer.Name))
						    {
							continue;
						    }
						ServerInfo si = (ServerInfo) writer.serversHashtable.get(tempsvr);
						if(debug_on)
						    {
							System.out.println("j="+j+", si="+si);
						    }
						if(null == si)
						    {
							continue;
						    }
						if(si == writer.curServer)
						    {
							continue;
						    }
						if(si.Name.equals(writer.curServer.Name))
						    {
							continue;
						    }
						if(si.bufferNames == null)
						    {
							continue;
						    }
						si.bufferNames.removeElement(bufs[i]);
					    }
				    }
			    }
		    }
		if(debug_on)
		    {
			System.out.println(serverName+" selected.");
		    }
		if(null == serverName)
		    {
			serverHostField.setText("localhost");
			return;
		    }
		if(null == writer.serversHashtable)
		    {
			return;
		    }
		ServerInfo si = (ServerInfo) writer.serversHashtable.get(serverName);
		if(null != si)
		    {
			writer.curServer = si;
			if(debug_on)
			    {
				System.out.println("writer.curServer="+writer.curServer);
			    }
			serverHostField.setText(si.Host);
			int buf_indexes[] = bufsInServerList.getSelectedIndexes();
			for(int i = 0; i < buf_indexes.length; i++)
			    {
				if(debug_on)
				    {
					System.out.println("bufsInServerList.deselect(buf_indexes["+i+"] ="+buf_indexes[i]+")");
				    }
				bufsInServerList.deselect(buf_indexes[i]);
			    }
			for(int i = 0; i < si.bufferNames.size(); i++)
			    {
				String bufname = (String) si.bufferNames.elementAt(i);
				if(debug_on)
				    {
					System.out.println("si.bufferNames.elementAt(i="+i+") ="+bufname);
				    }
				for(int j = 0; j < bufsInServerList.getItemCount(); j++)
				    {
					String temp = bufsInServerList.getItem(j);
					if(debug_on)
					    {
						System.out.println("bufsInServerList.getItem(j="+j+") = "+temp);
					    }
					if(temp.equals(bufname))
					    {
						if(debug_on)
						    {
							System.out.println("bufsInServerList.select("+j+")");
						    }
						bufsInServerList.select(j);
						break;
					    }
				    }
			    }
			if(debug_on)
			    {
				buf_indexes = bufsInServerList.getSelectedIndexes();
				for(int i = 0; i < buf_indexes.length; i++)
				    {
					if(debug_on)
					    {
						System.out.println("buf_indexes["+i+"] ="+buf_indexes[i]);
					    }
				    }
			    }
		    }
		if(debug_on)
		    {
			System.out.println("");
		    }
		bufsInServerChanged = false;
	    }
        catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    int last_modulesSelected_index = 0;

    FileTypeInfo fileToUpdate = null;

    /*
     * This function is required to implement java.awt.event.ItemListener. It is called whenever the
     * state of a Checkbox, List or Choice is changed by the user.
     */
    public void itemStateChanged(ItemEvent evt)
    {

	try
	    {
                
                UpdateWriter();
		if(debug_on)
		    {
			System.out.println("itemStateChanged("+evt+")");
		    }
		if(inside_query|| inside_run || shutting_down || inside_alert)
		    {
			if(debug_on)
			    {
				System.out.println("Ignoring event.");
			    }
			return;
		    }
		Object event_source = evt.getSource();
		if(event_source == terminalCommandField ||
		   event_source == makeCommandField ||
		   event_source == rcslibdirField ||
		   event_source == appnameField ||
		   event_source == appdirField ||
		   event_source == userdirField  ||
		   event_source == cppExtField ||
		   event_source == hppExtField ||
		   event_source == objExtField ||
		   event_source == platList ||
		   event_source == addPlatField)
		    {
			synchronized(this)
			    {
				MarkCommonFiles();
			    }
		    }
		if(event_source == modulesList)
		    {
			synchronized(this)
			    {
				int sel_index = modulesList.getSelectedIndex();
				String new_module_name = modulesList.getSelectedItem();
				if(writer.curModule != null)
				    {
					if(new_module_name.equals(writer.curModule.Name))
					    {
						return;
					    }
				    }
				if(writer.curModule != null)
				    {
					if(cmdList.getItemCount() > 0)
					    {
						writer.curModule.cmdsAvailable.removeAllElements();
						for(int i = 0; i < cmdList.getItemCount(); i++)
						    {
							if(debug_on)
							    {
								System.out.println("cmdList.getItem("+i+") = "+cmdList.getItem(i));
							    }
							writer.curModule.cmdsAvailable.addElement( cmdList.getItem(i));
						    }
					    }
					if(debug_on)
					    {
						System.out.println("****************************************************");
						System.out.println("");
					    }
				    }
				if(null != new_module_name && null != writer.modulesHashtable)
				    {
					writer.curModule = (ModuleInfo) writer.modulesHashtable.get(new_module_name);
					if(null == writer.curModule)
					    {
						writer.curModule  = new ModuleInfo( new DiagNMLMsgDictCreator(), rcs.nml.NMLConnection.Creator );
						writer.curModule.Name = new_module_name;
						writer.modulesHashtable.put(new_module_name, writer.curModule);
						writer.curModule = (ModuleInfo) writer.modulesHashtable.get(new_module_name);
					    }
					if(null != writer.curModule)
					    {
						LoadListsFromCurModule();
					    }
				    }
				if(generateEverything_thread == null)
				    {
					BuildHierarchyDisplay();
				    }
			    }
			return;
		    }

		if(generateEverything_thread != null)
		    {
			System.err.println("Ignoring evt="+evt+" because it occured while either loading a configuration or generating a source file.");
			return;
		    }

		if(debug_on)
		    {
			System.out.println("itemStateChanged("+evt+")");
		    }


		if(event_source == mainLoopList)
		    {
			synchronized(this)
			    {
				int sel_index = mainLoopList.getSelectedIndex();
				if(writer.curmainLoop != null)
				    {
					String modules[] =  modsInLoopList.getSelectedItems();
					writer.curmainLoop.clearModules();
					for(int i = 0; i < modules.length; i++)
					    {
						writer.curmainLoop.addModule(modules[i]);
					    }
					if(debug_on)
					    {
						System.out.println("writer.curmainLoop.getModules() = "+writer.curmainLoop.getModules());
					    }
					try
					    {
						writer.curmainLoop.cycle_time = Double.valueOf(cycletimeField.getText()).doubleValue();
						writer.curmainLoop.host = mainLoopHostField.getText();
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
				    }
				int module_indexes[] = modsInLoopList.getSelectedIndexes();
				for(int i = 0; i < module_indexes.length; i++)
				    {
					modsInLoopList.deselect(module_indexes[i]);
				    }
				String new_mainLoop_name = mainLoopList.getSelectedItem();
				if(debug_on)
				    {
					System.out.println("new_mainLoop_name = "+new_mainLoop_name);
				    }
				if(null != new_mainLoop_name && null != writer.mainloopsHashtable)
				    {
					writer.curmainLoop = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(new_mainLoop_name);
					if(null != writer.curmainLoop)
					    {
						if(null != modulesList)
						    {
							modsInLoopList.removeAll();
							for(int i = 0; i < modulesList.getItemCount(); i++)
							    {
								modsInLoopList.add(modulesList.getItem(i));
								if(null != writer.curmainLoop)
								    {
									if(null != writer.curmainLoop.getModules())
									    {
										for(int j = 0; j < writer.curmainLoop.getModules().size(); j++)
										    {
											if(modsInLoopList.getItem(i).equals((String) writer.curmainLoop.getModules().elementAt(j)))
											    {
												modsInLoopList.select(i);
												break;
											    }
										    }
									    }
								    }
							    }
						    }
						cycletimeField.setText(String.valueOf(writer.curmainLoop.cycle_time));
						if(null == writer.curmainLoop.host)
						    {
							writer.curmainLoop.host = "localhost";
						    }
						mainLoopHostField.setText(writer.curmainLoop.host);
					    }
				    }
				UpdateMainLoopInfo();
				MarkLoopFiles(new_mainLoop_name);
			    }
			return;
		    }


		if(event_source == auxList)
		    {
			synchronized(this)
			    {
				int selected_auxList_index = auxList.getSelectedIndex();
				String aux = auxList.getSelectedItem();
				if(debug_on)
				    {
					System.out.println("");
					System.out.println("selected_auxList_index = "+selected_auxList_index);
					System.out.println("aux = "+aux);
					System.out.println("writer.curModule.Name = "+writer.curModule.Name);
				    }
				UpdateAuxStuff(aux);
			    }
			return;
		    }

		if(event_source == filesList)
		    {
			synchronized(this)
			    {
				int sel_index = filesList.getSelectedIndex();
				String file = getFilesListItem(sel_index);
				if(debug_on)
				    {
					System.out.println("itemStateChangedfile:(event_source == filesList) " +file+ " selected");
				    }
				if(null == file)
				    {
					return;
				    }
				FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(file);
				if(debug_on)
				    {
					System.out.println("itemStateChangedfile:(event_source == filesList): fti="+fti);
				    }
				if(null == fti)
				    {
					System.err.println("No entry in file types hashtable for "+file);
					return;
				    }
				if(null == current_fti)
				    {
					current_fti = fti;
				    }
				if(fti.in_use)
				    {
					if(debug_on)
					    {
						Thread.dumpStack();
						System.out.println("File in use "+fti);
					    }
					return;
				    }
				fti.in_use = true;
				if(text_file_needs_saving)
				    {
					SaveCurrentTextFile();
				    }
				if(fti.file.exists() && !fti.file.canWrite() && autoCheckOutCheckbox.getState())
				    {
					CheckOutFile(fti.file);
				    }
				if(fti.up_to_date)
				    {
					ReadFileIntoTextArea(fti.file.toString(), FileTypeInfo.typeToString(fti.type),fti);
					if(!fti.file.exists())
					    {
						fileCheckOutCheckbox.setState(true);
						fileTextArea.setEditable(true);
					    }
					else if(fti.file.canWrite())
					    {
						fileCheckOutCheckbox.setState(true);
						fileTextArea.setEditable(true);
					    }
					else
					    {
						fileCheckOutCheckbox.setState(false);
						fileTextArea.setEditable(false);
					    }
					fti.in_use = false;
					return;
				    }
				UpdateWriter();
				fileToUpdate = fti;
				StartLongFunction(UPDATE_FILE);
			    }
			return;
		    }
		if(event_source == subordinatesList)
		    {
			synchronized(this)
			    {
				hierarchy_modified = true;
				boolean curModuleWasNull = false;
				if(null == modulesList || null == writer)
				    {
					return;
				    }
				if(null == writer.modulesHashtable)
				    {
					return;
				    }
				if(null == writer.curModule)
				    {
					curModuleWasNull = true;
					if(null != modulesList && null != writer.modulesHashtable && null != modulesList.getSelectedItem() )
					    {
						writer.curModule = (ModuleInfo) writer.modulesHashtable.get(modulesList.getSelectedItem());
					    }
				    }
				if(null != writer.curModule)
				    {
					try
					    {
						int index = -1;
						try
						    {
							Integer Iindex = (Integer) evt.getItem();
							index = Iindex.intValue();
						    }
						catch(ClassCastException cce)
						    {
							cce.printStackTrace();
							System.err.println("Object returned by evt.getItem() is not an Integer.");
						    }
						catch(Exception e)
						    {
							e.printStackTrace();
						    }
						if(index >= 0 && index < subordinatesList.getItemCount())
						    {
							String subname = subordinatesList.getItem(index);
							if(debug_on)
							    {
								System.out.println("index = "+index+", subname="+subname);
							    }
							if(evt.getStateChange() == ItemEvent.SELECTED )
							    {
								ModuleInfo parent = writer.curModule;
								int parents_checked = 0;
								while(parent != null )
								    {
									if(debug_on)
									    {
										System.out.println("Checking if "+subname+" is a parent of "+parent.Name);
									    }
									if(subname.equals(parent.Name))
									    {
										Alert("You can not make a module a subordinate of itself.");
										for(int i = 0; i < subordinatesList.getItemCount(); i++)
										    {
											String temp = subordinatesList.getItem(i);
											if(temp.equals(subname))
											    {
												subordinatesList.deselect(i);
												break;
											    }
										    }
										if(curModuleWasNull)
										    {
											writer.curModule = null;
										    }
										return;
									    }
									parent = parent.parent;
									if(parents_checked >= modulesList.getItemCount())
									    {
										Alert("Invalid Hierarchy");
										if(curModuleWasNull)
										    {
											writer.curModule = null;
										    }
										return;
									    }
									parents_checked++;
								    }
								file_list_needs_to_be_modified = true;
								if(debug_on)
								    {
									System.out.println("Adding "+subname+" as a subordinate of "+writer.curModule.Name);
								    }
								writer.curModule.children_names.addElement(subname);
								writer.curModule.designLog += new Date() + " Adding "+subname+" as a subordinate of "+writer.curModule.Name+"\n";
								writer.designLog += new Date() + " Adding "+subname+" as a subordinate of "+writer.curModule.Name+"\n";
								for(int i = 0; i < modulesList.getItemCount(); i++)
								    {
									if(i == modulesList.getSelectedIndex())
									    {
										continue;
									    }
									String modName = modulesList.getItem(i);
									ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
									if(null == modInfo)
									    {
										continue;
									    }
									for(int j = 0; j < modInfo.children_names.size(); j++)
									    {
										String child = (String) modInfo.children_names.elementAt(j);
										if(child.equals(subname))
										    {
											modInfo.children_names.remove(j);
											modInfo.designLog += new Date()+ " Remove "+child+" as a subordinate of "+modInfo.Name+"\n";
											writer.designLog += new Date()+ " Remove "+child+" as a subordinate of "+modInfo.Name+"\n";
											break;
										    }
									    }
								    }
							    }
							else
							    {
								file_list_needs_to_be_modified = true;
								String deselected_child = subordinatesList.getItem(index);
								if(debug_on)
								    {
									System.out.println("Remove "+deselected_child+" as a subordinate of "+writer.curModule.Name);
								    }
								writer.curModule.designLog += new Date()+" Remove "+deselected_child+" as a subordinate of "+writer.curModule.Name+"\n";
								writer.designLog +=  new Date()+" Remove "+deselected_child+" as a subordinate of "+writer.curModule.Name+"\n";
								for(int i = 0; i < writer.curModule.children_names.size(); i++)
								    {
									String child = (String) writer.curModule.children_names.elementAt(i);
									if(deselected_child.equals(child))
									    {
										writer.curModule.children_names.remove(i);
										break;
									    }
								    }
								ModuleInfo childModule = (ModuleInfo) writer.modulesHashtable.get(deselected_child);
								if(null != childModule)
								    {
									childModule.parent = null;
								    }
							    }

							MarkModuleFiles(modulesList.getSelectedItem(),false);
							BuildHierarchyDisplay();
						    }
					    }
					catch(Exception e)
					    {
						e.printStackTrace();
					    }
				    }
			    }
			return;
		    }

		if(event_source == serverList)
		    {
			synchronized(this)
			    {
				UpdateServerInfo();
				MarkCommonFiles();
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }

		if(event_source == bufsInServerList)
		    {
			synchronized(this)
			    {
				bufsInServerChanged = true;
				UpdateServerInfo();
				MarkCommonFiles();
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }

		if(event_source == modsInLoopList)
		    {
			synchronized(this)
			    {
				modulesInMainLoopChanged = true;
				UpdateMainLoopInfo();
				MarkCommonFiles();
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }

		if(event_source == screenChoice )
		    {
			synchronized(this)
			    {
				if(prevViewString.equals("Files"))
				    {
					lastFileSelected = getFilesListItem(filesList.getSelectedIndex());
				    }
				currentViewString = screenChoice.getSelectedItem();
				if(currentViewString.equals("Hierarchy") && hierarchy_modified)
				    {
					BuildHierarchyDisplay();
				    }
				if(currentViewString.equals("Loops/Servers"))
				    {
					UpdateServerInfo();
				    }
				if(currentViewString.equals("Files") && file_list_needs_to_be_modified)
				    {
					UpdateServerInfo();
					CreateFileList(userdirField.getText(),false);
					if(filesList.getSelectedIndex() < 0 && lastFileSelected != null)
					    {
						for(int i = 0; i < filesList.getItemCount(); i++)
						    {
							String filename = getFilesListItem(i);
							if(lastFileSelected.equals(filename))
							    {
								filesList.select(i);
								FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
								if(null == fti)
								    {
									System.err.println("No entry in file types hashtable for "+filename);
									break;
								    }
								if(fti.in_use)
								    {
									if(debug_on)
									    {
										Thread.dumpStack();
										System.out.println("File in use "+fti);
									    }
									break;
								    }
								fti.in_use = true;
								if(text_file_needs_saving)
								    {
									SaveCurrentTextFile();
								    }
								fti.generated = false;
								fileToUpdate = fti;
								StartLongFunction(UPDATE_FILE);
								break;
							    }
						    }
					    }
				    }
				choicePanelLayout.show(choicePanel,currentViewString);
				prevViewString = currentViewString;
			    }
			return;
		    }
		if(event_source == CmdOrSubChoice )
		    {
			synchronized(this)
			    {
				CmdOrSubLayout.show(CmdOrSubPanel,CmdOrSubChoice.getSelectedItem());
				if(CmdOrSubChoice.getSelectedItem().toUpperCase().indexOf("AUX") >= 0)
				    {
					if(auxList.getSelectedIndex() < 0)
					    {
						if(auxList.getItemCount() > 0)
						    {
							auxList.select(0);
							String aux = auxList.getSelectedItem();
							if(null != aux)
							    {
								UpdateAuxStuff(aux);
							    }
							auxInputCheckbox.setEnabled(true);
							auxUpdateCheckbox.setEnabled(true);
						    }
					    }
				    }
			    }
			return;
		    }

		if(event_source == fileCheckOutCheckbox)
		    {
			synchronized(this)
			    {
				int file_index = filesList.getSelectedIndex();
				String file = getFilesListItem(file_index);
				if(debug_on)
				    {
					System.out.println(file + " selected");
				    }
				if(null == file)
				    {
					return;
				    }
				FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(file);
				if(null == fti)
				    {
					System.err.println("No entry in file types hashtable for "+file);
					return;
				    }
				if(fti.in_use)
				    {
					if(debug_on)
					    {
						Thread.dumpStack();
						System.out.println("File in use "+fti);
					    }
					return;
				    }
				if(fileCheckOutCheckbox.getState())
				    {
					CheckOutFile(fti.file);
				    }
				else
				    {
					CheckInFile(fti.file);
				    }
				if(fti.file.exists() && !fti.file.canWrite())
				    {
					fileCheckOutCheckbox.setState(false);
					fileTextArea.setEditable(false);
				    }
				else
				    {
					fileCheckOutCheckbox.setState(true);
					fileTextArea.setEditable(true);
				    }
				monitored_repaint();
			    }
			return;
		    }

		if(event_source == useMergerCheckbox)
		    {
			useMerger = useMergerCheckbox.getState();
			return;
		    }

		if(event_source == makeBackupsCheckbox)
		    {
			makeBackups = makeBackupsCheckbox.getState();
			return;
		    }

		if(event_source == singleDirCheckbox)
		    {
			singleDir = singleDirCheckbox.getState();
			return;
		    }

		if(event_source ==   auxUpdateCheckbox)
		    {
			synchronized(this)
			    {
				int aux_index = auxList.getSelectedIndex();
				if(aux_index < 0 || aux_index > auxList.getItemCount())
				    {
					return;
				    }
				String aux = auxList.getItem(aux_index);
				if(null == writer.curModule)
				    {
					return;
				    }
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(aux);
				if(null == bi)
				    {
					return;
				    }
				ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(writer.curModule.Name);
				if(null == ci)
				    {
					return;
				    }
				ci.updateEveryCycle = auxUpdateCheckbox.getState();
				writer.curModule.designLog += new Date() + " Auxilliary channel "+aux+": Update Every Cycle set to "+ci.updateEveryCycle+" for module "+writer.curModule.Name+"\n";
				writer.designLog += new Date() + " Auxilliary channel "+aux+": Update Every Cycle set to "+ci.updateEveryCycle+" for module "+writer.curModule.Name+"\n";
				MarkModuleFiles(writer.curModule.Name,false);
			    }
			return;
		    }

		if(event_source == auxInputCheckbox)
		    {
			synchronized(this)
			    {
				boolean is_input = auxInputCheckbox.getState();
				int aux_index = auxList.getSelectedIndex();
				if(aux_index < 0)
				    {
					return;
				    }
				String aux = auxList.getItem(aux_index);
				if(null == writer.curModule)
				    {
					return;
				    }
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(aux);
				if(null == bi)
				    {
					return;
				    }
				ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(writer.curModule.Name);
				if(null == ci)
				    {
					return;
				    }
				ci.isWriter = !is_input;
				if(is_input)
				    {
					writer.curModule.designLog += new Date() + " Auxilliary channel "+aux+" changed to input for module "+writer.curModule.Name+"\n";
					writer.designLog += new Date() + " Auxilliary channel "+aux+" changed to input for module "+writer.curModule.Name+"\n";

					if(null == writer.curModule.AuxInputNames)
					    {
						writer.curModule.AuxInputNames = new Vector();
					    }
					writer.curModule.AuxInputNames.addElement(aux);


					if(null != writer.curModule.AuxOutputNames)
					    {
						for(int i = 0; i < writer.curModule.AuxOutputNames.size(); i++)
						    {
							String AuxOutput = (String) writer.curModule.AuxOutputNames.elementAt(i);
							if(AuxOutput.equals(aux))
							    {
								writer.curModule.AuxOutputNames.remove(i);
								if(null == writer.curModule.deletedAuxOutputNames)
								    {
									writer.curModule.deletedAuxOutputNames = new Vector();
								    }
								writer.curModule.deletedAuxOutputNames.addElement(aux);
								break;
							    }
						    }
					    }
					if(null == bi.readerNames)
					    {
						bi.readerNames = new ArrayList<String>();
					    }
					bi.readerNames.add(writer.curModule.Name);
					for(int i = 0; i < bi.writerNames.size(); i++)
					    {
						String writername = (String) bi.writerNames.get(i);
						if(writername.equals(writer.curModule.Name))
						    {
							bi.writerNames.remove(i);
							break;
						    }
					    }

				    }
				else
				    {
					writer.curModule.designLog += new Date() + " Auxilliary channel "+aux+" changed to output for module "+writer.curModule.Name+"\n";
					writer.designLog += new Date() + " Auxilliary channel "+aux+" changed to output for module "+writer.curModule.Name+"\n";

					if(null == writer.curModule.AuxOutputNames)
					    {
						writer.curModule.AuxOutputNames = new Vector();
					    }
					writer.curModule.AuxOutputNames.addElement(aux);

					if(null != writer.curModule.AuxInputNames)
					    {
						for(int i = 0; i < writer.curModule.AuxInputNames.size(); i++)
						    {
							String AuxInput = (String) writer.curModule.AuxInputNames.elementAt(i);
							if(AuxInput.equals(aux))
							    {
								writer.curModule.AuxInputNames.remove(i);
								if(null == writer.curModule.deletedAuxInputNames)
								    {
									writer.curModule.deletedAuxInputNames = new Vector();
								    }
								writer.curModule.deletedAuxInputNames.addElement(aux);
								break;
							    }
						    }
					    }

					if(null == bi.writerNames)
					    {
						bi.writerNames = new ArrayList<String>();
					    }
					bi.writerNames.add(writer.curModule.Name);
					for(int i = 0; i < bi.readerNames.size(); i++)
					    {
						String readername = (String) bi.readerNames.get(i);
						if(readername.equals(writer.curModule.Name))
						    {
							bi.readerNames.get(i);
							break;
						    }
					    }


				    }
				MarkModuleFiles(writer.curModule.Name,false);
			    }
			return;
		    }
		if(event_source == debugCheckbox)
		    {
			synchronized(this)
			    {
				debug_on = debugCheckbox.getState();
				CodeGen.debug_on = debug_on;
				ModuleInfo.debug_on = debug_on;
				Merger.debug_on = debug_on;
				rcsDesignWriter.debug_on = debug_on;
				HierarchyPanel.debug_on = debug_on;
				QueryDialog.debug_on = debug_on;
				AlertDialog.debug_on = debug_on;
				StandAloneApplet.debug_on = debug_on;
				rcsdesignFrame.debug_on = debug_on;
				URLLoadInfoPanel.debug_on = debug_on;
				URL_and_FileLoader.debug_on=debug_on;
			    }
			return;
		    }
		if(event_source == fileCheckinTypeChoice)
		    {
			synchronized(this)
			    {
				default_version_control_type = fileCheckinTypeChoice.getSelectedItem();
				MarkCommonFiles();
				setup_version_control_info();
			    }
			return;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.out.println("evt="+evt);
	    }
    }

    /*
     *
     * This function is called after the user clicks any button or presses <ENTER>  in any TextField.
     */
    public void actionPerformed(ActionEvent evt)
    {
	try
	    {
                UpdateWriter();
		if(debug_on)
		    {
			System.out.println("actionPerformed("+evt+")");
		    }

		Object event_source = evt.getSource();

		if(event_source == terminalCommandField ||
		   event_source == makeCommandField ||
		   event_source == rcslibdirField ||
		   event_source == appnameField ||
		   event_source == appdirField ||
		   event_source == userdirField  ||
		   event_source == cppExtField ||
		   event_source == hppExtField ||
		   event_source == objExtField ||
		   event_source == platList ||
		   event_source == addPlatField)
		    {
			synchronized(this)
			    {
				MarkCommonFiles();
			    }
		    }

		if(event_source == goButton)
		    {
			read_everything_needed = false;
			if(goButton.getLabel().equals(" STOP ACTION "))
			    {
				KillGenerateEverythingThread();
				GeneratingEverything = false;
				run_function = DO_NOTHING;
				try
				    {
					Thread.sleep(50);
				    }
				catch(Exception e)
				    {
				    }
				goButton.setLabel("Create Source");
				monitored_repaint();
				try
				    {
					Thread.sleep(50);
				    }
				catch(Exception e)
				    {
				    }
				return;
			    }
			synchronized(this)
			    {
				writer.AppName = appnameField.getText();
				writer.setUserDir(userdirField.getText());
				if(writer.getUserDir().endsWith(File.separator))
				    {
					writer.setUserDir(writer.getUserDir().substring(0,writer.getUserDir().length()-1));
				    }
				File userDirFile = new File(writer.getUserDir());
				if(!userDirFile.getName().equals(writer.AppName) && userDirFile.getName().length() > 0)
				    {
					writer.setUserDir(userDirFile.getParent() +File.separator+ writer.AppName);
					userdirField.setText(writer.getUserDir());
				    }
				File AppDirFile = new File(writer.AppDir);
				if(!AppDirFile.getName().equals(writer.AppName))
				    {
					File newAppDirFile = new File(AppDirFile,writer.AppName);
					writer.AppDir = newAppDirFile.toString();
					appdirField.setText(writer.AppDir);
				    }
				StartLongFunction(CREATE_SOURCE);
				read_everything_needed = false;
				hierarchy_modified = true;
				return;
			    }
		    }

		if(generateEverything_thread != null)
		    {
			System.err.println("Ignoring evt="+evt+" because it occured while either loading a configuration or generating a source file.");
			return;
		    }

		if(inside_query|| inside_run || shutting_down || inside_alert)
		    {
			if(debug_on)
			    {
				System.out.println("Ignoring event.");
			    }
			return;
		    }

		if(event_source == serverHostField)
		    {
			synchronized(this)
			    {
				String serverName = serverList.getSelectedItem();
				ServerInfo si = (ServerInfo) writer.serversHashtable.get(serverName);
				if(null != si)
				    {
					si.Host = serverHostField.getText();
				    }
				MarkCommonFiles();
				return;
			    }
		    }

		if(event_source == mainLoopHostField)
		    {
			synchronized(this)
			    {
				String loopName = mainLoopList.getSelectedItem();
				rcsdesignMainLoopInfo mli = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(loopName);
				if(null != mli)
				    {
					mli.host = mainLoopHostField.getText();
				    }
				MarkCommonFiles();
			    }
			return;
		    }

		if(event_source == delServerButton)
		    {
			synchronized(this)
			    {
				file_list_needs_to_be_modified = true;
				String serverName = serverList.getSelectedItem();
				writer.serversHashtable.remove(serverName);
				serverList.remove(serverList.getSelectedIndex());
				serverList.select(0);
				MarkCommonFiles();
			    }
			return;
		    }

		if(event_source == addServerField)
		    {
			synchronized(this)
			    {
				file_list_needs_to_be_modified = true;
				String serverName = addServerField.getText()+"svr";
				String serverHost = serverHostField.getText();
				String serverInfo = "is_server=true;\nhost="+serverHost+";\n";
				ServerInfo si = new ServerInfo(serverName, serverInfo);
				/* System.out.println("Adding Server");
				   System.out.println("serverName="+serverName);
				   System.out.println("serverHost = "+serverHost);
				   System.out.println("serverInfo="+serverInfo);
				   System.out.println("si = "+si); */
				writer.serversHashtable.put(serverName, si);
				boolean server_added = false;
				for(int i = 0; i < serverList.getItemCount(); i++)
				    {
					String sn = serverList.getItem(i);
					System.out.println("sn="+sn);
					if(sn.compareTo(serverName) >= 0)
					    {
						serverList.add(serverName,i);

						serverList.select(i);
						server_added = true;
						break;
					    }
				    }
				if(!server_added)
				    {
					serverList.add(serverName);
					serverList.select(serverList.getItemCount()-1);
				    }
				addServerField.setText("");
				MarkCommonFiles();
				return;
			    }
		    }




		if(event_source == checkInEverythingButton)
		    {
			synchronized(this)
			    {
				StartLongFunction(CHECK_IN_FILES);
			    }
			return;
		    }


		if(event_source == removeBackupsButton)
		    {
			synchronized(this)
			    {
				RemoveBackups();
			    }
			return;
		    }



		if(event_source == addauxField)
		    {
			synchronized(this)
			    {
				if(null == writer.curModule)
				    {
					return;
				    }
				auxInputCheckbox.setEnabled(true);
				auxUpdateCheckbox.setEnabled(true);
				String name = addauxField.getText();
				boolean isInput = auxInputCheckbox.getState();
				if(isInput)
				    {
					writer.curModule.AddAuxInput(name);
				    }
				else
				    {
					writer.curModule.AddAuxOutput(name);
				    }
				auxList.add(name);
				addauxField.setText("");
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(name);
				if(null == bi)
				    {
					bi = new BufferInfo();
					bi.Name = name;
					if(debug_on)
					    {
						System.out.println("Adding BufferInfo for "+name);
					    }
					writer.buffersHashtable.put(name, bi);
				    }
				ChannelInfo ci = new ChannelInfo();
				ci.Name = name;
				ci.ModuleName = writer.curModule.Name;
				ci.isWriter = !isInput;
				ci.updateEveryCycle = auxUpdateCheckbox.getState();
				bi.channelsHashtable.put(ci.ModuleName, ci);
				if(isInput)
				    {
					if(bi.readerNames == null)
					    {
						bi.readerNames = new ArrayList<String>();
					    }
					bi.readerNames.add(writer.curModule.Name);
				    }
				else
				    {
					if(bi.writerNames == null)
					    {
						bi.writerNames = new ArrayList<String>();
					    }
					bi.writerNames.add(writer.curModule.Name);
				    }
				if(null == ModuleInfo.AllAuxChannels)
				    {
					ModuleInfo.AllAuxChannels = new Vector();
				    }
				if(null == writer.auxChannelsVector)
				    {
					writer.auxChannelsVector = ModuleInfo.AllAuxChannels;
				    }
				boolean already_in_channels_vector = false;
				for(int i = 0; i < writer.auxChannelsVector.size(); i++)
				    {
					String auxFromVector = (String) writer.auxChannelsVector.elementAt(i);
					if(name.equals(auxFromVector))
					    {
						already_in_channels_vector = true;
						break;
					    }
				    }
				if(!already_in_channels_vector)
				    {
					writer.auxChannelsVector.addElement(name);
				    }
				if(debug_on)
				    {
					System.out.println("Adding ChannelInfo for module = "+ci.ModuleName+" to buffer = "+name);
					System.out.println("bi = "+bi);
					System.out.println("bi.channelsHashtable = "+bi.channelsHashtable);
				    }
				BuildHierarchyDisplay();
				writer.designLog += new Date() + " Adding auxilliary channel "+name + " to "+ writer.curModule.Name+"\n";
				writer.curModule.designLog += new Date() + " Adding auxilliary channel "+name + " to "+ writer.curModule.Name+"\n";
				MarkModuleFiles(writer.curModule.Name, false);
				CreateBufferList();
				return;
			    }
		    }

		if(event_source == delauxButton)
		    {
			synchronized(this)
			    {
				String aux = auxList.getSelectedItem();
				if(null == aux)
				    {
					return;
				    }
				int aux_index = auxList.getSelectedIndex();
				auxList.remove(aux_index);
				if(null == writer.curModule)
				    {
					return;
				    }
				if(null !=  writer.curModule.predefined_type_files)
				    {
					for(int i = 0; i < writer.curModule.predefined_type_files.size(); i++)
					    {
						String str = (String) writer.curModule.predefined_type_files.elementAt(i);
						if(str.endsWith(aux+"n"+writer.hpp_ext))
						    {
							writer.curModule.predefined_type_files.remove(i);
						    }
					    }
				    }
				writer.curModule.designLog += new Date() + " removed auxilliary channel "+aux+"\n";
				writer.designLog +=  new Date() + " removed auxilliary channel "+aux+" from "+writer.curModule.Name+"\n";
				MarkModuleFiles(writer.curModule.Name,false);
				if(null != writer.curModule.AuxInputNames)
				    {
					for(int i = 0; i < writer.curModule.AuxInputNames.size(); i++)
					    {
						String AuxInput = (String) writer.curModule.AuxInputNames.elementAt(i);
						if(AuxInput.equals(aux))
						    {
							writer.curModule.AuxInputNames.remove(i);
							if(null == writer.curModule.deletedAuxInputNames)
							    {
								writer.curModule.deletedAuxInputNames = new Vector();
							    }
							writer.curModule.deletedAuxInputNames.addElement(aux);
							break;
						    }
					    }
				    }
				if(null != writer.curModule.AuxOutputNames)
				    {
					for(int i = 0; i < writer.curModule.AuxOutputNames.size(); i++)
					    {
						String AuxOutput = (String) writer.curModule.AuxOutputNames.elementAt(i);
						if(AuxOutput.equals(aux))
						    {
							writer.curModule.AuxOutputNames.remove(i);
							if(null == writer.curModule.deletedAuxOutputNames)
							    {
								writer.curModule.deletedAuxOutputNames = new Vector();
							    }
							writer.curModule.deletedAuxOutputNames.addElement(aux);
							break;
						    }
					    }
				    }
				BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(aux);
				if(null == bi)
				    {
					return;
				    }
				if(null != bi.readerNames)
				    {
					for(int i = 0; i < bi.readerNames.size(); i++)
					    {
						String readername = (String) bi.readerNames.get(i);
						if(readername.equals(writer.curModule.Name))
						    {
							bi.readerNames.remove(i);
							break;
						    }
					    }
				    }
				if(null != bi.writerNames)
				    {
					for(int i = 0; i < bi.writerNames.size(); i++)
					    {
						String writername = bi.writerNames.get(i);
						if(writername.equals(writer.curModule.Name))
						    {
							bi.writerNames.remove(i);
							break;
						    }
					    }
				    }
				bi.channelsHashtable.remove(writer.curModule.Name);
				int num_readers = 0;
				if(bi.readerNames != null)
				    {
					num_readers = bi.readerNames.size();
				    }
				int num_writers = 0;
				if(bi.writerNames != null)
				    {
					num_writers = bi.writerNames.size();
				    }
				if(num_readers <= 1 && num_writers <= 1 && bi.channelsHashtable.size() <= 1)
				    {
					writer.buffersHashtable.remove(aux);
					for(int i = 0; i < writer.auxChannelsVector.size(); i++)
					    {
						String aux_from_list = (String) writer.auxChannelsVector.elementAt(i);
						if(aux.equals(aux_from_list))
						    {
							writer.auxChannelsVector.remove(i);
							break;
						    }
					    }
					File deletedFilesDir = new File(writer.getUserDir()+File.separator+"trash");
					if(!deletedFilesDir.exists())
					    {
						deletedFilesDir.mkdirs();
					    }
					String auxHeaderName = null;
					if(singleDir)
					    {
						auxHeaderName = aux+"n"+writer.hpp_ext;
					    }
					else
					    {
						auxHeaderName = writer.getUserDir()+File.separator+"src"+File.separator+"intf"+File.separator+aux+"n"+writer.hpp_ext;
					    }
					File auxHeaderFile = new File(auxHeaderName);

					if(auxHeaderFile.exists())
					    {
						File deletedFile = new File(deletedFilesDir, aux+"n"+writer.hpp_ext);
						auxHeaderFile.renameTo(deletedFile);
					    }
					

					String auxCppName = null;
					if(singleDir)
					    {
						auxCppName = aux+"n"+writer.cpp_ext;
					    }
					else
					    {
						auxCppName = writer.getUserDir()+File.separator+"src"+File.separator+"intf"+File.separator+aux+"n"+writer.cpp_ext;
					    }
					File auxCppFile = new File(auxCppName);
					if(auxCppFile.exists())
					    {
						File deletedFile = new File(deletedFilesDir, aux+"n"+writer.cpp_ext);
						auxCppFile.renameTo(deletedFile);
					    }

					String auxGenName = null;
					if(singleDir)
					    {
						auxGenName = aux+"n.gen";

					    }
					else
					    {
						auxGenName = writer.getUserDir()+File.separator+"src"+File.separator+"intf"+File.separator+aux+"n.gen";
					    }
					File auxGenFile = new File(auxGenName);

					if(auxGenFile.exists())
					    {
						File deletedFile = new File(deletedFilesDir, aux+"n.gen");
						auxGenFile.renameTo(deletedFile);
					    }
				    }
				CreateBufferList();
				return;
			    }
		    }



		if(event_source == hppExtField)
		    {
			synchronized(this)
			    {
				String new_hpp_ext = hppExtField.getText();
				if(!new_hpp_ext.equals(writer.hpp_ext))
				    {
					writer.designLog += new Date()+" C++ header file extension changed from "+writer.hpp_ext+" to "+new_hpp_ext+"\n";
					MarkAllFiles();
				    }
				writer.hpp_ext = hppExtField.getText();
				file_list_needs_to_be_modified = true;
			    }
		    }
		if(event_source == cppExtField)
		    {
			synchronized(this)
			    {
				String new_cpp_ext = cppExtField.getText();
				if(!new_cpp_ext.equals(writer.cpp_ext))
				    {
					writer.designLog += new Date()+" C++ file extension changed from "+writer.cpp_ext+" to "+new_cpp_ext+"\n";
					MarkAllFiles();
				    }
				writer.cpp_ext = cppExtField.getText();
				file_list_needs_to_be_modified = true;
			    }
		    }
		if(event_source == saveFileButton)
		    {
			synchronized(this)
			    {
				SaveCurrentTextFile();
			    }
			return;
		    }
		if(event_source == updateFileButton)
		    {
			synchronized(this)
			    {
				UpdateServerInfo();
				int file_index = filesList.getSelectedIndex();
				String file = getFilesListItem(file_index);
				if(debug_on)
				    {
					System.out.println(file + " selected");
				    }
				if(null == file)
				    {
					return;
				    }
				FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(file);
				if(null == fti)
				    {
					System.err.println("No entry in file types hashtable for "+file);
					return;
				    }
				if(fti.in_use)
				    {
					if(debug_on)
					    {
						System.out.println("File in use "+fti);
					    }
					return;
				    }
				fti.in_use = true;
				if(text_file_needs_saving)
				    {
					SaveCurrentTextFile();
				    }
				fti.generated = false;
				UpdateWriter();
				fileToUpdate = fti;
				StartLongFunction(UPDATE_FILE);
			    }
			return;
		    }
		if(event_source == appnameField)
		    {
			synchronized(this)
			    {
				String new_AppName = appnameField.getText();
				if(!new_AppName.equals(writer.AppName))
				    {
					writer.designLog += new Date()+" Application name changed from "+writer.AppName+" to "+new_AppName+"\n";
					MarkAllFiles();
				    }
				writer.setUserDir(userdirField.getText());
				File userDirFile = new File(writer.getUserDir());
				if(userDirFile.getName().equals(writer.AppName))
				    {
					writer.setUserDir(userDirFile.getParent());
				    }
				writer.AppDir = appdirField.getText();
				File AppDirFile = new File(writer.AppDir);
				if(AppDirFile.getName().equals(writer.AppName))
				    {
					writer.AppDir = AppDirFile.getParent();
				    }
				writer.AppName = appnameField.getText();
				userDirFile = new File(writer.getUserDir());
				if(!userDirFile.getName().equals(writer.AppName))
				    {
					writer.setUserDir(userDirFile.getParent()+writer.AppName);
					userdirField.setText(writer.getUserDir());
				    }
				AppDirFile = new File(writer.AppDir);
				if(!AppDirFile.getName().equals(writer.AppName))
				    {
					File newAppDirFile = new File(AppDirFile,writer.AppName);
					writer.AppDir = newAppDirFile.toString();
					appdirField.setText(writer.AppDir);
				    }
				userDirFile.mkdirs();
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }

		if(event_source == addModuleField)
		    {
			synchronized(this)
			    {
				hierarchy_modified = true;
				String new_module_name = addModuleField.getText();
				writer.designLog += new Date() +" adding Module "+new_module_name+"\n";
				boolean set_server_needed = false;
				String curSvName = serverList.getSelectedItem();
				if(serverList.getSelectedItem() == null)
				    {
					set_server_needed = true;
				    }
				else if (!curSvName.startsWith(writer.AppName))
				    {
					set_server_needed = true;
				    }
				if(!writer.modulesHashtable.containsKey(new_module_name))
				    {
					ModuleInfo modInfo = new ModuleInfo( new DiagNMLMsgDictCreator(), rcs.nml.NMLConnection.Creator );
					modulesList.add(new_module_name);
					subordinatesList.add(new_module_name);
					modsInLoopList.add(new_module_name);
					modInfo.Name = new_module_name;

					modInfo.m_loadingPanel = loadPanel;
					modInfo.moduleClassName = modInfo.Name.toUpperCase()+"_MODULE";
					modInfo.designLog += new Date() + " created.\n";
					if(null == modInfo.predefined_type_files)
					    {
						modInfo.predefined_type_files = new Vector();
					    }
					if(singleDir)
					    {
						modInfo.cmdsTypeFile = modInfo.Name+"n"+writer.hpp_ext;
						modInfo.statsTypeFile = modInfo.Name+"n"+writer.hpp_ext;
					    }
					else
					    {
						modInfo.cmdsTypeFile = "src"+File.separator+"intf"+File.separator+modInfo.Name+"n"+writer.hpp_ext;
						modInfo.statsTypeFile = "src"+File.separator+"intf"+File.separator+modInfo.Name+"n"+writer.hpp_ext;
						modInfo.SourceCodeDirectory = "src"+File.separator+modInfo.Name+File.separator;
					    }
					modInfo.cmdsBaseClass = new Vector();
					if(null != modInfo.m_stat_read_Connection)
					    {
						if(null != modInfo.m_stat_read_Connection.get_buffer_name())
						    {
							modInfo.m_stat_read_Connection.set_buffer_name(modInfo.Name+"_sts");
						    }
					    }
					if(null != modInfo.m_cmd_read_Connection)
					    {
						if(null != modInfo.m_cmd_read_Connection.get_buffer_name())
						    {
							modInfo.m_cmd_read_Connection.set_buffer_name(modInfo.Name+"_cmd");
						    }
					    }

					modInfo.NMLConfigurationFile = appnameField.getText()+".nml";
					modInfo.releaseIncludeDirectory  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"include";
					if(writer.mswinDevPlat)
					    {
						modInfo.releaseLibrary  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+appnameField.getText()+".lib";
					    }
					else
					    {
						modInfo.releaseLibrary  = appdirField.getText() + File.separator+"plat"+File.separator+"$(PLAT)"+File.separator+"lib"+File.separator+"lib"+appnameField.getText()+".a";
					    }
					modInfo.cmdsAvailable = new Vector();
					modInfo.cmdsAvailable.addElement(new_module_name.toUpperCase()+"_INIT");
					modInfo.cmdsAvailable.addElement(new_module_name.toUpperCase()+"_HALT");
					writer.modulesHashtable.put(new_module_name,modInfo );
					file_list_needs_to_be_modified = true;
				    }
				if(modulesList.getSelectedIndex() < 0)
				    {
					if(null != cmdList)
					    {
						cmdList.removeAll();
						cmdList.add(new_module_name.toUpperCase()+"_INIT");
						cmdList.add(new_module_name.toUpperCase()+"_HALT");
					    }
					for(int i = 0; i < modulesList.getItemCount(); i++)
					    {
						if(modulesList.getItem(i).equals(new_module_name))
						    {
							modulesList.select(i);
						    }
					    }
					writer.curModule =  (ModuleInfo) writer.modulesHashtable.get(new_module_name);
				    }
				else
				    {
					String parentModName = modulesList.getSelectedItem();
					if(null != parentModName)
					    {
						ModuleInfo parentModule = (ModuleInfo) writer.modulesHashtable.get(parentModName);
						if(null != parentModule)
						    {
							parentModule.designLog += new Date()+" subordinate "+new_module_name+" added.\n";
						    }
						MarkModuleFiles(parentModName, false);
					    }
					for(int i = 0; i < subordinatesList.getItemCount(); i++)
					    {
						if(subordinatesList.getItem(i).equals(new_module_name))
						    {
							subordinatesList.select(i);
						    }
					    }

				    }

				UpdateServerInfo();
				CreateBufferList();
				if(set_server_needed)
				    {
					int i;
					boolean appsvr_found = false;
					for(i = 0; i < serverList.getItemCount(); i++)
					    {
						String svName = serverList.getItem(i);
						if(svName.startsWith(writer.AppName))
						    {
							serverList.select(i);
							ServerInfo tempServer = (ServerInfo) writer.serversHashtable.get(svName);
							if(null != tempServer)
							    {
								writer.curServer = tempServer;
								writer.curServer.bufferNames.addElement(new_module_name+"_cmd");
								writer.curServer.bufferNames.addElement(new_module_name+"_sts");
								appsvr_found = true;
							    }
							break;
						    }
					    }
					if(!appsvr_found)
					    {
						serverList.add(writer.AppName+"svr");
						serverList.select(serverList.getItemCount() -1);
						ServerInfo tempServer = new ServerInfo(writer.AppName+"svr","host=\"localhost\";");
						writer.serversHashtable.put(writer.AppName+"svr",tempServer);
						writer.curServer = (ServerInfo) writer.serversHashtable.get(writer.AppName+"svr");
						if(null != writer.curServer)
						    {
							writer.curServer.bufferNames.addElement(new_module_name+"_cmd");
							writer.curServer.bufferNames.addElement(new_module_name+"_sts");
						    }
					    }
				    }
				UpdateServerInfo();
				String loop_name = mainLoopList.getSelectedItem();
				boolean loop_set_needed = false;
				if(loop_name == null)
				    {
					loop_set_needed = true;
				    }
				else if(!loop_name.startsWith(writer.AppName))
				    {
					loop_set_needed = true;
				    }
				if(loop_set_needed)
				    {
					if(debug_on)
					    {
						System.out.println("");
						System.out.println("Setting current main loop to "+writer.AppName+" before adding module.");
						System.out.println("writer.curmainLoop = "+writer.curmainLoop);
					    }
					if(writer.curmainLoop != null)
					    {
						String modules[] =  modsInLoopList.getSelectedItems();
						writer.curmainLoop.clearModules();
						for(int i = 0; i < modules.length; i++)
						    {
							writer.curmainLoop.addModule(modules[i]);
						    }
						if(debug_on)
						    {
							System.out.println("writer.curmainLoop.getModules() = "+writer.curmainLoop.getModules());
						    }
						try
						    {
							writer.curmainLoop.cycle_time = Double.valueOf(cycletimeField.getText()).doubleValue();
							writer.curmainLoop.host = mainLoopHostField.getText();
						    }
						catch(Exception e)
						    {
							e.printStackTrace();
						    }
					    }
					int module_indexes[] = modsInLoopList.getSelectedIndexes();
					for(int i = 0; i < module_indexes.length; i++)
					    {
						modsInLoopList.deselect(module_indexes[i]);
					    }
					Thread.sleep(10);
					int i;
					boolean apploop_found = false;
					for(i = 0; i < mainLoopList.getItemCount(); i++)
					    {
						String loopName = mainLoopList.getItem(i);
						if(debug_on)
						    {
							System.out.println("mainLoopList.getItem(i="+i+") ="+loopName);
						    }
						if(loopName.startsWith(writer.AppName))
						    {
							mainLoopList.select(i);
							loop_name = loopName;
							rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(loopName);
							if(null != loopInfo)
							    {
								writer.curmainLoop =  loopInfo;
								loopInfo.addModule(new_module_name);
							    }
							apploop_found = true;
							break;
						    }
					    }
					if(!apploop_found)
					    {
						rcsdesignMainLoopInfo loopInfo  = new rcsdesignMainLoopInfo(writer.AppName);
						writer.mainloopsHashtable.put(writer.AppName, loopInfo);
						mainLoopList.add(writer.AppName);
						mainLoopList.select(mainLoopList.getItemCount() -1);
						loop_name = writer.AppName;
						writer.curmainLoop =  loopInfo;
					    }
					String new_mainLoop_name = mainLoopList.getSelectedItem();
					if(debug_on)
					    {
						System.out.println("new_mainLoop_name = "+new_mainLoop_name);
					    }
					if(null != new_mainLoop_name && null != writer.mainloopsHashtable)
					    {
						writer.curmainLoop = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(new_mainLoop_name);
						if(null != writer.curmainLoop)
						    {
							if(null != modulesList)
							    {
								modsInLoopList.removeAll();
								Thread.sleep(10);
								for(i = 0; i < modulesList.getItemCount(); i++)
								    {
									modsInLoopList.add(modulesList.getItem(i));
									if(null != writer.curmainLoop)
									    {
										if(null != writer.curmainLoop.getModules())
										    {
											for(int j = 0; j < writer.curmainLoop.getModules().size(); j++)
											    {
												if(modsInLoopList.getItem(i).equals((String) writer.curmainLoop.getModules().elementAt(j)))
												    {
													modsInLoopList.select(i);
													break;
												    }
											    }
										    }
									    }
								    }
							    }
							cycletimeField.setText(String.valueOf(writer.curmainLoop.cycle_time));
							if(null == writer.curmainLoop.host)
							    {
								writer.curmainLoop.host = "localhost";
							    }
							mainLoopHostField.setText(writer.curmainLoop.host);
						    }
					    }
					UpdateMainLoopInfo();
					MarkLoopFiles(new_mainLoop_name);
				    }
				if(debug_on)
				    {
					System.out.println("loop_name = "+loop_name);
				    }
				for(int i = 0; i < modsInLoopList.getItemCount(); i++)
				    {
					if(modsInLoopList.getItem(i).equals(new_module_name))
					    {
						modsInLoopList.select(i);
						int tries = 0;
						while(!modsInLoopList.isIndexSelected(i) && tries < 20)
						    {
							if(tries > 1)
							    {
								System.err.println("modsInLoopList.select("+i+") called but modsInLoopList.isIndexSelected("+i+") returned false.");
							    }
							Thread.sleep(20);
							modsInLoopList.select(i);
							tries++;
						    }
						if(debug_on)
						    {
							System.out.println("modsInLoopList.select("+i+")");
						    }
					    }
				    }
				if(debug_on)
				    {
					System.out.println("writer.curmainLoop = "+writer.curmainLoop);
					String mods_in_loop[] = modsInLoopList.getSelectedItems();
					System.out.println("modsInLoopList.getSelectedItems() :");
					for(int i = 0; i < mods_in_loop.length; i++)
					    {
						System.out.println("\t"+mods_in_loop[i]);
					    }
				    }
				if(null != loop_name)
				    {
					rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo)    writer.mainloopsHashtable.get(loop_name);
					if(debug_on)
					    {
						System.out.println("writer.mainloopsHashtable.get("+loop_name+")="+loopInfo);
					    }
					if(null != loopInfo)
					    {
						writer.curmainLoop = loopInfo;
						boolean add_to_loop = true;
						for(int i = 0; i < loopInfo.getModules().size(); i++)
						    {
							if(debug_on)
							    {
								System.out.println("loopInfo.getModules().elementAt("+i+") = "+loopInfo.getModules().elementAt(i));
							    }
							if(new_module_name.equals((String) loopInfo.getModules().elementAt(i)))
							    {
								add_to_loop = false;
								break;
							    }
						    }
						if(add_to_loop)
						    {
							loopInfo.addModule(new_module_name);
							if(debug_on)
							    {
								System.out.println("loopInfo.addModule(new_module_name ="+new_module_name+");");
								System.out.println("add_to_loop="+add_to_loop);
							    }

						    }
					    }
				    }
				if(debug_on)
				    {
					System.out.println("writer.curmainLoop = "+writer.curmainLoop);
					String mods_in_loop[] = modsInLoopList.getSelectedItems();
					System.out.println("modsInLoopList.getSelectedItems() :");
					for(int i = 0; i < mods_in_loop.length; i++)
					    {
						System.out.println("\t"+mods_in_loop[i]);
					    }
				    }
				addModuleField.setText("");
				BuildHierarchyDisplay();
				CreateBufferList();
			    }
			return;
		    }
		if(event_source == addmainLoopField)
		    {
			synchronized(this)
			    {
				String new_mainLoop_name = addmainLoopField.getText();
				writer.designLog += new Date() + " Adding main loop "+ new_mainLoop_name+"\n";
				MarkCommonFiles();
				if(!writer.mainloopsHashtable.containsKey(new_mainLoop_name))
				    {
					mainLoopList.add(new_mainLoop_name);
					rcsdesignMainLoopInfo tempMli = new rcsdesignMainLoopInfo(new_mainLoop_name);
					writer.mainloopsHashtable.put(new_mainLoop_name, tempMli );
				    }
				if(mainLoopList.getSelectedIndex() < 0)
				    {
					for(int i = 0; i < mainLoopList.getItemCount(); i++)
					    {
						if(mainLoopList.getItem(i).equals(new_mainLoop_name))
						    {
							mainLoopList.select(i);
						    }
					    }
					writer.curmainLoop =  (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(new_mainLoop_name);
				    }
				addmainLoopField.setText("");
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }
		if(event_source == addPlatField)
		    {
			synchronized(this)
			    {
				platList.add(addPlatField.getText());
				addPlatField.setText("");
			    }
			return;
		    }
		if(event_source == addLibField)
		    {
			synchronized(this)
			    {
				libsList.add(addLibField.getText());
				addLibField.setText("");
			    }
			return;
		    }
		if(event_source == addIncludeField)
		    {
			synchronized(this)
			    {
				includesList.add(addIncludeField.getText());
				addIncludeField.setText("");
			    }
			return;
		    }
		if(event_source == addcmdField)
		    {
			synchronized(this)
			    {
				String cur_mod_name = modulesList.getSelectedItem();
				if(null != cur_mod_name && null != writer.curModule)
				    {
					if(!cur_mod_name.equals(writer.curModule.Name))
					    {
						Alert("Current Module Invalid "+cur_mod_name+" != "+writer.curModule.Name);
						int selected_index = modulesList.getSelectedIndex();
						modulesList.deselect(selected_index);
						writer.curModule = null;
						return;
					    }
					String cmdName = cur_mod_name.toUpperCase()+"_"+addcmdField.getText().toUpperCase();
					for(int j = 0; j < cmdList.getItemCount(); j++)
					    {
						String cmdFromList = cmdList.getItem(j);
						if(cmdFromList.equals(cmdName))
						    {
							addcmdField.setText("");
							return;
						    }
					    }
					if(debug_on)
					    {
						System.out.println("Adding the Command "+cmdName+" to "+writer.curModule.Name);
					    }
					writer.curModule.designLog += new Date() + " Adding the Command "+cmdName+" to "+writer.curModule.Name+"\n";
					writer.designLog += new Date() + " Adding the Command "+cmdName+" to "+writer.curModule.Name+"\n";
					MarkModuleFiles(writer.curModule.Name, true);
					cmdList.add(cmdName);
					writer.curModule.cmdsAvailable.removeAllElements();
					for(int i = 0; i < cmdList.getItemCount(); i++)
					    {
						writer.curModule.cmdsAvailable.addElement( cmdList.getItem(i));
					    }
				    }
				addcmdField.setText("");
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }
		if(event_source == delModuleButton)
		    {
			synchronized(this)
			    {
				hierarchy_modified = true;
				int mod_index = modulesList.getSelectedIndex();
				if(mod_index >= 0)
				    {
					String mod_to_delete = modulesList.getItem(mod_index);
					if(null != writer.modulesHashtable && null != mod_to_delete)
					    {
						ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(mod_to_delete);
						if(null != modInfo)
						    {
							if(null != modInfo.parent)
							    {
								if(null != modInfo.parent.Name)
								    {
									MarkModuleFiles(modInfo.parent.Name, false);
								    }
							    }
						    }
						if(writer.modulesHashtable.containsKey(mod_to_delete))
						    {
							writer.modulesHashtable.remove(mod_to_delete);
						    }
					    }
					modulesList.remove(mod_index);
					for(int i = 0; i < subordinatesList.getItemCount(); i++)
					    {
						if(subordinatesList.getItem(i).equals(mod_to_delete))
						    {
							subordinatesList.remove(i);
							continue;
						    }
						subordinatesList.deselect(i);
					    }
					for(int i = 0; i < modsInLoopList.getItemCount(); i++)
					    {
						if(modsInLoopList.getItem(i).equals(mod_to_delete))
						    {
							modsInLoopList.remove(i);
							continue;
						    }
					    }
					DeleteModuleFiles(mod_to_delete);
				    }
				file_list_needs_to_be_modified = true;
				BuildHierarchyDisplay();
				CreateBufferList();
			    }
			return;
		    }
		if(event_source == delmainLoopButton)
		    {
			synchronized(this)
			    {
				int main_index = mainLoopList.getSelectedIndex();
				if(main_index >= 0)
				    {
					String main_to_delete = mainLoopList.getItem(main_index);
					writer.designLog += new Date() +" Remove the main loop "+main_to_delete+"\n";
					if(null != writer.mainloopsHashtable && null != main_to_delete)
					    {
						if(writer.mainloopsHashtable.containsKey(main_to_delete))
						    {
							writer.mainloopsHashtable.remove(main_to_delete);
						    }
					    }
					mainLoopList.remove(main_index);
				    }
				for(int i = 0; i < modsInLoopList.getItemCount(); i++)
				    {
					modsInLoopList.deselect(i);
				    }
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }
		if(event_source == delPlatButton)
		    {
			synchronized(this)
			    {
				int plat_index = platList.getSelectedIndex();
				if(plat_index >= 0)
				    {
					platList.remove(plat_index);
				    }
			    }
			return;
		    }
		if(event_source == delLibButton)
		    {
			synchronized(this)
			    {
				int Lib_index = libsList.getSelectedIndex();
				if(Lib_index >= 0)
				    {
					libsList.remove(Lib_index);
				    }
			    }
			return;
		    }
		if(event_source == delIncludeButton)
		    {
			synchronized(this)
			    {
				int Include_index = includesList.getSelectedIndex();
				if(Include_index >= 0)
				    {
					includesList.remove(Include_index);
				    }
			    }
			return;
		    }
		if(event_source == printHashtablesButton)
		    {
			synchronized(this)
			    {
				PrintHashtables();
			    }
			return;
		    }

		if(event_source == delcmdButton)
		    {
			synchronized(this)
			    {
				int cmd_index = cmdList.getSelectedIndex();
				String cmdName = cmdList.getSelectedItem();
				if(cmd_index >= 0)
				    {
					cmdList.remove(cmd_index);
				    }
				String cur_mod_name = modulesList.getSelectedItem();
				if(null != cur_mod_name && null != writer.curModule)
				    {
					if(!cur_mod_name.equals(writer.curModule.Name))
					    {
						Alert("Current Module Invalid "+cur_mod_name+" != "+writer.curModule.Name);
						int selected_index = modulesList.getSelectedIndex();
						modulesList.deselect(selected_index);
						writer.curModule = null;
						return;
					    }
					if(writer.curModule.deleted_commands == null)
					    {
						writer.curModule.deleted_commands = new Vector();
					    }
					writer.curModule.deleted_commands.addElement(cmdName);
				    }
				writer.curModule.designLog += new Date() + " Removing the Command "+cmdName+" from "+writer.curModule.Name+"\n";
				writer.designLog += new Date() + " Removing the Command "+cmdName+" from "+writer.curModule.Name+"\n";
				MarkModuleFiles(writer.curModule.Name, true);
				file_list_needs_to_be_modified = true;
			    }
			return;
		    }
		if(event_source == printButton)
		    {
			synchronized(this)
			    {
				if(screenChoice.getSelectedItem().equals("Hierarchy"))
				    {
					hierarchyPanel.print();
				    }
				else if(screenChoice.getSelectedItem().equals("Files"))
				    {
					PrintCurrentFile();
				    }
			    }
			return;
		    }

		if(event_source == makeButton)
		    {
			synchronized(this)
			    {
				writer.setUserDir(userdirField.getText());
				//          SystemProps.cd(writer.UserDir);
				if(!writer.mswinDevPlat)
				    {
					writer.MakeCommand = makeCommandField.getText();
					String make_command = writer.MakeCommand;
					writer.TerminalCommand = terminalCommandField.getText();
					String terminal_launch_command = writer.TerminalCommand;
					if(terminal_launch_command.length() > 0 &&
					   !terminal_launch_command.endsWith(" "))
					    {
						terminal_launch_command += " ";
					    }
					if(null != make_command)
					    {
						if(make_command.length() > 1)
						    {
							make_command = ReplaceVars(terminal_launch_command+make_command);
							System.out.println("Executing "+make_command );
							Runtime.getRuntime().exec(make_command);

						    }
					    }
				    }
				else
				    {
					RunMakeBatchFile(null);
				    }
			    }
			return;
		    }

		if(event_source == runButton)
		    {
			synchronized(this)
			    {
				writer.setUserDir(userdirField.getText());
				//          SystemProps.cd(writer.UserDir);
				String run_command = runCommandField.getText();
				writer.TerminalCommand = terminalCommandField.getText();
				String terminal_launch_command = writer.TerminalCommand;
				if(terminal_launch_command.length() > 0 &&
				   !terminal_launch_command.endsWith(" "))
				    {
					terminal_launch_command += " ";
				    }
				if(null != run_command)
				    {
					if(run_command.length() > 1)
					    {
						run_command = ReplaceVars(terminal_launch_command+run_command);
						System.out.println("Executing "+run_command);
						Runtime.getRuntime().exec(run_command);
					    }
				    }
				if(null != main_window)
				    {
					main_window.toBack();
				    }
			    }
			return;
		    }

		if(event_source == diagButton)
		    {
			synchronized(this)
			    {
				writer.setUserDir(userdirField.getText());
				//          SystemProps.cd(writer.UserDir);
				if( null != diagCommandField)
				    {
					diag_cmd = diagCommandField.getText();
					
				    }
				if(null == diag_cmd || diag_cmd.length() < 2)
				    {
					diag_cmd=writer.java_cmd_prefix+" -jar "+writer.RcsLibDir+File.separator+"plat"+File.separator+"java"+File.separator+"lib"+File.separator+"diag_NB.jar";
				    }
				if(null != diag_cmd)
				    {
					if(diag_cmd.length() > 1)
					    {
						diag_cmd += " HierarchyFile="+writer.AppName+".cfg  ";
						String terminal_launch_command = terminalCommandField.getText();
						if(terminal_launch_command.length() > 0 &&
						   !terminal_launch_command.endsWith(" "))
						    {
							terminal_launch_command += " ";
						    }
						diag_cmd = ReplaceVars(terminal_launch_command+diag_cmd);
						System.out.println("Executing "+diag_cmd);
						Runtime.getRuntime().exec(diag_cmd);
					    }
				    }
				if(null != main_window)
				    {
					main_window.toBack();
				    }
			    }
			return;
		    }

		if(event_source == helpButton)
		    {
			synchronized(this)
			    {
				writer.setUserDir(userdirField.getText());
				//          SystemProps.cd(writer.UserDir); 
				if(null != browserCommandField)
				    {
					browser_cmd = browserCommandField.getText();
				    }
				String help_command = browser_cmd + " http://isd.cme.nist.gov/proj/rcs_lib/  ";
				if(null != help_command)
				    {
					if(help_command.length() > 1)
					    {
						help_command = ReplaceVars(help_command);
						System.out.println("Executing "+help_command);
						Runtime.getRuntime().exec(help_command);
					    }
				    }
				if(null != main_window)
				    {
					main_window.toBack();
				    }
			    }
			return;
		    }

		if(event_source == importButton)
		    {
			synchronized(this)
			    {
				ImportController();
			    }
			return;
		    }

		if(event_source == openButton)
		    {
			synchronized(this)
			    {
				OpenController();
			    }
			return;
		    }

		if(event_source == newButton)
		    {
			synchronized(this)
			    {
				NewController();
			    }
			return;
		    }

		if(event_source == updateAllFilesButton)
		    {
			synchronized(this)
			    {
				update_all_files=true;
				MarkAllFiles();
				writer.AppName = appnameField.getText();
				writer.setUserDir(userdirField.getText());
				if(writer.getUserDir().endsWith(File.separator))
				    {
					writer.setUserDir(writer.getUserDir().substring(0,writer.getUserDir().length()-1));
				    }
				File userDirFile = new File(writer.getUserDir());
				if(!userDirFile.getName().equals(writer.AppName) && userDirFile.getName().length() > 0)
				    {
					writer.setUserDir(userDirFile.getParent() +File.separator+ writer.AppName);
					userdirField.setText(writer.getUserDir());
				    }
				File AppDirFile = new File(writer.AppDir);
				if(!AppDirFile.getName().equals(writer.AppName))
				    {
					File newAppDirFile = new File(AppDirFile,writer.AppName);
					writer.AppDir = newAppDirFile.toString();
					appdirField.setText(writer.AppDir);
				    }
				StartLongFunction(CREATE_SOURCE);
				read_everything_needed = false;
				hierarchy_modified = true;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    void StartLongFunction(int new_run_type)
    {
	try
	    {
		KillGenerateEverythingThread();
		run_function = new_run_type;
		if(GeneratingEverything)
		    {
			GeneratingEverything = false;
			run_function = DO_NOTHING;
		    }
		else
		    {
			GeneratingEverything = true;
			GenerateEverything();
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    /*
     * This function is called when the thread that can either be used to generate code or to read configuration
     * files should be terminated. Such as by a user request.
     * Thread.destroy() is not implemented and Thread.stop is deprecated.
     *  However there is no other reasonable way of stopping the thread. Using an abort flag as the
     * JDK documentation suggests will not work because it would need to be checked in too many places.
     */
    private void KillGenerateEverythingThread()
    {
	if(null == generateEverything_thread)
	    {
		return;
	    }
	try
	    {
		if(null != generateEverything_thread)
		    {
			//generateEverything_thread.stop();
			generateEverything_thread = null;
		    }
	    }
	catch(Exception e)
	    {
	    }
	try
	    {
		Thread.sleep(50);
	    }
	catch(Exception e)
	    {
	    }
	EnableAllControls();
	generateEverything_thread = null;
    }

    String ReplaceVar(String in, String from, String to)
    {
	if(debug_on)
	    {
		System.out.println("Replace "+from+" with "+to+" in "+in);
	    }
	int fromindex = in.indexOf(from);
	if(fromindex < 0)
	    {
		return in;
	    }
	String out = in;
	while(fromindex >= 0)
	    {
		if(debug_on)
		    {
			System.out.println("out = "+out);
			System.out.println("fromindex = "+fromindex);
		    }
		String firstpart = "";
		if(fromindex > 0)
		    {
			firstpart = out.substring(0,fromindex);
		    }
		int endindex = fromindex + from.length();
		if(debug_on)
		    {
			System.out.println("endindex = "+endindex);
		    }
		String lastpart = "";
		if(endindex < out.length())
		    {
			lastpart = out.substring(endindex);
		    }
		out =  firstpart+ to + lastpart;
		fromindex = out.indexOf(from, fromindex + to.length()+1);
	    }
	return out;
    }

    String ReplaceVars(String cmd)
    {
	String plat = platList.getSelectedItem();
	if(null != plat)
	    {
		cmd = ReplaceVar(cmd, "$PLAT",plat);
		cmd = ReplaceVar(cmd, "$(PLAT)",plat);
		cmd = ReplaceVar(cmd, "%PLAT%",plat);
	    }
	writer.AppName = appnameField.getText();
	cmd = ReplaceVar(cmd, "$APPNAME", writer.AppName);
	cmd = ReplaceVar(cmd, "$(APPNAME)", writer.AppName);
	cmd = ReplaceVar(cmd, "%APPNAME%", writer.AppName);
	writer.AppDir = appdirField.getText();
	cmd = ReplaceVar(cmd, "$APPDIR", writer.AppDir);
	cmd = ReplaceVar(cmd, "$(APPDIR)", writer.AppDir);
	cmd = ReplaceVar(cmd, "%APPDIR%", writer.AppDir);
	writer.setUserDir(userdirField.getText());
	cmd = ReplaceVar(cmd, "$USER_DIR", writer.getUserDir());
	cmd = ReplaceVar(cmd, "$(USER_DIR)", writer.getUserDir());
	cmd = ReplaceVar(cmd, "%USER_DIR%", writer.getUserDir());
	writer.RcsLibDir = rcslibdirField.getText();
	cmd = ReplaceVar(cmd, "$RCSLIB_MAIN_DIR", writer.RcsLibDir);
	cmd = ReplaceVar(cmd, "$(RCSLIB_MAIN_DIR)", writer.RcsLibDir);
	cmd = ReplaceVar(cmd, "%RCSLIB_MAIN_DIR%", writer.RcsLibDir);
	cmd = ReplaceVar(cmd, "$RCS_DIR", writer.RcsLibDir);
	cmd = ReplaceVar(cmd, "$(RCS_DIR)", writer.RcsLibDir);
	cmd = ReplaceVar(cmd, "%RCS_DIR%", writer.RcsLibDir);
	return cmd;
    }

    String lastFailedToSaveTextFile = null;
    void SaveCurrentTextFile()
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsDesign.SaveCurrentTextFile() : current_fti="+current_fti);
		    }

		String currentText = fileTextArea.getText();
		if(null == currentText)
		    {
			return;
		    }
		if(currentText.length() < 1)
		    {
			return;
		    }
		try
		    {
			File parent = new File(current_fti.file.getParent());
			if(!parent.exists())
			    {
				parent.mkdirs();
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		if(current_fti.file.exists())
		    {
			if(!current_fti.file.canWrite())
			    {
				if(null != lastFailedToSaveTextFile)
				    {
					if(lastFailedToSaveTextFile.equals(current_fti.file.toString()))
					    {
						return;
					    }
				    }
				Alert("Can not save "+current_fti.file);
				lastFailedToSaveTextFile = current_fti.file.toString();
				return;
			    }
		    }
		FileOutputStream fos = new FileOutputStream(current_fti.file);
		byte b[] = currentText.getBytes();
		fos.write(b);
		fos.close();
		while(true)
		    {
			String old_label = fileTextLabel.getText();
			if(old_label.endsWith("(*)"))
			    {
				String new_label = old_label.substring(0,old_label.length()-3);
				fileTextLabel.setText(new_label);
				continue;
			    }
			else if(old_label.endsWith("(*)"))
			    {
				String new_label = old_label.substring(0,old_label.length()-1);
				fileTextLabel.setText(new_label);
				continue;
			    }
			break;
		    }
		current_fti.generated = false;
		text_file_needs_saving = false;
	    }
	catch(Exception e)
	    {
		if(null != lastFailedToSaveTextFile)
		    {
			if(lastFailedToSaveTextFile.equals(current_fti.file.toString()))
			    {
				return;
			    }
		    }
		Alert("Can not save "+current_fti.file+" : "+e.getMessage());
		lastFailedToSaveTextFile = current_fti.file.toString();
		e.printStackTrace();
	    }

    }


    void PrintCurrentFile()
    {
	if(null == current_fti)
	    {
		return;
	    }
	if(null == current_fti.file)
	    {
		return;
	    }
	System.out.println("Printing "+current_fti.file+"  . . .");
	int font_height = 10;
	int font_width = 5;
	try
	    {
		Font f = getFont();
		if(null != f)
		    {
			FontMetrics fm = getFontMetrics(f);
			if(null != fm)
			    {
				font_height = fm.getHeight();
				if(font_height < 10)
				    {
					font_height = 10;
				    }
				int font_widths[] = fm.getWidths();
				font_width = 0;
				for(int i = 0; i < font_widths.length; i++)
				    {
					if(font_width < font_widths[i])
					    {
						font_width = font_widths[i];
					    }
				    }
				if(font_width < 5)
				    {
					font_width = 5;
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(debug_on)
	    {
		System.out.println("font_height = "+font_height);
		System.out.println("font_width = "+font_width);
	    }
	while(null == parentFrame)
	    {
		try
		    {
			parentFrame = (Frame) getParent();
		    }
		catch(ClassCastException e)
		    {
			parentFrame = null;
			continue;
		    }
		break;
	    }
	String currentText = fileTextArea.getText();
	StringTokenizer  tokenizer = new StringTokenizer(currentText,"\r\n");
	int page = 1;
	PrintJob pj = Toolkit.getDefaultToolkit().getPrintJob(parentFrame,current_fti.name+page , null);
	if (pj != null)
	    {
		Dimension page_dim = pj.getPageDimension();
		while(tokenizer.hasMoreTokens())
		    {
			Graphics g = pj.getGraphics();
			if(null == g.getFont())
			    {
				g.setFont(this.getFont());
			    }
			g.setColor(Color.black);
			int line_spacing = font_height+1;
			line_spacing += 5 - (line_spacing%5);
			int line = 0;
			while(tokenizer.hasMoreTokens() && line*line_spacing+40 < page_dim.height)
			    {
				String token = tokenizer.nextToken();
				int xpos = 0;
				StringTokenizer tabTokenizer = new StringTokenizer(token,"\t");
				while(tabTokenizer.hasMoreTokens())
				    {
					String detabbedToken = tabTokenizer.nextToken();
					g.drawString(detabbedToken,10+xpos*font_width,20+line*line_spacing);
					xpos += detabbedToken.length() ;
					xpos += 3 - (xpos%3);
				    }
				line++;
			    }
			g.dispose();
			page++;
		    }
		pj.end();
	    }
    }

    public boolean canShutdown()
    {
	QueryDialog.cancel = false;
	shutting_down = true;
	try
	    {
		cleanup();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	shutting_down = !QueryDialog.cancel;
	return !QueryDialog.cancel;
    }

    public void startShutdown()
    {
	cleanup();
    }

    int CountFilesCheckedOut()
    {
	int count = 0;
	try
	    {
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			if(null == filename)
			    {
				continue;
			    }
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    {
				continue;
			    }
			if(fti.file.exists() && fti.file.canWrite())
			    {
				count++;
			    }
		    }
		return count;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		return 0;
	    }
    }

    public void cleanup()
    {
	shutting_down=true;
	StartLongFunction(CLEANUP);
    }

    protected void cleanup_in_background()
    {
	try
	    {
		boolean yes_to_all = false;
		boolean no_to_all = false;
		shutting_down = true;
		if(null != main_window)
		    {
			main_window.toBack();
		    }
		if(text_file_needs_saving && null != writer && null != current_fti)
		    {
			QueryDialog qd = Query("File "+current_fti.list_name +" has been modified.\nSave it?");
			while(!qd.done)
			    {
				try
				    {
					Thread.sleep(100);
				    }
				catch(Exception e)
				    {
					e.printStackTrace();
				    }
			    }
			if(qd.ok)
			    {
				writer.BackupFile(current_fti.file);
				SaveCurrentTextFile();
			    }
			if(QueryDialog.cancel)
			    {
				return;
			    }
		    }
		if(null == writer)
		    {
			return;
		    }
		int files_not_up_to_date = CountFilesNotUpToDate();
		if(files_not_up_to_date > 0 && modulesList.getItemCount() > 0)
		    {
			QueryDialog.cancel = false;
			QueryDialog qd = Query(files_not_up_to_date +" files are not up to date.\nUpdate them now?");
			while(!qd.done)
			    {
				try
				    {
					Thread.sleep(100);
				    }
				catch(Exception e)
				    {
					e.printStackTrace();
				    }
			    }
			if(qd.ok)
			    {
				if(!QueryDialog.cancel)
				    {
					CreateDirectories(userdirField.getText(),null);
				    }
				if(!QueryDialog.cancel)
				    {
					FillDirectories(userdirField.getText());
				    }
			    }
			if(!QueryDialog.cancel && !qd.no_to_all)
			    {
				int files_checkedout = CountFilesCheckedOut();
				if(files_checkedout > 0 && null != fileCheckinTypeChoice)
				    {
					qd= Query(files_checkedout +" files have been checked out of "+fileCheckinTypeChoice.getSelectedItem()+". \nCheck them back in now?");
					if(qd.ok)
					    {
						CheckInAllFiles();
					    }
				    }
			    }
		    }
		if(!QueryDialog.cancel)
		    {
			if(null != main_window)
			    {
				main_window.dispose();
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    boolean InsideResizableFrame()
    {
	if(!in_resizable_window)
	    {
		return false;
	    }

	Frame frm = null;
	Container container = null;
	try
	    {
		container = getParent();
		if(debug_on)
		    {
			System.out.println(String.valueOf(container));
		    }
		frm = (Frame) container;
	    }
	catch(ClassCastException cce)
	    {
		return m_fStandAlone;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		return m_fStandAlone;
	    }
	if(null == frm)
	    {
		return m_fStandAlone;
	    }
	return frm.isResizable();
    }

    boolean in_resizable_window = false;

    int old_width = 0;
    int old_height = 0;
    boolean resizing = false;
    int resize_width;
    int resize_height;

    protected void MarkCommonFiles()
    {
	if(null == filesList || null == writer)
	    {
		return;
	    }
	if(null == writer.fileTypeInfoHashtable)
	    {
		return;
	    }
	try
	    {
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    {
				throw new Exception("No FileTypeInfo for "+filename);
			    }
			if( fti.name.indexOf(".nml") >= 0 ||
			    fti.name.indexOf(".cfg") >= 0 ||
			    fti.name.indexOf(".bat") >= 0 ||
			    fti.name.indexOf("run") >= 0 ||
			    fti.name.indexOf(writer.AppName) >= 0 ||
			    fti.name.indexOf("svr") >= 0 ||
			    fti.name.indexOf("main") >= 0 ||
			    fti.name.indexOf("Makefile") >= 0)
			    {
				fti.up_to_date = false;
			    }
		    }
		CreateFileList(writer.getUserDir(),false);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    @SuppressWarnings("unchecked")
    protected void MarkAllFiles()
    {
	if(null == filesList || null == writer)
	    {
		return;
	    }
	if(null == writer.fileTypeInfoHashtable)
	    {
		return;
	    }
	try
	    {
		System.out.println("MarkAllFiles\n");
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    {
				throw new Exception("No FileTypeInfo for "+filename);
			    }
			fti.up_to_date = false;
			writer.fileTypeInfoHashtable.put(filename,fti);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    protected void MarkLoopFiles(String loopName)
    {
	if(null == filesList || null == writer)
	    {
		return;
	    }
	if(null == writer.fileTypeInfoHashtable)
	    {
		return;
	    }
	try
	    {
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    throw new Exception("No FileTypeInfo for "+filename);
			{
			}
			if( fti.name.indexOf(loopName) >= 0 ||
			    fti.name.indexOf(".nml") >= 0 ||
			    fti.name.indexOf(".cfg") >= 0 ||
			    (fti.list_name.indexOf("main") >= 0 && fti.name.indexOf("Makefile") >= 0))
			    {
				fti.up_to_date = false;
			    }
		    }
		CreateFileList(writer.getUserDir(),false);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    protected void MarkModuleFiles(String moduleName, boolean include_intf)
    {
	if(null == filesList || null == writer)
	    {
		return;
	    }
	if(null == writer.fileTypeInfoHashtable)
	    {
		return;
	    }
	try
	    {
		CreateFileList(writer.getUserDir(),false);
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    {
				throw new Exception("No FileTypeInfo for "+filename);
			    }
			if(fti.type == FileTypeInfo.MAIN_DESIGN_LOG  ||
			   fti.type == FileTypeInfo.MODULE_CONFIGURATION ||
			   fti.type == FileTypeInfo.INTF_MAKEFILE)
			    {
				fti.up_to_date = false;
				filesList.replaceItem(fti.list_name+" (*)",i);
				if(debug_on)
				    {
					System.out.println("Marking "+fti.list_name);
				    }
				continue;
			    }
			if(!include_intf)
			    {
				if(fti.list_name.indexOf("intf") >= 0 ||
				   fti.type == FileTypeInfo.NML_MODULE_MESSAGE_HEADER ||
				   fti.type == FileTypeInfo.NML_MODULE_MESSAGE_CPP ||
				   fti.type == FileTypeInfo.NML_MODULE_MESSAGE_CODEGEN_SCRIPT)
				    {
					continue;
				    }
			    }
			if(null == fti.module_name)
			    {
				continue;
			    }
			if(fti.module_name.equals(moduleName) ||
			   fti.list_name.indexOf(moduleName) >= 0)
			    {
				if(debug_on)
				    {
					System.out.println("Marking "+fti.list_name);
				    }
				filesList.replaceItem(fti.list_name+" (*)",i);
				fti.up_to_date = false;
			    }
		    }
		CreateFileList(writer.getUserDir(),false);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    /*
     * The function textValueChanged is required to implement TextListener. It is
     * called when the text of a TextArea is changed.
     */
    public void textValueChanged(TextEvent evt)
    {
	if(inside_query|| inside_run || shutting_down || inside_alert)
	    {
		if(debug_on)
		    {
			System.out.println("Ignoring event.");
		    }
		return;
	    }
	if(!updating_files)
	    {
		if(evt.getSource() == fileTextArea)
		    {
			synchronized(this)
			    {
				if(null != current_fti)
				    {
					if(!text_file_needs_saving)
					    {
						String title = fileTextLabel.getText();
						title += " (*)";
						fileTextLabel.setText(title);
					    }
					text_file_needs_saving = true;
				    }
			    }
			return;
		    }
	    }
    }

    int   last_hierarchyHorzScrollbar_arg = 0;
    int   last_hierarchyVertScrollbar_arg = 0;

    /*
     *
     * The function adjustmentValueChanged is required to implement adjustmentListener.
     * This function is called when the user moves a scrollbar.
     */
    public void adjustmentValueChanged(AdjustmentEvent evt)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsDesign.adjustmentValueChaned("+evt+")");
		    }
		if(inside_query|| inside_run || shutting_down || inside_alert)
		    {
			if(debug_on)
			    {
				System.out.println("Ignoring event.");
			    }
			return;
		    }

		if(evt.getSource() == hierarchyHorzScrollbar)
		    {
			int arg = evt.getValue();
			int temp = hierarchyHorzScrollbar.getValue();
			if(arg != last_hierarchyHorzScrollbar_arg ||
			   arg != temp)
			    {
				last_hierarchyHorzScrollbar_arg = arg;
				if(temp > 0)
				    {
					hierarchyPanel.scroll_x = temp;
					hierarchyHorzScrollbar.setValue(temp);
					hierarchyPanel.monitored_repaint();
				    }
				if(hierarchyPanel.scroll_x < 0)
				    {
					hierarchyPanel.scroll_x = 0;
				    }
			    }
		    }
		if(evt.getSource() == hierarchyVertScrollbar)
		    {
			int arg = evt.getValue();
			int temp = hierarchyVertScrollbar.getValue();
			if(arg != last_hierarchyVertScrollbar_arg ||
			   arg != temp)
			    {
				last_hierarchyVertScrollbar_arg = arg;
				if(temp > 0)
				    {
					hierarchyVertScrollbar.setValue(temp);
					hierarchyPanel.scroll_y = temp;
					hierarchyPanel.monitored_repaint();
				    }
				if(hierarchyPanel.scroll_y < 0)
				    {
					hierarchyPanel.scroll_y = 0;
				    }
			    }
		    }

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    Thread generateEverything_thread;

    void GenerateEverything()
    {
	if(null == generateEverything_thread)
	    {
		GeneratingEverything = true;
		DisableAllControls();
		if(null != goButton)
		    {
			goButton.setEnabled(true);
		    }
		generateEverything_thread = new Thread(this);
		generateEverything_thread.start();
	    }
    }

    protected boolean UpdateAuxStuff(String aux)
    {
	if(null == aux || null == writer.curModule)
	    {
		return true;
	    }
	BufferInfo bi = (BufferInfo) writer.buffersHashtable.get(aux);
	if(null == bi)
	    {
		return true;
	    }
	if(debug_on)
	    {
		System.out.println("BufferInfo found.");
		System.out.println("bi = "+bi);
		System.out.println("bi.channelsHashtable = "+bi.channelsHashtable);
	    }
	ChannelInfo ci = (ChannelInfo) bi.channelsHashtable.get(writer.curModule.Name);
	if(null == ci)
	    {
		return true;
	    }
	if(debug_on)
	    {
		System.out.println("ChannelInfo found.");
	    }
	auxInputCheckbox.setState(!ci.isWriter);
	//auxOutputCheckbox.setState(ci.isWriter);
	auxUpdateCheckbox.setState(ci.updateEveryCycle);
	return false;
    }


    public void start()
    {
	if(debug_on)
	    {
		System.out.println("rcsdesign starting . . .");
	    }
    }

    public void stop()
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("rcsdesign stopping . . .");
		    }
		// Thread.stop is deprecated and Thread.destroy() is not implemented.
		// But there is no other good way of cleaning up that thread.
		KillGenerateEverythingThread();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    static final protected int DO_NOTHING = 1;
    static final protected int READ_CONFIGURATION = 2;
    static final protected int CREATE_SOURCE = 3;
    static final protected int CHECK_IN_FILES = 4;
    static final protected int UPDATE_FILE=5;
    static final protected int CLEANUP=6;

    protected int run_function = DO_NOTHING;
    protected boolean updating_files = false;

    public void run()
    {
	try
	    {
		inside_run=true;
		goButton.setLabel(" STOP ACTION ");
		monitored_repaint();
		try
		    {
			Thread.sleep(50);
		    }
		catch(Exception e)
		    {
		    }
		synchronized(this)
		    {
			switch(run_function)
			    {
			    case DO_NOTHING:
				break;

			    case CLEANUP:
				cleanup_in_background();
				break;

			    case UPDATE_FILE:
				try
				    {
					if(fileToUpdate != null)
					    {
						updating_files=true;
						FileTypeInfo fti = fileToUpdate;
						fileTextArea.setEditable(false);

						writer.UpdateFile(fti);
						filesList.replaceItem(fti.list_name, filesList.getSelectedIndex());
						if(!fti.file.exists())
						    {
							fileCheckOutCheckbox.setState(true);
							fileTextArea.setEditable(true);
						    }
						else if(fti.file.canWrite())
						    {
							fileTextArea.setEditable(true);
							fileCheckOutCheckbox.setState(true);
						    }
						else
						    {
							fileCheckOutCheckbox.setState(false);
							fileTextArea.setEditable(false);
						    }
						for(int i = 0; i < filesList.getItemCount(); i++)
						    {
							String f = filesList.getItem(i);
							if(f.equals(fti.list_name))
							    {
								filesList.select(i);
								break;
							    }
						    }
						current_fti = fti;
						fti.in_use = false;
						updating_files=false;
					    }
					fileToUpdate=null;
				    }
				catch(Exception e)
				    {
					e.printStackTrace();
				    }
				break;

			    case READ_CONFIGURATION:
				if(debug_on)
				    {
					System.out.println("rcsDesign.run() : read_everything_needed="+read_everything_needed);
				    }
				if(read_everything_needed)
				    {
					ReadEverything();
					int selected_module_index = modulesList.getSelectedIndex();
					if(selected_module_index >=  0)
					    {
						String selected_module_name = modulesList.getSelectedItem();
						if(null == selected_module_name || null == writer.modulesHashtable)
						    {
							modulesList.deselect(selected_module_index);
							writer.curModule = null;
							return;
						    }
						writer.curModule = (ModuleInfo) writer.modulesHashtable.get(selected_module_name);
						if(null == writer.curModule)
						    {
							modulesList.deselect(selected_module_index);
							writer.curModule = null;
							return;
						    }
						if(!selected_module_name.equals(writer.curModule.Name))
						    {
							Alert("Current Module Invalid "+selected_module_name+" != "+writer.curModule.Name);
							modulesList.deselect(selected_module_index);
							writer.curModule = null;
							return;
						    }
					    }
				    }
				break;

			    case CREATE_SOURCE:
				{
				    updating_files=true;
				    fileTextArea.setEditable(false);
				    QueryDialog.cancel = false;
				    GeneratingEverything = true;
				    String checkin_dir = null;
				    if(null != fileCheckinDirectoryField)
					{
					    checkin_dir = fileCheckinDirectoryField.getText();
					}
				    String appdir_string = appdirField.getText();
				    String user_dir_string = userdirField.getText();
				    if(appdir_string != null)
					{
					    File app_dir_file = new File(appdir_string);
					    if(!appdir_string.equals(user_dir_string) && app_dir_file.exists() && app_dir_file.isDirectory() && app_dir_file.canWrite())
						{
						    CreateDirectories(appdir_string, checkin_dir);
						}
					}
				    if(!QueryDialog.cancel)
					{
					    CreateDirectories(user_dir_string,null);
					}
				    if(!QueryDialog.cancel && !appdir_string.equals(user_dir_string))
					{
					    String filecheckindir = null;
					    if(null != fileCheckinDirectoryField)
						{
						    filecheckindir = fileCheckinDirectoryField.getText();
						}
					    CreateSymLinks(user_dir_string, appdir_string,filecheckindir );
					}
				    if(!QueryDialog.cancel)
					{
					    FillDirectories(userdirField.getText());
					}
				    updating_files=false;
				    fileTextArea.setEditable(true);
				}
				break;

			    case CHECK_IN_FILES:
				CheckInAllFiles();
				break;

			    default:
				System.err.println("Invalid run_function = "+run_function);
				break;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	GeneratingEverything = false;
	run_function = DO_NOTHING;
	goButton.setLabel("Create Source");
	EnableAllControls();
	monitored_repaint();
	try
	    {
		Thread.sleep(50);
	    }
	catch(Exception e)
	    {
	    }
	generateEverything_thread = null;
	inside_run=false;
    }


    void CreateSymLinks(String dir1, String dir2,  String cs)
    {
	try
	    {
		if(null == writer)
		    {
			return;
		    }
		if(writer.mswinDevPlat)
		    {
			return;
		    }
		if(null == default_version_control_type)
		    {
			return;
		    }
		if(default_version_control_type.equalsIgnoreCase("NONE"))
		    {
			return;
		    }
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Creating Symbolic Links("+dir1+","+dir2+","+cs+") . . . ";
			loadPanel.bytes_read = 0;
			loadPanel.content_length = modulesList.getItemCount() + 5;
			if(debug_on)
			    {
				System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
			    }
			loadPanel.repaint();
		    }
		if(null == dir1)
		    {
			return;
		    }
		if(dir1.length() == 0)
		    {
			return;
		    }
		File dir1File = null;
		dir1File = new File(dir1);
		if(!dir1File.exists())
		    {
			return;
		    }
		if(!dir1File.isDirectory() || !dir1File.canWrite())
		    {
			System.err.println("Can not create subdirectories under "+dir1File);
			return;
		    }
		if(null == dir2)
		    {
			return;
		    }
		if(dir2.length() == 0)
		    {
			return;
		    }
		File dir2File = null;
		dir2File = new File(dir2);
		if(!dir2File.exists())
		    {
			return;
		    }
		if(!dir2File.isDirectory())
		    {
			System.err.println(dir2File +" is not a directory.");
			return;
		    }
		if(null == cs)
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
			    }
			return;
		    }
		if(cs.length() < 1)
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
			    }
			return;
		    }
		if(null == symLinkCommandField)
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
				System.out.println("symLinkCommandField= null");
			    }
			return;
		    }
		String symLinkCmd = symLinkCommandField.getText();
		if(null == symLinkCmd)
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
				System.out.println("symLinkCmd="+symLinkCmd);
			    }
			return;
		    }
		if(symLinkCmd.length() < 1)
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
				System.out.println("symLinkCmd="+symLinkCmd);
			    }
			return;
		    }

		File dir2csFile = new File(dir2File, cs);
		if(dir2csFile.exists() && dir2csFile.isDirectory())
		    {
			ExecuteInDirectory(dir1File.toString(), symLinkCmd+" "+dir2csFile+" "+cs);
		    }
		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }
		File srcdir1File = new File(dir1File,"src");
		File srcdir2File = new File(dir2File,"src");
		if(!srcdir1File.exists() || !srcdir2File.exists() || !srcdir1File.isDirectory() || !srcdir2File.isDirectory() || !srcdir1File.canWrite())
		    {
			if(debug_on)
			    {
				System.out.println("Exiting CreateSymLinks("+dir1+", "+dir2+",  "+ cs+") early.");
				System.out.println("srcdir1File = "+srcdir1File);
				System.out.println("srcdir2File = "+srcdir2File);
			    }
			return;
		    }
		File srcCsdir1File = new File(srcdir1File,cs);
		File srcCsdir2File = new File(srcdir2File,cs);
		if(srcCsdir2File.exists() && srcCsdir2File.isDirectory() && !srcCsdir1File.exists())
		    {
			ExecuteInDirectory(srcdir1File.toString(), symLinkCmd+" "+srcCsdir2File+" "+cs);
		    }
		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }
		if(srcdir1File.isDirectory() && srcdir1File.canWrite())
		    {
			for(int i = 0; i < modulesList.getItemCount(); i++)
			    {
				String moddir = modulesList.getItem(i);
				File moddir1File = new File(srcdir1File, moddir);
				File moddir2File = new File(srcdir2File, moddir);
				if(!moddir1File.exists() || !moddir2File.exists() || !moddir2File.isDirectory() || !moddir1File.isDirectory())
				    {
					continue;
				    }
				if(!moddir1File.canWrite())
				    {
					continue;
				    }
				File moddir2CsFile = new File(moddir2File,cs);
				File moddir1CsFile = new File(moddir1File,cs);
				if(moddir2CsFile.exists() && moddir2CsFile.isDirectory() && !moddir1CsFile.exists())
				    {
					ExecuteInDirectory(moddir1File.toString(), symLinkCmd+" "+moddir2CsFile+" "+cs);
				    }
				if(null != loadPanel)
				    {
					loadPanel.bytes_read++;
					loadPanel.repaint();
				    }
			    }
			File intfdir1File = new File(srcdir1File, "intf");
			File intfdir2File = new File(srcdir2File, "intf");
			if(intfdir1File.exists() && intfdir2File.exists() && intfdir1File.isDirectory() && intfdir2File.isDirectory() &&
			   intfdir1File.canWrite())
			    {
				File intfcsdir1File = new File(intfdir1File, cs);
				File intfcsdir2File = new File(intfdir2File, cs);
				if(intfcsdir2File.exists() && intfcsdir2File.isDirectory() && ! intfcsdir1File.exists())
				    {
					ExecuteInDirectory(intfdir1File.toString(), symLinkCmd+" "+intfcsdir2File+" "+cs);
				    }
			    }
			if(null != loadPanel)
			    {
				loadPanel.bytes_read++;
				loadPanel.repaint();
			    }

			File maindir1File = new File(srcdir1File, "main");
			File maindir2File = new File(srcdir2File, "main");
			if(maindir1File.exists() && maindir2File.exists() && maindir1File.isDirectory() && maindir2File.isDirectory() &&
			   maindir1File.canWrite())
			    {
				File maincsdir1File = new File(maindir1File, cs);
				File maincsdir2File = new File(maindir2File, cs);
				if(maincsdir2File.exists() && maincsdir2File.isDirectory() && ! maincsdir1File.exists())
				    {
					ExecuteInDirectory(maindir1File.toString(), symLinkCmd+" "+maincsdir2File+" "+cs);
				    }
			    }

			if(null != loadPanel)
			    {
				loadPanel.bytes_read++;
				loadPanel.repaint();
			    }

			File utildir1File = new File(srcdir1File, "util");
			File utildir2File = new File(srcdir2File, "util");
			if(utildir1File.exists() && utildir2File.exists() && utildir1File.isDirectory() && utildir2File.isDirectory() &&
			   utildir1File.canWrite())
			    {
				File utilcsdir1File = new File(utildir1File, cs);
				File utilcsdir2File = new File(utildir2File, cs);
				if(utilcsdir2File.exists() && utilcsdir2File.isDirectory() && ! utilcsdir1File.exists())
				    {
					ExecuteInDirectory(utildir1File.toString(), symLinkCmd+" "+utilcsdir2File+" "+cs);
				    }
			    }
		    }

		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }



    void CreateDirectories(String dir, String cs)
    {
	try
	    {
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Creating Directories(dir) . . . ";
			loadPanel.bytes_read = 0;
			loadPanel.content_length = platList.getItemCount()*5 + modulesList.getItemCount()*2 + 10;
			if(debug_on)
			    {
				System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
			    }
			loadPanel.repaint();
		    }
		if(null == dir)
		    {
			return;
		    }
		if(dir.length() == 0)
		    {
			return;
		    }
		File dirFile = null;
		dirFile = new File(dir);
		if(!dirFile.exists())
		    {
			dirFile.mkdirs();
		    }
		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }
		if(!dirFile.isDirectory() || !dirFile.canWrite())
		    {
			System.err.println("Can not create subdirectories under "+dirFile);
			return;
		    }
		if(singleDir)
		    {
			return;
		    }
		if(null != cs)
		    {
			File maincsdirFile = new File(dirFile,cs);
			if(!maincsdirFile.exists())
			    {
				maincsdirFile.mkdir();
			    }
			if(null != loadPanel)
			    {
				loadPanel.bytes_read++;
				loadPanel.repaint();
			    }
		    }
		File srcdirFile = new File(dirFile,"src");
		if(!srcdirFile.exists())
		    {
			srcdirFile.mkdir();
		    }
		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }
		if(srcdirFile.isDirectory() && srcdirFile.canWrite())
		    {
			for(int i = 0; i < modulesList.getItemCount(); i++)
			    {
				String moddir = modulesList.getItem(i);
				File moddirFile = new File(srcdirFile, moddir);
				if(!moddirFile.exists())
				    {
					moddirFile.mkdir();
				    }
				if(null != cs && moddirFile.isDirectory() && moddirFile.canWrite())
				    {
					File csFile = new File(moddirFile,cs);
					if(!csFile.exists())
					    {
						csFile.mkdir();
					    }
				    }
				if(null != loadPanel)
				    {
					loadPanel.bytes_read+=2;
					loadPanel.repaint();
				    }
			    }
			File intfdirFile = new File(srcdirFile, "intf");
			if(!intfdirFile.exists())
			    {
				intfdirFile.mkdir();
			    }
			if(null != cs && intfdirFile.isDirectory() && intfdirFile.canWrite())
			    {
				File csFile = new File(intfdirFile,cs);
				if(!csFile.exists())
				    {
					csFile.mkdir();
				    }
			    }
			if(null != loadPanel)
			    {
				loadPanel.bytes_read+=2;
				loadPanel.repaint();
			    }
			File maindirFile = new File(srcdirFile, "main");
			if(!maindirFile.exists())
			    {
				maindirFile.mkdir();
			    }
			if(null != cs && maindirFile.isDirectory() && maindirFile.canWrite())
			    {
				File csFile = new File(maindirFile,cs);
				if(!csFile.exists())
				    {
					csFile.mkdir();
				    }
			    }
			if(null != loadPanel)
			    {
				loadPanel.bytes_read+=2;
				loadPanel.repaint();
			    }
			File utildirFile = new File(srcdirFile, "util");
			if(!utildirFile.exists())
			    {
				utildirFile.mkdir();
			    }
			if(null != cs && utildirFile.isDirectory() && utildirFile.canWrite())
			    {
				File csFile = new File(utildirFile,cs);
				if(!csFile.exists())
				    {
					csFile.mkdir();
				    }
			    }
			if(null != loadPanel)
			    {
				loadPanel.bytes_read+=2;
				loadPanel.repaint();
			    }
		    }
		File platdirFile = new File(dirFile,"plat");
		if(!platdirFile.exists())
		    {
			platdirFile.mkdir();
		    }
		if(null != loadPanel)
		    {
			loadPanel.bytes_read++;
			loadPanel.repaint();
		    }
		if(platdirFile.isDirectory() && platdirFile.canWrite())
		    {
			for(int i = 0; i < platList.getItemCount(); i++)
			    {
				String platsubdir = platList.getItem(i);
				File platsubdirFile = new File(platdirFile, platsubdir);
				if(debug_on)
				    {
					System.out.println("PLAT Sub-directory = "+platsubdirFile);
				    }
				if(!platsubdirFile.exists())
				    {
					platsubdirFile.mkdir();
				    }
				if( platsubdirFile.isDirectory() && platsubdirFile.canWrite())
				    {
					File platsrcFile = new File(platsubdirFile,"src");
					if(!platsrcFile.exists())
					    {
						platsrcFile.mkdir();
					    }
					File platincludeFile = new File(platsubdirFile,"include");
					if(!platincludeFile.exists())
					    {
						platincludeFile.mkdir();
					    }
					File platlibFile = new File(platsubdirFile,"lib");
					if(!platlibFile.exists())
					    {
						platlibFile.mkdir();
					    }
					File platbinFile = new File(platsubdirFile,"bin");
					if(!platbinFile.exists())
					    {
						platbinFile.mkdir();
					    }
					if(null != loadPanel)
					    {
						loadPanel.bytes_read+=5;
						loadPanel.repaint();
					    }
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }


    int CountFilesNotUpToDate()
    {
	int count = 0;
	if(null == filesList || null == writer)
	    {
		return 0;
	    }
	if(null ==  writer.fileTypeInfoHashtable)
	    {
		return 0;
	    }
	for(int i = 0; i < filesList.getItemCount(); i++)
	    {
		String filename = getFilesListItem(i);
		if(null == filename)
		    {
			continue;
		    }

		FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
		if(null == fti)
		    {
			continue;
		    }
		if(fti.up_to_date)
		    {
			count++;
		    }
	    }
	if(debug_on)
	    {		
		System.out.println("CountFilesNotUpToDate = "+count+": filesList.getItemCount()="+filesList.getItemCount());
	    }
	return count;
    }

    void FillDirectories(String dir)
    {
	try
	    {
		int files_updated = 0;
		int files_not_up_to_date = 0;
		boolean orig_prompt_replace = overwritePromptCheckbox.getState();
		if(text_file_needs_saving)
		    {
			SaveCurrentTextFile();
		    }
		CreateFileList(dir, false);
		UpdateWriter();
		files_not_up_to_date = CountFilesNotUpToDate();
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Creating Source . . . ";
			loadPanel.bytes_read = 0;
			loadPanel.content_length = filesList.getItemCount() - files_not_up_to_date;
			if(debug_on)
			    {
				System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
			    }
			loadPanel.repaint();
		    }
		writer.quitFillDirectories = false;
		for(int i = 0; i < filesList.getItemCount(); i++)
		    {
			String filename = getFilesListItem(i);
			if(debug_on)
			    {
				System.out.println("filename="+filename+", i="+i);
			    }
			boolean check_in_file = true;
			FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
			if(null == fti)
			    {
				throw new Exception("No FileTypeInfo for "+filename);
			    }
			if(fti.up_to_date)
			    {
				if(debug_on)
				    {
					System.out.println(filename+" already up to date.");
				    }
				continue;
			    }
			if(debug_on)
			    {
				System.out.println("updating "+fti);
			    }
			if(null != loadPanel)
			    {
				loadPanel.URLname = "Creating Source ("+filename+") . . . ";
				loadPanel.bytes_read = files_updated;
				loadPanel.content_length =  filesList.getItemCount() - files_not_up_to_date;
				if(debug_on)
				    {
					System.out.println("URLname = "+loadPanel.URLname+", bytes_read = "+loadPanel.bytes_read+", content_length ="+loadPanel.content_length);
				    }
				loadPanel.repaint();
				try
				    {
					Thread.sleep(10);
				    }
				catch(Exception e)
				    {
				    }
			    }
			files_updated++;
			if(fti.file.exists() && fti.file.canWrite())
			    {
				check_in_file = false;
			    }
			writer.UpdateFile(fti);
			if(null != autoCheckInCheckbox)
			    {
				auto_checkin = autoCheckInCheckbox.getState();
			    }
			if(check_in_file && auto_checkin  )
			    {
				CheckInFile(fti.file);
			    }
			if(writer.quitFillDirectories)
			    {
				break;
			    }
		    }
		if(orig_prompt_replace)
		    {
			overwritePromptCheckbox.setState(true);
		    }
		if(null != loadPanel)
		    {
			loadPanel.URLname = "Application updated at "+currentTime()+". ";
			loadPanel.bytes_read = 1;
			loadPanel.content_length = 1;
			loadPanel.repaint();
		    }
		update_all_files=false;
		CreateFileList(dir,false);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    long config_file_date;
    String currentModuleNameForAddFileTypeInfo = null;
    String currentServerNameForAddFileTypeInfo = null;
    String currentLoopNameForAddFileTypeInfo = null;
    String currentAuxForAddFileTypeInfo = null;
    long module_log_date = 0;

    @SuppressWarnings("unchecked")
    FileTypeInfo AddFileTypeInfoItem(String name, String list_name, File dirFile, int type)
    {
	if(null == writer)
	    {
		return null;
	    }
	if(debug_on)
	    {
		System.out.println("Adding "+list_name+" to the file list. (size = "+filesList.getItemCount()+")");
	    }
	FileTypeInfo fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(list_name);
	if(null == fti)
	    {
		fti = new FileTypeInfo();
		fti.name = name;
		fti.module_name = currentModuleNameForAddFileTypeInfo;
		fti.server_name = currentServerNameForAddFileTypeInfo;
		fti.loop_name = currentLoopNameForAddFileTypeInfo;
		fti.list_name = list_name;
		fti.type = type;
		fti.aux = currentAuxForAddFileTypeInfo;
		if(debug_on)
		    {
			System.out.println("dirFile="+dirFile+", fti.name="+fti.name);
		    }
		fti.file = new File(dirFile, fti.name);
		if(debug_on)
		    {
			System.out.println("fti.file = "+fti.file);
		    }
		fti.generated = fti.file.exists() && list_generated;
		switch(fti.type)
		    {
		    case FileTypeInfo.NML_MODULE_DESIGN_LOG:
		    case FileTypeInfo.MAIN_DESIGN_LOG:
			fti.up_to_date = false;
			break;

		    case FileTypeInfo.NML_MODULE_HEADER:
		    case FileTypeInfo.NML_MODULE_CPP:
			if(module_log_date > 0)
			    {
				fti.up_to_date = fti.file.exists() && (fti.file.lastModified() >= module_log_date);
			    }
			else
			    {
				fti.up_to_date = fti.file.exists();
			    }
			break;

		    case FileTypeInfo.TOP_MAKEFILE:
		    case FileTypeInfo.MAIN_MAKEFILE:
		    case FileTypeInfo.INTF_MAKEFILE:
		    case FileTypeInfo.UTIL_MAKEFILE:
		    case FileTypeInfo.MAIN_LOOP_CPP:
		    case FileTypeInfo.SERVER_CPP:
		    case FileTypeInfo.NML_CONFIGURATION:
			if(config_file_date > 0)
			    {
				fti.up_to_date = fti.file.exists() && (fti.file.lastModified() >= config_file_date);
			    }
			else
			    {
				fti.up_to_date = fti.file.exists();
			    }
			break;

		    case FileTypeInfo.MODULE_CONFIGURATION:
		    default:
			fti.up_to_date = fti.file.exists();
			break;

		    }
		if(update_all_files)
		    {
			fti.up_to_date=false;
		    }
		writer.fileTypeInfoHashtable.put(fti.list_name,fti);
	    }
	if(!fti.up_to_date)
	    {
		filesList.add(fti.list_name+" (*)");
	    }
	else
	    {
		filesList.add(fti.list_name);
	    }
	return fti;
    }

    boolean list_generated = false;

    @SuppressWarnings("unchecked")
    void CreateFileList(String dir, boolean generated)
    {
	try
	    {
		
		HashSet gen_hs = new HashSet();
		list_generated = generated;

		if(debug_on)
		    {
			Thread.dumpStack();
			System.out.println("CreateFileList("+dir+", "+generated+")");
		    }

		CreateBufferList();

		if(null == dir)
		    {
			dir = ".";
		    }
		FileTypeInfo fti = null;

		File dirFile = null;
		dirFile = new File(dir);
		try
		    {
			if(!dirFile.exists())
			    {
				dirFile.mkdirs();
			    }
			else if(!dirFile.isDirectory() || !dirFile.canWrite())
			    {
				return;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }

		if(writer.curModule != null)
		    {
			String subnames[] = subordinatesList.getSelectedItems();
			writer.curModule.children_names.removeAllElements();
			for(int i = 0; i < subnames.length; i++)
			    {
				if(subnames[i] != null)
				    {
					writer.curModule.children_names.addElement(subnames[i]);
				    }
			    }
			writer.curModule.cmdsAvailable.removeAllElements();
			for(int i = 0; i < cmdList.getItemCount(); i++)
			    {
				writer.curModule.cmdsAvailable.addElement( cmdList.getItem(i));
			    }
		    }
		if(null == writer.curmainLoop)
		    {
			String loopname = mainLoopList.getSelectedItem();
			if(null != loopname)
			    {
				writer.curmainLoop = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(loopname);
			    }
		    }
		if(writer.curmainLoop != null)
		    {
			String modules[] = modsInLoopList.getSelectedItems();
			if(debug_on)
			    {
				System.out.println("String modules[] = modsInLoopList.getSelectedItems(); ="+modules);
			    }
			writer.curmainLoop.clearModules();
			for(int i = 0; i < modules.length; i++)
			    {
				if(debug_on)
				    {
					System.out.println("modules["+i+"] = "+modules[i]);
				    }
				writer.curmainLoop.addModule(modules[i]);
			    }
			try
			    {
				writer.curmainLoop.cycle_time = Double.valueOf(cycletimeField.getText()).doubleValue();
				writer.curmainLoop.host = mainLoopHostField.getText();
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
		    }
		for(int i = 0; i < mainLoopList.getItemCount(); i++)
		    {
			String mainloopname = mainLoopList.getItem(i);
			rcsdesignMainLoopInfo loopInfo = (rcsdesignMainLoopInfo) writer.mainloopsHashtable.get(mainloopname);
			if(null == loopInfo)
			    {
				continue;
			    }
			for(int j = 0; j < loopInfo.getModules().size(); j++)
			    {
				String modName = (String) loopInfo.getModules().elementAt(j);
				if(null != modName)
				    {
					ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(modName);
					if(null == modInfo)
					    {
						continue;
					    }
					modInfo.MainLoopName = mainloopname;
					modInfo.cycle_time = loopInfo.cycle_time;
					modInfo.host = loopInfo.host;
				    }
			    }
		    }
		String selectedFile = getFilesListItem(filesList.getSelectedIndex());
		filesList.removeAll();
		String fti_name = null;
		String fti_list_name = null;
		currentModuleNameForAddFileTypeInfo = null;
		currentLoopNameForAddFileTypeInfo = null;
		currentAuxForAddFileTypeInfo = null;
		currentServerNameForAddFileTypeInfo = null;

		module_log_date = 0;

		fti_name = appnameField.getText()+".cfg";
		fti_list_name = fti_name;
		fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.MODULE_CONFIGURATION);
		config_file_date = fti.file.lastModified();

		fti_name = "design.log";
		fti_list_name = fti_name;
		fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.MAIN_DESIGN_LOG);
		config_file_date = fti.file.lastModified();

		fti_name = ".ec";
		fti_list_name = fti_name;
		fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.EC_SCRIPT);


		if(singleDir)
		    {
			
			fti_name = "GNUmakefile";
			fti_list_name = fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.SINGLE_DIR_GNUMAKEFILE);

		    }
		else
		    {
			
			fti_name = "Makefile";
			fti_list_name = fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.TOP_MAKEFILE);

			fti_name = "Makefile.inc";
			fti_list_name = fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.INCLUDE_MAKEFILE);

		    }

		fti_name = appnameField.getText()+".nml.local";
		fti_list_name = fti_name;
		fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.NML_CONFIGURATION);

		if(writer.mswinDevPlat)
		    {
			fti_name = appnameField.getText()+".bat";
		    }
		else
		    {
			fti_name = "run."+appnameField.getText();
		    }
		fti_list_name = fti_name;
		fti = AddFileTypeInfoItem(fti_name, fti_list_name, dirFile, FileTypeInfo.SCRIPT);
		File srcdirFile =null;
		if(singleDir)
		    {
			srcdirFile = dirFile;
		    }
		else
		    {
			srcdirFile =  new File(dirFile,"src");
		    }

		Vector auxBuffersAdded = new Vector();
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			String moddir = modulesList.getItem(i);

			ModuleInfo modInfo = (ModuleInfo) writer.modulesHashtable.get(moddir);

			currentModuleNameForAddFileTypeInfo = moddir;
			currentAuxForAddFileTypeInfo = null;
			File moddirFile = null;
			if(singleDir)
			    {
				moddirFile = dirFile;
			    }
			else
			    {
				moddirFile = new File(srcdirFile, moddir);
			    }
			
			if(!singleDir)
			    {
				fti_name = "design.log";
				fti_list_name = "src"+File.separator+moddir+File.separator+fti_name;
				fti = AddFileTypeInfoItem(fti_name, fti_list_name, moddirFile, FileTypeInfo.NML_MODULE_DESIGN_LOG);
				module_log_date = 0;
				if(fti.file.exists())
				    {
					module_log_date = fti.file.lastModified();
				    }
			    }

			fti_name = moddir+writer.hpp_ext;
			if(singleDir)
			    {
				fti_list_name = fti_name;
			    }
			else
			    {
				fti_list_name = "src"+File.separator+moddir+File.separator+fti_name;
			    }
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, moddirFile, FileTypeInfo.NML_MODULE_HEADER);


			if(singleDir)
			    {
				fti_name = moddir+"_module"+writer.cpp_ext;
				fti_list_name = fti_name;
			    }
			else
			    {
				fti_name = moddir+writer.cpp_ext;
				fti_list_name = "src"+File.separator+moddir+File.separator+fti_name;
			    }
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, moddirFile, FileTypeInfo.NML_MODULE_CPP);

			if(!singleDir)
			    {
				fti_name = "Makefile";
				fti_list_name = "src"+File.separator+moddir+File.separator+fti_name;
				fti = AddFileTypeInfoItem(fti_name, fti_list_name, moddirFile, FileTypeInfo.MODULE_MAKEFILE);
			    }

			File intfdirFile = null;
			
			if(singleDir)
			    {
				intfdirFile = dirFile;
			    }
			else
			    {
				intfdirFile = new File(srcdirFile, "intf");
			    }
			fti_name = moddir+"n"+writer.hpp_ext;
			if(singleDir)
			    {
				fti_list_name = fti_name;
			    }
			else
			    {
				fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
			    }
			if(null != modInfo && null != modInfo.cmdsTypeFile)
			    {
				fti_list_name = modInfo.cmdsTypeFile;
				int sep_index = fti_list_name.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fti_name = fti_list_name.substring(sep_index+1);
				    }
				else
				    {
					fti_name = fti_list_name;
				    }
			    }
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_HEADER);
	
			if(null != modInfo && 
			   null != modInfo.statsTypeFile && 
			   !modInfo.statsTypeFile.equals(modInfo.cmdsTypeFile))
			    {
				fti_list_name = modInfo.statsTypeFile;
				int sep_index = fti_list_name.lastIndexOf(File.separator);
				if(sep_index > 0)
				    {
					fti_name = fti_list_name.substring(sep_index+1);
				    }
				else
				    {
					fti_name = fti_list_name;
				    }
				fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_HEADER);

			    }

			if(!singleDir)
			    {
				fti_name = moddir+"n"+writer.cpp_ext;
				fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
				if(null != modInfo && null != modInfo.cmdsTypeFile)
				    {
					fti_list_name = modInfo.cmdsTypeFile;
					int period_index = fti_list_name.lastIndexOf('.');
					if(period_index >0)
					    {
						fti_list_name = fti_list_name.substring(0,period_index)+writer.cpp_ext;
					    }
					int sep_index = fti_list_name.lastIndexOf(File.separator);
					if(sep_index > 0)
					    {
						fti_name = fti_list_name.substring(sep_index+1);
					    }
					else
					    {
						fti_name = fti_list_name;
					    }
				    }
				fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_CPP);

				if(null != modInfo && 
				   null != modInfo.statsTypeFile && 
				   !modInfo.statsTypeFile.equals(modInfo.cmdsTypeFile))
				    {
					fti_list_name = modInfo.statsTypeFile;
					int period_index = fti_list_name.lastIndexOf('.');
					if(period_index >0)
					    {
						fti_list_name = fti_list_name.substring(0,period_index)+writer.cpp_ext;
					    }
					int sep_index = fti_list_name.lastIndexOf(File.separator);
					if(sep_index > 0)
					    {
						fti_name = fti_list_name.substring(sep_index+1);
					    }
					else
					    {
						fti_name = fti_list_name;
					    }
					fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_CPP);
				    }

				fti_name = moddir+"n.gen";
				fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;

				if(null != modInfo && null != modInfo.cmdsTypeFile)
				    {
					fti_list_name = modInfo.cmdsTypeFile;
					int period_index = fti_list_name.lastIndexOf('.');
					if(period_index >0)
					    {
						fti_list_name = fti_list_name.substring(0,period_index)+".gen";
					    }
					int sep_index = fti_list_name.lastIndexOf(File.separator);
					if(sep_index > 0)
					    {
						fti_name = fti_list_name.substring(sep_index+1);
					    }
					else
					    {
						fti_name = fti_list_name;
					    }
				    }

				if(gen_hs.contains(fti_name))
				    {
					continue;
				    }
				else
				    {
					gen_hs.add(fti_name);
				    }
				fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_CODEGEN_SCRIPT);
				if(null != modInfo && 
				   null != modInfo.statsTypeFile && 
				   !modInfo.statsTypeFile.equals(modInfo.cmdsTypeFile))
				    {
					fti_list_name = modInfo.statsTypeFile;
					int period_index = fti_list_name.lastIndexOf('.');
					if(period_index >0)
					    {
						fti_list_name = fti_list_name.substring(0,period_index)+".gen";
					    }
					int sep_index = fti_list_name.lastIndexOf(File.separator);
					if(sep_index > 0)
					    {
						fti_name = fti_list_name.substring(sep_index+1);
					    }
					else
					    {
						fti_name = fti_list_name;
					    }
					if(gen_hs.contains(fti_name))
					    {
						continue;
					    }
					else
					    {
						gen_hs.add(fti_name);
					    }
					fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MODULE_MESSAGE_CODEGEN_SCRIPT);
				    }
			    }

			ModuleInfo mi = (ModuleInfo) writer.modulesHashtable.get(moddir);
			if(null != mi)
			    {
				if(null != mi.AuxOutputNames)
				    {
					for(int j = 0; j < mi.AuxOutputNames.size(); j++)
					    {
						String aux = (String) mi.AuxOutputNames.elementAt(j);
						boolean aux_already_added = false;
						for(int aux_i = 0; aux_i < auxBuffersAdded.size(); aux_i++)
						    {
							String prev_aux = (String) auxBuffersAdded.elementAt(aux_i);
							if(prev_aux.equals(aux))
							    {
								aux_already_added = true;
								break;
							    }
						    }
						if(aux_already_added)
						    {
							continue;
						    }
						currentAuxForAddFileTypeInfo = aux;
						auxBuffersAdded.addElement(aux);
						fti_name = aux+"n"+writer.hpp_ext;
						if(singleDir)
						    {
							fti_list_name = fti_name;
						    }
						else
						    {
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
						    }
						fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_HEADER);
						if(!singleDir)
						    {
							fti_name = aux+"n"+writer.cpp_ext;
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
							fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_CPP);

							fti_name = aux+"n.gen";
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
							if(gen_hs.contains(fti_name))
							    {
								continue;
							    }
							else
							    {
								gen_hs.add(fti_name);
							    }
							fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_CODEGEN_SCRIPT);
						    }
					    }
				    }
				if(null != mi.AuxInputNames)
				    {
					for(int j = 0; j < mi.AuxInputNames.size(); j++)
					    {
						String aux = (String) mi.AuxInputNames.elementAt(j);
						boolean aux_already_added = false;
						for(int aux_i = 0; aux_i < auxBuffersAdded.size(); aux_i++)
						    {
							String prev_aux = (String) auxBuffersAdded.elementAt(aux_i);
							if(prev_aux.equals(aux))
							    {
								aux_already_added = true;
								break;
							    }
						    }
						if(aux_already_added)
						    {
							continue;
						    }
						currentAuxForAddFileTypeInfo = aux;
						auxBuffersAdded.addElement(aux);
						fti_name = aux+"n"+writer.hpp_ext;
						if(singleDir)
						    {
							fti_list_name = fti_name;
						    }
						else
						    {
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
						    }
						fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_HEADER);

						if(!singleDir)
						    {
							fti_name = aux+"n"+writer.cpp_ext;
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
							fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_CPP);
							
							fti_name = aux+"n.gen";
							fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
							if(gen_hs.contains(fti_name))
							    {
								continue;
							    }
							else
							    {
								gen_hs.add(fti_name);
							    }
						    }
						fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_AUX_MESSAGE_CODEGEN_SCRIPT);
					    }
				    }
			    }
		    }


		currentModuleNameForAddFileTypeInfo = null;
		currentAuxForAddFileTypeInfo = null;

		module_log_date = 0;

		File intfdirFile = null;
		if(singleDir)
		    {
			intfdirFile = dirFile;
		    }
		else
		    {
			intfdirFile = new File(srcdirFile, "intf");
		    }

		if(!singleDir)
		    {
			for(int i =0; i < ModuleInfo.headerFiles.size(); i++)
			    {
				String header = (String) ModuleInfo.headerFiles.elementAt(i);
				File hFile = new File(intfdirFile,header);
				if(hFile.exists())
				    {
					String gen_base = header;
					int pindex = gen_base.indexOf('.');
					if(pindex > 0)
					    {
						gen_base = gen_base.substring(0,pindex);
					    }
					String gen_file = gen_base+".gen";
					fti_name = gen_file;
					if(gen_hs.contains(fti_name))
					    {
						continue;
					    }
					else
					    {
						gen_hs.add(fti_name);
					    }
					fti_list_name = "src"+File.separator+"intf"+File.separator+fti_name;
					fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.NML_MISC_MESSAGE_CODEGEN_SCRIPT);
				    }
			    }				
			fti_name = "Makefile";
			fti_list_name =  "src"+File.separator+"intf"+File.separator+fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, intfdirFile, FileTypeInfo.INTF_MAKEFILE);

		    }

		File maindirFile = null;
		if(singleDir)
		    {
			maindirFile = dirFile;
		    }
		else
		    {
			maindirFile = new File(srcdirFile, "main");
		    }
		for(int i = 0; i < mainLoopList.getItemCount(); i++)
		    {
			String loop_name = mainLoopList.getItem(i);
			currentLoopNameForAddFileTypeInfo = loop_name;
			fti_name = loop_name+"main"+writer.cpp_ext;
			if(singleDir)
			    {
				fti_list_name = fti_name;
			    }
			else
			    {
				fti_list_name = "src"+File.separator+"main"+File.separator+fti_name;
			    }
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, maindirFile, FileTypeInfo.MAIN_LOOP_CPP);
		    }

		currentLoopNameForAddFileTypeInfo = null;

		for(int i = 0; i < serverList.getItemCount(); i++)
		    {
			String server_name = serverList.getItem(i);
			currentServerNameForAddFileTypeInfo = server_name;
			fti_name = server_name+writer.cpp_ext;
			if(singleDir)
			    {
				fti_list_name = fti_name;
			    }
			else
			    {
				fti_list_name = "src"+File.separator+"main"+File.separator+fti_name;
			    }
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, maindirFile, FileTypeInfo.SERVER_CPP);
		    }

		if(!singleDir)
		    {
			currentServerNameForAddFileTypeInfo = null;
			fti_name = "Makefile";
			fti_list_name = "src"+File.separator+"main"+File.separator+fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, maindirFile, FileTypeInfo.MAIN_MAKEFILE);

			File utildirFile = new File(srcdirFile, "util");
			fti_name = "Makefile";
			fti_list_name = "src"+File.separator+"util"+File.separator+fti_name;
			fti = AddFileTypeInfoItem(fti_name, fti_list_name, utildirFile, FileTypeInfo.UTIL_MAKEFILE);
		    }

		if(selectedFile != null)
		    {
			for(int k = 0; k < filesList.getItemCount(); k++)
			    {
				String filename = getFilesListItem(k);
				if(selectedFile.equals(filename))
				    {
					filesList.select(k);
					fti = (FileTypeInfo) writer.fileTypeInfoHashtable.get(filename);
					ReadFileIntoTextArea(fti.file.toString(), FileTypeInfo.typeToString(fti.type),fti);
					break;
				    }
			    }
		    }
		file_list_needs_to_be_modified = false;

	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		Alert("RCS Application could not be created.\nException occured.\n"+e.toString());
	    }
    }

    // Place additional applet clean up code here.  destroy() is called when
    // when you applet is terminating and being unloaded.
    //-------------------------------------------------------------------------
    public void destroy()
    {
	rcsdesign_count--;
    }

    // rcsdesign Paint Handler
    //--------------------------------------------------------------------------
    public void paint(Graphics g)
    {
	if(repaint_count > 0)
	    {
		repaint_count --;
	    }
	super.paint(g);
    }


    protected void LoadListsFromCurModule()
    {
	String new_module_name = writer.curModule.Name;

	if(null != subordinatesList)
	    {
		subordinatesList.removeAll();
		for(int i = 0; i < modulesList.getItemCount(); i++)
		    {
			subordinatesList.add(modulesList.getItem(i));
			if(null != writer.curModule)
			    {
				if(null != writer.curModule.children_names)
				    {
                                        for(int j = 0; j < writer.curModule.children_names.size(); j++)
					    {
						if(modulesList.getItem(i).equals(((String) writer.curModule.children_names.elementAt(j))))
						    {
							subordinatesList.select(i);
							break;
						    }
					    }
				    }
			    }
		    }
	    }
	if(null != cmdList)
	    {
		cmdList.removeAll();
		if(null != writer.curModule.cmdsAvailable)
		    {
			storeCmdsLoop:                                    for(int j = 0; j < writer.curModule.cmdsAvailable.size(); j++)
			    {
				String cmdName = (String) writer.curModule.cmdsAvailable.elementAt(j);
				if(null == cmdName)
				    {
					continue;
				    }
				int eqIndex = cmdName.indexOf('=');
				if(eqIndex > 0)
				    {
					cmdName = cmdName.substring(0,eqIndex);
				    }
				if(debug_on)
				    {
					System.out.println("Checking "+cmdName+" back to list.");
				    }
				for(int k = 0; k < cmdList.getItemCount(); k++)
				    {
					String cmdFromList = cmdList.getItem(k);
					if(debug_on)
					    {
						System.out.println("Comparing "+cmdName+" and "+cmdFromList+".");
					    }
					if(cmdFromList.equals(cmdName))
					    {
						continue storeCmdsLoop;
					    }
				    }
				if(debug_on)
				    {
					System.out.println("Adding "+cmdName+" back to list.");
				    }
				cmdList.add(cmdName);
			    }
		    }
	    }
	if(null != auxList)
	    {
		auxList.removeAll();
		if(null != writer.curModule.AuxInputNames)
		    {
			for(int i = 0; i < writer.curModule.AuxInputNames.size(); i++)
			    {
				String name = (String) writer.curModule.AuxInputNames.elementAt(i);
				auxList.add(name);
			    }
		    }
		if(null != writer.curModule.AuxOutputNames)
		    {
			for(int i = 0; i < writer.curModule.AuxOutputNames.size(); i++)
			    {
				String name = (String) writer.curModule.AuxOutputNames.elementAt(i);
				auxList.add(name);
			    }
		    }
		if(auxList.getItemCount() > 0)
		    {
			auxList.select(0);
			String aux = auxList.getSelectedItem();
			if(null != aux)
			    {
				UpdateAuxStuff(aux);
			    }
			auxInputCheckbox.setEnabled(true);
			auxUpdateCheckbox.setEnabled(true);
		    }
	    }
    }
}
