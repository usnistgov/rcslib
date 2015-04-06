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

import diagapplet.CodeGen.*;
import diagapplet.utils.StandAloneApplet;
import java.awt.Button;
import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.Choice;
import java.awt.Frame;
import java.awt.Label;
import java.awt.TextArea;
import java.awt.TextField;
import java.io.File;
import rcs.utils.URL_and_FileLoader;

/**
 * Graphical base class for RCS Design tool.
 * 
 * @author Will Shackleford
 */
public class rcsDesignGui extends StandAloneApplet
{

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613921L;

    public String currentLoadedFile = "";
    public Label filesListLabel = null;
    public Label fileTextLabel = null;
    public java.awt.List filesList = null;
    public TextArea fileTextArea = null;
    public CodeGen codeGenerationApplet = null;
    public CheckboxGroup overwriteGroup = null;
    public Checkbox overwriteAlwaysCheckbox = null;
    public Checkbox overwritePromptCheckbox = null;
    public Checkbox overwriteNeverCheckbox = null;
    static public boolean debug_on = false;
    public Checkbox makeBackupsCheckbox = null;
    public Checkbox singleDirCheckbox = null;
    public Choice screenChoice = null;
    public boolean inside_alert = false;
    public boolean inside_query = false;
    public boolean inside_run = false;
    public String diag_cmd =null;
    public Label libsLabel;
    public java.awt.List libsList;
    public Label addLibLabel;
    public TextField addLibField;
    public Button delLibButton;
    public Label includesLabel;
    public java.awt.List includesList;
    public Label addIncludeLabel;
    public TextField addIncludeField;
    public Button delIncludeButton;

    public Label mainLoopLabel = null;
    public java.awt.List mainLoopList = null;
    public Label addmainLoopLabel = null;
    public TextField addmainLoopField = null;
    public Button delmainLoopButton = null;
    public Label modsInLoopLabel = null;
    public java.awt.List modsInLoopList = null;
    public Label mainLoopHostLabel = null;
    public TextField mainLoopHostField = null;
    public Label cycletimeLabel = null;
    public TextField cycletimeField = null;

    public Label serverLabel = null;
    public java.awt.List serverList = null;
    public Label addServerLabel = null;
    public TextField addServerField = null;
    public Button delServerButton = null;
    public Label bufsInServerLabel = null;
    public java.awt.List bufsInServerList = null;
    public Label serverHostLabel = null;
    public TextField serverHostField = null;



    public Label javaCommandLabel = null;
    public TextField javaCommandField = null;
    public Label javaSetupLabel = null;
    public TextArea javaSetupArea = null;
    public Checkbox useJavaInScriptsCheckbox = null;

    public Label appnameLabel = null;
    public TextField appnameField = null;
    public Label appdirLabel = null;
    public TextField appdirField = null;
    public Label userdirLabel = null;
    public TextField userdirField = null;
    public Label rcslibdirLabel = null;
    public TextField rcslibdirField = null;
    public Label platLabel = null;
    public java.awt.List platList = null;
    public Label addPlatLabel = null;
    public TextField addPlatField = null;
    public Button delPlatButton = null;
    public Label overwriteLabel = null;
    public Checkbox useMergerCheckbox = null;
    public Button  removeBackupsButton = null;
    public Label devPlatTypeLabel = null;
    public CheckboxGroup devPlatTypeGroup = null;
    public Checkbox unixCheckbox = null;
    public Checkbox mswinCheckbox = null;
    public Label cppExtLabel = null;
    public TextField cppExtField = null;
    public Label hppExtLabel = null;
    public TextField hppExtField = null;
    public Label objExtLabel = null;
    public TextField objExtField = null;
    public Label makeCommandLabel = null;
    public TextField makeCommandField = null;
    public Label runCommandLabel = null;
    public TextField runCommandField = null;
    public Label terminalCommandLabel = null;
    public TextField terminalCommandField = null;
    public Label diagCommandLabel = null;
    public TextField diagCommandField = null;
    public Label browserCommandLabel = null;
    public TextField browserCommandField = null;
    public Label fileCheckinTypeLabel = null;
    public Choice fileCheckinTypeChoice = null;
    public Label fileCheckinDirectoryLabel = null;
    public TextField fileCheckinDirectoryField = null;
    public Label checkOutCommandLabel = null;
    public TextField checkOutCommandField = null;
    public Label checkInCommandLabel = null;
    public TextField checkInCommandField = null;
    public Label symLinkCommandLabel = null;
    public TextField symLinkCommandField = null;
    public Button checkInEverythingButton = null;
    public Checkbox autoCheckInCheckbox = null;
    public Checkbox autoCheckOutCheckbox = null;
    public Checkbox fileCheckOutCheckbox = null;
    public Label fileAsterixLabel = null;

    public diagapplet.utils.CountList modulesList = null;
    public Label addModuleLabel = null;
    public TextField addModuleField = null;

    public FileTypeInfo current_fti = null;
    public boolean text_file_needs_saving = false;
    public boolean list_modules_by_number=false;
    public String default_version_control_type = "NONE";

    public String default_version_control_directory = null;
    public String default_version_control_checkout_command = null;
    public String default_version_control_checkin_command = null;
    public String default_version_control_symlink_command = "ln -s ";

    public boolean auto_checkin = false;
    public boolean auto_checkout = false;

    public Frame parentFrame = null;

    public static void printThreadInfo()
    {
	try
	    {
		Thread tarray[] = new Thread[100];
		int tarray_length = Thread.enumerate(tarray);
		for(int i = 0; i< tarray_length; i++)
		    {
			System.out.println("Thread :"+i+" "+tarray[i]+" alive:"+tarray[i].isAlive()+" priority:"+tarray[i].getPriority());
			//try
			// {
			//   if(!tarray[i].isAlive())
			//     {
			//        System.out.println("frames:"+tarray[i].countStackFrames());
			//    }
			//}
			//catch(Exception e)
			// {
			//   e.printStackTrace();
			// }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public QueryDialog Query(String str)
    {
	try
	    {
		inside_query=true;
		if(debug_on)
		    {
			System.out.println("Query:"+str);
			Thread.dumpStack();
			printThreadInfo();
		    }
		if(null == parentFrame)
		    {
			try
			    {
				parentFrame = (Frame) getParent();
			    }
			catch(ClassCastException e)
			    {
				parentFrame = new Frame();
			    }
		    }
		QueryDialog qd = new QueryDialog(parentFrame,"RCS-Design Query",str);
		qd.setVisible(true);
		if(debug_on)
		    {
			System.out.println("Waiting for QueryDialog . . .");
		    }
		while(!qd.done)
		    {
			try
			    {
				Thread.sleep(300);
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
			if(debug_on)
			    {
				System.out.print(" .");
			    }
		    }
		inside_query=false;
		return qd;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	inside_query=false;
	return null;
    }



    public void Alert(String str)
    {
	inside_alert=true;
	if(debug_on)
	    {
		Thread.dumpStack();
	    }
	synchronized(this)
	    {
		if(AlertDialog.count > 3)
		    {
			inside_alert=false;
			return;
		    }
		if(debug_on)
		    {
			System.out.println("Alert:"+str);
		    }
		try
		    {
			Frame f = new Frame();
			AlertDialog ad = new AlertDialog(f,"RCS-Design Notice",str);
			ad.setVisible(true);
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		inside_alert=false;
	    }
	return;
    }

    public void ReadFileIntoTextArea(String s, String label, FileTypeInfo fti)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("ReadFileIntoTextArea(String s="+s+", String label="+label+", FileTypeInfo fti="+fti+") called.");
		    }
		if(null != fti)
		    {
			current_fti = fti;
		    }
		URL_and_FileLoader loader = new URL_and_FileLoader(s);
		if(null == loader)
		    {
			return;
		    }
		String line = loader.readLine();
		if(null == line)
		    {
			return;
		    }
		currentLoadedFile = s;
		StringBuffer tempBuffer = new StringBuffer();
		while(line != null)
		    {
			tempBuffer.append(line+"\n");
			line = loader.readLine();
		    }
		fileTextArea.setText(tempBuffer.toString());
		fileTextLabel.setText(s+" "+label);
		loader.close();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    // Always override this method
    public void CheckOutFile(File f)
    {
	return;
    }

    // Always override this method
    public void CheckInFile(File f)
    {
	return;
    }

}
