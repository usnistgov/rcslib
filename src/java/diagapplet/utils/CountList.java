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

package diagapplet.utils;

import java.awt.*;
import java.applet.*;
import java.awt.event.*;

/**
 * AWT List object that counts events.
 * @author Will Shackleford
 */
public class CountList extends List implements ItemListener, CountListInterface
{
    public int count = 0;
    public static StandAloneApplet parent_applet = null;

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613896L;

    
    public CountList()
    {
	super();
    }

    public CountList(int  rows, boolean  multipleSelections)
    {
	super(rows,multipleSelections);
	addItemListener(this);
    }

    public void itemStateChanged(ItemEvent  event)
    {
	count++;
//	try
//	    {
//		if(null != parent_applet)
//		    {
//			if(!parent_applet.m_fStandAlone)
//			    {
//				AppletContext context = parent_applet.getAppletContext();
//				if(null != context)
//				    {
//					int selected_index = getSelectedIndex();
//					if(null != context && -1 != selected_index)
//					    {
//						System.out.println("Setting status to "+getSelectedItem());
//						context.showStatus(getSelectedItem());
//					    }
//				    }
//			    }
//		    }
//	    }
//	catch(Exception e)
//	    {
//		e.printStackTrace();
//	    }
    }
}
