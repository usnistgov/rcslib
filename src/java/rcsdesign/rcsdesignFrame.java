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

//******************************************************************************
// rcsdesignFrame.java:
//
//******************************************************************************
package rcsdesign;

import java.awt.*;
import java.awt.event.*;
import diagapplet.utils.StandAloneApplet;

//==============================================================================
// STANDALONE APPLICATION SUPPORT
//      This frame class acts as a top-level window in which the applet appears
// when it's run as a standalone application.
//==============================================================================
class rcsdesignFrame extends Frame implements WindowListener
{

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613924L;


    public static boolean debug_on = false;
    StandAloneApplet  innerApplet = null;
    Dimension prevSize = null;
    static int rcsDesign_frames = 0;
    static public boolean in_an_applet = false;
    InnerAppletRepainter repainter = null;
    public boolean ready_for_resizing = false;
    public static boolean force_it=false;
    public static int MIN_WIDTH=120;
    public static int MIN_HEIGHT=120;
    public static int MAX_WIDTH=920;
    public static int MAX_HEIGHT=920;
    

    // rcsdesignFrame constructor
    //--------------------------------------------------------------------------
    public rcsdesignFrame(String str)
    {
	super (str);
	prevSize = getSize();
	rcsDesign_frames++;
	addWindowListener(this);
    }



    public  void resizeInnerApplet()
    {
	return;
    }
 
    /*
     * The functions windowOpened, windowClosing, windowClosed, windowActivated,
     * windowDeactivated, windowIconified, and windowDeiconified are needed
     * to implement WindowListener and basically replace handleEvent from JDK 1.0.x
     *
     */

    public void windowOpened(WindowEvent evt)
    {
	resizeInnerApplet();
    }

    public void windowClosing(WindowEvent evt)
    {
	try
	    {
		if(null != innerApplet)
		    {
			innerApplet.startShutdown();
		    }
		else
		    {
			dispose();
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void windowClosed(WindowEvent evt)
    {
	try
	    {
		rcsDesign_frames--;
		if(rcsDesign_frames <= 0 && !in_an_applet)
		    {
			System.exit(0);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void windowIconified(WindowEvent evt)
    {
    }

    public void windowDeiconified(WindowEvent evt)
    {
	resizeInnerApplet();
    }

    public void windowActivated(WindowEvent evt)
    {
	resizeInnerApplet();
    }

    public void windowDeactivated(WindowEvent evt)
    {
	resizeInnerApplet();
    }
}
