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
// CodeGenFrame.java:
//
//******************************************************************************
package diagapplet.CodeGen;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;

//==============================================================================
// STANDALONE APPLICATION SUPPORT
//      This frame class acts as a top-level window in which the applet appears
// when it's run as a standalone application.
//==============================================================================
class CodeGenFrame extends Frame implements WindowListener
{
    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613916L;

    static public int codegen_frames = 0;
    static public boolean in_an_applet = false;
//    Container innerApplet = null;


    // CodeGenFrame constructor
    //--------------------------------------------------------------------------
    public CodeGenFrame(String str)
    {
	super (str);
	addWindowListener(this);
	codegen_frames++;
	prevSize = getSize();
    }

    boolean inside_manual_resize = false;
    static public boolean debug_on = false;
    Dimension prevSize=null;

    public void manual_resize(int new_width, int new_height)
    {
	inside_manual_resize = true;
	setSize(new_width, new_height);
	int tries = 0;
	if(debug_on)
	    {
		System.out.println("CodeGenFrame resizing from "+prevSize.width+"x"+prevSize.height+" to "+new_width+"x"+new_height);
	    }
	Dimension d = getSize();
	while(tries < 20 &&
	      d.width != new_width && d.height != new_height)
	    {
		try
		    {
			Thread.sleep(50);
			d = getSize();
			tries++;
			setSize(new_width, new_height);
			if(debug_on)
			    {
				System.out.println("d = CodeGenFrame.size() = "+d);
			    }
		    }
		catch(Exception e)
		    {
		    }
	    }
	prevSize = new Dimension(new_width, new_height);
	inside_manual_resize = false;
    }


    /*
     * The functions windowOpened, windowClosing, windowClosed, windowActivated,
     * windowDeactivated, windowIconified, and windowDeiconified are needed
     * to implement WindowListener and basically replace handleEvent from JDK 1.0.x
     *
     */

    public void windowOpened(WindowEvent evt)
    {
    }

    public void windowClosing(WindowEvent evt)
    {
//	try
//	    {
//		if(null != innerApplet)
//		    {
//			innerApplet.stop();
//			innerApplet = null;
//			removeAll();
//		    }
//	    }
//	catch(Exception e)
//	    {
//		e.printStackTrace();
//	    }
	try
	    {
		dispose();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void windowClosed(WindowEvent evt)
    {
	codegen_frames--;
	if(codegen_frames <= 0 && !in_an_applet)
	    {
		System.exit(0);
	    }
    }

    public void windowIconified(WindowEvent evt)
    {
    }

    public void windowDeiconified(WindowEvent evt)
    {
    }

    public void windowActivated(WindowEvent evt)
    {
    }

    public void windowDeactivated(WindowEvent evt)
    {
    }
}
