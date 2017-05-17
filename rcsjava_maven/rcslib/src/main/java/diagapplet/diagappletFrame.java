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

package diagapplet;

//******************************************************************************
// diagappletFrame.java:
//
//******************************************************************************
import java.awt.*;
import java.awt.event.*;
import diagapplet.utils.StandAloneApplet;

//==============================================================================
// STANDALONE APPLICATION SUPPORT
//      This frame class acts as a top-level window in which the applet appears
// when it's run as a standalone application.
//==============================================================================
class diagappletFrame extends Frame implements WindowListener, ComponentListener
{


    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613909L;

    // diagappletFrame constructor
    //--------------------------------------------------------------------------
    volatile StandAloneApplet      innerApplet = null;
    volatile Dimension prevSize = null;
    int diagapplet_frames = 0;
    public boolean in_an_applet = false;
    public volatile boolean force_it=false;
    public volatile boolean ready_for_resize=false;

    public diagappletFrame(String str)
    {
	super (str);
	setResizable(true);
	prevSize = getSize();
	addWindowListener(this);
	addComponentListener(this);
	diagapplet_frames++;
    }

    public void RecheckSize()
    {
	prevSize = getSize();
    }

    public void resizeInnerApplet()
    {
	Dimension d = getSize();
	int tries = 0;
	if(!ready_for_resize && !force_it)
	    {
		if(debug_on)
		    {
			System.out.println("diagappletFrame.resizeInnerApplet(): !ready_for_resize");
		    }
		return;
	    }
	try
	    {
		while(d.width < 1 || d.height < 1)
		    {
			if(tries > 20)
			    {
				break;
			    }
			try
			    {
				Thread.sleep(20);
			    }
			catch(Exception e)
			    {
				e.printStackTrace();
			    }
			d = getSize();
			tries++;
		    }
		int new_width = d.width;
		int new_height = d.height;
		if(new_width < 640 || new_height < 460)
		    {
			if(new_width < 640)
			    {
				new_width = 640;
			    }
			if(new_height < 460)
			    {
				new_height = 460;
			    }
			setSize(new_width,new_height);
		    }
		if((Math.abs(prevSize.width - new_width) > 5 ||
		    Math.abs(prevSize.height - new_height) > 5) ||
		   force_it)
		    {
			force_it=false;
			if(null != innerApplet)
			    {
				innerApplet.setVisible(false);
			    }
			invalidate();
			if(debug_on)
			    {
				System.out.println("DiagappletFrame resizing to "+new_width+"X"+new_height);
			    }
			removeAll();
			Insets is = getInsets();
			if(is.right < 10)
			    {
				is.right = 10;
			    }
			if(is.left < 10)
			    {
				is.left = 10;
			    }
			if(is.top < 10)
			    {
				is.top = 10;
			    }
			if(is.bottom < 10)
			    {
				is.bottom = 10;
			    }
//			if(rcs.utils.BrowserInfo.IsNetscapeFourOrLater())
//			    {
//				is.bottom += 30;
//			    }
			if(null != innerApplet)
			    {
				innerApplet.manual_resize(new_width - is.right - is.left,
							  new_height - is.top - is.bottom);
				add("Center",innerApplet);
			    }
			validate();
			if(null != innerApplet)
			    {
				innerApplet.setVisible(true);
				innerApplet.repaint_count = 10;
				if(null != innerApplet.main_applet_thread)
				    {
					innerApplet.main_applet_thread.interrupt();
					//innerApplet.main_applet_thread.resume();
				    }
			    }
			repaint();
		    }
		prevSize = new Dimension(new_width, new_height);
		if(null != innerApplet)
		    {
			innerApplet.requestFocus();
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		System.err.println("d = "+d);
		System.err.println("prevSize = "+prevSize);
	    }
    }

    volatile boolean inside_manual_resize = false;
    static public boolean debug_on = false;
    public void manual_resize(int new_width, int new_height)
    {
	inside_manual_resize = true;
	setSize(new_width, new_height);
	int tries = 0;
	if(debug_on)
	    {
		System.out.println("diagappletFrame resizing from "+prevSize.width+"x"+prevSize.height+" to "+new_width+"x"+new_height);
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
				System.out.println("d = rcsdesignFrame.size() = "+d);
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
	resizeInnerApplet();
    }

    public void windowClosing(WindowEvent evt)
    {
	try
	    {
//		if(null != innerApplet)
//		    {
//			innerApplet.stop();
//			innerApplet = null;
//			removeAll();
//		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
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
	try
	    {
		if(debug_on)
		    {
			System.out.println("diagappletFrame.windowClosed();");
		    }
		diagapplet_frames--;
		if(diagapplet_frames <= 0 && !in_an_applet)
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
    }

    public void componentResized(ComponentEvent evt)
    {
	resizeInnerApplet();
    }

    public void componentShown(ComponentEvent evt)
    {
	resizeInnerApplet();
    }

    public void componentHidden(ComponentEvent evt)
    {
    }

    public void componentMoved(ComponentEvent evt)
    {
    }
}
