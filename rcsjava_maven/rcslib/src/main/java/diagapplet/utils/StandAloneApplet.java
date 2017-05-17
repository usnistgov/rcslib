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

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;




/**
 * Wrapper that allows a class to be run as an applet or standalone outside a browser.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class StandAloneApplet extends Container implements ComponentListener
{

    public void init() {
        
    }
    
    public void start() {
        
    }
    
    public void stop() {
        
    }
    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613901L;

    // STANDALONE APPLICATION SUPPORT:
    //            m_fStandAlone will be set to true if applet is run standalone
    //--------------------------------------------------------------------------
    volatile public boolean m_fStandAlone = false;

    // THREAD SUPPORT:
    //            m_diagapplet    is the Thread object for the applet
    //--------------------------------------------------------------------------
    volatile public Thread  main_applet_thread = null;

    volatile public Dimension last_size = null;
    volatile public boolean initialized = false;
    volatile public boolean inside_init = false;
    volatile public boolean inside_resizeable_window = false;
    volatile public int repaint_count = 0;

    public static boolean debug_on = false;
    public static long resize_next_time =0;

    public StandAloneApplet()
    {
	super();
	try
	    {
		addComponentListener(this);
	    }
	catch(Throwable t)
	    {
	    }
	last_size = getSize();
    }

    public void manual_resize(int new_width, int new_height)
    {
	if(debug_on)
	    {
		System.out.println("StandAloneApplet.manual_resize("+new_width+","+new_height+")");
		System.out.println("last_size = "+last_size);
		System.out.println("initialized = "+initialized);
		System.out.println("inside_init = "+inside_init);
		System.out.println("inside_resizeable_window = "+inside_resizeable_window);
		Thread.dumpStack();
	    }
	try
	    {
		if(inside_resizeable_window)
		    {
			if(Math.abs(last_size.width - new_width) > 35 ||
			   Math.abs(last_size.height - new_height) > 35)
			    {
				super.setSize(new_width,new_height);
//				if(initialized && !inside_init)
//				    {
//					removeAll();
//					init();
//				    }
				last_size.width = new_width;
				last_size.height = new_height;                                                }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void cleanup()
    {

    }

    public void startShutdown()
    {
    }

    public boolean canShutdown()
    {
	try
	    {
		cleanup();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	return true;
    }

    public void componentResized(ComponentEvent evt)
    {
	Dimension d = getSize();
	//System.out.println("System.currentTimeMillis()="+System.currentTimeMillis()+", resize_next_time="+resize_next_time+", d="+d);
	if(System.currentTimeMillis() < resize_next_time)
	    {
		return;
	    }
	if(d.width > 0 && d.height > 0)
	    {
		manual_resize(d.width, d.height);
	    }
    }

    public void componentShown(ComponentEvent evt)
    {
	Dimension d = getSize();
	if(d.width > 0 && d.height > 0)
	    {
		manual_resize(d.width, d.height);
	    }
    }

    public void componentHidden(ComponentEvent evt)
    {
    }

    public void componentMoved(ComponentEvent evt)
    {
    }
}
