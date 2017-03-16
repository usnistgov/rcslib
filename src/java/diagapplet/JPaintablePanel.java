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


/*
 * JPaintablePanel.java
 *
 * Created on December 10, 2006, 8:29 AM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */

package diagapplet;

import java.awt.Graphics;
import javax.swing.JPanel;

/**
 * A JPanel that calls a seperate PainterInterface object to do its painting.
 * Used by {@link diagapplet.utils.ImageJPanel} and {@link diag_NB_UI}.
 * @author shackle
 */
public class JPaintablePanel extends JPanel
{
    private static final long serialVersionUID = 2613936L;

    protected diagapplet.PainterInterface painter=null;
    
    /**
     * Set the external painter object that will be used every time the panel needs to be repainted.
     * @param _painter new painter
     */
    public void set_painter(diagapplet.PainterInterface _painter)
    {
	painter=_painter;
    }

    /** Creates a new instance of JPaintablePanel */
    public JPaintablePanel()
    {
    }
    
    protected void paintComponent(Graphics g)
    {
	super.paintComponent(g);
	if(null != painter)
	{
	    painter.paintComponent(g);
	}
    }
    
}
