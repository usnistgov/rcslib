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
 * PainterInterface.java
 *
 * Created on December 10, 2006, 8:37 AM
 *
 */

package diagapplet;

import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * Interface that allows the creation classes resposible for painting a component without
 * needing to be derived from java.awt.Component or javax.swing.JComponent. 
 * @author shackle
 */
public interface PainterInterface
{
    
    /**
     * Paint to the graphics object. Typically called by a components paintComponent() method.
     * @param g Grahics object used for painting.
     */
    public void paintComponent(Graphics g);
    
    /**
     * Get rectangle stored with set_selected_rect().
     * @return rectangle last selected.
     */
    public Rectangle get_selected_rect();
    
    /**
     * Store a rectangle typically selected by the user via Mouse or MouseMotion event.
     * @param rect rectangle to select
     */
    public void set_selected_rect(Rectangle rect);
}
