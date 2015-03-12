/*
 * XYZPanel.java
 *
 * Created on July 5, 2007, 10:17 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package jplotxyz;

import java.awt.Graphics;
import java.awt.Image;

/**
 *
 * @author shackle
 */
public class XYZPanel extends javax.swing.JPanel
{
    
    public void paintComponent(Graphics g)
    {
	super.paintComponent(g);
	if(null != image_to_paint)
	{
	    g.drawImage(image_to_paint,0,0,null);
	}
    }
    
    /** Creates a new instance of XYZPanel */
    public XYZPanel()
    {
    }

	/**
	 * Holds value of property image_to_paint.
	 */
	private java.awt.Image image_to_paint=null;

	/**
	 * Getter for property image_to_paint.
	 * @return Value of property image_to_paint.
	 */
	public java.awt.Image getImage_to_paint()
	{
		return this.image_to_paint;
	}

	/**
	 * Setter for property image_to_paint.
	 * @param image_to_paint New value of property image_to_paint.
	 */
	public void setImage_to_paint(java.awt.Image image_to_paint)
	{
		this.image_to_paint = image_to_paint;
	}
    
}
