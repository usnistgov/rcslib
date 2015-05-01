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
 * To change this template, choose Tools | Templates
 * 
 */
package diagapplet.utils;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;

/**
 *
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class fullScreenJPanelClass extends JPanel {

    Image img = null;
    int x_off = 0;
    int y_off = 0;

    
    public void SetImage(BufferedImage _img_in) {
        double hscale = ((double) this.getHeight() / _img_in.getHeight());
        double wscale = ((double) this.getWidth() / _img_in.getWidth());
        if (hscale > wscale) {
            img = _img_in.getScaledInstance(this.getWidth(),
                    ((int) (_img_in.getHeight() * wscale)),
                    BufferedImage.SCALE_DEFAULT);
            x_off = 0;
            y_off = (this.getHeight() - img.getHeight(this)) / 2;
        } else {
            img = _img_in.getScaledInstance(
                    ((int) (_img_in.getWidth() * hscale)),
                    this.getHeight(),
                    BufferedImage.SCALE_DEFAULT);
            x_off = (this.getWidth() - img.getWidth(this)) / 2;
            y_off = 0;
        }
	this.repaint();
    }

    
    @Override
    public void paintComponent(Graphics g) {
        if (null != img) {
	    g.drawImage(img, x_off, y_off, null);
        }
    }
}

