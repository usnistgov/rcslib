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
 * ImageJPanel.java
 *
 * Created on December 15, 2006, 6:01 PM
 */
package diagapplet.utils;

import diagapplet.PainterInterface;
import java.awt.Dimension;
import java.awt.GraphicsDevice;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Vector;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollBar;

/**
 * Swing JPanel that displays an image inside a scrollpane.
 * @author  shackle
 */
public class ImageJPanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 2613936L;
    public ImagePainter img_painter = null;
    private Vector<Runnable> select_rect_change_runnable_vector = null;

    public void AddRunOnSelecteRectChange(Runnable r) {
        if (null == select_rect_change_runnable_vector) {
            select_rect_change_runnable_vector = new Vector<Runnable>();
        }
        select_rect_change_runnable_vector.add(r);
    }

    private void RunAllSelectRectChangeRunnables() {
        try {
            if (null != select_rect_change_runnable_vector) {
                for (int i = 0; i < select_rect_change_runnable_vector.size(); i++) {
                    select_rect_change_runnable_vector.get(i).run();
                }
            }

        } catch (Exception exception) {
            exception.printStackTrace();
        }

    }

    public void setSub_painter(PainterInterface sub_painter) {
        img_painter.setSub_painter(sub_painter);
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void refresh() {
        img_painter.refresh();
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public int get_xstart() {
        return img_painter.get_xstart();
    }

    public int get_ystart() {
        return img_painter.get_ystart();
    }

    public double get_scale() {
        return img_painter.get_scale();
    }

    public void SetFit(boolean f) {
        if (f) {
            Dimension inner_dim = jPaintablePanel2.getPreferredSize();
            Dimension outer_dim = jScrollPane1.getViewport().getSize();
            if (inner_dim.width > outer_dim.width ||
                    inner_dim.height > outer_dim.height) {
                jPaintablePanel2.setSize(outer_dim);
                jPaintablePanel2.setPreferredSize(outer_dim);
                jPaintablePanel2.revalidate();
            }
        }
        img_painter.SetVP(jScrollPane1.getViewport().getWidth(), jScrollPane1.getViewport().getHeight());
        img_painter.SetFit(f);
    }

    public void SetZoom(int z) {
        img_painter.SetZoom(z);
    }

    public void LoadByteArray(byte ba[], int w, int h) {
        img_painter.LoadByteArray(ba, w, h);
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void LoadImage(String s) {
        img_painter.LoadImage(s);
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void LoadImageF(File f) {
        img_painter.LoadImageF(f);
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void LoadImage(BufferedImage _img) {
        img_painter.LoadImage(_img);
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void ClearImage() {
        img_painter.ClearImage();
        if (null != this.fullScreenJPanel) {
            this.fullScreenJPanel.SetImage(img_painter.img);
        }
    }

    public void SetStartPos(int x, int y) {
        img_painter.SetStartPos(x, y);
    }

    public void CombineByteArray(byte ba[], int x, int y, int w, int h) {
        img_painter.CombineByteArray(ba, x, y, w, h);
    }

    public void ClearMarks() {
        img_painter.ClearMarks();
    }

    public void AddMark(byte red, byte green, byte blue, int x, int y) {
        img_painter.AddMark(red, green, blue, x, y);
    }

    public File get_last_saved_image_file() {
        return img_painter.get_last_saved_image_file();
    }

    public void SetCenterPosition(int x, int y) {
        try {
            JScrollBar hsb = jScrollPane1.getHorizontalScrollBar();
            if (null != hsb) {
                int w = (jScrollPane1.getSize().width / 2);
                if (x < w) {
                    hsb.setValue(0);
                } else {
                    hsb.setValue(x - w);
                }
                hsb.setEnabled(false);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            JScrollBar vsb = jScrollPane1.getVerticalScrollBar();
            if (null != vsb) {
                int h = (jScrollPane1.getSize().height / 2);
                if (y < h) {
                    vsb.setValue(0);
                } else {
                    vsb.setValue(y - h);
                }
                vsb.setEnabled(false);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void ClearCenterPosition() {
        try {
            JScrollBar hsb = jScrollPane1.getHorizontalScrollBar();
            if (null != hsb) {
                hsb.setEnabled(true);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            JScrollBar vsb = jScrollPane1.getVerticalScrollBar();
            if (null != vsb) {
                vsb.setEnabled(true);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void set_show_selected_rect(boolean _show) {
        if (null != img_painter) {
            img_painter.set_show_selected_rect(_show);
        }
    }

    public void set_record_images(boolean _b) {
        img_painter.set_record_images(_b);
    }

    public void AddPaintablePanelMouseListener(MouseListener ml) {
        jPaintablePanel2.addMouseListener(ml);
    }
    
    public void AddPaintablePanelMouseMotionListener(MouseMotionListener mml) {
        jPaintablePanel2.addMouseMotionListener(mml);
    }

    /** Creates new form ImageJPanel */
    public ImageJPanel() {
        initComponents();
        img_painter = new ImagePainter();
        jPaintablePanel2.set_painter(img_painter);
        img_painter.SetComponent(jPaintablePanel2);
        jPaintablePanel2.setPreferredSize(jScrollPane1.getViewport().getSize());
        jPaintablePanel2.revalidate();
        img_painter.SetVP(jScrollPane1.getViewport().getWidth(), jScrollPane1.getViewport().getHeight());
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents()
    {

	jScrollPane1 = new javax.swing.JScrollPane();
	jPaintablePanel2 = new diagapplet.JPaintablePanel();

	jPaintablePanel2.addMouseListener(new java.awt.event.MouseAdapter()
				  {

				      public void mouseClicked(java.awt.event.MouseEvent evt)
				      {
					  jPaintablePanel2MouseClicked(evt);
				      }

				      public void mousePressed(java.awt.event.MouseEvent evt)
				      {
					  jPaintablePanel2MousePressed(evt);
				      }

				      public void mouseReleased(java.awt.event.MouseEvent evt)
				      {
					  jPaintablePanel2MouseReleased(evt);
				      }
				  });
	jPaintablePanel2.addComponentListener(new java.awt.event.ComponentAdapter()
				      {

					  public void componentResized(java.awt.event.ComponentEvent evt)
					  {
					      jPaintablePanel2ComponentResized(evt);
					  }
				      });
	jPaintablePanel2.addMouseMotionListener(new java.awt.event.MouseMotionAdapter()
					{

					    public void mouseDragged(java.awt.event.MouseEvent evt)
					    {
						jPaintablePanel2MouseDragged(evt);
					    }
					});

	javax.swing.GroupLayout jPaintablePanel2Layout = new javax.swing.GroupLayout(jPaintablePanel2);
	jPaintablePanel2.setLayout(jPaintablePanel2Layout);
	jPaintablePanel2Layout.setHorizontalGroup(
		jPaintablePanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGap(0, 636, Short.MAX_VALUE));
	jPaintablePanel2Layout.setVerticalGroup(
		jPaintablePanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addGap(0, 543, Short.MAX_VALUE));

	jScrollPane1.setViewportView(jPaintablePanel2);

	javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
	this.setLayout(layout);
	layout.setHorizontalGroup(
		layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 337, Short.MAX_VALUE));
	layout.setVerticalGroup(
		layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING).addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 255, Short.MAX_VALUE));
    }// </editor-fold>//GEN-END:initComponents

    private void jPaintablePanel2ComponentResized(java.awt.event.ComponentEvent evt)//GEN-FIRST:event_jPaintablePanel2ComponentResized
    {//GEN-HEADEREND:event_jPaintablePanel2ComponentResized

        img_painter.SetVP(jScrollPane1.getViewport().getWidth(), jScrollPane1.getViewport().getHeight());
    }//GEN-LAST:event_jPaintablePanel2ComponentResized
    private Rectangle select_rect = null;

    public void Scroll_To_XY(int x, int y) {
        this.jScrollPane1.getHorizontalScrollBar().setValue(x);
        this.jScrollPane1.getVerticalScrollBar().setValue(y);
        repaint();
    }

    private void jPaintablePanel2MouseDragged(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jPaintablePanel2MouseDragged
    {//GEN-HEADEREND:event_jPaintablePanel2MouseDragged

        select_rect = img_painter.get_selected_rect();
        if (null == select_rect || mouse_released) {
            select_rect = new Rectangle();
            select_rect.x = evt.getX();
            select_rect.y = evt.getY();
        }
        int evt_x = evt.getX();
        int evt_y = evt.getY();
        if (evt_x < select_rect.x) {
            select_rect.width += (select_rect.x - evt_x);
            select_rect.x = evt_x;
        } else if (evt_x > select_rect.x) {
            select_rect.width = (evt_x - select_rect.x);
        }
        if (evt_y < select_rect.y) {
            select_rect.height += (select_rect.y - evt_y);
            select_rect.y = evt_y;
        } else if (evt_y > select_rect.y) {
            select_rect.height = (evt_y - select_rect.y);
        }
        img_painter.set_selected_rect(select_rect);
        mouse_released = false;
        this.RunAllSelectRectChangeRunnables();
        this.refresh();
    //diagapplet.diag_common.ErrorPrint("selected_rect="+select_rect+", evt="+evt);
    }//GEN-LAST:event_jPaintablePanel2MouseDragged
    private boolean mouse_released = true;

    public Rectangle get_selected_scaled_rect() {
        try {
            if (null != img_painter) {
                return img_painter.get_scaled_selected_rect();
            }
        } catch (Exception exception) {
            exception.printStackTrace();
        }
        return null;
    }
    int x = 0;

    private void jPaintablePanel2MouseReleased(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jPaintablePanel2MouseReleased
    {//GEN-HEADEREND:event_jPaintablePanel2MouseReleased
        mouse_released = true;
    }//GEN-LAST:event_jPaintablePanel2MouseReleased
    
        int y=0;
        JPopupMenu jpop = null;
    javax.swing.JCheckBoxMenuItem jpopFullScreenCheckboxMenuItem = null;
    JMenuItem jpopNoFullScreenMenuItem = null;
    JMenuItem jpopSaveImageMenuItem = null;
    JMenuItem jpopSetZoomMenuItem = null;
    JMenuItem jpopSetFitMenuItem = null;
    boolean is_full_screen = false;
    fullScreenJPanelClass fullScreenJPanel = null;
    JFrame fullScreenJFrame = null;

    private void ShowFullScreen() {
        
            System.out.println("ShowFullScreen");
            
        fullScreenJFrame = new JFrame();
        fullScreenJFrame.setUndecorated(true);
	fullScreenJPanel = new fullScreenJPanelClass();
	fullScreenJPanel.setSize(Toolkit.getDefaultToolkit().getScreenSize());
	fullScreenJPanel.SetImage(img_painter.img);
	fullScreenJPanel.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt){
                  jPaintablePanel2MouseClicked(evt);
              }
            
            @Override
            public void mousePressed(java.awt.event.MouseEvent evt) {
                  jPaintablePanel2MousePressed(evt);
            }

            @Override
             public void mouseReleased(java.awt.event.MouseEvent evt){
                  jPaintablePanel2MouseReleased(evt);
            }
          });
          
          KeyListener kl = new KeyListener(){
            @Override
              public void keyTyped(KeyEvent e){
                  System.out.println("e=" + e);
                  if (e.getKeyCode() == KeyEvent.VK_ESCAPE)
                  {
                      ClearFullScreen();
                  }
              }
            @Override
              public void keyPressed(KeyEvent e){
                  System.out.println("e=" + e);
                  if (e.getKeyCode() == KeyEvent.VK_ESCAPE){
                      ClearFullScreen();
                  }
              }

            @Override
            public void keyReleased(KeyEvent e){
                System.out.println("e=" + e);
                if (e.getKeyCode() == KeyEvent.VK_ESCAPE)
		{
                    ClearFullScreen();
		}
            }
          };
          fullScreenJPanel.addKeyListener(kl);
          fullScreenJFrame.addKeyListener(kl);
          fullScreenJFrame.add(fullScreenJPanel);
          fullScreenJFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
          
          GraphicsDevice gd = this.getGraphicsConfiguration().getDevice();
          gd.setFullScreenWindow(fullScreenJFrame);
          is_full_screen = true;
    }

    private void ClearFullScreen()
    {
	System.out.println("ClearFullScreen");
	if (null != fullScreenJFrame)
	{
	    fullScreenJFrame.dispose();
	    fullScreenJFrame = null;
	}
	fullScreenJPanel = null;
	is_full_screen = false;
        this.jpopFullScreenCheckboxMenuItem.setSelected(false);
	GraphicsDevice gd = this.getGraphicsConfiguration().getDevice();
	gd.setFullScreenWindow(null);
    }

    private void popup_create()
    {
	jpop = new JPopupMenu();
	jpopFullScreenCheckboxMenuItem = new JCheckBoxMenuItem("Full Screen");
	jpopFullScreenCheckboxMenuItem.addActionListener(new ActionListener(){
            public void actionPerformed(ActionEvent e){
                if (e.getSource() == jpopFullScreenCheckboxMenuItem) {
                    System.out.println("jpopFullScreenCheckboxMenuItem.isSelected()=" + jpopFullScreenCheckboxMenuItem.isSelected());
                    if (!is_full_screen){
                        ShowFullScreen();
                    } else {
                        ClearFullScreen();
                    }
                    if (is_full_screen != jpopFullScreenCheckboxMenuItem.isSelected()){
                        jpopFullScreenCheckboxMenuItem.setSelected(is_full_screen);
                    }
                }
            }
        });
	jpop.add(this.jpopFullScreenCheckboxMenuItem);
        jpopSaveImageMenuItem = new JMenuItem("Save Image As ...");
        jpopSaveImageMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                SaveImage.SaveImageAs(img_painter.img,getParent());
            }
        } );
        jpop.add(jpopSaveImageMenuItem);
        this.jpopSetFitMenuItem = new JMenuItem("Fit");
        this.jpopSetFitMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                SetFit(true);
                img_painter.ReloadImage();
                refresh();
            }
        } );
        jpop.add(this.jpopSetFitMenuItem);
        
        
        this.jpopSetZoomMenuItem = new JMenuItem("Zoom");
        this.jpopSetZoomMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String newZoomString = JOptionPane.showInputDialog("New integer zoom value. (current zoom = "+img_painter.GetZoom()+")");
                SetFit(false);
                SetZoom(Integer.valueOf(newZoomString).intValue());
                img_painter.ReloadImage();
                refresh();
            }
        } );
        jpop.add(this.jpopSetZoomMenuItem);
    }

    private void popup_show(java.awt.event.MouseEvent evt)
    {
	try
	{
	    System.out.println("evt=" + evt);
	    if (null == jpop)
	    {
		popup_create();
	    }
	    if (null != jpop)
	    {
		jpop.show(evt.getComponent(), evt.getX(), evt.getY());
	    }
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    public void set_hide_byte_array(boolean _hide_byte_array)
    {
	img_painter.set_hide_byte_array(_hide_byte_array);
    }
    public BufferedImage GetByteArrayImage()
    {
	return img_painter.GetByteArrayImage();
    }

    public BufferedImage getImage()
    {
        return this.img_painter.getImage();
    }
    
    private void jPaintablePanel2MousePressed(java.awt.event.MouseEvent evt)
    {//GEN-FIRST:event_jPaintablePanel2MousePressed

	if (evt.isPopupTrigger())
	{
	    popup_show(evt);
	}
    }//GEN-LAST:event_jPaintablePanel2MousePressed

    private void jPaintablePanel2MouseClicked(java.awt.event.MouseEvent evt)
    {//GEN-FIRST:event_jPaintablePanel2MouseClicked
	if (evt.isPopupTrigger())
	{
	    popup_show(evt);
	}
    }//GEN-LAST:event_jPaintablePanel2MouseClicked

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private diagapplet.JPaintablePanel jPaintablePanel2;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables
}
