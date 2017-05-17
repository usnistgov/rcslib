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
 * ImagePainter.java
 *
 * Created on December 15, 2006, 6:04 PM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */
package diagapplet.utils;

import diagapplet.PainterInterface;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.imageio.ImageIO;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

/**
 *
 * @author shackle
 */
class ImagePainter
	implements diagapplet.PainterInterface
{
    static private boolean out_of_temp_space_warning_given=false;

    /** Creates a new instance of ImagePainter */
    public ImagePainter()
    {
    }
    public BufferedImage img = null;
    public Image scaled_img = null;
    private BufferedImage sub_img = null;
    private int sub_img_x;
    private int sub_img_y;
    private int sub_img_width;
    private int sub_img_height;
    private int scale_img_x;
    private int scale_img_y;
    private int scale_img_width;
    private int scale_img_height;
    private int xstart = 0;
    private int ystart = 0;
    private double last_fit_scale = 1.0;
    public JPanel c = null;
    byte b[] = null;
    int b_width;
    int b_height;
    int b_size;
    Color c_array[][] = null;
    private int zoom = 1;
    private int last_zoom = 1;
    private boolean fit = false;
    private boolean last_fit = false;
    private int vp_width = 0;
    private int vp_height = 0;
    private int last_w = 0;
    private int last_h = 0;
    private boolean record_images = false;
    File last_saved_image_file = null;
    private int last_c_w = 0;
    private int last_c_h = 0;
    private int last_i_w = 0;
    private int last_i_h = 0;
    byte init_red = 0;
    byte init_green = 0;
    byte init_blue = (byte) 255;
    private boolean need_refresh = true;
    private Rectangle selected_rect = null;

    @Override
    public void set_selected_rect(Rectangle rect)
    {
	this.selected_rect = rect;
    }

    @Override
    public Rectangle get_selected_rect()
    {
	return this.selected_rect;
    }

    private Rectangle scaled_selected_rect=null;
    
    public Rectangle get_scaled_selected_rect()
    {
	try
	{
	    if (null == this.selected_rect)
	    {
		scaled_selected_rect = null;
		return null;
	    }
	    Rectangle scaled_rectangle = new Rectangle();
	    final double cur_scale = get_scale();
	    if(cur_scale > 0.0)
	    {
		final double cur_scale_inv = 1.0/cur_scale;
		scaled_rectangle.x = (int) (selected_rect.x * cur_scale_inv);
		scaled_rectangle.width = (int) (selected_rect.width *cur_scale_inv);
		scaled_rectangle.y = (int) (selected_rect.y * cur_scale_inv);
		scaled_rectangle.height = (int) (selected_rect.height* cur_scale_inv);
	    }
	    this.scaled_selected_rect = scaled_rectangle;

	}
	catch (Exception exception)
	{
	    exception.printStackTrace();
	}
	return scaled_selected_rect;
    }
    
    public int get_xstart()
    {
	return xstart;
    }

    public int get_ystart()
    {
	return ystart;
    }

    public double get_scale()
    {
	if (fit)
	{
	    return last_fit_scale;
	}
	else if (zoom > 1)
	{
	    return (double) zoom;
	}
	else if (zoom < -1)
	{
	    return -1.0 / zoom;
	}
	else
	{
	    return 1.0;
	}
    }

    public void set_record_images(boolean _b)
    {
	record_images = _b;
    }

    public File get_last_saved_image_file()
    {
	return last_saved_image_file;
    }

    private class img_mark
    {

	int rgb;
	int x;
	int y;
    }
    private Vector<img_mark> marks = null;

    public void ClearMarks()
    {
	marks = null;
    }

    public void AddMark(byte red_byte, byte green_byte, byte blue_byte, int x, int y)
    {
	try
	{
	    if (null == marks)
	    {
		marks = new Vector<img_mark>();
	    }
	    img_mark new_mark = new img_mark();
	    int red = (int) red_byte;
	    int green = (int) green_byte;
	    int blue = (int) blue_byte;
	    if (red < 0)
	    {
		red += 256;
	    }
	    if (green < 0)
	    {
		green += 256;
	    }
	    if (blue < 0)
	    {
		blue += 256;
	    }
	    new_mark.rgb = (new Color(red, green, blue)).getRGB();
	    new_mark.x = x;
	    new_mark.y = y;
	    marks.add(new_mark);
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}
    }

    public void SetVP(int w, int h)
    {
	this.vp_width = w;
	this.vp_height = h;
    }

    public void SetFit(boolean f)
    {
	fit = f;
    }

    public void SetComponent(JPanel _c)
    {
	c = _c;
    }

    public void SetZoom(int _zoom)
    {
	this.zoom = _zoom;
    }
    
    public int GetZoom()
    {
        return this.zoom;
    }

    public void SetStartPos(int x, int y)
    {
	xstart = x;
	ystart = y;
    }

    public void CombineByteArray(byte ba[], int x, int y, int w, int h)
    {
	int old_xstart = xstart;
	int old_ystart = ystart;
	int old_height = b_height;
	int old_width = b_width;
	x -= xstart;
	y -= ystart;
	if (x < 0)
	{
	    xstart += x;
	    b_width -= x;
	    x = 0;
	}
	if (y < 0)
	{
	    ystart += y;
	    b_height -= y;
	    y = 0;
	}
	if (b_width < x + w)
	{
	    b_width = x + w;
	}
	if (b_height < y + h)
	{
	    b_height = y + h;
	}
	if (b_width != old_width || b_height != old_height || old_xstart != xstart || old_ystart != ystart)
	{
	    byte new_b[] = new byte[b_height * b_width * 3];
	    for (int i = 0; i < new_b.length; i += 3)
	    {
		if (Thread.interrupted())
		{
		    return;
		}
		new_b[i] = init_red;
		new_b[i + 1] = init_green;
		new_b[i + 2] = init_blue;
	    }
	    for (int i = 0; i < old_width; i++)
	    {
		for (int j = 0; j < old_height; j++)
		{
		    if (Thread.interrupted())
		    {
			return;
		    }
		    int new_index = ((j + (old_ystart - ystart)) * b_width + i + (old_xstart - xstart)) * 3;
		    int old_index = (j * old_width + i) * 3;
		    new_b[new_index] = b[old_index];
		    new_b[new_index + 1] = b[old_index + 1];
		    new_b[new_index + 2] = b[old_index + 2];
		}
	    }
	    b = new_b;
	}
	for (int i = 0; i < w; i++)
	{
	    for (int j = 0; j < h; j++)
	    {
		if (Thread.interrupted())
		{
		    return;
		}
		int b_index = ((j + y) * b_width + i + x) * 3;
		int ba_index = (j * w + i) * 3;
		b[b_index] = ba[ba_index];
		b[b_index + 1] = ba[ba_index + 1];
		b[b_index + 2] = ba[ba_index + 2];
	    }
	}
	LoadByteArray(b, b_width, b_height);
    }
    
    public BufferedImage GetByteArrayImage()
    {
	if(null == b || b_width < 1 || b_height < 1)
	{
	    return null;
	}
	BufferedImage bi = new BufferedImage(b_width,
		     b_height,
		     BufferedImage.TYPE_INT_ARGB);
	for(int i = 0; i < b_width; i++)
	{
	    for(int j=0; j < b_height; j++)
	    {
		if (Thread.interrupted())
		{
		    return null;
		}
		int red = (int) b[(j * b_width + i) * 3];
		int green = (int) b[(j * b_width + i) * 3 + 1];
		int blue = (int) b[(j * b_width + i) * 3 + 2];
		if (red < 0)
		{
		    red += 256;
		}
		if (green < 0)
		{
		    green += 256;
		}
		if (blue < 0)
		{
		    blue += 256;
		}
		//c_array[i][j] = (new Color(red,green,blue));
		int rgb = (new Color(red, green, blue)).getRGB();
		bi.setRGB(i,j, rgb);
	    }
	}
	if (null != marks)
	{
	    for (img_mark mrk : marks)
	    {
		int x = mrk.x - xstart;
		int y = mrk.y - ystart;
		if (Thread.interrupted())
		{
		    return null;
		}
		if (bi.getWidth() > x && x > 0 && bi.getHeight() > y  && y > 0)
		{
		    bi.setRGB(x, y, mrk.rgb);
		}
	    }
	}
	return bi;
    }
    
    private boolean hide_byte_array;
    
    public void set_hide_byte_array(boolean _hide_byte_array)
    {
	this.hide_byte_array = _hide_byte_array;
    }
    

    public void LoadByteArray(byte ba[], int width, int height)
    {
	b = ba;
	double fit_scale = 1.0;
	int old_size = b_size;
	int old_height = b_height;
	int old_width = b_width;
	b_size = ba.length;
	b_width = width;
	b_height = height;
	if(hide_byte_array)
	{
	    return;
	}
	boolean new_img = false;
	int cur_zoom = zoom;
	if (fit)
	{
	    cur_zoom = 1;
	}
	if (cur_zoom < 1)
	{
	    cur_zoom = 1;
	}
	if (img == null || img.getHeight() != (b_height * cur_zoom) || img.getWidth() != (b_width * cur_zoom))
	{
	    new_img = true;
	    img = new BufferedImage(b_width * cur_zoom, b_height * cur_zoom, BufferedImage.TYPE_INT_ARGB);
	//System.out.println("img="+img);
	}
	int min_i = Integer.MAX_VALUE;
	int max_i = Integer.MIN_VALUE;
	int min_j = Integer.MAX_VALUE;
	int max_j = Integer.MIN_VALUE;
	for (int i = 0; i < b_width; i++)
	{
	    for (int j = 0; j < b_height; j++)
	    {
		if (Thread.interrupted())
		{
		    return;
		}
		int red = (int) b[(j * b_width + i) * 3];
		int green = (int) b[(j * b_width + i) * 3 + 1];
		int blue = (int) b[(j * b_width + i) * 3 + 2];
		if (red < 0)
		{
		    red += 256;
		}
		if (green < 0)
		{
		    green += 256;
		}
		if (blue < 0)
		{
		    blue += 256;
		}
		//c_array[i][j] = (new Color(red,green,blue));
		int rgb = (new Color(red, green, blue)).getRGB();
		try
		{
		    if (new_img || rgb != img.getRGB(i * cur_zoom, j * cur_zoom))
		    {
			if (min_i > i)
			{
			    min_i = i;
			}
			if (max_i < i)
			{
			    max_i = i;
			}
			if (min_j > j)
			{
			    min_j = j;
			}
			if (max_j < j)
			{
			    max_j = j;
			}
			for (int xi = 0; xi < cur_zoom; xi++)
			{
			    for (int yj = 0; yj < cur_zoom; yj++)
			    {
				img.setRGB(i * cur_zoom + xi, j * cur_zoom + yj, rgb);
			    }
			}
		    }
		}
		catch (Exception e)
		{
		    System.err.println("i=" + i + ", j=" + j + ", cur_zoom=" + cur_zoom + ", img.getWidth()=" + img.getWidth() + ", img.getHeight()=" + img.getHeight());
		    e.printStackTrace();
		    img = null;
		    break;
		}
	    }
	}
	if (sub_painter != null)
	{
	    sub_painter.paintComponent(img.getGraphics());
	}
	if (null != marks)
	{
	    for (img_mark mrk : marks)
	    {
		int x = mrk.x - xstart;
		int y = mrk.y - ystart;
		if (Thread.interrupted())
		{
		    return;
		}
		if (img.getWidth() > x * cur_zoom && x > 0 && img.getHeight() > y * cur_zoom && y > 0)
		{
		    int i = x;
		    int j = y;
		    if (new_img || mrk.rgb != img.getRGB(i * cur_zoom, j * cur_zoom))
		    {
			if (min_i > i)
			{
			    min_i = i;
			}
			if (max_i < i)
			{
			    max_i = i;
			}
			if (min_j > j)
			{
			    min_j = j;
			}
			if (max_j < j)
			{
			    max_j = j;
			}
			if (!fit && zoom > 1)
			{
			    for (int xp = 0; xp < zoom; xp++)
			    {
				for (int yp = 0; yp < zoom; yp++)
				{
				    img.setRGB(x * cur_zoom + xp, y * cur_zoom + yp, mrk.rgb);
				}
			    }
			}
			else
			{
			    img.setRGB(x, y, mrk.rgb);
			}
		    }
		}
	    }
	}
	if (min_i < 2)
	{
	    min_i = 0;
	}
	else
	{
	    min_i -= 2;
	}
	if (max_i > width - 3)
	{
	    max_i = width - 1;
	}
	else
	{
	    max_i += 2;
	}
	if (min_j < 2)
	{
	    min_j = 0;
	}
	else
	{
	    min_j -= 2;
	}
	if (max_j > height - 3)
	{
	    max_j = height - 1;
	}
	else
	{
	    max_j += 2;
	}

	try
	{
	    if (null != img && record_images)
	    {
		last_saved_image_file = null;
		
		File tf = File.createTempFile("java_img_painter", ".jpg");
                long free_space = tf.getParentFile().getFreeSpace();
                long needed_free_space = (long) (1e6 + 20*img.getHeight()*img.getWidth());
                if(free_space < needed_free_space)
                {
                    if(!out_of_temp_space_warning_given)
                    {
                        out_of_temp_space_warning_given=true;
                        String s = "Disk space low ("+ (free_space/1024)+"k) for dir "+tf.getParent()+" : will not record images.";
                        JOptionPane.showMessageDialog(null,s , "Map record Warning", JOptionPane.WARNING_MESSAGE);
                        System.err.println(s);        
                    }
                }
                else
                {
                    //	System.out.println("tf="+tf.getAbsolutePath());
                    if(img.getWidth() % 16 != 0 || img.getHeight() % 16 != 0)
                    {
                        BufferedImage img_to_record = new BufferedImage(img.getWidth() + (16 - (img.getWidth()%16)),
                                img.getHeight() + (16 - (img.getHeight()%16)),
                                BufferedImage.TYPE_INT_RGB);
                        img_to_record.getGraphics().drawImage(img, 0,0,null);
                        ImageIO.write(img_to_record, "jpg", tf);
                    }
                    else
                    {
                        ImageIO.write(img, "jpg", tf);
                    }
                    last_saved_image_file = tf;
                }
	    }
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}

	if (c != null && img != null)
	{
	    scaled_img = null;
	    scale_img_x = 0;
	    scale_img_y = 0;
	    int c_w = c.getWidth();
	    int c_h = c.getHeight();
	    int i_w = img.getWidth();
	    int i_h = img.getHeight();
	    if (fit)
	    {
		double wscale = ((double) c_w) / ((double) i_w);
		double hscale = ((double) c_h) / ((double) i_h);
		if (wscale < hscale)
		{
		    fit_scale = wscale;
		}
		else
		{
		    fit_scale = hscale;
		}
	    }
	    if (new_img || need_refresh || old_width != width || old_height != height ||
		    cur_zoom != last_zoom || last_fit != fit || last_fit_scale != fit_scale ||
		    last_i_w != i_w || last_i_h != i_h || last_c_w != c_w || last_c_h != c_h)
	    {
		if (fit)
		{
		    scaled_img = img.getScaledInstance((int) (img.getWidth(null) * fit_scale), (int) (img.getHeight(null) * fit_scale), Image.SCALE_FAST);
		//c.setPreferredSize(new Dimension(scaled_img.getWidth(null),scaled_img.getHeight(null)));
		}
		else
		{
		    c.setPreferredSize(new Dimension(b_width * cur_zoom, b_height * cur_zoom));
		}
		c.revalidate();
		c.repaint();
		sub_img = null;
	    }
	    else
	    {
		sub_img_x = min_i;
		sub_img_y = min_j;
		sub_img_width = (max_i - min_i);
		sub_img_height = (max_j - min_j);
		if (cur_zoom > 1)
		{
		    sub_img_x *= cur_zoom;
		    sub_img_y *= cur_zoom;
		    sub_img_width *= cur_zoom;
		    sub_img_height *= cur_zoom;
		}
		if (sub_img_width <= 0 || sub_img_height <= 0)
		{
		    return;
		}
		if (sub_img_width < img.getWidth() || sub_img_height < img.getHeight())
		{
		    sub_img = img.getSubimage(sub_img_x, sub_img_y, sub_img_width, sub_img_height);
		    if (fit)
		    {
			scaled_img = sub_img.getScaledInstance((int) (sub_img.getWidth(null) * fit_scale), (int) (sub_img.getHeight(null) * fit_scale), Image.SCALE_FAST);
			scale_img_x = (int) (sub_img_x * fit_scale);
			scale_img_y = (int) (sub_img_y * fit_scale);
			scale_img_height = (int) (sub_img_height * fit_scale);
			scale_img_width = (int) (sub_img_width * fit_scale);
			//c.repaint();
			c.repaint(scale_img_x, scale_img_y, scale_img_width, scale_img_height);
		    }
		    else
		    {
			c.repaint(sub_img_x, sub_img_y, sub_img_width, sub_img_height);
		    }
		}
		else
		{
		    if (fit)
		    {
			scaled_img = img.getScaledInstance((int) (img.getWidth(null) * fit_scale), (int) (img.getHeight(null) * fit_scale), Image.SCALE_FAST);
		    }
		    c.repaint();
		}
	    }
	    last_i_w = i_w;
	    last_i_h = i_h;
	    last_c_w = c_w;
	    last_c_h = c_h;
	}
	last_zoom = cur_zoom;
	last_fit = fit;
	last_fit_scale = fit_scale;
    }

    public void refresh()
    {
	sub_img = null;
	need_refresh = true;
	scale_img_x = 0;
	scale_img_y = 0;
	if (fit && null != img)
	{
	    scaled_img = img.getScaledInstance((int) (img.getWidth(null) * last_fit_scale), (int) (img.getHeight(null) * last_fit_scale), Image.SCALE_FAST);
	//c.setPreferredSize(new Dimension(scaled_img.getWidth(null),scaled_img.getHeight(null)));
	}
	else if(zoom == 0 || zoom == 1)
	{
	    scaled_img = null;
	    if (null != img)
	    {
		c.setPreferredSize(new Dimension(img.getWidth(null), img.getHeight(null)));
	    }
	}
	c.revalidate();
	c.repaint();
	sub_img = null;
	need_refresh = true;
    }

    @SuppressWarnings("deprecation")
    private void ReadPpm(File f)
    {
	try
	{
	    byte old_b[] = b;
	    int old_size = b_size;
	    b = null;
	    img = null;
	    int ppm_width = 0;
	    int ppm_height = 0;
	    int ppm_size = 0;
	    FileInputStream fis = new FileInputStream(f);
	    DataInputStream dis = new DataInputStream(fis);
	    //BufferedReader br= new BufferedReader(new InputStreamReader(fis));
	    String l = dis.readLine();
	    boolean hw_read = false;
	    while (l != null)
	    {
		if (l.startsWith("#") || l.startsWith("P6"))
		{
		    l = dis.readLine();
		    continue;
		}
		if (!hw_read)
		{
		    StringTokenizer st = new StringTokenizer(l, " ");
		    ppm_width = Long.valueOf(st.nextToken()).intValue();
		    ppm_height = Long.valueOf(st.nextToken()).intValue();
		    ppm_size = 3 * ppm_width * ppm_height;
		    if (old_size == ppm_size && old_b != null)
		    {
			b = old_b;
		    }
		    else
		    {
			b_size = ppm_size;
			b = new byte[b_size];
		    }
		    dis.readLine();
		    hw_read = true;
		    dis.readFully(b);
		    l = null;
		    break;
		}
	    }
	    dis.close();
	    fis.close();
	    dis = null;
	    fis = null;
	    if (ppm_width > 0 && ppm_height > 0)
	    {
		LoadByteArray(b, ppm_width, ppm_height);
	    }
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}
    }

    public void ClearImage()
    {
	this.img = null;
	this.scaled_img = null;
	this.sub_img = null;
	this.selected_rect = null;
	this.marks = null;
	this.b = null;
    }

    public void LoadImage(String s)
    {
	LoadImageF(new File(s));
    }

    public void LoadImageF(File f)
    {
	try
	{
	    if (f.getName().endsWith(".ppm"))
	    {
		ReadPpm(f);
		return;
	    }
	    LoadImage(ImageIO.read(f));
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}
    }

    static private boolean bad_fit_warn_given=false;
   
    public void LoadImage(BufferedImage _img)
    {
        this.img = _img;
        ReloadImage();
    }
    
    public void ReloadImage()
    {
	try
	{
	    sub_img = null;
	    scale_img_x = 0;
	    scale_img_y = 0;
	    sub_img_x = 0;
	    sub_img_y = 0;
	    if (fit)
	    {
		if (null == img || img.getHeight() < 1 || img.getWidth() < 1 ||
			vp_width < 1 || vp_height < 1)
		{
		    if(!bad_fit_warn_given)
		    {
			System.err.println("Can not fit image: image or viewport not initialized.");
			bad_fit_warn_given=true;
		    }
		    last_fit_scale = 1.0;
		}
		else
		{
		    double ratio_w = ((double) img.getWidth()) / ((double) vp_width);
		    double ratio_h = ((double) img.getHeight()) / ((double) vp_height);

		    if (ratio_w > ratio_h)
		    {
			last_fit_scale = 1.0 / ratio_w;
			scaled_img = img.getScaledInstance(vp_width, (int) (img.getHeight() / ratio_w), Image.SCALE_FAST);
		    }
		    else
		    {
			last_fit_scale = 1.0 / ratio_h;
			scaled_img = img.getScaledInstance((int) (img.getWidth() / ratio_h), vp_height, Image.SCALE_FAST);
		    }
		    c.repaint();
		}
	    }
	    else if (c != null && img != null)
	    {
		if (zoom > 1)
		{
		    scaled_img = img.getScaledInstance((int) (img.getWidth() * zoom), img.getHeight() * zoom, Image.SCALE_FAST);
		    if (last_w != scaled_img.getWidth(null) || last_h != scaled_img.getHeight(null))
		    {
			last_w = scaled_img.getWidth(null);
			last_h = scaled_img.getHeight(null);
			c.setPreferredSize(new Dimension(last_w, last_h));
			c.revalidate();
		    }
		}
		else if (zoom < -1)
		{
		    scaled_img = img.getScaledInstance((int) (img.getWidth() / (-zoom)), img.getHeight() / (-zoom), Image.SCALE_FAST);
		    if (last_w != scaled_img.getWidth(null) || last_h != scaled_img.getHeight(null))
		    {
			last_w = scaled_img.getWidth(null);
			last_h = scaled_img.getHeight(null);
			c.setPreferredSize(new Dimension(last_w, last_h));
			c.revalidate();
		    }
		}
		else
		{
		    scaled_img = null;
		    if (last_w != img.getWidth(null) || last_h != img.getHeight(null))
		    {
			last_w = img.getWidth(null);
			last_h = img.getHeight(null);
			c.setPreferredSize(new Dimension(last_w, last_h));
			c.revalidate();
		    }
		}
		c.repaint();
	    }
	    last_fit = fit;
	    last_zoom = zoom;
	}
	catch (Exception e)
	{
	    e.printStackTrace();
	}
    }

    private boolean show_selected_rect=false;
    
    public void set_show_selected_rect(boolean _show)
    {
	this.show_selected_rect = _show;
    }
    
    public BufferedImage getImage()
    {
        return this.img;
    }
    
    public void paintComponent(Graphics g)
    {
        if(!fit && zoom > 1)
        {
            System.out.println("fit = " + fit);
            System.out.println("scaled_img = " + scaled_img);
            System.out.println("zoom = " + zoom);
        }
	if (null != scaled_img)
	{
	    g.drawImage(scaled_img, scale_img_x, scale_img_y, null);
//	    g.setColor(Color.MAGENTA);
//	    g.drawRect(scale_img_x,scale_img_y,scaled_img.getWidth(null),scaled_img.getHeight(null));
//	    if(null != sub_img)
//	    {
//		g.drawImage(sub_img,0,img.getHeight()+100,null);
//	    }
	}
	else if (sub_img != null && !need_refresh)
	{
	    g.drawImage(sub_img, sub_img_x, sub_img_y, null);
	    sub_img = null;
	}
	else if (img != null)
	{
	    g.drawImage(img, 0, 0, null);
	    sub_img = null;
	}
	if (null != selected_rect && show_selected_rect)
	{
	    g.setColor(Color.WHITE);
	    g.drawRect(selected_rect.x, selected_rect.y, selected_rect.width, selected_rect.height);
	    if(selected_rect.x > 0 && selected_rect.y > 0)
	    {
		g.setColor(Color.BLACK);
		g.drawRect(selected_rect.x-1, selected_rect.y-1, selected_rect.width+2, selected_rect.height+2);
	    }	    
	}
	need_refresh = false;
    }
    /**
     * Holds value of property sub_painter.
     */
    private PainterInterface sub_painter = null;

    /**
     * Getter for property sub_painter.
     * @return Value of property sub_painter.
     */
    public PainterInterface getSub_painter()
    {
	return this.sub_painter;
    }

    /**
     * Setter for property sub_painter.
     * @param sub_painter New value of property sub_painter.
     */
    public void setSub_painter(PainterInterface sub_painter)
    {
	this.sub_painter = sub_painter;
    }
}
