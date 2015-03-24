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

import java.awt.Panel;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Color;


/**
 * AWT Panel for displaying progress while loading a URL.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public	class URLLoadInfoPanel extends Panel implements URLLoadInfoPanelInterface
{
    static public boolean ignore_repaint_requests = false;
    public boolean use_color = true;
    long last_repaint_time = 0;
    static final long MAX_REPAINT_TIME = 1000;
    static final long MIN_REPAINT_TIME = 500;
    public String URLname = null;
    public int content_length = -1;
    public int bytes_read = 0;
    Dimension prefDim = null;
    static public boolean debug_on=false;

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613898L;


    public void set_bytes_read(int _bytes_read)
    {
	bytes_read = _bytes_read;
    }

    public int get_bytes_read()
    {
	return bytes_read;
    }

    public void set_content_length(int _content_length)
    {
	content_length = _content_length;
    }

    public int get_content_length()
    {
	return content_length;
    }

    public void set_URLname(String _URLname)
    {
	URLname = _URLname;
    }

    public String get_URLname()
    {
	return URLname;
    }

    public String toString()
    {
	return "{ URLname="+URLname+"; content_length="+content_length+"; bytes_read="+bytes_read+"; }";
    }

    public URLLoadInfoPanel()
    {
	super();
	URLname = "";
	content_length = -1;
	bytes_read = 0;
	if(null == prefDim)
	    {
		prefDim =new Dimension(420,40);
	    }
	setSize(prefDim);
    }

    public URLLoadInfoPanel(int pref_width, int pref_height)
    {
	this();
	prefDim =new Dimension(pref_width,pref_height);
	setSize(prefDim);
    }

    public Dimension getPreferredSize()
    {
	return prefDim;
    }

    public Dimension getMinimumSize()
    {
	return prefDim;
    }

    public void updateDisplay()
    {
	if(ignore_repaint_requests)
	    {
		return;
	    }
	if(debug_on)
	    {
		System.out.println(this.toString());
	    }
	if(System.currentTimeMillis() - last_repaint_time > MAX_REPAINT_TIME)
	    {
		force_repaint(2);
	    }
	else if(System.currentTimeMillis() - last_repaint_time > MIN_REPAINT_TIME && isVisible())
	    {
		repaint();
	    }
    }

    boolean repainted = false;
   
    public void force_repaint(int max_tries)
    {
	if(ignore_repaint_requests)
	    {
		repainted=true;
		return;
	    }
	if(!isVisible())
	    {
		repainted=true;
		return;
	    }
	repainted = false;
	int tries = 0;
	if(max_tries > 10)
	    {
		max_tries=10;
	    }
	while(!repainted && tries < max_tries)
	    {
		tries++;
		if(debug_on)
		    {
			System.out.println("\nURLLoadInfoPanel.force_repaint() : tries="+tries);
			//Thread.dumpStack();
		    }
		if(ignore_repaint_requests)
		    {
			repainted=true;
			return;
		    }
		if(System.currentTimeMillis() - last_repaint_time <= MAX_REPAINT_TIME)
		    {
			repainted=true;
			return;
		    }
		try
		    {
			repaint();
			Thread.sleep(10);
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
			repainted = true;
			return;
		    }
	    }
	repainted=true;
    }


    public void paint(Graphics g)
    {
	repainted=true;
	try
	    {
		last_repaint_time = System.currentTimeMillis();
		if(debug_on)
		    {
			System.out.println("URLLoadInfoPanel.paint() : last_repaint_time ="+last_repaint_time);
		    }
		String str = URLname + " ( "+bytes_read+" out of "+content_length+" )";
		g.drawString(str,10,20);
		g.drawRect(10,24,prefDim.width - 18,11);
		if(content_length > 0 )
		    {
			if(bytes_read > 0)
			    {
				if(bytes_read < content_length)
				    {
					if(use_color)
					    {
						g.setColor(Color.blue);
					    }
					else
					    {
						g.setColor(Color.black);
					    }
					int percent_done = ((prefDim.width - 20)*bytes_read)/content_length;
					g.fillRect(11,25,percent_done,9);
				    }
				else if(bytes_read == content_length)
				    {
					if(use_color)
					    {
						g.setColor(Color.green);
					    }
					else
					    {
						g.setColor(Color.black);
					    }
					g.fillRect(11,25,prefDim.width - 20,9);
				    }
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	repainted = true;
    }

	public void inc_bytes_read(int _bytes_read_inc)
	{
	    bytes_read += _bytes_read_inc;
	}
}
