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

package rcsdesign;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

/*
 *
 * AlertDialog
 *
 */
class AlertDialog extends Dialog implements Runnable, ActionListener, WindowListener
{

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613918L;

    TextArea msgArea = null;
    Button okButton = null;
    FlowLayout lm = null;
    Dimension d = new Dimension(480,240);
    static public boolean debug_on = false;
    static int count=0;

    Thread timeoutThread = null;
    public boolean running = false;
    public boolean done = false;

    public void run()
    {
	if(debug_on)
	    {
		System.out.println("AlertDialog.timeoutThread started.\n");
	    }
	try
	    {
		for(int i = 0; i < 1000; i++)
		    {
			Thread.sleep(10);
			System.out.print(i/10.0+"%\r");
		    }
		System.out.println("         ");
	    }
	catch(InterruptedException ie)
	    {
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(running)
	    {
		//synchronized(this) { done = true; notifyAll(); };
		done=true;
		System.out.println("done=true;");
		dispose();
		count--;
		System.out.println("AlertDialog timeout.");
	    }
	running=false;
    }

    public AlertDialog(Frame parent, String title, String message)
    {
	super(parent, true);
	count++;
	init(title,message);
    }

    public void init(String title, String message)
    {
	setVisible(false);
	setTitle(title);
	lm = new FlowLayout();
	setLayout(lm);
	int text_length = 10;
	int text_width = 50;

	/*
	  try
	  {
	  Font f = getFont();
	  int font_height = 10;
	  int font_width = 10;
	  if(null != f)
          {
	  FontMetrics fm = getFontMetrics(f);
	  if(null != fm)
	  {
	  font_height = fm.getHeight();
	  if(font_height < 10)
	  {
	  font_height = 10;
	  }
	  int font_widths[] = fm.getWidths();
	  font_width = 0;
	  int nonzero_fontwidths = 0;
	  int fontwidth_sum = 0;
	  for(int i = 0; i < font_widths.length; i++)
	  {
	  if(font_widths[i] > 0)
	  {
	  nonzero_fontwidths++;
	  fontwidth_sum += font_widths[i];
	  }
	  }
	  font_width = fontwidth_sum/nonzero_fontwidths+1;
	  if(font_width < 10)
	  {
	  font_width = 10;
	  }
	  }
          }
	  if(debug_on)
          {
	  System.out.println("font_height = "+font_height);
	  System.out.println("font_width = "+font_width);
          }
	  text_length = d.height/font_height;
	  if(text_length < 8)
          {
	  text_length = 8;
          }
	  text_width = d.width/font_width;
	  if(text_width < 20)
          {
	  text_width = 20;
          }
	  }
	  catch(Exception e)
	  {
	  e.printStackTrace();
	  }
	*/

	msgArea = new AutoSizedTextArea(message,d.width-20,d.height -100,100,20);
	text_width = msgArea.getColumns();
	text_length = msgArea.getRows();
	if(debug_on)
	    {
		System.out.println("text_length = "+text_length);
		System.out.println("text_width = "+text_width);
	    }
	if(message.length() > text_width)
	    {
		StringTokenizer messageTokenizer = new StringTokenizer(message,"\r\n");
		String new_message = "";
		while(messageTokenizer.hasMoreTokens())
		    {
			String line = messageTokenizer.nextToken();
			if(line.length() > text_width)
			    {
				for(int offset = text_width-4; offset < line.length(); offset+=(text_width-4))
				    {
					line = line.substring(0,offset)+"- \\\n"+line.substring(offset);
				    }
			    }
			new_message += line +"\n";
		    }
		message = new_message;
	    }

	msgArea.setEditable(false);
	add(msgArea);
	okButton = new Button("OK");
	add(okButton);
	okButton.addActionListener(this);

	setSize(d);
	int tries = 0;
	if(debug_on)
	    {
		System.out.println("Showing AlertDialog with message = "+message);
	    }
	//timeoutThread = new Thread(this);
	//timeoutThread.start();
	running = true;
	addWindowListener(this);
	pack();
    }

    public void actionPerformed(ActionEvent evt)
    {
	if(debug_on)
	    {
		System.out.println("AlertDialog.actionPerformed("+evt+")");
	    }
	if(evt.getSource() == okButton)
	    {
		//synchronized(this) { done = true; notifyAll(); };
		done=true;
		dispose();
		count--;
		running=false;
		try
		    {
			if(null != timeoutThread)
			    {
				timeoutThread.interrupt();
				timeoutThread = null;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
		return;
	    }
    }

    public Dimension getPreferredSize()
    {
	return d;
    }

    public Dimension getMinimumSize()
    {
	return d;
    }

    /*
     * The functions windowOpened, windowClosing, windowClosed, windowActivated,
     * windowDeactivated, windowIconified, and windowDeiconified are needed
     * to implement WindowListener and basically replace handleEvent
     * from JDK 1.0.x
     *
     */

    public void windowOpened(WindowEvent evt)
    {
    }

    public void windowClosing(WindowEvent evt)
    {
	if(debug_on)
	    {
		System.out.println("windowClosing("+evt+")");
	    }
	try
	    {
		done = true;
		dispose();
		count--;
		running=false;
		try
		    {
			if(null != timeoutThread)
			    {
				timeoutThread.interrupt();
				timeoutThread = null;
			    }
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void windowClosed(WindowEvent evt)
    {
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

    public static  void main(String args[])
    {
	Frame f = new Frame();
	System.out.println("AlertDialog test program started.");
	AlertDialog ad = new AlertDialog(f,"Test","TestMsg");
	System.out.println("AlertDialog object created.");
	ad.setVisible(true);
	System.out.println("AlertDialog object show() returned.");
	while(!ad.done)
	    {
		try
		    {
			Thread.sleep(100);
		    }
		catch(Exception e)
		    {
		    }
	    }
	System.out.println("ad.done");
	System.exit(0);
    }


}
