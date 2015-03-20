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
import java.util.*;
import java.awt.event.*;

/*
 *
 * QueryDialog
 *
 */
class QueryDialog extends Dialog  implements ActionListener
{

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613919L;

    GridBagLayout lmInner = null;
    Panel innerPanel = null;
    TextArea msgArea = null;
    Button yesButton = null;
    Button noButton = null;
    Button yesToAllButton = null;
    Button noToAllButton = null;

    Button cancelButton = null;
    FlowLayout lm = null;
    public boolean done = false;
    public boolean ok = false;
    public boolean yes_to_all = false;
    public boolean no_to_all = false;
    static public boolean cancel = false;
    static public boolean debug_on = false;


    Dimension d = new Dimension(480,240);

    QueryDialog(Frame parent, String title, String message)
    {
	super(parent, title, false);
	lm = new FlowLayout(FlowLayout.LEFT);
	setLayout(lm);
	int text_length = 10;
	int text_width = 40;

	try
	    {
		Font f = getFont();
		int font_height = 8;
		int font_width = 8;
		if(null != f)
		    {
			FontMetrics fm = getFontMetrics(f);
			if(null != fm)
			    {
				font_height = fm.getHeight();
				if(font_height < 8)
				    {
					font_height = 8;
				    }
				int font_widths[] = fm.getWidths();
				font_width = 0;
				for(int i = 0; i < font_widths.length; i++)
				    {
					if(font_width < font_widths[i])
					    {
						font_width = font_widths[i];
					    }
				    }
				if(font_width < 8)
				    {
					font_width = 8;
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
	if(debug_on)
	    {
		System.out.println("text_length = "+text_length);
		System.out.println("text_width = "+text_width);
	    }
	innerPanel = new Panel();
	lmInner = new GridBagLayout();
	innerPanel.setLayout(lmInner);
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
	GridBagConstraints c = new GridBagConstraints();
	msgArea = new TextArea(message,text_length/2,text_width -5);
	msgArea.setEditable(false);
	innerPanel.add(msgArea);
	c.gridx = 0;
	c.gridy = 0;
	c.gridwidth = 5;
	lmInner.setConstraints(msgArea,c);
	yesButton = new Button("YES");
	innerPanel.add(yesButton);
	yesButton.addActionListener(this);
	c.gridx = 0;
	c.gridy = 1;
	c.gridwidth = 1;
	lmInner.setConstraints(yesButton,c);
	noButton = new Button("NO");
	innerPanel.add(noButton);
	c.gridx = 1;
	c.gridy = 1;
	c.gridwidth = 1;
	lmInner.setConstraints(noButton,c);
	noButton.addActionListener(this);
	yesToAllButton = new Button("YES TO ALL");
	innerPanel.add(yesToAllButton);
	c.gridx = 2;
	c.gridy = 1;
	c.gridwidth = 1;
	lmInner.setConstraints(yesToAllButton,c);
	yesToAllButton.addActionListener(this);
	noToAllButton = new Button("NO TO ALL");
	innerPanel.add(noToAllButton);
	c.gridx = 3;
	c.gridy = 1;
	c.gridwidth = 1;
	lmInner.setConstraints(noToAllButton,c);
	noToAllButton.addActionListener(this);
	cancelButton = new Button("CANCEL");
	innerPanel.add(cancelButton);
	c.gridx = 4;
	c.gridy = 1;
	c.gridwidth = 1;
	lmInner.setConstraints(cancelButton,c);
	cancelButton.addActionListener(this);
	add(innerPanel);
	setSize(d);
	if(debug_on)
	    {
		System.out.println("Showing QueryDialog with message = "+message);
	    }
    }

    /*
     *
     * This function is called after the user clicks any button or presses <ENTER>  in any TextField.
     */
    public void actionPerformed(ActionEvent evt)
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("QueryDialog.actionPerformed("+evt+")");
		    }
		if(evt.getSource() == yesButton)
		    {
			if(debug_on)
			    {
				System.out.println("yes");
			    }
			ok = true;
			cancel = false;
			done=true;
			dispose();
			return;
		    }

		if(evt.getSource() == noButton)
		    {
			if(debug_on)
			    {
				System.out.println("no");
			    }
			ok = false;
			done=true;
			dispose();
			return;
		    }

		if(evt.getSource() == yesToAllButton)
		    {
			if(debug_on)
			    {
				System.out.println("yes to all");
			    }
			ok = true;
			cancel = false;
			yes_to_all = true;
			no_to_all = false;
			done=true;
			dispose();
			return;
		    }
		if(evt.getSource() == noToAllButton)
		    {
			if(debug_on)
			    {
				System.out.println("no to all");
			    }
			ok = false;
			yes_to_all = false;
			no_to_all = true;
			done=true;
			dispose();
			return;
		    }
		if(evt.getSource() == cancelButton)
		    {
			if(debug_on)
			    {
				System.out.println("cancel");
			    }
			ok = false;
			cancel = true;
			done=true;
			dispose();
			return;
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
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
}
