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

/*
 *
 * QueryDialog
 *
 */
class QueryStringDialog extends Dialog implements ActionListener
{

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613920L;


    GridBagLayout lmInner = null;
    Panel innerPanel = null;
    TextArea msgArea = null;
    TextField queryField = null;
    Button okButton = null;
    FlowLayout lm = null;
    public boolean done = false;
    public boolean ok = false;
    static public boolean cancel = false;
    static public boolean debug_on = false;
    String queryString = "";

    Dimension d = new Dimension(480,240);

    QueryStringDialog(Frame parent, String title, String message, String default_string)
    {
	super(parent, title, true);
	setSize(d);
	lm = new FlowLayout(FlowLayout.LEFT);
	setLayout(lm);
	int text_length = 10;
	int text_width = 40;

	try
	    {
		Font f = getFont();
		if(null == f)
		    {
			Font smallFont = null;
			smallFont = new Font("TimesRoman",Font.PLAIN, 12);
			setFont(smallFont);
			f = getFont();
		    }
		int font_height = 10;
		int font_width = 6;
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
				font_width = fm.stringWidth("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")/62;
				if(font_width < 6)
				    {
					font_width = 6;
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
	GridBagConstraints c = new GridBagConstraints();
	msgArea = new TextArea(message,text_length/2,text_width -5);
	msgArea.setEditable(false);
	innerPanel.add(msgArea);
	c.gridx = 0;
	c.gridy = 0;
	c.gridwidth = 3;
	c.anchor = GridBagConstraints.NORTHWEST;
	c.fill = GridBagConstraints.HORIZONTAL;
	lmInner.setConstraints(msgArea,c);
	if(null == default_string)
	    {
		default_string  = "";
	    }
	queryField = new TextField(default_string,20);
	innerPanel.add(queryField);
	c.gridx = 0;
	c.gridy = 1;
	c.gridwidth = 3;
	lmInner.setConstraints(queryField,c);
	queryField.requestFocus();
	queryField.addActionListener(this);

	okButton = new Button("OK");
	innerPanel.add(okButton);
	c.gridx = 0;
	c.gridy = 2;
	c.gridwidth = 1;
	c.fill = GridBagConstraints.NONE;
	lmInner.setConstraints(okButton,c);
	okButton.addActionListener(this);
	add(innerPanel);
	setSize(d);
	if(debug_on)
	    {
		System.out.println("Showing QueryStringDialog with message = "+message);
	    }
	setVisible(true);
    }

    public void actionPerformed(ActionEvent evt)
    {
	if(evt.getSource()  == okButton || evt.getSource() == queryField)
            {
		ok = true;
		done = true;
		queryString = queryField.getText();
		dispose();
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



}
