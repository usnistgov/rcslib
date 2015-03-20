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


class AutoSizedTextArea extends TextArea
{

    public static boolean debug_on = false;

    /**
     *   Unique id for this class.
     * see documentation
     * for Serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613917L;

    AutoSizedTextArea(String txt, int width, int height, int maxcols, int maxrows)
    {
	super(txt);
	Frame tempFrame = new Frame();
	tempFrame.add(this);
	tempFrame.pack();
	if(debug_on)
	    {
		System.out.println("AutoSizedTextArea(String txt="+txt+", int width="+width+", int height="+height+")");
	    }
	int cols = width/5;
	int rows = height/5;
	if(cols > maxcols)
	    {
		cols = maxcols;
	    }
	if(rows > maxrows)
	    {
		rows = maxrows;
	    }
	Dimension dm = getMinimumSize(rows,cols);
	Dimension dp = getPreferredSize(rows,cols);
	if(debug_on)
	    {
		System.out.println("cols="+cols+", rows="+rows+", dp="+dp+", dm="+dm);
	    }
	while((dm.width > width || dp.width  > width) && cols > 10)
	    {
		int bad_width = dp.width;
		if(dm.width > dp.width)
		    {
			bad_width = dm.width;
		    }
		int diff = (bad_width - width)/20;
		if(diff < 1)
		    {
			diff = 1;
		    }
		cols -= diff;
		dm = getMinimumSize(rows,cols);
		dp = getPreferredSize(rows,cols);
		if(debug_on)
		    {
			System.out.println("cols="+cols+", rows="+rows+", dp="+dp+", dm="+dm);
		    }
	    }
	if(debug_on)
	    {
		System.out.println("AutoSizedTextArea():   setColumns("+cols+");");
	    }
	setColumns(cols);
	while((dm.height > height || dp.height  > height) && rows > 2)
	    {
		int bad_height = dp.height;
		if(dm.height > dp.height)
		    {
			bad_height = dm.height;
		    }
		int diff = (bad_height - height)/20;
		if(diff < 1)
		    {
			diff = 1;
		    }
		rows -= diff;
		dm = getMinimumSize(rows,cols);
		dp = getPreferredSize(rows,cols);
		if(debug_on)
		    {
			System.out.println("cols="+cols+", rows="+rows+", dp="+dp+", dm="+dm);
		    }
	    }
	if(debug_on)
	    {
		System.out.println("AutoSizedTextArea():   setRows("+rows+");");
	    }
	setRows(rows);
	if(debug_on)
	    {
		System.out.println("cols="+cols+", rows="+rows+", dp="+dp+", dm="+dm);
		//System.out.println("AutoSizedTextArea():   tempFrame.dispose();");
	    }
	//tempFrame.dispose();
	if(debug_on)
	    {
		System.out.println("AutoSizedTextArea() constructor complete.\n");
	    }
    }
}
