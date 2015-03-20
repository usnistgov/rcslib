/*
 * PlotterCommon.java
 *
 * Created on December 31, 2006, 9:09 AM
 *
 *
 * The NIST RCS (Real-time Control Systems)
 * library is public domain software, however it is preferred
 * that the following disclaimers be attached.
 *
 * Software Copywrite/Warranty Disclaimer
 *
 *   This software was developed at the National Institute of Standards and
 * Technology by employees of the Federal Government in the course of their
 * official duties. Pursuant to title 17 Section 105 of the United States
 * Code this software is not subject to copyright protection and is in the
 * public domain. NIST Real-Time Control System software is an experimental
 * system. NIST assumes no responsibility whatsoever for its use by other
 * parties, and makes no guarantees, expressed or implied, about its
 * quality, reliability, or any other characteristic. We would appreciate
 * acknowledgement if the software is used. This software can be
 * redistributed and/or modified freely provided that any derivative works
 * bear some notice that they are derived from it, and any modified
 * versions bear some notice that they have been modified.
 *
 */

package diagapplet.plotter;

import java.awt.Color;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Random;

/**
 *
 * @author shackle
 */
class PlotterCommon
{
    
    /** Creates a new instance of PlotterCommon */
    public PlotterCommon()
    {
    }
    
    public static boolean debug_on=false;
    
    // Cooordinate types
    public static final int CARTESIAN_COORD_TYPE = 1;
    public static final int POLAR_COORD_TYPE = 2;
    
    static private Hashtable<String,Color> reverseColorsHashtable=null;
    static private Hashtable<Color,String> colorsHashtable=null;
    static private Enumeration<Color> colors_enum = null;
    
    static
    {
	init_colors_hashtable();
    }
    
    static private void init_colors_hashtable()
    {
	colorsHashtable = new Hashtable<Color,String>();
	colorsHashtable.put(Color.black,"black");
	colorsHashtable.put(Color.yellow.brighter(),"lightYellow");
	colorsHashtable.put(Color.red.brighter(),"lightRed");
	colorsHashtable.put(Color.blue.brighter(),"lightBlue");
	colorsHashtable.put(Color.red,"red");
	colorsHashtable.put(Color.cyan.brighter(),"lightCyan");
	colorsHashtable.put(Color.magenta.brighter(),"lightMagenta");
	colorsHashtable.put(Color.pink,"pink");
	colorsHashtable.put(Color.orange,"orange");
	colorsHashtable.put(Color.orange.brighter(),"lightOrange");
	colorsHashtable.put(Color.white,"white");
	colorsHashtable.put(Color.blue,"blue");
	colorsHashtable.put(Color.blue.darker(),"darkBlue");
	colorsHashtable.put(Color.red.darker(),"darkRed");
	colorsHashtable.put(Color.green,"green");
	colorsHashtable.put(Color.green.darker(),"darkGreen");
	colorsHashtable.put(Color.green.brighter(),"lightGreen");
	colorsHashtable.put(Color.cyan,"cyan");
	colorsHashtable.put(Color.cyan.darker(),"darkCyan");
	colorsHashtable.put(Color.magenta,"magenta");
	colorsHashtable.put(Color.magenta.darker(),"darkMagenta");
	colorsHashtable.put(Color.orange.darker(),"darkOrange");
	colorsHashtable.put(Color.yellow,"yellow");
	colorsHashtable.put(Color.yellow.darker(),"darkYellow");
	colorsHashtable.put(Color.darkGray,"darkGray");
	colorsHashtable.put(Color.gray,"gray");
	colorsHashtable.put(Color.lightGray,"lightGray");
	reverseColorsHashtable = new Hashtable<String,Color>();
	colors_enum= colorsHashtable.keys();
	while(colors_enum.hasMoreElements())
	    {
		Color c = colors_enum.nextElement();
		String n = colorsHashtable.get(c);
		reverseColorsHashtable.put(n,c);
	    }
    }

	static private Random r=null;

    public static Color nextColor()
    {
		if(null == r)
		{
			r =new Random(0);
		}
		return new Color(r.nextInt(255),r.nextInt(255),r.nextInt(255));
    }
    
    public static Color StringToColor(String name)
    {
	Color new_color = null;
	try
	{
	    if(null != reverseColorsHashtable)
	    {
		new_color = reverseColorsHashtable.get(name);
	    }
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
	try
	{
	    if(null == new_color)
	    {
		new_color = Color.getColor(name);
	    }
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
	return new_color;
    }
    
    static public void DebugPrint(String s)
    {
	try
	{
	    if(!debug_on)
	    {
		return;
	    }
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    System.out.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    static public void DebugPrint2(String s)
    {
	try
	{
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    System.out.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    
    static public void ErrorPrint(String s)
    {
	try
	{
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    if(debug_on)
	    {
		System.out.println("ErrorPrint + "+ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	    }
	    System.err.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
}
