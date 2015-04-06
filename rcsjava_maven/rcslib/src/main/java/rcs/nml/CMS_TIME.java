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


package rcs.nml;

import java.util.StringTokenizer;

/**
 * Date structure wih built in NML update and 
 * a special update format for XML.
 * 
 * @author Will Shackleford 
 */
public class CMS_TIME 
{
    public long hours=1;
    public long minutes=0;
    public double seconds=0;
    int timezoneoffsethours=0;

    public CMS_TIME()
    {
    }
    

    public CMS_TIME(String str)
    {
	setWithString(str);
    }
    
    public void setWithString(String str)
    {
	try
	    {
		if(null == str)
		    {
			return;
		    }
		StringTokenizer st = new StringTokenizer(str,":");
		if(st == null)
		    {
			return;
		    }
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		String tok = st.nextToken();
		hours = Long.parseLong(tok);
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		tok = st.nextToken();
		minutes = Long.parseLong(tok);
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		tok = st.nextToken();
		seconds = Double.parseDouble(tok);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void update(NMLFormatConverter nml_fc)
    {
	nml_fc.update_CMS_TIME(this);
    }
    
    public void update_for_non_xml(NMLFormatConverter nml_fc)
    {
	nml_fc.beginClass("CMS_TIME",null);
	hours = nml_fc.update_with_name("hours",hours);
	minutes = nml_fc.update_with_name("minutes",minutes);
	seconds = nml_fc.update_with_name("seconds",seconds);
	timezoneoffsethours = nml_fc.update_with_name("timezoneoffsethours",timezoneoffsethours);
	nml_fc.endClass("CMS_TIME",null);
    }

    public String toString()
    {
	String hstr = ""+hours;
	if(hours < 10)
	    {
		hstr = "0"+hstr;
	    }
	String mstr = ""+minutes;
	if(minutes < 10)
	    {
		mstr = "0"+mstr;
	    }
	String sstr = ""+seconds;
	if(seconds < 10)
	    {
		sstr = "0"+sstr;
	    }
	if(sstr.endsWith(".0"))
	    {
		sstr = sstr.substring(0,sstr.length()-2);
	    }
	return ""+hstr+":"+mstr+":"+sstr;
    }
}
