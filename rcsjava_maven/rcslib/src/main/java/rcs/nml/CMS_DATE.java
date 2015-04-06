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
public class CMS_DATE
{
    public long years=1971;
    public long months=1;
    public long days=16;

    public CMS_DATE()
    {
    }

    public CMS_DATE(String str)
    {
	setWithString(str);
    }
    
    public void setWithString(String str)
    {
	try
	    {
		if(str == null)
		    {
			return;
		    }
		StringTokenizer st = new StringTokenizer(str,"-");
		if(st == null)
		    {
			return;
		    }
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		String tok = st.nextToken();
		years = Long.parseLong(tok);
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		tok = st.nextToken();
		months = Long.parseLong(tok);
		if(!st.hasMoreTokens())
		    {
			return;
		    }
		tok = st.nextToken();
		days = Long.parseLong(tok);
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

	
    public void update(NMLFormatConverter nml_fc)
    {
	nml_fc.update_CMS_DATE(this);
    }
    
    public void update_for_non_xml(NMLFormatConverter nml_fc)
    {
	nml_fc.beginClass("CMS_DATE",null);
	years = nml_fc.update_with_name("years",years);
	months = nml_fc.update_with_name("months",months);
	days = nml_fc.update_with_name("days",days);
	nml_fc.endClass("CMS_DATE",null);
    }

    public String toString()
    {
	String ystr="";
	ystr = ""+years;
	String mstr = ""+months;
	if(months < 10)
	    {
		mstr="0"+mstr;
	    }
	String daystr = ""+days;
	if(days < 10)
	    {
		daystr = "0"+daystr;
	    }
	return ""+ystr+"-"+mstr+"-"+daystr;
    }
}
