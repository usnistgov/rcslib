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

package diagapplet.CodeGen;

import java.util.Hashtable;
import java.util.Enumeration;

/**
 * A class that includes informataion on the various values and their assocated names
 * with any enumeration.
 * @author Will Shackleford
 */
public class EnumTypeInfo
{
    public String Name;
    public String Info;
    public String NameSpace="";
    public String CppQualifiedName="";
    public boolean insideNameSpace=false;
    public Hashtable hashtable  = null;
    public final Hashtable<String,Integer> reverse_hashtable ;
    public Hashtable override_names_hashtable = null;
    public boolean typedef=false;
    public boolean generate_symbol_lookup=false;
    
    public String toString()
    {
	String str = super.toString();
	if(typedef)
	    {
		str+= " /* typedef */ ";
	    }
	str += " enum "+Name+"{";
	try
	    {
		Enumeration keys = hashtable.keys();
		while(keys.hasMoreElements())
		    {
			
			Integer key = (Integer) keys.nextElement();
			String val = (String) hashtable.get(key);
			str+= val+"="+key+",";
		    }
		str+="};";
	    }
	catch(Exception e)
	    {
	    }
	return str;
    }
    
			    
  public EnumTypeInfo()
  {
      hashtable = new Hashtable();
      reverse_hashtable = new Hashtable<>();
      override_names_hashtable = new Hashtable();
  }
}
