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

/**
 * The class is used to store the value a single variable  extracted
 * from a large or potentially large message and that was automatically
 * collected at some frequency.
 * 
 * @author Will Shackleford
 */
public class NMLSingleVarLog
{
    public rcs.nml.NMLSingleVarLogItem items_list[] = null;
    public int last_items_sent_size=0;

    public String toString()
    {
	String str = super.toString() +" , last_items_sent_size="+last_items_sent_size+" ,  items_list="+items_list+"\n";
	if(items_list == null)
	    {
		return str; 
	    }
	else
	    {
		for(int i = 0 ; i < items_list.length && i < last_items_sent_size; i++)
		    {
			if(items_list[i] != null)
			    {
				str+= "items_list["+i+"].timestamp="+items_list[i].timestamp+" , "+"items_list["+i+"].value="+items_list[i].value+"\n";
			    }
		    }
	    }
	return str;
    }
    

};
