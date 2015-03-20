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

class info_array_elem_info
{
    String varType=null;
    String tail=null;
    String sections[] = null;
    int array_lengths[] = null;
    int return_indexes[] = null;
    boolean array_ends[] = null;
    int first_new_section=-1;
    boolean is_char_array;
    boolean last_array_was_char_array;
    boolean can_ndla_skip;

    static String ias(int ia[])
    {
	if(null == ia)
	    {
		return "{{!null!}}";
	    }
	String s="{";
	for(int i = 0; i < ia.length-1;i++)
	    {
		s+= ia[i]+",";
	    }
	s += ia[ia.length-1]+"}";
	return s;
    }

    static String sas(String sa[])
    {
	if(null == sa)
	    {
		return "{{!null!}}";
	    }
	String s="{";
	for(int i = 0; i < sa.length-1;i++)
	    {
		if(null == sa[i])
		    {
			s += "} sa.length="+sa.length;
			return s;
		    }
		s+= sa[i]+",";
	    }
	if(null == sa[sa.length-1])
	    {
		s += "} sa.length="+sa.length;
		return s;
	    }
	s += sa[sa.length-1]+"}";
	return s;
    }

    public String toString()
    {
	return "varType="+varType+", sections="+sas(sections)+", array_lengths="+ias(array_lengths)+", return_indexes="+ias(return_indexes)+", tail="+tail+", first_new_section="+first_new_section+", is_char_array="+is_char_array+", last_array_was_char_array="+last_array_was_char_array;
    }
}
