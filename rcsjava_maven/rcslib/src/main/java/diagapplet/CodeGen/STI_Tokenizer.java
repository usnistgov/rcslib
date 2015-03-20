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

import rcs.utils.StackTracePrinter;


class STI_Tokenizer implements STI_TokenizerInterface
{
    diagapplet.CodeGen.StructureTypeInfo sti=null;
    int token_number = 0;
    int count_since_no_skip=0;
    int array_indexes[] = null;
    int info_array_index = 0;

    static public void DebugPrint2(String s)
    {
	try
	     {
		 Throwable t = new Throwable();
		 System.out.println(StackTracePrinter.ThrowableToShortList(t)+" "+s);
		 System.out.println("time="+System.currentTimeMillis());
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
		 System.err.println(StackTracePrinter.ThrowableToShortList(t)+" "+s);
	     }
	 catch(Exception e)
	     {
		 e.printStackTrace();
	     }
     }    


    public boolean skipInfoTokenInSameArray()
    {
	if(count_since_no_skip < 2)
	    {
		//DebugPrint2("NOT skipping info_array_index="+info_array_index);
		return false;
	    }
	if(info_array_index >= sti.array_elem_info.length)
	    {
		//DebugPrint2("NOT skipping info_array_index="+info_array_index);
		count_since_no_skip=0;
		return false;
	    }
	info_array_elem_info iaei = sti.array_elem_info[info_array_index];
	if(iaei == null)
	    {
		//DebugPrint2("NOT skipping info_array_index="+info_array_index);
		count_since_no_skip=0;
		return false;
	    }
	if(!iaei.can_ndla_skip)
	    {
		//DebugPrint2("NOT skipping info_array_index="+info_array_index+"iaei="+iaei+", array_indexes="+ias(array_indexes));
		count_since_no_skip=0;
		return false;
	    }
	if(array_indexes[iaei.array_lengths.length-1] == 0)
	    {
		//DebugPrint2("NOT skipping info_array_index="+info_array_index+", iaei="+iaei+", array_indexes="+ias(array_indexes));
		count_since_no_skip=0;
		return false;
	    }
	if(info_array_index < sti.array_elem_info.length-1)
	    {
		info_array_elem_info iaei_p1 = sti.array_elem_info[info_array_index+1];
		if(null != iaei_p1 &&
		   iaei_p1.array_lengths.length >=  iaei.array_lengths.length &&
		   iaei_p1.first_new_section  > 0 &&
		   iaei_p1.first_new_section >= iaei.array_lengths.length)
		    {
			token_number++;
			//DebugPrint2("skipping info_array_index="+info_array_index+", iaei="+iaei+", array_indexes="+ias(array_indexes)+", iaei_p1="+iaei_p1+", token_number="+token_number);
			info_array_index++;
			return true;
		    }
	    }
	if(array_indexes[iaei.array_lengths.length-1] < 
	   iaei.array_lengths[iaei.array_lengths.length-1]-1)
	    {
		// info_array_elem_info iaei_p1 = null;
// 		if(info_array_index < sti.array_elem_info.length-1)
// 		    {
// 			iaei_p1 = sti.array_elem_info[info_array_index+1];
// 		    }
		token_number++;
		//DebugPrint2("skipping info_array_index="+info_array_index+", iaei="+iaei+", array_indexes="+ias(array_indexes)+", iaei_p1="+iaei_p1+", token_number="+token_number);
		info_array_index = iaei.return_indexes[iaei.array_lengths.length-1];
		array_indexes[iaei.array_lengths.length-1]++;
		return true;
	    }
	//DebugPrint2("NOT skipping info_array_index="+info_array_index+", iaei="+iaei+", array_indexes="+ias(array_indexes));
	count_since_no_skip=0;
	return false;
    }

    public boolean skipInfoTokenInSameStruct()
    {
	if(info_array_index >= sti.array_elem_info.length-1)
	    {
		//DebugPrint2("NOT Skipping "+info_array_index+" in same struct.  info_array["+info_array_index+"]="+info_array[info_array_index]+"\n");
		return false;
	    }
	if(sti.same_struct[info_array_index+1] && sti.same_struct[info_array_index])
	    {	
		token_number++;
		//DebugPrint2("Skipping "+info_array_index+" in same struct. info_array["+info_array_index+"]="+info_array[info_array_index]+", token_number="+token_number+"\n");
		info_array_index++;
		return true;
	    }
	//DebugPrint2("NOT Skipping "+info_array_index+" in same struct.  info_array["+info_array_index+"]="+info_array[info_array_index]+"\n");
	return false;
    }

    public String nextToken()
    {
	if(null == sti.array_elem_info)
	    {
		Thread.dumpStack();
		return null;
	    }
	if(info_array_index >= sti.array_elem_info.length)
	    {
		Thread.dumpStack();
		return null;
	    }
	//	int orig_info_array_index=info_array_index;
	count_since_no_skip++;
	info_array_elem_info iaei = sti.array_elem_info[info_array_index];
	if(iaei == null)
	    {
		String s = sti.info_array[info_array_index];
		info_array_index++;
		token_number++;
		if(null == s)
		    {
			Thread.dumpStack();
			ErrorPrint("sti.info_array["+info_array_index+"] = null, sti.info_array.length = "+sti.info_array.length);
		    }
		//DebugPrint2("s="+s+", info_array_index="+info_array_index+", array_indexes="+ias(array_indexes)+", token_number="+token_number);
		return s;
	    }
	String s = iaei.varType+" ";
	if(null != iaei.sections)
	    {
		for(int i = 0; i < iaei.sections.length; i++)
		    {
			if(iaei.sections[i] == null)
			    {
				break;
			    }
			s += iaei.sections[i] +"[" + array_indexes[i] + "]";
		    }
	    }
	s += iaei.tail;
	//DebugPrint2("info_array["+info_array_index+"]="+info_array[info_array_index]+", sti.array_elem_info["+info_array_index+"]="+sti.array_elem_info[info_array_index]+", s="+s);
	int min_diff_index = -1;
	info_array_index++;
	if(info_array_index <  sti.array_elem_info.length  &&
	   sti.array_elem_info[info_array_index] != null)
	    {
		min_diff_index = sti.array_elem_info[info_array_index].first_new_section-1;
	    }
	//DebugPrint2("min_diff_index="+min_diff_index+", iaei.array_lengths.length="+iaei.array_lengths.length);
	if(null != iaei.array_lengths)
	    {
		for(int i = iaei.array_lengths.length-1; i >= 0 && i > min_diff_index; i--)
		    {
			//DebugPrint2("array_indexes["+i+"]="+array_indexes[i]+", iaei.array_lengths["+i+"]="+iaei.array_lengths[i]);
			if(array_indexes[i] < iaei.array_lengths[i]-1)
			    {
				array_indexes[i]++;
				info_array_index = iaei.return_indexes[i];
				break;
			    }
			array_indexes[i] = 0;
		    }
	    }
	token_number++;
	//DebugPrint2("s="+s+", info_array_index="+info_array_index+", info_array["+orig_info_array_index+"]="+info_array[orig_info_array_index]+", array_indexes="+ias(array_indexes)+", orig_info_array_index="+orig_info_array_index+",  iaei="+iaei+", token_number="+token_number);
	return s;
    }


    public void throwAwayToken()
    {
	if(null == sti.array_elem_info)
	    {
		Thread.dumpStack();
		return;
	    }
	if(info_array_index >= sti.array_elem_info.length)
	    {
		Thread.dumpStack();
		return;
	    }
	//	int orig_info_array_index=info_array_index;
	count_since_no_skip++;
	info_array_elem_info iaei = sti.array_elem_info[info_array_index];
	if(iaei == null)
	    {
		String s = sti.info_array[info_array_index];
		info_array_index++;
		token_number++;
		if(null == s)
		    {
			Thread.dumpStack();
			ErrorPrint("sti.info_array["+info_array_index+"] = null, sti.info_array.length = "+sti.info_array.length);
		    }
		//DebugPrint2("s="+s+", info_array_index="+info_array_index+", array_indexes="+ias(array_indexes)+", token_number="+token_number);
		return;
	    }
	//String s = iaei.varType+" ";
	if(null != iaei.sections)
	    {
		for(int i = 0; i < iaei.sections.length; i++)
		    {
			if(iaei.sections[i] == null)
			    {
				break;
			    }
			//s += iaei.sections[i] +"[" + array_indexes[i] + "]";
		    }
	    }
	//s += iaei.tail;
	//DebugPrint2("info_array["+info_array_index+"]="+info_array[info_array_index]+", sti.array_elem_info["+info_array_index+"]="+sti.array_elem_info[info_array_index]+", s="+s);
	int min_diff_index = -1;
	info_array_index++;
	if(info_array_index <  sti.array_elem_info.length  &&
	   sti.array_elem_info[info_array_index] != null)
	    {
		min_diff_index = sti.array_elem_info[info_array_index].first_new_section-1;
	    }
	//DebugPrint2("min_diff_index="+min_diff_index+", iaei.array_lengths.length="+iaei.array_lengths.length);
	if(null != iaei.array_lengths)
	    {
		for(int i = iaei.array_lengths.length-1; i >= 0 && i > min_diff_index; i--)
		    {
			//DebugPrint2("array_indexes["+i+"]="+array_indexes[i]+", iaei.array_lengths["+i+"]="+iaei.array_lengths[i]);
			if(array_indexes[i] < iaei.array_lengths[i]-1)
			    {
				array_indexes[i]++;
				info_array_index = iaei.return_indexes[i];
				break;
			    }
			array_indexes[i] = 0;
		    }
	    }
	token_number++;
	//DebugPrint2("s="+s+", info_array_index="+info_array_index+", info_array["+orig_info_array_index+"]="+info_array[orig_info_array_index]+", array_indexes="+ias(array_indexes)+", orig_info_array_index="+orig_info_array_index+",  iaei="+iaei+", token_number="+token_number);
	return;
    }
    STI_Tokenizer(diagapplet.CodeGen.StructureTypeInfo  new_sti)
    {
	sti = new_sti;
	if(sti.array_lengths != null)
	    {
		array_indexes = new int[sti.array_lengths.length];
	    }
    }
    
    public boolean hasMoreTokens()
    {
	if(sti == null || sti.info_array == null || sti.array_elem_info == null)
	    {
		return false;
	    }
	//DebugPrint2("info_array_index="+info_array_index+", info_array.length="+ info_array.length+", array_indexes[0]="+array_indexes[0]);
	if(info_array_index >= sti.info_array.length)
	    {
		return false;
	    }
	if(info_array_index == sti.info_array.length-1 &&
	   null == sti.info_array[info_array_index])
	    {
		return false;
	    }
	return true;
    }
    
    public boolean hasMoreElements()
    {
	return hasMoreTokens();
    }

    public Object nextElement()
    {
	return nextToken();
    }
}
