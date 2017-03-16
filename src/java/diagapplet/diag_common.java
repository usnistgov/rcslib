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


/*
 * diag_common.java
 *
 * Created on December 12, 2006, 6:36 PM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */

package diagapplet;

/**
 *
 * @author shackle
 */
class diag_common
{
    
    private static boolean debug_on=false;
    private static boolean interrupt_loading=false;
    private static boolean debug_mem=false;
    
    static public boolean get_debug_on()
    {
	return debug_on;
    }
    
    static void set_debug_on(boolean _debug_on)
    {
	debug_on = _debug_on;
    }
    
    static public boolean get_interrupt_loading()
    {
	return interrupt_loading;
    }
    
    static void set_interrupt_loading(boolean _interrupt_loading)
    {
	interrupt_loading = _interrupt_loading;
    }
    
    static public boolean get_debug_mem()
    {
	return debug_mem;
    }
    
    static void set_debug_mem(boolean _debug_mem)
    {
	debug_mem = _debug_mem;
    }
    
    static public void DebugPrint(String s)
    {
	try
	{
	    if(debug_on)
	    {
		Throwable t = new Throwable();
		StackTraceElement ste[] = t.getStackTrace();
		System.out.println(ste[1].getFileName()+":"+ste[1].getLineNumber()+" "+s);
	    }
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    static public void PrintMemUsage(String msg)
    {
	if(debug_mem)
	{
	    System.out.println(msg);
	    System.out.println("Free Memory :\t"+Runtime.getRuntime().freeMemory() +"\t:\t"+ (Runtime.getRuntime().freeMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().freeMemory()/1048576)+"M");
	    System.out.println("Total Memory :\t"+Runtime.getRuntime().totalMemory() +"\t:\t"+ (Runtime.getRuntime().totalMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().totalMemory()/1048576)+"M");
	    System.out.println("Max Memory :\t"+Runtime.getRuntime().maxMemory() +"\t:\t"+ (Runtime.getRuntime().maxMemory()/1024)+"k\t:\t"+ (Runtime.getRuntime().maxMemory()/1048576)+"M");
	    long mem_used = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
	    System.out.println("Used Memory :\t"+mem_used +"\t:\t"+ (mem_used/1024)+"k\t:\t"+ (mem_used/1048576)+"M");
	}
    }
    static public void ErrorPrint(String s)
    {
	try
	{
	    Throwable t = new Throwable();
	    StackTraceElement ste[] = t.getStackTrace();
	    if(ste.length > 2)
	    {
		System.err.println("ERROR: "+
			ste[2].getClassName()+"."+ste[2].getMethodName()+"("+ste[2].getFileName()+":"+ste[2].getLineNumber()+") "+
			ste[1].getClassName()+"."+ste[1].getMethodName()+"("+ste[1].getFileName()+":"+ste[1].getLineNumber()+") "+
			" "+s);
	    }
	    else
	    {
		System.err.println("ERROR: "+
			ste[1].getClassName()+"."+ste[1].getMethodName()+"("+ste[1].getFileName()+":"+ste[1].getLineNumber()+") "+
			" "+s);
	    }
	}
	catch(Exception e)
	{
	    e.printStackTrace();
	}
    }
    
    
    /** Creates a new instance of diag_common */
    private diag_common()
    {
    }
    
}
