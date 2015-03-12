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


package rcs.posemath;

public class PmException extends Exception
{

    public static final int PM_DIV_ERR  = -4;
    public static final int PM_ERR  = -1;
    public static final int PM_IMPL_ERR  = -2;
    public static final int PM_NORM_ERR  = -3;

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613894L;

    protected static String GetErrorString(int errno)
    {
	if(errno ==  PM_ERR)
	    {
		return "Unspecified Error";
	    }
	else if(errno == PM_IMPL_ERR)
	    {
		return "Function Not Implemented";
	    }
	else if(errno ==  PM_NORM_ERR)
	    {
		return "Argument Should have been normalized";
	    }
	else if(errno == PM_DIV_ERR)
	    {
		return "divide by zero attempted";
	    }
	else if(errno == 0)
	    {
		return "no error";
	    }
	return "illegal value for pmErrno ("+errno+")";
    }

    public int pmErrno = 0;
    public PmException(int errno, String msg)
    {
	super(GetErrorString(errno)+": "+msg);
	pmErrno = errno;
    }
}
