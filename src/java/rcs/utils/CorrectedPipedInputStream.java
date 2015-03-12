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

package rcs.utils;


import java.io.InputStream;
import java.io.IOException;

import rcs.utils.CorrectedPipedOutputStreamInterface;
import rcs.utils.CorrectedPipeData;

/**
* This class provides the same interface as java.io.PipedInputStream
* except that it corrects the problem that when java.io.PipedOutputStream
* has 1k or more written to it, it blocks until the some of the data is read
* from the input pipe before more can be written.
* CorrectedPipedInputStream/CorrectedPipedOutputStream only block
* for mutual exclusion but will allow any amount of data(atleast
* until you run out of memory) to be written to the pipe without waiting
* for a read.
*
* <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
*
*
* </pre>
*
* @see          rcs.utils.CorrectedPipedOutputStream
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*/
public class CorrectedPipedInputStream extends InputStream implements rcs.utils.CorrectedPipedInputStreamInterface
{
    //CorrectedPipedOutputStream out_stream = null;
    CorrectedPipeData pipe_data = null;
    static final public boolean debug_on = false;
    
    // Constructors
    public CorrectedPipedInputStream()
    {
	//out_stream = null;
	pipe_data = new  CorrectedPipeData();
    }
    
    public CorrectedPipedInputStream(CorrectedPipedOutputStreamInterface out)
    {
	//out_stream = out;
	pipe_data = out.get_pipe_data();
	//out_stream.in_stream = this;
    }

    public CorrectedPipeData get_pipe_data()
    {
	return pipe_data;
    }

    // Methods
    public int available()
    {
	if(null == pipe_data)
	    {
		return 0;
	    }
	if(null == pipe_data.buffer)
	    {
		return 0;
	    }
	if( pipe_data.length > pipe_data.buffer.length)
	    {
		return 0;
	    }
	if(pipe_data.offset < pipe_data.length)
	    {
		return 0;
	    }
	return pipe_data.length - pipe_data.offset;

    }

    public void close()
    {
    }

    public int read()   throws IOException
    {
	if(null == pipe_data)
	    {
		throw new IOException();
	    }
	while(true)
	    {
		if(null != pipe_data.buffer)
		    {
			if(pipe_data.buffer.length > 0 &&
			   pipe_data.offset < pipe_data.length)
			    {
				break;
			    }
		    }
		try
		    {
			pipe_data.WaitForData();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
			throw new IOException();
		    }
	    }
	if(pipe_data.offset < 0)
	    {
		throw new IOException();
	    }
	if(pipe_data.offset > pipe_data.length)
	    {
		throw new IOException();
	    }
	if(pipe_data.length > pipe_data.buffer.length)
	    {
		throw new IOException();
	    }
	int retval = 0;
	synchronized(pipe_data)
	    {
		retval = (int)  pipe_data.buffer[pipe_data.offset];
		if(retval < 0)
		    {
			retval += 256;
		    }
		pipe_data.offset++;
	    }
	pipe_data.PostNewData();
	if(debug_on)
	    {
		System.out.println("CorrectedPipedInputStream.read() returning "+retval+". (off = "+pipe_data.offset+", len = "+pipe_data.length+")");
	    }
	return retval;
    }

    public int read(byte  b[]) throws IOException
    {
	return read(b,0,b.length);
    }

    public int read(byte  b[], int  off, int  len)      throws IOException
    {
	int bytes_read = 0;
	if(debug_on)
	    {
		System.out.println("CorrectedPipedInputStream: pipe_data.buffer = "+new String(pipe_data.buffer,off,len) +", pipe_data.offset  = "+pipe_data.offset+", pipe_data.length = "+pipe_data.length);
	    }
        if(len == 0)
        {
            return 0;
        }
        if(len < 0)
        {
            throw new IOException("len = "+len);
        }
	if(null == pipe_data)
	    {
		throw new IOException();
	    }
	while(true)
	    {
		if(null != pipe_data.buffer)
		    {
			if(pipe_data.buffer.length > 0 &&
			   pipe_data.offset < pipe_data.length)
			    {
				break;
			    }
		    }
		try
		    {
			pipe_data.WaitForData();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
			throw new IOException();
		    }
	    }
	if(pipe_data.offset < 0)
	    {
		throw new IOException();
	    }
	if(pipe_data.offset > pipe_data.length)
	    {
		throw new IOException();
	    }
	if(pipe_data.length > pipe_data.buffer.length)
	    {
		throw new IOException();
	    }
	synchronized(pipe_data)
	    {
		while(bytes_read < len && pipe_data.offset < pipe_data.length)
		    {
                        b[bytes_read+off] =  pipe_data.buffer[pipe_data.offset];
                        pipe_data.offset++;
                        bytes_read++;
		    }
	    }
	pipe_data.PostNewData();
	if(debug_on)
	    {
		System.out.println("CorrectedPipedInputStream: b = "+new String(b,off,len) +", off  = "+off+", len = "+len+", bytes_read = "+bytes_read);
	    }

	return bytes_read;
    }
}
