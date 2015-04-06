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

import java.io.OutputStream;
import java.io.IOException;

import rcs.utils.CorrectedPipedOutputStreamInterface;
import rcs.utils.CorrectedPipedInputStreamInterface;

/**
* This class provides the same interface as java.io.PipedOutputStream
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
* @see          rcs.utils.CorrectedPipedInputStream
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*/
public class CorrectedPipedOutputStream extends OutputStream implements CorrectedPipedOutputStreamInterface
{
    //  CorrectedPipedInputStream in_stream = null;
  CorrectedPipeData pipe_data = null;
  static final boolean debug_on = false;

  // Constructors
  public CorrectedPipedOutputStream()
  {
      //in_stream = null;
    pipe_data = new  CorrectedPipeData();
  }

  public CorrectedPipedOutputStream(CorrectedPipedInputStreamInterface in)
  {
      //in_stream = in;
      pipe_data = in.get_pipe_data();
      //      in_stream.out_stream = this;
  }

  // Methods
    public CorrectedPipeData get_pipe_data()
    {
	return pipe_data;
    }

  public void close()
  {
  }

  public void write(int b) throws IOException
  {
    byte btemp[] = new byte[1];
    btemp[0] = (byte) b;
    write(btemp);
  }

  public void write(byte  b[])  throws IOException
  {
    write(b,0,b.length);
  }

  public void write(byte  b[], int  off, int  len)      throws IOException
  {
    if(debug_on)
      {
        System.out.println("CorrectedPipedOutputStream.write("+new String(b,off,len) +") called. off = "+off+", len = "+len);
      }
     if(len == 0)
      {
         pipe_data.PostNewData();
         return;
      }
      if(len < 0)
      {
            throw new IOException("len = "+len);
      }
    synchronized(pipe_data)
      {
        int old_bytes_left;
        int bytes_copied = 0;
        if(pipe_data.buffer == null)
          {
            pipe_data.buffer = new byte[((len/1024)+2)*1024];
            pipe_data.offset = 0;
            pipe_data.length = len;
            while(bytes_copied < len )
              {
                pipe_data.buffer[bytes_copied] =  b[bytes_copied+off];
                bytes_copied++;
              }
          }
        else
          {
            old_bytes_left = pipe_data.length - pipe_data.offset;
            if(pipe_data.length + len   > pipe_data.buffer.length)
              {
                byte temp_buffer[] = new byte[(((old_bytes_left + len)/1024)+2)*1024];
                while(pipe_data.offset < pipe_data.length)
                  {
                    temp_buffer[bytes_copied] =  pipe_data.buffer[pipe_data.offset];
                    pipe_data.offset++;
                    bytes_copied++;
                  }
                bytes_copied = 0;
                while(bytes_copied < len )
                  {
                    temp_buffer[bytes_copied + old_bytes_left] =  b[bytes_copied+off];
                    bytes_copied++;
                  }
                pipe_data.offset = 0;
                pipe_data.buffer = temp_buffer;
                pipe_data.length =      old_bytes_left + len;
              }
            else
              {
                bytes_copied = 0;
                while(bytes_copied < len )
                  {
                    pipe_data.buffer[bytes_copied+pipe_data.length] =  b[bytes_copied+off];
                    bytes_copied++;
                  }
                pipe_data.length += len;
              }
          }
        pipe_data.PostNewData();
      }
    if(debug_on)
      {
        System.out.println("CorrectedPipedOutputStream: pipe_data.buffer = "+new String(pipe_data.buffer,pipe_data.offset,pipe_data.length) +", pipe_data.offset  = "+pipe_data.offset+", pipe_data.length = "+pipe_data.length);
      }
  }
}
