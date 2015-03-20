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

/**
* Contains information used by CorrectedPipedInputStream and
* CorrectedPipedOutputStream.
*
* <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
*
* </pre>
*
* @see          rcs.utils.CorrectedPipedInputStream
* @see          rcs.utils.CorrectedPipedOutputStream
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*/
class CorrectedPipeData
{
        byte buffer[] = null;
        int offset = 0;
        int length = 0;
  int wait_count = 0;
  int notify_count = 0;

  public synchronized void WaitForData() throws Exception
  {
    if(notify_count > wait_count)
      {
        wait_count = notify_count;
        return;
      }
      try
      {
        wait();
      }
      catch(Exception e)
      {
          System.err.println("notify_count="+notify_count+", wait_count="+wait_count+",length="+length+", offset="+offset+",buffer.length="+buffer.length);
          throw e;
      }
  }

  public synchronized void  PostNewData()
  {
    if(notify_count < wait_count)
      {
        notify_count = wait_count;
      }
    notify_count++;
    notifyAll();
  }
}
