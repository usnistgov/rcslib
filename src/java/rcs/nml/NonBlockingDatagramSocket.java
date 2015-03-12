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

import java.io.*;
import java.net.*;


class NonBlockingDatagramSocket extends DatagramSocket
{
  public DatagramPacket currentPacket = null;
  public DatagramPacket lastPacket = null;
  public Exception lastException = null;
  public long packets_received = 0;
  // Thread waitingThread = null;
  // long waitingThreadRunStartTime = 0;

  NonBlockingDatagramSocket() throws SocketException
  {
    super();
  }

  NonBlockingDatagramSocket(int port) throws SocketException
  {
    super(port);
  }

  int orig_timeout;

  public static boolean debug_on = false;

  public boolean ready(int timeout) throws Exception
  {
    //rcs.nml.debugInfo.debugPrintStream.println("currentPacket = "+currentPacket+",lastPacket = "+lastPacket);
    if(null == lastPacket && null == currentPacket)
      {
        return false;
      }
    if(null == currentPacket)
      {
        return true;
      }
    try
      {
        orig_timeout = getSoTimeout();
        setSoTimeout(timeout);
        super.receive(currentPacket);
        setSoTimeout(orig_timeout);
        lastPacket = currentPacket;
        currentPacket = null;
        packets_received++;
        if(debug_on)
          {
            rcs.nml.debugInfo.debugPrintStream.println("new packet "+packets_received);
          }
        return true;
      }
    catch(InterruptedIOException iioe)
      {
        setSoTimeout(orig_timeout);
        if(debug_on)
          {
              rcs.nml.debugInfo.debugPrintStream .println("timeout");
          }
        return false;
      }
  }


  /*
  public void run()
  {
    try
      {
        waitingThreadRunStartTime = System.currentTimeMillis();
        if(null == currentPacket)
          {
            return;
          }
        super.receive(currentPacket);
        lastPacket = currentPacket;
        waitingThread = null;
      }
    catch(Exception e)
      {
        lastException = e;
      }
  }
  */

  public void receive(DatagramPacket dp) throws IOException
  {
      if(currentPacket == null)
          {
	      currentPacket = dp;
          }
  }
}
