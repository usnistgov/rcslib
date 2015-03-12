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

package rcsdesign;

import diagapplet.utils.StandAloneApplet;


class InnerAppletRepainter implements Runnable
{
  StandAloneApplet innerApplet = null;
  Thread repainter_thread = null;
  long interval = 10;
  long timeout = 1000;

  boolean quit_flag = false;

  public void start()
  {
    quit_flag = false;
    repainter_thread = new Thread(this);
    repainter_thread.start();
  }

  public void stop()
  {
    quit_flag = true;
    if(null != repainter_thread)
      {
        repainter_thread.interrupt();
        repainter_thread = null;
      }
  }
  public void run()
  {
    long start_time = System.currentTimeMillis();
    while(innerApplet.repaint_count > 0
          && System.currentTimeMillis() -start_time < timeout
          && !repainter_thread.isInterrupted() && !quit_flag)
      {
        innerApplet.repaint();
        try
          {
            Thread.sleep(interval);
          }
        catch(InterruptedException ie)
          {
            break;
          }
        catch(Exception e)
          {
            e.printStackTrace();
          }
      }
  }

  InnerAppletRepainter(StandAloneApplet inner_applet, long _interval, long _timeout)
  {
    innerApplet = inner_applet;
    interval = _interval;
    timeout = _timeout;
  }

}
