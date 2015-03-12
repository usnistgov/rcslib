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

import java.util.Vector;

/*
 *
 * rcsdesignMainLoopInfo
 *
 */
 class rcsdesignMainLoopInfo
{
  private Vector modules = new Vector();
  public double cycle_time = 0.1;
  public String host = "localhost";
  public String Name = null;
  public static boolean debug_on = false;

  public void clearModules()
  {
    modules = new Vector();
    if(debug_on)
      {
        Thread.dumpStack();
      }
  }

  public Vector getModules()
  {
    return modules;
  }

  @SuppressWarnings("unchecked")
  public void addModule(String modName)
  {
    if(debug_on)
      {
        System.out.println("rcsdesignMainLoopInfo.addModule("+modName+")");
      }
    modules.addElement(modName);
  }

  public rcsdesignMainLoopInfo(String _name)
  {
    if(debug_on)
      {
        System.out.println("_name="+_name);
        Thread.dumpStack();
      }
    Name = _name;
  }

  public String toString()
  {
    String str = super.toString()+" rcsdesignMainLoopInfo { Name = "+Name+", host="+host+", cycle_time="+cycle_time+", modules=[";
    if(null != modules)
      {
        for(int i = 0; i < modules.size(); i++)
          {
            str +=(String)  modules.elementAt(i);
            if(i < modules.size() - 1)
              {
                str += ", ";
              }
          }
      }
    else
      {
        str += " null ";
      }
    str += "] } ";
    return str;
  }
}
