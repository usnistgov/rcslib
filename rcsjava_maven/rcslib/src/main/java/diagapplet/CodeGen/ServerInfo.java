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


import java.util.*;


/**
 * Information about a server tracked only by Design tool.
 * @author Will Shackleford
 */
public class ServerInfo
{
  public String Name=null;
  public String Info=null;
  public String Host=null;
  public Vector bufferNames=null;
  public int id = 0;
  static public int count = 0;

  public String toString()
  {
    String str = super.toString()+" ServerInfo: { Name="+Name+", Info="+Info+", Host = "+Host+", id="+id+", count="+count+", bufferNames = [";
    if(null == bufferNames)
      {
        str += "null";
      }
    else
      {
        for(int i = 0; i < bufferNames.size() ; i++)
          {
            str += ((String) bufferNames.elementAt(i));
            if(i < bufferNames.size() - 1)
              {
                str += ", ";
              }
          }
      }
    str += "] } ";
    return str;
  }

  public ServerInfo(String _name, String _info)
  {
    count++;
    id = count;
    Name = _name;
    Info = _info;
    bufferNames = new Vector();
    ParseInfo();
  }

  protected void ParseInfo()
  {
    try
      {
        StringTokenizer lineTokenizer = new StringTokenizer(Info, "\r\n;");
        while(lineTokenizer.hasMoreTokens())
          {
            String line = lineTokenizer.nextToken();
            if(line == null)
              {
                break;
              }
            if(line.length() < 2)
              {
                continue;
              }
            int eq_index = line.indexOf('=');
            if(eq_index < 0 || eq_index >= line.length())
              {
                continue;
              }
            String var = line.substring(0,eq_index);
            String val = line.substring(eq_index+1);
            SetValue(var, val);
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

    // @SuppressWarnings("unchecked")
  protected void SetValue(String var, String val)
  {
    try
      {
        if(null == var || null == val)
          {
            return;
          }
        if(var.length() < 1 || val.length() < 1)
          {
            return;
          }
        char varc = var.charAt(0);
        while(varc == ' ' || varc == '\t' || varc == '\r' || varc == '\n' || varc == '\b')
          {
            var = var.substring(1);
            varc = var.charAt(0);
            if(var.length() < 1)
              {
                return;
              }
          }
        char valc = val.charAt(0);
        while(valc == ' ' || valc == '\t' || valc == '\r' || valc == '\n' || valc == '\b' || valc == '"')
          {
            val = val.substring(1);
            valc = val.charAt(0);
            if(val.length() < 1)
              {
                return;
              }
          }
        valc = val.charAt(val.length()-1);
        while(valc == ' ' || valc == '\t' || valc == '\r' || valc == '\n' || valc == '\b' || valc == '"')
          {
            val = val.substring(0,val.length()-1);
            valc = val.charAt(val.length()-1);
            if(val.length() < 1)
              {
                return;
              }
          }
        if(var.length() < 1 || val.length() < 1)
          {
            return;
          }
        var = var.toUpperCase();
        if(var.startsWith("HOST"))
          {
            Host = val;
          }
        else if(var.startsWith("BUF"))
          {
            if(null == bufferNames)
              {
                bufferNames = new Vector();
              }
            bufferNames.addElement(val);
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

}
