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

import java.io.*;
import java.util.*;


/**
 * FilenameFilter which checks for a pattern match used by design tool.
 * 
 * @author Will Shackleford
 */
public class SimpleFileFilter implements FilenameFilter
{
        public String pattern;
  public static boolean debug_on = false;

        public SimpleFileFilter(String _pattern)
        {
                pattern = _pattern;
        }

        static public boolean CheckPatternMatch(String word, String pat)
        {
                String orig_word = word;
                StringTokenizer patTokens = new StringTokenizer(pat,"*");
                if(!pat.startsWith("*") && patTokens.hasMoreTokens())
                {
                        String start = patTokens.nextToken();
                        if(!word.startsWith(start))
                        {
                                return false;
                        }
                        word=word.substring(start.length());
                }
                String token = null;
                int token_index = -1;
                while(patTokens.hasMoreTokens())
                {
                         token = patTokens.nextToken();
                         token_index = word.indexOf(token);
                         if(token_index < 0)
                         {
                                 return false;
                         }
                         word = word.substring(token_index+token.length());
                }
                if(!pat.endsWith("*") && token != null)
                {
                        if(!orig_word.endsWith(token))
                        {
                                return false;
                        }
                }
                return true;
        }

    // Every class implementing FilenameFilter must have
        // this function.
        public boolean accept(File  dir, String  name)
        {
                boolean retval = CheckPatternMatch(name,pattern);
                if(debug_on)
                  {
                    System.out.println("SimpleFilenameFilter.accept: "+name+" matches "+pattern+": "+retval);
                  }
                return retval;
        }
}
