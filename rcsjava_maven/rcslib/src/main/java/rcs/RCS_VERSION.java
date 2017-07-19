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

/* RCS_VERSION.java
* Provides a means for Java Applications to check the version of the
* RCS Library being used.
* The install_rcs script uses sed to replace 2016.11.21 in
* RCS_VERSION.java.perm  --> RCS_VERSION.java
* file with the appropriate version number. This is to provide consistency
* in the version numbers between Java and C++.
*/


package rcs;

import java.util.StringTokenizer;

/**
* Class for checking at run-time which version of the RCS Library
* is being used.
* <pre>
* Related Documentation:
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>,
* <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/rcsvers.html">RCS Version Functions</a>
*
* Source Code:
* <A HREF="RCS_VERSION.java">RCS_VERSION.java</a>
*
* </pre>
*
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*
*/
public class RCS_VERSION extends Object
{
        /**
        * String in which the version number 2016.11.21 is stored.
        */
        public static final String version_string =  "2016.11.21";

        /**
        * String in which the library was compiled is stored.
        */
        public static final String date_string =  "Wed Jul 19 11:11:13 EDT 2017";

        /**
        * String in which a brief message describing the library, it's version
        * and compile date etc. is stored.
        */
        public static final String info_string = "@(#)" + " RCS_LIBRARY_VERSION: 2016.11.21 Compiled on "+ "Wed Jul 19 11:11:13 EDT 2017" +" for the java platform.\n";

        /**
        * Function that returns the info_string.
        *
        * @return info_string
        */
        public String toString()
        {
                return super.toString() + info_string;
        }

        /**
        * Function that prints the info_string to System.out
        */
        static public void print()
        {
                System.out.println(info_string);
        }

        /**
        * Running this class as a stand-alone application displays
        * the RCS Library version information and the Java System
        * properties.
        *
        * @param args  NOT used.
        */
        static public void main(String args[])
        {
                System.out.println("RCS Library Properties:");
                print();
        }


        /**
        * Compare the version of the library with some other version.
        * If the argument only includes a major version number, the
        * minor version number is ignored.
        *
        * @param str_to_compare A version to compare with the library being used.
        *
        * @return 0 if the versions are match, a negative number if
        *       the library is older than the version passed as an argument,
        *       or a positive number if the library is newer than the
        *       version passed as an argument.
        */
        static public int compare(String str_to_compare)
        {
                try
                {
                        StringTokenizer compare_tokenizer = new StringTokenizer(str_to_compare,".");
                        StringTokenizer version_tokenizer = new StringTokenizer(version_string,".");
                        while(version_tokenizer.hasMoreTokens() && compare_tokenizer.hasMoreTokens())
                        {

                                String version_token = version_tokenizer.nextToken();
                                String compare_token = compare_tokenizer.nextToken();
                                int version_number = Integer.valueOf(version_token).intValue();
                                int compare_number = Integer.valueOf(compare_token).intValue();
                                if(version_number - compare_number != 0)
                                {
                                        return version_number-compare_number;
                                }
                        }
                }
                catch(Exception e)
                {
                        e.printStackTrace();
                }
                return 0;
        }
}
