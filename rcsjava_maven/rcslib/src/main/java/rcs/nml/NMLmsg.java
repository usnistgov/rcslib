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


/**
* This is the abstract base class of all messages sent or
* recieved via NML.
 * 
* @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
*/
public abstract class NMLmsg implements Cloneable
{
        /**
        * Unique Identifier for the type of message
        */
        public int type = 0;

        /**
        * not used in Java
        */
        protected long size = 0;

    protected NMLmsg()
    {
    }

        /**
        * This constructor is to be used by derived types.
        *
        * Example:
        * <pre>
        * class MY_MSG extends NMLmsg
        * {
        *       int i;
        *       double d;
        *
        *       public MY_MSG()
        *       {
        *               super(1001);
        *       }
        * }
        * </pre>
        *
        * @param _type Unique Identifier for the type of message
        */
        public NMLmsg(int _type)
        {
                //rcs.nml.debugInfo.debugPrintStream .println("NMLmsg("+_type+") constructed.");
                type = _type;
                size = 0; // no sizeof operator in Java, so we can't do Raw Communications
                // like we could do in C++.
        }

        /**
        * This function should be overloaded to provide a function
        * that will convert this message to a nuetral format that can
        * be used, by many different types of hosts.
        *
        * @param nml_fc NMLFormatConverter that provides functions for updating
        * all of the basic data types.
        */
        public void update(rcs.nml.NMLFormatConverter nml_fc)
        {
        }
	
	public NMLmsg clone() throws CloneNotSupportedException
	{
	    NMLmsg clone_object = (NMLmsg) super.clone();
	    return clone_object;
	}
}
