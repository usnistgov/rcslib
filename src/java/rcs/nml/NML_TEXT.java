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
 * Class used to send an operator interface a small text message to display.
 * 
 * @author Will Shackleford 
 */
public class NML_TEXT extends NMLmsg
{
        public byte text[] = new byte[256];


        // Constructor
        public NML_TEXT()
        {
                super(2);
        }


        public void update(NMLFormatConverter nml_fc)
        {
                super.update(nml_fc);
                nml_fc.update(text,256);

        }
}
