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
 * Interface for classes to be called when NML errors occur during formatting to provide
 * more informative error messages.
 * 
 * @author Will Shackleford 
 */
public interface NMLFormatConvertErrCallbackInterface
{
    public void tokensNotUsed(NMLMessageDictionary nmd, int num_tokens, String input_string, boolean warn_given);    
    public void bytesNotUsed(NMLMessageDictionary nmd, int bytes_in_input_stream, String bufName,boolean warn_given);
    public void miscError(NMLMessageDictionary nmd);
    public void miscError(NMLMessageDictionary nmd, Exception e);
}
