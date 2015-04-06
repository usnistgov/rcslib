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

/**
 * NMLMessageDictionaries allow the marshalling and unmarshalling of NML messages.
 * They are generally created using the CodeGenerator and therefore have type information
 * built in at compile-time. The Diag version must discover the type information at runtime.
 * 
 * This interface allows the CodeGen code to interact with a DiagNMLMsgDict without an 
 * explicit dependancy.
 * 
 * Many of the functions added relate to logging error messages.
 * 
 * The need to eliminate circular dependancies came from seperately compiling each java
 * file in a Makefile with a variety of compilers. It is likely no longer necessary
 * to eliminate the circular dependancies.
 * 
 * @see rcs.nml.NMLFormatConvertErrCallbackInterface
 * @author Will Shackleford
 */
public interface DiagNMLMsgDictInterface extends rcs.nml.NMLMessageDictionary
{
    /**
     * Used to set a ModuleInfo object, needed for this to function.
     * The non type safe use of an object parameter
     * @param mi new module info object
     */
    void SetModuleInfoObject(ModuleInfoInterface mi);
    
    /**
     * Returns the number of errors that have occured related to this dictionary.
     * incremented automatically.
     * @return failed_count.
     */
    int get_failed_count();
    
    /**
     * Used to log a warning message if the dictionary results do not
     * match the expected input.
     * @param num_tokens -- number tokens in input_string
     * @param input_string -- comma delimited string to be parsed
     * @param warn_given -- if already warned do not warn again.
     */
    void tokensNotUsed(int num_tokens, String input_string, boolean warn_given);
    
    /**
     * Used to log a warning message if the dictionary results do not
     * match the expected input.
     * @param bytes_in_input_stream -- number of bytes read 
     * @param bufName -- NML buffer name
     * @param warn_given -- if already warned do not warn again.
     */
    void bytesNotUsed(int bytes_in_input_stream, String bufName,boolean warn_given);
    
    /**
     * Log Information about this dictionary when generic NML error occurs.
     * called by NMLFormatConvertErrCallbackInterface 
     */
    void miscError();
    
    /**
     * Log Information about this dictionary when  NML error occurs with a related Exception
     * called by NMLFormatConvertErrCallbackInterface 
     * @param e exception to log
     */
    void miscError(Exception e);
}
