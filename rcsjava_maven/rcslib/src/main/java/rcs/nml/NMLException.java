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
* The base class for all exceptions thrown by NML.
*/
public class NMLException extends Exception
{
    /**
     * The buffer name of the NMLConnection that was being used,
     * when the exception was thrown.
     */
    public String buffer_name =null;

    /**
     * The configuration file read by the NMLConnection that was being used,
     * when the exception was thrown.
     */
    public String config_file =null;

    /**
     * If an exception was thrown by some function called from within
     * NML such as a socket call, the exception will be caught and
     * wrapped in an NMLException and then thrown.
     */
    public Exception internal_exception = null;


    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613893L;
    
    public NMLConnectionInterface nci=null;
    
    public String buffer_line = null;

    //@Override
    public Throwable getCause()
    {
        return this.internal_exception;
    }
    
    public NMLException(String _message, NMLConnectionInterface _nci)
    {
        super(_message +"\nNMLConnection Description:\n"+_nci.toString()+"\nEnd NMLConnection Description.\n");
        this.nci = _nci;
        this.buffer_line = _nci.getBufferLine();
        this.buffer_name = _nci.get_buffer_name();
        this.config_file = _nci.get_configuration_file();
        
    }

    public NMLException(String _message, NMLConnectionInterface _nci, Exception _internal_exception)
    {
        super(_message +"\nNMLConnection Description:\n"+_nci.toString()+"\nEnd NMLConnection Description.\n");
        this.nci = _nci;
        this.internal_exception = _internal_exception;
        this.buffer_line = _nci.getBufferLine();
        this.buffer_name = _nci.get_buffer_name();
        this.config_file = _nci.get_configuration_file();
        
    }

//    public NMLException(String _message, String _buffer_name, String _config_file, String _buffer_line)
//    {
//	super(_message+"\n\tbuffer_name="+_buffer_name+",\n\tconfig_file="+_config_file+",\n\tbuffer_line="+_buffer_line);
//	buffer_name = _buffer_name;
//	config_file = _config_file;
//	buffer_line = _buffer_line;
//	internal_exception = null;
//    }
//    
//    
//
//    public NMLException(String _message, String _buffer_name, String _config_file, Exception _internal_exception)
//    {
//	super(_message+"\n\tbuffer_name="+_buffer_name+",\n\tconfig_file= "+_config_file);
//	buffer_name = _buffer_name;
//	config_file = _config_file;
//	internal_exception = _internal_exception;
//    }

}
