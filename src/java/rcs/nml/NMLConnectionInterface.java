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
 * An interface to NMLConnection used by CodeGenenerator and diagnostics tool to
 * avoid circular dependencies.
 * @author Will Shackleford 
 */
public interface NMLConnectionInterface
{
    /**
     * Get the time in milliseconds when the last connect() was attempted.
     * (To determin how long it has been since the last connection subtract 
     * from the current time given with System.currentTimeMillis();
     * @return connect_time
     */
    public long get_connect_time();
    
    /**
     * Get the time in milliseconds when the last disconnect() was attempted.
     * (To determin how long it has been since disconnected subtract
     * from the current time given with System.currentTimeMillis();
     * @return disconnect_time
     */
    public long get_disconnect_time();
    
    /**
     * When the NMLConnection performs a verify bufname (which it normally
     * does in the constructor) it requests the buffername from 
     * the server that corresponds to the selected port and buffer number.
     * If this is not the same as the buffer passed to the constructor it indicates
     * that configuration used by the server is inconsistant with the one being used by this process.
     * Regardless the value is stored and can be retrieved later with
     * get_returned_buffer_name().
     * @return returned_buffer_name
     */
    public String get_returned_buffer_name();
    
    
    /**
     * When the NMLConnection performs a verify bufname (which it normally
     * does in the constructor) it requests the buffername from 
     * the server that corresponds to the selected port and buffer number.
     * If this is not the same as the buffer passed to the constructor it indicates
     * that configuration used by the server is inconsistant with the one being used by this process.
     * @return true if the host could be contacted and returned the expected buffername, or false if the 
     * server could not be contacted or the buffernames do not match.
     */
    public boolean verify_bufname();
    
    /**
     * Get the host name where the buffer is supposed to be located.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return host
     */
    public String get_host();
    
    
    /**
     * Set the host name where the buffer is supposed to be located which 
     * will be used for the next connection attempt. Either host names or IP addresses
     * can be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param s new hostname
     */
    public void set_host(String s);
    
    
    /**
     * Return the TCP or UDP port number associated with this connection.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return port number
     */
    public int get_port();
    
    /**
     * Set the TCP or UDP port to be used for the next time a connect() is performed.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param _port new port number
     */
    public void set_port(int _port);
    
    
    /**
     * Get the buffer_number for this buffer.
     * The buffer number is sent with each request to the server to identify which buffer
     * should be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return buffer_number
     */
    public int get_buffer_number();
    
    /**
     * Set the buffer_number for this buffer.
     * The buffer number is sent with each request to the server to identify which buffer
     * should be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param _buffer_number new buffer number
     */
    public void set_buffer_number(int _buffer_number);
    
    /**
     * Get the name of the NML configuration file being used.
     * @return configuration_file
     */
    public String get_configuration_file();
    
    /**
     * Set the name of the NML configuration file.
     * This should be followed with ReadNMLConfigurationFileNoThrow() to actually have
     * the configration file read. 
     * @param _configuration_file new configuration file
     */
    public void set_configuration_file(String _configuration_file);
    
    
    /**
     * Get the buffer name.
     * 
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @return buffer_name
     */
    public String get_buffer_name();
    
    /**
     * Set the buffer name.
     * 
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     *
     * @param _buffer_name new buffer name
     */
    public void set_buffer_name(String _buffer_name);
    
    /**
     * Get the process name.
     * 
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @return process_name
     */
    public String get_process_name();
    
    /**
     * Set the process name.
     * 
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @param _process_name new process name
     */
    public void set_process_name(String _process_name);
    
    
    /**
     * Disconnect closing all sockets.
     */
    public void disconnect();
    
    /**
     * Connect as connect() but simply print a message to strderr and return -1 
     * in case of an error rather than throwing an exception.
     * 
     * NOTE: ReadNMLConfigurationFile and the NMLConnection() constructor with 4 arguments.
     * call connect() internally so it is only necessary to call this if 
     * the first connect() failed, or disconnect() was called or the 0 arguments
     * constructor was used to createt the NMLConnection.
     * 
     * @return 0 connect was ok. -1 error occured.
     */
    public int connectNoThrow();
    
    /**
     * Connect or reconnect to the sockets as appropriate.
     * Most errors throw an exception. Unfortunately the return value also
     * needs to be checked since a few errors only cause a -1 return value.
     * 
     * NOTE: ReadNMLConfigurationFile and the NMLConnection() constructor with 4 arguments.
     * call connect() internally so it is only necessary to call this if 
     * the first connect() failed, or disconnect() was called or the 0 arguments
     * constructor was used to createt the NMLConnection.
     * * 
     * @return 0 connect was ok. -1 error occured.
     * @throws Exception when connection fails
     */
    public int connect() throws Exception;
    
    
    /**
     * Convert the string into an NMLmsg and send it to this channel.
     * Depending on the server configuration the string may also be sent directly to 
     * the server to be converted on the server end.
     * The string should be comma delimited starting with the type and size.
     * ie:
     * 1001,0,99
     * 
     * @param s string to be converted 
     * @return 0 write ok, -1 an error occured.
     * @throws NMLException  when write fails
     */
    public int  writeDataString(String s) throws NMLException;
    
    /**
     * Convert the string into an NMLmsg and send it to this channel.
     * Depending on the server configuration the string may also be sent directly to 
     * the server to be converted on the server end.
     * The string should be comma delimited starting with the type and size.
     * ie:
     * 1001,0,99
     * 
     * The same as writeDataString except it never throws an exception merely prints
     * a message and returns -1.
     *
     * @param s string to be converted 
     * @return 0 write ok, -1 an error occured.
     */
    public int  writeDataStringNoThrow(String s);
    
    
    /**
     * Read a message and convert it to a comma delimeted string.
     * Depending on the server configuration the string might also be sent
     * from a server that did the conversion on the server side.
     * 
     * It marks the buffer as read and removes the message from the queue if 
     * the buffer is queued.
     * 
     * It is the same as readDataString except it prints a  error message and 
     * returns -1 instead of throwing an exception.
     * 
     * @return message read in comma delimited form.
     */
    public String readDataStringNoThrow();
    
    
    /**
     * Peek a message and convert it to a comma delimeted string.
     * Depending on the server configuration the string might also be sent
     * from a server that did the conversion on the server side.
     * 
     * Peeking is the same as reading except that it does not modify the 
     * buffer it reads. It does not mark the buffer as read or remove the message
     * from a queued_buffer.
     *
     * It is the same as peekDataString except it prints a  error message and 
     * returns -1 instead of throwing an exception.
     * 
     * @return message read in comma delimited form.
     */
    public String peekDataStringNoThrow();

    /**
     * Get the value of the connected property. Set to true by a successful connect()
     * or constructor set to false by disconnect().
     * 
     * @return Is the Channel and its underlying socket connected?
     */
    public boolean is_connected();
    
    
    /**
     * Get the buffer line from the configuration file.
     * @return BufferLine
     */
    public String getBufferLine();

    /**
     * Get the number of times a NoThrow fuction returned -1 for this object.
     * @return noThrowErrorCount.
     */
    public int getNoThrowErrorCount();
    
    /**
     * Get the type of message in the currently in the buffer.
     * NOTEL: another process may change this before you have a chance to read the
     * message.
     * @return msg_type
     * @throws Exception when get_msg_type fails
     */
    public int get_msg_type() throws Exception;
    
    /**
     * Get the number of messages that have been successfully written to the buffer, 
     * including messages written by other processes/systems.
     * 
     * @return msg_count
     * @throws Exception when get_msg_count fails
     */
    public int get_msg_count() throws Exception;
    
    
    /**
     * Each message has an id used to determine if it is new.
     * 
     * @return id of last message read.
     */
    public int get_last_id_read();
    
    
    
    /**
     * Set the id which will be compared with messages recieved to determine if 
     * the message is new.
     * A message in new if its id does not equal  the value set here.
     * 
     * @param _id new value for id
     */
    public void set_last_id_read(int _id);
    

    /**
     * Reread the NML Configuration and reconnect.
     * 
     * Same as ReadNMLConfiguration() except errors are only printed to
     * stderr and no exceptions are thrown.
     * 
     */
    public void ReadNMLConfigurationFileNoThrow();

    /**
     * Set the message dictionary.
     * The message dictionary sets the set of message types that can be read or written
     * from this channel.
     * 
     * @param new_dict new dictionary object
     */
    public void SetMessageDictionary(NMLMessageDictionary new_dict);
    
    /**
     * Set an interface that will have its member functions called for various errors.
     * Used by the diagnostics tools to improve error reporting.
     * 
     * @param new_nfceci new callback object
     */
    public void SetFormatConvertErrCallback(NMLFormatConvertErrCallbackInterface new_nfceci);

    /**
     * Setup a single variable log.
     * Occationally it is useful to read only a single variable from a large message and
     * there for reduce the amount of bandwidth required to log that variable.
     * 
     * @param varname  name of the variable to log
     * @param maxlogsize maximum number of variable updates to store
     * @param period time in seconds between checks to see if the variable changed.
     * @param type type of expected message, no data is logged when other message types are in the buffer.
     * @return -1 if an error occurs, positive integer id of the log used with getSingleVarLog(), and closeSingleVarLog()
     */
    public int setupSingleVarLog( String varname, int maxlogsize, double period, int type);
    
    /**
     * Get a NMLSingleVaLog object associated with the var_log_number that should have been 
     * returned by setupSingleVarLog()
     * 
     * @param var_log_number number assoctiated with var_log
     * @return singleVarLog
     */
    public NMLSingleVarLog getSingleVarLog( int var_log_number);
    
    
    /**
     * Stop collecting data on a single variable setup in setupSingleVarLog().
     * 
     * @param var_log_number number associated with var log
     * @return 0 ok, -1 comm error in sending close request.
     */
    public int closeSingleVarLog( int var_log_number);
    
    /**
     * Get the message string associated with the last exception thrown including
     * those printed  but caught internally by NoThrow functions.
     * @return exception_string
     */
    public String get_last_exception_string();
    
    public int getAllocation_size_max();
    public void setAllocation_size_max(int allocation_size_max);
    
    public String read_errlog_string() throws NMLException; 
}
