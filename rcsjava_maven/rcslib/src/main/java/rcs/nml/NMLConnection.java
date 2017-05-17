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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.Hashtable;
import java.util.StringTokenizer;
import rcs.utils.StrToLong;
import rcs.utils.URL_and_FileLoader;
import java.util.Vector;
import rcs.utils.StackTracePrinter;

/**
 * Class for connecting to NML buffers, ussually via network sockets.<br>
 * <pre>
 * Related Documentation:
 * <A HREF="http://www.isd.mel.nist.gov/projecst/rcslib">RCS Library</a>, <A HREF="http://www.isd.mel.nist.gov/projects/rcslib/NMLjava.html">NML Programmers Guide (Java Version)</a>
 *
 * </pre>
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 *
 */
public class NMLConnection implements NMLConnectionInterface {

    static protected Hashtable previously_read_nml_configurations = null;
    static private final int MEM_SAVE_SIZE = 8192;
    static private int force_socket_so_timeout = 0;
    private Selector inputSelector = null;

    /**
     * @return the default_poll_state
     */
    public static boolean isDefault_poll_state() {
        return default_poll_state;
    }

    /**
     * @param aDefault_poll_state the default_poll_state to set
     */
    public static void setDefault_poll_state(boolean aDefault_poll_state) {
        default_poll_state = aDefault_poll_state;
    }
    protected NMLFormatConverterBase format_converter = null;
    protected NMLMessageDictionary message_dictionary = null;
    private String BufferLine = null;
    private String disconnect_stack_trace = null;
    volatile private long connect_time = System.currentTimeMillis();
    volatile private long disconnect_time = System.currentTimeMillis();    // Protocol Options
    protected static final int NML_STCP_PROTOCOL_TYPE = 1;
    protected static final int NML_TCP_PROTOCOL_TYPE = 2;
    protected static final int NML_UDP_PROTOCOL_TYPE = 3;
    protected int protocol_option = NML_TCP_PROTOCOL_TYPE;    // Data format Options
    protected static final int NML_ASCII_ENCODING_TYPE = 4;
    protected static final int NML_DISP_ENCODING_TYPE = 5;
    protected static final int NML_XDR_ENCODING_TYPE = 6;
    protected static final int NML_XML_ENCODING_TYPE = 7;
    protected static final int NML_PACKED_ENCODING_TYPE = 8;
    protected static final int NML_PACKEDL64_ENCODING_TYPE = 9;
    private int data_format_option = NML_PACKED_ENCODING_TYPE;
    private int serial_number = 0;
    private int last_id_read = 0;
    private int max_tries = 20;
    private String last_exception_message = null;    // REMOTE_CMS_REQUEST_TYPE's
    protected static final int REMOTE_CMS_READ_REQUEST_TYPE = 1;
    protected static final int REMOTE_CMS_WRITE_REQUEST_TYPE = 2;
    protected static final int REMOTE_CMS_GET_KEYS_REQUEST_TYPE = 7;
    protected static final int REMOTE_CMS_LOGIN_REQUEST_TYPE = 8;
    protected static final int REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE = 9;
    protected static final int REMOTE_CMS_READ_COMBINED_REQUEST_TYPE = 10;
    protected static final int REMOTE_CMS_BLOCKING_READ_REQUEST_TYPE = 11;
    protected static final int REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE = 12;
    protected static final int REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE = 13;
    protected static final int REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE = 14;
    protected static final int REMOTE_CMS_GET_DIAG_INFO_REQUEST_TYPE = 15;
    protected static final int REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE = 16;
    protected static final int REMOTE_CMS_GET_QUEUE_LENGTH_REQUEST_TYPE = 17;
    protected static final int REMOTE_CMS_GET_SPACE_AVAILABLE_REQUEST_TYPE = 18;
    protected static final int REMOTE_CMS_WRITE_WITH_PRIORITY_REQUEST_TYPE = 19;
    protected static final int REMOTE_CMS_WRITE_WITH_BITWISE_OP_REQUEST_TYPE = 20;
    protected static final int REMOTE_CMS_WAIT_FOR_WRITE_REQUEST_TYPE = 21;
    protected static final int REMOTE_CMS_WAIT_FOR_READ_REQUEST_TYPE = 22;
    protected static final int REMOTE_CMS_WAIT_FOR_QUEUE_LENGTH_REQUEST_TYPE = 23;
    protected static final int REMOTE_CMS_WAIT_FOR_CLEAR_REQUEST_TYPE = 24;
    protected static final int REMOTE_CMS_WAIT_FOR_ANYTHING_REQUEST_TYPE = 25;
    protected static final int REMOTE_CMS_GET_READ_COUNT_REQUEST_TYPE = 26;
    protected static final int REMOTE_CMS_GET_IS_CLEAR_REQUEST_TYPE = 27;
    protected static final int REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE = 28;
    protected static final int REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE = 29;
    protected static final int REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE = 30;
    protected static final int REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE = 31;
    protected boolean bad_host = false;
    protected static Vector unknown_hosts_vector = null;
    private int request_type = REMOTE_CMS_READ_REQUEST_TYPE;    // CMS_REMOTE_SUBSCRIPTION_REQUEST_TYPE's
    private static final int CMS_POLLED_SUBSCRIPTION = 1;
    // public static final int CMS_NO_SUBSCRIPTION = 2;
    //public static final int CMS_VARIABLE_SUBSCRIPTION = 3;    // CMS_INTERNAL_ACCESS_TYPE's
    protected static final int CMS_READ_ACCESS = 1;
    protected static final int CMS_CHECK_IF_READ_ACCESS = 2;
    protected static final int CMS_PEEK_ACCESS = 3;
    protected static final int CMS_WRITE_ACCESS = 4;
    protected static final int CMS_WRITE_IF_READ_ACCESS = 5;
    private int access_type = CMS_READ_ACCESS;
    private int remote_status = 0;
    private int message_size = 0;
    private int was_read = 0;
    private int input_bytes_read = 0;
    private boolean reply_header_received = false;
    private boolean write_reply_received = true;
    private boolean input_buffer_ready = false;
    protected int output_data_size = 0;
    private double min_compatible_version = 0.0;
    private boolean confirm_write = false;
    private boolean use_subscription = false;
    private int subscription_id = 0;
    private double subscription_period = 0.0;
    private boolean poll = false;
    private static boolean default_poll_state = false;
    private boolean diag_enabled = false;
    private int connection_number = 0;
    private String bufname_returned = null;
    private boolean read_only = false;
    private boolean write_only = false;

    /**
     * Get the time in milliseconds when the last connect() was attempted.
     * (To determin how long it has been since the last connection subtract
     * from the current time given with System.currentTimeMillis();
     * @return connect_time
     */
    public long get_connect_time() {
        return connect_time;
    }

    /**
     * Get the time in milliseconds when the last disconnect() was attempted.
     * (To determin how long it has been since disconnected subtract
     * from the current time given with System.currentTimeMillis();
     * @return disconnect_time
     */
    public long get_disconnect_time() {
        return disconnect_time;
    }

    /**
     * Stop collecting data on a single variable setup in setupSingleVarLog().
     *
     * @return 0 ok, -1 comm error in sending close request.
     */
    public String get_last_exception_string() {
        return this.last_exception_message;
//        if (null == last_exception_string) {
//            return null;
//        }
//        
//        return s;
    }

    private void set_last_exception_message(NMLException last_exception) {
        String s = last_exception.getMessage() + "\n";
        if (last_exception.internal_exception != null) {
            s += last_exception.internal_exception.getClass().getName() + " : "
                    + last_exception.internal_exception.getMessage() + "\n"
                    + StackTracePrinter.ThrowableToStackTraceString(last_exception.internal_exception);
        } else {
            s += StackTracePrinter.ThrowableToStackTraceString(last_exception);
        }
        if (m_SocketAddress != null) {
            s += "m_SocketAddress = " + m_SocketAddress.toString() + "\n";
        }
        s += last_exception.getMessage() + "\n";
        if (last_exception.internal_exception != null) {
            s += last_exception.internal_exception.getMessage() + "\n";
        }
        if (s.length() > 100000) {
            s = s.substring(0, 100000);
        }
        this.last_exception_message = s;
    }

    private NMLException create_NMLException(String _message) {
        NMLException last_exception = new NMLException(_message, this);
        this.set_last_exception_message(last_exception);
        return last_exception;
    }

    private NMLException create_NMLException(String _message, String _buffer_line) {
        NMLException last_exception = new NMLException(_message + "\n BufferLine: " + _buffer_line, this);
        this.set_last_exception_message(last_exception);
        return last_exception;
    }

    private NMLException create_NMLException(String _message,
            Exception _internal_exception) {
        NMLException last_exception =
                new NMLException(_message, this, _internal_exception);
        this.set_last_exception_message(last_exception);
        return last_exception;
    }

    /**
     * Each message has an id used to determine if it is new.
     *
     * @return id of last message read.
     */
    public int get_last_id_read() {
        return last_id_read;
    }

    /**
     * Set the id which will be compared with messages recieved to determine if
     * the message is new.
     * A message in new if its id does not equal  the value set here.
     *
     * @param _id new value of id
     */
    public void set_last_id_read(int _id) {
        last_id_read = _id;
    }

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
    public String get_returned_buffer_name() {
        return bufname_returned;
    }
    private int noThrowErrorCount = 0;

    /**
     * Get the number of times a NoThrow fuction returned -1 for this object.
     * @return noThrowErrorCount.
     */
    public int getNoThrowErrorCount() {
        return noThrowErrorCount;
    }

    String appendInfo(String in, String info) {
        int last_line_index = in.lastIndexOf('\n');
        int line_len = in.length() - last_line_index;
        String out;
        if (line_len > 70) {
            out = in + ",\n\t" + info;
        } else {
            out = in + ", " + info;
        }
        return out;
    }

    public String toString() {
        String s = super.toString();
        try {
            s += " {\n";
            s = appendInfo(s, "buffer_name=" + buffer_name);
            s = appendInfo(s, "process_name=" + process_name);
            if (null != configuration_file) {
                s = appendInfo(s, "configuration_file=" + configuration_file);
            }
            s = appendInfo(s, "BufferLine=" + BufferLine);
            if (port != 0) {
                s = appendInfo(s, "port=" + port);
            }
//            s = appendInfo(s, "output_buffer=" + output_buffer);
//            s = appendInfo(s, "input_buffer=" + input_buffer);
            if (noThrowErrorCount != 0) {
                s = appendInfo(s, "noThrowErrorCount=" + noThrowErrorCount);
            }
            s = appendInfo(s, "connected=" + connected);
            s = appendInfo(s, "connect_time=" + connect_time);
            if (serial_number != 0) {
                s = appendInfo(s, "serial_number=" + serial_number);
            }
            if (last_id_read != 0) {
                s = appendInfo(s, "last_id_read=" + last_id_read);
            }
            s = appendInfo(s, "access_type=" + access_type);
            s = appendInfo(s, "remote_status=" + remote_status);
            if (message_size != 0) {
                s = appendInfo(s, "message_size=" + message_size);
            }
            s = appendInfo(s, "was_read=" + was_read);
            if (input_bytes_read != 0) {
                s = appendInfo(s, "input_bytes_read=" + input_bytes_read);
            }
            s = appendInfo(s, "reply_header_received=" + reply_header_received);
            s = appendInfo(s, "write_reply_received=" + write_reply_received);
            s = appendInfo(s, "input_buffer_ready=" + input_buffer_ready);
            if (output_data_size != 0) {
                s = appendInfo(s, "output_data_size=" + output_data_size);
            }
//            if (null != this.get_last_exception_string()) {
//                s += "\tLast Exception :\n";
//                s += this.get_last_exception_string();
//                s += "\n\tEND Last Exception\n";
//            }
            s = appendInfo(s, "disconnect_stack_trace = " + this.disconnect_stack_trace.replace('\n', ':'));
            s += "}\n";

        } catch (Exception e) {
            e.printStackTrace();
        }
        return s;
    }
    protected NMLmsg last_msg_read = null;
    /**
     * Set config_debug_on to true to print out additional information
     * while reading the NML Congiguration file.
     */
    static private boolean config_debug_on = rcs.nml.debugInfo.debug_on;
    /**
     * Set read_debug_on to true to print out additional information
     * while reading.
     */
    private boolean read_debug_on = rcs.nml.debugInfo.debug_on;
    /**
     * Set write_debug_on to true to print out additional information
     * while writing.
     */
    private boolean write_debug_on = rcs.nml.debugInfo.debug_on;
    private String input_string = "";
    /**
     * Name of buffer to connect to. (used in configuration file)
     */
    private String buffer_name = null;
    /**
     * Name of this process as used in configuration file.
     */
    private String process_name = null;
    private boolean using_nmlcfgsvr = false;
    /**
     * Name/URL the configuration file to read.
     */
    private String configuration_file = null;
    private int input_bytes_ready = 0;
    private byte input_buffer[] = null;
    private byte output_buffer[] = null;
    private static final int NML_DEFAULT_BUFFER_SIZE = 2048;
    private int buffer_size;
    /**
     * TCP port of NML server.
     */
    private int port = 0;
    /**
     * buffer_number from NML configuration file.
     */
    private int buffer_number = 0;
    /**
     * name of the computer the NML server runs on.
     */
    private String host = null;
    protected boolean read_request_sent = false;
    private boolean null_error_reported = false;
    private volatile DataInputStream m_InputStream = null;
    private volatile DataOutputStream m_OutputStream = null;
    private Socket m_Socket = null;
    private SocketChannel tcpSocketChannel = null;
    private java.net.InetSocketAddress m_SocketAddress = null;
    private NonBlockingDatagramSocket udpSocket = null;
    private InetAddress udpServerAddress = null;
    private ByteArrayOutputStream baOutputStream = null;
    private ByteArrayInputStream baInputStream = null;
    private DatagramPacket udpInputPacket = null;
    private long udpRetryTimeoutMillis = 30;
    private long lastUdpRequestTime = 0;
    private int broadcast_port = 0;
    /**
     * True when this object is connected to the NML server.
     * All reads and writes will fail when this is false.
     */
    private boolean connected = false;

    static private void DebugPrint(String s) {
        try {
            if (!rcs.nml.debugInfo.debug_on || null == rcs.nml.debugInfo.debugPrintStream) {
                return;
            }
            Throwable t = new Throwable();
            String exceptionText = StackTracePrinter.ThrowableToStackTraceString(t);
            String filename = StackTracePrinter.ThrowableTextToFileName(exceptionText, 2);
            int line = StackTracePrinter.ThrowableTextToLine(exceptionText, 2);
            rcs.nml.debugInfo.debugPrintStream.println(filename + ":" + line + " (time=" + System.currentTimeMillis() + ") " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
//	static public void main(String args[])
//	{
//		ErrorPrint("foo");
//	}
    //    static private void DebugPrint2(String s) {
//        try {
//            Throwable t = new Throwable();
//            StackTraceElement ste[] = t.getStackTrace();
//            System.out.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " (time=" + System.currentTimeMillis() + ") " + s);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
    private static Vector NMLErrorAppenders = null;

    /**
     * Add an opject to be called with each error to append messages.
     * Multiple appenders can be added.
     *
     * @param nea listener for error events
     */
    static public void AddNMLErrorAppender(NMLErrorAppender nea) {
        if (null == NMLErrorAppenders) {
            NMLErrorAppenders = new Vector();
        }
        NMLErrorAppenders.add(nea);
    }
    static public boolean do_not_print_errors = false;

    static private void AppendError(String err_string) {
        try {
            if (do_not_print_errors) {
                return;
            }
            System.err.println(err_string);
            if (null != NMLErrorAppenders) {
                for (int i = 0; i < NMLErrorAppenders.size(); i++) {
                    NMLErrorAppender nea = (NMLErrorAppender) NMLErrorAppenders.elementAt(i);
                    nea.AppendError(err_string);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static private void ErrorPrint(String s) {
        try {
            Throwable t = new Throwable();
//            StackTraceElement ste[] = t.getStackTrace();
            String slist = StackTracePrinter.ThrowableToShortList(t);
            int cindex = slist.indexOf(',');
            if (cindex > 0) {
                slist = slist.substring(cindex + 1);
            }
            AppendError("ERROR: "
                    + slist + "\nERROR: \t" + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void check_socket_so_timeout_setting() {
        try {
            String prop = System.getProperty("NML_FORCE_SO_SOCKET_TIMEOUT");
            if (prop != null) {
                this.force_socket_so_timeout = Integer.valueOf(prop).intValue();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * This constructs an NMLConnection which will not work until
     * the host and port are set manually or
     * by reading the NML configuration file with   ReadNMLConfigurationFile().
     * Most users should use the other constructor with several parameters.
     *
     * @see rcs.nml.NMLConnection#NMLConnection(rcs.nml.NMLMessageDictionary, java.lang.String, java.lang.String, java.lang.String)
     * @see rcs.nml.NMLConnection#ReadNMLConfigurationFile(java.lang.String, java.lang.String, java.lang.String)
     *
     */
    public NMLConnection() {
        //  rcs.nml.debugInfo.debugPrintStream.print("\r\nconstructing NMLConnection\r\n");
        try {
            input_buffer = null;
            output_buffer = null;
            buffer_size = NML_DEFAULT_BUFFER_SIZE;
            poll = default_poll_state;
            check_socket_so_timeout_setting();
            SetFormatConverter(new PackedFormatConverter(false));

        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
    }

    /**
     * This functions sets the NMLMessageDictionary for this connection.
     * The purpose of the NMLMessageDictionary is to provide a means
     * for the NMLConnection to determine the structure of messages being
     * sent or received using a message type. (An integer passed within
     * all NML messages.)
     *
     * @param new_dict the NMLMessageDictionary for this NMLConnection to use.
     * @see rcs.nml.NMLMessageDictionary
     */
    public void SetMessageDictionary(NMLMessageDictionary new_dict) {
        message_dictionary = new_dict;
        if (null != format_converter) {
            format_converter.SetMessageDictionary(new_dict);
        }
        if (null != buffer_name && null != format_converter) {
            format_converter.SetBufName(buffer_name);
        }
    }
    protected NMLFormatConvertErrCallbackInterface nfceci = null;

    /**
     * Set an interface that will have its member functions called for various errors.
     * Used by the diagnostics tools to improve error reporting.
     *
     * @param new_nfceci listenter for format error events
     */
    public void SetFormatConvertErrCallback(NMLFormatConvertErrCallbackInterface new_nfceci) {
        nfceci = new_nfceci;
        if (null != format_converter) {
            format_converter.SetFormatConvertErrCallback(new_nfceci);
        }
    }

    /**
     * This functions gets the NMLMessageDictionary for this connection.
     *
     * @return the implementation of NMLMessageDictionary used by this
     * NMLConnection
     *
     * @see rcs.nml.NMLConnection#SetMessageDictionary(rcs.nml.NMLMessageDictionary)
     * @see rcs.nml.NMLMessageDictionary
     */
    public NMLMessageDictionary GetMessageDictionary()  {
        if (null != format_converter) {
            message_dictionary = format_converter.GetMessageDictionary();
        }
        return message_dictionary;
    }

    /**
     * This functions sets the NMLFormatConverter for this connection.
     * The NMLFormatConverter is resposible for converting each of
     * the basic data types to some neutral format that can be
     * used on many different platforms. Most users should either accept
     * the default (XDRFormatConverter) or allow the format to be specified
     * in the NML configuration file rather than calling this function
     * directly.
     *
     * @param new_fc the NMLFormatConverter for this Connection to use.
     * @throws rcs.nml.NMLException when nml_fc is not extended from NMLFormatConverterBase
     *
     * @see rcs.nml.NMLFormatConverter
     */
    public void SetFormatConverter(NMLFormatConverter new_fc) throws NMLException {
        try {
            format_converter = (NMLFormatConverterBase) new_fc;
        } catch (ClassCastException e) {
            ErrorPrint("This NMLFormatConverter is not a subclass of NMLFormatConverterBase.");
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            throw create_NMLException(" !ERROR! This NMLFormatConverter is not a subclass of NMLFormatConverterBase.", e);
        }
        if (null != message_dictionary && null != format_converter) {
            format_converter.SetMessageDictionary(message_dictionary);
        }
        if (null != buffer_name && null != format_converter) {
            format_converter.SetBufName(buffer_name);
        }
        if (null != nfceci && null != format_converter) {
            format_converter.SetFormatConvertErrCallback(nfceci);
        }
    }

    /**
     * This functions gets the NMLFormatConverter for this connection.
     *
     * @return the NMLFormatConverter for this connection
     *
     * @see rcs.nml.NMLConnection#SetFormatConverter(rcs.nml.NMLFormatConverter)
     */
    public NMLFormatConverter GetFormatConverter() {
        return format_converter;
    }

    /**
     * This constuctor creates a fully functional NML Connection
     * using the application defined NMLMessageDictionary,  read
     * the NML configuration file,  and connects immediately.
     * This is the constructor most users should use.
     *
     * @param msg_dict the NMLMessageDictionary for this connection to use (the NMLMessageDictionary allows this Connection to determine the message structure from the message type)
     * @param BufferName the name of the buffer to connect to, (must match one of the buffers in the configuration file.)
     * @param ProcessName the name of the process that will use this connection (must match one of the process names in the configuration file)
     * @param ConfigurationFile the file name or URL of an NML configuration file (URL's should either be complete, or they can be relative to
     * rcs.utils.URL_and_FileLoader.current_directory)
     * @exception  rcs.nml.NMLException
     *            If the configuration file can not be read, the file is improperly formatted or the buffer or process can
     *            can not be found in it.
     *
     */
    public NMLConnection(NMLMessageDictionary msg_dict, String BufferName, String ProcessName, String ConfigurationFile) throws NMLException {
        this();
        ReadNMLConfigurationFile(BufferName, ProcessName, ConfigurationFile);
        SetMessageDictionary(msg_dict);
        if (null != nfceci && null != format_converter) {
            format_converter.SetFormatConvertErrCallback(nfceci);
        }
    }
    private String additional_options[] = null;

    private void ParseOption(String opt) throws NMLException {
        if (opt.equals("read_only")) {
            if (this.write_only) {
                throw this.create_NMLException("Attempt to set read_only when write_only already set.");
            }
            this.read_only = true;
        } else if (opt.equals("write_only")) {
            if (this.read_only) {
                throw this.create_NMLException("Attempt to set write_only when read_only already set.");
            }
            this.write_only = true;
        }
    }

    public void ParseAdditionalOptions(String _additional_options[]) throws NMLException {
        this.additional_options = _additional_options;
        if (null != this.additional_options) {
            for (int i = 0; i < this.additional_options.length; i++) {
                ParseOption(this.additional_options[i]);
            }
        }
    }

    /**
     * This constuctor creates a fully functional NML Connection
     * using the application defined NMLMessageDictionary,  read
     * the NML configuration file,  and connects immediately.
     * This is the constructor most users should use.
     *
     * @param msg_dict the NMLMessageDictionary for this connection to use (the NMLMessageDictionary allows this Connection to determine the message structure from the message type)
     * @param BufferName the name of the buffer to connect to, (must match one of the buffers in the configuration file.)
     * @param ProcessName the name of the process that will use this connection (must match one of the process names in the configuration file)
     * @param ConfigurationFile the file name or URL of an NML configuration file (URL's should either be complete, or they can be relative to
     * rcs.utils.URL_and_FileLoader.current_directory)
     * @param _additional_options array of additional options
     * @exception  rcs.nml.NMLException
     *            If the configuration file can not be read, the file is improperly formatted or the buffer or process can
     *            can not be found in it.
     *
     */
    public NMLConnection(NMLMessageDictionary msg_dict, String BufferName, String ProcessName, String ConfigurationFile, String _additional_options[]) throws NMLException {
        this();
        ParseAdditionalOptions(_additional_options);
        ReadNMLConfigurationFile(BufferName, ProcessName, ConfigurationFile);
        SetMessageDictionary(msg_dict);
        if (null != nfceci && null != format_converter) {
            format_converter.SetFormatConvertErrCallback(nfceci);
        }
    }

    /**
     * This function reads configuration information from the NML configuration file.
     *
     * @param BufferName the name of the buffer to connect to, (must match one of the buffers in the configuration file.)
     * @param ProcessName the name of the process that will use this connection (must match one of the process names in the configuration file)
     * @param ConfigurationFile the file name or URL of an NML configuration file (URL's should either be complete, or they can be relative to
     * rcs.utils.URL_and_FileLoader.current_directory)
     * @exception  rcs.nml.NMLException
     *            If the configuration file can not be read, the file is improperly formatted or the buffer or process can
     *            can not be found in it.
     *
     */
    public void ReadNMLConfigurationFile(String BufferName, String ProcessName, String ConfigurationFile) throws NMLException {
        buffer_name = BufferName;
        process_name = ProcessName;
        configuration_file = ConfigurationFile;
        ReadNMLConfigurationFile();
        connect();
        if (connected && !verify_bufname()) {
            disconnect();
            ErrorPrint("bufname_returned=" + bufname_returned);
            throw create_NMLException(" !ERROR! Verify Bufname returned false.");
        }
    }

    /**
     * Clear any data saved from previously read configuration files.
     */
    public static void ClearStaticData() {
        previously_read_nml_configurations = null;
    }

    /**
     * Parse or return configution info from a previously parsed NML configuration file.
     *
     * @param configuration_file name of configuration file to read
     * @return nml configuraton info
     */
    public static NMLConfigInfo GetConfigInfo(String configuration_file) {
        NMLConfigInfo config_info = null;
        if (configuration_file.startsWith("nmlcfgsvr:")) {
            return GetAllConfigInfoFromServer(configuration_file.substring(10));
        }
        if (previously_read_nml_configurations != null) {
            config_info = (NMLConfigInfo) previously_read_nml_configurations.get(configuration_file);
            if (config_info != null) {
                return config_info;
            }
        }
        config_info = LoadConfigurationFileData(configuration_file);
        return config_info;
    }

    /**
     * Should NML store configuration data to make
     * connecting to new buffers faster?
     *
     * Default : true
     */
    static public boolean keep_old_configs = true;


    static NMLConfigInfo LoadConfigurationFileData(String configuration_file) {
        NMLConfigInfo config_info = new NMLConfigInfo();
        NMLBufferConfigInfo bufferConfigInfo = null;
        config_info.file_name = configuration_file;
        URL_and_FileLoader loader = null;
        String current_line = null;
        String buffer_name = null;
        String process_name = null;
        loader = new URL_and_FileLoader(configuration_file);
        while (true) {
            current_line = loader.readLine();
            if (current_line == null) {
                break;
            }
            if (config_debug_on) {
                DebugPrint(current_line);
            }
            if (current_line.length() < 3) {
                continue;
            }
            if (current_line.startsWith("nmlcfgsvr:")) {
                config_info.nmlcfgsvr = current_line.substring(10);
                break;
            }
            if (current_line.startsWith("#")) {
                int header_dir_index = current_line.indexOf("HEADER_DIR=");
                if (header_dir_index > 0) {
                    if (null == config_info.header_dir_vector) {
                        config_info.header_dir_vector = new Vector();
                        config_info.header_dir_vector.add(current_line.substring(header_dir_index + 11).trim());
                    }
                }
                int include_dir_index = current_line.indexOf("INCLUDE_DIR=");
                if (include_dir_index > 0) {
                    if (null == config_info.include_dir_vector) {
                        config_info.include_dir_vector = new Vector();
                        config_info.include_dir_vector.add(current_line.substring(include_dir_index + 12).trim());
                    }
                }
                continue;
            }
            if (current_line.startsWith("B")) {
                StringTokenizer tokenizer = new StringTokenizer(current_line, " \t");
                if (null == tokenizer) {
                    continue;
                }
                int token_number = 0;
                while (tokenizer.hasMoreTokens()) {
                    String token = tokenizer.nextToken();
                    if (null == token) {
                        break;
                    }
                    if (config_debug_on) {
                        DebugPrint("token " + token_number + " = " + token);
                    }
                    if (token_number == 1) {
                        buffer_name = token;
                        bufferConfigInfo = (NMLBufferConfigInfo) config_info.buffer_configurations.get(buffer_name);
                        if (bufferConfigInfo == null) {
                            bufferConfigInfo = new NMLBufferConfigInfo();
                            bufferConfigInfo.buffer_name = buffer_name;
                            bufferConfigInfo.buffer_line = current_line;
                            config_info.buffer_configurations.put(buffer_name, bufferConfigInfo);
                        } else {
                            if (null == bufferConfigInfo.buffer_line) {
                                bufferConfigInfo.buffer_line = current_line;
                            }
                        }
                        break;
                    }
                    token_number++;
                }
            }
            if (current_line.startsWith("P")) {
                StringTokenizer tokenizer = new StringTokenizer(current_line, " \t");
                if (null == tokenizer) {
                    continue;
                }
                int token_number = 0;
                while (tokenizer.hasMoreTokens()) {
                    String token = tokenizer.nextToken();
                    if (null == token) {
                        break;
                    }
                    if (config_debug_on) {
                        DebugPrint("token " + token_number + " = " + token);
                    }
                    if (token_number == 1) {
                        process_name = token;
                    }
                    if (token_number == 2) {
                        buffer_name = token;
                        bufferConfigInfo = (NMLBufferConfigInfo) config_info.buffer_configurations.get(buffer_name);
                        if (bufferConfigInfo == null) {
                            bufferConfigInfo = new NMLBufferConfigInfo();
                            bufferConfigInfo.buffer_name = buffer_name;
                            config_info.buffer_configurations.put(buffer_name, bufferConfigInfo);
                        }
                        bufferConfigInfo.add_process_line(process_name, current_line);
                        if (buffer_name.compareTo("default") == 0 && process_name.compareTo("default") == 0) {
                            config_info.DefaultProcessLine = current_line;
                        }
                        break;
                    }
                    token_number++;
                }
            }
        }
        if (keep_old_configs) {
            if (null == previously_read_nml_configurations) {
                previously_read_nml_configurations = new Hashtable();
            }
            previously_read_nml_configurations.put(configuration_file, config_info);
        }
        return config_info;
    }
    /**
     * Read preset configuration file.
     * @exception  rcs.nml.NMLException
     *            If the configuration file can not be read, the file is improperly formatted or the buffer or process can
     *            can not be found in it.
     */
    NMLBufferConfigInfo default_buf_info = null;

    public static NMLConfigInfo GetAllConfigInfoFromServer(String server) {
        String hostname = server;
        String portstring = "";
        String timeoutstring = "";
        String createstring = "";
        String optionsstring = "";
        int port = 11671;
        boolean closing = false;
        NMLConfigInfo nci = null;

        Socket nmlcfgsvrSocket = null;
        try {

            if (config_debug_on) {
                DebugPrint("GetConfigAllInfoFromServer(server=" + server + ",) called.");
            }

            int colon1_index = server.indexOf(':');
            int colon2_index = -1;
            if (colon1_index > 0) {
                hostname = server.substring(0, colon1_index);
            }
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                portstring = server.substring(colon1_index + 1, colon2_index);
            }
            colon1_index = colon2_index;
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                timeoutstring = server.substring(colon1_index + 1, colon2_index);
            }
            colon1_index = colon2_index;
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                createstring = server.substring(colon1_index + 1, colon2_index);
            }
            if (colon2_index > 4) {
                optionsstring = server.substring(colon2_index);
            }
            if (hostname.equals("")) {
                hostname = null;// Java doc says Socket(null,...) will use local loopback address 127.0.0.1
            }
            if (!portstring.equals("")) {
                try {
                    port = Integer.parseInt(portstring, 10);
                } catch (Exception e) {
                    e.printStackTrace();
                    port = 11671;
                }
            }
            nmlcfgsvrSocket = new Socket(hostname, port);
            DataOutputStream nmlcfgsvrOutputStream = new DataOutputStream(nmlcfgsvrSocket.getOutputStream());
            //DataInputStream nmlcfgsvrInputStream = new DataInputStream(nmlcfgsvrSocket.getInputStream());
            String req_string = "list your_address=" + (nmlcfgsvrSocket.getInetAddress().getHostAddress()) + "\r\n";
            nmlcfgsvrOutputStream.writeBytes(req_string);
            nmlcfgsvrOutputStream.flush();
            BufferedReader br = new BufferedReader(new InputStreamReader(nmlcfgsvrSocket.getInputStream()));
            String buffer_line = br.readLine();
            if (buffer_line == null || buffer_line.startsWith("NO")) {
                DebugPrint("List failed.\n");
                return null;
            }
            nci = new NMLConfigInfo();
            while (buffer_line != null) {
                StringTokenizer st = new StringTokenizer(buffer_line, " \t\r\n");
                if (!st.hasMoreElements()) {
                    buffer_line = br.readLine();
                    continue;
                }
                String tok0 = st.nextToken();
                if (tok0.equalsIgnoreCase("#END_LIST")) {
                    break;
                }
                if (tok0 == null || !tok0.equalsIgnoreCase("B")) {
                    buffer_line = br.readLine();
                    continue;
                }
                if (!st.hasMoreElements()) {
                    buffer_line = br.readLine();
                    continue;
                }
                String tok1 = st.nextToken();
                if (tok1 == null) {
                    buffer_line = br.readLine();
                    continue;
                }
                NMLBufferConfigInfo nbci = new NMLBufferConfigInfo();
                nbci.buffer_line = buffer_line;
                nbci.buffer_name = tok1;
                nci.buffer_configurations.put(nbci.buffer_name, nbci);
                buffer_line = br.readLine();
            }
            closing = true;
            nmlcfgsvrOutputStream.writeBytes("quit");
            nmlcfgsvrOutputStream.flush();
            br.close();
            nmlcfgsvrOutputStream.close();
            nmlcfgsvrSocket.close();
            return nci;
        } catch (Exception e) {
            if (!closing) {
                e.printStackTrace();
                ErrorPrint("server=" + server + ", hostname=" + hostname + ",port=" + port + ", timeoutstring=" + timeoutstring + ", createstring=" + createstring + ", optionsstring=" + optionsstring);
            }
        } finally {
            if (null != nmlcfgsvrSocket) {
                try {
                    nmlcfgsvrSocket.close();
                } catch (Throwable t) {
                }
                ;
                nmlcfgsvrSocket = null;
            }
        }
        return nci;
    }

    //@SuppressWarnings("unchecked")
    private NMLConfigInfo GetConfigInfoFromServer(String server, String buffer, String processName) {
        String hostname = server;
        String portstring = "";
        String timeoutstring = "";
        String createstring = "";
        String optionsstring = "";
        int nmlcfgsvr_port = 11671;
        Socket nmlcfgsvrSocket = null;

        try {

            if (config_debug_on) {
                DebugPrint("GetConfigInfoFromServer(server=" + server + ",buffer=" + buffer + ", processName=" + processName + ") called.");
            }

            int colon1_index = server.indexOf(':');
            int colon2_index = -1;
            if (colon1_index > 0) {
                hostname = server.substring(0, colon1_index);
            }
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                portstring = server.substring(colon1_index + 1, colon2_index);
            }
            colon1_index = colon2_index;
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                timeoutstring = server.substring(colon1_index + 1, colon2_index);
            }
            colon1_index = colon2_index;
            if (colon1_index >= 0) {
                colon2_index = server.indexOf(':', colon1_index + 1);
            }
            if (colon1_index >= 0 && colon2_index > colon1_index + 1) {
                createstring = server.substring(colon1_index + 1, colon2_index);
            }
            if (colon2_index > 4) {
                optionsstring = server.substring(colon2_index);
            }
            if (hostname.equals("")) {
                hostname = null;// Java doc says Socket(null,...) will use local loopback address 127.0.0.1
            }
            if (!portstring.equals("")) {
                try {
                    nmlcfgsvr_port = Integer.parseInt(portstring, 10);
                } catch (Exception e) {
                    e.printStackTrace();
                    nmlcfgsvr_port = 11671;
                }
            }
            nmlcfgsvrSocket = new Socket(hostname, nmlcfgsvr_port);
            DataOutputStream nmlcfgsvrOutputStream = new DataOutputStream(nmlcfgsvrSocket.getOutputStream());
            //DataInputStream nmlcfgsvrInputStream = new DataInputStream(nmlcfgsvrSocket.getInputStream());
            String req_string = "get " + buffer + " " + processName + " your_address=" + (nmlcfgsvrSocket.getInetAddress().getHostAddress()) + "\r\n";
            nmlcfgsvrOutputStream.writeBytes(req_string);
            nmlcfgsvrOutputStream.flush();
            BufferedReader br = new BufferedReader(new InputStreamReader(nmlcfgsvrSocket.getInputStream()));
            String buffer_line = br.readLine();
            if (buffer_line.startsWith("NO")) {
                DebugPrint("NMLCFGSVR reports " + buffer + " not available.\n");
                return null;
            }
            String proc_line = br.readLine();
            NMLConfigInfo nci = new NMLConfigInfo();
            NMLBufferConfigInfo nbci = new NMLBufferConfigInfo();
            nbci.buffer_name = buffer;
            nbci.buffer_line = buffer_line;
            nbci.add_process_line(processName, proc_line);
            nci.buffer_configurations.put(buffer, nbci);
            if (config_debug_on) {
                DebugPrint("GetConfigInfoFromServer: server=" + server + ", hostname=" + hostname + ",port=" + nmlcfgsvr_port + ", timeoutstring=" + timeoutstring + ", createstring=" + createstring + ", optionsstring=" + optionsstring + ", buffer=" + buffer);
                DebugPrint("GetConfigInfoFromServer: buffer_line=" + buffer_line);
                DebugPrint("GetConfigInfoFromServer: proc_line=" + proc_line);
            }
            last_recheck_nmlcfgsvr_time = System.currentTimeMillis();
            br.close();
            nmlcfgsvrOutputStream.close();
            nmlcfgsvrSocket.close();
            return nci;
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("server=" + server + ", hostname=" + hostname + ",port=" + nmlcfgsvr_port + ", timeoutstring=" + timeoutstring + ", createstring=" + createstring + ", optionsstring=" + optionsstring + ", buffer=" + buffer);
        } finally {
            if (null != nmlcfgsvrSocket) {
                try {
                    nmlcfgsvrSocket.close();
                } catch (Throwable t) {
                }
                ;
                nmlcfgsvrSocket = null;
            }
        }
        return null;
    }
    boolean called_from_no_throw = false;
    NMLBufferConfigInfo buf_info = null;

    private boolean CheckHost(String hostname) {
        try {
            if (hostname == null) {
                return false;
            }
            return (java.net.InetAddress.getByName(hostname) != null);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Rereadn the NML Configuration file.
     * @throws rcs.nml.NMLException when configuration is invalid
     */
    public void ReadNMLConfigurationFile() throws NMLException {
        String current_line = null;
        boolean BufferLineFound = false;
        boolean ProcessLineFound = false;
        boolean ConfigurationFileRead = false;
        NMLConfigInfo config_info = null;
        buf_info = null;

        m_SocketAddress = null;
        if (process_name == null) {
            process_name = "jdiag";
        }

        try {
            disconnect();
            if (config_debug_on) {
                DebugPrint("ReadNMLConfigurationFile(): buffer_name = " + buffer_name + ", process_name = " + process_name + ", configuration_file = " + configuration_file + ",");
            }
            if (null == configuration_file) {
                throw this.create_NMLException("ReadNMLConfigurationFile() called with configuration_file == null.");
            }


            if (configuration_file.startsWith("nmlcfgsvr:")) {
                using_nmlcfgsvr = true;
                config_info = GetConfigInfoFromServer(configuration_file.substring(10), buffer_name, process_name);
            } else {
                if (null == previously_read_nml_configurations) {
                    previously_read_nml_configurations = new Hashtable();
                } else {
                    config_info = (NMLConfigInfo) previously_read_nml_configurations.get(configuration_file);
                }
            }
            if (null == config_info) {
                LoadConfigurationFileData(configuration_file);
                config_info = (NMLConfigInfo) previously_read_nml_configurations.get(configuration_file);
            }

            buf_info = (NMLBufferConfigInfo) config_info.buffer_configurations.get(buffer_name);
            if (null == buf_info && config_info.nmlcfgsvr != null) {
                config_info = GetConfigInfoFromServer(config_info.nmlcfgsvr, buffer_name, process_name);
                buf_info = (NMLBufferConfigInfo) config_info.buffer_configurations.get(buffer_name);
            }
            if (null == buf_info) {
                if (called_from_no_throw) {
                    ErrorPrint("!ERROR! Can't find buffer line. buffer_name=" + buffer_name + ", configuration_file=" + configuration_file);
                    return;
                }
                if (configuration_file != null
                        && configuration_file.length() > 0
                        && !configuration_file.startsWith("nmlcfgsvr:")) {
                    File confF = new File(configuration_file);
                    if(!confF.exists()) {
                        File confP = confF.getParentFile();
                        File last_confP = confF;
                        while(null != confP && !confP.exists()) {
                            last_confP = confP;
                            confP = confP.getParentFile();
                        }
                        if(null != last_confP && last_confP != confF) {
                            throw create_NMLException(last_confP + " parent of "+ confF +" does not exist.");
                        }
                        throw create_NMLException(" !ERROR! Configuration file \"" + configuration_file + "\" does not exist.");
                    }
                }
                throw create_NMLException(" !ERROR! Can't find buffer line for "+buffer_name);
            }
            buf_info.used = true;
            String buffer_line = buf_info.buffer_line;
            String process_line = null;
            if (null != buf_info) {
                try {
                    process_line = (String) buf_info.getProcessLine(process_name);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            if (null == process_line) {
                process_line = (String) buf_info.getProcessLine("default");
            }
            if (null == process_line) {
                process_line = config_info.DefaultProcessLine;
            }
            if (null == process_line) {
                if (null == default_buf_info) {
                    default_buf_info = (NMLBufferConfigInfo) config_info.buffer_configurations.get("default");
                }
                if (default_buf_info != null) {
                    try {
                        if (null != default_buf_info) {
                            process_line = (String) default_buf_info.getProcessLine(process_name);
                            if (null == process_line) {
                                process_line = (String) default_buf_info.getProcessLine("default");
                            }
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            data_format_option = NML_PACKED_ENCODING_TYPE;
            ConfigurationFileRead = true;
            if (config_debug_on) {
                DebugPrint("buffer_line=" + buffer_line);
                DebugPrint("process_line=" + process_line);
            }
            if (null != process_line) {
                current_line = process_line;
                if (current_line.startsWith("P")) {
                    StringTokenizer tokenizer = new StringTokenizer(current_line, " \t");
                    int token_number = 0;
                    while (tokenizer.hasMoreTokens() && token_number < 10) {
                        String token = tokenizer.nextToken();
                        if (config_debug_on) {
                            DebugPrint("token " + token_number + " = " + token);
                        }
                        if (token_number == 1) {
                            if (token.compareTo(process_name) != 0
                                    && token.compareTo("default") != 0) {
                                break;
                            }
                        }
                        if (token_number == 2) {
                            if (token.compareTo(buffer_name) == 0) {
                                ProcessLineFound = true;
                            } else if (token.compareTo("default") == 0) {
                                ProcessLineFound = true;
                            } else {
                                break;
                            }
                        }
                        if (token_number == 7 && ProcessLineFound) {
                            try {
                                double timeout_d = Double.valueOf(token).doubleValue();
                                if (timeout_d > 0.01) {
                                    max_tries = (int) (timeout_d * 100.0);
                                } else {
                                    max_tries = 1;
                                    poll = true;
                                }
                            } catch (Exception e) {
                            }
                            if (token.toUpperCase().startsWith("INF")) {
                                max_tries = -1;
                            }
                            if (config_debug_on) {
                                DebugPrint("max_tries=" + max_tries + ", token = " + token);
                            }
                        }
                        if (token_number == 9 && ProcessLineFound) {
                            connection_number = Integer.valueOf(token).intValue();
                            if (config_debug_on) {
                                DebugPrint("connection_number=" + connection_number + ", token = " + token);
                            }
                        }
                        token_number++;
                    }
                    int poll_index = current_line.toUpperCase().indexOf("POLL");
                    if (poll_index >= 0) {
                        poll = true;
                    }
                    int sub_index = current_line.toUpperCase().indexOf("SUB=");
                    if (sub_index >= 0) {
                        poll = true;
                        String sub_period_string = current_line.substring(sub_index + 4);
                        if (config_debug_on) {
                            DebugPrint("sub_period_string =" + sub_period_string);
                        }
                        StringTokenizer t2 = new StringTokenizer(sub_period_string, "\r\n \t");
                        String token2 = t2.nextToken();
                        subscription_period = Double.valueOf(token2).doubleValue();
                        if (config_debug_on) {
                            DebugPrint("subscription_period =" + subscription_period);
                        }
                    }
                } // current_line.startsWith("P")
            } // process_line != null

            current_line = buffer_line;
            StringTokenizer tokenizer = new StringTokenizer(current_line, " \t");
            BufferLine = current_line;
            int token_number = 0;
            boolean encoding_type_set = false;
            while (tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();
                if (null == token) {
                    break;
                }
                if (config_debug_on) {
                    DebugPrint("token " + token_number + " = " + token);
                }
                if (token_number == 1) {
                    if (token.equals(buffer_name)) {
                        BufferLineFound = true;
                    } else {
                        break;
                    }
                }
                if (token_number == 2) {
                    if (token.startsWith("nmlcfgsvr:")) {
                        config_info = GetConfigInfoFromServer(token.substring(10), buffer_name, process_name);
                        buf_info = (NMLBufferConfigInfo) config_info.buffer_configurations.get(buffer_name);
                        buffer_line = current_line = BufferLine = buf_info.buffer_line;
                        tokenizer = new StringTokenizer(current_line, " \t");
                        token_number = 0;
                        continue;
                    }
                }
                if (token_number == 3) {
                    host = token;
                    if (null != unknown_hosts_vector) {
                        if (unknown_hosts_vector.contains(host)) {
                            //System.err.println("Skipping bad host: "+host + " for buffer name "+buffer_name);
                            bad_host = true;
                            throw create_NMLException(" !ERROR! Host :" + host + " has already thrown UknownHostException.");
                        }
                    }
                    if (!CheckHost(host)) {
                        if (null == unknown_hosts_vector) {
                            unknown_hosts_vector = new Vector();
                        }
                        unknown_hosts_vector.add(host);
                        bad_host = true;
                        throw create_NMLException(" !ERROR! Host :" + host + " failed CheckHost().");
                    }
                    if (config_debug_on) {
                        DebugPrint("host = " + host);
                    }
                }
                if (token_number == 4) {
                    try {
                        buffer_size = (int) (StrToLong.convert(token)) * 4;
                    } catch (Exception e) {
                        ErrorPrint("Invalid buffer size token (" + token + ") in " + configuration_file + " for " + buffer_name);
                        ErrorPrint("Using default buffer size.");
                        e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                        buffer_size = NML_DEFAULT_BUFFER_SIZE;
                    }
                    if (buffer_size <= 0) {
                        buffer_size = NML_DEFAULT_BUFFER_SIZE;
                    }
                    if (this.allocation_size_max <= 0) {
                        long fm = Runtime.getRuntime().freeMemory();
                        if (Runtime.getRuntime().maxMemory() > 0
                                && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
                            fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
                        }
                        if (buffer_size * 2 + MEM_SAVE_SIZE > fm) {
                            Runtime.getRuntime().gc();
                            fm = Runtime.getRuntime().freeMemory();
                            if (Runtime.getRuntime().maxMemory() > 0
                                    && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
                                fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
                            }
                        }
                        if (buffer_size * 2 + MEM_SAVE_SIZE > fm && buffer_size > 0) {
                            buffer_size /= 2;
                        }
                        if (config_debug_on) {
                            DebugPrint("buffer_size = " + buffer_size + " free memory=" + fm);
                        }
                        if (buffer_size * 2 + MEM_SAVE_SIZE > fm) {
                            ErrorPrint("buffer_size=" + buffer_size + ", freeMemory()=" + fm + ", free memory required = " + ((2 * buffer_size) + MEM_SAVE_SIZE));
                            ErrorPrint("Consider using -Xmx to increase memory available to java. (ie java -Xmx256m to increase to 256Mb.)");
                            input_buffer = null;
                            output_buffer = null;
                            buffer_size = (int) ((fm - MEM_SAVE_SIZE) / 2);
                            if (buffer_size < 0) {
                                buffer_size = 0;
                            }
                            throw create_NMLException(" !ERROR! Buffer size is too big for this system.");
                        }
                    }
                }
                if (token_number == 7) {
                    try {
                        buffer_number = ((Integer.valueOf(token)).intValue());
                    } catch (Exception e) {
                        ErrorPrint("Invalid buffer number token (" + token + ") in " + configuration_file + " for " + buffer_name);
                        e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                    }
                    if (config_debug_on) {
                        DebugPrint("buffer_number = " + buffer_number);
                    }
                }
                if (token_number > 7) {
                    int port_index = -1;
                    if (-1 != (port_index = token.indexOf("TCP="))) {
                        protocol_option = NML_TCP_PROTOCOL_TYPE;
                        String port_string = token.substring(port_index + 4);
                        if (config_debug_on) {
                            DebugPrint("Setting port number: port_string = " + port_string);
                        }
                        try {
                            port = ((Integer.valueOf(port_string)).intValue());
                        } catch (Exception e) {
                            ErrorPrint("Invalid port token (" + token + ") in " + configuration_file + " for " + buffer_name);
                            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                        }
                        if (config_debug_on) {
                            DebugPrint("port = " + port);
                        }
                    }
                    if (-1 != (port_index = token.indexOf("diag"))) {
                        diag_enabled = true;
                    } else if (-1 != (port_index = token.indexOf("DIAG"))) {
                        diag_enabled = true;
                    }
                    if (-1 != (port_index = token.indexOf("STCP="))) {
                        protocol_option = NML_STCP_PROTOCOL_TYPE;
                    }
                    if (-1 != (port_index = token.indexOf("UDP="))) {
                        protocol_option = NML_UDP_PROTOCOL_TYPE;
                        String port_string = token.substring(port_index + 4);
                        if (config_debug_on) {
                            DebugPrint("Setting port number: port_string = " + port_string);
                        }
                        try {
                            port = ((Integer.valueOf(port_string)).intValue());
                        } catch (Exception e) {
                            ErrorPrint("Invalid port token (" + token + ") in " + configuration_file + " for " + buffer_name);
                            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                        }
                        if (config_debug_on) {
                            DebugPrint("port = " + port);
                        }
                    }

                    if (0 == token.compareTo("packedl64")) {
                        data_format_option = NML_PACKEDL64_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("PACKEDL64")) {
                        data_format_option = NML_PACKEDL64_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("packed")) {
                        data_format_option = NML_PACKED_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("PACKED")) {
                        data_format_option = NML_PACKED_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("xdr")) {
                        data_format_option = NML_XDR_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("XDR")) {
                        data_format_option = NML_XDR_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("xml")) {
                        data_format_option = NML_XML_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("XML")) {
                        data_format_option = NML_XML_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("disp")) {
                        data_format_option = NML_DISP_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("DISP")) {
                        data_format_option = NML_DISP_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("ascii")) {
                        data_format_option = NML_ASCII_ENCODING_TYPE;
                        encoding_type_set = true;
                    } else if (0 == token.compareTo("ASCII")) {
                        data_format_option = NML_ASCII_ENCODING_TYPE;
                        encoding_type_set = true;
                    }


                    int version_index = token.toUpperCase().indexOf("VERSION=");
                    if (version_index >= 0) {
                        try {
                            String version_string = token.substring(version_index + 8);
                            min_compatible_version = Double.valueOf(version_string).doubleValue();
                        } catch (Exception e) {
                            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                        }
                    }
                    int confirm_write_index = token.toUpperCase().indexOf("CONFIRM_WRITE");
                    if (confirm_write_index >= 0) {
                        confirm_write = true;
                    }
                }
                token_number++;

            }
            if (!encoding_type_set && BufferLineFound) {
                ErrorPrint("Encoding type not set for " + buffer_name + " in " + configuration_file);
                ErrorPrint("add xdr, xml, disp or packed explicitly to the end of the buffer line.");
            }

            int broadcast_port_index = current_line.toUpperCase().indexOf("BROADCAST_PORT=");
            if (broadcast_port_index >= 0) {
                String broadcast_port_string = current_line.substring(broadcast_port_index + 15);
                if (config_debug_on) {
                    DebugPrint("broadcast_port_string =" + broadcast_port_string);
                }
                StringTokenizer t2 = new StringTokenizer(broadcast_port_string, "\r\n \t");
                String token2 = t2.nextToken();
                broadcast_port = Integer.valueOf(token2).intValue();
                if (config_debug_on) {
                    DebugPrint(" =" + broadcast_port);
                }
            }// end while loop parsing tokens on on line
            if (BufferLineFound) {
                if (buffer_size > 0) {
                    long fm = Runtime.getRuntime().freeMemory();
                    if (Runtime.getRuntime().maxMemory() > 0
                            && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
                        fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
                    }
                    if (buffer_size * 2 + MEM_SAVE_SIZE > fm) {
                        Runtime.getRuntime().gc();
                        fm = Runtime.getRuntime().freeMemory();
                        if (Runtime.getRuntime().maxMemory() > 0
                                && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
                            fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
                        }
                    }
                    if (config_debug_on) {
                        DebugPrint("buffer_size = " + buffer_size + " free memory=" + fm);
                    }
                    if (this.allocation_size_max <= 0 && buffer_size * 2 + MEM_SAVE_SIZE > fm) {
                        ErrorPrint("buffer_size=" + buffer_size + ", freeMemory()=" + fm + ", free memory required = " + ((2 * buffer_size) + MEM_SAVE_SIZE));
                        ErrorPrint("Consider using -Xmx to increase memory available to java. (ie java -Xmx256m to increase to 256Mb.)");
                        input_buffer = null;
                        output_buffer = null;
                        throw create_NMLException(" !ERROR! Buffer size(" + buffer_size + ") is too big for this system.\n\tFree Memory :" + fm);
                    }
                    input_buffer = null; //new byte[buffer_size];
                    output_buffer = null; // new byte[buffer_size];
                }
                switch (data_format_option) {
                    case NML_XDR_ENCODING_TYPE:
                        SetFormatConverter(new XDRFormatConverter());
                        break;

                    case NML_PACKED_ENCODING_TYPE:
                        SetFormatConverter(new PackedFormatConverter(false));
                        break;

                    case NML_PACKEDL64_ENCODING_TYPE:
                        SetFormatConverter(new PackedFormatConverter(true));
                        break;

                    case NML_XML_ENCODING_TYPE:
                        SetFormatConverter(new XMLFormatConverter());
                        break;

                    case NML_DISP_ENCODING_TYPE:
                        SetFormatConverter(new DISPFormatConverter());
                        break;

                    default:
                        ErrorPrint("Invalid data_format_option.");
                        SetFormatConverter(new PackedFormatConverter(false));
                        break;
                }
                if (null != buf_info) {
                    buf_info.used = true;
                }
            } else {
                if (!ConfigurationFileRead) {
                    throw create_NMLException("Can not read configuration file.");
                } else {
                    throw create_NMLException("Can not find  buffer line :default_buf_info=" + default_buf_info);
                }
            }
            if (!ProcessLineFound && null != process_name && !process_name.equals("jdiag")) {
                ErrorPrint("NMLConnection : Can not find process line for " + process_name + " connecting to " + buffer_name + " in " + configuration_file + ". buf_info=" + buf_info + ", default_buf_info=" + default_buf_info);
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("Error reading NML configuration file " + configuration_file);
            ErrorPrint("BufferName = " + buffer_name);
            ErrorPrint("ProcessName = " + process_name);
            if (null != current_line) {
                ErrorPrint("last line read = " + current_line);
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            throw create_NMLException(" !ERROR! Misc. Error", e);
        }
    }

    protected void finalize() {
        disconnect();
    }
    private long last_connect_err_time = 0;
    private static int total_connect_errors = 0;
    private long last_recheck_nmlcfgsvr_time = 0;
    private static String last_failed_connect_host = null;
    private int last_failed_connect_port = 0;
    private int allocation_size_max = -1;

    private void AllocateBuffers() throws Exception {
        long fm = Runtime.getRuntime().freeMemory();
        if (Runtime.getRuntime().maxMemory() > 0
                && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
            fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
        }
        int allocation_size = buffer_size;
        if (allocation_size > this.allocation_size_max && this.allocation_size_max > 0) {
            allocation_size = this.allocation_size_max;
        }
        if (allocation_size * 2 + MEM_SAVE_SIZE > fm) {
            Runtime.getRuntime().gc();
            fm = Runtime.getRuntime().freeMemory();
            if (Runtime.getRuntime().maxMemory() > 0
                    && Runtime.getRuntime().maxMemory() > Runtime.getRuntime().totalMemory()) {
                fm += (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory());
            }
        }
        if (config_debug_on) {
            DebugPrint("allocation_size = " + allocation_size + " free memory=" + fm);
        }
        if (allocation_size * 2 + MEM_SAVE_SIZE > fm) {
            ErrorPrint("buffer_size=" + buffer_size + ", freeMemory()=" + fm + ", free memory required = " + ((2 * buffer_size) + MEM_SAVE_SIZE));
            ErrorPrint("Consider using -Xmx to increase memory available to java. (ie java -Xmx256m to increase to 256Mb.)");
            input_buffer = null;
            output_buffer = null;
            buffer_size = (int) ((fm - MEM_SAVE_SIZE) / 2);
            if (buffer_size < 0) {
                buffer_size = 0;
            }
            throw create_NMLException(" !ERROR! Buffer size is too big for this system.");
        }
        int input_size_to_allocate = allocation_size;
        if (write_only && allocation_size > 2048) {
            input_size_to_allocate = 2048;
        }
        if (null == input_buffer || input_buffer.length != input_size_to_allocate) {
            input_buffer = new byte[input_size_to_allocate];
        }
        if (null == output_buffer) {
            if (read_only && allocation_size > 2048) {
                output_buffer = new byte[2048];
            } else {
                output_buffer = new byte[buffer_size];
            }
        }
        if (null == input_buffer) {
            output_buffer = null;
            throw create_NMLException("!ERROR! input buffer is null.");
        }
        if (null == output_buffer) {
            input_buffer = null;
            throw create_NMLException("!ERROR! output buffer is null.");
        }
    }
    private boolean bad_host_connect_exception_thrown = false;
    private boolean bad_port_connect_exception_thrown = false;
    private boolean bad_protocol_option_connect_exception_thrown = false;

    private void tcpWrite() throws IOException {
        baOutputStream.flush();
        byte ba[] = baOutputStream.toByteArray();
        this.tcpSocketChannel.write(ByteBuffer.wrap(ba));
        baOutputStream.reset();
    }

    private int tcpAvailable() throws IOException {
        return this.tcpSocketChannel.socket().getInputStream().available();
    }
    private int old_data = 0;

    private int tcpPollRead(int len) throws IOException, NMLException {
        if (poll && tcpAvailable() < len) {
            return 0;
        }
        return tcpRead(len);
    }

    private int tcpRead(int len) throws IOException, NMLException {
        if (old_data == 0) {
            baInputStream.reset();
        }
        long t = System.currentTimeMillis();
        long end_time = max_tries * 10 + t + 1;
        int bytes_read = old_data;
        while ((max_tries < 0 || t < end_time) && bytes_read < len) {
            if (max_tries < 0) {
                inputSelector.select();
            } else {
                inputSelector.select(end_time - t);
            }
            int read_ret = this.tcpSocketChannel.read(ByteBuffer.wrap(input_buffer, bytes_read, len - bytes_read));
            if (read_ret < 0) {
                return read_ret;
            }
            bytes_read += read_ret;
        }
        if (bytes_read != len) {
            long time_diff = t - end_time + max_tries * 10 + 1;
            old_data = bytes_read;
            throw create_NMLException(" !ERROR! Timeout Error tcpRead(" + len + "), bytes_read=" + bytes_read + ", time_diff=" + time_diff + ", available=" + tcpAvailable());
        }
        old_data = 0;
        return bytes_read;
    }

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
     * @throws rcs.nml.NMLException when connection fails
     */
    synchronized public int connect() throws NMLException {
        try {
            disconnect();
            connected = false;
            if (bad_host) {
                if (!this.bad_host_connect_exception_thrown) {
                    return -1;
                } else {
                    this.bad_host_connect_exception_thrown = true;
                    throw create_NMLException(" !ERROR! bad_host=true");
                }
            }

            connect_time = System.currentTimeMillis();
            if (using_nmlcfgsvr
                    && connect_time - last_recheck_nmlcfgsvr_time > 2000) {
                ReadNMLConfigurationFile();
                last_recheck_nmlcfgsvr_time = System.currentTimeMillis();
                AllocateBuffers();
            } else {
                AllocateBuffers();
            }
            connect_time = System.currentTimeMillis();

            read_request_sent = false;
            if (port <= 0) {
                if (!this.bad_port_connect_exception_thrown) {
                    return -1;
                } else {
                    this.bad_port_connect_exception_thrown = true;
                    throw create_NMLException(" !ERROR! port=" + port);
                }
            }
            if (connected) {
                disconnect();
            }


            if (config_debug_on) {
                DebugPrint("Connecting (port=" + port + ", protocol_option=" + protocol_option + ", host = " + host + ") . . .");
                DebugPrint("buf_info=" + buf_info);
                DebugPrint("force_socket_so_timeout=" + force_socket_so_timeout);
                //Thread.dumpStack();
            }

            /* ****************************************************************** */
            // javac now gives a warning if this Socket constructor is used,
            // saying the method has been deprecated.
            // m_Socket = new Socket(host,port,true);
	    /* ******************************************************************* */
            switch (protocol_option) {
                case NML_STCP_PROTOCOL_TYPE:
                    if (null == m_SocketAddress) {
                        m_SocketAddress = new java.net.InetSocketAddress(host, port);
                    }
                    m_Socket.setSoTimeout(this.force_socket_so_timeout);
                    m_Socket.setTcpNoDelay(true);
                    m_Socket.connect(m_SocketAddress, force_socket_so_timeout);
                    if (!this.m_Socket.isConnected()) {
                        m_Socket.close();
                        m_Socket = null;
                        m_SocketAddress = null;
                        last_connect_err_time = System.currentTimeMillis();
                        total_connect_errors++;
                        throw create_NMLException(" !ERROR! Connect to host=" + host + ",port=" + port + " timed out after " + force_socket_so_timeout + "ms. .");
                    }
                    this.m_InputStream = new DataInputStream(m_Socket.getInputStream());
                    this.m_OutputStream = new DataOutputStream(m_Socket.getOutputStream());
                    break;

                case NML_TCP_PROTOCOL_TYPE:

                    if (null == m_SocketAddress) {
                        m_SocketAddress = new java.net.InetSocketAddress(host, port);
                    }
                    this.tcpSocketChannel = SocketChannel.open();
                    this.tcpSocketChannel.configureBlocking(false);
                    this.m_Socket = this.tcpSocketChannel.socket();
                    this.m_Socket.setTcpNoDelay(true);
                    Selector connectSelector = SelectorProvider.provider().openSelector();
                    SelectionKey sk = tcpSocketChannel.register(connectSelector, SelectionKey.OP_CONNECT);
                    if (!this.tcpSocketChannel.connect(m_SocketAddress)) {
                        if (force_socket_so_timeout > 0) {
                            connectSelector.select(force_socket_so_timeout);
                        } else {
                            connectSelector.select();
                        }
                        this.tcpSocketChannel.finishConnect();
                    }
                    sk.cancel();
                    connectSelector.close();

//                        while ((System.currentTimeMillis() - start_connect_time < 2 * force_socket_so_timeout || tries < 2)
//                                && (m_Socket == null || !m_Socket.isConnected())) {
//                            if (Thread.interrupted()) {
//                                Thread.currentThread().interrupt();
//                                throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                            }
//                            try {
//                                m_Socket.connect(m_SocketAddress, force_socket_so_timeout);
//                                if (force_socket_so_timeout > 0
//                                        && null != m_Socket
//                                        && m_Socket.getSoTimeout() != force_socket_so_timeout) {
//                                    m_Socket.setSoTimeout(force_socket_so_timeout);
//                                }
//                            } catch (java.net.ConnectException e) {
//                                throw create_NMLException(" !ERROR! Connect to host=" + host + ",port=" + port + " failed.", e);
//                            } catch (Exception e) {
//                                long time_diff = System.currentTimeMillis() - last_connect_err_time;
//                                if (tries == 0
//                                        && time_diff > 10000
//                                        && total_connect_errors < 100) {
//                                    ErrorPrint("force_socket_so_timeout=" + force_socket_so_timeout + ", port=" + port + ", host=" + host + ", m_Socket=" + m_Socket);
//                                    e.printStackTrace();
//                                }
//                            }
//                            if (Thread.interrupted()) {
//                                Thread.currentThread().interrupt();
//                                throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                            }
//                            Thread.sleep(10);
//                            tries++;
//                        }
                    if (!this.tcpSocketChannel.isConnected()) {
                        m_SocketAddress = null;
                        last_connect_err_time = System.currentTimeMillis();
                        total_connect_errors++;
                        throw create_NMLException(" !ERROR! Connect to host=" + host + ",port=" + port + " timed out after " + force_socket_so_timeout + "ms. .");
                    }
//                    } else {
//                        // No timeout.
////						long start_connect_time = System.currentTimeMillis();
////						int tries = 0;
////						while ((System.currentTimeMillis() - start_connect_time < 2 * force_socket_so_timeout || tries < 2 || force_socket_so_timeout < 0) &&
////								!m_Socket.isConnected()) {
//                        //System.out.println("Thread.currentThread() = " + Thread.currentThread());
//                        if (Thread.interrupted()) {
//                            Thread.currentThread().interrupt();
//                            throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                        }
//                        try {
//                            // We would like to switch to SocketChannel so thread.interrupt would interrupt it however it seems
//                            // to cause later operations to timeout.
////								socket_channel = SocketChannel.open();
////								socket_channel.connect(m_SocketAddress);
////								socket_channel.finishConnect();
////								m_Socket = socket_channel.socket();
////								m_Socket.setTcpNoDelay(true);
//                            m_Socket = new Socket();
//                            m_Socket.setTcpNoDelay(true);
//                            while (!m_Socket.isConnected()) {
//                                m_Socket.connect(m_SocketAddress, 2000);
//                                if (Thread.interrupted()) {
//                                    Thread.currentThread().interrupt();
//                                    throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                                }
//                            }
//                        } catch (Exception e) {
//                            m_Socket = null;
//                            if (Thread.interrupted()) {
//                                Thread.currentThread().interrupt();
//                                throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                            }
//                            throw this.create_NMLException("Failed to connect to " + m_SocketAddress + " : host=" + host + ", port=" + port, e);
//                        }
//                        if (Thread.interrupted()) {
//                            Thread.currentThread().interrupt();
//                            throw create_NMLException(" !ERROR! Thread.interrupted() returned true. Connect to host=" + host + ",port=" + port + " failed.");
//                        }
//
//                        if (!m_Socket.isConnected()) {
//                            m_SocketAddress = null;
//                            throw create_NMLException(" !ERROR! Connect to host=" + host + ",port=" + port + " timed out after " + force_socket_so_timeout + "ms. .");
//                        }
//                    }

//                    m_OutputStream = new DataOutputStream(m_Socket.getOutputStream());
//                    m_InputStream = new DataInputStream(m_Socket.getInputStream());
//					m_OutputStream = new DataOutputStream(m_Socket.getOutputStream());
//					m_InputStream = new DataInputStream(m_Socket.getInputStream());
                    inputSelector = SelectorProvider.provider().openSelector();
                    this.tcpSocketChannel.register(inputSelector, SelectionKey.OP_READ);
                    baOutputStream = new ByteArrayOutputStream();
                    baInputStream = new ByteArrayInputStream(input_buffer);
                    m_OutputStream = new DataOutputStream(baOutputStream);
                    m_InputStream = new DataInputStream(baInputStream);
                    break;

                case NML_UDP_PROTOCOL_TYPE:
                    if (broadcast_port > 0 && subscription_period > 1E-4 && subscription_period < 600.0) {
                        udpSocket = new NonBlockingDatagramSocket(broadcast_port);
                    } else {
                        udpSocket = new NonBlockingDatagramSocket();
                    }
                    udpServerAddress = InetAddress.getByName(host);
                    baOutputStream = new ByteArrayOutputStream();
                    baInputStream = new ByteArrayInputStream(input_buffer);
                    m_OutputStream = new DataOutputStream(baOutputStream);
                    m_InputStream = new DataInputStream(baInputStream);
                    udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                    break;

                default:
                    if (!this.bad_protocol_option_connect_exception_thrown) {
                        return -1;
                    } else {
                        this.bad_protocol_option_connect_exception_thrown = true;
                        throw create_NMLException(" !ERROR! protocol_option=" + protocol_option);
                    }
            }
            write_reply_received = true;
            read_request_sent = false;
            input_buffer_ready = false;
            input_bytes_read = 0;
            last_msg_read = null;
            message_size = 0;
            serial_number = 0;

            /*set = new SocketSet();
            set.port = port;
            set.m_Socket = m_Socket;
            set.m_OutputStream = m_OutputStream;
            set.m_InputStream = m_InputStream;
            set.count = 1;
            m_socketSetHashtable.put(portInteger, set); */
            connected = true;
            if (read_debug_on || write_debug_on || config_debug_on) {
                rcs.nml.debugInfo.debugPrintStream.print("Socket openned to " + host + ":" + port + ".\r\n");
            }
            if (diag_enabled) {
                if (setDiagInfo() < 0) {
                    throw create_NMLException(" !ERROR! Couldn't send diagnostics info");
                }
            }
            if (subscription_period > 1E-4 && subscription_period < 600.0) {
                if (setSubscriptionPeriod(subscription_period) < 0) {
                    throw create_NMLException(" !ERROR! Subscription error");
                }
            }
            connect_time = System.currentTimeMillis();
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (java.net.SocketException e) {
            if (!host.equals(last_failed_connect_host) || last_failed_connect_port != port) {
                e.printStackTrace();
                ErrorPrint("force_socket_so_timeout=" + force_socket_so_timeout);
                ErrorPrint("\r\nCan't connect to port " + port + " on host " + host + "\r\n");
                last_failed_connect_host = host;
                last_failed_connect_port = port;
                throw create_NMLException(" !ERROR! Can't connect to port " + port + " on host " + host + "\r\n", e);
            }
            return (-1);
        } catch (Exception e) {
            e.printStackTrace();
            ErrorPrint("force_socket_so_timeout=" + force_socket_so_timeout);
            ErrorPrint("\r\nCan't connect to port " + port + " on host " + host + "\r\n");
            throw create_NMLException(" !ERROR! Can't connect to port " + port + " on host " + host + "\r\n", e);
        }
        return 0;
    }

    /**
     * Disconnect this object from the NML server. This
     * allows the NML server to shutdown and come back up
     * while preserving the configuration information in this
     * object.
     *
     * All reads and writes will fail while the object is disconnected.
     */
    public void disconnect() {
        try {
            old_data = 0;
            //final StackTraceElement ste_array[] = Thread.currentThread().getStackTrace();
            this.disconnect_stack_trace = StackTracePrinter.ThrowableToStackTraceString(new Throwable());
            //this.disconnect_stack_trace = this.disconnect_stack_trace.substring(this.disconnect_stack_trace.indexOf(" at"), 1);
            //            for (int i = 0; i < ste_array.length; i++) {
//                final StackTraceElement ste = ste_array[i];
//                disconnect_stack_trace += "\t" + ste.getClassName() + "." + ste.getMethodName();
//                if (null != ste.getFileName()) {
//                    disconnect_stack_trace += "(" + ste.getFileName() + ":" + ste.getLineNumber() + ")";
//                }
//                disconnect_stack_trace += "\n";
//            }
            input_buffer = null;
            output_buffer = null;
            disconnect_time = System.currentTimeMillis();
            read_request_sent = false;

            if (protocol_option == NML_UDP_PROTOCOL_TYPE
                    && use_subscription && subscription_id > 0) {
                cancelUDPSubscription();
            }

            if (connected) {
                if (m_OutputStream != null) {
                    m_OutputStream.close();
                    m_OutputStream = null;
                }
                if (m_InputStream != null) {
                    m_InputStream.close();
                    m_InputStream = null;
                }
//                if (m_Socket != null) {
//                    m_Socket.close();
//                    m_Socket = null;
//                }
                if (udpSocket != null) {
                    udpSocket.close();
                    udpSocket = null;
                }
                if (null != this.inputSelector) {
                    this.inputSelector.close();
                    this.inputSelector = null;
                }
                if (null != this.m_Socket) {
                    this.m_Socket.close();
                    this.m_Socket = null;
                }
                if (null != this.tcpSocketChannel) {
                    this.tcpSocketChannel.close();
                    this.tcpSocketChannel = null;
                }
                if (null != baOutputStream) {
                    baOutputStream.reset();
                }
                baOutputStream = null;
                baInputStream = null;
                if (read_debug_on || write_debug_on || config_debug_on) {
                    rcs.nml.debugInfo.debugPrintStream.print("Socket " + port + " closed.\r\n");
                }
            }
            disconnect_time = System.currentTimeMillis();
            connected = false;
        } catch (Exception e) {
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
        }
    }

    /**
     * When the NMLConnection performs a verify bufname (which it normally
     * does in the constructor) it requests the buffername from
     * the server that corresponds to the selected port and buffer number.
     * If this is not the same as the buffer passed to the constructor it indicates
     * that configuration used by the server is inconsistant with the one being used by this process.
     * @return true if the host could be contacted and returned the expected buffername, or false if the
     * server could not be contacted or the buffernames do not match.
     */
    synchronized public boolean verify_bufname() {
        try {
            if (Thread.interrupted() || !connected) {
                return false;
            }
            if (read_debug_on) {
                DebugPrint("NMLConnection.read() called for buffer (" + buffer_name + ")");
            }
            switch (protocol_option) {
                case NML_TCP_PROTOCOL_TYPE:
                    return verify_bufnameTCP();

                case NML_UDP_PROTOCOL_TYPE:
                    return true; // hack

                case NML_STCP_PROTOCOL_TYPE:
                    return true; // fixme actually do the check

                default:
                    ErrorPrint("NMLConnection.verify_bufname(): Invalid protocol_option = " + protocol_option + " -- buffer_name = " + buffer_name);
                    return false;
            }
            //          return null;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Read a NMLmsg.
     *
     * @return null if the message in the
     * buffer has already been read by this NMLConnection, otherwise it
     * returns the NMLmsg read.
     *
     * @exception  rcs.nml.NMLException
     *            The read failed (ussually because of some network error).
     *
     */
    synchronized public NMLmsg read() throws NMLException {
        if (Thread.interrupted() || !connected) {
            return null;
        }
        if (write_only) {
            throw create_NMLException(" !ERROR! Attempt to read write_only buffer.");
        }
        if (read_debug_on) {
            DebugPrint("NMLConnection.read() called for buffer (" + buffer_name + ")");
        }
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return readTCP();

            case NML_UDP_PROTOCOL_TYPE:
                return readUDP();

            case NML_STCP_PROTOCOL_TYPE:
                if (null == format_converter) {
                    return null;
                }
                NMLmsg temp = format_converter.convertStringToMsg(readSTCPDataString());
                if (format_converter.error_in_update) {
                    throw create_NMLException(" !ERROR! Format Error");
                }
                return temp;

            default:
                ErrorPrint("NMLConnection.read(): Invalid protocol_option = " + protocol_option + " -- buffer_name = " + buffer_name);
                return null;
        }
        //          return null;
    }

    /**
     * Send additional diagnostics info to server so it can better track which process
     * is doing what.
     *
     * @return 0 for success, -1 for error
     */
    public int setDiagInfo() {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return setTCPDiagInfo();

            default:
                break;
        }
        return -1;
    }

    protected int setTCPDiagInfo() {
        try {
            if (null == m_OutputStream || null == m_InputStream) {
                return -1;
            }
            m_OutputStream.writeInt(serial_number);
            serial_number++;
            request_type = REMOTE_CMS_SET_DIAG_INFO_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            int pad = 0;
            m_OutputStream.writeInt(pad);
            m_OutputStream.writeInt(pad);
            byte bname[] = new byte[32];
            String temp_process_name = process_name;
            if (temp_process_name.length() > 15) {
                temp_process_name = temp_process_name.substring(0, 15);
            }
            m_OutputStream.writeBytes(temp_process_name);

            int extra_bytes = 16 - temp_process_name.length();
            if (config_debug_on) {
                DebugPrint("setTCPDiagInfo(): temp_process_name=" + temp_process_name + ", extra_bytes=" + extra_bytes);
            }
            if (extra_bytes > 0) {
                m_OutputStream.write(bname, 0, extra_bytes);
            }
            String host_sysinfo = "";
            try {
                InetAddress local_host = InetAddress.getLocalHost();
                if (null != local_host) {
                    String host_name = local_host.getHostName();
                    if (null != host_name) {
                        host_sysinfo += host_name + ", ";
                    }
                }
            } catch (Exception e) {
                e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            }
            try {
                String java_version = System.getProperty("java.version");
                if (null != java_version) {
                    if (Character.isDigit(java_version.charAt(0))) {
                        java_version = "Java " + java_version;
                    }
                    host_sysinfo += java_version + ", ";
                }
                String os_name = System.getProperty("os.name");
                if (null != os_name) {
                    host_sysinfo += os_name + " ";
                }
                String os_version = System.getProperty("os.version");
                if (null != os_version) {
                    host_sysinfo += os_version + ", ";
                }
                String os_arch = System.getProperty("os.arch");
                if (null != os_arch) {
                    host_sysinfo += os_arch;
                }
            } catch (Exception e) {
                e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            }
            if (host_sysinfo.length() > 31) {
                host_sysinfo = host_sysinfo.substring(0, 31);
            }
            m_OutputStream.writeBytes(host_sysinfo);
            if (host_sysinfo.length() < 32) {
                m_OutputStream.write(bname, host_sysinfo.length(), 32 - host_sysinfo.length());
            }

            int pid = 0;
            m_OutputStream.writeInt(pid);
            m_OutputStream.writeInt(connection_number);
            double rcslib_ver = 0.0;
            try {
                //rcslib_ver = Double.valueOf(rcs.RCS_VERSION.version_string).doubleValue();
                rcslib_ver = 2004.2;
            } catch (Exception e) {
                e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            }
            m_OutputStream.writeDouble(rcslib_ver);
            int reverse_flag = 0x01020304;
            m_OutputStream.writeInt(reverse_flag);
            m_OutputStream.flush();
            this.tcpWrite();
            return 0;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    /**
     * Setup a single variable log.
     * Occationally it is useful to read only a single variable from a large message and
     * there for reduce the amount of bandwidth required to log that variable.
     *
     * @param varname  name of the variable to log
     * @param maxlogsize maximum number of variable updates to store
     * @param period time in seconds between checks to see if the variable changed.
     * @param type type of expected message, no data is logged when other message types are in the buffer.
     * @return -1 if an error occurs, positive integer id of the log otherwise
     */
    synchronized public int setupSingleVarLog(String varname, int maxlogsize, double period, int type) {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return setupTCPSingleVarLog(varname, maxlogsize, period, type);

            case NML_UDP_PROTOCOL_TYPE:
            //return setUDPSubscriptionPeriod(period);

            default:
                ErrorPrint("No single var logs allowed for protocol_option " + protocol_option);
                break;
        }
        return -1;
    }

    private int setupTCPSingleVarLog(String varname, int maxlogsize, double period, int type) {
        try {
            if (null == m_OutputStream || null == m_InputStream) {
                return -1;
            }
            int poll_interval_millis = (int) (period * 1000.0);
            input_buffer_ready = false;
            m_OutputStream.writeInt(serial_number);
            serial_number++;
            request_type = REMOTE_CMS_SETUP_SINGLE_VAR_LOG_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            m_OutputStream.writeInt(0);
            m_OutputStream.writeInt(0);
            byte byte_buf[] = new byte[256];
            byte vbytes[] = varname.getBytes();
            for (int i = 0; i < 255 && i < vbytes.length; i++) {
                byte_buf[i] = vbytes[i];
            }
            for (int i = vbytes.length; i < 255; i++) {
                byte_buf[i] = 0;
            }
            byte_buf[255] = 0;
            m_OutputStream.write(byte_buf);
            m_OutputStream.writeInt(poll_interval_millis);
            m_OutputStream.writeInt(maxlogsize);
            m_OutputStream.writeInt(type);
            this.tcpWrite();
            this.tcpRead(12);
            int returned_serial_number = m_InputStream.readInt();
            if (returned_serial_number != serial_number) {
                ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Can't setupSingleVarLog.");
                return -1;
            }
            m_InputStream.readInt(); // status_code
            int var_log_list_id = m_InputStream.readInt();
            return var_log_list_id;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    /**
     * Get a NMLSingleVarLog object associated with the var_log_number that should have been
     * returned by setupSingleVarLog()
     *
     * @param var_log_number number of var log object
     * @return singleVarLog
     */
    synchronized public NMLSingleVarLog getSingleVarLog(int var_log_number) {
        if (var_log_number < 0) {
            return null;
        }
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return getTCPSingleVarLog(var_log_number);

            case NML_UDP_PROTOCOL_TYPE:
            //return setUDPSubscriptionPeriod(period);

            default:
                ErrorPrint("No single var logs allowed for protocol_option " + protocol_option);
                break;
        }
        return null;
    }
    NMLSingleVarLog svl = null;
    boolean get_single_var_log_request_sent = false;
    boolean get_single_var_log_reply_header_recvd = false;
    boolean get_single_var_log_reply_data_recvd = false;
    int get_single_var_log_items_sent = 0;

    private NMLSingleVarLog getTCPSingleVarLog(int var_log_number) {
        try {
            if (null == m_OutputStream || null == m_InputStream) {
                return null;
            }
            if (var_log_number < 0) {
                throw this.create_NMLException("getTCPSingleVarLog(" + var_log_number + ")");
            }
            if (!get_single_var_log_request_sent) {
                input_buffer_ready = false;
                m_OutputStream.writeInt(serial_number);
                serial_number++;
                request_type = REMOTE_CMS_GET_SINGLE_VAR_LOG_REQUEST_TYPE;
                m_OutputStream.writeInt(request_type);
                m_OutputStream.writeInt(buffer_number);
                m_OutputStream.writeInt(0);
                m_OutputStream.writeInt(var_log_number);
                this.tcpWrite();
                get_single_var_log_request_sent = true;
                get_single_var_log_reply_header_recvd = false;
                get_single_var_log_reply_data_recvd = false;
                get_single_var_log_items_sent = 0;
            }
            if (!get_single_var_log_reply_header_recvd) {
                this.tcpRead(16);
                int returned_serial_number = m_InputStream.readInt();
                if (returned_serial_number != serial_number) {
                    ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    ErrorPrint("Can't getSingleVarLog.");
                    return null;
                }
                m_InputStream.readInt(); // status code
                get_single_var_log_items_sent = m_InputStream.readInt();
                int total_items_logged = m_InputStream.readInt();
                get_single_var_log_request_sent = false;
                get_single_var_log_reply_header_recvd = true;
                get_single_var_log_reply_data_recvd = false;
                if (read_debug_on) {
                    DebugPrint("get_single_var_log_items_sent=" + get_single_var_log_items_sent + ", total_items_logged=" + total_items_logged);
                }
            }
            if (!get_single_var_log_reply_data_recvd) {
                this.tcpRead( 16 * get_single_var_log_items_sent);
                if (svl == null) {
                    svl = new NMLSingleVarLog();
                }
                svl.last_items_sent_size = get_single_var_log_items_sent;
                if (svl.items_list == null
                        || svl.items_list.length < get_single_var_log_items_sent) {
                    svl.items_list = new NMLSingleVarLogItem[get_single_var_log_items_sent];
                }
                for (int i = 0; i < get_single_var_log_items_sent; i++) {
                    if (null == svl.items_list[i]) {
                        svl.items_list[i] = new NMLSingleVarLogItem();
                    }
                    svl.items_list[i].value = m_InputStream.readDouble();
                    svl.items_list[i].timestamp = m_InputStream.readDouble();
                    //System.out.println("i="+i+", svl.items_list[i].value="+svl.items_list[i].value+", svl.items_list[i].timestamp="+svl.items_list[i].timestamp);
                }
                get_single_var_log_request_sent = false;
                get_single_var_log_reply_header_recvd = false;
                get_single_var_log_reply_data_recvd = true;
            }
            // System.out.println("svl="+svl);
            return svl;
        } catch (Exception e) {
            //e.printStackTrace();
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return null;
        }
    }

    /**
     * Stop collecting data on a single variable setup in setupSingleVarLog().
     *
     * @param var_log_number number of log to close
     * @return 0 ok, -1 comm error in sending close request.
     */
    synchronized public int closeSingleVarLog(int var_log_number) {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return closeTCPSingleVarLog(var_log_number);

            case NML_UDP_PROTOCOL_TYPE:
            //return setUDPSubscriptionPeriod(period);

            default:
                ErrorPrint("No single var logs allowed for protocol_option " + protocol_option);
                break;
        }
        return -1;
    }

    private int closeTCPSingleVarLog(int var_log_number) {
        try {
            if (null == m_OutputStream || null == m_InputStream) {
                return -2;
            }
            input_buffer_ready = false;
            m_OutputStream.writeInt(serial_number);
            serial_number++;
            request_type = REMOTE_CMS_CLOSE_SINGLE_VAR_LOG_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            m_OutputStream.writeInt(0);
            m_OutputStream.writeInt(var_log_number);
            this.tcpWrite();
            this.tcpRead(8);
            int returned_serial_number = m_InputStream.readInt();
            if (returned_serial_number != serial_number) {
                ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Can't closeSingleVarLog.");
                return -1;
            }
            m_InputStream.readInt(); // status code
            return 0;
        } catch (Exception e) {
            //e.printStackTrace();
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    /**
     * Tell the server to send this process messages from the buffer every period
     * seconds without needing a request message.
     * @param period time to wait between sending messages
     * @return 0 for success, -1 for error
     */
    public int setSubscriptionPeriod(double period) {

        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return setTCPSubscriptionPeriod(period);

            case NML_UDP_PROTOCOL_TYPE:
                return setUDPSubscriptionPeriod(period);

            default:
                ErrorPrint("No subscriptions allowed for protocol_option " + protocol_option);
                break;
        }
        return -1;
    }

    protected int setUDPSubscriptionPeriod(double period) {
        if (min_compatible_version < 3.13 && min_compatible_version > 1e-6) {
            return 0;
        }
        boolean subscription_set = false;
        int subscription_interval_millis = (int) (period * 1000);
        long start_set_subscription_interval = System.currentTimeMillis();
        reply_header_received = false;
        try {
            while (!subscription_set) {
                if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    if (reply_header_received) {
                        serial_number++;
                    }
                    m_OutputStream.writeInt(serial_number);
                    m_OutputStream.writeInt(CMS_POLLED_SUBSCRIPTION);
                    m_OutputStream.writeInt(subscription_interval_millis);
                    m_OutputStream.writeInt(last_id_read);
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP setSubscription request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();
                    int request_length = 24;
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                } else {
                    baInputStream.reset();
                    int returned_serial_number = m_InputStream.readInt();
                    if (read_debug_on) {
                        DebugPrint("UDP setSubscription reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number);
                    }
                    if (returned_serial_number != serial_number) {
                        serial_number_repeats++;
                        if (read_debug_on) {
                            DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                        }
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        continue;
                    }
                    reply_header_received = true;
                    remote_status = m_InputStream.readInt();
                    subscription_id = m_InputStream.readInt();
                    if (read_debug_on) {
                        DebugPrint("remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                        DebugPrint(" -- buffer_name = " + buffer_name);
                    }
                    udpSocket.lastPacket = null;
                    subscription_set = true;
                    use_subscription = true;
                    continue;
                }
            }
            Thread.sleep(10);
            if (start_set_subscription_interval - System.currentTimeMillis() > 3000) {
                ErrorPrint("Timed out trying to setup the subscription.");
                return -1;
            }
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
        return 0;
    }

    protected int cancelUDPSubscription() {
        if (min_compatible_version < 3.13 && min_compatible_version > 1e-6) {
            return 0;
        }
        boolean subscription_set = false;
        long start_set_subscription_interval = System.currentTimeMillis();
        reply_header_received = false;
        try {
            while (!subscription_set) {
                if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_CANCEL_SUBSCRIPTION_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    if (reply_header_received) {
                        serial_number++;
                    }
                    m_OutputStream.writeInt(serial_number);
                    m_OutputStream.writeInt(subscription_id);
                    m_OutputStream.writeInt(0);
                    m_OutputStream.writeInt(0);
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP setSubscription request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();
                    int request_length = 24;
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                } else {
                    baInputStream.reset();
                    int returned_serial_number = m_InputStream.readInt();
                    if (read_debug_on) {
                        DebugPrint("UDP setSubscription reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number);
                    }
                    if (returned_serial_number != serial_number) {
                        serial_number_repeats++;
                        if (read_debug_on) {
                            DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                        }
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        continue;
                    }
                    reply_header_received = true;
                    remote_status = m_InputStream.readInt();
                    subscription_id = m_InputStream.readInt();
                    if (read_debug_on) {
                        DebugPrint("remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                        DebugPrint(" -- buffer_name = " + buffer_name);
                    }
                    udpSocket.lastPacket = null;
                    subscription_set = true;
                    use_subscription = false;
                    continue;
                }
            }
            Thread.sleep(10);
            if (start_set_subscription_interval - System.currentTimeMillis() > 3000) {
                ErrorPrint("Timed out trying to cancel the subscription.");
                return -1;
            }
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
        return 0;
    }

    protected int setTCPSubscriptionPeriod(double period) {
        try {
            if (null == m_OutputStream || null == m_InputStream) {
                use_subscription = false;
                return -1;
            }
            int poll_interval_millis = (int) (period * 1000.0);
            input_buffer_ready = false;
            m_OutputStream.writeInt(serial_number);
            serial_number++;
            request_type = REMOTE_CMS_SET_SUBSCRIPTION_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            int subscription_type = CMS_POLLED_SUBSCRIPTION;
            m_OutputStream.writeInt(subscription_type);
            m_OutputStream.writeInt(poll_interval_millis);
            this.tcpWrite();
            this.tcpRead(8);
            int returned_serial_number = m_InputStream.readInt();
            if (returned_serial_number != serial_number) {
                ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Can't set subscription period.");
                use_subscription = false;
                return -1;
            }
            int success = m_InputStream.readInt();
            if (success == 0) {
                use_subscription = false;
                return -1;
            }
            if (read_debug_on) {
                DebugPrint("setTCPSubscriptionPeriod(" + period + ") succeeds.");
            }
            use_subscription = true;
            return 0;
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            use_subscription = false;
            return -1;
        }
    }

    protected void read_raw_data() throws NMLException {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                readTCP_raw_data();
                break;

            case NML_UDP_PROTOCOL_TYPE:
                readUDP_raw_data();
                break;

            case NML_STCP_PROTOCOL_TYPE:
                if (null == format_converter) {
                    return;
                }
                readSTCPDataString();
                break;

            default:
                ErrorPrint("NMLConnection.read_raw_data(): Invalid protocol_option = " + protocol_option + " -- buffer_name = " + buffer_name);
                return;
        }
        //          return null;
    }

    protected boolean verify_bufnameTCP() throws NMLException {
        if (Thread.interrupted() || !connected || null == m_InputStream || null == m_OutputStream) {
            return false;
        }
        if (port < 1) {
            String buffer_line = null;
            if (buf_info != null) {
                buffer_line = buf_info.buffer_line;
            }
            ErrorPrint("this=" + this + ", port = " + port);
            throw create_NMLException(" !ERROR! Invalid Port.", buffer_line);
        }
        if (null == format_converter) {
            throw create_NMLException(" !ERROR! No Format Converter.");
        }
        if (null == m_OutputStream || null == m_InputStream) {
            if (!null_error_reported) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
            }
            null_error_reported = true;
            throw create_NMLException(" !ERROR! No input or output stream.");
        }
        try {
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                if (!write_reply_received) {
                    input_buffer_ready = false;
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.readTCP() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    input_bytes_ready = m_InputStream.available();
                    if (input_bytes_ready < 12) {
                        return false;
                    }
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.readTCP() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            input_buffer_ready = false;
            m_OutputStream.writeInt(serial_number);
            request_type = REMOTE_CMS_GET_BUF_NAME_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            m_OutputStream.writeInt(0);
            m_OutputStream.writeInt(0);
            if (read_debug_on) {
                DebugPrint("TCP verify_buf request sent.");
                DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
            }
            serial_number++;
            m_OutputStream.flush();
            this.tcpWrite();
            if (this.tcpRead(40) != 40) {
                return false;
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
            input_bytes_ready = m_InputStream.available();
            reply_header_received = false;
            if (read_debug_on) {
                DebugPrint("NMLConnection.verify_bufnameTCP() getting reply_header.");
            }
//            int tries = 0;
//            int vb_max_tries = max_tries;
//            if (vb_max_tries < 200) {
//                vb_max_tries = 200;
//            }
//            while ((tries < vb_max_tries || vb_max_tries < 0)
//                    && input_bytes_ready < 40) {
//                if (tries > 0 && read_debug_on) {
//                    DebugPrint("tries = " + tries);
//                }
//                Thread.sleep(10);
//                if (Thread.interrupted() || !connected || null == m_InputStream || null == m_OutputStream) {
//                    return false;
//                }
//                if (null == m_InputStream) {
//                    throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
//                }
//                input_bytes_ready = m_InputStream.available();
//                tries++;
//            }
//            if (tries >= vb_max_tries && vb_max_tries > 0) {
//                throw create_NMLException(" !ERROR! Timeout Error (tries=" + tries + ", vb_max_tries=" + vb_max_tries + ",timeout = " + (((double) vb_max_tries) / 100.0) + ")., input_bytes_ready=" + input_bytes_ready);
//            }
            int returned_serial_number = m_InputStream.readInt();
            if (use_subscription) {
                serial_number++;
            }
            if (returned_serial_number != serial_number) {
                throw this.create_NMLException("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
            }
            remote_status = m_InputStream.readInt();
            byte returned_bufname_bytes[] = new byte[32];
            m_InputStream.read(returned_bufname_bytes, 0, 32);
            bufname_returned = new String(returned_bufname_bytes);
            bufname_returned = bufname_returned.trim();
            if (read_debug_on || config_debug_on) {
                DebugPrint("TCP verify_bufname reply received.");
                DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                DebugPrint("buffer_number=" + buffer_number);
                DebugPrint("buffer_name = " + buffer_name);
                DebugPrint("bufname_returned = " + bufname_returned);
                DebugPrint("last_id_read=" + last_id_read);
                DebugPrint("buffer_name.length()=" + buffer_name.length());
                DebugPrint("bufname_returned.length()=" + bufname_returned.length());
                DebugPrint("bufname_returned{" + bufname_returned + "}.compareTo(buffer_name{" + buffer_name + "}) = " + bufname_returned.compareTo(buffer_name));
            }
            input_bytes_read = 0;
            reply_header_received = true;
            if (buffer_name.length() < 32) {
                if (bufname_returned.compareTo(buffer_name) == 0) {
                    return true;
                }
            } else if (buffer_name.substring(0, 31).equals(bufname_returned)) {
                return true;
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
        return false;
    }

    protected void readTCP_raw_data() throws NMLException {

        if (port < 1) {
            String buffer_line = null;
            if (buf_info != null) {
                buffer_line = buf_info.buffer_line;
            }
            ErrorPrint("this=" + this + ", port = " + port);
            throw create_NMLException(" !ERROR! Invalid Port.", buffer_line);
        }
        if (null == format_converter) {
            throw create_NMLException(" !ERROR! No Format Converter.");
        }
//        if (null == m_OutputStream || null == m_InputStream) {
//            if (!null_error_reported) {
//                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
//                ErrorPrint("Stream is null.");
//            }
//            null_error_reported = true;
//            throw create_NMLException(" !ERROR! No input or output stream.");
//        }
        try {
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                if (!write_reply_received) {
                    input_buffer_ready = false;
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.readTCP() -- Still have not received write reply for buffer " + buffer_name);
                    }
//                    if (null == m_InputStream) {
//                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
//                    }
                    this.tcpRead(12);
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.readTCP() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            if (!read_request_sent && !use_subscription) {
                input_buffer_ready = false;
                m_OutputStream.flush();
                m_OutputStream.writeInt(serial_number);
                request_type = REMOTE_CMS_READ_REQUEST_TYPE;
                m_OutputStream.writeInt(request_type);
                m_OutputStream.writeInt(buffer_number);
                access_type = CMS_READ_ACCESS;
                m_OutputStream.writeInt(access_type);
                m_OutputStream.writeInt(last_id_read);
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                reply_header_received = false;
                if (read_debug_on) {
                    DebugPrint("TCP read request sent.");
                    DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                    DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                }
                serial_number++;
                tcpWrite(); // flush whatever was writtent to m_OutputStream out to TCP
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
//            input_bytes_ready = this.tcpAvailable();
            readRawDataFromTCPSocket();
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    protected NMLmsg readTCP() throws NMLException {
        readTCP_raw_data();
        if (!input_buffer_ready) {
            return null;
        }
        NMLmsg temp = format_converter.convertRawDataToMsg(input_buffer, 0, message_size);
        if (format_converter.error_in_update) {
            throw create_NMLException(" !ERROR! Format error");
        }
        return temp;
    }

    protected NMLmsg readUDP() throws NMLException {
        readUDP_raw_data();
        if (!input_buffer_ready) {
            return null;
        }
        NMLmsg temp = format_converter.convertRawDataToMsg(input_buffer, 20, message_size);
        if (format_converter.error_in_update) {
            throw create_NMLException(" !ERROR! Format error");
        }
        return temp;
    }
    int serial_number_repeats = 0;
    int zero_messages = 0;
    int id_repeats = 0;

    protected void readUDP_raw_data() throws NMLException {
        try {
            if (port < 1) {
                throw create_NMLException(" !ERROR! Invalid Port.");
            }
            if (null == format_converter) {
                throw create_NMLException(" !ERROR! No Format Converter.");
            }
            if (use_subscription && udpSocket.currentPacket == null) {
                udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                udpSocket.receive(udpInputPacket);
            }
            if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                if (((udpRetryTimeoutMillis < 1) || (lastUdpRequestTime < 1)
                        || (System.currentTimeMillis() - lastUdpRequestTime > udpRetryTimeoutMillis) || (reply_header_received && !read_request_sent)) && !use_subscription) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_READ_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    serial_number++;
                    m_OutputStream.writeInt(serial_number);
                    access_type = CMS_READ_ACCESS;
                    m_OutputStream.writeInt(access_type);
                    m_OutputStream.writeInt(last_id_read);
                    if (min_compatible_version > 3.13 || min_compatible_version < 1e-6) {
                        m_OutputStream.writeInt(0);
                    }
                    if (min_compatible_version > 3.43 || min_compatible_version < 1e-6) {
                        m_OutputStream.writeInt(0);
                    }
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP read request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();

                    int request_length = 20;
                    if (min_compatible_version > 3.13 || min_compatible_version < 1e-6) {
                        request_length = 24;
                    }
                    if (min_compatible_version > 3.43 || min_compatible_version < 1e-6) {
                        request_length = 28;
                    }
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                }
            } else {
                baInputStream.reset();
                int returned_serial_number = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("UDP read reply received.");
                    DebugPrint("returned_serial_number=" + returned_serial_number);
                }
                if (use_subscription) {
                    serial_number++;
                    if (returned_serial_number < serial_number) {
                        // Old packet ignore it.
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        return;
                    }
                } else if (returned_serial_number != serial_number) {
                    serial_number_repeats++;
                    if (read_debug_on) {
                        DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                    }
                    udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                    udpSocket.receive(udpInputPacket);
                    return;
                }
                read_request_sent = false;
                reply_header_received = true;
                remote_status = m_InputStream.readInt();
                message_size = m_InputStream.readInt();
                if (message_size == 0) {
                    zero_messages++;
                    if (read_debug_on) {
                        DebugPrint("zero_messages = " + zero_messages);
                    }
                }
                int write_id = m_InputStream.readInt();
                if (write_id == last_id_read) {
                    id_repeats++;
                    if (read_debug_on) {
                        DebugPrint("write_id = last_id_read = " + write_id + ", id_repeats = " + id_repeats);
                    }
                }
                was_read = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                    DebugPrint("write_id = " + write_id);
                    DebugPrint("message_size = " + message_size + ", was_read = " + was_read + " -- buffer_name = " + buffer_name);
                }
                if (message_size == 0 || write_id == last_id_read) {
                    input_buffer_ready = false;
                    last_id_read = write_id;
                    reply_header_received = true;
                    udpSocket.lastPacket = null;
                    return;
                }
                // DebugPrint("udpInputPacket.getLength() ="+udpInputPacket.getLength()+", message_size = "+message_size+", input_buffer.length = "+input_buffer.length);
                if (udpInputPacket.getLength() < message_size + 20) {
                    throw create_NMLException(" !ERROR! udpInputPacket.getLength() =" + udpInputPacket.getLength() + ", message_size = " + message_size);
                }
                last_id_read = write_id;
                reply_header_received = true;
                udpSocket.lastPacket = null;
                input_buffer_ready = true;
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    private void readRawDataFromTCPSocket() throws NMLException, IOException {
        try {
            if (Thread.interrupted() || null == m_InputStream || null == m_OutputStream || !connected) {
                throw create_NMLException("Interrupted or invalid stream.");
            }
            if (read_debug_on) {
                DebugPrint("NMLConnection.readRawDataFromTCPSocket() called for buffer (" + buffer_name + ")");
                DebugPrint("reply_header_recieved=" + reply_header_received);
                DebugPrint("input_bytes_ready=" + input_bytes_ready);
                DebugPrint("message_size=" + message_size);
                DebugPrint("input_bytes_read=" + input_bytes_read);
                DebugPrint("Time since request sent:" + (System.currentTimeMillis() - this.request_sent_time_millis) + "ms.");
            }
            if (!reply_header_received) {
                if (read_debug_on) {
                    DebugPrint("NMLConnection.readRawDataFromTCPSocket() getting reply_header.");
                }
                if (tcpRead(20) == 0 && poll) {
                    return;
                }
//                input_bytes_ready = this.tcpAvailable();
//                while ((tries < max_tries || max_tries < 0) && input_bytes_ready < 20 && !poll) {
//                    if (Thread.interrupted() || null == m_InputStream) {
//                        throw create_NMLException("Interrupted or invalid stream.");
//                    }
//                    try {
//                        if (tries > 0 && read_debug_on) {
//                            DebugPrint("tries = " + tries);
//                        }
//                        Thread.sleep(10);
//                    } catch (InterruptedException ie) {
//                        return;
//                    } catch (Exception e) {
//                    }
//                    if (Thread.interrupted()) {
//                        return;
//                    }
//                    input_bytes_ready = this.tcpAvailable();
//                    tries++;
//                }
//                if (!poll && tries >= max_tries && max_tries > 0) {
//                    throw create_NMLException(" !ERROR! Timeout Error (4)(timeout = " + (((double) max_tries) / 100.0) + ").");
//                }
//                if (poll) {
//                    input_bytes_ready = m_InputStream.available();
//                }
//                if (poll && input_bytes_ready < 20) {
//                    reply_header_received = false;
//                    if (read_debug_on) {
//                        DebugPrint("Polling and only " + input_bytes_ready + " bytes are ready.");
//                    }
//                    return;
//                }
                int returned_serial_number = m_InputStream.readInt();
                if (use_subscription) {
                    serial_number++;
                }
                if (returned_serial_number != serial_number) {
                    throw this.create_NMLException("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                }
                remote_status = m_InputStream.readInt();
                message_size = m_InputStream.readInt();
                last_id_read = m_InputStream.readInt();
                was_read = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("TCP read/peek reply received.");
                    DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                    DebugPrint("message_size = " + message_size + ", was_read = " + was_read + " -- buffer_name = " + buffer_name);
                    DebugPrint("last_id_read=" + last_id_read);
                }
                if (message_size == 0) {
                    read_request_sent = false;
                    return;
                }
                input_bytes_read = 0;
                reply_header_received = true;
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
//            input_bytes_ready = m_InputStream.available();
//            if (input_bytes_ready + input_bytes_read < message_size && poll) {
//                input_buffer_ready = false;
//                if (read_debug_on) {
//                    DebugPrint("Polling and only " + input_bytes_ready + " bytes are ready.");
//                }
//                input_bytes_read += m_InputStream.read(input_buffer, input_bytes_read, input_bytes_ready);
//                return;
//            }
//            int tries = 0;
//            while ((tries < max_tries || max_tries < 0)
//                    && input_bytes_read < message_size && (!poll || tries == 0)) {
//                while ((tries < max_tries || max_tries < 0)
//                        && input_bytes_ready < (message_size - input_bytes_read) && input_bytes_ready < 2048 && (!poll || tries == 0)) {
//                    if (Thread.interrupted() || null == m_InputStream) {
//                        return;
//                    }
//                    try {
//                        if (read_debug_on && tries > 0) {
//                            DebugPrint("tries = " + tries + ", input_bytes_ready=" + input_bytes_ready + ", input_bytes_read=" + input_bytes_read);
//                        }
//                        Thread.sleep(10);
//                    } catch (InterruptedException ie) {
//                        return;
//                    } catch (Exception e) {
//                    }
//                    if (Thread.interrupted() || null == m_InputStream) {
//                        return;
//                    }
//                    if (null == m_InputStream) {
//                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
//                    }
//                    input_bytes_ready = m_InputStream.available();
//                    tries++;
//                }
//                if (!poll && tries >= max_tries && max_tries > 0) {
//                    throw create_NMLException(" !ERROR! Timeout Error (5).(timeout = " + (((double) max_tries) / 100.0) + ", message_size = " + message_size + ", input_bytes_ready=" + input_bytes_ready + ")");
//                }
//                int bytes_to_read = input_bytes_ready;
//                if (message_size - input_bytes_read < input_bytes_ready) {
//                    bytes_to_read = message_size - input_bytes_read;
//                }
//                if (read_debug_on) {
//                    DebugPrint("bytes_to_read = " + bytes_to_read);
//                }
//                input_bytes_read += m_InputStream.read(input_buffer, input_bytes_read, bytes_to_read);
//            }
//            if (!poll && tries >= max_tries && max_tries > 0 && input_bytes_read < message_size) {
//                throw create_NMLException(" !ERROR! Timeout Error(6)(timeout = " + (((double) max_tries) / 100.0) + ", message_size = " + message_size + ", input_bytes_read=" + input_bytes_read + ").");
//            }
            input_bytes_read = this.tcpRead(message_size);
            if (input_bytes_read >= message_size && reply_header_received) {
                reply_header_received = false;
                read_request_sent = false;
                input_buffer_ready = true;
            }
        } catch (NMLException nml_e) {
            if (read_debug_on) {
                nml_e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            }
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    /**
     * Read an NMLmsg but do not change the was_read flag.
     *
     * @return null if the message in the
     * buffer has already been read by this NMLConnection or no message
     * has yet been written to the buffer, otherwise it
     * returns the NMLmsg read.
     *
     * @exception  rcs.nml.NMLException
     *            The peek failed.
     */
    synchronized public NMLmsg peek() throws NMLException {
        if (Thread.interrupted() || !connected) {
            return null;
        }
        if (write_only) {
            throw create_NMLException(" !ERROR! Attempt to peek write_only buffer.");
        }
        if (read_debug_on) {
            DebugPrint("NMLConnection.peek() called for buffer (" + buffer_name + ")");
        }
        if (!connected) {
            connect();
        }
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return peekTCP();

            case NML_UDP_PROTOCOL_TYPE:
                return peekUDP();

            case NML_STCP_PROTOCOL_TYPE:
                if (null == format_converter) {
                    if (read_debug_on) {
                        DebugPrint("NULL format_converter");
                    }
                    return null;
                }
                NMLmsg temp = format_converter.convertStringToMsg(peekSTCPDataString());
                if (format_converter.error_in_update) {
                    throw create_NMLException(" !ERROR! Format error");
                }
                return temp;

            default:
                ErrorPrint("NMLConnection.peek(): Invalid protocol_option = " + protocol_option + " -- buffer_name = " + buffer_name);
                return null;
        }
        //  return null;
    }

    protected void peek_raw_data() throws NMLException {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                peekTCP_raw_data();
                break;

            case NML_UDP_PROTOCOL_TYPE:
                peekUDP_raw_data();
                break;

            case NML_STCP_PROTOCOL_TYPE:
                if (null == format_converter) {
                    return;
                }
                peekSTCPDataString();
                break;

            default:
                ErrorPrint("NMLConnection.peek_raw_data(): Invalid protocol_option = " + protocol_option + " -- buffer_name = " + buffer_name);
                return;
        }
        //          return null;
    }
    protected long request_sent_time_millis;

    /**
     * Get the value of request_sent_time_millis
     *
     * @return the value of request_sent_time_millis
     */
    public long getRequest_sent_time_millis() {
        return request_sent_time_millis;
    }

    protected void peekTCP_raw_data() throws NMLException {
        try {
            if (port < 1) {
                throw create_NMLException(" !ERROR! Invalid Port.");
            }
            if (null == format_converter) {
                throw create_NMLException(" !ERROR! No Format Converter.");
            }
            if (null == m_OutputStream || null == m_InputStream) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
                null_error_reported = true;
                throw create_NMLException(" !ERROR! No input or output stream.");
            }
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                if (!write_reply_received) {
                    if (read_debug_on) {
                        DebugPrint("NMLConnection.peekTCP_raw_data() Handling old write reply.\n");
                    }
                    input_buffer_ready = false;
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.peekTCP() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    this.tcpRead(12);
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.peekTCP() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            if (!read_request_sent && !use_subscription) {
                input_buffer_ready = false;
                m_OutputStream.writeInt(serial_number);
                request_type = REMOTE_CMS_READ_REQUEST_TYPE;
                m_OutputStream.writeInt(request_type);
                m_OutputStream.writeInt(buffer_number);
                access_type = CMS_PEEK_ACCESS;
                m_OutputStream.writeInt(access_type);
                m_OutputStream.writeInt(last_id_read);
                tcpWrite();
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                reply_header_received = false;
                if (read_debug_on) {
                    DebugPrint("TCP peek request sent.");
                    DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                    DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                }
                serial_number++;
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
            readRawDataFromTCPSocket();
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't peek NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    protected NMLmsg peekTCP() throws NMLException {
        peekTCP_raw_data();
        if (!input_buffer_ready) {
            if (read_debug_on) {
                DebugPrint("input_buffer_ready=false;");
            }
            return null;
//			throw create_NMLException("input buffer not ready.");
        }
        if (read_debug_on) {
            DebugPrint("TCP peek converting Raw Data(message_size = " + message_size + ") to NMLmsg." + " -- buffer_name = " + buffer_name);
        }
        if (message_size < 1) {
            throw create_NMLException(" !ERROR! Message size of zero.");
        }
        NMLmsg temp = format_converter.convertRawDataToMsg(input_buffer, 0, message_size);
        if (format_converter.error_in_update) {
            String s = format_converter.error_in_update_string;
            if (s == null) {
                s = "(null)";
            }
            throw create_NMLException(" !ERROR! Format error:" + s);
        }
        if (read_debug_on) {
            DebugPrint("temp=" + temp);
        }
        return temp;
    }

    protected NMLmsg peekUDP() throws NMLException {
        peekUDP_raw_data();
        if (!input_buffer_ready) {
            return null;
        }
        NMLmsg temp = format_converter.convertRawDataToMsg(input_buffer, 20, message_size);
        if (format_converter.error_in_update) {
            String s = format_converter.error_in_update_string;
            if (s == null) {
                s = "(null)";
            }
            throw create_NMLException(" !ERROR! Format error:" + s);
        }
        return temp;

    }

    protected void peekUDP_raw_data() throws NMLException {
        try {
            if (port < 1) {
                throw create_NMLException(" !ERROR! Invalid Port.");
            }
            if (null == format_converter) {
                throw create_NMLException(" !ERROR! No Format Converter.");
            }
            if (use_subscription && udpSocket.currentPacket == null) {
                udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                udpSocket.receive(udpInputPacket);
            }
            if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                if (((udpRetryTimeoutMillis < 1) || (lastUdpRequestTime < 1)
                        || (System.currentTimeMillis() - lastUdpRequestTime > udpRetryTimeoutMillis) || (reply_header_received && !read_request_sent)) && !use_subscription) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_READ_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    serial_number++;
                    m_OutputStream.writeInt(serial_number);
                    access_type = CMS_PEEK_ACCESS;
                    m_OutputStream.writeInt(access_type);
                    m_OutputStream.writeInt(last_id_read);
                    if (min_compatible_version > 3.13 || min_compatible_version < 1e-6) {
                        m_OutputStream.writeInt(0);
                    }
                    if (min_compatible_version > 3.43 || min_compatible_version < 1e-6) {
                        m_OutputStream.writeInt(0);
                    }
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP peek request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();

                    int request_length = 20;
                    if (min_compatible_version > 3.13 || min_compatible_version < 1e-6) {
                        request_length = 24;
                    }
                    if (min_compatible_version > 3.43 || min_compatible_version < 1e-6) {
                        request_length = 28;
                    }
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                }
            } else {
                baInputStream.reset();
                int returned_serial_number = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("UDP peek reply received.");
                    DebugPrint("returned_serial_number=" + returned_serial_number);
                }
                if (use_subscription) {
                    serial_number++;
                    if (returned_serial_number < serial_number) {
                        // Old packet ignore it.
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        return;
                    }
                } else if (returned_serial_number != serial_number) {
                    serial_number_repeats++;
                    if (read_debug_on) {
                        DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                    }
                    udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                    udpSocket.receive(udpInputPacket);
                    return;
                }
                read_request_sent = false;
                reply_header_received = true;
                remote_status = m_InputStream.readInt();
                message_size = m_InputStream.readInt();
                if (message_size == 0) {
                    zero_messages++;
                    if (read_debug_on) {
                        DebugPrint("zero_messages = " + zero_messages);
                    }
                }
                int write_id = m_InputStream.readInt();
                if (write_id == last_id_read) {
                    id_repeats++;
                    if (read_debug_on) {
                        DebugPrint("write_id = last_id_read = " + write_id + ", id_repeats = " + id_repeats);
                    }
                }
                was_read = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("remote_status = " + remote_status + ", buffer_number = " + buffer_number);
                    DebugPrint("write_id = " + write_id);
                    DebugPrint("message_size = " + message_size + ", was_read = " + was_read + " -- buffer_name = " + buffer_name);
                }
                if (message_size == 0 || write_id == last_id_read) {
                    input_buffer_ready = false;
                    last_id_read = write_id;
                    reply_header_received = true;
                    udpSocket.lastPacket = null;
                    return;
                }
                // DebugPrint("udpInputPacket.getLength() ="+udpInputPacket.getLength()+", message_size = "+message_size+", input_buffer.length = "+input_buffer.length);
                if (udpInputPacket.getLength() < message_size + 20) {
                    throw create_NMLException(" !ERROR! udpInputPacket.getLength() =" + udpInputPacket.getLength() + ", message_size = " + message_size);
                }
                reply_header_received = true;
                udpSocket.lastPacket = null;
                input_buffer_ready = true;
                last_id_read = write_id;
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    protected int UDP_get_msg_count() throws NMLException {
        try {
            if (port < 1) {
                throw create_NMLException(" !ERROR! Invalid Port.");
            }
            if (null == format_converter) {
                throw create_NMLException(" !ERROR! No Format Converter.");
            }
            if (use_subscription && udpSocket.currentPacket == null) {
                udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                udpSocket.receive(udpInputPacket);
            }
            if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                if (((udpRetryTimeoutMillis < 1) || (lastUdpRequestTime < 1)
                        || (System.currentTimeMillis() - lastUdpRequestTime > udpRetryTimeoutMillis) || (reply_header_received && !read_request_sent)) && !use_subscription) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    serial_number++;
                    m_OutputStream.writeInt(serial_number);
                    access_type = CMS_PEEK_ACCESS;
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP peek request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();

                    int request_length = 12;
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                }
            } else {
                baInputStream.reset();
                int returned_serial_number = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("UDP peek reply received.");
                    DebugPrint("returned_serial_number=" + returned_serial_number);
                }
                if (use_subscription) {
                    serial_number++;
                    if (returned_serial_number < serial_number) {
                        // Old packet ignore it.
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        return -1;
                    }
                } else if (returned_serial_number != serial_number) {
                    serial_number_repeats++;
                    if (read_debug_on) {
                        DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                    }
                    udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                    udpSocket.receive(udpInputPacket);
                    return -1;
                }
                read_request_sent = false;
                reply_header_received = true;
                remote_status = m_InputStream.readInt();
                reply_header_received = true;
                udpSocket.lastPacket = null;
                input_buffer_ready = true;
                return m_InputStream.readInt();
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
        return -1;
    }

    protected int UDP_get_msg_type() throws NMLException {
        try {
            if (port < 1) {
                throw create_NMLException(" !ERROR! Invalid Port.");
            }
            if (null == format_converter) {
                throw create_NMLException(" !ERROR! No Format Converter.");
            }
            if (use_subscription && udpSocket.currentPacket == null) {
                udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                udpSocket.receive(udpInputPacket);
            }
            if (!udpSocket.ready((int) udpRetryTimeoutMillis)) {
                if (((udpRetryTimeoutMillis < 1) || (lastUdpRequestTime < 1)
                        || (System.currentTimeMillis() - lastUdpRequestTime > udpRetryTimeoutMillis) || (reply_header_received && !read_request_sent)) && !use_subscription) {
                    input_buffer_ready = false;
                    m_OutputStream.flush();
                    baOutputStream.reset();
                    request_type = REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE;
                    m_OutputStream.writeInt(request_type);
                    m_OutputStream.writeInt(buffer_number);
                    serial_number++;
                    m_OutputStream.writeInt(serial_number);
                    access_type = CMS_PEEK_ACCESS;
                    request_sent_time_millis = System.currentTimeMillis();
                    read_request_sent = true;
                    reply_header_received = false;
                    if (read_debug_on) {
                        DebugPrint("UDP peek request sent.");
                        DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                        DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                        DebugPrint("lastUdpRequestTime = " + lastUdpRequestTime);
                    }
                    m_OutputStream.flush();
                    byte ba[] = baOutputStream.toByteArray();

                    int request_length = 12;
                    if (ba.length < request_length) {
                        throw create_NMLException(" !ERROR! Insufficient output byte array size.");
                    }
                    DatagramPacket dp = new DatagramPacket(ba, request_length, udpServerAddress, port);
                    udpSocket.send(dp);
                    if (null == udpSocket.currentPacket) {
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                    }
                    lastUdpRequestTime = System.currentTimeMillis();
                }
            } else {
                baInputStream.reset();
                int returned_serial_number = m_InputStream.readInt();
                if (read_debug_on) {
                    DebugPrint("UDP peek reply received.");
                    DebugPrint("returned_serial_number=" + returned_serial_number);
                }
                if (use_subscription) {
                    serial_number++;
                    if (returned_serial_number < serial_number) {
                        // Old packet ignore it.
                        udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                        udpSocket.receive(udpInputPacket);
                        return -1;
                    }
                } else if (returned_serial_number != serial_number) {
                    serial_number_repeats++;
                    if (read_debug_on) {
                        DebugPrint("serial_number=" + serial_number + ", returned_serial_number=" + returned_serial_number + ",serial_number_repeats =" + serial_number_repeats);
                    }
                    udpInputPacket = new DatagramPacket(input_buffer, buffer_size);
                    udpSocket.receive(udpInputPacket);
                    return -1;
                }
                read_request_sent = false;
                reply_header_received = true;
                remote_status = m_InputStream.readInt();
                reply_header_received = true;
                udpSocket.lastPacket = null;
                input_buffer_ready = true;
                return m_InputStream.readInt();
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
        return -1;
    }


    /**
     * Reads an NMLmsg and converts it to a String.
     *
     * @return null if the message in the
     *    buffer has already been read by this NMLConnection or no message
     *    has yet been written to the buffer, otherwise it
     *    returns the NMLmsg read converted to a string.
     *    The string is a comma separated list of the parameters
     *    in the NMLmsg in the order they are updated, starting with
     *    the type and size.
     *
     * @exception  rcs.nml.NMLException
     *            The read failed.
     *
     * @see rcs.nml.NMLConnection#read()
     */
    synchronized public String readDataString() throws NMLException {
        if (Thread.interrupted() || !connected) {
            return null;
        }
        if (write_only) {
            throw create_NMLException(" !ERROR! Attempt to read write_only buffer.");
        }
        if (read_debug_on) {
            DebugPrint("NMLConnection.readDataString() called for buffer (" + buffer_name + ")");
        }
        if (protocol_option == NML_STCP_PROTOCOL_TYPE
                && data_format_option == NML_DISP_ENCODING_TYPE) {
            input_string = readSTCPDataString();
            if (read_debug_on) {
                DebugPrint("input_string = " + input_string);
            }
            return input_string;
        } else {
            if (data_format_option != NML_DISP_ENCODING_TYPE) {
                if (null == format_converter) {
                    return null;
                }
                input_string = format_converter.convertMsgToString(read());
                if (read_debug_on) {
                    DebugPrint("input_string = " + input_string);
                }
            } else {
                read_raw_data();
                if (input_buffer_ready) {
                    input_string = new String(input_buffer, 0, message_size);
                } else {
                    input_string = null;
                }
            }
            return input_string;
        }
    }

    protected String readSTCPDataString() throws NMLException {
        String request_string;
        String reply_string = "";
        int bytes_to_read = 0;
        int input_end_of_line = 0;

        if (port < 1) {
            throw create_NMLException(" !ERROR! Invalid Port.");
        }
        if (null == m_OutputStream || null == m_InputStream) {
            if (!null_error_reported) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
            }
            null_error_reported = true;
            throw create_NMLException(" !ERROR! No input or output stream.");
        }
        try {
            if (!read_request_sent) {
                request_string = "read(" + buffer_number + "):\n";
                m_OutputStream.writeBytes(request_string);
                m_OutputStream.flush();
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                input_string = "";
                if (read_debug_on) {
                    DebugPrint("readSTCPDataString(): Read request sent. request_string = " + request_string + ", (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
            }
            int tries = 0;
            while ((input_end_of_line <= 0 && !poll
                    && (tries < max_tries || max_tries < 0))
                    || tries == 0) {
                tries++;
                if (null == m_InputStream) {
                    throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                }
                input_bytes_ready = m_InputStream.available();
                if (read_debug_on) {
                    DebugPrint("readSTCPDataString(): " + input_bytes_ready + " bytes ready. , (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
                if (input_bytes_ready < 1) {
                    if (poll) {
                        return null;
                    } else {
                        Thread.sleep(10);
                        continue;
                    }
                }
                if (null == m_InputStream) {
                    throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                }
                input_bytes_ready = m_InputStream.available();
                if (input_bytes_ready < 1 && poll) {
                    return null;
                }
                if (input_bytes_ready < buffer_size) {
                    bytes_to_read = input_bytes_ready;
                } else {
                    bytes_to_read = buffer_size;
                }
                input_bytes_read = m_InputStream.read(input_buffer, 0, bytes_to_read);
                if (input_bytes_read < 1) {
                    return null;
                }
                String string_to_add = new String(input_buffer, 0, input_bytes_read);
                if (null == input_string) {
                    input_string = "";
                }
                if (null != string_to_add) {
                    input_string += string_to_add;
                }
                input_end_of_line = input_string.indexOf('\n');
                if (input_end_of_line <= 0 && !poll
                        && (tries < max_tries || max_tries < 0)) {
                    try {
                        Thread.sleep(10);
                    } catch (Exception e) {
                        e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                    }
                }
            }
            if (!poll && tries >= max_tries && max_tries > 0) {
                throw create_NMLException(" !ERROR! Timeout Error(7).(timeout = " + (((double) max_tries) / 100.0) + ")");
            }

            if (input_end_of_line < 1) {
                return null;
            }
            reply_string = input_string.substring(0, input_end_of_line);
            if (reply_string.charAt(input_end_of_line - 1) == '\r') {
                reply_string = reply_string.substring(0, input_end_of_line - 1);
            }
            if (reply_string.charAt(0) == '\r') {
                reply_string = reply_string.substring(1);
            }
            input_string = input_string.substring(input_end_of_line + 1);
            if (reply_string.startsWith("ERR") || reply_string.startsWith("null")) {
                read_request_sent = false;
                if (reply_string.length() > 5) {
                    if (reply_string.charAt(5) == '0') {
                        return null;
                    }
                }
                if (-1 != reply_string.indexOf("Shutdown")) {
                    disconnect();
                }
                throw new Exception();
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + " reply_string= " + reply_string + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
        read_request_sent = false;
        return reply_string;
    }

    /**
     * Reads an NMLmsg using peek() and converts it to a String.
     *
     * @return null if the message in the
     *    buffer has already been read by this NMLConnection or no message
     *    has yet been written to the buffer, otherwise it
     *    returns the NMLmsg read converted to a string.
     *    The string is a comma separated list of the parameters
     *    in the NMLmsg in the order they are updated, starting with
     *    the type and size.
     *
     * @exception  rcs.nml.NMLException
     *            The peek failed.
     *
     * @see rcs.nml.NMLConnection#peek()
     */
    synchronized public String peekDataString() throws NMLException {
        if (Thread.interrupted() || !connected) {
            return null;
        }
        if (write_only) {
            throw create_NMLException(" !ERROR! Attempt to peek write_only buffer.");
        }
        if (read_debug_on) {
            DebugPrint("NMLConnection.peekDataString() called for buffer (" + buffer_name + ")");
        }
        if (!connected) {
            connect();
        }
        if (protocol_option == NML_STCP_PROTOCOL_TYPE
                && data_format_option == NML_DISP_ENCODING_TYPE) {
            input_string = peekSTCPDataString();
            if (read_debug_on) {
                DebugPrint("input_string = " + input_string);
            }
            return input_string;
        } else {
            if (data_format_option != NML_DISP_ENCODING_TYPE) {
                if (null == format_converter) {
                    if (read_debug_on) {
                        DebugPrint("NULL format_converter.\n");
                    }
                    return null;
                }
                input_string = format_converter.convertMsgToString(peek());
                if (read_debug_on) {
                    DebugPrint("input_string = " + input_string);
                }
            } else {
                peek_raw_data();
                if (input_buffer_ready) {
                    input_string = new String(input_buffer, 0, message_size);
                } else {
                    input_string = null;
                }
            }
            return input_string;
        }
    }

    protected String peekSTCPDataString() throws NMLException {
        String request_string;
        String reply_string = "";
        int input_bytes_ready = 0;
        int input_bytes_read = 0;
        int bytes_to_read = 0;
        if (port < 1) {
            throw create_NMLException(" !ERROR! Invalid Port.");
        }
        if (null == m_OutputStream || null == m_InputStream) {
            if (!null_error_reported) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
            }
            null_error_reported = true;
            throw create_NMLException(" !ERROR! No input or output stream.");
        }
        try {
            if (!read_request_sent) {
                request_string = "peek(" + buffer_number + "):\n";
                m_OutputStream.writeBytes(request_string);
                m_OutputStream.flush();
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                input_string = "";
                if (read_debug_on) {
                    DebugPrint("peekSTCPDataString(): Read request sent. request_string = " + request_string + ", (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
            }
            int tries = 0;
            int input_end_of_line = 0;

            while ((input_end_of_line <= 0 && !poll
                    && (tries < max_tries || max_tries < 0))
                    || tries == 0) {
                if (read_debug_on) {
                    DebugPrint("input_end_of_line = " + input_end_of_line + ", poll=" + poll);
                    DebugPrint("tries = " + tries);
                }
                tries++;
                if (null == m_InputStream) {
                    throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                }
                input_bytes_ready = m_InputStream.available();
                if (read_debug_on) {
                    DebugPrint("peekSTCPDataString(): " + input_bytes_ready + " bytes ready. , (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
                if (input_bytes_ready < 1) {
                    if (poll) {
                        return null;
                    } else {
                        try {
                            Thread.sleep(10);
                        } catch (Exception e) {
                        }
                        continue;
                    }
                }
                if (input_bytes_ready < buffer_size) {
                    bytes_to_read = input_bytes_ready;
                } else {
                    bytes_to_read = buffer_size;
                }
                input_bytes_read = m_InputStream.read(input_buffer, 0, bytes_to_read);
                if (read_debug_on) {
                    DebugPrint("peekSTCPDataString(): " + input_bytes_read + " bytes received. , (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
                if (input_bytes_read < 1) {
                    return null;
                }
                String string_to_add = new String(input_buffer, 0, input_bytes_read);
                if (null == input_string) {
                    input_string = "";
                }
                if (null == string_to_add) {
                    return null;
                }
                if (string_to_add.length() < 1) {
                    return null;
                }
                if (read_debug_on) {
                    DebugPrint("peekSTCPDataString(): Data received. string_to_add = <" + string_to_add + ">, (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                }
                input_string += string_to_add;
                input_end_of_line = input_string.indexOf('\n');
                if (input_end_of_line <= 0 && !poll
                        && (tries < max_tries || max_tries < 0)) {
                    try {
                        Thread.sleep(10);
                    } catch (Exception e) {
                        e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
                    }
                }
            }
            if (!poll && tries >= max_tries && max_tries > 0) {
                throw create_NMLException(" !ERROR! Timeout Error(8).(timeout = " + (((double) max_tries) / 100.0) + ")");
            }
            if (input_end_of_line <= 0) {
                return null;
            }
            reply_string = input_string.substring(0, input_end_of_line);
            if (read_debug_on) {
                DebugPrint("peekSTCPDataString(): reply received. reply_string = " + reply_string + ", (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
            }
            if (null != reply_string) {
                if (reply_string.length() > 0 && reply_string.length() > input_end_of_line - 1) {
                    if (reply_string.charAt(input_end_of_line - 1) == '\r') {
                        reply_string = reply_string.substring(0, input_end_of_line - 1);
                    }
                    if (reply_string.charAt(0) == '\r') {
                        reply_string = reply_string.substring(1);
                    }
                }
            }
            input_string = input_string.substring(input_end_of_line + 1);
            read_request_sent = false;
            while (true) {
                if (reply_string.length() < 1) {
                    return null;
                }
                if (reply_string.charAt(0) != ' ') {
                    break;
                }
                reply_string = reply_string.substring(1);
            }
            if (reply_string.startsWith("E") || reply_string.startsWith("null")) {
                if (reply_string.length() > 5) {
                    if (reply_string.charAt(5) == '0') {
                        return null;
                    }
                }
                if (-1 != reply_string.indexOf("Shutdown")) {
                    disconnect();
                }
                throw new Exception();
            }
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + " reply_string = " + reply_string + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
        read_request_sent = false;
        return reply_string;
    }


    protected int writeTCP(NMLmsg msg) {
        try {
            if (null == msg) {
                return -1;
            }
            if (port < 1) {
                return -1;
            }
            if (null == format_converter) {
                return -1;
            }
            if (Thread.interrupted() || !connected) {
                return -1;
            }
            if (null == output_buffer) {
                throw create_NMLException("output_buffer is null");
            }
            if (null == m_OutputStream || null == m_InputStream) {
                if (!null_error_reported) {
                    ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                    ErrorPrint("Stream is null.");
                }
                null_error_reported = true;
                return -1;
            }
            long start_millis = System.currentTimeMillis();
            NMLmsg msg_copy = msg;
            while (read_request_sent) {
                peekTCP();
                if (read_request_sent && ((System.currentTimeMillis()) - start_millis) > 2000) {
                    ErrorPrint("NMLConnection.writeTCP()  -- timed out waiting for !read_request_sent.");
                    if (read_request_sent) {
                        return -1;
                    }
                }
            }
            msg = msg_copy;
            if (write_debug_on) {
                DebugPrint("NMLConnection.write() sending message of type = " + msg.type);
            }
            int confirm_write_tries = 0;
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                while (!write_reply_received) {
                    confirm_write_tries++;
                    if (write_debug_on) {
                        DebugPrint("NMLConnection.writeTCP() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    this.tcpRead(12);
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")");
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on) {
                        DebugPrint("NMLConnection.writeTCP() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            if (format_converter.convertMsgToRawData(output_buffer, buffer_size, msg) < 0) {
                throw create_NMLException(" !ERROR! Format Error");
            }
            m_OutputStream.writeInt(serial_number);
            request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            access_type = CMS_WRITE_ACCESS;
            m_OutputStream.writeInt(access_type);
            m_OutputStream.writeInt(format_converter.raw_data_size);
            m_OutputStream.write(output_buffer, 0, format_converter.raw_data_size);
            this.tcpWrite();
            if (write_debug_on) {
                DebugPrint("TCP write request sent.");
                DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + ", raw_data_size = " + format_converter.raw_data_size);
                DebugPrint("output_buffer =" + output_buffer[0] + " " + output_buffer[1] + " " + output_buffer[2] + " " + output_buffer[3]);
            }
            write_reply_received = false;
            serial_number++;
            return 0;

        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }

    }

    private int writeTCP_raw_data() {
        try {
            if (port < 1) {
                return -1;
            }
            if (null == format_converter) {
                return -1;
            }
            if (null == m_OutputStream || null == m_InputStream) {
                if (!null_error_reported) {
                    ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                    ErrorPrint("Stream is null.");
                }
                null_error_reported = true;
                return -1;
            }
            long start_millis = System.currentTimeMillis();
            while (read_request_sent) {
                peekTCP();
                if (read_request_sent && ((System.currentTimeMillis()) - start_millis) > 2000) {
                    ErrorPrint("NMLConnection.writeTCP()  -- timed out waiting for !read_request_sent.");
                    if (read_request_sent) {
                        return -1;
                    }
                }
            }
            int confirm_write_tries = 0;
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                while (!write_reply_received) {
                    if (write_debug_on) {
                        DebugPrint("NMLConnection.writeTCP() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    this.tcpRead(12);
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")");
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on) {
                        DebugPrint("NMLConnection.writeTCP() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            m_OutputStream.writeInt(serial_number);
            request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            access_type = CMS_WRITE_ACCESS;
            m_OutputStream.writeInt(access_type);
            m_OutputStream.writeInt(output_data_size);
            m_OutputStream.write(output_buffer, 0, output_data_size);
            this.tcpWrite();
            if (write_debug_on) {
                DebugPrint("TCP write request sent.");
                DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + ", raw_data_size = " + format_converter.raw_data_size);
                DebugPrint("output_data_size=" + output_data_size);
            }
            write_reply_received = false;
            serial_number++;
            return 0;

        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    protected int writeUDP(NMLmsg msg) {

        try {
            if (null == msg) {
                return -1;
            }
            if (port < 1) {
                return -1;
            }
            if (null == format_converter) {
                return -1;
            }
            if (format_converter.convertMsgToRawData(output_buffer, buffer_size, msg) < 0) {
                throw create_NMLException(" !ERROR! Format Error");
            }
            m_OutputStream.flush();
            baOutputStream.reset();
            request_type = REMOTE_CMS_WRITE_REQUEST_TYPE;
            m_OutputStream.writeInt(request_type);
            m_OutputStream.writeInt(buffer_number);
            serial_number++;
            m_OutputStream.writeInt(serial_number);
            access_type = CMS_WRITE_ACCESS;
            m_OutputStream.writeInt(access_type);
            m_OutputStream.writeInt(format_converter.raw_data_size);
            if (min_compatible_version > 3.13 || min_compatible_version < 1e-6) {
                m_OutputStream.writeInt(0);
            }
            if (min_compatible_version > 3.43 || min_compatible_version < 1e-6) {
                m_OutputStream.writeInt(0);
            }
            m_OutputStream.write(output_buffer, 0, format_converter.raw_data_size);
            m_OutputStream.flush();
            byte ba[] = baOutputStream.toByteArray();
            DatagramPacket dp = new DatagramPacket(ba, ba.length, udpServerAddress, port);
            udpSocket.send(dp);
            if (write_debug_on) {
                DebugPrint("UDP write request sent.");
                DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + ", raw_data_size = " + format_converter.raw_data_size);
            }
            write_reply_received = false;
            return 0;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    /**
     * Writes an NMLmsg.
     *
     * @param msg the NMLmsg to write.
     * @return 0 if the write was successful, -1 if there was an error
     * @throws rcs.nml.NMLException when write fails
     */
    synchronized public int write(NMLmsg msg) throws NMLException {
        if (null == msg) {
            return -1;
        }
        if (read_only) {
            throw create_NMLException(" !ERROR! Attempt to write read_only buffer.");
        }
        if (Thread.interrupted() || !connected) {
            return -1;
        }
        if (write_debug_on) {
            DebugPrint("NMLConnection.write() sending message of type = " + msg.type);
        }
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return writeTCP(msg);

            case NML_UDP_PROTOCOL_TYPE:
                return writeUDP(msg);

            case NML_STCP_PROTOCOL_TYPE:
                if (null == format_converter) {
                    return -1;
                }
                String temp = format_converter.convertMsgToString(msg);
                if (format_converter.error_in_update) {
                    return -1;
                }
                return writeSTCPDataString(temp);

            default:
                ErrorPrint("NMLConnection.write(): Invalid protocol_option = " + protocol_option);
                return -1;
        }
        //return null;
    }

    private int write_raw_data() {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return writeTCP_raw_data();

            case NML_UDP_PROTOCOL_TYPE:
                return writeUDP_raw_data();


            default:
                ErrorPrint("NMLConnection.write(): Invalid protocol_option = " + protocol_option);
                return -1;
        }
        //return null;
    }

    private int writeUDP_raw_data() {
        return 0;
    }

    /**
     * Convert the string into an NMLmsg and send it to this channel.
     * Depending on the server configuration the string may also be sent directly to
     * the server to be converted on the server end.
     * The string should be comma delimited starting with the type and size.
     * ie:
     * 1001,0,99
     *
     * @param dataString string to be converted
     * @return 0 write ok, -1 an error occured.
     */
    synchronized public int writeDataString(String dataString) throws NMLException {
        if (read_only) {
            throw create_NMLException(" !ERROR! Attempt to write read_only buffer.");
        }
        if (null == dataString) {
            return -1;
        }
        if (Thread.interrupted() || !connected) {
            return -1;
        }
        if (protocol_option == NML_STCP_PROTOCOL_TYPE
                && data_format_option == NML_DISP_ENCODING_TYPE) {
            return writeSTCPDataString(dataString);
        } else {
            if (data_format_option != NML_DISP_ENCODING_TYPE) {
                if (null == format_converter) {
                    return -1;
                }
                return write(format_converter.convertStringToMsg(dataString));
            } else {
                output_buffer = dataString.getBytes();
                output_data_size = dataString.length();
                return write_raw_data();
            }
        }
    }

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
     * @param dataString string to be converted
     * @return 0 write ok, -1 an error occured.
     */
    public int writeDataStringNoThrow(String dataString) {
        try {
            return writeDataString(dataString);
        } catch (Exception e) {
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
    }

    protected int writeSTCPDataString(String dataString) {
        String request_string;
        try {
            if (write_debug_on) {
                DebugPrint("NMLConnection.writeSTCPDataString(" + dataString + ")");
            }
            if (null == dataString) {
                return -1;
            }
            if (port < 1) {
                return -1;
            }
            if (null == m_OutputStream || null == m_InputStream) {
                if (!null_error_reported) {
                    ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
                    ErrorPrint("Stream is null.");
                }
                null_error_reported = true;
                return -1;
            }
            if (null == dataString) {
                ErrorPrint("NMLConnnection.write() -- attempt to send null dataString.");
                return -1;
            }
            if (dataString.length() < 1) {
                ErrorPrint("NMLConnnection.write() -- attempt to send  dataString with length less than 1.");
                return -1;
            }
            request_string = "write(" + buffer_number + "): " + dataString + "\n";
            m_OutputStream.writeBytes(request_string);
            m_OutputStream.flush();
        } catch (Exception e) {
            ErrorPrint("\r\nCan't write NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")");
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            return -1;
        }
        return 0;
    }

    /**
     * Get the host name where the buffer is supposed to be located.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return host
     */
    public String get_host() {
        return host;
    }

    /**
     * Set the host name where the buffer is supposed to be located which
     * will be used for the next connection attempt. Either host names or IP addresses
     * can be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param _host new value for host
     */
    public void set_host(String _host) {
        host = _host;
    }

    /**
     * Get the name of the NML configuration file being used.
     * @return configuration_file
     */
    public String get_configuration_file() {
        return configuration_file;
    }

    /**
     * Set the name of the NML configuration file.
     * This should be followed with ReadNMLConfigurationFileNoThrow() to actually have
     * the configration file read.
     * @param _configuration_file new value of configuration file
     */
    public void set_configuration_file(String _configuration_file) {
        configuration_file = _configuration_file;
    }

    /**
     * Get the buffer name.
     *
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @return buffer_name
     */
    public String get_buffer_name() {
        return buffer_name;
    }

    /**
     * Set the buffer name.
     *
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     *
     * @param _buffer_name new value of buffer_name
     */
    public void set_buffer_name(String _buffer_name) {
        buffer_name = _buffer_name;
    }

    /**
     * Get the process name.
     *
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @return process_name
     */
    public String get_process_name() {
        return process_name;
    }

    /**
     * Set the process name.
     *
     * This is normally set by being passed as an argument to the NMLConnection() constructor.
     * @param _process_name new process name
     */
    public void set_process_name(String _process_name) {
        process_name = _process_name;
    }

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
    public String readDataStringNoThrow() {
        try {
            return readDataString();
        } catch (Exception e) {
            noThrowErrorCount++;
            e.printStackTrace();
        }
        return null;
    }

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
    public String peekDataStringNoThrow() {
        try {
            return peekDataString();
        } catch (Exception e) {
            e.printStackTrace();
            noThrowErrorCount++;
        }
        return null;
    }

    /**
     * Return the TCP or UDP port number associated with this connection.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return port number
     */
    public int get_port() {
        return port;
    }

    /**
     * Set the TCP or UDP port to be used for the next time a connect() is performed.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param _port new port value
     */
    public void set_port(int _port) {
        if (config_debug_on) {
            DebugPrint("this=" + this + ", port=" + port + ",_port=" + _port);
        }
        port = _port;
    }

    /**
     * Get the buffer_number for this buffer.
     * The buffer number is sent with each request to the server to identify which buffer
     * should be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @return buffer_number
     */
    public int get_buffer_number() {
        return buffer_number;
    }

    /**
     * Set the buffer_number for this buffer.
     * The buffer number is sent with each request to the server to identify which buffer
     * should be used.
     * This is normally obtained from the BufferLine of the config file or from the nmlcfgsvr.
     * @param _buffer_number new buffer number
     */
    public void set_buffer_number(int _buffer_number) {
        buffer_number = _buffer_number;
    }

    /**
     * Reread the NML Configuration and reconnect.
     *
     * Same as ReadNMLConfiguration() except errors are only printed to
     * stderr and no exceptions are thrown.
     *
     */
    public void ReadNMLConfigurationFileNoThrow() {
        try {
            called_from_no_throw = true;
            ReadNMLConfigurationFile();
            called_from_no_throw = false;
        } catch (Exception e) {
            e.printStackTrace();
            noThrowErrorCount++;
        }
    }

    /**
     * Connect as connect() but simply print a message to strderr and return -1
     * in case of an error rather than throwing an exception.
     *
     * ReadNMLConfigurationFile and the NMLConnection() constructor with 4 arguments.
     * call connect() internally so it is only necessary to call this if
     * the first connect() failed, or disconnect() was called or the 0 arguments
     * constructor was used to createt the NMLConnection.
     *
     * @return 0 connect was ok. -1 error occured.
     */
    public int connectNoThrow() {
        try {
            return connect();
        } catch (Exception e) {
            input_buffer = null;
            output_buffer = null;
            e.printStackTrace();
            noThrowErrorCount++;
        }
        try {
            disconnect_time = System.currentTimeMillis();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Get the value of the connected property. Set to true by a successful connect()
     * or constructor set to false by disconnect().
     *
     * @return Is the Channel and its underlying socket connected?
     */
    public boolean is_connected() {
        return connected;
    }


    /**
     * Get the buffer line from the configuration file.
     * @return BufferLine
     */
    public String getBufferLine() {
        return BufferLine;
    }
    /**
     * Creator singleton used to avoid circular dependancies in old
     * diagapplet/codegen/rcs design code.
     *
     */
    public static final NMLConnectionCreator Creator = new NMLConnectionCreator();

    public void set_read_debug_on(boolean b) {
        read_debug_on = b;
    }

    public void set_write_debug_on(boolean b) {
        write_debug_on = b;
    }

    /**
     * Print messages to the console/stderr  to debug the config related  code.
     * @param b whether to print config debug messages
     */
    public static void set_config_debug_on(boolean b) {
        //Thread.dumpStack();
        config_debug_on = b;
        //DebugPrint2("config_debug_on="+config_debug_on);
    }

    public String read_errlog_string() throws NMLException {
        NML_ERROR ne = (NML_ERROR) read();
        if (null == ne) {
            return null;
        }
        int ne_length = 0;
        while (ne_length < ne.error.length && ne.error[ne_length] != 0) {
            ne_length++;
        }
        return new String(ne.error, 0, ne_length);
    }

    private int TCP_get_msg_type() throws NMLException {
        if (port < 1) {
            throw create_NMLException(" !ERROR! Invalid Port.");
        }
        if (null == format_converter) {
            throw create_NMLException(" !ERROR! No Format Converter.");
        }
        if (null == m_OutputStream || null == m_InputStream) {
            if (!null_error_reported) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
            }
            null_error_reported = true;
            throw create_NMLException(" !ERROR! No input or output stream.");
        }
        try {
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                if (!write_reply_received) {
                    if (read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_type() Handling old write reply.\n");
                    }
                    input_buffer_ready = false;
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_type() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    input_bytes_ready = m_InputStream.available();
                    if (input_bytes_ready < 12) {
                        return -1;
                    }
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_type() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            if (!read_request_sent && !use_subscription) {
                input_buffer_ready = false;
                m_OutputStream.writeInt(serial_number);
                request_type = REMOTE_CMS_GET_MSG_TYPE_REQUEST_TYPE;
                m_OutputStream.writeInt(request_type);
                m_OutputStream.writeInt(buffer_number);
                access_type = CMS_PEEK_ACCESS;
                m_OutputStream.writeInt(access_type);
                m_OutputStream.writeInt(last_id_read);
                m_OutputStream.flush();
                this.tcpWrite();
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                reply_header_received = false;
                if (read_debug_on) {
                    DebugPrint("TCP peek request sent.");
                    DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                    DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                }
                serial_number++;
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
            input_bytes_ready = m_InputStream.available();
            if (read_debug_on) {
                DebugPrint("NMLConnection.readRawDataFromTCPSocket() getting reply_header.");
            }
            this.tcpRead(12);
            int returned_serial_number = m_InputStream.readInt();
            if (use_subscription) {
                serial_number++;
            }
            if (returned_serial_number != serial_number) {
                ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
            }
            remote_status = m_InputStream.readInt();
            reply_header_received = true;
            read_request_sent = false;
            return m_InputStream.readInt();
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't TCP_get_msg_type NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            //e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    synchronized public int get_msg_type() throws NMLException {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return TCP_get_msg_type();

            case NML_UDP_PROTOCOL_TYPE:
                return UDP_get_msg_type();

            default:
                peekDataString();
                break;
        }
        if (null == format_converter) {
            return -1;
        }
        return format_converter.msg_type;
    }

    private int TCP_get_msg_count() throws NMLException {
        if (port < 1) {
            throw create_NMLException(" !ERROR! Invalid Port.");
        }
        if (null == format_converter) {
            throw create_NMLException(" !ERROR! No Format Converter.");
        }
        if (null == m_OutputStream || null == m_InputStream) {
            if (!null_error_reported) {
                ErrorPrint("\r\nCan't read NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
                ErrorPrint("Stream is null.");
            }
            null_error_reported = true;
            throw create_NMLException(" !ERROR! No input or output stream.");
        }
        try {
            if (confirm_write
                    || (min_compatible_version < 2.58 && min_compatible_version > 1e-6)) {
                if (!write_reply_received) {
                    if (read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_count() Handling old write reply.\n");
                    }
                    input_buffer_ready = false;
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_count() -- Still have not received write reply for buffer " + buffer_name);
                    }
                    if (null == m_InputStream) {
                        throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
                    }
                    if(12 != this.tcpPollRead(12)) {
                        return -1;
                    }
                    int returned_serial_number = m_InputStream.readInt();
                    if (returned_serial_number != serial_number) {
                        ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
                    }
                    remote_status = m_InputStream.readInt();
                    was_read = m_InputStream.readInt();
                    if (write_debug_on || read_debug_on) {
                        DebugPrint("NMLConnection.TCP_get_msg_count() -- Write reply received.");
                        DebugPrint("returned_serial_number=" + returned_serial_number + ", remote_status = " + remote_status);
                    }
                    write_reply_received = true;
                }
            }
            if (!read_request_sent && !use_subscription) {
                input_buffer_ready = false;
                m_OutputStream.writeInt(serial_number);
                request_type = REMOTE_CMS_GET_MSG_COUNT_REQUEST_TYPE;
                m_OutputStream.writeInt(request_type);
                m_OutputStream.writeInt(buffer_number);
                access_type = CMS_PEEK_ACCESS;
                m_OutputStream.writeInt(access_type);
                m_OutputStream.writeInt(last_id_read);
                m_OutputStream.flush();
                this.tcpWrite();
                request_sent_time_millis = System.currentTimeMillis();
                read_request_sent = true;
                reply_header_received = false;
                if (read_debug_on) {
                    DebugPrint("TCP peek request sent.");
                    DebugPrint("serial_number=" + serial_number + ", request_type = " + request_type + ", buffer_number = " + buffer_number);
                    DebugPrint("access_type = " + access_type + ", last_id_read = " + last_id_read + " -- buffer_name = " + buffer_name);
                }
                serial_number++;
            }
            if (null == m_InputStream) {
                throw this.create_NMLException("input stream closed unexpectedly, disconnect_stack_trace=" + this.disconnect_stack_trace);
            }
            input_bytes_ready = m_InputStream.available();
            if (read_debug_on) {
                DebugPrint("NMLConnection.readRawDataFromTCPSocket() getting reply_header.");
            }
            this.tcpRead(12);
            int returned_serial_number = m_InputStream.readInt();
            if (use_subscription) {
                serial_number++;
            }
            if (returned_serial_number != serial_number) {
                ErrorPrint("returned_serial_number(" + returned_serial_number + ") != serial_number(" + serial_number + ")" + " -- buffer_name = " + buffer_name);
            }
            remote_status = m_InputStream.readInt();
            reply_header_received = true;
            read_request_sent = false;
            return m_InputStream.readInt();
        } catch (NMLException nml_e) {
            throw nml_e;
        } catch (Exception e) {
            ErrorPrint("\r\nCan't TCP_get_msg_count NML (port=" + port + " buffer_number=" + buffer_number + " host=" + host + ")" + " -- buffer_name = " + buffer_name);
            if (null != e.getMessage()) {
                System.err.print("\r\n" + e.getMessage() + "\r\n");
            }
            e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
            input_buffer_ready = false;
            throw create_NMLException(" !ERROR! Misc. Error.", e);
        }
    }

    synchronized public int get_msg_count() throws NMLException {
        switch (protocol_option) {
            case NML_TCP_PROTOCOL_TYPE:
                return TCP_get_msg_count();

             case NML_UDP_PROTOCOL_TYPE:
                return UDP_get_msg_count();

            default:
                break;
        }
        return get_last_id_read();
    }

    public boolean isRead_only() {
        return read_only;
    }

    public void setRead_only(boolean read_only) {
        this.read_only = read_only;
    }

    public boolean isWrite_only() {
        return write_only;
    }

    public void setWrite_only(boolean write_only) {
        this.write_only = write_only;
    }

    public int getAllocation_size_max() {
        return allocation_size_max;
    }

    public void setAllocation_size_max(int allocation_size_max) {
        this.allocation_size_max = allocation_size_max;
    }

    /**
     * @return the poll
     */
    public boolean isPoll() {
        return poll;
    }

    /**
     * @param poll the poll to set
     */
    public void setPoll(boolean poll) {
        this.poll = poll;
    }
}
