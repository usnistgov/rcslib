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
import rcs.nml.NMLConnectionInterface;
import rcs.nml.NMLException;

/**
 * Stores referencees to all of the information needed by the Design or Diagnostics tools
 * related to  a single NML Buffer. 
 */
public class BufferInfo {

    public boolean bufferline_headers_checked = false;

    public String [] getBufferLineHeaders() {
        if(null != read_nml) {
            LinkedList<String> bufline_headers = null;
            bufline_headers = new LinkedList<String>();
            String bufline = read_nml.getBufferLine();
            String buftokens[] = bufline.split("[ \t]+");
            for(int i = 0; i < buftokens.length; i++) {
                int hindex = bufline.indexOf("header=");
                while(hindex >= 0) {
                    String header = bufline.substring(hindex+7);
                    bufline_headers.add(header);
                }
            }
            return bufline_headers.toArray(new String[bufline_headers.size()]);
        }
        return null;
    }

    static private final boolean double_buffer_nml = false;
    /**
     * Name of buffer used in NML constructor and config file.
     */
    public String Name;
    /**
     * ServerInfo associated with this buffer. Used only by Design tool.
     */
    public ServerInfo si = null;
    /**
     * Integer Id used only by  Design Tool.
     */
    public int id;
    /**
     * List of all the names of all processes known to read from this buffer. 
     * Used only by Design tool.
     */
    public List<String> readerNames;
    /**
     * List of all the names of all processes known to write to this buffer. 
     * Used only by Design tool.
     */
    public List<String> writerNames;
    /**
     * Hashtable matching module Names to ChannelInfo objects associated with this buffer.
     * Used only by Design tool.
     */
    public Hashtable channelsHashtable = null;
    /**
     * NML Configuration file associated with this buffer.
     */
    public String configFile;
    public int last_selected_msg_index = -1;
    private Hashtable previous_messages_hashtable = null;

    public Hashtable getPreviousMessagesHashtable() {
	return previous_messages_hashtable;
    }

    public void setPreviousMessagesHashtable(Hashtable _ht) {
	previous_messages_hashtable = _ht;
    }

    static public rcs.nml.NMLFormatConvertErrCallbackInterface get_nml_format_err_callback() {
	return DiagNMLFormatConvertErrCallback.dnfcecb;
    }
    private static int num_buffers;

    public void SetPreviousMessage(String s) {
	if (null == previous_messages_hashtable) {
	    previous_messages_hashtable = new Hashtable();
	}
	int index_of_comma = s.indexOf(',');
	if (index_of_comma > 0) {
	    String idString = s.substring(0, index_of_comma);
	    Long idLong = Long.valueOf(idString);
	    previous_messages_hashtable.put(idLong, s);
	}
    }

    public String GetPreviousMessage(Long l) {
	if (null == previous_messages_hashtable) {
	    return null;
	}
	return (String) previous_messages_hashtable.get(l);
    }

    public int writeDataString(String s) throws NMLException {
	SetPreviousMessage(s);
	if (null != write_nml) {
	    return write_nml.writeDataString(s);
	}
	return -1;
    }
    /**
     * Connection used by Diagnostics tool for writing to this buffer.
     */
    public NMLConnectionInterface write_nml = null;
    /**
     * Connection used by Diagnostics tool for reading from this buffer.
     */
    public NMLConnectionInterface read_nml = null;
    /**
     * The hightest variable number needed for plotting. Messages only needed for
     * plotting can stop being parsed after this number.
     */
    public int max_plot_var_number = 0;
    /**
     * Has the diagnosics tool already plotted this buffer since reading it?
     */
    public boolean has_been_plotted = false;
    /**
     * Message data after conversion to a comma delimited string.
     * Same as last string returned by getMessageData()
     */
    public String message_data = null;
    /**
     * How many times have we read new data from this buffer? Can be used to determine
     * if plotting/redisplay etc is necessary.
     */
    public int new_data_count = 0;
    /**
     * NMLMessageDictionary associated with the read channel.
     */
    public DiagNMLMsgDictInterface read_msg_dict = null;
    /**
     * NMLMessageDictionary associated with the write channel.
     */
    public DiagNMLMsgDictInterface write_msg_dict = null;
    /**
     * ModuleInfo associated with this buffer. 
     */
    public ModuleInfoInterface mi = null;
    //static public volatile boolean debug_on = false;
    static private final boolean auto_disconnect = true;

    private synchronized void ChangeNMLConfigurationFile(NMLConnectionInterface nci, String new_config_file) {
	if (null != nci) {
	    nci.disconnect();
	    nci.set_configuration_file(new_config_file);
	    nci.ReadNMLConfigurationFileNoThrow();
	    nci.connectNoThrow();
	}
    }

    /**
     * Change to a new configuration file. Disconnecting and reconnecting with new
     * parameters each of the NML Connections as necessary.
     * @param new_config_file new value of configuration file name
     */
    public void SetNMLConfigFile(String new_config_file) {
	if (double_buffer_nml) {
	    ChangeNMLConfigurationFile(write_nml, new_config_file);
	}
	ChangeNMLConfigurationFile(read_nml, new_config_file);
    }
    private Vector filteredMessagesAvailable = null;
    private String filter = null;
    private boolean filter_checked = false;

    /**
     * Get the list of messages that can be sent and received from this buffer.
     * @return Vector of messages available.
     */
    public Vector getMsgsAvailable() {
	try {
	    if (null != filteredMessagesAvailable) {
		return filteredMessagesAvailable;
	    }
	    if (null == mi) {
		return null;
	    }
	    if (null == mi.getAuxAvailableMessageFilters() || filter_checked) {
		return mi.getAuxMessages();
	    }
	    Vector filter_v = mi.getAuxAvailableMessageFilters();
	    for (int i = 0; i < filter_v.size(); i++) {
		String s = (String) filter_v.elementAt(i);
		if (s.startsWith(Name + ":")) {
		    filter = s.substring(s.indexOf(':') + 1);
		    break;
		}
	    }
	    if (filter == null) {
		filter_checked = true;
		return mi.getAuxMessages();
	    }
	    filteredMessagesAvailable = new Vector();
	    Vector message_v = mi.getAuxMessages();
	    for (int i = 0; i < message_v.size(); i++) {
		String s = (String) message_v.elementAt(i);
		if (s.matches(filter+"=.*")) {
		    filteredMessagesAvailable.add(s);
		}
	    }
	    filter_checked = true;
	    return filteredMessagesAvailable;
	} catch (Exception e) {
	    e.printStackTrace();
	    return null;
	}
    }

    public String toString() {
	String str = super.toString() + " BufferInfo { Name=" + Name + ", configFile=" + configFile + ", id=" + id + ", \n";
	if (null != readerNames) {
	    str += "readerNames = [";
	    for (int i = 0; i < readerNames.size(); i++) {
		str += readerNames.get(i);
		if (i < readerNames.size() - 1) {
		    str += ", ";
		}
	    }
	    str += "], \n";
	} else {
	    str += "readerNames=null;\n";
	}
	if (null != writerNames) {
	    str += "writerNames = [";
	    for (int i = 0; i < writerNames.size(); i++) {
		str += writerNames.get(i);
		if (i < writerNames.size() - 1) {
		    str += ", ";
		}
	    }
	    str += "], \n";
	} else {
	    str += "writerNames=null;\n";
	}
	if (null != mi) {
	    Vector msgsAvailable = mi.getAuxMessages();
	    if (null != msgsAvailable) {
		str += "msgsAvailable = [";
		for (int i = 0; i < msgsAvailable.size(); i++) {
		    str += (String) msgsAvailable.elementAt(i);
		    if (i < msgsAvailable.size() - 1) {
			str += ", ";
		    }
		}
		str += "], \n";
	    } else {
		str += "msgsAvailable=null;\n";
	    }
	} else {
	    str += "msgsAvailable=null;\n";
	}

	if (null != si) {
	    str += "si.Name=" + si.Name + ", \n";
	}
	str += "getMessageDataErrors=" + getMessageDataErrors + ",";
	str += "last_new_data_time=" + last_new_data_time + ",";
	str += "last_null_time=" + last_null_time + ",";
	str += "consecutive_nulls=" + consecutive_nulls + ",";
	str += "new_data_count=" + new_data_count + "\n";
	str += "read_nml=" + read_nml + "\n";
	if (read_nml != null) {
	    str += "read_nml.get_connected()=" + read_nml.is_connected() + "\n";
	    str += "read_nml.get_connect_time()=" + read_nml.get_connect_time() + "\n";
	    str += "read_nml.get_disconnect_time()=" + read_nml.get_disconnect_time() + "\n";
	}
	if (double_buffer_nml) {
	    str += "write_nml=" + write_nml + "\n";
	    if (write_nml != null) {
		str += "write_nml.get_connected()=" + write_nml.is_connected() + "\n";
		str += "write_nml.get_connect_time()=" + write_nml.get_connect_time() + "\n";
		str += "write_nml.get_disconnect_time()=" + write_nml.get_disconnect_time() + "\n";
	    }
	}
	str += "\tread_msg_dict=" + read_msg_dict + ",\n";
	str += "} ";
	return str;
    }

    public BufferInfo() {
	num_buffers++;
	id = num_buffers;
	channelsHashtable = new Hashtable();
    }
    private int getMessageDataErrors = 0;
    private long last_new_data_time = 0;
    private long last_null_time = 0;
    private long consecutive_nulls = 0;

    /**
     * Read data from the buffer and convert to a comma delimited string.
     * @param read_id used to determine if this data is new if data has been written into the buffer 
     * but still has the same id as read_id then the data will be considered old.
     * @return comma delimeted string
     */
    public String getMessageData(int read_id) {
	String temp_data = null;
	int starting_failed_count = read_msg_dict.get_failed_count();
	int starting_error_count = read_nml.getNoThrowErrorCount();
	if (null == read_nml) {
	    getMessageDataErrors++;
	    return null;
	}
	if (!read_nml.is_connected() &&
		read_nml.get_disconnect_time() + 2000 < System.currentTimeMillis() &&
		read_nml.get_connect_time() + 2000 < System.currentTimeMillis()) {
	    read_nml.connectNoThrow();
	    read_nml.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
	    if (double_buffer_nml) {
		if (null != write_nml) {
		    write_nml.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
		}
		if (!write_nml.is_connected()) {
		    write_nml.connectNoThrow();
		}
	    } else {
		write_nml = read_nml;
	    }
	}
	try {
	    read_nml.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
	    temp_data = read_nml.peekDataStringNoThrow();
	    if (read_msg_dict.get_failed_count() > starting_failed_count) {
		getMessageDataErrors++;
	    }
	} catch (Exception e) {
	    getMessageDataErrors++;
	    e.printStackTrace();
	    message_data = null;
	    if (auto_disconnect) {
		read_nml.disconnect();
		if (double_buffer_nml) {
		    write_nml.disconnect();
		}
	    }
	    return null;
	}
	if (temp_data != null) {
	    new_data_count = read_nml.get_last_id_read();
	    message_data = temp_data;
	    last_new_data_time = System.currentTimeMillis();
	    consecutive_nulls = 0;
	    return message_data;
	} else {
	    if (starting_error_count != read_nml.getNoThrowErrorCount()) {
		getMessageDataErrors++;
		if (auto_disconnect) {
		    read_nml.disconnect();
		    if (double_buffer_nml) {
			write_nml.disconnect();
		    }
		}
		return null;
	    }
	    if (consecutive_nulls > 1 &&
		    last_new_data_time + 30000 < System.currentTimeMillis() &&
		    last_null_time + 40000 > System.currentTimeMillis() &&
		    read_nml.get_disconnect_time() + 2000 < System.currentTimeMillis() &&
		    read_nml.get_connect_time() + 60000 < System.currentTimeMillis() &&
		    new_data_count > 0) {
		getMessageDataErrors++;
		consecutive_nulls++;
		last_null_time = System.currentTimeMillis();
		if (auto_disconnect) {
		    read_nml.disconnect();
		    if (double_buffer_nml) {
			write_nml.disconnect();
		    }
		}
		return null;
	    }
	    consecutive_nulls++;
	    last_null_time = System.currentTimeMillis();
	}
	if (read_id != new_data_count) {
	    return message_data;
	}
	return null;
    }

    /**
     * Create an extra channel cloning the options in the read channel.
     * @return extra nml channel
     */
    public NMLConnectionInterface createExtraChannel() {
	NMLConnectionInterface nci = null;
	try {
	    nci = mi.get_nml_creator().NewNMLConnection();
	    nci.SetMessageDictionary(read_msg_dict);
	    nci.SetFormatConvertErrCallback(DiagNMLFormatConvertErrCallback.dnfcecb);
	    if (null != read_nml) {
		nci.set_buffer_number(read_nml.get_buffer_number());
		nci.set_port(read_nml.get_port());
		if (null != read_nml.get_configuration_file()) {
		    nci.set_configuration_file(read_nml.get_configuration_file());
		}
		if (null != read_nml.get_buffer_name()) {
		    nci.set_buffer_name(read_nml.get_buffer_name());
		}
		if (null != read_nml.get_process_name()) {
		    nci.set_process_name(read_nml.get_process_name());
		}
	    }
	    nci.ReadNMLConfigurationFileNoThrow();
	    nci.connectNoThrow();
	} catch (Exception e) {
	    e.printStackTrace();
	    return null;
	}
	return nci;
    }
    /**
     * Holds value of property connected.
     */
    private boolean connected;

    /**
     * Getter for property connected.
     * @return Value of property connected.
     */
    public synchronized boolean isConnected() {
	this.connected = true;
	if (null == read_nml) {
	    this.connected = false;
	} else if (!read_nml.is_connected()) {
	    this.connected = false;
	} else if (read_nml != write_nml) {
	    if (null == write_nml) {
		this.connected = false;
	    } else if (!write_nml.is_connected()) {
		this.connected = false;
	    }
	}
	return this.connected;
    }

    /**
     * Setter for property connected.
     * @param connected New value of property connected.
     */
    public synchronized void setConnected(boolean connected) {
	this.connected = connected;
	if (!this.connected) {
	    if (read_nml != null) {
		read_nml.disconnect();
	    }
	    if (write_nml != read_nml && write_nml != null) {
		write_nml.disconnect();
	    }
	} else {
	    if (read_nml != null && !read_nml.is_connected()) {
		read_nml.connectNoThrow();
	    }
	    if (write_nml != read_nml && write_nml != null && !write_nml.is_connected()) {
		write_nml.connectNoThrow();
	    }
	}
    }
}
