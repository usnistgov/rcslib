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

import java.io.FileInputStream;
import java.util.*;

/**
 * The XMLFormatConverter converts NML message classes to XML.
 * XML stands for Extensible Markup Language
 * Most users should not use it directly.
 *
 * <pre>
 * Related Documentation:
 * <A HREF="http://isd.cme.nist.gov/projects/rcslib">RCS Library</a>, 
 * <A HREF="http://isd.cme.nist.gov/projects/rcslib/NMLjava.html">NML Programmers Guide (Java Version)</a>,
 * <A HREF="http://www.w3c.org/XML">Extensible Markup Language (XML)</a>
 *
 * </pre>
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 */
class XMLNodeInfo {

    String name = null;
    String content = null;
    String full = null;
    Vector children = null;
    int last_child_retrieved = 0;
    boolean simple = true;
    XMLNodeInfo parent = null;
    Hashtable attributes = null;
    int startindex = 0;
    int id = 0;
    static int last_id = 0;

    public XMLNodeInfo() {
	last_id++;
	id = last_id;
    }

    public String toString() {
	String pstring = "";
	if (parent != null &&
		parent.name != null) {
	    pstring = ",parent=" + parent.name;
	}
	String content_string = "";
	if (null != content && simple) {
	    content_string = ",content={" + content + "}";
	}
	String atstring = "";
	if (attributes != null) {
	    atstring = ",attributes={";
	    Enumeration atkeys = attributes.keys();
	    if (atkeys != null) {
		while (atkeys.hasMoreElements()) {
		    String a = (String) atkeys.nextElement();
		    String v = (String) attributes.get(a);
		    atstring += " " + a + "=\"" + v + "\"";
		}
	    }
	    atstring += "}";
	}
	String childrenstring = "";
	if (children != null) {
	    childrenstring = ",chilren=" + children.size();
	}
	return "xmlNodeInfo{name=" + name + ",id=" + id + pstring + atstring + content_string + childrenstring + "}";
    }
}

/**
 * Used internally by NML to convert messages to and from XML.
 * 
 * @author Will Shackleford
 */
public class XMLFormatConverter extends NMLFormatConverterBase {

    public static byte align_bytes[] = new byte[4];
    public static boolean auto_align = true;
    public static boolean debug_on = rcs.nml.debugInfo.debug_on;
    Vector parsedStringHash = null;
    int beginclassindex = 0;
    int endclassindex = 0;
    int beginnextsubclassindex = 0;
    int endlastsubclassindex = 0;
    int inside_class_not_found = 0;
    String current_class_string = null;
    boolean strict_mode = true;
    XMLNodeInfo xniCurrentNode = null;
    XMLNodeInfo xniTopNode = null;
    String root_name = null;
    int beginclassvar_output_string_length = 0;
    Stack beginclassvar_output_string_lengthStack = null;

    protected String convertMsgToString(NMLmsg msg) {
	try {
	    rewind();
	    if (null == msg) {
		return null;
	    }
	    decoding = false;
	    use_string = true;
	    if (null == output_string_buffer) {
		output_string_buffer = new StringBuffer(((int) (4 * msg.size)));
	    }
	    msg_to_update = msg;
	    msg_type = msg.type;
	    msg_size = msg.size;
	    bytes_in_input_stream = 0;
	    bytes_in_input_stream_known = false;
	    // msg_type = update_with_name("type",msg_type);
	    // msg_size = update_with_name("size",msg_size);
	    output_string_buffer.append(msg_type);
	    output_string_buffer.append(',');
	    output_string_buffer.append(msg_size);
	    msg.update(this);
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("convertMsgToString, output_string =" + output_string_buffer.toString());
	    }
	    return output_string_buffer.toString();
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return null;
    }

    public String convertMsgToXML(NMLmsg msg) {
	try {
	    rewind();
	    if (null == msg) {
		return null;
	    }
	    root_name = null;
	    decoding = false;
	    use_string = false;
	    class_count = 0;
	    output_string_buffer.setLength(0);
	    output_string_buffer.append("<?xml version=\"1.0\" ?>");
	    msg_to_update = msg;
	    msg_type = msg.type;
	    msg_size = msg.size;
	    autonameint = 0;
	    bytes_in_input_stream = 0;
	    bytes_in_input_stream_known = false;
	    cmd_msg_updated = false;
	    stat_msg_updated = false;
	    always_update_cmd_msg = true;
	    always_update_stat_msg = true;
	    msg.update(this);
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("convertMsgToString, output_string =" + output_string_buffer.toString());
	    }
	    return output_string_buffer.toString();
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return null;
    }

    public NMLmsg readXmlFromFile(String filename) {
	try {
	    FileInputStream fis = new FileInputStream(filename);
	    byte b[] = new byte[fis.available()];
	    fis.read(b);
	    String xmlstring = new String(b);
	    return convertXMLToMsg(xmlstring);
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return null;
    }

    public NMLmsg readXmlFromURL(java.net.URL url) {
	try {
	    java.net.URLConnection url_connection = url.openConnection();
	    java.io.InputStream is = url_connection.getInputStream();
	    byte b[] = new byte[is.available()];
	    is.read(b);
	    String xmlstring = new String(b);
	    return convertXMLToMsg(xmlstring);
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return null;
    }

    static public String limitString(String str, int maxlen) {
	try {
	    if (str.length() < maxlen) {
		return str;
	    }
	    return str.substring(0, (maxlen * 3) / 4) + "( . . . section cut here . . .)" + str.substring(str.length() - (maxlen / 4)) + "##" + (str.length() - maxlen) + " chars cut##";
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return str;
    }
    //    @SuppressWarnings("unchecked")

    void findAttributes(XMLNodeInfo xni) {
	try {

	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("findAttributes(" + xni + ")");
	    }
	    if (null == xni) {
		return;
	    }
	    if (null == xni.full) {
		return;
	    }
	    int starttagstartindex = xni.full.indexOf('<');
	    if (starttagstartindex < 0) {
		starttagstartindex = 0;
	    }
	    int starttagendindex = 0;
	    int slashindex = xni.full.indexOf('/', starttagstartindex + 1);
	    int gtindex = xni.full.indexOf('>', starttagstartindex + 1);

	    if (slashindex > 0 && (slashindex < gtindex || gtindex < 0)) {
		starttagendindex = slashindex;
	    } else {
		starttagendindex = gtindex;
	    }
	    if (starttagendindex < 0) {
		starttagendindex = xni.full.length();
	    }
	    String starttag = xni.full.substring(starttagstartindex + 1, starttagendindex);
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("findAttributes starttag=" + starttag);
	    }
	    StringTokenizer tokenizer = new StringTokenizer(starttag, " \t\r\n\b");
	    if (!tokenizer.hasMoreTokens()) {
		return;
	    }
	    String name_to_throwout = tokenizer.nextToken();
	    if (!tokenizer.hasMoreTokens()) {
		return;
	    }
	    while (tokenizer.hasMoreTokens()) {
		String tok = tokenizer.nextToken();
		while (((!tok.endsWith("\"") && tok.indexOf('"') >= 0) || tok.indexOf('=') < 0) && tokenizer.hasMoreTokens()) {
		    if (tok.indexOf('"') >= 0) {
			tok += " " + tokenizer.nextToken();
		    } else {
			tok += tokenizer.nextToken();
		    }
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("findAttributes tok=" + tok);
		}
		int eqindex = tok.indexOf('=');
		if (eqindex < 0) {
		    continue;
		}
		String name = tok.substring(0, eqindex);
		String value = tok.substring(eqindex + 1);
		int vqindex = value.indexOf('"');
		int vqlindex = value.lastIndexOf('"');
		if (vqindex >= 0 && vqlindex > vqindex) {
		    value = value.substring(vqindex + 1, vqlindex);
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("Attribute of " + name_to_throwout + " name=\"" + name + "\", value=\"" + value + "\"");
		}
		if (null == xni.attributes) {
		    xni.attributes = new Hashtable();
		}
		xni.attributes.put(name, value);
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("findAttributes(" + xni + ") returning.");
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    boolean parseBoolean(String str) throws Exception {
	if (null == str) {
	    rcs.nml.debugInfo.debugPrintStream.println("bad string for parseBoolean(" + str + ")");
	    SetErrorInUpdate("bad string for parseBoolean(" + str + ")");

	    return false;
	}
	if (str.equals("true") || str.equals("1") || str.equals("TRUE")) {
	    return true;
	}
	if (str.equals("false") || str.equals("0") || str.equals("FALSE")) {
	    return false;
	}
	rcs.nml.debugInfo.debugPrintStream.println("bad string for parseBoolean(" + str + ")");
	error_in_update = true;
	return false;
    }
    //    @SuppressWarnings("unchecked")

    Vector parseString(String xmlstring, XMLNodeInfo parent) throws Exception {
	try {
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("parseString(" + limitString(xmlstring, 300) + ", " + parent + ") xmlstring.length()=" + xmlstring.length());
	    }
	    Vector v = new Vector();
	    XMLNodeInfo xni = new XMLNodeInfo();

	    int endnodeindex = findEndOfFirstNode(xmlstring, xni);
	    int lastendnodeindex = 0;
	    if (null == root_name) {
		root_name = xni.name;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("root_name=" + root_name);
		}
	    }
	    while (endnodeindex > 0 && endnodeindex > lastendnodeindex) {
		xni.full = xmlstring.substring(lastendnodeindex, endnodeindex);
		findAttributes(xni);
		v.addElement(xni);
		xni.parent = parent;
		if (!xni.simple) {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("parsing children for " + xni);
		    }
		    xni.children = parseString(xni.content, xni);
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("parseString found (" + xni + ")");
		    rcs.nml.debugInfo.debugPrintStream.println("xni.simple=" + xni.simple + ",xni.name=" + xni.name + ",xni.content={" + limitString(xni.content, 200) + "},xni.full={" + limitString(xni.full, 200) + "}");
		}
		xni = new XMLNodeInfo();
		lastendnodeindex = endnodeindex;
		endnodeindex += findEndOfFirstNode(xmlstring.substring(lastendnodeindex), xni);
	    }
	    if (parent != null) {
		parent.children = v;
	    }
	    return v;
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("Can't parse string {" + limitString(xmlstring, 300) + "} with parent=" + parent);
	    if (strict_mode) {
		throw e;
	    } else {
		SetErrorInUpdate(e.toString());
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    }
	}
	return null;
    }

    protected NMLmsg convertRawDataToMsg(byte b[], int offset, int size) {
	try {
	    String str = new String(b, offset, size);
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.convertRawDataToMsg(b=" + b + ",b.length=" + b.length + ",offset=" + offset + ",size=" + size + ",str=" + limitString(str, 200));
	    }
	    return convertXMLToMsg(str);
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return null;
    }

    protected int convertMsgToRawData(byte b[], int size, NMLmsg msg) {
	try {
	    String str = convertMsgToXML(msg);
	    byte b2[] = str.getBytes();
	    for (int i = 0; i < size && i < b2.length && i < b.length; i++) {
		b[i] = b2[i];
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.convertRawMsgToRawData(b.length=" + b.length + ",b2.length=" + b2.length + ",size=" + size + ",msg=" + msg + ",str=" + limitString(str, 200));
	    }
	    raw_data_size = b2.length;
	    return b2.length;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return -1;
    }

    public NMLmsg convertXMLToMsg(String xml) {
	try {
	    rewind();
	    root_name = null;
	    decoding = true;
	    use_string = false;
	    class_count = 0;
	    input_string = StripXmlComments(xml);
	    xniTopNode = new XMLNodeInfo();
	    parsedStringHash = parseString(input_string, xniTopNode);
	    type_string = root_name;
	    xniCurrentNode = xniTopNode;
	    current_class_string = input_string;
	    inside_class_not_found = 0;
	    autonameint = 0;
	    beginclassindex = 0;
	    beginnextsubclassindex = xml.length();
	    endlastsubclassindex = 0;
	    endclassindex = xml.length();
	    bytes_in_input_stream = 0;
	    bytes_in_input_stream_known = false;
	    cmd_msg_updated = false;
	    stat_msg_updated = false;
	    always_update_cmd_msg = true;
	    always_update_stat_msg = true;
	    if (null == msg_dict) {
		rcs.nml.debugInfo.debugPrintStream.println("No Message Dictionary!!!");
		return null;
	    }
	    if (msg_dict.formatMsg(this) < 0) {
		rcs.nml.debugInfo.debugPrintStream.println("Format Message Error, msg_type = " + msg_type + ", msg_dict = " + msg_dict + ", formatter = " + this);
		if (first_format_error) {
		    Thread.dumpStack();
		    first_format_error = false;
		}
		return null;
	    }
	    if (msg_to_update != null) {
		((NMLmsg) msg_to_update).type = msg_type;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("convertXmlToMsg(" + limitString(xml, 200) + " returning " + msg_to_update);
	    }
	    return (NMLmsg) msg_to_update;
	} catch (Exception e) {
	    SetErrorInUpdate(e.toString());
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}
	return null;
    }

    public String toString() {
	return super.toString() + " = " + getClass().getName() + "\n}";
    }
    int autonameint = 0;

    protected String autoname() {
	autonameint++;
	return "autovar--" + autonameint;
    }

    public boolean update(boolean x) {
	return update_with_name(autoname(), x);
    }

    public void update(boolean x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public byte update(byte x) {
	return update_with_name(autoname(), x);
    }

    public void update(byte x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public char update(char x) {
	return update_with_name(autoname(), x);
    }

    public void update(char x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public short update(short x) {
	return update_with_name(autoname(), x);
    }

    public void update(short x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public int update(int x) {
	return update_with_name(autoname(), x);
    }

    public void update(int x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public long update(long x) {
	return update_with_name(autoname(), x);
    }

    public void update(long x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public float update(float x) {
	return update_with_name(autoname(), x);
    }

    public void update(float x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    public double update(double x) {
	return update_with_name(autoname(), x);
    }

    public void update(double x[], int num_elements) {
	update_with_name(autoname(), x, num_elements);
    }

    XMLNodeInfo getXni(String name) throws Exception {
	XMLNodeInfo xni = null;
	if (error_in_update && strict_mode) {
	    return null;
	}
	if (debug_on) {
	    rcs.nml.debugInfo.debugPrintStream.println("getXni(" + name + ") called with xniCurrentNode=" + xniCurrentNode);
	}
	if (null == xniCurrentNode) {
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("getXni(" + name + " returning null ((null == xniCurrentNode))");
	    }
	    return null;
	}
	if (null == xniCurrentNode.children) {
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("getXni(" + name + " returning null ((null == xniCurrentNode.children))");
	    }
	    return null;
	}
	for (int i = xniCurrentNode.last_child_retrieved; i < xniCurrentNode.children.size(); i++) {
	    xni = (XMLNodeInfo) xniCurrentNode.children.elementAt(i);
	    if (name.equals(xni.name)) {
		xniCurrentNode.last_child_retrieved = i + 1;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("getXni(" + name + ") with i=" + i + " returning (" + xni + ")");
		}
		return xni;
	    }
	}
	for (int i = 0; i < xniCurrentNode.last_child_retrieved && i < xniCurrentNode.children.size(); i++) {
	    xni = (XMLNodeInfo) xniCurrentNode.children.elementAt(i);
	    if (name.equals(xni.name)) {
		xniCurrentNode.last_child_retrieved = i + 1;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("getXni(" + name + ") with i=" + i + " returning (" + xni + ")");
		}
		return xni;
	    }
	}
	return null;
    }

    public String get_content(String name) throws Exception {
	try {
	    if (error_in_update && strict_mode) {
		return null;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") called with xniCurrentNode=" + xniCurrentNode);
	    }
	    if (!use_string) {
		if (name.equals("")) {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content with empty string returning " + xniCurrentNode.content);
		    }
		    return xniCurrentNode.content;
		}
		XMLNodeInfo xni = getXni(name);
		if (null == xni) {
		    if (strict_mode) {
			throw new Exception(" !ERROR! Can't find child " + name + " in  " + xniCurrentNode);
		    }
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") returning null.");
		    }
		    return null;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") returning " + xni.content + ".");
		}
		if (null == xni.content && next_default != null) {
		    String r = next_default;
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
		    }
		    return r;
		}
		next_default = null;
		return xni.content;
	    } else {
		String token;
		token = input_string_tokenizer.nextToken();
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") returning (token from input_string_tokenizer = " + token + ") .");
		}
		if (null == token && next_default != null) {
		    String r = next_default;
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
		    }
		    return r;
		}
		next_default = null;
		return token;
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.get_content(" + name + ") threw exception.\n");
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
		SetErrorInUpdate(e.toString());
	    }
	}
	if (next_default != null) {
	    String r = next_default;
	    next_default = null;
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
	    }
	    return r;
	}
	next_default = null;
	return null;
    }

    public String get_attribute(String name) throws Exception {
	try {
	    if (error_in_update && strict_mode) {
		return null;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("get_attribute(" + name + ") called with xniCurrentNode=" + xniCurrentNode);
	    }
	    if (!use_string) {
		if (xniCurrentNode.attributes == null) {
		    if (strict_mode) {
			throw new Exception(" !ERROR! Can't find attribute " + name + " in  " + xniCurrentNode + " which has no attributes");
		    }
		    if (next_default != null) {
			String r = next_default;
			next_default = null;
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
			}
			return r;
		    }
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_attribute(" + name + ") returning null.");
		    }
		    return null;
		}
		String str = (String) xniCurrentNode.attributes.get(name);
		if (null == str) {
		    if (strict_mode) {
			throw new Exception(" !ERROR! Can't find attribute " + name + " in  " + xniCurrentNode);
		    }
		    if (next_default != null) {
			String r = next_default;
			next_default = null;
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
			}
			return r;
		    }
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_attribute(" + name + ") returning null.");
		    }
		    return null;
		}
		if (str == null && next_default != null) {
		    String r = next_default;
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
		    }
		    return r;
		}
		next_default = null;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("get_attribute(" + name + ") returning " + str + ".");
		}
		return str;
	    } else {
		String token;
		token = input_string_tokenizer.nextToken();
		if (token == null && next_default != null) {
		    String r = next_default;
		    next_default = null;
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
		    }
		    return r;
		}
		next_default = null;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") returning (token from input_string_tokenizer = " + token + ") .");
		}
		return token;
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.get_attribute(" + name + ") threw exception.\n");
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
		SetErrorInUpdate(e.toString());
	    }
	}
	if (next_default != null) {
	    String r = next_default;
	    next_default = null;
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("get_content(" + name + ") using set defualt:" + r);
	    }
	    return r;
	}
	next_default = null;
	return null;
    }

    public boolean update_with_name(String name, boolean x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (class_count < 1) {
		return x;
	    }
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = parseBoolean(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    //				output_string += "<"+name+">"+x+"</"+name+">";
		    output_string_buffer.append('<');
		    output_string_buffer.append(name);
		    output_string_buffer.append('>');
		    output_string_buffer.append(x);
		    output_string_buffer.append("</");
		    output_string_buffer.append(name);
		    output_string_buffer.append('>');
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public byte update_with_name(String name, byte x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (class_count < 1) {
		return x;
	    }
	    if (array_val >= 0 && add_array_indexes_to_name) {
		name = name + "-" + array_val;
		array_val = -1;
	    }
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = Byte.parseByte(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public char update_with_name(String name, char x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (class_count < 1) {
		return x;
	    }
	    if (array_val >= 0 && add_array_indexes_to_name) {
		name = name + "-" + array_val;
		array_val = -1;
	    }
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = (char) Integer.parseInt(content);
		}
	    } else {

		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public short update_with_name(String name, short x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (class_count < 1) {
		return x;
	    }
	    if (array_val >= 0 && add_array_indexes_to_name) {
		name = name + "-" + array_val;
		array_val = -1;
	    }
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    if (updating_unsigned) {
			x = (short) Integer.parseInt(content);
		    } else {
			x = Short.parseShort(content);
		    }

		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public int update_with_name(String name, int x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (class_count < 1) {
		return x;
	    }
	    if (array_val >= 0 && add_array_indexes_to_name) {
		name = name + "-" + array_val;
		array_val = -1;
	    }
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    if (updating_unsigned) {
			x = (int) Long.parseLong(content);
		    } else {
			x = Integer.parseInt(content);
		    }

		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public long update_with_name(String name, long x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	if (array_val >= 0 && add_array_indexes_to_name) {
	    name = name + "-" + array_val;
	    array_val = -1;
	}
	try {
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = Long.parseLong(content);
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	    return x;
	}

	if (use_string) {
	    output_string_buffer.append(',');
	    output_string_buffer.append(x);
	} else {
	    if (name.length() > 0) {
		output_string_buffer.append('<');
		output_string_buffer.append(name);
		output_string_buffer.append('>');
	    }
	    output_string_buffer.append(x);
	    if (name.length() > 0) {
		output_string_buffer.append("</");
		output_string_buffer.append(name);
		output_string_buffer.append('>');
	    }
	    //output_string += "<"+name+">"+x+"</"+name+">";
	}
	return x;
    }

    public float update_with_name(String name, float x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	if (array_val >= 0 && add_array_indexes_to_name) {
	    name = name + "-" + array_val;
	    array_val = -1;
	}
	try {
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = Float.parseFloat(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    public double update_with_name(String name, double x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	if (array_val >= 0 && add_array_indexes_to_name) {
	    name = name + "-" + array_val;
	    array_val = -1;
	}
	try {
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    x = Double.parseDouble(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    if (name.length() > 0) {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    output_string_buffer.append(x);
		    if (name.length() > 0) {
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
		    }
		    //output_string += "<"+name+">"+x+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return x;
    }

    void convert_xml_string_to_byte_array(String name, String content, byte x[], int num_elements) throws Exception {
	try {
	    boolean inside_cdata = false;
	    if (null == content) {
		return;
	    }
	    int j = 0;
	    int i = 0;
	    for (i = 0; j < content.length() && i < num_elements; i++) {
		char c = content.charAt(j);
		if (c == '<' && !inside_cdata && j < content.length() - 10) {
		    if (content.substring(j, j + 9).equals("<![CDATA[")) {
			inside_cdata = true;
			j += 9;
			i--;
			continue;
		    }
		}
		if (c == ']' && inside_cdata && j < content.length() - 3) {
		    if (content.substring(j, j + 3).equals("]]>")) {
			inside_cdata = false;
			j += 3;
			i--;
			continue;
		    }
		}
		if (c != '&' || inside_cdata) {
		    x[i] = (byte) c;
		    j++;
		} else {
		    int semicolonindex = content.indexOf(';', j);
		    if (semicolonindex < i) {
			break;
		    }
		    String ref = content.substring(j + 1, semicolonindex);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("ref=" + ref);
		    }
		    j += ref.length() + 2;
		    if (ref.startsWith("#x")) {
			Integer hexvalue = Integer.valueOf(ref.substring(2).toUpperCase(), 16);
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("hexvalue=" + hexvalue);
			}
			x[i] = hexvalue.byteValue();
		    } else if (ref.startsWith("#")) {
			Integer decvalue = Integer.valueOf(ref.substring(1));
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("decvalue=" + decvalue);
			}
			x[i] = decvalue.byteValue();
		    } else if (ref.equals("amp")) {
			x[i] = (byte) '&';
		    } else if (ref.equals("quot")) {
			x[i] = (byte) '\"';
		    } else if (ref.equals("apos")) {
			x[i] = (byte) '\'';
		    } else if (ref.equals("lt")) {
			x[i] = (byte) '<';
		    } else if (ref.equals("gt")) {
			x[i] = (byte) '>';
		    }
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x[" + i + "]=" + x[i] + ",c=" + c + ",i=" + i + ",j=" + j);
		}
	    }
	    for (; i < num_elements; i++) {
		x[i] = 0;
	    }
	} catch (Exception e) {
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    }
	}
    }

    String convert_byte_array_to_xml_string(String name, byte x[], int num_elements) throws Exception {
	try {
	    String str = "";
	    int i = 0;
	    for (i = 0; i < num_elements; i++) {
		if (x[i] == 0) {
		    break;
		}
		char c = (char) x[i];
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x[i]=" + x[i] + ",c=" + c);
		}
		String stradd = "";
		switch (c) {
		    case '&':
			stradd = "&amp;";
			break;

		    case '<':
			str += "&lt;";
			break;

		    case '>':
			str += "&gt;";
			break;

		    case '\'':
			str += "&apos;";
			break;

		    case '\"':
			str += "&quot;";
			break;

		    case '\r':
			str += "&#x0D;";
			break;

		    case '\n':
			str += "&#x0A;";
			break;

		    default:
			if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
				c == '.' || c == ',' || c == ':' || c == ';' || c == '=' || c == '+' ||
				c == '_' || c == '-' || c == '(' || c == ')' || c == '*' || c == '^' ||
				c == '%' || c == '$' || c == '#' || c == '@' || c == '`' || c == '~' ||
				c == '/' || c == '?' || c == '\\' || c == ' ') {
			    String xstr = new String(x, i, 1);
			    stradd = xstr;
			} else {
			    int hx = (int) x[i];
			    if (hx < 0) {
				hx += 256;
			    }
			    hx = hx & 0xFF;
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("hx=" + hx);
			    }
			    String hex = Integer.toHexString(hx).toUpperCase();
			    if (hex.length() < 2) {
				hex = "0" + hex;
			    }
			    stradd = "&#x" + hex + ";";
			}
			break;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("stradd=" + stradd);
		}
		str += stradd;
	    }
	    return str;
	} catch (Exception e) {
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    }
	}
	return null;
    }

    void convert_xml_string_to_char_array(String name, String content, char x[], int num_elements) throws Exception {
	try {
	    boolean inside_cdata = false;
	    if (null == content) {
		return;
	    }
	    int j = 0;
	    int i = 0;
	    for (i = 0; j < content.length() && i < num_elements; i++) {
		char c = content.charAt(j);
		if (c == '<' && !inside_cdata && j < content.length() - 10) {
		    if (content.substring(j, j + 9).equals("<![CDATA[")) {
			inside_cdata = true;
			j += 9;
			i--;
			continue;
		    }
		}
		if (c == ']' && inside_cdata && j < content.length() - 3) {
		    if (content.substring(j, j + 3).equals("]]>")) {
			inside_cdata = false;
			j += 3;
			i--;
			continue;
		    }
		}
		if (c != '&' || inside_cdata) {
		    x[i] = c;
		    j++;
		} else {
		    int semicolonindex = content.indexOf(';', j);
		    if (semicolonindex < i) {
			break;
		    }
		    String ref = content.substring(j + 1, semicolonindex);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("ref=" + ref);
		    }
		    j += ref.length() + 2;
		    if (ref.startsWith("#x")) {
			Integer hexvalue = Integer.valueOf(ref.substring(2).toUpperCase(), 16);
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("hexvalue=" + hexvalue);
			}
			x[i] = (char) hexvalue.byteValue();
		    } else if (ref.startsWith("#")) {
			Integer decvalue = Integer.valueOf(ref.substring(1));
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("decvalue=" + decvalue);
			}
			x[i] = (char) decvalue.byteValue();
		    } else if (ref.equals("amp")) {
			x[i] = (byte) '&';
		    } else if (ref.equals("quot")) {
			x[i] = (byte) '\"';
		    } else if (ref.equals("apos")) {
			x[i] = (byte) '\'';
		    } else if (ref.equals("lt")) {
			x[i] = (byte) '<';
		    } else if (ref.equals("gt")) {
			x[i] = (byte) '>';
		    }
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x[" + i + "]=" + x[i] + ",c=" + c + ",i=" + i + ",j=" + j);
		}
	    }
	    for (; i < num_elements; i++) {
		x[i] = 0;
	    }
	} catch (Exception e) {
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    }
	}
    }

    String convert_char_array_to_xml_string(String name, char x[], int num_elements) throws Exception {
	try {
	    String str = "";
	    int i = 0;
	    for (i = 0; i < num_elements; i++) {
		if (x[i] == 0) {
		    break;
		}
		char c = (char) x[i];
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x[i]=" + x[i] + ",c=" + c);
		}
		String stradd = "";
		switch (c) {
		    case '&':
			stradd = "&amp;";
			break;

		    case '<':
			str += "&lt;";
			break;

		    case '>':
			str += "&gt;";
			break;

		    case '\'':
			str += "&apos;";
			break;

		    case '\"':
			str += "&quot;";
			break;

		    case '\r':
			str += "&#x0D;";
			break;

		    case '\n':
			str += "&#x0A;";
			break;

		    default:
			if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
				c == '.' || c == ',' || c == ':' || c == ';' || c == '=' || c == '+' ||
				c == '_' || c == '-' || c == '(' || c == ')' || c == '*' || c == '^' ||
				c == '%' || c == '$' || c == '#' || c == '@' || c == '`' || c == '~' ||
				c == '/' || c == '?' || c == '\\' || c == ' ') {
			    String xstr = new String(x, i, 1);
			    stradd = xstr;
			} else {
			    int hx = (int) x[i];
			    if (hx < 0) {
				hx += 256;
			    }
			    hx = hx & 0xFF;
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("hx=" + hx);
			    }
			    String hex = Integer.toHexString(hx).toUpperCase();
			    if (hex.length() < 2) {
				hex = "0" + hex;
			    }
			    stradd = "&#x" + hex + ";";
			}
			break;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("stradd=" + stradd);
		}
		str += stradd;
	    }
	    return str;
	} catch (Exception e) {
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    }
	}
	return null;
    }

    public void update_with_name(String name, byte x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_content(name);
		convert_xml_string_to_byte_array(name, content, x, num_elements);
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + new String(x));
		}
	    } else {
		if (use_string) {
		    for (i = 0; i < num_elements; i++) {
			if (x[i] == 0) {
			    break;
			}
		    }
		    String str = new String(x, 0, i);
		    output_string_buffer.append(',');
		    output_string_buffer.append(str);
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + (new String(x)));
		    }
		    String str = convert_byte_array_to_xml_string(name, x, num_elements);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("str=" + str);
		    }
		    if (name.equals("")) {
			output_string_buffer.append(str);
		    } else {
			output_string_buffer.append('<');
			output_string_buffer.append(name);
			output_string_buffer.append('>');
			output_string_buffer.append(str);
			output_string_buffer.append("</");
			output_string_buffer.append(name);
			output_string_buffer.append('>');
			//output_string += "<"+name+">"+str+"</"+name+">";
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, boolean x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}

	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_unsigned_with_name(String name, byte x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (decoding) {
		String content = get_content(name);
		if (content != null) {
		    for (i = 0; i < num_elements && i < content.length() / 2; i++) {
			String hex = content.substring(i * 2, i * 2 + 2).toUpperCase();
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("hex=" + hex);
			}
			Integer hexvalue = Integer.valueOf(hex, 16);
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("hexvalue=" + hexvalue);
			}
			x[i] = hexvalue.byteValue();
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("x[" + i + "]=" + x[i]);
			}
		    }
		}
	    } else {
		if (use_string) {
		    for (i = 0; i < num_elements; i++) {
			if (x[i] == 0) {
			    break;
			}
		    }
		    String str = new String(x, 0, i);
		    output_string_buffer.append(',');
		    output_string_buffer.append(str);
		} else {
		    String fullhex = "";
		    for (i = 0; i < num_elements; i++) {
			int hx = (int) x[i];
			if (hx < 0) {
			    hx += 256;
			}
			hx = hx & 0xFF;
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("hx=" + hx);
			}
			String hex = Integer.toHexString(hx).toUpperCase();
			if (hex.length() < 2) {
			    hex = "0" + hex;
			}
			fullhex += hex;
		    }
		    output_string_buffer.append('<');
		    output_string_buffer.append(name);
		    output_string_buffer.append('>');
		    output_string_buffer.append(fullhex);
		    output_string_buffer.append("</");
		    output_string_buffer.append(name);
		    output_string_buffer.append('>');
		    //output_string += "<"+name+">"+fullhex+"</"+name+">";
		}
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, char x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		String os = output_string_buffer.toString();
		String xs = x.toString();
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + xs + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + os);
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}

	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, short x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}
	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, int x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}
	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, long x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}
	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, float x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}
	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_with_name(String name, double x[], int num_elements) {
	String orig_next_default = next_default;
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (x.length < num_elements) {
		throw new Exception(" !ERROR! For " + name + ": (x.length=" + x.length + ") < (num_elements=" + num_elements + ")\n");
	    }

	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		next_default = orig_next_default;
		if (add_array_indexes_to_name) {
		    x[i] = update_with_name(name + "-" + i, x[i]);
		} else {
		    x[i] = update_with_name(name, x[i]);
		}
	    }
	    next_default = null;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }
    int class_count = 0;

    public int check_type_info(NML_ENUM_INFO info) {
	try {
	    if (use_string) {
		return msg_type;
	    }
	    if (decoding) {
		Integer I = (Integer) info.string_to_int_hash.get(root_name);
		msg_type = I.intValue();
	    } else {
		root_name = (String) info.int_to_string_hash.get(msg_type);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConvert.check_type_info(" + info + ") failed with root_name=" + root_name);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return msg_type;
    }
    String inside_base_class = null;
    int base_class_count = 0;

    public void beginBaseClass(String name) {
	inside_base_class = name;
	base_class_count++;
    }

    public void endBaseClass(String name) {
	if (base_class_count > 1) {
	    base_class_count--;
	} else {
	    base_class_count = 0;
	    inside_base_class = null;
	}
    }

    //    @SuppressWarnings("unchecked")
    public void beginClass(String name, String base) {
	try {
	    if (error_in_update && strict_mode) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("beginClass(" + name + "," + base + ") inside_class_not_found=" + inside_class_not_found + ",class_count=" + class_count);
	    }
	    if (null != inside_base_class) {
		if (!name.equals(inside_base_class)) {
		    rcs.nml.debugInfo.debugPrintStream.println("beginClass(" + name + "," + base + ") : inside_base_class=" + inside_base_class);
		}
		return;
	    }
	    if (use_string || error_in_update) {
		class_count++;
		return;
	    }
	    if (decoding) {
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("xniCurrentNode=" + xniCurrentNode);
		}
		if (class_count == 0 && xniCurrentNode == xniTopNode) {
		    XMLNodeInfo xni = getXni(name);
		    if (xni == null) {
			throw new Exception(" !ERROR! Can't find " + name + " in " + xniCurrentNode);
		    }
		    xniCurrentNode = xni;
		}
	    }
	    if (class_count <= 0 && !use_string && !decoding) {
		if (null == beginclassvar_output_string_lengthStack) {
		    beginclassvar_output_string_lengthStack = new Stack();
		}
		Integer I = Integer.valueOf(beginclassvar_output_string_length);
		beginclassvar_output_string_lengthStack.push(I);
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("beginclassvar_output_string_lengthStack.push(" + I + ")");
		}
		beginclassvar_output_string_length = output_string_buffer.length();
		output_string_buffer.append('<');
		output_string_buffer.append(name);
		output_string_buffer.append('>');
		//output_string +="<"+name+">";
	    }
	    class_count++;
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void endClass(String name, String base) {
	try {
	    if (error_in_update && strict_mode) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("endClass(" + name + "," + base + ") inside_class_not_found=" + inside_class_not_found + ",class_count=" + class_count);
	    }
	    if (null != inside_base_class) {
		return;
	    }
	    class_count--;
	    if (use_string || error_in_update) {
		return;
	    }
	    if (decoding) {
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("xniCurrentNode=" + xniCurrentNode);
		}
	    }
	    if (class_count <= 0 && !use_string && !decoding) {
		output_string_buffer.append("</");
		output_string_buffer.append(name);
		output_string_buffer.append('>');
		//output_string +="</"+name+">";
	    }
	    if (this.unboundedNames != null && this.unboundedNames.size() > class_count + 1) {
		this.unboundedNames.setSize(class_count + 1);
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    String StripXmlComments(String str) {
	try {
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("StripXmlComments(" + limitString(str, 300) + ") called.");
	    }
	    int xmlqinfostartindex = str.indexOf("<?");
	    while (xmlqinfostartindex >= 0) {
		int xmlqinfoendindex = str.indexOf("?>", xmlqinfostartindex);
		String prexmlqinfoString = str.substring(0, xmlqinfostartindex);
		String postxmlqinfoString = "";
		if (xmlqinfoendindex > 0) {
		    postxmlqinfoString = str.substring(xmlqinfoendindex + 2);
		}
		str = prexmlqinfoString + postxmlqinfoString;
		xmlqinfostartindex = str.indexOf("<?", xmlqinfostartindex);
	    }
	    int commentstartindex = str.indexOf("<!--");
	    while (commentstartindex >= 0) {
		int commentendindex = str.indexOf("-->", commentstartindex);
		String precommentString = str.substring(0, commentstartindex);
		String postcommentString = "";
		if (commentendindex > 0) {
		    postcommentString = str.substring(commentendindex + 3);
		}
		str = precommentString + postcommentString;
		commentstartindex = str.indexOf("<!--", commentstartindex);
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	if (debug_on) {
	    rcs.nml.debugInfo.debugPrintStream.println("StripXmlComments returning (" + limitString(str, 300) + ").");
	}
	return str;
    }

    int findEndOfFirstNode(String str, XMLNodeInfo xni) throws Exception {
	try {
	    if (strict_mode && error_in_update) {
		return -1;
	    }
	    int ltindex = -1;
	    int tagnameend = 0;
	    String tagname = null;
	    while (null == tagname) {
		if (ltindex < 0) {
		    ltindex = str.indexOf('<');
		} else {
		    ltindex = str.indexOf('<', ltindex + 1);
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("ltindex = " + ltindex);
		}
		if (ltindex < 0) {
		    return -1;
		}
		if (ltindex < str.length() - 10) {
		    while (str.substring(ltindex, ltindex + 9).equals("<![CDATA[") && ltindex >= 0 && ltindex < str.length() - 10) {
			ltindex = str.indexOf("]]>", ltindex + 9);
			if (ltindex < 0) {
			    return -1;
			}
			ltindex = str.indexOf('<', ltindex + 1);
		    }
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("ltindex = " + ltindex);
		}
		if (ltindex < 0) {
		    return -1;
		}
		tagnameend = ltindex + 1;
		char c = str.charAt(tagnameend);
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
		    while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '-' || c == '_') {
			tagnameend++;
			c = str.charAt(tagnameend);
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("tagnameend = " + tagnameend + ",c=" + c);
			}
		    }
		    tagname = str.substring(ltindex + 1, tagnameend);
		}
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("tagname=" + tagname);
	    }
	    xni.name = tagname;
	    int gtindex = str.indexOf('>', tagnameend);
	    if (gtindex < 0) {
		if (strict_mode) {
		    throw new Exception(" !ERROR! bad node = {" + limitString(str, 200) + "}");
		}
		return -1;
	    }
	    if (str.charAt(gtindex - 1) == '/') {
		xni.simple = true;
		xni.content = "";
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("findEndOfFirstNode(" + limitString(str.substring(0, gtindex), 200) + ") <empty/> returning " + gtindex);
		}
		return gtindex;
	    }
	    int origgtindex = gtindex;
	    int lt2 = str.indexOf('<', gtindex);
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("lt2=" + lt2);
	    }
	    if (lt2 > 0) {
		if (lt2 < str.length() - 10) {
		    while (str.substring(lt2, lt2 + 9).equals("<![CDATA[")) {
			lt2 = str.indexOf("]]>", lt2 + 9);
			if (lt2 < 0) {
			    return -1;
			}
			lt2 = str.indexOf('<', lt2 + 1);
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("lt2=" + lt2);
			}
		    }
		}
		int possibleendindex = lt2 + tagname.length() + 3;
		if (lt2 > 0 && possibleendindex < str.length() &&
			str.substring(lt2, possibleendindex).equals("</" + tagname + ">")) {
		    xni.simple = true;
		    xni.content = str.substring(origgtindex + 1, lt2);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("findEndOfFirstNode(" + limitString(str.substring(0, possibleendindex), 200) + ") returning " + possibleendindex);
		    }
		    return possibleendindex;
		}
	    }
	    int begintagcount = 1;
	    int endtagcount = 0;
	    int begintagindex = -1;
	    int endtagindex = -1;
	    while (endtagcount < begintagcount) {
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("endtagcount =" + endtagcount + ", begintagcount=" + begintagcount);
		}
		begintagindex = str.indexOf("<" + tagname, gtindex);
		while (begintagindex > 0) {
		    int endbegintagindex = begintagindex + tagname.length() + 1;
		    char c = str.charAt(endbegintagindex);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("begintagindex=" + begintagindex + ",endbegintagindex=" + endbegintagindex + ",c=" + c + ",tag=" + str.substring(begintagindex, endbegintagindex));
		    }
		    if (Character.isWhitespace(c) || c == '>' || c == '/') {
			break;
		    }
		    begintagindex = str.indexOf("<" + tagname, begintagindex + 1);
		}
		endtagindex = str.indexOf("</" + tagname, gtindex);
		while (endtagindex > 0) {
		    int endendtagindex = endtagindex + tagname.length() + 2;
		    char c = str.charAt(endendtagindex);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("endtagindex=" + endtagindex + ",endendtagindex=" + endendtagindex + ",c=" + c + ",tag=" + str.substring(endtagindex, endendtagindex + 1));
		    }
		    if (Character.isWhitespace(c) || c == '>' || c == '/') {
			break;
		    }
		    endtagindex = str.indexOf("</" + tagname, endtagindex + 1);
		}
		if (endtagindex < 0) {
		    if (strict_mode) {
			throw new Exception(" !ERROR! Can't find endtag for " + tagname + " : bad node string = {" + limitString(str, 300) + "}");
		    }
		    return -1;
		}
		if (begintagindex > 0 && begintagindex < endtagindex) {
		    begintagcount++;
		    gtindex = str.indexOf('>', begintagindex);
		    if (gtindex < 0) {
			if (strict_mode) {
			    throw new Exception(" !ERROR! bad node string = {" + limitString(str, 200) + "}");
			}
			return -1;
		    }
		} else {
		    endtagcount++;
		    gtindex = str.indexOf('>', endtagindex);
		    if (gtindex < 0) {
			if (strict_mode) {
			    throw new Exception(" !ERROR! bad node string = {" + limitString(str, 200) + "}");
			}
			return -1;
		    }
		}
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("findEndOfFirstNode(" + limitString(str.substring(0, gtindex + 1), 200) + ") returning " + (gtindex + 1));
	    }
	    xni.simple = false;
	    xni.content = str.substring(origgtindex + 1, endtagindex);
	    return gtindex + 1;
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("findEndOfFirstNode(" + limitString(str, 300) + "," + xni + ") threw exception.");
	    if (strict_mode) {
		throw e;
	    } else {
		e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
		SetErrorInUpdate(e.toString());
	    }
	}
	return -1;
    }
    //    @SuppressWarnings("unchecked")

    public void beginClassVar(String name) {
	try {
	    if (use_string || error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("beginClassVar(" + name + ") inside_class_not_found=" + inside_class_not_found + ",class_count=" + class_count);
	    }
	    if(classVarArrayIndex >= 0 && add_array_indexes_to_name) {
		name=name+"-"+classVarArrayIndex;
		classVarArrayIndex=-1;
	    }
	    if (decoding) {
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("xniCurrentNode=" + xniCurrentNode);
		}
		if (inside_class_not_found > 0) {
		    inside_class_not_found++;
		    current_class_string = "";
		    return;
		}
		XMLNodeInfo xni = getXni(name);
		if (null == xni) {
		    inside_class_not_found = 1;
		    return;
		}
		xniCurrentNode = xni;
	    }

	    if (!use_string && !decoding) {
		if (null == beginclassvar_output_string_lengthStack) {
		    beginclassvar_output_string_lengthStack = new Stack();
		}
		Integer I = Integer.valueOf(beginclassvar_output_string_length);
		beginclassvar_output_string_lengthStack.push(I);
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("beginclassvar_output_string_lengthStack.push(" + I + ")");
		}
		beginclassvar_output_string_length = output_string_buffer.length();
		output_string_buffer.append('<');
		output_string_buffer.append(name);
		output_string_buffer.append('>');
		//output_string +="<"+name+">";
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void endClassVar(String name) {
	try {
	    if (use_string || error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("endClassVar(" + name + ")");
	    }
	    if(classVarArrayIndex >= 0 && add_array_indexes_to_name) {
		name=name+"-"+classVarArrayIndex;
		classVarArrayIndex=-1;
	    }
	    if (!decoding) {
		if (beginclassvar_output_string_lengthStack != null) {
		    if (!beginclassvar_output_string_lengthStack.empty()) {
			Integer I = (Integer) beginclassvar_output_string_lengthStack.pop();
			if (debug_on) {
			    rcs.nml.debugInfo.debugPrintStream.println("beginclassvar_output_string_lengthStack.pop()=" + I);
			}
			beginclassvar_output_string_length = I.intValue();
		    } else {
			beginclassvar_output_string_length = 0;
			error_in_update = true;
			rcs.nml.debugInfo.debugPrintStream.println("beginclassvar_output_string_lengthStack.empty()");
			return;
		    }
		}
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("endClassVar(" + name + ") inside_class_not_found=" + inside_class_not_found + ",class_count=" + class_count);
	    }
	    if (decoding) {
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("xniCurrentNode=" + xniCurrentNode);
		}
		if (inside_class_not_found > 0) {
		    inside_class_not_found--;
		}
		if (inside_class_not_found > 0) {
		    return;
		}
		xniCurrentNode = xniCurrentNode.parent;
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("endClassVar(" + name + ") setting beginclassindex=" + beginclassindex + ",xniCurrentNode =" + xniCurrentNode + ",current_class_string=(" + limitString(current_class_string, 300) + ").");
		}
	    }

	    if (!use_string && !decoding) {
		output_string_buffer.append("</");
		output_string_buffer.append(name);
		output_string_buffer.append('>');
		//output_string +="</"+name+">";
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    private boolean isUnboundedClassArray(String name) {
	if (null == this.unboundedNames) {
	    return false;
	}
	if (this.unboundedNames.size() <= class_count) {
	    return false;
	}
	String ubclassname = (String) this.unboundedNames.get(class_count);
	if (null == ubclassname) {
	    return false;
	}
	if (ubclassname.compareTo(name) == 0) {
	    return true;
	}
	return false;
    }

    public void beginClassArrayElem(String name, int elemnum) {
	if (add_array_indexes_to_name && !isUnboundedClassArray(name)) {
	    beginClassVar(name + "-" + elemnum);
	} else {
	    beginClassVar(name);
	}
    }

    public void endClassArrayElem(String name, int elemnum) {
	if (add_array_indexes_to_name && !isUnboundedClassArray(name)) {
	    endClassVar(name + "-" + elemnum);
	} else {
	    endClassVar(name);
	}
    }

    public int update_enumeration_with_name(String name,
	    int enumin,
	    NML_ENUM_INFO info) {
	try {
	    if (strict_mode && error_in_update) {
		return enumin;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_enumeration_with_name(" + name + "," + enumin + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	try {
	    if (null != info) {
		if (decoding) {
		    if (null != info.string_to_int_hash) {
			String str = get_content(name);
			if (str != null) {
			    Integer I = null;
			    if (use_string) {
				I = Integer.valueOf(str);
			    } else {
				I = (Integer) info.string_to_int_hash.get(str);
			    }
			    if (I != null) {
				enumin = I.intValue();
			    } else {
				throw new Exception(" !ERROR! XMLFormatConverter.update_enumeration_with_name(" + name + ") " + str + " doesnt seem to be a valid value for this type of enum " + info.name);
			    }
			}
		    } else {
			throw new Exception(" !ERROR! XMLFormatConverter.update_enumeration_with_name(" + name + ") info.string_to_int_hash is null.\n");
		    }
		} else {
		    if (use_string) {
			output_string_buffer.append(',');
			output_string_buffer.append(enumin);
			//output_string += ","+enumin;
		    } else {
			if (null != info.int_to_string_hash) {
			    String str = (String) info.int_to_string_hash.get(Integer.valueOf(enumin));
			    if (null != str) {
				output_string_buffer.append('<');
				output_string_buffer.append(name);
				output_string_buffer.append('>');
				output_string_buffer.append(str);
				output_string_buffer.append("</");
				output_string_buffer.append(name);
				output_string_buffer.append('>');

				//output_string +="<"+name+">"+str+"</"+name+">";
				return enumin;
			    }
			    throw new Exception(" !ERROR! XMLFormatConverter.update_enumeration_with_name(" + name + ") integer value " + enumin + "is not one of the recognized values for this enumeration type " + info.name + ".\n");
			} else {
			    throw new Exception(" !ERROR! XMLFormatConverter.update_enumeration_with_name(" + name + ") info.int_to_string_hash is null.\n");
			}
		    }
		}
	    } else {
		throw new Exception(" !ERROR! XMLFormatConverter.update_enumeration_with_name(" + name + ") info is null.\n");
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
	return enumin;
    }

    public void update_enumeration_array_with_name(String name,
	    int enumin[], int num_elements,
	    NML_ENUM_INFO info) {
	try {
	    if (enumin.length < num_elements) {
		throw new Exception("XMLFormatConverter.update_enumeration_array_with_name( " + name + ") enumin.length = " + enumin.length + " but num_elements = " + num_elements + ".\n");
	    }
	    for (int i = 0; i < num_elements; i++) {
		if (error_in_update) {
		    break;
		}
		if (add_array_indexes_to_name) {
		    enumin[i] = update_enumeration_with_name(name + "-" + i, enumin[i], info);
		} else {
		    enumin[i] = update_enumeration_with_name(name, enumin[i], info);
		}
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, byte x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		convert_xml_string_to_byte_array(name, content, x, num_elements);
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + new String(x));
		}
	    } else {
		if (use_string) {
		    for (i = 0; i < num_elements; i++) {
			if (x[i] == 0) {
			    break;
			}
		    }
		    String str = new String(x, 0, i);
		    output_string_buffer.append(',');
		    output_string_buffer.append(str);
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + (new String(x)));
		    }
		    String str = convert_byte_array_to_xml_string(name, x, num_elements);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("str=" + str);
		    }
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string_buffer.toString().substring(0,upperclass_index);
			    //String out_end = output_string_buffer.toString().substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + str + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    //output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+str+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, char x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		convert_xml_string_to_char_array(name, content, x, num_elements);
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + new String(x));
		}
	    } else {
		if (use_string) {
		    for (i = 0; i < num_elements; i++) {
			if (x[i] == 0) {
			    break;
			}
		    }
		    String str = new String(x, 0, i);
		    output_string_buffer.append(',');
		    output_string_buffer.append(str);
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + (new String(x)));
		    }
		    String str = convert_char_array_to_xml_string(name, x, num_elements);
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println("str=" + str);
		    }
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string_buffer.toString().substring(0,upperclass_index);
			    //String out_end = output_string_buffer.toString().substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + str + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    //output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+str+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, short x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		StringTokenizer tz = new StringTokenizer(content, ", \t");
		while (i < num_elements && tz.hasMoreTokens()) {
		    String tok = tz.nextToken();
		    x[i] = Short.valueOf(tok).shortValue();
		    i++;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + x);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(Short.toString(x[0]));
		    for (i = 1; i < num_elements; i++) {
			output_string_buffer.append("," + Short.toString(x[i]));
		    }
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + x);
		    }
		    String insert_string = " " + name + "=\"" + Short.toString(x[0]);
		    for (i = 1; i < num_elements; i++) {
			insert_string += "," + Short.toString(x[i]);
		    }
		    insert_string += "\"";
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    output_string_buffer.insert(upperclass_index, insert_string);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, int x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		StringTokenizer tz = new StringTokenizer(content, ", \t");
		while (i < num_elements && tz.hasMoreTokens()) {
		    String tok = tz.nextToken();
		    x[i] = Integer.valueOf(tok).intValue();
		    i++;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + x);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(Integer.toString(x[0]));
		    for (i = 1; i < num_elements; i++) {
			output_string_buffer.append("," + Integer.toString(x[i]));
		    }
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + x);
		    }
		    String insert_string = " " + name + "=\"" + Integer.toString(x[0]);
		    for (i = 1; i < num_elements; i++) {
			insert_string += "," + Integer.toString(x[i]);
		    }
		    insert_string += "\"";
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    output_string_buffer.insert(upperclass_index, insert_string);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, long x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		StringTokenizer tz = new StringTokenizer(content, ", \t");
		while (i < num_elements && tz.hasMoreTokens()) {
		    String tok = tz.nextToken();
		    x[i] = Long.valueOf(tok).longValue();
		    i++;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + x);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(Long.toString(x[0]));
		    for (i = 1; i < num_elements; i++) {
			output_string_buffer.append("," + Long.toString(x[i]));
		    }
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + x);
		    }
		    String insert_string = " " + name + "=\"" + Long.toString(x[0]);
		    for (i = 1; i < num_elements; i++) {
			insert_string += "," + Long.toString(x[i]);
		    }
		    insert_string += "\"";
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    output_string_buffer.insert(upperclass_index, insert_string);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, float x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		StringTokenizer tz = new StringTokenizer(content, ", \t");
		while (i < num_elements && tz.hasMoreTokens()) {
		    String tok = tz.nextToken();
		    x[i] = Float.valueOf(tok).floatValue();
		    i++;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + x);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(Float.toString(x[0]));
		    for (i = 1; i < num_elements; i++) {
			output_string_buffer.append("," + Float.toString(x[i]));
		    }
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + x);
		    }
		    String insert_string = " " + name + "=\"" + Float.toString(x[0]);
		    for (i = 1; i < num_elements; i++) {
			insert_string += "," + Float.toString(x[i]);
		    }
		    insert_string += "\"";
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    output_string_buffer.insert(upperclass_index, insert_string);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public void update_attribute_with_name(String name, double x[], int num_elements) {
	try {
	    if (strict_mode && error_in_update) {
		return;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + "," + num_elements + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return;
	}
	int i = 0;
	try {
	    if (num_elements > x.length) {
		throw new Exception(" !ERROR! update_with_name(" + name + ") num_elements=" + num_elements + " but x.length=" + x.length + "\n");
	    }

	    if (decoding) {
		String content = get_attribute(name);
		StringTokenizer tz = new StringTokenizer(content, ", \t");
		while (i < num_elements && tz.hasMoreTokens()) {
		    String tok = tz.nextToken();
		    x[i] = Double.valueOf(tok).doubleValue();
		    i++;
		}
		if (debug_on) {
		    rcs.nml.debugInfo.debugPrintStream.println("name=" + name + ",x=" + x);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(Double.toString(x[0]));
		    for (i = 1; i < num_elements; i++) {
			output_string_buffer.append("," + Double.toString(x[i]));
		    }
		} else {
		    if (debug_on) {
			rcs.nml.debugInfo.debugPrintStream.println(name + "=" + x);
		    }
		    String insert_string = " " + name + "=\"" + Double.toString(x[0]);
		    for (i = 1; i < num_elements; i++) {
			insert_string += "," + Double.toString(x[i]);
		    }
		    insert_string += "\"";
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    output_string_buffer.insert(upperclass_index, insert_string);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    SetErrorInUpdate(e.toString());
	}
    }

    public boolean update_attribute_with_name(String name, boolean x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = parseBoolean(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);

			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public byte update_attribute_with_name(String name, byte x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Byte.parseByte(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public char update_attribute_with_name(String name, char x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = (char) Integer.parseInt(content);
		}
	    } else {
		if (use_string) {
		    output_string_buffer.append(',');
		    output_string_buffer.append(x);
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public short update_attribute_with_name(String name, short x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Short.parseShort(content);
		}
	    } else {
		if (use_string) {
		    //output_string += ","+x;
		    output_string_buffer.append(',');
		    output_string_buffer.append('x');
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);

			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public int update_attribute_with_name(String name, int x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Integer.parseInt(content);
		}
	    } else {
		if (use_string) {
		    //output_string += ","+x;
		    output_string_buffer.append(',');
		    output_string_buffer.append('x');
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    //output_string = out_start +insert_string +out_end;

			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public long update_attribute_with_name(String name, long x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Long.parseLong(content);
		}
	    } else {
		if (use_string) {
		    //output_string += ","+x;
		    output_string_buffer.append(',');
		    output_string_buffer.append('x');
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    //output_string = out_start +insert_string +out_end;

			    // output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public float update_attribute_with_name(String name, float x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Float.parseFloat(content);
		}
	    } else {
		if (use_string) {
		    //output_string += ","+x;
		    output_string_buffer.append(',');
		    output_string_buffer.append('x');
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);
			    //output_string = out_start +insert_string +out_end;

			    //output_string += output_string.substring(0,upperclass_index)+" "+name+"=\""+x+"\""+output_string.substring(upperclass_index);
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    public double update_attribute_with_name(String name, double x) {
	try {
	    if (strict_mode && error_in_update) {
		return x;
	    }
	    if (debug_on) {
		rcs.nml.debugInfo.debugPrintStream.println("XMLFormatConverter.update_with_name(" + name + "," + x + ") class_count=" + class_count + ",decoding=" + decoding + ",use_string=" + use_string + ",output_string=" + output_string_buffer.toString());
	    }
	} catch (Exception e) {
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	}

	if (class_count < 1) {
	    return x;
	}
	try {
	    if (decoding) {
		String content = get_attribute(name);
		if (content != null) {
		    x = Double.parseDouble(content);
		}
	    } else {
		if (use_string) {
		    //output_string += ","+x;
		    output_string_buffer.append(',');
		    output_string_buffer.append('x');
		} else {
		    int upperclass_index = beginclassvar_output_string_length;
		    if (upperclass_index > 0) {
			upperclass_index = output_string_buffer.toString().indexOf('>', upperclass_index);
			if (upperclass_index > 1) {
			    //String out_start = output_string.substring(0,upperclass_index);
			    //String out_end = output_string.substring(upperclass_index);
			    String insert_string = " " + name + "=\"" + x + "\"";
			    if (debug_on) {
				rcs.nml.debugInfo.debugPrintStream.println("insert_string=" + limitString(insert_string, 100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_start="+limitString(out_start,100));
				//rcs.nml.debugInfo.debugPrintStream.println("out_end="+limitString(out_end,100));

			    }
			    output_string_buffer.insert(upperclass_index, insert_string);

			    //output_string = out_start +insert_string +out_end;
			}
		    }
		}
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return x;
    }

    int count_items(String name) throws Exception {
	int items = 0;
	if (null == xniCurrentNode) {
	    return 0;
	}
	if (null == xniCurrentNode.children) {
	    return 0;
	}
	XMLNodeInfo xni = null;
	for (int i = 0; i < xniCurrentNode.children.size(); i++) {
	    xni = (XMLNodeInfo) xniCurrentNode.children.elementAt(i);
	    if (name.equals(xni.name)) {
		items++;
	    }
	}
	return items;
    }

    public int update_dla_length_with_name(String name, int x) {
	if(error_in_update) {
	    return 0;
	}
	try {
	    if (use_string || add_array_indexes_to_name) {
		return update_with_name(name, x);
	    } else if (decoding) {
		String arrayname = name;
		if (arrayname.endsWith("_length")) {
		    arrayname = arrayname.substring(0, arrayname.length() - 7);
		}
		x = count_items(arrayname);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_dla_length_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }
    private Vector unboundedNames = null;

    public int get_length_of_unbounded(String typename, String varname, Object[] oarray) {
	try {
	    int l = 0;
	    if (decoding) {
		l = count_items(varname);
	    } else {
		if (null == oarray) {
		    l = 0;
		} else {
		    l = oarray.length;
		}
	    }
	    if (l > 0) {
		if (null == unboundedNames) {
		    unboundedNames = new Vector();
		}
		unboundedNames.setSize(class_count + 1);
		unboundedNames.setElementAt(varname, class_count);
	    }
	    return l;
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in get_length_of_unbounded : typename=" + typename + ",varname=" + varname + ",oarray = " + oarray);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return -1;
    }

    public byte[] update_unbounded_attribute_with_name(String name, byte[] x) {
	try {
	    if (decoding) {
		boolean orig_strict_mode = strict_mode;
		strict_mode = false;
		String str = get_attribute(name);
		strict_mode = orig_strict_mode;
		if (x == null && null != str && str.length() > 0) {
		    x = new byte[str.length() + 1];
		}
		if (null != str && str.length() > 0 && x.length < str.length() + 1) {
		    x = new byte[str.length() + 1];
		}
		if (str == null) {
		    x = new byte[1];
		    x[0] = 0;
		    return x;
		}
		if (str.length() < 1) {
		    x = new byte[1];
		    x[0] = 0;
		    return x;
		}
	    }
	    if (x != null) {
		update_attribute_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public byte[] update_unbounded_with_name(String name, byte[] x) {
	try {
	    if (decoding) {
		boolean orig_strict_mode = strict_mode;
		strict_mode = false;
		String str = get_content(name);
		strict_mode = orig_strict_mode;
		if (x == null && null != str && str.length() > 0) {
		    x = new byte[str.length() + 1];
		}
		if (null != str && str.length() > 0 && x.length < str.length() + 1) {
		    x = new byte[str.length() + 1];
		}
		if (str == null) {
		    x = new byte[1];
		    x[0] = 0;
		    return x;
		}
		if (str.length() < 1) {
		    x = new byte[1];
		    x[0] = 0;
		    return x;
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public short[] update_unbounded_with_name(String name, short[] x) {
	try {
	    if (decoding) {
		int len = count_items(name);
		if (len < 1) {
		    x = null;
		    return x;
		}
		if (null == x) {
		    x = new short[len];
		}
		if (x.length != len) {
		    x = new short[len];
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public int[] update_unbounded_with_name(String name, int[] x) {
	try {
	    if (decoding) {
		int len = count_items(name);
		if (len < 1) {
		    x = null;
		    return x;
		}
		if (null == x) {
		    x = new int[len];
		}
		if (x.length != len) {
		    x = new int[len];
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public long[] update_unbounded_with_name(String name, long[] x) {
	try {
	    if (decoding) {
		int len = count_items(name);
		if (len < 1) {
		    x = null;
		    return x;
		}
		if (null == x) {
		    x = new long[len];
		}
		if (x.length != len) {
		    x = new long[len];
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public float[] update_unbounded_with_name(String name, float[] x) {
	try {
	    if (decoding) {
		int len = count_items(name);
		if (len < 1) {
		    x = null;
		    return x;
		}
		if (null == x) {
		    x = new float[len];
		}
		if (x.length != len) {
		    x = new float[len];
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public double[] update_unbounded_with_name(String name, double[] x) {
	try {
	    if (decoding) {
		int len = count_items(name);
		if (len < 1) {
		    x = null;
		    return x;
		}
		if (null == x) {
		    x = new double[len];
		}
		if (x.length != len) {
		    x = new double[len];
		}
	    }
	    if (x != null) {
		update_with_name(name, x, x.length);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_unbounded_with_name : name=" + name + ",x = " + x);
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
	return (x);
    }

    public void update_CMS_TIME(CMS_TIME time) {
	try {
	    if (decoding) {
		beginClass("CMS_TIME", null);
		time.setWithString(get_content(""));
		endClass("CMS_TIME", null);
	    } else {
		beginClass("CMS_TIME", null);
		output_string_buffer.append(time.toString());
		endClass("CMS_TIME", null);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_CMS_TIME");
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
    }

    public void update_CMS_DATE(CMS_DATE date) {
	try {
	    if (decoding) {
		beginClass("CMS_DATE", null);
		date.setWithString(get_content(""));
		endClass("CMS_DATE", null);
	    } else {
		beginClass("CMS_DATE", null);
		output_string_buffer.append(date.toString());
		endClass("CMS_DATE", null);
	    }
	} catch (Exception e) {
	    rcs.nml.debugInfo.debugPrintStream.println("error in update_CMS_DATE");
	    e.printStackTrace(rcs.nml.debugInfo.debugPrintStream);
	    error_in_update = true;
	}
    }
}
