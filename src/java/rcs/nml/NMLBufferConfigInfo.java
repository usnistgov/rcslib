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
 * Class encapsulating information from the configuration file/server about an NML buffer.
 * 
 * @author Will Shackleford 
 */
public class NMLBufferConfigInfo {

	public String buffer_name = null;
	public String buffer_line = null;
	private java.util.Hashtable process_lines = new java.util.Hashtable();
	public boolean used = false;

	public String toString() {
		String retstring = super.toString();
		try {
			retstring += " = buffer_name=" + buffer_name + ",\n\t buffer_line=" + buffer_line + ",\n\t  process_lines = " + process_lines;
			java.util.Enumeration process_line_keys = process_lines.keys();
			while (process_line_keys.hasMoreElements()) {
				String pname = (String) process_line_keys.nextElement();
				String pline = (String) process_lines.get(pname);
				retstring += "\n\tpname=" + pname + ", \tpline=" + pline;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return retstring;
	}

	public synchronized String getProcessLine(String process_name) {
		try {
			if (null != process_lines && process_name != null) {
				return (String) process_lines.get(process_name);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	public synchronized void add_process_line(String process_name, String process_line) {
		try {
			if (process_name == null || process_line == null) {
				return;
			}
			if (process_lines == null) {
				process_lines = new java.util.Hashtable();
			}
			process_lines.put(process_name, process_line);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
