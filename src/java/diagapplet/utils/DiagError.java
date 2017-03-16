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
/*
 * DiagError.java
 *
 * Created on June 29, 2007, 1:56 PM
 *
 * To change this template, choose Tools | Template Manager
 * 
 */
package diagapplet.utils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Vector;

/**
 * Class that should be used so that errors are visible in diag error log text area.
 * and also System.err.
 * @author Will Shackleford
 */
public class DiagError {

	/** Creates a new instance of DiagError */
	public DiagError() {
	}
	private static Vector ErrorAppenders = null;

	public static void AddDiagErrorAppender(DiagErrorAppendInterface deai) {
		try {
			if (ErrorAppenders == null) {
				ErrorAppenders = new Vector();
			}
			ErrorAppenders.add(deai);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void ClearErrorAppendersList() {
		ErrorAppenders = null;
	}
	static public boolean shutting_down = false;

	public static void PrintException(Exception e)
	{
		StringWriter sw = new StringWriter();
		e.printStackTrace(new PrintWriter(sw));
		println(sw.toString());
	}

	public static void println(String s) {
		try {
			if (shutting_down) {
				return;
			}
			System.err.println(s);
			if (ErrorAppenders != null) {
				for (int i = 0; i < ErrorAppenders.size(); i++) {
					DiagErrorAppendInterface deai = (DiagErrorAppendInterface) ErrorAppenders.elementAt(i);
					deai.AppendError(s);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
