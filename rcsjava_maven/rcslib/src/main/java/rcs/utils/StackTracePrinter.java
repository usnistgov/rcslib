package rcs.utils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.StringTokenizer;
import rcs.nml.NMLConnection;

/**
 * Utilities for printing StackTrace info with old JDK 1.4 API's
 *
 * @author shackle
 */
public class StackTracePrinter {


	static public String ThrowableToStackTraceString(final Throwable t) {
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		t.printStackTrace(pw);
		String s = sw.toString();
                int at0 = s.indexOf("at ");
                if(at0 > 0) {
                    s = s.substring(at0+3);
                }
                int at1 = s.indexOf("at ");
                int at2 = (at1 > 0)?s.indexOf("at ",at1+1):0;
                int at3 = (at2 > 0)?s.indexOf("at ",at2+1):0;
                int at4 = (at3 > 0)?s.indexOf("at ",at3+1):0;
                if(at4 > 0) {
                    s = s.substring(0, at4);
                }
                int evq_index = s.indexOf("at java.awt.EventQueue");
                if(evq_index > 0) {
                    s = s.substring(0, evq_index);
                }
                int ive_index = s.indexOf("at java.awt.event.InvocationEvent");
                if(ive_index > 0) {
                    s = s.substring(0, ive_index);
                }
                return s;
	}

	static public String ThrowableTextToFileName(String exceptionText, final int stack_level) {
		//skip through first two "at ..."
		for (int i = 0; i < stack_level; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
		exceptionText = exceptionText.substring(3, exceptionText.indexOf('\n', 1));

		int pindex = exceptionText.indexOf('(');
		if (pindex < 0) {
			return null;
		}
		exceptionText = exceptionText.substring(pindex + 1, exceptionText.indexOf(':', pindex));

		//	retValue = printWhereAmI();
		return exceptionText;
	}

	static public String ThrowableToShortList(final Throwable t) {
		String exceptionText = ThrowableToStackTraceString(t);
		return ThrowableTextToShortList(exceptionText);
	}

	static public String ThrowableTextToShortList(String exceptionText) {
		exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		StringTokenizer tz = new StringTokenizer(exceptionText, "\r\n");
		String out = "";
		while (tz.hasMoreTokens()) {
			String line = tz.nextToken();
                        if(line.contains("Unknown Source")) {
                            continue;
                        }
			int popen_index = line.indexOf('(');
			int pclose_index = line.indexOf(')');
			if (popen_index < 0 || pclose_index <= popen_index) {
				break;
			}
			String fileline = line.substring(popen_index + 1, pclose_index);
			if (out.length() > 0) {
				out += ",";
			}
			out += fileline;
		}
		return out;
	}

	static public String ThrowableFileName(final Throwable t, final int stack_level) {
		String exceptionText = ThrowableToStackTraceString(t);
		return ThrowableTextToFileName(exceptionText, stack_level);
	}

	static public int ThrowableToLine(final Throwable t, final int stack_level) {
		String exceptionText = ThrowableToStackTraceString(t);
		return ThrowableTextToLine(exceptionText, stack_level);
	}

	static public int ThrowableTextToLine(String exceptionText, final int stack_level) {
		//skip through first two "at ..."
		for (int i = 0; i < stack_level; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
		exceptionText = exceptionText.substring(3, exceptionText.indexOf('\n', 1));

		// get line number
		exceptionText = exceptionText.substring(exceptionText.indexOf(':') + 1, exceptionText.indexOf(')'));
		return Integer.parseInt(exceptionText);
	}

}
