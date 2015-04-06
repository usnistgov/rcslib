/*****************************************************************************
DISCLAIMER:
This software was produced by the National Institute of Standards
and Technology (NIST), an agency of the U.S. government, and by statute is
not subject to copyright in the United States.  Recipients of this software
assume all responsibility associated with its operation, modification,
maintenance, and subsequent redistribution.

See NIST Administration Manual 4.09.07 b and Appendix I. 
 *****************************************************************************/
/*!
\file   rcs_states.java
\brief State declarations to match those of RCS_STAT_MSG
Note that the line printing routine are based off of http://www.fincher.org/tips/Languages/java/java.shtml

\code CVS Status:
$Author: dr_steveb $
$Revision: 1.2 $
$Date: 2009/01/19 03:59:11 $
\endcode

\author Stephen Balakirsky
\date   03/2/2005
 */
package rcs.utils;

// Import the custom message dictionary
import java.io.*;


// Import all NML, CMS, and RCS classes and interfaces
import rcs.nml.*;

public class rcs_states {

	static public final int RCS_ADMIN_ZERO = 0;
	static public final int ADMIN_UNINITIALIZED = 1;
	static public final int ADMIN_INITIALIZED = 2;
	static public final int ADMIN_SHUT_DOWN = 3;
	static public final int UNINITIALIZED_STATE = -1;
	static public final int NEW_COMMAND = -2;
	static public final int NOP_STATE = -3;
	static public final int SE0 = -10;
	static public final int SE1 = -11;
	static public final int SE2 = -12;
	static public final int SE3 = -13;
	static public final int SE4 = -14;
	static public final int SE5 = -15;
	static public final int SE6 = -16;
	static public final int SE7 = -17;
	static public final int SE8 = -18;
	static public final int SE9 = -19;
	static public final int S0 = 0;
	static public final int S1 = 1;
	static public final int S2 = 2;
	static public final int S3 = 3;
	static public final int S4 = 4;
	static public final int S5 = 5;
	static public final int S6 = 6;
	static public final int S7 = 7;
	static public final int S8 = 8;
	static public final int S9 = 9;
	static public final int S10 = 10;
	static public final int S11 = 11;
	static public final int S12 = 12;
	static public final int S13 = 13;
	static public final int S14 = 14;
	static public final int S15 = 15;
	static public final int S16 = 16;
	static public final int S17 = 17;
	static public final int S18 = 18;

	/**
	 * Get a formatted stack trace.
	 * @return String with formatted stack trace.
	 */
	public static String printWhereAmI() {
		//create exception and write its stack trace to a String
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		new Exception("printWhereAmI()").printStackTrace(pw);
		pw.close();
		String exceptionText = sw.toString();

		//skip through first two "at ..."
		for (int i = 0; i < 2; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
	/*
		System.out.println("\n **printWhereAmI(): " +
		exceptionText.substring(0,exceptionText.indexOf("at ",1)));
		 */
		return (exceptionText.substring(0, exceptionText.indexOf("at ", 1)));
	}

	/**
	 * Checks stat.state == stateIn and sets the source_file and source line 
	 * so that one can check which line of the state table matched.
	 * @param stat The status message to be written out via NML.	
	 * @param stateIn state to check, (NEW_COMMAND, or one of the S? values defined above.)
	 * @return stat.state == stateIn
	 */
	public static boolean state_match(RCS_STAT_MSG stat, int stateIn) {
		//create exception and write its stack trace to a String
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		new Exception("printWhereAmI()").printStackTrace(pw);
		pw.close();
		String exceptionText = sw.toString();

		//skip through first two "at ..."
		for (int i = 0; i < 2; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
		exceptionText = exceptionText.substring(3, exceptionText.indexOf("at ", 1));

		//	retValue = printWhereAmI();
		stat.source_file = exceptionText.getBytes();

		// get line number
		exceptionText = exceptionText.substring(exceptionText.indexOf(":") + 1, exceptionText.indexOf(")"));
		stat.source_line = Integer.parseInt(exceptionText);
		stat.line = stat.source_line;
		if (stat.state == stateIn) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Sets the source file and source line.
	 * (seems a little redundant especially if you just called state_match())
	 * @param stat message to update
	 */
	public static void state_new(RCS_STAT_MSG stat) {
		//create exception and write its stack trace to a String
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		new Exception("printWhereAmI()").printStackTrace(pw);
		pw.close();
		String exceptionText = sw.toString();

		//skip through first two "at ..."
		for (int i = 0; i < 2; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
		exceptionText = exceptionText.substring(3, exceptionText.indexOf("at ", 1));

		//	retValue = printWhereAmI();
		stat.source_file = exceptionText.getBytes();

		// get line number
		exceptionText = exceptionText.substring(exceptionText.indexOf(":") + 1, exceptionText.indexOf(")"));
		stat.source_line = Integer.parseInt(exceptionText);
		stat.line = stat.source_line;
	}

	/**
	 * Sets stat.state to the next state.
	 * @param stat message to update
	 * @param next (NEW_COMMAND, or one of the S* constants in this class.) 
	 */
	public static void state_next(RCS_STAT_MSG stat, int next) {
		stat.state = next;
	}

	/**
	 * Sets stat.status to the next status
	 * @param stat message to update
	 * @param next (NEW_COMMAND, or one of the S* constants in this class.)
	 */
	public static void status_next(RCS_STAT_MSG stat, int next) {
		stat.status = next;
	}

	/**
	 * Sets the stat.source_line and stat.source_file,
	 * called when all state_match()'s fail.
	 * @param stat message to update
	 */
	public static void state_default(RCS_STAT_MSG stat) {
		//create exception and write its stack trace to a String
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		new Exception("printWhereAmI()").printStackTrace(pw);
		pw.close();
		String exceptionText = sw.toString();

		//skip through first two "at ..."
		for (int i = 0; i < 2; i++) {
			exceptionText = exceptionText.substring(exceptionText.indexOf("at ", 1));
		}
		//clip off remaining stack trace
		exceptionText = exceptionText.substring(3, exceptionText.indexOf("at ", 1));

		//	retValue = printWhereAmI();
		stat.source_file = exceptionText.getBytes();

		// get line number
		exceptionText = exceptionText.substring(exceptionText.indexOf(":") + 1, exceptionText.indexOf(")"));
		stat.source_line = Integer.parseInt(exceptionText);
		stat.line = stat.source_line;
	}
}

