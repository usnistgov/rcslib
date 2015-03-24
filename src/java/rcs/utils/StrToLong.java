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
package rcs.utils;

import java.util.*;

/**
 * Convert a string to an integer accepting a somewhat larger range of strings
 * than String.parseInt() and adding some additional error printing.
 * 
 * @author Will Shackleford
 */
public class StrToLong {

	static int error_print_count = 0;

	static public void ErrorPrint(String s) {
		try {
			error_print_count++;
			if (error_print_count < 5) {
				Thread.dumpStack();
			}
			Throwable t = new Throwable();
			System.err.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	static public boolean bad_token = false;

	public static long convert(String str) throws NumberFormatException {
		long multiplier = 1;
		bad_token = false;
		StringTokenizer tokenizer = new StringTokenizer(str, " \t\r\n\b:;[]()+");
		while (tokenizer.hasMoreTokens()) {
			multiplier = 1;
			String token = tokenizer.nextToken();
			if (null == token) {
				throw new NumberFormatException(str);
			}
			if (token.startsWith("-")) {
				multiplier = -1;
				token = token.substring(1);
			}
			if (!Character.isDigit(token.charAt(0))) {
				ErrorPrint("StrToLong.convert(" + str + ") contains bad token{" + token + "}.\n");
				bad_token = true;
				return 0;
			}
			int point_index = token.indexOf(".");
			if (point_index > 0) {
				token = token.substring(0, point_index);
			} else if (point_index == 0) {
				return 0;
			}
			try {
				if (token.startsWith("0x")) {
					return multiplier * Long.parseLong(token.substring(2), 16);
				} else if (token.startsWith("0") && !token.equals("0")) {
					return multiplier * Long.parseLong(token.substring(1), 8);
				} else {
					return multiplier * Long.parseLong(token);
				}
			} catch (NumberFormatException e) {
				continue;
			}
		}
		throw new NumberFormatException(str);
	}

	public static void main(String args[]) {
		System.out.println("convert(3.14) = " + convert("3.14"));
		System.out.println("convert(((NMLTYPE) 1006)) = " + convert("((NMLTYPE) 1006)"));

		System.out.println("convert((32)) = " + convert("(32)"));
		System.out.println("convert((0x100)) = " + convert("(0x100)"));
	}
}
