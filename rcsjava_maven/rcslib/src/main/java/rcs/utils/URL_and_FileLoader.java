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

import java.io.*;
import java.net.*;
import java.util.*;

/**
 *       Class used to read text files one line at a time
 *       either as a local file or via an HTTP server.
 * <pre>
 * Related Documentation:
 * <A HREF="http://isd.cme.nist.gov/proj/rcs_lib">RCS Library</a>, <A HREF="http://isd.cme.nist.gov/proj/rcs_lib/NMLjava.html">NML Programmers Guide (Java Version)</a>
 *
 * </pre>
 *
 * @author       Will Shackleford -- <A HREF="mailto:shackle@cme.nist.gov">shackle@cme.nist.gov</a>
 *
 */
public class URL_and_FileLoader {

    public int content_length = 0;
    public int bytes_read = 0;
    public int lines_read = 0;
    static public boolean default_use_caches = true;
    static public boolean interrupt_file_loading = false;
    static public boolean debug_file_loading = false;
    static private Vector SearchPath = null;

    static public Vector get_SearchPath() {
	return SearchPath;
    }
    static public String current_directory = null;
    public String name = null;
    public boolean is_local = false;
    public int HTTPport = 0;
    public String HTTPhost = null;
    private URL url_object = null;
    private URLConnection url_connection = null;
    private InputStreamReader isr = null;
    private BufferedReader br = null;
    private File file_object = null;
    private FileInputStream fis = null;
    //StackTraceElement last_ste[] = null;
	/*
    This appears to have been an old work around that is no longer necessary.
    private char buf[] = null;
    private static final boolean buffer_internally = false;
     */
    public static boolean debug_on = false;
    public String str = null;
    public StringTokenizer tokenizer = null;
    private boolean closed = false;

    public String toString() {
	String s = super.toString() + "\n";
	s += "\t\tname=" + name + "\n";
	s += "\t\tclosed=" + closed + "\n";
	s += "\t\tcurrent_directory=" + current_directory + "\n";
	s += "\t\tlast_line=" + last_line + "\n";
	s += "\t\tSearchPath=" + SearchPath + "\n";
	s += "\t\tinterrupt_file_loading=" + interrupt_file_loading + "\n";
	s += "\t\tbytes_read=" + bytes_read + "\n";
	s += "\t\tlines_read=" + lines_read + "\n";
	s += "\t\tcontent_length=" + content_length + "\n";
//      s+= "\t\tlast_ste="+last_ste+"\n";
//      if(null != last_ste)
//      {
//          for(int ils = 0; ils < last_ste.length ; ils++)
//          {
//              s+= "\t\t"+last_ste[ils]+"\n";
//          }
//      }
	return s;
    }

    static {
	try {
	    try {
		java.util.Properties p = java.lang.System.getProperties();
		if (null != p) {
		    String cwd = p.getProperty("user.dir");
		    if (null != cwd) {
			current_directory = cwd;
		    }
		}
	    } catch (Exception e) {
		System.out.println(" Can not determine current directory.");
		e.printStackTrace();
	    }

	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void setUseCaches(boolean _cache) {
	if (null != url_connection) {
	    url_connection.setUseCaches(_cache);
	}
    }

    public boolean getUseCaches() {
	if (null != url_connection) {
	    return url_connection.getUseCaches();
	}
	return default_use_caches;
    }

    public URL_and_FileLoader(InputStream is) {
	isr = new InputStreamReader(is);
	br = new BufferedReader(isr);
	content_length = 0;
	bytes_read = 0;
	lines_read = 0;
    }

    public URL_and_FileLoader(String _name) {
	try {
	    name = _name;
	    ConnectToFile();
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
    public boolean TryNameSucceeded = false;
    public String StringToSucceed = null;

    private boolean TryName(String name_to_try) {
	try {
	    TryNameSucceeded = false;
	    if (debug_on) {
		System.out.println("URL_and_FileLoader: Trying " + name_to_try);
	    }

	    if (name_to_try.startsWith("http:")) {
		url_object = new URL(name_to_try);
		HTTPport = url_object.getPort();
		HTTPhost = url_object.getHost();
		url_connection = url_object.openConnection();
		url_connection.setUseCaches(default_use_caches);
		isr = new InputStreamReader(url_connection.getInputStream());
		br = new BufferedReader(isr);
		content_length = url_connection.getContentLength();
		bytes_read = 0;
		is_local = false;
		lines_read = 0;
	    } else {
		if (name_to_try.startsWith("file:")) {
		    name_to_try = name.substring(5);
		}
		file_object = new File(name_to_try);
		fis = new FileInputStream(name_to_try);
		isr = new InputStreamReader(fis);
		br = new BufferedReader(isr);
		content_length = (int) file_object.length();
		bytes_read = 0;
		is_local = true;
		HTTPport = -1;
		HTTPhost = null;
		lines_read = 0;
	    }
	    TryNameSucceeded = true;
	    StringToSucceed = name_to_try;
	    return true;
	} catch (Exception e) {
	    if (debug_on) {
		System.err.println("Error trying to open to " + name_to_try);
		e.printStackTrace();
	    }
	}
	return false;
    }

    static public boolean CheckInSearchPath(String new_dir) {
	if (null != SearchPath) {
	    for (int i = 0; i < SearchPath.size(); i++) {
		String s2 = SearchPath.elementAt(i).toString();
		if (new_dir.compareTo(s2) == 0) {
		    return true;
		}
	    }
	}
	return false;
    }

    //  @SuppressWarnings("unchecked")
    static public void AddToSearchPath(String new_dir) {
	try {
	    if (null == new_dir) {
		return;
	    }
	    if (null == SearchPath) {
		SearchPath = new Vector();
	    }
	    if (SearchPath.size() > 20) {
		return;
	    }
	    if (!new_dir.startsWith("http:")) {
		File f = new File(new_dir);
		if (f.exists()) {
		    new_dir = f.getCanonicalPath();
		}
	    }
	    if (!new_dir.startsWith("http:") && File.separatorChar != '/') {
		new_dir = new_dir.replace('/', File.separatorChar);
	    }
	    if (!new_dir.endsWith("\\") && !new_dir.endsWith("/") && !new_dir.endsWith(File.separator)) {
		if (new_dir.startsWith("http:")) {
		    new_dir += "/";
		} else {
		    new_dir += File.separator;
		}
	    }
	    if (CheckInSearchPath(new_dir)) {
		return;
	    }
	    File parentFile = (new File(new_dir)).getParentFile();
	    if (!new_dir.startsWith(".") && parentFile.exists()) {
		for (int i = 0; i < SearchPath.size(); i++) {
		    String sfrom_list = SearchPath.elementAt(i).toString();
		    if (sfrom_list.startsWith(".." + File.separator)) {
			String s4 = parentFile.getCanonicalPath() + sfrom_list.substring(2);
			File f = new File(s4);
			String s5 = f.getCanonicalPath();
			if (!s5.endsWith(File.separator)) {
			    s5 = s5 + File.separator;
			}
			if (f.exists() && !CheckInSearchPath(s5)) {
			    SearchPath.addElement(s5);
			}
		    }
		}
	    } else if (new_dir.startsWith(".." + File.separator)) {
		for (int i = 0; i < SearchPath.size(); i++) {
		    String sfrom_list = SearchPath.elementAt(i).toString();
		    if (!sfrom_list.startsWith(".")) {
			parentFile = new File(sfrom_list).getParentFile();
			if (parentFile.exists()) {
			    String s4 = parentFile.getCanonicalPath() + new_dir.substring(2);
			    File f = new File(s4);
			    String s5 = f.getCanonicalPath();
			    if (!s5.endsWith(File.separator)) {
				s5 = s5 + File.separator;
			    }
			    if (f.exists() && !CheckInSearchPath(s5)) {
				SearchPath.addElement(s5);
			    }
			}
		    }
		}
	    }
	    SearchPath.addElement(new_dir);
	    String dotdot_string = File.separator + "..";
	    int dotdot_index = new_dir.indexOf(dotdot_string);
	    int slash_index = -1;
	    if (dotdot_index > 0) {
		slash_index = new_dir.lastIndexOf(File.separatorChar, dotdot_index - 1);
	    }
	    while (slash_index > 0 && dotdot_index > slash_index + 3) {
		new_dir = new_dir.substring(0, slash_index) + new_dir.substring(dotdot_index + dotdot_string.length());
		for (int i = 0; i < SearchPath.size(); i++) {
		    if (new_dir.compareTo(SearchPath.elementAt(i).toString()) == 0) {
			return;
		    }
		}
		SearchPath.add(new_dir);
		dotdot_index = new_dir.indexOf(dotdot_string);
		slash_index = -1;
		if (dotdot_index > 0) {
		    slash_index = new_dir.lastIndexOf(File.separatorChar, dotdot_index);
		}
	    }
	    if (debug_on) {
		System.out.println("URL_and_FileLoader: Adding " + new_dir + " to the search path.\n");
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private void ConnectToFile() {
	try {
//        last_ste = Thread.currentThread().getStackTrace();
	    String orig_name = name;
	    TryNameSucceeded = false;
	    if (interrupt_file_loading) {
		return;
	    }
	    if (debug_on) {
		System.out.println("URL_and_FileLoader: Openning " + name);
	    }

	    if (name.startsWith("~/") && File.separatorChar == '/' && !(new File(name).exists())) {
		name = System.getProperty("user.home") + name.substring(1);
	    }

	    if (!TryName(name)) {
		if (-1 == name.indexOf(":") && null != current_directory) {
		    File curDirFile = new File(current_directory);
		    if (curDirFile.exists() && curDirFile.isDirectory()) {
			if (File.separator.equals("/")) {
			    if (debug_on) {
				System.out.println("Replacing \\ with / in " + name);
			    }
			    name = name.replace('\\', '/');
			}
			if (File.separator.equals("\\")) {
			    if (debug_on) {
				System.out.println("Replacing / with \\ in " + name);
			    }
			    name = name.replace('/', '\\');
			}
			while ((name.startsWith("../") || name.startsWith("..\\")) && curDirFile.getParentFile() != null) {
			    name = name.substring(3);
			    curDirFile = curDirFile.getParentFile();
			}
			File file_to_try = new File(curDirFile, name);
			name = file_to_try.toString();
		    } else {
			if (!current_directory.endsWith("\\") &&
				!current_directory.endsWith("/")) {
			    name = current_directory + "/" + name;
			} else {
			    name = current_directory + name;
			}
		    }
		    TryName(name);
		}

		if (!TryNameSucceeded) {
		    if (null != SearchPath) {
			for (int i = 0; i < SearchPath.size(); i++) {
			    String dirToCheck = (String) SearchPath.elementAt(i);
			    if (null == dirToCheck) {
				break;
			    }
			    name = dirToCheck + orig_name;
			    if (TryName(name)) {
				break;
			    }
			    String tail_name = orig_name;
			    while (tail_name.startsWith("../") || tail_name.startsWith("..\\") && dirToCheck.lastIndexOf(File.separator) >= 0) {
				tail_name = tail_name.substring(3);
				dirToCheck = dirToCheck.substring(0, dirToCheck.lastIndexOf(File.separator));
				name = dirToCheck + tail_name;
				if (TryName(name)) {
				    break;
				}
			    }
			    if (TryNameSucceeded) {
				break;
			    }
			}
		    }
		}
		if (!TryNameSucceeded) {
		    if (null != SearchPath) {
			String tail = orig_name;
			int sindex = orig_name.lastIndexOf('/');
			if (sindex > 0) {
			    tail = orig_name.substring(sindex + 1);
			}
			int bindex = tail.lastIndexOf('\\');
			if (bindex > 0) {
			    tail = tail.substring(bindex + 1);
			}
			if (!tail.equals(orig_name)) {
			    for (int i = 0; i < SearchPath.size(); i++) {
				String dirToCheck = (String) SearchPath.elementAt(i);
				if (null == dirToCheck) {
				    break;
				}
				name = dirToCheck + tail;
				if (TryName(name)) {
				    break;
				}
			    }
			}
		    }
		}
	    }
	    if (!TryNameSucceeded) {
		return;
	    }


	    File ftemp = new File(name);
	    if (ftemp.exists()) {
		String tempdir = ftemp.getParent();
		if (null != tempdir) {
		    if (tempdir.length() > 1) {
			AddToSearchPath(tempdir);
		    }

		}
	    } else {
		int last_slash_index = name.lastIndexOf('/');
		int last_pathsep_index = name.lastIndexOf(File.pathSeparatorChar);
		int end_dir_index = -1;
		if (last_slash_index > last_pathsep_index) {
		    end_dir_index = last_slash_index;
		} else {
		    end_dir_index = last_pathsep_index;
		}
		if (end_dir_index > 0) {
		    AddToSearchPath(name.substring(0, end_dir_index + 1));
		}
	    }
	    /*
	    This appears to be an old work around that is no longer necessary.
	    if(buffer_internally && content_length > 0)
	    {
	    buf = new char[content_length];
	    br.read(buf,0,content_length);
	    close();
	    str = new String(buf);
	    tokenizer = new StringTokenizer(str,"\r\n");
	    }
	     */

	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
    private String last_line = null;

    public String readLine() {
	try {
//        last_ste = Thread.currentThread().getStackTrace();
	    if (null != tokenizer) {
		if (!tokenizer.hasMoreTokens()) {
		    return null;
		}
		last_line = tokenizer.nextToken();
		if (null == last_line) {
		    return null;
		}
		bytes_read += last_line.length();
		lines_read++;
		return last_line;
	    }
	    if (interrupt_file_loading) {
		return null;
	    }
	    if (null != br) {
		last_line = br.readLine();
		if (null != last_line) {
		    bytes_read += last_line.length();
		    lines_read++;
		    return last_line;
		}
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	return null;
    }

    public BufferedReader getBufferedReader() {
	return br;
    }

    public void close() {
	try {
	    closed = true;
//        last_ste = Thread.currentThread().getStackTrace();
	    if (null != br) {
		br.close();
		br = null;
	    }
	    if (null != isr) {
		isr.close();
		isr = null;
	    }
	    if (null != fis) {
		fis.close();
		fis = null;
	    }
	    file_object = null;
	    url_connection = null;
	    url_object = null;
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
