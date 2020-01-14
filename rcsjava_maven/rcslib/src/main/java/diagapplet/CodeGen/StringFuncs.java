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

import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.Iterator;
import java.util.Vector;
import rcs.utils.StackTracePrinter;

/**
 * Class with some utility funtions used handling Strings, environment variables etc.
 * @author Will Shackleford
 */
public class StringFuncs {

    static void ErrorPrint(String s) {
        try {
            Throwable t = new Throwable();
            if (ModuleInfo.curFileName != null && ModuleInfo.curFileName.length() > 0 && ModuleInfo.curFileLineNumber > 0) {
                System.err.println(ModuleInfo.curFileName + ":" + ModuleInfo.curFileLineNumber + " (" + StackTracePrinter.ThrowableToShortList(t) + ")  " + s);
                return;
            }
            System.err.println(StackTracePrinter.ThrowableToShortList(t) + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void DetermineDependancyRank(StructureTypeInfo ti,
            CodeGenCommonInterface2 cgc,
            Hashtable sin) {
        try {
            if (!ti.dependancy_rank_determined) {
                int max_var_rank = 0;
                StringTokenizer st = new StringTokenizer(ti.PreFinalPassInfo, ";");
                while (st.hasMoreTokens()) {
                    String info_token = st.nextToken();
                    SplitInfoToken sit = new SplitInfoToken(info_token);
                    if (null == sit.cpp_type) {
                        ErrorPrint("bad info_token \"" + info_token + "\" in ti={" + ti + "} \n\n");
                        continue;
                    }
                    boolean is_class = cgc.CheckForCppClass(sit.cpp_type);
                    if (is_class) {
                        StructureTypeInfo var_type_info = (StructureTypeInfo) sin.get(sit.cpp_type);
                        if (null == var_type_info) {
                            ErrorPrint("Can't find info for " + sit.cpp_type);
                        } else if (!var_type_info.dependancy_rank_determined) {
                            DetermineDependancyRank(var_type_info, cgc, sin);
                        }
                        if (null != var_type_info && max_var_rank < var_type_info.dependancy_rank + 1) {
                            max_var_rank = var_type_info.dependancy_rank + 1;
                        }
                    }
                }
                ti.dependancy_rank = max_var_rank;
                ti.dependancy_rank_determined = true;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //    @SuppressWarnings("unchecked")
    public static String[] SortClassList(String orig_classlist[], CodeGenCommonInterface2 cgc, Hashtable sin) {
        try {
            Vector type_info_vector = new Vector();
            for (int i = 0; i < orig_classlist.length; i++) {
                StructureTypeInfo ti1 = (StructureTypeInfo) sin.get(orig_classlist[i]);
                ti1.dependancy_rank_determined = false;
                ti1.dependancy_rank = 0;
                type_info_vector.addElement(ti1);
            }
            for (int j = 0; j < type_info_vector.size(); j++) {
                StructureTypeInfo ti2 = (StructureTypeInfo) type_info_vector.elementAt(j);
                DetermineDependancyRank(ti2, cgc, sin);
            }
            TreeSet ts = new TreeSet(type_info_vector);
            Iterator itr = ts.iterator();
            String new_array[] = new String[ts.size()];
            int k = 0;
            while (itr.hasNext()) {
                StructureTypeInfo ti3 = (StructureTypeInfo) itr.next();
                new_array[k] = ti3.getName();
                k++;
            }
            return new_array;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String GetParameter(String strName, String args[], java.util.Hashtable optionsHashtable, String unused_args[]) {
        String strValue = null;
        try {
            if (null != args) {
                int i;
                String strArg = strName + "=";
                String fullargstring = "";

                for (int j = 0; j < args.length; j++) {
                    fullargstring += "args[" + j + "]=" + args[j] + ", ";
                }

                for (i = 0; i < args.length; i++) {
                    if (args[i].length() <= strArg.length()) {
                        continue;
                    }

                    if (strArg.equalsIgnoreCase(args[i].substring(0, strArg.length()))) {

                        // Found matching parameter on command lineNumber, so extract its value.
                        // If in double quotes, remove the quotes.
                        //---------------------------------------------------------------
                        strValue = args[i].substring(strArg.length());
                        if (strValue.startsWith("\"")) {
                            strValue = strValue.substring(1);
                            if (strValue.endsWith("\"")) {
                                strValue = strValue.substring(0, strValue.length() - 1);
                            }
                        }
//                        System.out.println("used "+args[i]);
                        if (null != unused_args) {
                            for (int j = 0; j < unused_args.length; j++) {
                                if (unused_args[j] != null && unused_args[j].compareTo(args[i]) == 0) {
                                    unused_args[j] = null;
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (null == strValue && null != optionsHashtable) {
                strValue = (String) optionsHashtable.get(strName);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        try {
            if (null == strValue) {
                strValue = StringFuncs.getenv(strName);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
        return strValue;
    }

    private static synchronized String internal_getenv(String s) {
        try {
            java.util.Properties p = System.getProperties();
            if (p == null) {
                return null;
            }
            String v = p.getProperty(s);
            if (v != null) {
                return v;
            }
            String jv = p.getProperty("java.version");
            if (jv == null) {
                return null;
            }
            if (jv.startsWith("1.4")) {
                return null;
            }
            return System.getenv(s);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    static private Hashtable ht = null;
    static private String env_debug_string = null;

    public static synchronized String getenv(String s) {
        if (null != ht) {
            String sv = (String) ht.get(s);
            if (null != sv) {
                return sv;
            }
        }
        if (null == ht) {
            ht = new Hashtable();
        }
//	if(null == env_debug_string)
//	{
//	    env_debug_string ="";
//	}
        String sv = internal_getenv(s);
//	env_debug_string += s+"="+sv+";\n";
        if (null != sv) {
            ht.put(s, sv);
        }
//	System.out.println("env_debug_string="+env_debug_string);
//	System.out.println("Properties:");
//	System.getProperties().list(System.out);
        return sv;
    }

    public static String replaceAllInString(String orig_str, String findme, String replace_with) {
        String outstr = orig_str;
        try {
            int index = outstr.indexOf(findme);
            int findmelen = findme.length();
            while (index >= 0) {
                if (index + findmelen < outstr.length()) {
                    outstr = outstr.substring(0, index) + replace_with
                            + outstr.substring(index + findmelen);
                } else {

                    outstr = outstr.substring(0, index) + replace_with;
                }
                index = outstr.indexOf(findme, index + 1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return outstr;
    }

    public static String replaceFirstInString(String orig_str, String findme, String replace_with) {
        String outstr = orig_str;
        try {
            int index = outstr.indexOf(findme);
            int findmelen = findme.length();
            if (index >= 0) {
                if (index + findmelen < outstr.length()) {
                    outstr = outstr.substring(0, index) + replace_with
                            + outstr.substring(index + findmelen);
                } else {

                    outstr = outstr.substring(0, index) + replace_with;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return outstr;
    }

    static public String replace_white_space(String in) {
        String out = in.replace(' ', '_');
        out = out.replace('\t', '_');
        out = out.replace('\n', '_');
        out = out.replace('\r', '_');
        return out;
    }
}
