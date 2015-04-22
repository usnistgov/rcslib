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

import rcs.utils.URL_and_FileLoader;

/**
 * The command line only version of the CodeGenerator and generally newer preferred
 * method of creating format/update functions. Uses fake interfaceses GUI classes to eliminate
 * dependancies on java.awt and javax.swing needed by the older graphical tool. (CodeGen.java)
 * @author Will Shackleford  {@literal <william.shackleford@nist.gov>}
 */
public class CodeGenCmdLine {
    //public static String []orig_args=null;

    private static boolean debug_on = false;
    private static String includePath = "";
    private static boolean print_prompt = true;

    /**
     * Main function, call with "--help" to see full list of possible arguments.
     * or the name of a C++ header file to generate the defualt format and update functions.
     * @param args
     */
    public static void main(String args[]) {
        try {
            for (int i = 0; i < args.length; i++) {
                if (args[i].equalsIgnoreCase("--help")
                        || args[i].equalsIgnoreCase("-help")
                        || args[i].equalsIgnoreCase("help")) {
                    if (!CodeGenCommon.help_printed) {
                        CodeGenCommon.printHelp();
                    }
                }
                if (args[i].compareTo("-I") == 0 && i < args.length-2) {
                    URL_and_FileLoader.AddToSearchPath(args[i].substring(2));
                    includePath += args[i+1] + ";";
                    i++;
                    continue;
                }
                if (args[i].startsWith("-I") && i < args.length -1) {
                    URL_and_FileLoader.AddToSearchPath(args[i].substring(2));
                    includePath += args[i].substring(2) + ";";
                    continue;
                }
//                System.out.println("i = " + i);
//                System.out.println("args[i] = " + args[i]);
                if (args[i].compareTo("--Exclude") == 0 && i < args.length-1) {
                    ModuleInfo.addExcludedIncludePattern(args[i+1]);
                    i++;
                    continue;
                }

                if (args[i].startsWith("-o") && i < args.length - 1) {
                    i++;
                    CodeGenCommon.SetOutputFileName(args[i]);
                    continue;
                }
                if (args[i].startsWith("-D") && args[i].length() > 2) {
                    ModuleInfo.AddDCmdOption(args[i]);
                    continue;
                }
                if (args[i].indexOf('=') < 0) {
                    if (args[i].endsWith(".gen")) {
                        args[i] = "script=" + args[i];
                    } else {
                        args[i] = "HHFile=" + args[i];
                    }
                }
                if (args[i].equals("debug_on=true")) {
                    debug_on = true;
                }
                if (args[i].equals("noprompt")) {
                    print_prompt = false;
                }
            }

            if (debug_on) {
                System.out.println("cgc = new CodeGenCommon();");
            }
            CodeGenCommonInterface2 cgc = new CodeGenCommon();
            cgc.set_update_with_name(true);
            cgc.set_debug_on(debug_on);
            cgc.set_ClassList(new diagapplet.utils.FakeFastListPanel());
            cgc.append_includePath(includePath);
            cgc.set_print_prompt(print_prompt);

            //orig_args = args;

            if (debug_on) {
                System.out.println("cgc.GetParameters(" + args + ");");
            }
            cgc.GetParameters(args);

            if (debug_on) {
                System.out.println("cgc.get_script_file() = " + cgc.get_script());
            }

            cgc.set_display_on(false);
            if (cgc.get_script() == null) {
                if (debug_on) {
                    System.out.println("cgc.set_script_file(cgc.createScriptFile(" + args + "));");
                }
                cgc.set_script(cgc.createScript(args));
                if (debug_on) {
                    System.out.println("cgc.get_script_file() = " + cgc.get_script());
                }
            }
            if (debug_on) {
                System.out.println("cgc.RunScriptFile(" + cgc.get_script() + ");");
            }
            cgc.RunScript(cgc.get_script());
            if (cgc.get_error_count() != 0) {
                System.exit(1);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
