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

import rcs.nml.NMLMessageDictionary;
import rcs.nml.NMLFormatConverter;
import rcs.nml.NMLFormatConverterBase;

/**
 * * NMLMessageDictionaries allow the marshalling and unmarshalling of NML messages.
 * They are generally created using the CodeGenerator and therefore have type information
 * built in at compile-time. The Diag version must discover the type information at runtime.
 * 
 * @author Will Shackleford
 */
public class DiagNMLMsgDict implements NMLMessageDictionary, DiagNMLMsgDictInterface {

    private ModuleInfo module_info = null;
    private DiagNMLmsg diag_nml_msg = null;

    public long getEstimatedSize(int _type) {
		return 0;
    }

    public long getMaxEstimatedSize() {
        return 0;
    }
    
    /**
     * When set to true debug_on enables extra calls to System.out for debugging.
     */
    public static volatile boolean debug_on = false;
    boolean cmd_stream = false;
    boolean stat_stream = false;
	
    public void tokensNotUsed(int num_tokens, String input_string, boolean warn_given) {
        if (null != diag_nml_msg) {
            diag_nml_msg.tokensNotUsed(num_tokens, input_string, warn_given);
        } else {
            if (!warn_given) {
                Thread.dumpStack();
				System.err.println("diag_nml_msg == null.");
                System.err.println("module_info=" + module_info);
            }
        }
    }

    public void bytesNotUsed(int bytes_in_input_stream, String bufName, boolean warn_given) {
        if (null != diag_nml_msg) {
            diag_nml_msg.bytesNotUsed(bytes_in_input_stream, bufName, warn_given);
        } else {
            if (!warn_given) {
                Thread.dumpStack();
                System.err.println("module_info=" + module_info);
            }
        }
    }

    static public void ErrorPrint(String s) {
        try {
            Throwable t = new Throwable();
            StackTraceElement ste[] = t.getStackTrace();
            if (debug_on) {
                System.out.println("ErrorPrint + " + ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
            }
            System.err.println(ste[1].getFileName() + ":" + ste[1].getLineNumber() + " " + s);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    boolean first_misc_error_occured = false;

    public void miscError() {
        if (null != diag_nml_msg) {
            diag_nml_msg.miscError();
        } else {
            if (!first_misc_error_occured) {
                Thread.dumpStack();
                System.err.println("module_info=" + module_info);
            }
        }
        first_misc_error_occured = true;
    }

    public void miscError(Exception e) {
        if (null != diag_nml_msg) {
            diag_nml_msg.miscError(e);
        } else {
            if (!first_misc_error_occured) {
                Thread.dumpStack();
                System.err.println("module_info=" + module_info);
                if (null != e) {
                    e.printStackTrace();
                }
            }
        }
        first_misc_error_occured = true;
    }

    public void SetModuleInfoObject(ModuleInfoInterface o) {
        module_info = (ModuleInfo) o;
    }
    int my_failed_count = 0;

    public int get_failed_count() {
        return DiagNMLmsg.failed_count;
    }

    public String toString() {
        return super.toString() + " = " + getClass().getName() + " {\n" +
                "module_info.Name=" + module_info.Name + ";\n" +
                "diag_nml_msg=" + diag_nml_msg + ";\n}";
    }

    public int formatMsg(NMLFormatConverter NMLfc) {
        try {
            NMLFormatConverterBase fc_base = (NMLFormatConverterBase) NMLfc;
            if (null == module_info) {
                System.err.println("DiagNMLMessageDictionary: module_info == null.");
                my_failed_count++;
                return -1;
            }
            if (null == ModuleInfo.m_structInfoHashTable) {
                System.err.println("DiagNMLMessageDictionary: ModuleInfo.m_structInfoHashTable == null.");
                my_failed_count++;
                return -1;
            }
            if (fc_base.decoding) {
                if (debug_on) {
                    System.out.println("DiagNMLMessageDictionary decoding data.");
                }
                if (null == diag_nml_msg) {
                    diag_nml_msg = new DiagNMLmsg(fc_base.msg_type);
                }
                diag_nml_msg.type = fc_base.msg_type;
            } else {
                if (debug_on) {
                    System.out.println("DiagNMLMessageDictionary encoding data.");
                }
                diag_nml_msg = (DiagNMLmsg) fc_base.msg_to_update;
            }
            fc_base.msg_to_update = diag_nml_msg;
            diag_nml_msg.module_info = module_info;
            //DiagNMLmsg.set_debug_on(debug_on);
            diag_nml_msg.cmd_stream = cmd_stream;
            diag_nml_msg.stat_stream = stat_stream;
            diag_nml_msg.update(fc_base);
            return 0;
        } catch (Exception e) {
            e.printStackTrace();
        }
        my_failed_count++;
        return -1;
    }
}
