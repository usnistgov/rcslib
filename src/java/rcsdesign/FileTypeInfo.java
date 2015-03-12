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

package rcsdesign;

import java.io.*;

/*
 *
 * FileTypeInfo
 *
 */
class FileTypeInfo
{
  public String name;
  public File file;
  public int type;
  boolean generated = false;
  boolean in_use = false;
  String module_name = null;
  String loop_name  = null;
  String server_name = null;
  boolean up_to_date = false;
  String list_name;
  String aux = null;

  static public final int NML_MODULE_MESSAGE_HEADER = 1;
  static public final int NML_AUX_MESSAGE_HEADER = 2;
  static public final int NML_MODULE_HEADER = 3;
  static public final int NML_MODULE_CPP = 4;
  static public final int NML_CONFIGURATION = 5;
  static public final int MODULE_MAKEFILE = 6;
  static public final int INTF_MAKEFILE = 7;
  static public final int INCLUDE_MAKEFILE = 8;
  static public final int MAIN_MAKEFILE = 9;
  static public final int SCRIPT = 10;
  static public final int MODULE_CONFIGURATION = 11;
  static public final int TOP_MAKEFILE = 12;
  static public final int MAIN_LOOP_CPP = 13;
  static public final int SERVER_CPP = 14;
  static public final int UTIL_MAKEFILE = 15;
  static public final int NML_MODULE_MESSAGE_CPP = 16;
  static public final int NML_MODULE_MESSAGE_CODEGEN_SCRIPT = 17;
  static public final int NML_AUX_MESSAGE_CPP = 18;
  static public final int NML_AUX_MESSAGE_CODEGEN_SCRIPT = 19;
  static public final int NML_MODULE_DESIGN_LOG = 20;
  static public final int MAIN_DESIGN_LOG = 21;
  static public final int EC_SCRIPT = 22;
  static public final int NML_MISC_MESSAGE_CODEGEN_SCRIPT = 23;
  static public final int SINGLE_DIR_GNUMAKEFILE = 24;

  public String toString()
  {
    return " FileTypeInfo: { file="+file+", name="+name+", type="+type+", module_name="+module_name+", loop_name="+loop_name+
      ", up_to_date="+up_to_date+", list_name="+list_name+", aux="+aux+" } ";
  }

  static public String typeToString(int type)
  {
    switch(type)
      {
      case MAIN_DESIGN_LOG:
        return "Main Design Log";

      case NML_MODULE_DESIGN_LOG:
        return "NML Module Design Log";

      case NML_MODULE_CPP:
        return "NML Module C++ file.";

      case NML_MODULE_HEADER:
        return "NML Module C++ Header file.";

      case NML_MODULE_MESSAGE_HEADER:
        return "NML Message C++ Header file.";

      case NML_MODULE_MESSAGE_CPP:
        return "NML Message C++ Header file.";

      case NML_MODULE_MESSAGE_CODEGEN_SCRIPT:
        return "NML Message CodeGen script.";

      case NML_AUX_MESSAGE_HEADER:
        return "NML Message Auxilliary C++ Header file.";

      case NML_AUX_MESSAGE_CPP:
        return "NML Message Auxilliary  C++ file.";

      case NML_AUX_MESSAGE_CODEGEN_SCRIPT:
        return "NML Message Auxilliary CodeGen script.";

      case MODULE_MAKEFILE:
        return "Makefile for a  module.";

      case INTF_MAKEFILE:
        return "Makefile for the interfaces directory.";

      case INCLUDE_MAKEFILE:
        return "Makefile definitions for inclusion.";

      case TOP_MAKEFILE:
        return "Makefile for top-level directory.";

      case MODULE_CONFIGURATION:
         return "Module Hierarchy Configuration File.";

      case NML_CONFIGURATION:
         return "NML Communications Configuration File.";

      case SCRIPT:
        return "Script for running the application.";

      case MAIN_MAKEFILE:
        return "Makefile for main directory.";

      case UTIL_MAKEFILE:
        return "Makefile for util directory.";

      case SERVER_CPP:
        return "NML Server C++ file";

      case MAIN_LOOP_CPP:
        return "Main Loop C++ file";

      case EC_SCRIPT:
        return "ec script";

      default:
        return "";
      }
  }
}
