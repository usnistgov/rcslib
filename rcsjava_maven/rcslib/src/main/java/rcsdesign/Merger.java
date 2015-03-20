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

import java.util.*;
import java.io.*;
import rcs.utils.*;


class BackupLineData
{
  public String orig_line;
  public String line_for_compare;
  public String subsection;
  public int subsection_number = 0;
  public int brace_count;
  public int paren_count;
}

/*
 *
 * Merger
 *
 */
class Merger
{
  Vector backup_lines = null;

  PrintWriter pos = null;

  Vector functions_defined = null;
  Vector classes_defined = null;

  public static boolean debug_on = false;
  public double template_version = 0.0;
  public boolean template_version_found = false;
  public boolean nml_mode = false;
  int last_line_matched = -1;
  int input_line = 0;

  boolean disabling_merge = false;
  boolean merge_disabled = false;
  boolean enabling_merge = false;

  boolean cpp_mode = false;

  int input_brace_count = 0;
  int input_paren_count = 0;

  boolean blank_line_needed = false;
  int output_brace_count = 0;
  int output_paren_count = 0;
  boolean Makefile_mode = false;
  String input_subsection = null;
  public int input_subsection_number = 0;

  File mergerLogFile = new File("merger.log");
  FileOutputStream logFOS;
  PrintWriter logPS;

  public Merger()
  {
    if(debug_on)
      {
        System.out.println(" new Merger()");
        Thread.dumpStack();
        try
          {
            if(mergerLogFile.exists())
              {
                mergerLogFile.delete();
              }
            logFOS = new FileOutputStream(mergerLogFile);
            logPS = new PrintWriter(logFOS);
          }
        catch(Exception e)
          {
            e.printStackTrace();
          }
      }
  }

  String MergeNMLBufferLines(String new_line, String old_line)
  {
    String buffer_name = "";
    String bufname_pad = "";
    String buftype = "SHMEM";
    String buffer_host = "localhost";
    String bufhost_pad = "";
    String bufsize_string = "0x2000";
    String rpc_string = "0x20002000";
    String buffer_number_string = "1";
    String max_procs_string = "16";
    String shmemkey_string = "1";
    String neut_string = "0";
    String tcp_string = "TCP=2000";
    String nml_attach_string = "";

    input_line = 0;
    int token_count = 0;
    StringTokenizer new_line_tokenizer = new StringTokenizer(new_line, " \t");
    while(new_line_tokenizer.hasMoreTokens())
      {
        String token = new_line_tokenizer.nextToken();
        switch(token_count)
          {
          case 0:               // B
            break;

          case 1:               // BufferName
            buffer_name = token;
            for(int i = buffer_name.length(); i < 16; i++)
              {
                bufname_pad += " ";
              }
            break;

          case 2:               // BufferType
            break;

          case 3:               // BufferHost
            buffer_host =token;
            for(int i = buffer_host.length(); i < 16; i++)
              {
                bufhost_pad += " ";
              }
            break;

          case 4:               // bufsize
            break;

          case 5:               // nuet?
            break;


          case 6:               // rpc_number
            rpc_string = token;
            break;

          case 7:               // buffer number
            buffer_number_string = token;
            break;

          case 8:               // max_procs
            max_procs_string = token;
            break;

          case 9:               // key
            shmemkey_string = token;
            break;

          case 10:              // TCP=?
            tcp_string = token;
            break;

          default:
            if(nml_attach_string.indexOf(token) < 0 && !token.equals("disp"))
              {
                nml_attach_string += " "+token;
              }
            break;
          }
        token_count++;
      }

    token_count = 0;
    StringTokenizer old_line_tokenizer = new StringTokenizer(old_line, " \t");
    while(old_line_tokenizer.hasMoreTokens())
      {
        String token = old_line_tokenizer.nextToken();
        switch(token_count)
          {
          case 0:               // B
            break;

          case 1:               // BufferName
            break;

          case 2:               // BufferType
            buftype = token;
            break;

          case 3:               // BufferHost
            break;

          case 4:               // bufsize
            bufsize_string = token;
            break;

          case 5:               // nuet?
            neut_string = token;
            break;


          case 6:               // rpc_number
            break;

          case 7:               // buffer number
            break;

          case 8:               // max_procs
            break;

          case 9:               // key
            break;

          case 10:              // TCP=?
            break;

          default:
            if(nml_attach_string.indexOf(token) < 0)
              {
                nml_attach_string += " "+token;
              }
            break;
          }
        token_count++;
      }
    return "B "+buffer_name+bufname_pad+" \t"+buftype+" \t"+buffer_host+bufhost_pad+" \t"+bufsize_string+"\t"+neut_string+"    \t"+rpc_string+" \t"+buffer_number_string+"        \t"+max_procs_string+"     \t"+shmemkey_string+" "+tcp_string+" "+nml_attach_string;
  }

  String NMLMerge(String new_line, String old_line)
  {
    if(new_line.startsWith("B"))
      {
        return MergeNMLBufferLines(new_line,old_line);
      }
    return new_line;
  }

  String MakeLineMerge(String new_line, String old_line)
  {
    if(new_line.startsWith("#"))
      {
        if(debug_on)
          {
            System.out.println("Merger.MakeLineMerge("+new_line+", "+old_line+") returning new_line ="+new_line);
          }
        return new_line;
      }
    if(debug_on)
      {
        System.out.println("Merger.MakeLineMerge("+new_line+", "+old_line+") returning old_line ="+old_line);
      }
    return old_line;
  }
  boolean  MakeLineCompare(String new_line, String old_line)
  {
    if(new_line.startsWith("#") || new_line.indexOf('=') >= 0 || new_line.indexOf(':') >= 0 ||
       new_line.startsWith("if") || new_line.startsWith("end"))
      {
        boolean retval = new_line.equals(old_line);
        if(debug_on)
          {
            System.out.println("Merger.MakeLineCompare("+new_line+", "+old_line+") returning "+retval);
          }
        return retval;
      }
    int tokens_compared = 0;
    boolean all_tokens_found = true;
    StringTokenizer new_line_tokenizer = new StringTokenizer(new_line," \t\r\n\b\\");
    while(new_line_tokenizer.hasMoreTokens())
      {
        String token = new_line_tokenizer.nextToken();
        if(debug_on)
          {
            System.out.println("token ="+token+", tokens_compared = "+tokens_compared);
          }
        if(old_line.indexOf(token) < 0)
          {
            all_tokens_found = false;
            break;
          }
        tokens_compared ++;
      }
    if(tokens_compared > 0 && all_tokens_found)
      {
        boolean retval = true;
        if(debug_on)
          {
            System.out.println("Merger.MakeLineCompare("+new_line+", "+old_line+") returning "+retval);
          }
        return retval;
      }
    boolean retval = new_line.equals(old_line);
    if(debug_on)
      {
        System.out.println("Merger.MakeLineCompare("+new_line+", "+old_line+") returning "+retval);
      }
    return retval;
  }

  public boolean FunctionFoundInExistingFile(String funcname)
  {
    if(debug_on)
      {
        System.out.println("Merger.FunctionFoundInExistingFile("+funcname+") called.");
      }
    if(!cpp_mode)
      {
        if(debug_on)
          {
            System.out.println("cpp_mode=false");
          }
        return false;
      }
    if(null == functions_defined)
      {
        if(debug_on)
          {
            System.out.println("functions_defined=null");
          }
        return false;
      }
    try
      {
        for(int i = 0; i < functions_defined.size(); i++)
          {
            String tempfuncname = (String) functions_defined.elementAt(i);
            if(debug_on)
              {
                System.out.println(" String tempfuncname = (String) functions_defined.elementAt("+i+"); ="+tempfuncname);
              }
            if(tempfuncname.equals(funcname))
              {
                return true;
              }
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return false;
  }


 public boolean ClassFoundInExistingFile(String funcname)
  {
    if(debug_on)
      {
        System.out.println("Merger.ClassFoundInExistingFile("+funcname+") called.");
      }
    if(!cpp_mode)
      {
        if(debug_on)
          {
            System.out.println("cpp_mode=false");
          }
        return false;
      }
    if(null == classes_defined)
      {
        if(debug_on)
          {
            System.out.println("classes_defined=null");
          }
        return false;
      }
    try
      {
        for(int i = 0; i < classes_defined.size(); i++)
          {
            String tempfuncname = (String) classes_defined.elementAt(i);
            if(debug_on)
              {
                System.out.println(" String tempfuncname = (String) classs_defined.elementAt("+i+"); ="+tempfuncname);
              }
            if(tempfuncname.equals(funcname))
              {
                return true;
              }
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return false;
  }


  boolean  NMLLineCompare(String linea, String lineb)
  {
    boolean retval = false;
    if(debug_on)
      {
        System.out.println("Merger.NMLLineCompare:");
        System.out.println("\t linea = \""+linea+"\";");
        System.out.println("\t lineb = \""+lineb+"\";");
      }
    if(linea.startsWith("#"))
      {
        int paren_index = linea.indexOf('(');
        if(paren_index > 0 && paren_index == lineb.indexOf('('))
        {
          linea = linea.substring(0,paren_index);
          lineb = lineb.substring(0,paren_index);
          if(debug_on)
            {
              System.out.println("Merger.NMLLineCompare:");
              System.out.println("paren_index="+paren_index);
              System.out.println("\t linea = \""+linea+"\";");
              System.out.println("\t lineb = \""+lineb+"\";");
            }
        }

        retval = linea.equals(lineb);
        if(debug_on)
          {
            System.out.println("Merger.NMLLineCompare() returns "+retval);
          }
        return retval;
      }
    int max_token_count = 2;
    if(linea.startsWith("P"))
      {
        max_token_count = 3;
      }
    StringTokenizer tokenizera = new StringTokenizer(linea, " \t");
    StringTokenizer tokenizerb = new StringTokenizer(lineb, " \t");
    int token_count = 0;
    while(tokenizera.hasMoreTokens() && tokenizerb.hasMoreTokens() && token_count < max_token_count)
      {
        token_count++;
        String tokena = tokenizera.nextToken();
        String tokenb = tokenizerb.nextToken();
        if(!tokena.equals(tokenb))
          {
            retval = false;
            if(debug_on)
              {
                System.out.println("Merger.NMLLineCompare() returns "+retval);
              }
            return retval;
          }
      }
    retval = true;
    if(debug_on)
      {
        System.out.println("Merger.NMLLineCompare() returns "+retval);
      }
    return retval;
  }

  public void Finish()
  {
    try
      {
        if(blank_line_needed)
          {
            FinalWriteLine("");
          }
        if(!merge_disabled && null != backup_lines && !Makefile_mode)
          {
            for(int i = last_line_matched+1; i < backup_lines.size(); i++)
              {
                BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);
                if(debug_on && null != logPS)
                  {
                    logPS.println("<"+i+">"+ bld.orig_line);
                  }
                FinalWriteLine(bld.orig_line);
              }
          }
        if(cpp_mode)
          {
            if(output_brace_count != 0)
              {
                pos.println("// There are "+output_brace_count+" more open braces than close braces.");
              }
            if(output_paren_count != 0)
              {
                pos.println("// There are "+output_paren_count+" more open braces than close braces.");
              }
            input_brace_count = 0;
            input_paren_count = 0;
            output_brace_count = 0;
            output_paren_count = 0;
          }
        if(debug_on)
          {
            logPS.close();
            logPS = null;
            logFOS.close();
            logFOS = null;
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }
  public void SetPrintWriter(PrintWriter ps)
  {
    pos = ps;
    input_brace_count = 0;
    input_paren_count = 0;
    output_brace_count = 0;
    output_paren_count = 0;
  }

    @SuppressWarnings("unchecked")
  public void GetBackupData(File backup_file)
  {
    try
      {
        inside_c_comment = false;
        template_version_found = false;
        String backup_subsection = null;
        String last_backup_subsection = null;
        int backup_subsection_number = 0;
        if(debug_on)
          {
            System.out.println("Merger.GetBackupData("+ backup_file+")");
          }
        input_brace_count = 0;
        input_paren_count = 0;
        output_brace_count = 0;
        output_paren_count = 0;
        int brace_count = 0;
        int paren_count = 0;
        merge_disabled = false;
        if(cpp_mode)
          {
            functions_defined = new Vector();
            classes_defined = new Vector();
          }
        if(!backup_file.exists())
          {
            System.err.println("Merger.GetBackupFile("+backup_file+"): File does not exist.");
            return;
          }
        if(!backup_file.canRead())
          {
            System.err.println("Merger.GetBackupFile("+backup_file+"): File can NOT be read.");
            return;
          }
        if(backup_file.length() < 1)
          {
            System.err.println("Merger.GetBackupFile("+backup_file+"): File has "+backup_file.length()+" length.");
            return;
          }
        if(debug_on)
          {
            System.out.println("Merger.SetBackup("+backup_file+")");
          }
        if(null != backup_file)
          {
            if(debug_on)
              {
                System.out.println("Creating FileInputStream");
              }
            URL_and_FileLoader loader = new URL_and_FileLoader(backup_file.toString());
            String str = loader.readLine();
            if(debug_on)
              {
                System.out.println(str);
              }
            backup_lines = new Vector();
            String funcname = null;
            String classname = null;
            inside_c_comment = false;
            template_version = 0.0;
            while(str != null)
              {
                if(debug_on)
                  {
                    System.out.println(backup_lines.size()+":"+str);
                  }
                if(str.indexOf("RCS-Design-MERGE-ENABLE") >= 0)
                  {
                    merge_disabled = false;
                  }
                if(!merge_disabled)
                  {
                    if(cpp_mode)
                      {
                        String sout = str;
                        if(paren_count == 0 && brace_count == 0 && null == funcname && null == classname)
                          {
                            backup_subsection = null;
                          }
                        int cpp_comment_index = sout.indexOf("//");
                        if(cpp_comment_index >= 0)
                          {
                            sout = sout.substring(0,cpp_comment_index);
                            if(debug_on)
                              {
                                System.out.println("cpp_comment_index="+cpp_comment_index+", sout="+sout+", str="+str);
                              }
                          }
                        int cstart_comment_index = sout.indexOf("/*");
                        int cend_comment_index = sout.indexOf("*/");
                        while(cstart_comment_index >= 0 && cend_comment_index > cstart_comment_index)
                        {
                          if(debug_on)
                            {
                              System.out.println("cstart_comment_index="+cstart_comment_index+", cend_comment_index="+cend_comment_index+", sout="+sout+", str="+str);
                            }
                          sout = sout.substring(0,cstart_comment_index)+ sout.substring(cend_comment_index+2);
                          cstart_comment_index = sout.indexOf("/*");
                          cend_comment_index = sout.indexOf("*/");
                        }
                        if(cend_comment_index >= 0 && inside_c_comment)
                          {
                            sout = sout.substring(cend_comment_index+2);
                            inside_c_comment = false;
                          }
                        if(cstart_comment_index >= 0 && !inside_c_comment)
                          {
                            sout = sout.substring(0,cstart_comment_index);
                            inside_c_comment = true;
                          }
                        if(!inside_c_comment)
                          {
                            int paren_index = sout.indexOf('(');
                            if(brace_count == 0 && (sout.indexOf("class") >= 0 || sout.indexOf("struct") >= 0) && paren_index < 0)
                              {
                                if(debug_on)
                                  {
                                    System.out.println("brace_count = "+brace_count+", sout="+sout);
                                  }
                                int classname_begin = 0;
                                if(classname_begin < sout.indexOf("class")+5)
                                  {
                                    classname_begin = sout.indexOf("class")+5;
                                  }
                                if(classname_begin < sout.indexOf("struct")+6)
                                  {
                                    classname_begin = sout.indexOf("struct")+6;
                                  }
                                while(classname_begin < sout.length())
                                  {
                                    char c = sout.charAt(classname_begin);
                                    if(Character.isLetterOrDigit(c) || c == '_')
                                      {
                                        break;
                                      }
                                    classname_begin++;
                                  }
                                int classname_end = sout.length()-1;
                                if(sout.indexOf(":") > 0)
                                  {
                                    classname_end = sout.indexOf(":") -1;
                                  }
                                if(sout.indexOf("public") > 0 && classname_end > sout.indexOf("public"))
                                  {
                                    classname_end = sout.indexOf("public") -1;
                                  }
                                if(sout.indexOf("private") > 0 && classname_end > sout.indexOf("private"))
                                  {
                                    classname_end = sout.indexOf("private") -1;
                                  }
                                if(sout.indexOf("protected") > 0 && classname_end > sout.indexOf("protected"))
                                  {
                                    classname_end = sout.indexOf("protected") -1;
                                  }
                                if(sout.indexOf("{") > 0 && classname_end > sout.indexOf("{"))
                                  {
                                    classname_end = sout.indexOf("protected") -1;
                                  }
                                while(classname_end > 0)
                                  {
                                    char c = sout.charAt(classname_end);
                                    if(Character.isLetterOrDigit(c) || c == '_')
                                      {
                                        classname_end++;
                                        break;
                                      }
                                    classname_end--;
                                  }
                                if(debug_on)
                                  {
                                    System.out.println("classname_begin="+classname_begin+", classname_end="+classname_end);
                                  }
                                if(classname_end > classname_begin)
                                  {
                                    classname = sout.substring(classname_begin, classname_end);
                                    if(null != classname && null == backup_subsection)
                                      {
                                        backup_subsection = classname;
                                      }
                                    if(debug_on)
                                      {
                                        System.out.println("classname="+classname);
                                      }
                                  }
                              }
                            if(brace_count == 0 && paren_index > 1)
                              {
                                if(debug_on)
                                  {
                                    System.out.println("brace_count="+brace_count+", paren_index="+paren_index+", sout="+sout);
                                  }
                                int funcname_end = paren_index;
                                while(funcname_end > 0)
                                  {
                                    char c = sout.charAt(funcname_end);
                                    if(Character.isLetterOrDigit(c) || c == '_')
                                      {
                                        break;
                                      }
                                    funcname_end--;
                                  }
                                int funcname_begin = funcname_end;
                                while(funcname_begin > 0)
                                  {
                                    char c = sout.charAt(funcname_begin);
                                    if(!Character.isLetterOrDigit(c) && c != '_' && c != ':')
                                      {
                                        funcname_begin++;
                                        break;
                                      }
                                    funcname_begin--;
                                  }

                                if(funcname_begin < funcname_end)
                                  {
                                    funcname = sout.substring(funcname_begin, funcname_end+1);
                                    if(funcname.equals("Parameter"))
                                      {
                                        funcname = null;
                                      }
                                    if(null != funcname && null == backup_subsection)
                                      {
                                        backup_subsection = funcname;
                                      }
                                    if(debug_on)
                                      {
                                        System.out.println("funcname = "+funcname);
                                      }
                                  }
                              }
                            int brace_index = sout.indexOf('{');
                            while(brace_index >= 0)
                              {
                                brace_count++;
                                brace_index = sout.indexOf('{',brace_index+1);
                              }
                            brace_index = sout.indexOf('}');
                            while(brace_index >= 0 )
                              {
                                brace_count--;
                                brace_index = sout.indexOf('}',brace_index+1);
                              }
                            if(funcname != null && brace_count > 0)
                              {
                                functions_defined.addElement(funcname);
                                backup_subsection = funcname;
                                backup_subsection_number++;
                                funcname = null;
                              }
                            if(classname != null && brace_count > 0)
                              {
                                classes_defined.addElement(classname);
                                backup_subsection = classname;
                                backup_subsection_number++;
                                classname=null;
                              }
                            while(paren_index >= 0 )
                              {
                                paren_count++;
                                paren_index = sout.indexOf('(',paren_index+1);
                              }
                            paren_index = sout.indexOf(')');
                            while(paren_index >= 0 )
                              {
                                paren_count--;
                                paren_index = sout.indexOf(')',paren_index+1);
                              }
                          }
                      }
                    if(Makefile_mode)
                      {
                        int eq_index = str.indexOf('=');
                        int colon_index = str.indexOf(':');
                        if(eq_index > 0)
                          {
                            if(debug_on)
                              {
                                System.out.println("eq_index  = "+eq_index);
                              }
                            backup_subsection = RemoveExtraWhiteSpace(str.substring(0,eq_index));
                             backup_subsection_number++;
                          }
                        else if(colon_index > 0)
                          {
                            if(debug_on)
                              {
                                System.out.println("colon_index  = "+colon_index);
                              }
                            backup_subsection = RemoveExtraWhiteSpace(str.substring(0,colon_index));
                             backup_subsection_number++;
                          }
                        if(str.length() < 1)
                          {
                            backup_subsection = null;
                          }
                        else if(str.startsWith("#") || str.startsWith("if") || str.startsWith("else") ||
                           str.startsWith("endif") || str.startsWith("include") || str.startsWith("!"))
                          {
                            backup_subsection = null;
                          }
                        if(null != backup_subsection)
                          {
                            if(backup_subsection.indexOf("PHONY") >= 0 || backup_subsection.startsWith("."))
                              {
                                backup_subsection = null;
                              }
                            else if(backup_subsection.length() < 1)
                              {
                                backup_subsection = null;
                              }
                          }
                        try
                          {
                            if(debug_on && !backup_subsection.equals(last_backup_subsection))
                              {
                                System.out.println("backup_subsection="+backup_subsection);
                                last_backup_subsection = backup_subsection;
                              }
                          }
                        catch(Exception e)
                          {
                          }
                      }
                    BackupLineData bld = new BackupLineData();
                    bld.orig_line = str;
                    bld.line_for_compare = RemoveExtraWhiteSpace(str);
                    int template_version_index = bld.line_for_compare.toUpperCase().indexOf("TEMPLATE VERSION");
                    if(Makefile_mode || cpp_mode)
                      {
                        bld.subsection = backup_subsection;
                        bld.subsection_number = backup_subsection_number;
                      }
                    if(template_version_index >= 0)
                      {
                        int template_version_start_index = template_version_index+new String("TEMPLATE VERSION").length();
                        if(template_version_start_index < bld.line_for_compare.length())
                          {
                            while(true)
                              {
                                if(template_version_start_index >=  bld.line_for_compare.length())
                                  {
                                    break;
                                  }
                                char c =  bld.line_for_compare.charAt(template_version_start_index);
                                if(Character.isDigit(c) ||
                                   c == '.'  || c == '+' || c == '-')
                                  {
                                    break;
                                  }
                                template_version_start_index++;
                              }
                            int template_version_stop_index = template_version_start_index;
                            boolean period_occured = false;
                           while(true)
                              {
                                if(template_version_stop_index >=  bld.line_for_compare.length())
                                  {
                                    break;
                                  }
                                char c =  bld.line_for_compare.charAt(template_version_stop_index);
                                if(!Character.isDigit(c) &&
                                   c != '.'  && c != '+' && c != '-')
                                  {
                                    break;
                                  }
                                if(c == '.')
                                  {
                                    if(period_occured)
                                      {
                                        break;
                                      }
                                    period_occured = true;
                                  }
                                template_version_stop_index++;
                              }
                            if(template_version_stop_index > template_version_start_index)
                              {
                                String vers_string = bld.line_for_compare.substring(template_version_start_index,template_version_stop_index);
                                template_version = Double.valueOf(vers_string).doubleValue();
                                template_version_found = true;
                                if(debug_on)
                                  {
                                    System.out.println("template_version="+template_version);
                                  }
                              }
                          }
                      }
                    bld.brace_count = brace_count;
                    bld.paren_count = paren_count;
                    backup_lines.addElement(bld);

                  }
                if(str.indexOf("RCS-Design-MERGE-DISABLE") >= 0)                  {
                    merge_disabled = true;
                  }
                str = loader.readLine();
              }
            loader.close();
            merge_disabled = false;
            brace_count = 0;
            paren_count = 0;
          }
        if(debug_on && null != backup_lines)
          {
            System.out.println("backup_lines.size() = "+backup_lines.size());
          }
        inside_c_comment = false;
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

  String RemoveExtraWhiteSpace(String str)
  {
    String newString = "";
    String separators = "(){}[]=+-*/.;,\"";
    try
      {
        StringTokenizer tokenizer = new StringTokenizer(str," \t\r\n\b");
        while(tokenizer.hasMoreTokens())
          {
            String token = tokenizer.nextToken();
            if(newString.length() > 0 && token.length() > 0)
              {
                char lastChar = newString.charAt(newString.length()-1);
                char nextChar = token.charAt(0);
                if(separators.indexOf(lastChar) < 0 && separators.indexOf(nextChar) < 0)
                  {
                    newString += " ";
                  }
              }
            newString += token;
          }
        if(debug_on)
          {
            // System.out.println("Merger.RemoveExtraWhiteSpace("+str+") returning "+newString);
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return newString;
  }


  public void DisableMerge(String msg_head)
  {
    try
      {
        input_line++;
        int matching_line = FindDisableLine();
        if(last_line_matched >= 0 && matching_line > last_line_matched)
          {
            boolean blank_line_from_backup_needed = false;
            for(int i = last_line_matched+1; i < matching_line; i++)
              {
                BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);
                if(bld.line_for_compare.length() < 1)
                  {
                    blank_line_from_backup_needed = true;
                    continue;
                  }
                if(blank_line_from_backup_needed)
                  {
                    if(debug_on && null != logPS)
                      {
                        logPS.println("");
                      }
                    FinalWriteLine("");
                    if(debug_on)
                      {
                        System.out.println("blank_line_from_backup");
                      }
                    blank_line_from_backup_needed = false;
                  }
                if(debug_on)
                  {
                    System.out.println("-----\t"+bld.orig_line);
                  }
                if(debug_on && null != logPS)
                  {
                    logPS.println("<"+i+">"+ bld.orig_line);
                  }
                FinalWriteLine(bld.orig_line);
              }
            last_line_matched = matching_line;
            if(debug_on)
              {
                System.out.println("last_line_matched = "+last_line_matched);
              }
          }
        if(null != msg_head)
          {
            if(debug_on && null != logPS)
              {
                logPS.println("("+input_line+")"+ msg_head+" RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.");
                logPS.println("");
              }
            FinalWriteLine(msg_head+" RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.");
            FinalWriteLine("");
          }
        else
          {
            if(debug_on && null != logPS)
              {
                logPS.println("("+input_line+") RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.");
                logPS.println("");
              }
            FinalWriteLine(" RCS-Design-MERGE-DISABLE Edits to the following area will NOT be preserved by the RCS-Design tool.");
            FinalWriteLine("");
          }
        merge_disabled = true;
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

  public void EnableMerge(String msg_head)
  {
    try
      {
        input_line++;
        int matching_line = FindEnableLine();
        if(matching_line > 0)
          {
            if(debug_on && matching_line > last_line_matched)
              {
                for(int i = last_line_matched; i < matching_line; i++)
                  {
                    BackupLineData bld =  (BackupLineData)  backup_lines.elementAt(i);
                    System.out.println("ddd "+i+" - "+bld.orig_line);
                  }
              }
            last_line_matched = matching_line;
            if(debug_on)
              {
                System.out.println("last_line_matched = "+last_line_matched);
              }
          }
        if(null != msg_head)
          {
            if(debug_on && null != logPS)
              {
                logPS.println("("+input_line+")"+ msg_head+" RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.");
                logPS.println("");
              }
            FinalWriteLine(msg_head+" RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.");
            FinalWriteLine("");
          }
        else
          {
            if(debug_on && null != logPS)
              {
                logPS.println("("+input_line+") RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.");
                logPS.println("");
              }
            FinalWriteLine(" RCS-Design-MERGE-ENABLE Edits after this line will be preserved by the RCS-Design tool.");
            FinalWriteLine("");
          }
        merge_disabled = false;
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

  int FindLine(String str)
  {
    try
      {
        if(null == backup_lines)
          {
            return -1;
          }

        if(Makefile_mode)
          {
            int eq_index = str.indexOf('=');
            int colon_index = str.indexOf(':');
            if(eq_index > 0)
              {
                input_subsection = RemoveExtraWhiteSpace(str.substring(0,eq_index));
                input_subsection_number++;
              }
            else if(colon_index > 0)
              {
                input_subsection = RemoveExtraWhiteSpace(str.substring(0,colon_index));
                input_subsection_number++;
              }
            if(str.length() < 1)
              {
                input_subsection = null;
              }
            else if(str.startsWith("#") || str.startsWith("if") || str.startsWith("else") ||
                    str.startsWith("endif") || str.startsWith("include") || str.startsWith("!"))
              {
                input_subsection = null;
              }
            if(null != input_subsection)
              {
                if(input_subsection.length() < 1)
                  {
                    input_subsection = null;
                  }
                else if(input_subsection.indexOf("PHONY") >= 0 ||
                   input_subsection.startsWith("."))
                  {
                    input_subsection = null;
                  }
              }
          }
        if((Makefile_mode || cpp_mode) && debug_on)
          {
            System.out.println("input_subsection="+input_subsection);
            System.out.println("input_subsection_number="+input_subsection_number);
          }


        String str_for_compare = RemoveExtraWhiteSpace(str);
        if(debug_on)
          {
            System.out.println("Merger.FindLine : str_for_compare = "+str_for_compare);
            if(cpp_mode)
              {
                System.out.println("Merger.FindLine : input_brace_count = "+input_brace_count);
                System.out.println("Merger.FindLine : input_paren_count = "+input_paren_count);
              }
          }

        for(int i = last_line_matched+1; i < backup_lines.size(); i++)
          {
            BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);

            if(bld.line_for_compare.length() < 1)
              {
                continue;
              }
            if(debug_on)
              {
                System.out.println("Merger.FindLine : i = "+i);
                System.out.println("Merger.FindLine : bld.line_for_compare = "+bld.line_for_compare);
                if(cpp_mode)
                  {
                    System.out.println("Merger.FindLine : bld.brace_count = "+bld.brace_count);
                    System.out.println("Merger.FindLine : bld.paren_count = "+bld.paren_count);
                  }
                if(Makefile_mode || cpp_mode)
                  {
                    System.out.println("Merger.FindLine : bld.subsection = "+bld.subsection);
                    System.out.println("Merger.FindLine : bld.subsection_number = "+bld.subsection_number);
                  }
              }
            if(cpp_mode &&
               (bld.brace_count != input_brace_count
                || bld.paren_count != input_paren_count))
              {
                continue;
              }
            if(nml_mode)
              {
                if(NMLLineCompare(bld.line_for_compare,str_for_compare))
                  {
                    return i;
                  }
                continue;
              }
            if((Makefile_mode || cpp_mode) && input_subsection != null)
              {
                if(!input_subsection.equals(bld.subsection))
                  {
                    if(bld.subsection_number > ((int) (input_subsection_number*1.1)) + 5)
                      {
                        if(debug_on)
                          {
                            System.out.println("FindLine: Returning -1 : input_subsection = "+input_subsection+", bld.subsection="+bld.subsection+", input_subsection_number ="+input_subsection_number+", bld.subsection_number="+bld.subsection_number);
                            System.out.println("FindLine: str_for_compare="+str_for_compare+", bld.line_for_compare="+bld.line_for_compare);
                          }
                        return -1;
                      }
                    else
                      {
                        continue;
                      }
                  }
              }
            if((Makefile_mode || cpp_mode) && bld.subsection != null)
              {
                if(!bld.subsection.equals(input_subsection))
                  {
                    if(bld.subsection_number > ((int) (input_subsection_number*1.1)) + 5)
                      {
                        if(debug_on)
                          {
                            System.out.println("FindLine: Returning -1 : input_subsection = "+input_subsection+", bld.subsection="+bld.subsection+", input_subsection_number ="+input_subsection_number+", bld.subsection_number="+bld.subsection_number);
                            System.out.println("FindLine: str_for_compare="+str_for_compare+", bld.line_for_compare="+bld.line_for_compare);
                          }
                        return -1;
                      }
                    else
                      {
                        continue;
                      }
                  }
              }
            if(Makefile_mode)
              {
                if(MakeLineCompare(str_for_compare, bld.line_for_compare))
                  {
                    return i;
                  }
                else
                  {
                    continue;
                  }
              }
            if(bld.line_for_compare.equals(str_for_compare))
              {
                return i;
              }

          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return -1;
  }

  public int DeleteLine(String str)
  {
    boolean orig_cpp_mode = cpp_mode;
    cpp_mode = false;
    if(debug_on)
      {
        System.out.println("Merger.Deleting "+str);
      }
    int line_num  = FindLine(str);
    cpp_mode = orig_cpp_mode;
    if(line_num > 0 && line_num < backup_lines.size())
      {
        if(debug_on)
          {
            System.out.println("Merger.Deleting line number "+line_num);
          }
        backup_lines.removeElementAt(line_num);
        return 0;
      }
    return -1;
  }

  int FindEnableLine()
  {
    try
      {
        if(null == backup_lines)
          {
            return -1;
          }
        for(int i = last_line_matched+1; i < backup_lines.size(); i++)
          {
            BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);
            if(bld.line_for_compare.length() < 1)
              {
                continue;
              }
            if(bld.line_for_compare.indexOf("RCS-Design-MERGE-ENABLE") >= 0)
              {
                return i;
              }
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return -1;
  }

  int FindDisableLine()
  {
    try
      {
        if(null == backup_lines)
          {
            return -1;
          }
        for(int i = last_line_matched+1; i < backup_lines.size(); i++)
          {
            BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);
            if(bld.line_for_compare.length() < 1)
              {
                continue;
              }
            if(bld.line_for_compare.indexOf("RCS-Design-MERGE-DISABLE") >= 0)
              {
                return i;
              }
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return -1;
  }

  String TabForCpp(String sin)
  {
    String sout = sin;
    try
      {
        int prev_output_brace_count = output_brace_count;
        int prev_output_paren_count = output_paren_count;
        if(null == sin)
          {
            return null;
          }
        if(sin.length() < 1)
          {
            return "";
          }
        int double_slash_index = sout.indexOf("//");
        int brace_index = sout.indexOf('{');
        while(brace_index >= 0)
          {
            output_brace_count++;
            brace_index = sout.indexOf('{',brace_index+1);
          }
        brace_index = sout.indexOf('}');
        while(brace_index >= 0 )
          {
            output_brace_count--;
            brace_index = sout.indexOf('}',brace_index+1);
          }
        int paren_index = sout.indexOf('(');
        while(paren_index >= 0 )
          {
            output_paren_count++;
            paren_index = sout.indexOf('(',paren_index+1);
          }
        paren_index = sout.indexOf(')');
        while(paren_index >= 0 )
          {
            output_paren_count--;
            paren_index = sout.indexOf(')',paren_index+1);
          }
        while(sout.length() > 1 )
          {
            if(sout.charAt(0) != ' ' && sout.charAt(0) != '\t')
              {
                break;
              }
            sout = sout.substring(1);
          }
        if(sout.length() < 1)
          {
            return "";
          }
        String tab_string = "";
        int start_i = 0;
        if(sout.charAt(sout.length()-1) == ':')
          {
            start_i = 1;
          }
        for(int i = start_i; i < output_brace_count && i < prev_output_brace_count; i++)
          {
            tab_string += "\t";
          }
        if(prev_output_paren_count > 0)
          {
            tab_string += "\t";
          }
        sout = tab_string + sout;
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
    return sout;
  }

  boolean inside_c_comment = false;
  String input_classname = null;
  String input_funcname = null;

  void SetInputBraceCount(String sin)
  {
    String sout = sin;

    try
      {
        int prev_input_brace_count = input_brace_count;
        if(null == sin)
          {
            return;
          }
        if(sin.length() < 1)
          {
            return;
          }

        int cpp_comment_index = sout.indexOf("//");
        if(cpp_comment_index >= 0)
          {
            sout = sout.substring(0,cpp_comment_index);
            if(debug_on)
              {
                System.out.println("cpp_comment_index="+cpp_comment_index+", sout="+sout+", sin="+sin);
              }
          }
        int cstart_comment_index = sout.indexOf("/*");
        int cend_comment_index = sout.indexOf("*/");
        while(cstart_comment_index >= 0 && cend_comment_index > cstart_comment_index)
          {
            if(debug_on)
              {
                System.out.println("cstart_comment_index="+cstart_comment_index+", cend_comment_index="+cend_comment_index+", sout="+sout+", sin="+sin);
              }
            sout = sout.substring(0,cstart_comment_index)+ sout.substring(cend_comment_index+2);
            cstart_comment_index = sout.indexOf("/*");
            cend_comment_index = sout.indexOf("*/");
          }
        if(cend_comment_index >= 0 && inside_c_comment)
          {
            sout = sout.substring(cend_comment_index+2);
            inside_c_comment = false;
          }
        if(cstart_comment_index >= 0 && !inside_c_comment)
          {
            sout = sout.substring(0,cstart_comment_index);
            inside_c_comment = true;
          }
        if(input_paren_count == 0 && input_brace_count == 0 && null == input_funcname && null == input_classname)
          {
            input_subsection = null;
          }
        if(!inside_c_comment)
          {
            int paren_index = sout.indexOf('(');
            if(input_brace_count == 0 && (sout.indexOf("class") >= 0 || sout.indexOf("struct") >= 0) && paren_index < 0)
              {
                if(debug_on)
                  {
                    System.out.println("input_brace_count = "+input_brace_count+", sout="+sout);
                  }
                int input_classname_begin = 0;
                if(input_classname_begin < sout.indexOf("class")+5)
                  {
                    input_classname_begin = sout.indexOf("class")+5;
                  }
                if(input_classname_begin < sout.indexOf("struct")+6)
                  {
                    input_classname_begin = sout.indexOf("struct")+6;
                  }
                while(input_classname_begin < sout.length())
                  {
                    char c = sout.charAt(input_classname_begin);
                    if(Character.isLetterOrDigit(c) || c == '_')
                      {
                        break;
                      }
                    input_classname_begin++;
                  }
                int input_classname_end = sout.length()-1;
                if(sout.indexOf(":") > 0)
                  {
                    input_classname_end = sout.indexOf(":") -1;
                  }
                if(sout.indexOf("public") > 0 && input_classname_end > sout.indexOf("public"))
                  {
                    input_classname_end = sout.indexOf("public") -1;
                  }
                if(sout.indexOf("private") > 0 && input_classname_end > sout.indexOf("private"))
                  {
                    input_classname_end = sout.indexOf("private") -1;
                  }
                if(sout.indexOf("protected") > 0 && input_classname_end > sout.indexOf("protected"))
                  {
                    input_classname_end = sout.indexOf("protected") -1;
                  }
                if(sout.indexOf("{") > 0 && input_classname_end > sout.indexOf("{"))
                  {
                    input_classname_end = sout.indexOf("protected") -1;
                  }
                while(input_classname_end > 0)
                  {
                    char c = sout.charAt(input_classname_end);
                    if(Character.isLetterOrDigit(c) || c == '_')
                      {
                        input_classname_end++;
                        break;
                      }
                    input_classname_end--;
                  }
                if(debug_on)
                  {
                    System.out.println("input_classname_begin="+input_classname_begin+", input_classname_end="+input_classname_end);
                  }
                if(input_classname_end > input_classname_begin)
                  {
                    input_classname = sout.substring(input_classname_begin, input_classname_end);
                    if(null != input_classname && null == input_subsection)
                      {
                        input_subsection = input_classname;
                      }
                    if(debug_on)
                      {
                        System.out.println("input_classname="+input_classname);
                      }
                  }
              }
            if(input_brace_count == 0 && paren_index > 1)
              {
                if(debug_on)
                  {
                    System.out.println("input_brace_count="+input_brace_count+", paren_index="+paren_index+", sout="+sout);
                  }
                int input_funcname_end = paren_index;
                while(input_funcname_end > 0)
                  {
                    char c = sout.charAt(input_funcname_end);
                    if(Character.isLetterOrDigit(c) || c == '_')
                      {
                        break;
                      }
                    input_funcname_end--;
                  }
                int input_funcname_begin = input_funcname_end;
                while(input_funcname_begin > 0)
                  {
                    char c = sout.charAt(input_funcname_begin);
                    if(!Character.isLetterOrDigit(c) && c != '_' && c != ':')
                      {
                        input_funcname_begin++;
                        break;
                      }
                    input_funcname_begin--;
                  }

                if(input_funcname_begin < input_funcname_end)
                  {
                    input_funcname = sout.substring(input_funcname_begin, input_funcname_end+1);
                    if(input_funcname.equals("Parameter"))
                      {
                        input_funcname = null;
                      }
                    if(null != input_funcname && null == input_subsection)
                      {
                        input_subsection = input_funcname;
                      }
                    if(debug_on)
                      {
                        System.out.println("input_funcname = "+input_funcname);
                      }
                  }
              }
            int brace_index = sout.indexOf('{');
            while(brace_index >= 0)
              {
                input_brace_count++;
                brace_index = sout.indexOf('{',brace_index+1);
              }
            brace_index = sout.indexOf('}');
            while(brace_index >= 0 )
              {
                input_brace_count--;
                brace_index = sout.indexOf('}',brace_index+1);
              }
            if(input_funcname != null && input_brace_count > 0)
              {
                input_subsection = input_funcname;
                input_subsection_number++;
                input_funcname = null;
              }
            if(input_classname != null && input_brace_count > 0)
              {
                input_subsection = input_classname;
                input_subsection_number++;
                input_classname=null;
              }
            while(paren_index >= 0 )
              {
                input_paren_count++;
                paren_index = sout.indexOf('(',paren_index+1);
              }
            paren_index = sout.indexOf(')');
            while(paren_index >= 0 )
              {
                input_paren_count--;
                paren_index = sout.indexOf(')',paren_index+1);
              }
          }

        /*
          int double_slash_index = sout.indexOf("//");
          int brace_index = sout.indexOf('{');
          while(brace_index >= 0)
          {
          input_brace_count++;
          brace_index = sout.indexOf('{',brace_index+1);
          }
          brace_index = sout.indexOf('}');
          while(brace_index >= 0 )
          {
          input_brace_count--;
          brace_index = sout.indexOf('}',brace_index+1);
          }
          int paren_index = sout.indexOf('(');
          while(paren_index >= 0 )
          {
          input_paren_count++;
          paren_index = sout.indexOf('(',paren_index+1);
          }
          paren_index = sout.indexOf(')');
          while(paren_index >= 0 )
          {
          input_paren_count--;
          paren_index = sout.indexOf(')',paren_index+1);
          }
          while(sout.length() > 1 )
          {
          if(sout.charAt(0) != ' ' && sout.charAt(0) != '\t')
          {
          break;
          }
          sout = sout.substring(1);
          }
          */
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }

  private void FinalWriteLine(String str)
  {
    if(null != str && null != pos)
      {
        if(cpp_mode)
          {
            pos.println(TabForCpp(str));
          }
        else
          {
            pos.println(str);
          }
      }
  }

  boolean pos_null_warning_issued = false;

  void WriteLine(String str)
  {
    try
      {
        input_line++;
        if(null == pos)
          {
            if(!pos_null_warning_issued)
              {
                System.err.println("Merger.pos = null");
                Thread.dumpStack();
                pos_null_warning_issued = true;
              }
            return;
          }
        if(debug_on)
          {
            System.out.println("Merger.WriteLine("+str+")");
            System.out.println("merge_disabled="+merge_disabled);
            System.out.println("last_line_matched = "+last_line_matched);
            if(null == backup_lines)
              {
                System.out.println("null == backup_lines");
              }
            if(last_line_matched >= 0 && null != backup_lines)
              {
                if(last_line_matched < backup_lines.size())
                  {
                    BackupLineData bld = (BackupLineData) backup_lines.elementAt(last_line_matched);
                    if(null != bld)
                      {
                        System.out.println("bld[last_line_matched].orig_line = "+bld.orig_line);
                      }
                  }
              }
          }
        if(str.length() < 1)
          {
            if(Makefile_mode)
              {
                input_subsection=null;
              }
            blank_line_needed = true;
            return;
          }
        if(cpp_mode)
          {
            SetInputBraceCount(str);
          }
        if(null == backup_lines || merge_disabled)
          {
            if(blank_line_needed)
              {
                if(debug_on && null != logPS)
                  {
                    logPS.println("");
                  }
                FinalWriteLine("");
                blank_line_needed = false;
              }
            if(debug_on && null != logPS)
              {
                logPS.println("("+input_line+")"+ str);
              }
            FinalWriteLine(str);
            return;
          }
        int matching_line = -1;
        if(!merge_disabled)
          {
            matching_line = FindLine(str);
            if(nml_mode && matching_line > 0)
              {
                BackupLineData bld = (BackupLineData) backup_lines.elementAt(matching_line);
                str = NMLMerge(str,bld.orig_line);
              }
            if(Makefile_mode && matching_line > 0)
              {
                BackupLineData bld = (BackupLineData) backup_lines.elementAt(matching_line);
                str = MakeLineMerge(str,bld.orig_line);
              }
            boolean line_from_backup_inserted = false;
            if(last_line_matched >= 0 && matching_line > last_line_matched)
              {
                boolean blank_line_from_backup_needed = false;
                for(int i = last_line_matched+1; i < matching_line; i++)
                  {
                    BackupLineData bld = (BackupLineData) backup_lines.elementAt(i);
                    if(bld.line_for_compare.length() < 1)
                      {
                        blank_line_from_backup_needed = true;
                        continue;
                      }
                    if(blank_line_from_backup_needed)
                      {
                        if(debug_on && null != logPS)
                          {
                            logPS.println("");
                          }
                        FinalWriteLine("");
                        if(debug_on)
                          {
                            System.out.println("blank_line_from_backup");
                          }
                        blank_line_from_backup_needed = false;
                      }
                    line_from_backup_inserted = true;
                    if(debug_on)
                      {
                        System.out.println("-----\t"+bld.orig_line);
                      }
                    if(debug_on && null != logPS)
                      {
                        logPS.println("<"+i+">"+bld.orig_line);
                      }
                    FinalWriteLine(bld.orig_line);
                  }
              }
          }
        if(blank_line_needed)
          {
            if(debug_on && null != logPS)
              {
                logPS.println("");
              }
            FinalWriteLine("");
            if(debug_on)
              {
                System.out.println("blank_line");
              }
            blank_line_needed = false;
          }
        if(debug_on && null != logPS)
          {
            logPS.println("("+input_line+")"+str);
          }
        FinalWriteLine(str);
        if(matching_line > 0)
          {
            last_line_matched = matching_line;
            if(debug_on)
              {
                System.out.println("last_line_matched = "+last_line_matched);
              }
          }
      }
    catch(Exception e)
      {
        e.printStackTrace();
      }
  }
}
