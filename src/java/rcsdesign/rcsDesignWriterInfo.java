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
import diagapplet.CodeGen.*;

 class rcsDesignWriterInfo
{
    public String AppName = "";
    public String AppDir = "";
    public String RcsLibDir = "";
    public String ConfigFile = "";
    private String UserDir = "";
    public String getUserDir()
    {
	return UserDir;
    }
    public void setUserDir(String new_user_dir)
    {
	//Thread.dumpStack();
	//System.out.println("new_user_dir="+new_user_dir+", UserDir="+UserDir);
	UserDir = new_user_dir;
	rcs.utils.URL_and_FileLoader.current_directory=new_user_dir;
	rcs.utils.URL_and_FileLoader.AddToSearchPath(new_user_dir);
    }

    // typically xterm -e or start 
    public String TerminalCommand="";
    public String MakeCommand="make ";
    public String java_cmd_prefix = "java ";
    public String java_classpath_separator = ":";
    public String java_setup = "";
    public String java_setup_file_name="";
    public String java_plat="java";
    public File dirFile = null;
    public Hashtable modulesHashtable = new Hashtable();
    public Hashtable serversHashtable = new Hashtable();
    public Hashtable mainloopsHashtable = new Hashtable();
    public ModuleInfo curModule = null;
    public ServerInfo curServer = null;
    public rcsdesignMainLoopInfo curmainLoop = null;
    public Hashtable buffersHashtable = new Hashtable();
    public Vector auxChannelsVector = null;
    public Hashtable fileTypeInfoHashtable = new Hashtable();
    public String cpp_ext = ".cc";
    public String hpp_ext = ".hh";
    public String obj_ext = ".o";
    public String DevPlat = "UNIX";
    public boolean useJavaInScripts = true;
    public boolean mswinDevPlat = false;
    public boolean useMerger = true;
    public boolean makeBackups = true;
    public boolean singleDir = true;
    public boolean quitFillDirectories = false;
    public rcsDesignGui gui = null;
    public String designLog = "";
    public boolean list_modules_by_number = false;
}
