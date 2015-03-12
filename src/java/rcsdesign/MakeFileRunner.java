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

class MakeFileRunner implements Runnable
{

        File dir;
        String command;
        static File f = null;
        String envp[] = null;

        MakeFileRunner(File _dir, String _command, String _envp[])
        {
                dir = _dir;
                command = _command;
                envp = _envp;
        }

        public void run()
        {
                try
                {
                        if(null != f)
                        {
                                if(f.exists())
                                {
                                        f.delete();
                                }
                                f = null;
                                Thread.sleep(100);
                        }
                        System.out.println("Running "+command+" in "+dir.toString());
                        if(null != envp)
                        {
                                System.out.println("Environment:");
                                for(int i = 0; i < envp.length; i++)
                                {
                                        System.out.println(envp[i]);
                                }
                        }
                        f = new File("tmpmake.bat");
                        try
                        {
                                if(f.exists())
                                {
                                        if(!f.canWrite())
                                        {
                                                return;
                                        }
                                        f.delete();
                                }
                        }
                        catch(Exception e)
                        {
                                e.printStackTrace();
                        }
                        FileOutputStream fos = new FileOutputStream(f);
                        PrintWriter ps = new PrintWriter(fos);
                        ps.println("cd "+dir.getAbsolutePath());
                        ps.println("");
                        ps.println(command);
                        ps.println("");
                        ps.println("pause");
                        ps.println("");
                        ps.flush();
                        ps.close();
                        fos.close();
                        Thread.sleep(100);
                        int tries = 0;
                        while(!f.exists() && tries > 20)
                        {
                                Thread.sleep(100);
                                tries++;
                        }
                        Process p = Runtime.getRuntime().exec("cmd /c start tmpmake.bat", envp);
                        p.waitFor();
                        Thread.sleep(2000);

                }
                catch(Exception e)
                {
                        e.printStackTrace();
                }
        }
}
