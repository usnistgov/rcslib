package pvjscript;

import java.awt.Color;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;

/*
 * To change this template, choose Tools | Templates
 * 
 */
/**
 *
 * @author shackle
 */
public class ProcessView {

    public static int BUF_LIMIT = 100000;
    //public static boolean DEBUG = false;
    final private String orig_cmd;

    public ProcessView(String cmdString) {
        this.orig_cmd = cmdString;
        this.setCommand(cmdString);
    }
    protected long StartTime;

    public String toString() {
        return orig_cmd;
    }

    /**
     * Get the value of StartTime
     *
     * @return the value of StartTime
     */
    public long getStartTime() {
        return StartTime;
    }
    private Thread es_print_thread = null;
    private Thread os_print_thread = null;
    private Thread wait_for_thread = null;
    protected OutputStream PipedOutput;

    /**
     * Get the value of PipedOutput
     *
     * @return the value of PipedOutput
     */
    public OutputStream getPipedOutput() {
        return PipedOutput;
    }

    /**
     * Set the value of PipedOutput
     *
     * @param PipedOutput new value of PipedOutput
     */
    public void setPipedOutput(OutputStream PipedOutput) {
        this.PipedOutput = PipedOutput;
    }
    protected String[] envp;

    /**
     * Get the value of envp
     *
     * @return the value of envp
     */
    public String[] getEnvp() {
        return envp;
    }

    /**
     * Set the value of envp
     *
     * @param envp new value of envp
     */
    public void setEnvp(String[] envp) {
        this.envp = envp;
    }
    protected File dir;

    /**
     * Get the value of dir
     *
     * @return the value of dir
     */
    public File getDir() {
        return dir;
    }

    /**
     * Set the value of dir
     *
     * @param dir new value of dir
     */
    public void setDir(File dir) {
        this.dir = dir;
    }
    protected boolean builtin;

    /**
     * Get the value of builtin
     *
     * @return the value of builtin
     */
    public boolean isBuiltin() {
        return builtin;
    }

    String Find(File dir, String pattern) throws Exception {
        pattern = pattern.replaceAll("[.]", "[.]");
        final String regexp = pattern.replaceAll("[*]", ".*");
        FilenameFilter filter = new FilenameFilter() {

            public boolean accept(File dir, String name) {
                return name.matches(regexp);
            }
        };
        return Find(dir, filter);
    }

    String Find(File dir, FilenameFilter filter) throws Exception {
        File fa[] = dir.listFiles(filter);
        String s = "";
        if (null != fa) {
            for (File f : fa) {
                s += f.toString();
                this.appendOut(f.toString());
            }
        }
        File dira[] = dir.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                return pathname.isDirectory();
            }
        });
        if (null != dira) {
            for (File subdir : dira) {
                s += Find(subdir, filter);
            }
        }
        return s;
    }
    private Thread builtin_thread = null;

    private boolean chkBuiltin(String cmd, ProcessView scriptPv) throws Exception {
        Runnable builtin_runnable = null;
        builtin = false;
        String args[] = cmd.split("[ \t]");
        if (args[0].compareTo("find") == 0) {
            final String badfind_msg = "find must be of the form find <dir> -name <pattern>";
            if (args.length != 4) {
                throw new Exception(badfind_msg);
            }
            if (args[2].compareTo("-name") != 0) {
                throw new Exception(badfind_msg);
            }
            File finddir = new File(args[1]);
            if (!finddir.isAbsolute()) {
                finddir = new File(dir, args[1]);
            }
            final String pattern = args[3];
            final File fdir = finddir;
            builtin_runnable = new Runnable() {

                public void run() {
                    try {
                        Find(fdir, pattern);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            };
        } else if (args[0].compareTo("pwd") == 0) {
            builtin_runnable = new Runnable() {

                public void run() {
                    try {
                        appendOut(dir.getAbsolutePath());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            };
        }
        if (null != builtin_runnable) {
            builtin = true;
            if (!this.equals(scriptPv)) {
                scriptPv.appendDebug("<builtin>+ " + this.Command, this.getBuiltinStyle());
            }
            this.appendDebug("<builtin>+ " + this.Command, this.getBuiltinStyle());
            builtin_thread = new Thread(builtin_runnable);
            builtin_thread.start();
            return true;
        }
        return false;
    }

    public void Start(ProcessView scriptPv) throws Exception {
        if (es_print_thread != null) {
            es_print_thread.interrupt();
            es_print_thread = null;
        }
        if (os_print_thread != null) {
            os_print_thread.interrupt();
            os_print_thread = null;
        }
        if (wait_for_thread != null) {
            wait_for_thread.interrupt();
            wait_for_thread = null;
        }
        if (chkBuiltin(this.getCommand(), scriptPv)) {
            return;
        }
        Runtime r = Runtime.getRuntime();
        final String cmdString = this.getCommand();
        this.StartTime = System.currentTimeMillis();
        String args[] = cmdString.split("[ \t]+");
        String csplit[] = args[0].split("/+");
        File f = new File(csplit[0]);
        if (csplit[0].compareTo(".") == 0) {
            f = this.getDir();
        }
        for (int i = 1; i < csplit.length; i++) {
            if (csplit[i].length() > 0) {
                f = new File(f, csplit[i]);
            }
        }
        File exef = new File(f.getParentFile(), f.getName() + ".exe");
        if (!f.exists() && exef.exists()) {
            f = exef;
        }
        String cmdString2 = f.toString();
        for (int i = 1; i < args.length; i++) {
            cmdString2 += " " + args[i];
        }
        try {
            this.p = r.exec(cmdString, envp, dir);
        } catch (java.io.IOException ioe) {
            this.p = r.exec(cmdString2, envp, dir);
            this.setCommand(cmdString2);
        }
        alive = true;
        this.getOutputStream();
        if (!this.equals(scriptPv)) {
            scriptPv.appendDebug("<external>+ " + this.Command, this.getExternalStyle());
        }
        this.appendDebug("<external>+ " + this.Command, this.getExternalStyle());
        final InputStream es = this.p.getErrorStream();
        wait_for_thread = new Thread(new Runnable() {

            public void run() {
                try {
                    p.waitFor();
                } catch (InterruptedException ie) {
                    // ignore
                }
                alive = false;
                appendDebug(Command + " exited.", getExternalStyle());
            }
        });
        wait_for_thread.start();
        es_print_thread = new Thread(new Runnable() {

            public void run() {
                try {
                    final InputStreamReader isr = new InputStreamReader(es);
                    final BufferedReader br = new BufferedReader(isr);
                    String line = null;
                    while ((line = br.readLine()) != null
                            && !Thread.currentThread().isInterrupted()) {
                        appendErr(line);
                    }
                    br.close();
                    isr.close();
                    es.close();
                } catch (Exception exception) {
                    exception.printStackTrace();
                }
            }
        });
        final InputStream is = this.p.getInputStream();
        os_print_thread = new Thread(new Runnable() {

            public void run() {
                try {
                    final InputStreamReader isr = new InputStreamReader(is);
                    final BufferedReader br = new BufferedReader(isr);
                    String line = null;
                    while ((line = br.readLine()) != null
                            && !Thread.currentThread().isInterrupted()) {
                        appendOut(line);
                    }
                    br.close();
                    isr.close();
                    is.close();
                } catch (Exception exception) {
                    exception.printStackTrace();
                }
            }
        });
        Thread.sleep(500);
        os_print_thread.start();
        es_print_thread.start();
        Thread.sleep(500);
    }

    public void close() {
        if (es_print_thread != null) {
            es_print_thread.interrupt();
            es_print_thread = null;
        }
        if (os_print_thread != null) {
            os_print_thread.interrupt();
            os_print_thread = null;
        }
        if (wait_for_thread != null) {
            wait_for_thread.interrupt();
            wait_for_thread = null;
        }
        if (null != this.builtin_thread) {
            this.builtin_thread.interrupt();
            this.builtin_thread = null;
        }
        if (null != this.PipedOutput) {
            try {
                this.PipedOutput.flush();
            } catch (IOException ex) {
//                Logger.getLogger(ProcessView.class.getName()).log(Level.SEVERE, null, ex);
            }
            this.PipedOutput = null;
        }
        if (null != this.outputStream) {
            try {
                this.outputStream.close();
            } catch (IOException ex) {
                //Logger.getLogger(ProcessView.class.getName()).log(Level.SEVERE, null, ex);
            }
            this.outputStream = null;
        }
        if (null != p) {
            p.destroy();
            p = null;
        }
    }

    protected void finalize() throws Throwable {
        super.finalize();
        close();
    }
    private String Err;
    public static final String PROP_ERR = "Err";

    /**
     * Get the value of Err
     *
     * @return the value of Err
     */
    public String getErr() {
        return Err;
    }

    public Style getErrStyle() {
        if (null == this.errStyle) {
            this.initDocument();
        }
        return this.errStyle;
    }

    public void appendErr(String s) {
        System.err.println(s);
        this.appendOutPlusErr(s, this.getErrStyle());
        if (null == this.Err) {
            this.setErr(s);
        } else {
            if (s.length() + Err.length() > BUF_LIMIT) {
                Err = Err.substring(s.length() + Err.length() - BUF_LIMIT);
                int nindex = Err.indexOf('\n');
                if (nindex < 0 || nindex > Err.length() - 1) {
                    setErr(s);
                    return;
                }
                Err = Err.substring(nindex + 1);
            }
            this.setErr(this.Err + "\n" + s);
        }
    }

    /**
     * Set the value of Err
     *
     * @param Err new value of Err
     */
    public void setErr(String Err) {
        String oldErr = this.Err;
        this.Err = Err;
        propertyChangeSupport.firePropertyChange(PROP_ERR, oldErr, Err);
    }
    private PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

    /**
     * Add PropertyChangeListener.
     *
     * @param listener listener to add
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        propertyChangeSupport.addPropertyChangeListener(listener);
    }

    /**
     * Remove PropertyChangeListener.
     *
     * @param prop property 
     * @param listener listener to remove
     */
    public void removePropertyChangeListener(String prop,
            PropertyChangeListener listener) {
        propertyChangeSupport.removePropertyChangeListener(prop, listener);
    }
    public static StringBuffer allOutputBuffer = new StringBuffer("");
    private String Out;
    public static final String PROP_OUT = "Out";

    /**
     * Get the value of Out
     *
     * @return the value of Out
     */
    public String getOut() {
        return Out;
    }

    public Style getOutStyle() {
        if (null == this.outStyle) {
            this.initDocument();
        }
        return this.outStyle;
    }

    public void appendOut(String s) throws Exception {
        if (null != this.PipedOutput) {
            this.PipedOutput.write((s + "\n").getBytes());
            this.PipedOutput.flush();
        }
        System.out.println(s);
        this.appendOutPlusErr(s, this.getOutStyle());
        if (Out == null) {
            setOut(s);
        } else {
            if (s.length() + Out.length() > BUF_LIMIT) {
                Out = Out.substring(s.length() + Out.length() - BUF_LIMIT);
                int nindex = Out.indexOf('\n');
                if (nindex < 0 || nindex > Out.length() - 1) {
                    setOut(s);
                    return;
                }
                Out = Out.substring(nindex + 1);
            }
            setOut(Out + "\n" + s);
        }
    }

    /**
     * Set the value of Out
     *
     * @param Out new value of Out
     */
    public void setOut(String Out) {
        String oldOut = this.Out;
        this.Out = Out;
        propertyChangeSupport.firePropertyChange(PROP_OUT, oldOut, Out);
    }
    private DefaultStyledDocument OutPlusErr;

    /**
     * Get the value of OutPlusErr
     *
     * @return the value of OutPlusErr
     */
    public DefaultStyledDocument getOutPlusErr() {
        if (null == OutPlusErr) {
            this.initDocument();
        }
        return OutPlusErr;
    }
    private DefaultStyledDocument OutPlusErrPlusDebug;

    /**
     * Get the value of OutPlusErr
     *
     * @return the value of OutPlusErr
     */
    public DefaultStyledDocument getOutPlusErrPlusDebug() {
        if (null == OutPlusErrPlusDebug) {
            this.initDocument();
        }
        return OutPlusErrPlusDebug;
    }
    private Style errStyle = null;
    private Style outStyle = null;
    private Style builtinStyle = null;
    private Style externalStyle = null;

    public Style getBuiltinStyle() {
        if (null == this.builtinStyle) {
            this.initDocument();
        }
        return this.builtinStyle;
    }

    public Style getExternalStyle() {
        if (null == this.externalStyle) {
            this.initDocument();
        }
        return this.externalStyle;
    }
    private StyleContext doc_sc = null;

    ;

    protected void initDocument() {
        //Initialize some styles.
        if (this.doc_sc == null) {
            this.doc_sc = new StyleContext();

            Style def = doc_sc.getStyle(StyleContext.DEFAULT_STYLE);

            errStyle = doc_sc.addStyle("err", def);
            StyleConstants.setForeground(errStyle, Color.red);

            outStyle = doc_sc.addStyle("out", def);
            StyleConstants.setForeground(outStyle, Color.black);

            externalStyle = doc_sc.addStyle("external", def);
            StyleConstants.setForeground(externalStyle, Color.green.darker());

            builtinStyle = doc_sc.addStyle("builtin", def);
            StyleConstants.setForeground(builtinStyle, Color.blue);
        }
        this.OutPlusErr = new DefaultStyledDocument(doc_sc);
        this.OutPlusErrPlusDebug = new DefaultStyledDocument(doc_sc);
    }

    public void appendOutPlusErr(String s, Style style) {
        try {
            if (null == this.OutPlusErr) {
                this.initDocument();
            }
            final int start = (s.length() + OutPlusErr.getLength() - BUF_LIMIT);
            if (start > 0) {
                OutPlusErr.remove(0, start);
            }
            OutPlusErr.insertString(OutPlusErr.getLength(), s + "\n", style);
            OutPlusErr.setLogicalStyle(OutPlusErr.getLength(), this.outStyle);
            if (style.equals(this.externalStyle) || style.equals(this.builtinStyle)) {
                System.err.println(s);
            }
            appendDebug(s, style);
        } catch (BadLocationException badLocationException) {
            badLocationException.printStackTrace();
        }
    }

    public void appendDebug(String s, Style style) {
        try {
            if (null == this.OutPlusErrPlusDebug) {
                this.initDocument();
            }
            final int start = (s.length() + OutPlusErrPlusDebug.getLength() - BUF_LIMIT);
            if (start > 0) {
                OutPlusErrPlusDebug.remove(0, start);
            }
            OutPlusErrPlusDebug.insertString(OutPlusErrPlusDebug.getLength(), s + "\n", style);
            OutPlusErrPlusDebug.setLogicalStyle(OutPlusErrPlusDebug.getLength(), this.outStyle);
        } catch (BadLocationException badLocationException) {
            badLocationException.printStackTrace();
        }
    }
    protected String Command;

    /**
     * Get the value of Command
     *
     * @return the value of Command
     */
    public String getCommand() {
        return Command;
    }

    /**
     * Set the value of Command
     *
     * @param Command new value of Command
     */
    public void setCommand(String Command) {
        this.Command = Command;
    }
    protected Process p;
    private OutputStream outputStream = null;

    public OutputStream getOutputStream() {
        if (null != outputStream) {
            return outputStream;
        }
        if (null != p) {
            outputStream = p.getOutputStream();
            return outputStream;
        }
        return null;
    }

    public int waitFor() throws Exception {
        if (null != builtin_thread) {
            builtin_thread.join();
            builtin_thread = null;
            alive = false;
            return 0;
        }
        if (null != p) {
            int r = p.waitFor();
            alive = false;
            return r;
        }
        return -1;
    }
    private boolean alive = false;

    public boolean isAlive() {
        return this.alive;
    }
}
