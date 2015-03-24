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

package diagapplet.utils;

import rcs.utils.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Custom AWT File Dialog to select a file.
 * Used only in old CodeGen and Design tool.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class ModifiedFileDialog extends Dialog implements ActionListener, KeyListener, WindowListener, ItemListener
{

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613903L;


    Label filterLabel = null;
    TextField filterField = null;
    Label filenameLabel = null;
    TextField filenameField = null;
    Label sortbyLabel = null;
    CheckboxGroup sortbyGroup=null;
    Checkbox sortbyNameCheckbox=null;
    Checkbox sortbySizeCheckbox=null;
    Checkbox sortbyDateCheckbox=null;
    Checkbox unsortedCheckbox=null;
    public String filename=null;
    Label dirLabel = null;
    FastListPanel dirList = null;
    FastListContainer dirListContainer = null;
    Label fileLabel = null;
    FastListPanel fileList = null;
    FastListContainer fileListContainer = null;
    Button okButton = null;
    Button cancelButton = null;
    Button refreshButton = null;
    GridBagLayout gbl = null;
    public boolean done = false;
    public boolean cancel = false;
    public int mode;
    static public boolean debug_on = false;
    File dirFile = null;

    public static int LOAD = FileDialog.LOAD;
    public static int SAVE = FileDialog.SAVE;


    public ModifiedFileDialog(Frame parent, String title,  int _mode, String dir, String filter)
    {
	super(parent,true);
	init(title,_mode,dir,filter);
    }

    public ModifiedFileDialog(Frame parent, String title,  int _mode)
    {
	super(parent,true);
	init(title,_mode,".","*");
    }

    public void setDirectory(String dir)
    {
	if(!dir.endsWith(File.separator))
	    {
		dir += File.separator;
	    }
	filenameField.setText(dir);
	updateDisplay();
    }

    public void setFile(String file)
    {
	try
	    {
		File d = new File(filenameField.getText());
		File f = new File(d,file);
		filenameField.setText(f.getCanonicalPath());
		updateDisplay();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public String getFile()
    {
	File f = null;
	if(!done)
	    {
		f= new File(filenameField.getText());
		if(debug_on)
		    {
			System.out.println("getFile() retuning f.getName() = "+ f.getName());
		    }
		return f.getName();
	    }
	else
	    {
		if(filename==null)
		    {
			return null;
		    }
		f= new File(filename);
		if(debug_on)
		    {
			System.out.println("getFile() retuning f.getName() = "+ f.getName());
		    }
		return f.getName();
	    }
    }

    public void wait_for_done()
    {
	try
	    {
		while(!done)
		    {
			Thread.sleep(100);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public String getDirectory()
    {
	try
	    {
		String fname = null;
		if(!done)
		    {
			fname = filenameField.getText();
		    }
		else
		    {
			fname = filename;
		    }
		File f = new File(fname);
		if(f.isDirectory())
		    {
			String retval = f.getCanonicalPath();
			if(!retval.endsWith(File.separator))
			    {
				retval += File.separator;
			    }
			return retval;
		    }
		File d = new File(f.getParent());
		String retval = d.getCanonicalPath();
		if(!retval.endsWith(File.separator))
		    {
			retval += File.separator;
		    }
		if(debug_on)
		    {
			System.out.println("getDirectory() retuning "+ retval);
		    }
		return retval;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
		return null;
	    }
    }

    public void init(String title,  int _mode, String dir, String filter)
    {
	Font f = new Font("Monospaced",Font.PLAIN,12);
	setFont(f);
	mode = _mode;
	gbl = new GridBagLayout();
	setLayout(gbl);
	GridBagConstraints c = new GridBagConstraints();

	setTitle(title);

	filterLabel = new Label("Filter");
	add(filterLabel);
	c.gridwidth = 7;
	c.gridx = 0;
	c.gridy = 0;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(filterLabel,c);
	filterLabel.addKeyListener(this);

	if(filter == null)
	    {
		filter = "*";
	    }
	filterField = new TextField(filter);
	add(filterField);
	c.gridwidth = 7;
	c.gridx = 0;
	c.gridy = 1;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(filterField,c);
	filterField.addActionListener(this);
	filterField.addKeyListener(this);


	filenameLabel = new Label("File Name (Press <TAB> for file completion.)");
	add(filenameLabel);
	c.gridwidth = 7;
	c.gridx = 0;
	c.gridy = 2;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(filenameLabel,c);
	filenameLabel.addKeyListener(this);

	File dFile = new File(dir);
	String dcanonical = dir;
	try
	    {
		if(dFile.exists())
		    {
			dcanonical = dFile.getCanonicalPath();
			if(null == dcanonical)
			    {
				dcanonical = dir;
			    }
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(!dcanonical.endsWith(File.separator))
	    {
		dcanonical += File.separator;
	    }
	filenameField = new TextField(dcanonical,80);
	add(filenameField);
	c.gridwidth = 7;
	c.gridx = 0;
	c.gridy = 3;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(filenameField,c);
	filenameField.addActionListener(this);
	filenameField.addKeyListener(this);


	sortbyLabel = new Label("Sorted By:");
	add(sortbyLabel);
	c.gridwidth = 1;
	c.gridx = 4;
	c.gridy = 4;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(sortbyLabel,c);


	sortbyGroup= new CheckboxGroup();
	sortbyNameCheckbox= new Checkbox("Name",sortbyGroup,true);
	add(sortbyNameCheckbox);
	c.gridwidth = 1;
	c.gridx = 5;
	c.gridy = 4;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(sortbyNameCheckbox,c);
	sortbyNameCheckbox.addItemListener(this);
	sortbyNameCheckbox.addKeyListener(this);


	sortbySizeCheckbox= new Checkbox("Size",sortbyGroup,false);
	add(sortbySizeCheckbox);
	c.gridwidth = 1;
	c.gridx = 6;
	c.gridy = 4;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(sortbySizeCheckbox,c);
	sortbySizeCheckbox.addItemListener(this);
	sortbySizeCheckbox.addKeyListener(this);

	sortbyDateCheckbox= new Checkbox("Date",sortbyGroup,false);
	add(sortbyDateCheckbox);
	c.gridwidth = 1;
	c.gridx = 7;
	c.gridy = 4;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(sortbyDateCheckbox,c);
	sortbyDateCheckbox.addItemListener(this);
	sortbyDateCheckbox.addKeyListener(this);



	dirLabel = new Label("Directory");
	add(dirLabel);
	c.gridwidth = 3;
	c.gridx = 0;
	c.gridy = 5;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(dirLabel,c);
	dirLabel.addKeyListener(this);

	fileLabel = new Label("Files");
	add(fileLabel);
	c.gridwidth = 5;
	c.gridx = 3;
	c.gridy = 5;
	c.fill = GridBagConstraints.HORIZONTAL;
	gbl.setConstraints(fileLabel,c);
	fileLabel.addKeyListener(this);

	dirList = new FastListPanel(15,40,false,this);
	dirListContainer = new FastListContainer(dirList);
	add(dirListContainer);
	c.gridwidth = 3;
	c.gridx = 0;
	c.gridy = 6;
	c.fill = GridBagConstraints.BOTH;
	gbl.setConstraints(dirListContainer,c);
	dirList.addActionListener(this);
	dirList.addKeyListener(this);


	fileList = new FastListPanel(15,60,false,this);
	fileListContainer = new FastListContainer(fileList);
	add(fileListContainer);
	c.gridwidth = 5;
	c.gridx = 3;
	c.gridy = 6;
	c.fill = GridBagConstraints.BOTH;
	gbl.setConstraints(fileListContainer,c);
	fileList.addActionListener(this);
	fileList.addKeyListener(this);

	okButton = new Button("OK");
	add(okButton);
	c.gridwidth = 1;
	c.gridx = 0;
	c.gridy = 7;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(okButton,c);
	okButton.addActionListener(this);
	okButton.addKeyListener(this);

	cancelButton = new Button("CANCEL");
	add(cancelButton);
	c.gridwidth = 1;
	c.gridx = 1;
	c.gridy = 7;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(cancelButton,c);
	cancelButton.addActionListener(this);
	cancelButton.addKeyListener(this);

	refreshButton = new Button("REFRESH");
	add(refreshButton);
	c.gridwidth = 1;
	c.gridx = 2;
	c.gridy = 7;
	c.fill = GridBagConstraints.NONE;
	gbl.setConstraints(refreshButton,c);
	refreshButton.addActionListener(this);
	refreshButton.addKeyListener(this);

	addKeyListener(this);
	addWindowListener(this);

	pack();
	updateDisplay();
    }

    public void itemStateChanged(ItemEvent evt)
    {
	updateDisplay();
    }


    public void actionPerformed(ActionEvent evt)
    {
	if(debug_on)
	    {
		System.out.println("ModifiedFileDialog.actionPerformed("+evt+")");
	    }
	if(evt.getSource() == cancelButton)
	    {
		cancel = true;
		done=true;
		dispose();
	    }
	if(evt.getSource() == okButton || evt.getSource() == filenameField)
	    {
		filename = filenameField.getText();
		if(debug_on)
		    {
			System.out.println("filenameField.getText()="+filenameField.getText());
		    }
		if(null == filename)
		    {
			Toolkit tk = Toolkit.getDefaultToolkit();
			tk.beep();
			return;
		    }
		else
		    {
			if(filename.length() < 1)
			    {
				Toolkit tk = Toolkit.getDefaultToolkit();
				tk.beep();
				filename = null;
				return;
			    }
			File f = new File(filename);
			if(f.isDirectory())
			    {
				if(evt.getSource() == okButton)
				    {
					String newfname = fileList.getSelectedItem();
					if(debug_on)
					    {
						System.out.println("String newfname = fileList.getSelectedItem() ="+newfname);
					    }
					if(newfname != null)
					    {
						int tabindex = newfname.indexOf('\t');
						if(tabindex > 0)
						    {
							newfname = newfname.substring(0,tabindex);
						    }
						if(newfname.length() > 0)
						    {
							f = new File(f,newfname);
						    }
						try
						    {
							filenameField.setText(f.getCanonicalPath());
							filename = f.getCanonicalPath();
						    }
						catch(Exception e)
						    {
							e.printStackTrace();
						    }
					    }
					else
					    {
						Toolkit tk = Toolkit.getDefaultToolkit();
						tk.beep();
						return;
					    }
				    }
			    }
			if(debug_on)
			    {
				System.out.println("f ="+f);
			    }
			if(mode == LOAD &&
			   !f.exists())
			    {
				System.err.println("File "+f+" does not exist.");
				Toolkit tk = Toolkit.getDefaultToolkit();
				tk.beep();
				filename = null;
				return;
			    }
			if(mode == SAVE &&
			   f.exists())
			    {
				if(debug_on)
				    {
					System.out.println("Saving over existing file.\n");
				    }
			    }
			done = true;
			dispose();
		    }
	    }
	if(evt.getSource() == dirList)
	    {
		try
		    {
			String fstring = filenameField.getText();
			File fFile = new File(fstring);
			if(debug_on)
			    {
				System.out.println("fFile="+fFile);
			    }
			File dFile = dirFile;
			if(null == dFile)
			    {
				dFile = fFile;
				if((!fFile.isDirectory() && !fstring.endsWith(File.separator)) &&
				   null != fFile.getParent())
				    {
					dFile = new File(fFile.getParent());
				    }
			    }
			if(debug_on)
			    {
				System.out.println("dFile="+dFile);
			    }
			String newfname = dirList.getSelectedItem();
			if(debug_on)
			    {
				System.out.println(" dirList.getSelectedItem() = "+ dirList.getSelectedItem());
			    }
			int tabindex = newfname.indexOf('\t');
			if(tabindex > 0)
			    {
				newfname = newfname.substring(0,tabindex);
			    }
			File gFile = new File(dFile,newfname);
			if(debug_on)
			    {
				System.out.println("gFile="+gFile);
			    }
			if(dirList.getSelectedItem().startsWith(".."))
			    {
				if(debug_on)
				    {
					System.out.println("Go up 1 directory.");
					System.out.println("dFile.getParent()="+dFile.getParent());
				    }
				gFile = new File(dFile.getParent());
			    }
			String canonical_fname = gFile.getCanonicalPath();
			if(null != canonical_fname)
			    {
				if(!canonical_fname.endsWith(File.separator))
				    {
					canonical_fname += File.separator;
				    }
			    }
			else
			    {
				System.err.println("Bad File: "+gFile);
			    }
			filenameField.setText(canonical_fname);
			updateDisplay();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
	    }
	if(evt.getSource() == fileList)
	    {
		try
		    {
			String fstring = filenameField.getText();
			File fFile = new File(fstring);
			File dFile = dirFile;
			if(null == dFile)
			    {
				dFile = fFile;
				if((!fFile.isDirectory() && !fstring.endsWith(File.separator)) &&
				   null != fFile.getParent())
				    {
					dFile = new File(fFile.getParent());
				    }
			    }
			String newfname = fileList.getSelectedItem();
			int tabindex = newfname.indexOf('\t');
			if(tabindex > 0)
			    {
				newfname = newfname.substring(0,tabindex);
			    }
			File gFile = new File(dFile,newfname);

			String canonical_fname = gFile.getCanonicalPath();
			filenameField.setText(canonical_fname);
			if(gFile.exists() && mode == LOAD)
			    {
				filename = canonical_fname;
				done=true;
				dispose();
				return;
			    }
			updateDisplay();
		    }
		catch(Exception e)
		    {
			e.printStackTrace();
		    }
	    }
	if(evt.getSource() == filterField)
	    {
		updateDisplay();
	    }
	if(evt.getSource() == filenameField)
	    {
		updateDisplay();
	    }
	if(evt.getSource() == refreshButton)
	    {
		updateDisplay();
	    }
    }

    boolean FileNameCompare(File f1, File f2)
    {
	boolean retval = false;
	try
	    {
		String f1n = f1.getName();
		String f2n = f2.getName();
		String f2un = f2n.toUpperCase();
		String f1un = f1n.toUpperCase();
		if(f2un.equals(f1un))
		    {
			retval = (f1n.compareTo(f2n) < 0);
		    }
		else
		    {
			retval = (f1un.compareTo(f2un) < 0);
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(debug_on)
	    {
		System.out.println("FileNameCompare("+f1+", "+f2+") returned "+retval);
	    }
	return retval;
    }

    boolean FileCompare(File f, File kf)
    {
	boolean retval = false;
	try
	    {
		if(sortbyNameCheckbox.getState())
		    {
			retval = (FileNameCompare(f,kf));
		    }
		else if(sortbySizeCheckbox.getState())
		    {
			retval = (f.length() < kf.length());
		    }
		else if(sortbyDateCheckbox.getState())
		    {
			retval = (f.lastModified() < kf.lastModified());
		    }
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	if(debug_on)
	    {
		System.out.println("FileCompare("+f+", "+kf+") returned "+retval);
	    }
	return retval;
    }

    @SuppressWarnings("unchecked")
	public void updateDisplay()
    {
	String filename_string = filenameField.getText();
	String filter_string = filterField.getText();
	File fFile = new File(filename_string);
	File dFile = fFile;
	String fname = null;
	if(null != fFile.getParent() &&
	   !fFile.isDirectory() &&
	   !filename_string.endsWith(File.separator))
	    {
		dFile = new File(fFile.getParent());
		fname = fFile.getName();
	    }
	String dlist[] = dFile.list();
	if(null == dlist)
	    {
		System.err.println("list() for "+dFile+" is null.");
		return;
	    }
	dirFile=dFile;
	dirList.clear();
	fileList.clear();
	if(dFile.getParent() != null)
	    {
		dirList.add("..");
	    }
	Vector dvector = new Vector();
	Vector fvector = new Vector();
	for(int i = 0; i < dlist.length; i++)
	    {
		File f = new File(dFile,dlist[i]);
		if(null != fname)
		    {
			if(fname.length() > 0)
			    {
				if(debug_on)
				    {
					System.out.println("fname="+fname+", dlist[i]="+dlist[i]+", statsWith="+dlist[i].startsWith(fname));
				    }
				if(!dlist[i].startsWith(fname))
				    {
					continue;
				    }
			    }
		    }
		//System.out.println("f = "+f);
		if(f.isDirectory())
		    {
			if(f.getName().startsWith("."))
			    {
				continue;
			    }
			int upper = dvector.size()-1;
			int lower = 0;
			int lastk = -1;
			int k = 0;
			while(upper > lower)
			    {
				k = (upper+lower)/2;
				if(k == lastk)
				    {
					break;
				    }
				lastk = k;
				if(debug_on)
				    {
					System.out.println("k="+k+", lower="+lower+", upper="+upper);
				    }
				File kf = (File) dvector.elementAt(k);
				if(FileNameCompare(f,kf))
				    {
					upper = k;
				    }
				else
				    {
					lower=k;
				    }
			    }
			if(k < dvector.size())
			    {
				File kf = (File) dvector.elementAt(k);
				if(!FileNameCompare(f,kf))
				    {
					k++;
				    }
			    }
			if(k < dvector.size())
			    {
				File kf = (File) dvector.elementAt(k);
				if(!FileNameCompare(f,kf))
				    {
					k++;
				    }
			    }
			if(k < dvector.size())
			    {
				if(debug_on)
				    {
					System.out.println(f.getName()+" inserted at dvector at "+k);
				    }
				dvector.insertElementAt(f,k);
			    }
			else
			    {
				if(debug_on)
				    {
					System.out.println(f.getName()+" added to  dvector");
				    }
				dvector.addElement(f);
			    }
		    }
		else
		    {
			int upper = fvector.size()-1;
			int lower = 0;
			int lastk = -1;
			int k = 0;
			while(upper > lower)
			    {
				k = (upper+lower)/2;
				if(k == lastk)
				    {
					break;
				    }
				lastk = k;
				if(debug_on)
				    {
					System.out.println("k="+k+", lower="+lower+", upper="+upper);
				    }
				File kf = (File) fvector.elementAt(k);
				if(debug_on)
				    {
					System.out.println("kf = "+kf);
				    }
				if(FileCompare(f,kf))
				    {
					upper = k;
				    }
				else
				    {
					lower = k;
				    }
			    }
			if(k < fvector.size())
			    {
				File kf = (File) fvector.elementAt(k);
				if(!FileCompare(f,kf))
				    {
					k++;
				    }
			    }
			if(k < fvector.size())
			    {
				File kf = (File) fvector.elementAt(k);
				if(!FileCompare(f,kf))
				    {
					k++;
				    }
			    }
			if(k < fvector.size())
			    {
				if(debug_on)
				    {
					System.out.println(f.getName()+" inserted in  fvector at "+k);
				    }
				fvector.insertElementAt(f,k);
			    }
			else
			    {
				if(debug_on)
				    {
					System.out.println(f.getName()+" added to  fvector");
				    }
				fvector.addElement(f);
			    }
		    }
	    }

	for(int di = 0; di < dvector.size(); di++)
	    {
		File f = (File) dvector.elementAt(di);
		if(null == f)
		    {
			continue;
		    }
		String perm = "--";
		if(f.canRead())
		    {
			perm ="R-";
		    }
		if(f.canWrite())
		    {
			perm = perm.charAt(0)+"W";
		    }
		String pad = "";
		int padlength = 30 - f.getName().length();
		if(padlength > 0)
		{
		    for(int ii = 0; ii < padlength; ii++)
			{
			    pad+= " ";
			}
		}
		dirList.add(f.getName() +"\t"+pad+"\t"+perm);
	    }

	for(int fi = 0; fi < fvector.size(); fi++)
	    {
		File f = (File) fvector.elementAt(fi);
		if(null == f)
		    {
			continue;
		    }
		String perm = "--";
		if(f.canRead())
		    {
			perm ="R-";
		    }
		if(f.canWrite())
		    {
			perm = perm.charAt(0)+"W";
		    }
		String pad = "";
		int padlength = 30 - f.getName().length();
		if(padlength > 0)
		{
		    for(int ii = 0; ii < padlength; ii++)
			{
			    pad+= " ";
			}
		}
		if(SimpleFileFilter.CheckPatternMatch(f.getName(),filter_string))
		    {
			fileList.add(f.getName() +"\t"+pad+"\t"+perm+"\t     \t"+f.length()+"\t    \t"+new Date(f.lastModified()));
		    }
	    }
	dirList.repaint();
	fileList.repaint();
	repaint();
    }

    public void keyPressed(KeyEvent evt)
    {
    }

    public void keyReleased(KeyEvent evt)
    {
    }


    @SuppressWarnings("unchecked")
	public void tryAutoComplete()
    {
	try
	    {
		if(debug_on)
		    {
			System.out.println("tryAutoComplete");
		    }
		String filename_string = filenameField.getText();
		if(null == filename_string)
		    {
			return;
		    }
		if(debug_on)
		    {
			System.out.println("filename_string ="+filename_string);
		    }
		String filter_string = filterField.getText();
		File fFile = new File(filename_string);
		File dFile = null;
		char last_char = filename_string.charAt(filename_string.length()-1);
		while(last_char == '\t')
		    {
			filename_string = filename_string.substring(0,filename_string.length()-1);
			last_char = filename_string.charAt(filename_string.length());
		    }
		if(debug_on)
		    {
			System.out.println("filename_string="+filename_string);
		    }
		if(filename_string.endsWith(File.separator))
		    {
			Toolkit tk = Toolkit.getDefaultToolkit();
			tk.beep();
			updateDisplay();
			return;
		    }
		if(fFile.isDirectory())
		    {
			Toolkit tk = Toolkit.getDefaultToolkit();
			tk.beep();
			updateDisplay();
			return;
		    }
		else
		    {
			dFile = new File(fFile.getParent());
		    }
		String dlist[] = dFile.list();
		dirList.clear();
		fileList.clear();
		if(dFile.getParent() != null)
		    {
			dirList.add("..");
		    }
		int matching_files = 0;
		Vector matches = new Vector();
		String first_match = null;
		if(debug_on)
		    {
			System.out.println("fFile.getName() = "+fFile.getName());
		    }
		if(fFile.getName().length() < 1)
		    {
			Toolkit tk = Toolkit.getDefaultToolkit();
			tk.beep();
			updateDisplay();
			return;
		    }
		for(int i = 0; i < dlist.length; i++)
		    {
			if(fFile.getName().regionMatches(0,dlist[i],0,fFile.getName().length()))
			    {
				if(debug_on)
				    {
					System.out.println("matching_files="+matching_files+", dlist[i]="+dlist[i]);
				    }
				File xf = new File(dFile,dlist[i]);
				if(!xf.isDirectory() && filter_string != null)
				    {
					if(!SimpleFileFilter.CheckPatternMatch(xf.getName(),filter_string))
					    {
						continue;
					    }
				    }
				matching_files++;
				if(null == first_match)
				    {
					first_match = dlist[i];
				    }
				matches.addElement(dlist[i]);
			    }
		    }
		if(debug_on)
		    {
			System.out.println("first_match="+first_match);
		    }
		if(matching_files > 1)
		    {
			int matchlength = first_match.length();
			for(int i = 0; i < matches.size(); i++)
			    {
				String si = (String) matches.elementAt(i);
				if(debug_on)
				    {
					System.out.println("si="+si+", matchlength="+matchlength);
				    }
				if(si.length() <= matchlength)
				    {
					matchlength = si.length();
				    }
				while(!si.regionMatches(0,first_match,0,matchlength))
				    {
					matchlength--;
					if(matchlength < 1)
					    {
						break;
					    }
				    }
			    }
			if(debug_on)
			    {
				System.out.println("matchlength="+matchlength);
			    }
			if(matchlength <= fFile.getName().length())
			    {
				Toolkit tk = Toolkit.getDefaultToolkit();
				tk.beep();
			    }
			else
			    {
				filename_string = (new File(dFile,first_match.substring(0,matchlength))).getCanonicalPath();
				filenameField.setText(filename_string);
			    }
		    }
		else if(matching_files == 1)
		    {
			filename_string = (new File(dFile,first_match)).getCanonicalPath();
			filenameField.setText(filename_string);
		    }
		else
		    {
			Toolkit tk = Toolkit.getDefaultToolkit();
			tk.beep();
		    }
		filenameField.requestFocus();
		updateDisplay();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void keyTyped(KeyEvent evt)
    {
	//System.out.println("keyTyped("+evt+")");
	//System.out.println("evt.getKeyCode() = "+evt.getKeyCode());
	//System.out.println("KeyEvent.VK_TAB = "+KeyEvent.VK_TAB);
	if(evt.getKeyCode() == KeyEvent.VK_TAB || evt.getKeyChar() == KeyEvent.VK_TAB)
	    {
		tryAutoComplete();
		filenameField.requestFocus();
	    }
    }

    /*
     * The functions windowOpened, windowClosing, windowClosed, windowActivated,
     * windowDeactivated, windowIconified, and windowDeiconified are needed
     * to implement WindowListener and basically replace handleEvent
     * from JDK 1.0.x
     *
     */

    public void windowOpened(WindowEvent evt)
    {
    }

    public void windowClosing(WindowEvent evt)
    {
	try
	    {
		done = true;
		dispose();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void windowClosed(WindowEvent evt)
    {
    }

    public void windowIconified(WindowEvent evt)
    {
    }

    public void windowDeiconified(WindowEvent evt)
    {
    }

    public void windowActivated(WindowEvent evt)
    {
    }

    public void windowDeactivated(WindowEvent evt)
    {
    }

    public static void main(String args[])
    {
	Frame f = new Frame();
	String dir = ".";
	if(args.length > 0)
	    {
		dir = args[0];
	    }
	if(args.length > 1)
	    {
		debug_on = true;
	    }
	ModifiedFileDialog mfd = new ModifiedFileDialog(f,"Test",LOAD,dir,"*");
	System.out.println("mfd created.\n");
	mfd.setVisible(true);
	System.out.println("mfd shown\n");
	while(!mfd.done)
	    {
		try
		    {
			Thread.sleep(100);
		    }
		catch(Exception e)
		    {
		    }
	    }
	System.out.println("mfd.filename = "+mfd.filename);
	System.out.println("mfd done.\n");
	System.exit(0);
    }
}
