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

package diagapplet;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import diagapplet.utils.URLLoadInfoPanel;
import rcs.utils.URL_and_FileLoader;

class LineInfo extends Object
{
        public boolean line_starts_inside_comment;
        public String line;
}

class CodeFileInfo extends Object
{
        Vector codeLinesVector = null;
        int line_number = 0;
        int max_line_width = 0;
        public String fileName = null;
}

/*
 *
 * CodeDisplayPanel
 *
 */
class CodeDisplayPanel extends Panel implements MouseListener
{

    /**
     *   Unique id for this class, Exception implements Serializable interface see documentation
     * for serializable interface for recommendations regarding this variable.
     */
    private static final long serialVersionUID = 2613908L;


    private boolean force_info_new = false;
    public URLLoadInfoPanel m_loadingPanel = null;
    boolean info_is_new = false;
    Hashtable state_table_files = new Hashtable();
    CodeFileInfo currentFileInfo = null;
    Color comment_color = new Color(0,0,128);
    int x = 0;
    int y = 0;
    Dimension d = null;
    int num_lines_on_screen;
    int total_lines;
    Font fn;
    FontMetrics fn_metrics;
    int line_height;
    int first_line_to_show = 0;
    int max_width;
    long last_scroll_y_update = 0;
    boolean use_color = true;

    public void reset()
    {
	state_table_files = null;
	first_line_to_show =0;
	last_scroll_y_update = 0;
	currentFileInfo = null;
	state_table_files = new Hashtable();
    }

    public CodeDisplayPanel()
    {
	this(700, 500);
	addMouseListener(this);
    }

    public CodeDisplayPanel(int new_width, int new_height)
    {
	try
	    {
		if(new_width > 1024)
		    {
			new_width = 1024;
		    }
		if(new_height > 1024)
		    {
			new_height = 1024;
		    }
		d = new Dimension(new_width, new_height);
		setSize(new_width, new_height);
		try
		    {
			setFont(new Font("Courier", Font.PLAIN,12));
		    }
		catch(Exception e)
		    {
		    }
		fn = getFont();
		if(null != fn)
		    {
			fn_metrics = getFontMetrics(fn);
			line_height = fn_metrics.getHeight();
		    }
		else
		    {
			line_height = 10;
		    }
		if(line_height < 10)
		    {
			line_height = 10;
		    }
		//line_height *= 1.2;
		num_lines_on_screen = d.height / line_height;
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
	setBackground(Color.white);
	addMouseListener(this);
    }

    public Dimension getPreferredSize()
    {
	return d;
    }

    public Dimension getMinimumSize()
    {
	return d;
    }



    @SuppressWarnings("unchecked")
    public void LoadCodeFile(String code_file_name)
    {
	try
	    {
		boolean inside_comment = false;
		int slash_index = -1;
		String line;
		if(null == code_file_name || code_file_name.length() < 1)
		    {
			return;
		    }
		if(code_file_name.equals("0") || code_file_name.equals("null") ||
		   code_file_name.equals("unknown") || code_file_name.equals(""))
		    {
			info_is_new = false;
			return;
		    }
		if(null != currentFileInfo)
		    {
			if(code_file_name.equals(currentFileInfo.fileName))
			    {
				info_is_new = false;
				return;
			    }
		    }
		if(null == state_table_files)
		    {
			state_table_files = new Hashtable();
		    }
		currentFileInfo = (CodeFileInfo) state_table_files.get(code_file_name);
		info_is_new = true;
		if(null != currentFileInfo)
		    {
			total_lines = currentFileInfo.codeLinesVector.size();
			max_width = currentFileInfo.max_line_width;
			last_scroll_y_update = 0;
			first_line_to_show = 0;
			repaint();
			return;
		    }
		info_is_new = true;
		//System.out.println("code_file_name = "+code_file_name);
		URL_and_FileLoader loader = new URL_and_FileLoader(code_file_name);
		currentFileInfo = new CodeFileInfo();
		currentFileInfo.fileName = code_file_name;
		if(null == currentFileInfo.codeLinesVector)
		    {
			currentFileInfo.codeLinesVector = new Vector();
		    }

		if(null != m_loadingPanel)
		    {
			m_loadingPanel.URLname = loader.name;
			m_loadingPanel.bytes_read = 0;
			m_loadingPanel.content_length = loader.content_length;
			m_loadingPanel.repaint();
		    }


		FileReadLoop:
		while(null != (line = loader.readLine()))
		    {
			if(null != m_loadingPanel)
			    {
				m_loadingPanel.bytes_read = loader.bytes_read;
				m_loadingPanel.repaint();
			    }
			LineInfo li = new LineInfo();
			li.line_starts_inside_comment = inside_comment;
			int tab_index = line.indexOf('\t');
			while(tab_index != -1)
			    {
				line = line.substring(0,tab_index) +"   "+line.substring(tab_index+1);
				tab_index = line.indexOf('\t');
			    }
			li.line = line;
			//System.out.println("line = "+line);
			//System.out.println("li.line_starts_inside_comment ="+li.line_starts_inside_comment);
			if(null != fn_metrics)
			    {
				int current_line_width = fn_metrics.stringWidth(line);
				if(currentFileInfo.max_line_width < current_line_width)
				    {
					currentFileInfo.max_line_width = current_line_width;
				    }
			    }
			currentFileInfo.codeLinesVector.addElement(li);
			slash_index = -1;
			while(true)
			    {
				slash_index = line.indexOf('/',slash_index+1);
				if(slash_index < 0 || slash_index >= line.length())
				    {
					break;
				    }
				//System.out.println("slash_index = "+slash_index);
				if(inside_comment && slash_index > 0)
				    {
					if(line.charAt(slash_index-1) == '*')
					    {
						inside_comment = false;
					    }
					continue;
				    }
				if(slash_index >= line.length() -1)
				    {
					break;
				    }
				if(!inside_comment && line.charAt(slash_index+1) == '/')
				    {
					continue FileReadLoop;
				    }
				if(!inside_comment && line.charAt(slash_index+1) == '*')
				    {
					inside_comment = true;
				    }
			    }
		    }
		total_lines = currentFileInfo.codeLinesVector.size();
		max_width = currentFileInfo.max_line_width;
		currentFileInfo.line_number = 1;
		state_table_files.put(code_file_name, currentFileInfo);
		last_scroll_y_update = 0;
		first_line_to_show = 0;
		repaint();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }

    public void paint(Graphics g)
    {
	if(null == currentFileInfo)
	    {
		return;
	    }
	for(int i = 0; i < num_lines_on_screen && i < total_lines - first_line_to_show; i++)
	    {
		LineInfo li = (LineInfo) currentFileInfo.codeLinesVector.elementAt(i+first_line_to_show);
		if(currentFileInfo.line_number == i+first_line_to_show)
		    {
			if(use_color)
			    {
				g.setColor(Color.red);
			    }
			else
			    {
				g.setColor(Color.black);
			    }
			g.fillRect(0, (int) (i*line_height+line_height*0.25), d.width, (int) (line_height*0.9));
			if(!use_color)
			    {
				g.setColor(Color.white);
				g.drawString(li.line, -x, (i+1)*line_height);
				continue;
			    }
		    }
		if(li.line_starts_inside_comment && use_color)
		    {
			g.setColor(comment_color);
			g.drawString(li.line, -x, (i+1)*line_height);
		    }
		else
		    {
			int cpp_comment_index = li.line.indexOf("//");
			int c_comment_index = li.line.indexOf("/*");
			int comment_index = -1;
			if(c_comment_index > -1)
			    {
				comment_index = c_comment_index;
			    }
			else if(cpp_comment_index > -1)
			    {
				comment_index = cpp_comment_index;
			    }
			if(comment_index > -1 && fn_metrics != null && use_color)
			    {
				g.setColor(Color.black);
				g.drawString(li.line.substring(0,comment_index), -x, (i+1)*line_height);
				int start_string_width = fn_metrics.stringWidth(li.line.substring(0,comment_index));
				g.setColor(comment_color);
				g.drawString(li.line.substring(comment_index), start_string_width-x, (i+1)*line_height);
			    }
			else
			    {
				g.setColor(Color.black);
				g.drawString(li.line, -x, (i+1)*line_height);
			    }
		    }
	    }
    }

    public void setScrollx(int newx)
    {
	if(x != newx)
	    {
		x = newx;
		repaint();
	    }
    }

    public void setScrolly(int newy)
    {
	if(newy < 0)
	    {
		return;
	    }
	last_scroll_y_update = System.currentTimeMillis();
	if(y != newy)
	    {
		first_line_to_show = newy;
		y = newy;
		last_scroll_y_update = System.currentTimeMillis();
		repaint();
	    }
    }

    public void mouseClicked(MouseEvent evt)
    {
	mousePressed(evt);
    }
    public void mouseEntered(MouseEvent evt)
    {
    }
    public void mouseExited(MouseEvent evt)
    {
    }
    public void mouseReleased(MouseEvent evt)
    {
    }
    public void mousePressed(MouseEvent  evt)
    {
	//	int x = evt.getX();
	//int y = evt.getY();
	forced_update();
    }
    
    public void forced_update()
    {
	last_scroll_y_update = 0;
	first_line_to_show = currentFileInfo.line_number - (currentFileInfo.line_number%(num_lines_on_screen/2));
	force_info_new = true;
	repaint();
    }

    public void setLineNumber(int new_line_number)
    {
	if(force_info_new)
	    {
		info_is_new = true;
		force_info_new = false;
	    }
	new_line_number--;
	//System.out.println("new_line_number = "+new_line_number);
	if(new_line_number <= 0)
	    {
		return;
	    }

	if(currentFileInfo.line_number != new_line_number)
	    {
		currentFileInfo.line_number = new_line_number ;
		info_is_new = true;
		if(new_line_number - first_line_to_show < num_lines_on_screen
		   && new_line_number > first_line_to_show && first_line_to_show > -1)
		    {
			repaint();
			return;
		    }
		if(System.currentTimeMillis() - last_scroll_y_update  < 5000 && last_scroll_y_update != 0)
		    {
			repaint();
			return;
		    }
		if(new_line_number > num_lines_on_screen/2)
		    {
			first_line_to_show = new_line_number - (new_line_number%(num_lines_on_screen/2));
		    }
		else
		    {
			first_line_to_show = 0;
		    }
		repaint();
	    }
    }
}
